#' Perform GLEAM test
#' @description
#' Run Genomic Loops Enrichment Analysis Method test
#' 
#' @param query,subject A vector of bedpe files or a list of genomic 
#' interaction data (\link[S4Vectors:Pairs-class]{Pairs} or
#'  \link[InteractionSet:GInteractions-class]{GInteractions}) or a list of
#'  GRanges object. 'subject' is optional if length of query > 1.
#' @param background The test will restricted within the 
#' region. The background is the background of subject if subject is available.
#' Otherwise, the background is the the background of second element of
#' comparison group.
#' @param method Distribution type for p-value.
#' @param \dots parameters used by
#' \link[InteractionSet:findOverlaps]{findOverlaps}.
#' @export
#' @importFrom S4Vectors queryHits subjectHits
#' @importFrom GenomeInfoDb seqlengths seqinfo
#' @importFrom IRanges reduce subsetByOverlaps findOverlaps width trim promoters
#' @importFrom InteractionSet regions
#' @importFrom GenomicRanges GRanges
#' @importFrom utils combn
#' @importFrom stats pbinom phyper
#' @examples
#' # example code
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' library(TxDb.Hsapiens.UCSC.hg38.knownGene)
#' ## set.seed(123)
#' ## background <- createGIbackground(fs)
#' ## gleamTest(fs, background = background)
#' ## gleamTest(fs, background = background, method = 'hyper')
#' grl <- GRangesList(exons=reduce(exons(TxDb.Hsapiens.UCSC.hg38.knownGene)),
#'  genes=reduce(genes(TxDb.Hsapiens.UCSC.hg38.knownGene)))
#' gleamTest(fs[seq_along(grl)], grl, background = grl[['exons']])
#' gleamTest(grl[c(2, 1)], grl, background = grl[['exons']])
#' gleamTest(grl, background = grl[['genes']])
gleamTest <- function(query, subject,
                      background,
                      method=c('binom', 'hyper'),
                      ...){
    method <- match.arg(method)
    gr <- GRanges()
    if(!missing(background)){
        if(inherits(background, c('GRanges', 'GInteractions'))){
            if(is(background, 'GRanges')){
                gr <- background
            }else{
                gr <- regions(background)
            }
        }
    }else{
        stop('background is required!')
    }
    query <- readGI(query)
    if(!missing(subject)){
        subject <- readGI(subject)
        stopifnot(length(query)==length(subject))
        ## filter the subject by background
        subject <- lapply(subject, function(q){
            subsetByOverlaps(q, gr, ...)
        })
    }
    ## filter the query by background
    query <- lapply(query, function(q){
        subsetByOverlaps(q, gr, ...)
    })
    
    dots <- list(...)
    maxgap <- dots$maxgap
    if(is.null(maxgap)) maxgap <- 0L
    if(maxgap<0) maxgap <- 0L
    gr.ext <- trim(
        promoters(gr, upstream = maxgap, downstream = width(gr) + maxgap))
    if(!missing(subject)){
        cmb <- mapply(list, query, subject, SIMPLIFY = FALSE)
        names(cmb) <- paste(names(query), names(subject),
                            sep = '_enriched_with_')
    }else{
        ## testing the correlation among the query
        cmb <- combn(names(query), 2, simplify = FALSE)
        names(cmb) <- vapply(cmb, paste, FUN.VALUE = character(1L),
                             collapse="_enriched_with_")
        cmb <- lapply(cmb, function(.ele){
            query[.ele]
        })
    }
    
    checkOverlapsRate <- function(s){
        s <- unique(s)
        ss <- subsetByOverlaps(s, gr, ...)
        if(length(ss)/length(s)<.8){
            warning('The overlaps of background to subject is only ',
                    round(100*(length(ss)/length(s)), 3), '%')
        }
        ss <- subsetByOverlaps(s, gr.ext, invert = TRUE, ...)
        return(c(ss, gr.ext))
    }
    res <- lapply(cmb, function(.ele){
        ## find out the total width of background
        q <- .ele[[1]]
        s <- .ele[[2]]
        if(is(q, 'GInteractions') && is(s, 'GInteractions')){## GI vs GI
            if(is(background, 'GInteractions')){
                bck <- background
            }else{
                stop('background must be GInteractions!')
            }
        }else{
            if(is(q, 'GInteractions')){## GI vs GR
                s <- reduce(s, min.gapwidth=0L)
                bck <- checkOverlapsRate(s)
            }else{
                if(is(s, 'GInteractions')){## GR vs GI
                    if(is(background, 'GInteractions')){
                        bck <- background
                    }else{
                        bck <- checkOverlapsRate(regions(s))
                    }
                }else{ ## GR vs GR
                    s <- reduce(s, min.gapwidth=0L)
                    bck <- checkOverlapsRate(s)
                }
            }
        }
        if(method=='hyper'){
            n_total <- length(bck)
            n_query <- length(unique(q))
            n_subject <- length(unique(s))
            ## check if all the query and subject in the background
            bck_ol_q <- findOverlaps(q, bck, ...)
            n_q_in_bck <- length(unique(queryHits(bck_ol_q)))
            prop_q_in_bck <- n_q_in_bck / n_query
            bck_ol_s <- findOverlaps(s, bck, ...)
            n_subj_in_bck <- length(unique(queryHits(bck_ol_s)))
            prop_subj_in_bck <- n_subj_in_bck / n_subject
            if(prop_subj_in_bck<0.8 || prop_q_in_bck<0.8){
                warning('The overlaps of background to query or subject is low')
            }
            ol <- findOverlaps(q, s, ...)
            n_hit <- length(unique(queryHits(ol)))
            if(n_subject > n_total){
                n_total <- n_subject
            }
            if(n_hit==0){
                pval <- 1
            }else{
                pval <- 1 - phyper(n_hit - 1, n_subject,
                                   n_total - n_subject, n_query)
            }
            return(c(
                total_events = n_total,
                query_events = n_query,
                subject_events = n_subject,
                overlapping_events = n_hit,
                pvalue = pval))
        }else{
            n_bck <- length(bck)
            n_subject <- length(unique(s))
            bck_ol_s <- findOverlaps(s, bck, ...)
            n_subj_in_bck <- length(unique(queryHits(bck_ol_s)))
            prop_subj_in_bck <- n_subj_in_bck / n_subject
            if(prop_subj_in_bck<0.8){
                warning('The overlaps of background to query or subject is low')
            }
            prop <- n_subj_in_bck / n_bck
            ol <- findOverlaps(q, s, ...)
            n_hit <- length(unique(queryHits(ol)))
            n_query <- length(q)
            if(prop>1){
                prop <- 1
                warning('The background size is smaller than the subject.')
            }
            if(n_hit==0){
                pval <- 1
            }else{
                pval <- 1 - pbinom(n_hit - 1, n_query, prop)
            }
            
            return(c(proportions = prop,
                     query_events = n_query,
                     overlapping_events = n_hit,
                     expected_events = prop * n_query,
                     pvalue = pval))
        }
    })
    
    return(do.call(rbind, res))
}

#' Create background by input GInteractions
#' 
#' @description
#' Create background based on the distance distribution of input GInteractions.
#' 
#' @param gi A vector of bedpe files or a list of genomic 
#' interaction data (\link[S4Vectors:Pairs-class]{Pairs} or
#'  \link[InteractionSet:GInteractions-class]{GInteractions}).
#' @param size The maximal size of the background
#' @importFrom InteractionSet regions
#' @importFrom utils combn
#' @importFrom S4Vectors first second
#' @importFrom IRanges distance
#' @importFrom GenomeInfoDb seqnames
#' @export
#' @examples
#' 
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)[1]
#' set.seed(123)
#' # createGIbackground(fs)
createGIbackground <- function(gi, size=2*lengths(gi)){
    gi <- readGI(gi)
    gi <- Reduce(c, gi)
    gi <- unique(gi)
    reg <- regions(gi)
    reg_cmb <- combn(seq_along(reg), 2, simplify = TRUE)
    if(all(seqnames(first(gi))==seqnames(second(gi)))){ # intra-chrom
        keep <- seqnames(reg[reg_cmb[1, ]]) == 
            seqnames(reg[reg_cmb[2, ]])
        reg_cmb <- reg_cmb[, as.logical(keep), drop = FALSE]
    }
    
    ## check the distribution of dd
    dd <- distance(first(gi), second(gi))
    dd <- dd[!is.infinite(dd)]
    ddt <- table(dd)
    dd_bck <- distance(reg[reg_cmb[1, ]], reg[reg_cmb[2, ]])
    keep <- dd_bck %in% as.numeric(names(ddt))
    reg_cmb <- reg_cmb[, keep, drop = FALSE]
    dd_bck <- distance(reg[reg_cmb[1, ]], reg[reg_cmb[2, ]])
    size <- min(size, ncol(reg_cmb))
    ddt <- round(size*ddt/sum(ddt))
    keep <- mapply(names(ddt), ddt, FUN=function(dist, prop){
        w <- which(dd_bck==as.numeric(dist))
        sample(w, prop, replace = TRUE)
    })
    keep <- unique(sort(unlist(keep)))
    reg_cmb <- reg_cmb[, keep, drop = FALSE]
    bck <- GInteractions(reg[reg_cmb[1, ]], reg[reg_cmb[2, ]])
    bck <- sort(unique(c(bck, gi)))
    return(bck)
}