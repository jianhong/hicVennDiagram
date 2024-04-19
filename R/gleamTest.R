#' Perform GLEAM test
#' @description
#' Run Genomic Loops Enrichment Analysis Method test
#' 
#' @param query,subject A vector of bedpe files or a list of genomic 
#' interaction data (\link[S4Vectors:Pairs-class]{Pairs} or
#'  \link[InteractionSet:GInteractions-class]{GInteractions}) or a list of
#'  GRanges object. 'subject' is optional if length of query > 1.
#' @param txdb An TxDb object to retrieve the sequence lengths, and genes 
#' annotations. Or an GRanges/GInteractions object.
#' @param background Background mode. The test will restricted within the 
#' region. The background is the background of subject if subject is available.
#' Otherwise, the background is the the background of second element of
#' comparsion group.
#' @param \dots parameters used by
#' \link[InteractionSet:findOverlaps]{findOverlaps}.
#' @export
#' @importFrom S4Vectors queryHits subjectHits
#' @importFrom GenomeInfoDb seqlengths seqinfo
#' @importFrom GenomicFeatures exonicParts intronicParts genes promoters
#' @importFrom IRanges reduce subsetByOverlaps findOverlaps width trim
#' @importFrom InteractionSet regions
#' @importFrom utils combn
#' @importFrom stats pbinom
#' @examples
#' # example code
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' library(TxDb.Hsapiens.UCSC.hg38.knownGene)
#' gleamTest(fs, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene)
#' grl <- GRangesList(exons=reduce(exons(TxDb.Hsapiens.UCSC.hg38.knownGene)),
#'  genes=reduce(genes(TxDb.Hsapiens.UCSC.hg38.knownGene)))
#' gleamTest(fs[seq_along(grl)], grl, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'gene')
#' gleamTest(grl, fs[seq_along(grl)], txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'gene')
#' gleamTest(grl[c(2, 1)], grl, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'gene')
#' gleamTest(grl, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'gene')
#' gleamTest(grl, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'genome')
gleamTest <- function(query, subject, txdb,
                      background=c('genome', 'gene', 'exon', 'intron'),
                      ...){
    background <- match.arg(background)
    stopifnot(inherits(txdb, c('TxDb', 'GRanges')))
    if(inherits(txdb, c('GRanges', 'GInteractions'))){
        if(background!='genome'){
            stop('background must be genome when txdb is GRanges or GInteractions.')
        }
        if(is(txdb, 'GRanges')){
            gr <- txdb
        }else{
            gr <- regions(txdb)
        }
    }else{
        gr <- switch(background,
                     'genome'=as(seqinfo(txdb), 'GRanges'),
                     'gene'=suppressWarnings(genes(txdb)),
                     'exon'=suppressWarnings(exonicParts(txdb)),
                     'intron'=suppressWarnings(intronicParts(txdb)))
        gr <- reduce(gr)
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
    getPval <- function(n_hit, n_total, prop){
        if(n_hit==0){
            p <- 1
        }else{
            p <- 1 - pbinom(n_hit - 1, n_total, prop)
        }
        p
    }
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
    
    res <- lapply(cmb, function(.ele){
        ## find out the total width of background
        q <- .ele[[1]]
        s <- .ele[[2]]
        if(is(s, 'GInteractions')){
            bck <- reduce(c(subsetByOverlaps(regions(s), gr, ...), gr.ext))
            prop <- sum(width(regions(s)))/sum(width(bck))
        }else{## GRanges
            s <- reduce(s)
            bck <- reduce(c(subsetByOverlaps(s, gr, ...), gr.ext))
            prop <- sum(width(s))/sum(width(bck))
        }
        
        ol <- findOverlaps(q, s, ...)
        n_hit <- length(unique(subjectHits(ol)))
        n_total <- length(s)
        c(prop = prop,
          n_obs = n_hit,
          n_total = n_total,
          n_exp = prop * n_total,
          pval = getPval(n_hit, n_total, prop))
    })
    
    return(do.call(rbind, res))
}