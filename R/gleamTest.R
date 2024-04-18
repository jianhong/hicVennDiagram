#' Perform GLEAM test
#' @description
#' Run Genomic Loops Enrichment Analysis Method test
#' 
#' @param query A vector of bedpe files or a list of genomic interaction data
#'  (\link[S4Vectors:Pairs-class]{Pairs} or
#'  \link[InteractionSet:GInteractions-class]{GInteractions})
#' @param subject A vector of bed files or a GRangesList object. Optional.
#' @param txdb An TxDb object to retrieve the sequence lengths, and genes 
#' annotations. Or an GRanges object.
#' @param background Background mode. The test will restricted within the 
#' region.
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
#' gr <- exons(TxDb.Hsapiens.UCSC.hg38.knownGene)
#' grl <- GRangesList(gr1=gr, gr2=gr, gr3=gr)
#' gleamTest(fs, grl, txdb=TxDb.Hsapiens.UCSC.hg38.knownGene,
#'  background = 'gene')
gleamTest <- function(query, subject, txdb,
                      background=c('genome', 'gene', 'exon', 'intron'),
                      ...){
    background <- match.arg(background)
    stopifnot(inherits(txdb, c('TxDb', 'GRanges')))
    if(!missing(subject)){
        stopifnot(is(subject, 'GRangesList'))
        stopifnot(length(query)==length(subject))
    }
    if(is(txdb, 'GRanges')){
        if(background!='genome'){
            stop('background must be genome when txdb is GRanges.')
        }
        gr <- txdb
    }else{
        gr <- switch(background,
                     'genome'=as(seqinfo(txdb), 'GRanges'),
                     'gene'=suppressWarnings(genes(txdb)),
                     'exon'=suppressWarnings(exonicParts(txdb)),
                     'intron'=suppressWarnings(intronicParts(txdb)))
        gr <- reduce(gr)
    }
    query <- readGI(query)
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
        ## filter the subject by background
        subject <- lapply(subject, function(q){
            subsetByOverlaps(q, gr, ...)
        })
        res <- mapply(function(q, s){
            s <- reduce(s)
            ## find out the total width of background
            bck <- reduce(c(subsetByOverlaps(s, gr, ...), gr.ext))
            prop <- sum(width(s))/sum(width(bck))
            ol <- findOverlaps(q, s, ...)
            n_hit <- length(unique(subjectHits(ol)))
            n_total <- length(s)
            c(prop = prop,
              n_obs = n_hit,
              n_exp = prop * n_total,
              pval = getPval(n_hit, n_total, prop))
        }, query, subject, SIMPLIFY = FALSE)
    }else{
        ## testing the correlation among the query
        cmb <- combn(names(query), 2, simplify = FALSE)
        names(cmb) <- vapply(cmb, paste, FUN.VALUE = character(1L),
                             collapse="_")
        res <- lapply(cmb, function(.ele){
            ## find out the total width of background
            s <- query[[.ele[2]]]
            bck <- reduce(c(subsetByOverlaps(regions(s), gr, ...), gr.ext))
            prop <- sum(width(regions(s)))/sum(width(bck))
            
            ol <- findOverlaps(query[[.ele[1]]], s,
                         use.region="both", ...)
            n_hit <- length(unique(subjectHits(ol)))
            n_total <- length(s)
            c(prop = prop,
              n_obs = n_hit,
              n_exp = prop * n_total,
              pval = getPval(n_hit, n_total, prop))
        })
    }
    
    return(do.call(rbind, res))
}