#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors first second mcols mcols<-
pairsToGInteractions <- function(x){
    x1 <- GInteractions(first(x), second(x))
    mcols(x1) <- mcols(x)
    x1
}

checkOL <- function(name1, name2, cmb){
    x <- c(name1, name2)
    out <- which(vapply(
        cmb,
        FUN = function(.ele) all(x %in% .ele),
        FUN.VALUE = logical(1L)))
    if(length(out)!=1){
        stop(name1, " and ", name2, "doesn't match any combination.")
    }
    out
}

reSortName <- function(x, sep="&"){
    x_s <- strsplit(x, sep)
    x_s <- lapply(x_s, sort)
    x_s <- vapply(x_s, paste, collapse="&", FUN.VALUE = character(1L))
    x_s
}

#' @importFrom rtracklayer import
readGI <- function(gi){
    if(is.character(gi)){
        if(length(names(gi))==length(gi)){
            n <- names(gi)
        }else{
            n <- basename(gi)
            if(any(duplicated(n))){
                n <- make.names(gi, unique = TRUE)
            }
        }
        gi <- lapply(gi, import)
        names(gi) <- n
    }
    if(length(names(gi))!=length(gi)){
        names(gi) <- paste0("gi_", seq_along(gi))
    }
    gi <- lapply(gi, function(.ele){
        if(!inherits(.ele, c("Pairs", "GInteractions", "GRanges"))){
            stop("gi must be a list of genomic interaction data in format of
                 Pairs, GInteractions, or GRanges.")
        }
        if(any(duplicated(.ele))){
            stop("gi must be a list of unique genomic interactions.")
        }
        if(is(.ele, "Pairs")){
            .ele <- pairsToGInteractions(.ele)
        }
        .ele
    })
    return(gi)
}