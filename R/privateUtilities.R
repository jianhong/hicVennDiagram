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
