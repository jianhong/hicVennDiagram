#' Construct intersections of sets
#'
#' Given a collection of bedpe files or a list of genomic interaction data,
#' \code{vennCount} will compute all possible combinations of interactions
#' and return an object of class \link{vennTable}, storing the combinations
#' as well as the number of elements in each intersection.
#'
#' @param gi A vector of bedpe files or a list of genomic interaction data
#'  (\link[S4Vectors:Pairs-class]{Pairs} or
#'  \link[InteractionSet:GInteractions-class]{GInteractions})
#' @param FUN Function to summarize the overlapping number.
#' @param \dots parameters used by
#' \link[InteractionSet:findOverlaps]{findOverlaps}
#' @export
#' @importFrom S4Vectors queryHits subjectHits
#' @importFrom IRanges findOverlaps
#' @importFrom rtracklayer import
#' @importFrom utils combn
#' @return An object of \link{vennTable}
#' @examples
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' vc <- vennCount(fs)
#'
vennCount <- function(gi, FUN = min, ...){
    stopifnot(is.function(FUN))
    gi <- readGI(gi)
    # get overlaps
    cmb <- combn(names(gi), 2, simplify = FALSE)
    names(cmb) <- vapply(cmb, paste, FUN.VALUE = character(1L), collapse="_")
    ol <- lapply(cmb, function(.ele){
        findOverlaps(gi[[.ele[1]]], gi[[.ele[2]]],
                     use.region="both", ...)
    })
    # create outcome table
    ncontrasts <- length(gi)
    outcomes <- lapply(seq.int(ncontrasts), function(j){
        rep(0:1,times=2^(j-1),each=2^(ncontrasts-j))
    })
    outcomes <- do.call(cbind, outcomes)
    colnames(outcomes) <- names(gi)
    rownames(outcomes) <- apply(outcomes, 1, paste, collapse="")
    ## get all the ids in each outcomes row for each inputs
    stopifnot(all(names(gi) %in% colnames(outcomes)))
    ids_in_ol <- apply(outcomes, 1, function(i){
        cn <- colnames(outcomes)[as.logical(i)]
        if(length(cn)==0) return(list())
        if(length(cn)==1){
            ## remove the overlaps
            out <- list(seq_along(gi[[cn]]))
            names(out) <- cn
            return(out)
        }
        y <- lapply(cn, function(.ele){
            others <- cn[cn!=.ele]
            ids <- lapply(others, function(.e){
                ol_id <- checkOL(.ele, .e, cmb)
                if(.ele==cmb[[ol_id]][1]){
                    fun <- queryHits
                }else{
                    fun <- subjectHits
                }
                .ol <- ol[[ol_id]]
                unique(fun(.ol))
            })
            Reduce(intersect, ids)
        })
        names(y) <- cn
        y
    }, simplify = FALSE)
    ## remove the double-counts
    ## for each row in outcomes, if all the TRUE samples exists in other outcomes
    ## row, remove the ids in other outcomes from this row
    overCountsRows <- apply(outcomes, 1, function(i){
        if(sum(i)==0) return(integer(0L))
        which(colSums(i*t(outcomes))==sum(i))
    }, simplify = FALSE)
    overCountsRows <- mapply(
        setdiff,
        overCountsRows, seq_along(overCountsRows), SIMPLIFY = FALSE)
    rm_over_counts <- lapply(
        seq_along(overCountsRows), FUN = function(this_row){
            this_group <- ids_in_ol[[this_row]]
            over_count_rows <- overCountsRows[[this_row]]
            if(length(over_count_rows)==0) return(this_group)
            for(i in over_count_rows){
                for(j in names(this_group)){
                    this_group[[j]] <- setdiff(
                        this_group[[j]],
                        ids_in_ol[[i]][[j]])
                }
            }
            this_group
        })

    names(rm_over_counts) <- rownames(outcomes)
    vennCounts <- lapply(colnames(outcomes), function(i){
        vapply(rm_over_counts, FUN = function(.ele){
            return(length(.ele[[i]]))
        }, FUN.VALUE = integer(1L))
    })
    vennCounts <- do.call(cbind, vennCounts)
    colnames(vennCounts) <- colnames(outcomes)

    counts <- vapply(rm_over_counts, FUN = function(.ele){
        x <- lengths(.ele)
        if(length(x)==0) return(0L)
        FUN(x)
    }, FUN.VALUE = integer(1L))

    overlapList <- lapply(rm_over_counts, function(.ele){
        mapply(.ele, names(.ele), FUN=function(.e, .n){
            gi[[.n]][.e]
        })
    })

    vennTable(combinations=outcomes,
              counts=counts,
              vennCounts=vennCounts,
              overlapList=overlapList)
}
