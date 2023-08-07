#' Class \code{"vennTable"}
#' @description An object of class \code{"vennTable"}
#'              represents Venn counts.
#' @aliases vennTable
#' @rdname vennTable-class
#' @slot combinations A logical \code{"matrix"}, specify the combinations.
#' @slot counts A \code{"numeric"} vector, the overall counts number for
#'  each combination.
#' @slot vennCounts A \code{"matrix"} object, specify the counts number for
#' each sample in the combination.
#' @slot overlapList \code{"list"}, overlapping list of the genomic
#'  interactions.
#' @import methods
#' @exportClass vennTable
#' @examples
#' vt <- vennTable()
#'
setClass("vennTable",
         representation(
             combinations="matrix",
             counts="numeric",
             vennCounts="matrix",
             overlapList="list"),
         prototype(
             combinations=matrix(),
             counts=numeric(0L),
             vennCounts=matrix(),
             overlapList=list()
         ),
         validity=function(object){
             if(length(object@counts)!=nrow(object@combinations)){
                 return("The length of counts slot are not identical to
                        the number of rows of combinations slot.")
             }
             if(!identical(names(object@counts), rownames(object@vennCounts))){
                 return("The names of counts are not identical to
                        the rownames of vennCounts")
             }
             if(!identical(names(object@counts), rownames(object@combinations))){
                 return("The names of counts are not identical to
                        the rownames of combinations")
             }
             return(TRUE)
         }
)

#' @rdname vennTable-class
#' @param \dots Each argument in \dots becomes an slot in the new vennTable.
#' @export
#' @return An object of vennTable.

vennTable <- function(...){
    new("vennTable", ...)
}

#' Method $
#' @rdname vennTable-class
#' @param x an object of vennTable
#' @param name slot name of vennTable
#' @exportMethod $
#' @aliases $,vennTable-method
setMethod("$", "vennTable", function(x, name) slot(x, name))
#' Method $<-
#' @rdname vennTable-class
#' @param value values to assign
#' @exportMethod $<-
#' @aliases $<-,vennTable-method
setReplaceMethod("$", "vennTable",
                 function(x, name, value){
                     slot(x, name, check = TRUE) <- value
                     x
                 })
#' Method `[[`
#' @rdname vennTable-class
#' @param x an object of vennTable
#' @param i slot name of vennTable
#' @exportMethod `[[`
#' @aliases `[[`,vennTable-method
setMethod("[[", "vennTable", function(x, i) slot(x, i))
#' Method $<-
#' @rdname vennTable-class
#' @exportMethod `[[<-`
#' @aliases `[[<-`,vennTable-method
setReplaceMethod("[[", "vennTable",
                 function(x, i, value){
                     slot(x, i, check = TRUE) <- value
                     x
                 })
#' @rdname vennTable-class
#' @param object an object of vennTable.
#' @exportMethod show
#'
#' @aliases show,vennTable-method
setMethod("show", "vennTable", function(object){
    cat("An object of vennTable\n")
    for(i in c("combinations", "counts", "vennCounts")){
        cat('Slot "', i, '":\n', sep = "")
        show(object[[i]])
        cat("\n")
    }
    cat('Slot "overlapList":\n')
    if(length(object@overlapList)>0){
        cat("A list with names", names(object@overlapList),
            " and lengths", lengths(object@overlapList))
    }else{
        cat("A list with 0 element.")
    }
})
