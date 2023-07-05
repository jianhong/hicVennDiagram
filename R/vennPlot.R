#' Venn diagram for the Venn count table
#'
#' Plot the overlaps counts by eulerr.
#'
#' @param vennTable An vennTable object, the first element in the output of
#'  \link{vennCount}.
#' @return A grid object.
#' @export
#' @importFrom eulerr euler
#' @examples
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' vc <- vennCount(fs)
#' vennPlot(vc)
vennPlot <- function(vennTable){
    stopifnot(is(vennTable, "vennTable"))
    combinations <- vennTable$combinations
    expInput <- vennTable$counts
    vennCounts <- vennTable$vennCounts
    names(expInput) <- apply(combinations, 1, FUN=function(i){
        paste(colnames(combinations)[as.logical(i)], collapse = "&")
    })
    fit <- euler(expInput[-1], shape = "ellipse")
    fit.original.values <- apply(vennCounts, 1, paste, collapse="/")
    names(fit.original.values) <- names(expInput)
    fit.original.values <- fit.original.values[-1]
    n1 <- names(fit$original.values)
    n2 <- names(fit.original.values)
    n1_s <- reSortName(n1, "&")
    n2_s <- reSortName(n2, "&")
    fake_fov <- seq_along(fit$original.values)
    names(fake_fov) <- n1
    fov <- fit.original.values[match(n1_s, n2_s)]
    names(fov) <- fake_fov
    fit$original.values <- fake_fov ##
    p <- plot(fit, quantities=TRUE)
    p$data$original.values <- fov
    p$data$centers$quantities <- fov[rownames(p$data$centers)]
    changeQuantityTag <- function(euler_grob, fov_map){
        if(length(euler_grob$children)>0){
            for(i in seq_along(euler_grob$children)){
                euler_grob$children[[i]] <-
                    changeQuantityTag(euler_grob$children[[i]], fov_map)
            }
        }else{
            if(grepl("tag.quantity", euler_grob$name)){
                euler_grob$label <- fov_map[euler_grob$label]
            }
        }
        euler_grob
    }
    p <- changeQuantityTag(p, fov)
    p
}
