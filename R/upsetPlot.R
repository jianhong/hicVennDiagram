#' UpSet plot for the Venn count table
#'
#' Plot the overlaps counts by ComplexUpset.
#'
#' @param vennTable An vennTable object, the first element in the output of
#'  \link{vennCount}.
#' @param label_all A list of parameters used by \link[ggplot2]{geom_label} for
#' text labels of counts for each group. If it set to FALSE or length of the
#' list is zero, the labels will be ignored.
#' @param coln_prefix The prefix to be removed for colnumn names of vennTable.
#' @param ... Parameters could be passed to \link[ComplexUpset:upset]{upset}
#'  except `data` and `intersect`.
#' @return A ggplot object.
#' @export
#' @importFrom ComplexUpset upset
#' @importFrom ggplot2 aes layer .data unit theme
#' @importFrom reshape2 melt
#' @examples
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' vc <- vennCount(fs)
#' upset_themes_fix <- lapply(ComplexUpset::upset_themes, function(.ele){
#' .ele[names(.ele) %in% names(formals(theme))]
#' })
#' upsetPlot(vc, theme = upset_themes_fix)
#' ## change the font size of lables and numbers
#' themes <- ComplexUpset::upset_modify_themes(
#'  ## get help by vignette('Examples_R', package = 'ComplexUpset')
#'         list('intersections_matrix'=
#'             ggplot2::theme(axis.text.y=ggplot2::element_text(size=24)))
#' )
#' themes <- lapply(themes, function(.ele){
#' .ele[names(.ele) %in% names(formals(theme))]
#' })
#' upsetPlot(vc, label_all=list(
#'                         na.rm = TRUE,
#'                         color = 'gray30',
#'                         alpha = .7,
#'                         label.padding = grid::unit(0.1, "lines"),
#'                         size = 5
#' ), themes = themes)
upsetPlot <- function(vennTable,
                      label_all=list(
                          na.rm = TRUE,
                          color = 'gray30',
                          alpha = .7,
                          label.padding = unit(0.1, "lines")
                      ), coln_prefix=NULL,
                      ...){
    stopifnot(is(vennTable, "vennTable"))
    if(!is.null(coln_prefix)){
        stopifnot(is.character(coln_prefix))
    }
    combinations <- vennTable$combinations
    expInput <- vennTable$counts
    vennCounts <- vennTable$vennCounts

    plotdata <- combinations[rep(rownames(combinations), expInput), ]
    plotdata <- as.data.frame(plotdata)
    p <- upset(data=plotdata, intersect=colnames(plotdata), ...)
    if(length(label_all)==0){
        return(p)
    }
    if(is.logical(label_all)){
        if(label_all[1]){
            return(p)
        }else{
            label_all <- list()
        }
    }
    intersection <- as.matrix(combinations)
    mode(intersection) <- "logical"
    intersection <- apply(intersection, 1, function(.ele)
        paste(which(.ele), collapse="-"))
    intersection <- cbind(intersection=intersection, as.data.frame(vennCounts))
    intersection <- intersection[intersection$intersection!="", , drop=FALSE]
    if(!is.null(coln_prefix)){
        colnames(intersection) <- sub(coln_prefix, "", colnames(intersection))
    }
    intersection <- melt(intersection,
                         id.vars="intersection",
                         variable.name = "group")
    intersection <- intersection[intersection$value!=0, ]
    levels(intersection$group) <- levels(p$data$group)
    p$layers <- c(p$layers,
                  layer(geom="label",
                        stat="identity",
                        position = "identity",
                        mapping=aes(label=.data$value),
                        data = intersection,
                        params = label_all))
    return(p)
}
