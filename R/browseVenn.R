#' Browse the venn plot
#' 
#' Brow the venn plot in a web browser to adjust the plot and export the result.
#' 
#' @param plot plots of \link{vennPlot} or \link{upsetPlot}
#' @param width width of the figure
#' @param height height of the figure
#' @return An object of class htmlwidget that will intelligently print itself
#' into HTML in a variety of contexts including the R console, within R
#' Markdown documents, and within Shiny output bindings.
#' @export
#' @importFrom htmlwidgets createWidget
#' @importFrom ggplot2 ggsave
#' @importFrom svglite svglite
#' @importFrom methods getPackageName
#' @examples
#' pd <- system.file("extdata", package = "hicVennDiagram", mustWork = TRUE)
#' fs <- dir(pd, pattern = ".bedpe", full.names = TRUE)
#' vc <- vennCount(fs)
#' p <- vennPlot(vc)
#' browseVenn(p)
#' 
browseVenn <- function(plot, 
                         width=NULL,
                         height=NULL){
    stopifnot("plot must be a grid plot or ggplot object"=
                  inherits(plot, c("gTree", "grob", "ggplot")))
    tmpf <- tempfile(fileext = ".svg")
    ggsave(filename = tmpf, plot = plot)
    content <- readLines(tmpf)
    # line 1 <?xml
    # line 2 <svg
    # last line </svg
    x <- list(
        data = paste(content[-1], collapse="\n")
    )
    
    htmlwidgets::createWidget(
        name = 'browseVenn',
        x = x,
        width = width,
        height = height,
        package = getPackageName()
    )
}


#' Shiny bindings for browseVenn
#'
#' Output and render functions for using browseVenn within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a browseVenn
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name browseVenn-shiny
#'
#' @export
#' @importFrom htmlwidgets shinyWidgetOutput
browseVennOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId, 'browseVenn', width, height, 
                                   package = getPackageName())
}

#' @rdname browseVenn-shiny
#' @export
#' @importFrom htmlwidgets shinyRenderWidget
renderbrowseVenn <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, browseVennOutput, env, quoted = TRUE)
}
