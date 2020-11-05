#' function to plot the dendrograms
#' @param heatmapClass object of class heatmap (required)
#' @param row (TRUE/FALSE) plot dendro for rows (default: TRUE)
#' @param col (TRUE/FALSE) plot dendro for columns (default: TRUE)
#' @description Plots the dendrograms from the hclust data stored in @PlotData
#' @keywords plot dendro
#' @importFrom magrittr %>%
#' @export
#' @examples
#' heatmapClass <- plotDendro(heatmapClass, row=TRUE, col=TRUE)

plotDendro <- function(heatmapClass, row=TRUE, col=TRUE) {
        if (row & "Hclust_Row" %in% names(heatmapClass@PlotData)) {
                heatmapClass@PlotData[["Row_dend"]] <- heatmapClass@PlotData$Hclust_Row %>%
                        ggdendro::dendro_data(type="rectangle") %>%
                        ggdendro::segment()
                heatmapClass@Plot[["Row_dend"]] <- heatmapClass@PlotData[["Row_dend"]] %>%
                        ggplot2::ggplot(
                                ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
                        ggplot2::geom_segment() +
                        ggplot2::coord_flip() +
                        ggplot2::scale_y_reverse() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                              axis.ticks = ggplot2::element_blank(),
                              axis.title = ggplot2::element_blank(),
                              panel.grid.major = ggplot2::element_blank(),
                              panel.grid.minor = ggplot2::element_blank(),
                              panel.background = ggplot2::element_blank()) +
                        ggplot2::scale_x_discrete()
        }
        if (col & "Hclust_Col" %in% names(heatmapClass@PlotData)) {
                heatmapClass@PlotData[["Col_dend"]] <- heatmapClass@PlotData$Hclust_Col %>%
                        ggdendro::dendro_data(type="rectangle") %>%
                        ggdendro::segment()
                heatmapClass@Plot[["Col_dend"]] <- heatmapClass@PlotData[["Col_dend"]] %>%
                        ggplot2::ggplot(
                                ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
                        ggplot2::geom_segment() +
                        ggplot2::theme(axis.text = ggplot2::element_blank(),
                              axis.ticks = ggplot2::element_blank(),
                              axis.title = ggplot2::element_blank(),
                              panel.grid.major = ggplot2::element_blank(),
                              panel.grid.minor = ggplot2::element_blank(),
                              panel.background = ggplot2::element_blank()) +
                        ggplot2::scale_x_discrete()
        }
        heatmapClass
}
