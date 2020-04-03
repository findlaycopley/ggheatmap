#' function to plot the dendrograms
#' @param heatmapClass object of class heatmap (required)
#' @param row (TRUE/FALSE) plot dendro for rows (default: TRUE)
#' @param col (TRUE/FALSE) plot dendro for columns (default: TRUE)
#' @description Plots the dendrograms from the hclust data stored in @PlotData
#' @keywords plot dendro
#' @export
#' @examples
#' heatmapClass <- plotDendro(heatmapClass, row=TRUE, col=TRUE)


plotDendro <- function(heatmapClass, row=TRUE, col=TRUE) {
        if (row & "Hclust_Row" %in% names(heatmapClass@PlotData)) {
                heatmapClass@PlotData[["Row_dend"]] <- heatmapClass@PlotData$Hclust_Row %>%
                        dendro_data(type="rectangle") %>%
                        segment()
                heatmapClass@Plot[["Row_dend"]] <- heatmapClass@PlotData[["Row_dend"]] %>%
                        ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
                        geom_segment() +
                        coord_flip() +
                        scale_y_reverse() +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank()) +
                        scale_x_discrete()
        }
        if (col & "Hclust_Col" %in% names(heatmapClass@PlotData)) {
                heatmapClass@PlotData[["Col_dend"]] <- heatmapClass@PlotData$Hclust_Col %>%
                        dendro_data(type="rectangle") %>%
                        segment()
                heatmapClass@Plot[["Col_dend"]] <- heatmapClass@PlotData[["Col_dend"]] %>%
                        ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
                        geom_segment() +
                        theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank()) +
                        scale_x_discrete()
        }
        heatmapClass
}
