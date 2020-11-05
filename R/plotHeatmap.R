#' Function to plot heatmap
#' @param heatmapClass object of class heatmap (required)
#' @param high colour for high scores (default: red)
#' @param mid colour for mid scores (default: white)
#' @param low colour for high scores (default: blue)
#' @param midpoint score for mid colour (default: 0 as data is z score by default)
#' @description Plots the dendrograms from the hclust data stored in PlotData
#' @keywords plot heatmap
#' @importFrom magrittr %>%
#' @export
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, high = "red", mid = "white", low = "blue", midpoint = 0)

plotHeatmap <- function(heatmapClass, high = "red", mid = "white", low = "blue", midpoint = 0) {
        heatmapClass@PlotData[["Heatmap"]] <- heatmapClass@ProcessedData %>%
                reshape2::melt()
                #'colnames'(c("Y", "X", "value"))
        print(heatmapClass@PlotData[["Heatmap"]])
        heatmapClass@Plot[["Heatmap"]] <- heatmapClass@PlotData[["Heatmap"]] %>%
                ggplot2::ggplot(
                        ggplot2::aes(x=factor(Var2, levels=colOrder(heatmapClass)),
                                     y=factor(Var1, levels=rowOrder(heatmapClass)),
                                     fill=value)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(low = low,
                                     mid = mid,
                                     high = high,
                                     midpoint = midpoint,
                                     name = ifelse(heatmapClass@Information$Scaling == "none",
                                                   "expression",
                                                   "z score")) +
                ggplot2::scale_y_discrete(position="right") +
                ggplot2::labs(x="",y="") +
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_text(angle = 90,
                                                 hjust = 1,
                                                 vjust=0.5)) +
                ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top",
                                              title.hjust = 0.5))

        heatmapClass
}
