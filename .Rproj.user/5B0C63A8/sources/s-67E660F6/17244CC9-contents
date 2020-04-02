#' Function to plot heatmap
#' @param heatmapClass object of class heatmap (required)
#' @param high colour for high scores (default: red)
#' @param mid colour for mid scores (default: white)
#' @param low colour for high scores (default: blue)
#' @param midpoint score for mid colour (default: 0 as data is z score by default)
#' @description Plots the dendrograms from the hclust data stored in PlotData
#' @keywords plot heatmap
#' @export
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, high = "red", mid = "white", low = "blue", midpoint = 0)

plotHeatmap <- function(heatmapClass, high = "red", mid = "white", low = "blue", midpoint = 0) {
        heatmapClass@PlotData[["Heatmap"]] <- heatmapClass@ProcessedData %>%
                melt()
        heatmapClass@Plot[["Heatmap"]] <- heatmapClass@PlotData[["Heatmap"]] %>%
                ggplot(aes(x=factor(X2, levels=colOrder(heatmapClass)),
                           y=factor(X1, levels=rowOrder(heatmapClass)),
                           fill=value)) +
                geom_tile() +
                scale_fill_gradient2(low = low,
                                     mid = mid,
                                     high = high,
                                     midpoint = midpoint,
                                     name = ifelse(heatmapClass@Information$Scaling == "none",
                                                   "expression",
                                                   "z score")) +
                scale_y_discrete(position="right") +
                labs(x="",y="") +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.text.x = element_text(angle = 90,
                                                 hjust = 1,
                                                 vjust=0.5)) +
                guides(fill = guide_colourbar(title.position = "top",
                                              title.hjust = 0.5))

        heatmapClass
}
