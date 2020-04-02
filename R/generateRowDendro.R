#' Function for row dendro
#' @param heatmapClass object of class heatmap (required)
#' @description uses 1 - pearson correlation to generate a distance matrix and then performs hclust to cluster the columns.
#' @keywords cluster columns
#' @export
#' @examples
#' heatmapClass <- generateRowDendro(heatmapClass)

generateRowDendro <- function(heatmapClass) {
        heatmapClass@PlotData[["Hclust_Row"]] <- hclust(as.dist(1-cor(t(heatmapClass@ProcessedData), method="pearson")), method="complete")
        heatmapClass@Information$rowOrder <- heatmapClass@PlotData$Hclust_Row$labels[heatmapClass@PlotData$Hclust_Row$order]
        heatmapClass@Information$rowDendro = TRUE
        heatmapClass
}
