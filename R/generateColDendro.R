#' Function for col dendro
#' @param heatmapClass object of class heatmap (required)
#' @description uses 1 - spearman correlation to generate a distance matrix and then performs hclust to cluster the columns.
#' @keywords cluster rows
#' @export
#' @examples
#' heatmapClass <- generateColDendro(heatmapClass)

generateColDendro <- function(heatmapClass) {
        heatmapClass@PlotData[["Hclust_Col"]] <- hclust(as.dist(1-cor(heatmapClass@ProcessedData, method="spearman")), method="complete")
        heatmapClass@PlotData[["Hclust_Col"]] <- hclust(dist(t(heatmapClass@RawData)))
        heatmapClass@Information$colOrder <- heatmapClass@PlotData$Hclust_Col$labels[heatmapClass@PlotData$Hclust_Col$order]
        heatmapClass@Information$colDendro = TRUE
        heatmapClass
}
