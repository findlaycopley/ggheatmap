#' Function to run pipeline
#' @param heatmapClass object of class heatmap (required)
#' @param scale (row/col/none) how should the data be scaled (default: row)
#' @param Rowv (TRUE/FALSE) should the rows be clustered (default: TRUE)
#' @param Colv (TRUE/FALSE) should the columns be clustered (default: TRUE)
#' @param PRINT TRUE/FALSE should the plot be displayed (default: TRUE)
#' @description Runs the entire pipeline to generate a heatmap
#' @keywords plot heatmap
#' @export
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, scale="row", Rowv = TRUE, Colv = TRUE)

y <- matrix(c(1,2,3,
              5,5,6,
              3,2,1,
              4,3,6), nrow=3) %>%
        'colnames<-'(c("sample1","sample2","sample3","sample4")) %>%
        'rownames<-'(c("gene1","gene2","gene3"))

ggheatmap <- function(matrix, scale="row", Rowv = TRUE, Colv = TRUE, PRINT = TRUE) {
        heatmapClass <- heatmapClass(RawData = matrix, Rowv = Rowv, Colv = Colv)
        print("Scale data frame")
        if (scale == "row" | scale == "col" ){
                heatmapClass <- scaleMatrix(heatmapClass, type=scale)
        } else {
                heatmapClass@ProcessedData <- heatmapClass@RawData
        }
        if (Rowv) {
                heatmapClass <- generateRowDendro(heatmapClass)
        }
        if (Colv) {
                heatmapClass <- generateColDendro(heatmapClass)
        }
        if (Colv | Rowv) {
                heatmapClass <- plotDendro(heatmapClass)
        }
        heatmapClass <- plotHeatmap(heatmapClass)
        heatmapClass <- buildHeatmap(heatmapClass, PRINT = PRINT)
        heatmapClass
}
