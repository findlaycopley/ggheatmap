#' Function to run pipeline
#' @param heatmapClass object of class heatmap (required)
#' @param scale (row/col/none) how should the data be scaled (default: row)
#' @param Rowv (TRUE/FALSE) should the rows be clustered (default: TRUE)
#' @param Colv (TRUE/FALSE) should the columns be clustered (default: TRUE)
#' @param PRINT TRUE/FALSE should the plot be displayed (default: TRUE)
#' @description Runs the entire pipeline to generate a heatmap
#' @keywords plot heatmap
#' @export ggheatmap
#' @export testData
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, scale="row", Rowv = TRUE, Colv = TRUE)
#' @importFrom magrittr %>%
ggheatmap <- function(matrix,
                      scale="row",
                      Rowv = TRUE,
                      Colv = TRUE,
                      PRINT = TRUE) {
        ## Create the heatmap class
        heatmapClass <- heatmapClass(RawData = matrix,
                                     Rowv = Rowv,
                                     Colv = Colv)
        ## Scale matrix based on the scale parameter
        print("Scale data frame")
        if (scale == "row" | scale == "col" ){
                ## If scale set (default row) scale the matrix
                heatmapClass <- scaleMatrix(heatmapClass, type=scale)
        } else {
                ## Of scale not set do not scale the matrix
                heatmapClass@ProcessedData <- heatmapClass@RawData
        }
        ## If Rowv true generate dendrograms to plot for rows
        if (Rowv) {
                heatmapClass <- generateRowDendro(heatmapClass)
        }
        ## If Colv true generate dendrograms to plot for cols
        if (Colv) {
                heatmapClass <- generateColDendro(heatmapClass)
        }
        ## If either Colv or Rowv are TRUE plot them
        if (Colv | Rowv) {
                heatmapClass <- plotDendro(heatmapClass)
        }
        ## Generate the heatmap
        heatmapClass <- plotHeatmap(heatmapClass)
        ## Build all the plots together.
        heatmapClass <- buildHeatmap(heatmapClass, PRINT = PRINT)
        heatmapClass
}

testData <- matrix(c(1,2,3,
                     5,5,6,
                     3,2,1,
                     4,3,6), nrow=3) %>%
        'colnames<-'(c("sample1","sample2","sample3","sample4")) %>%
        'rownames<-'(c("gene1","gene2","gene3"))
