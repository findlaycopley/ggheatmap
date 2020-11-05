#' Function to run pipeline
#' @param heatmapClass object of class heatmap (required)
#' @param scale (row/col/none) how should the data be scaled (default: row)
#' @param Rowv (TRUE/FALSE) should the rows be clustered (default: TRUE)
#' @param Colv (TRUE/FALSE) should the columns be clustered (default: TRUE)
#' @param PRINT TRUE/FALSE should the plot be displayed (default: TRUE)
#' @param verbose TRUE/FALSE shows extra info (default: FALSE)
#' @description Runs the entire pipeline to generate a heatmap
#' @keywords plot heatmap
#' @export ggheatmap
#' @export testData
#' @export testFeatureData
#' @export testSampleData
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, scale="row", Rowv = TRUE, Colv = TRUE)
#' @importFrom magrittr %>%
ggheatmap <- function(matrix,
                      scale="row",
                      Rowv = TRUE,
                      Colv = TRUE,
                      PRINT = TRUE,
                      verbose = FALSE,
                      SampleData = FALSE,
                      FeatureData = FALSE,
                      SampleColour = FALSE,
                      FeatureColour = FALSE) {
        ## Create the heatmap class
        heatmapClass <- heatmapClass(RawData = matrix,
                                     Rowv = Rowv,
                                     Colv = Colv)
        ## Scale matrix based on the scale parameter
        if (scale == "row" | scale == "col" ){
                if (verbose){print(paste("Scale matrix by", scale))}
                ## If scale set (default row) scale the matrix
                heatmapClass <- scaleMatrix(heatmapClass, type=scale)
        } else {
                if (verbose){print(print("Data not scaled"))}
                ## Of scale not set do not scale the matrix
                heatmapClass@ProcessedData <- heatmapClass@RawData
        }
        ## If Rowv true generate dendrograms to plot for rows
        if (Rowv) {
                if (verbose){print("generating row dendrogram")}
                heatmapClass <- generateRowDendro(heatmapClass)
        }
        ## If Colv true generate dendrograms to plot for cols
        if (Colv) {
                if (verbose){print("generating column dendrogram")}
                heatmapClass <- generateColDendro(heatmapClass)
        }
        ## If either Colv or Rowv are TRUE plot them
        if (Colv | Rowv) {
                if (verbose){print("Plotting dendrogram")}
                heatmapClass <- plotDendro(heatmapClass)
        }
        ## If there is sample data make the bar plot
        if (typeof(SampleData) == "list") {
                ## If colours provided use them
                if (typeof(SampleColour) == "list" |
                    typeof(SampleColour) == "character")
                {
                        heatmapClass <- plotColSideBar(heatmapClass,
                                                       SampleData,
                                                       Colours = SampleColour)
                } else {
                heatmapClass <- plotColSideBar(heatmapClass,
                                               SampleData)
                }
        }
        ## If there is feature data make the bar plot
        if (typeof(FeatureData) == "list") {
                ## If colours provided use them
                if (typeof(FeatureColour) == "list" |
                    typeof(FeatureColour) == "character")
                {
                        heatmapClass <- plotRowSideBar(heatmapClass,
                                                       FeatureData,
                                                       Colours = FeatureColour)
                } else {
                        heatmapClass <- plotColSideBar(heatmapClass,
                                                       FeatureData)
                }
        }
        ## Generate the heatmap
        heatmapClass <- plotHeatmap(heatmapClass)
        ## Build all the plots together.
        heatmapClass <- buildHeatmap(heatmapClass, PRINT = PRINT)
        heatmapClass
}

testData <- matrix(c(7,2,3,
                     5,5,6,
                     8,2,1,
                     4,3,6), nrow=3) %>%
        'colnames<-'(c("sample1","sample2","sample3","sample4")) %>%
        'rownames<-'(c("gene1","gene2","gene3"))

testFeatureData <- cbind(Feature = c("gene1","gene2","gene3"),
                         Item1 = c(1,2,1),
                         Item2 = c(4,3,4)) %>%
        as.data.frame()

testSampleData <- cbind(Sample = c("sample1","sample2","sample3","sample4"),
                         Item1 = c("Mut","WT","WT","Mut"),
                         Item2 = 1:4) %>%
        as.data.frame()

