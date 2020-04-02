#' Class for containing the heatmap and additional objects
#' @param RawData Matrix of gene expression (required)
#' @param ProcessedData Matrix of gene expression after processing (generated)
#' @param Plots list of all the plots generated (generated)
#' @param PlotData list of all data used to generate the plots. (generated)
#' @param ColInfo Data frame of info for column sidebars (optional)
#' @param RowData Data frame of info for row sidebars (optional)
#' @param Information list of various aspects of the data
#' @keywords Mutation Waterfall
#' @export
#' @examples
#' heatmapClass <- heatmapClass(RawData = expressionMatrix)

heatmapClass <- setClass("heatmapClass", slots = c(RawData="matrix",
                                                   ProcessedData="matrix",
                                                   SampleInfo="data.frame",
                                                   FeatureData="data.frame",
                                                   Plot="list",
                                                   PlotData="list",
                                                   Information="list",
                                                   Colv = "logical",
                                                   Rowv = "logical"),
                         prototype=list(
                                 Colv = TRUE,
                                 Rowv = TRUE
                         ))

colOrder <- function(heatmapClass) {
        if ("colOrder" %in% names(heatmapClass@Information)) {
                heatmapClass@Information$colOrder
        } else {
                colnames(heatmapClass@RawData)
        }
}

rowOrder <- function(heatmapClass){
        if ("rowOrder" %in% names(heatmapClass@Information)) {
                heatmapClass@Information$rowOrder
        } else {
                rownames(heatmapClass@RawData)
        }
}
