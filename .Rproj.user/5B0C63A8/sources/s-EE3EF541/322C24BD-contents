#' Function to scale the data
#' @param heatmapClass object of class heatmap (required)
#' @param type "row" or "column" scaling (default: row)
#' @description takes an object of the class heatmapClass and scales the data using scale, either by row (default) or by column.
#' @keywords scaling
#' @export
#' @examples
#' heatmapClass <- scaleMatrix(heatmapClass, type = "row")

scaleMatrix <- function(heatmapClass, type="row") {
        matrix <- heatmapClass@RawData
        if (type == "row") {
                matrix = t(matrix)
        }
        matrix = scale(matrix)
        if (type == "row") {
                matrix = t(matrix)
        }
        heatmapClass@ProcessedData <- matrix
        heatmapClass@Information[["Scaling"]] <- type
        heatmapClass
}
