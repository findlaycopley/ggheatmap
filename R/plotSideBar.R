#' Function to run pipeline
#' @param heatmapClass object of class heatmap (required)
#' @description Runs the entire pipeline to generate a heatmap
#' @keywords plot side colour bars
#' @export plotColSideBar
#' @export plotRowSideBar
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, scale="row", Rowv = TRUE, Colv = TRUE)
#' @importFrom magrittr %>%
#'
plotColSideBar <- function(heatmapClass, SampleData, Colours = FALSE) {
        heatmapClass@SampleInfo <- SampleData
        heatmapClass@PlotData$SampleData <- reshape2::melt(SampleData, id.var = "Sample")
        heatmapClass@Plot$SampleBar <- ggplot2::ggplot(heatmapClass@PlotData$SampleData,
                                                       ggplot2::aes(x = Sample, y = variable, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::scale_y_discrete(position="right")
        if (typeof(Colours) == "list" | typeof(Colours) == "character") {
                heatmapClass@Plot$SampleBar <- heatmapClass@Plot$SampleBar +
                        ggplot2::scale_fill_manual(position="right", values = Colours)
        }
        heatmapClass@PlotData$SampleBarLegend <- ggpubr::get_legend(heatmapClass@Plot$SampleBar)
        heatmapClass@Plot$SampleBar <- heatmapClass@Plot$SampleBar +
                ggplot2::theme(legend.position = "none",
                               axis.text.x = ggplot2::element_blank(),
                               axis.ticks.x = ggplot2::element_blank(),
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank()
                )
        heatmapClass
}

plotRowSideBar <- function(heatmapClass, FeatureData, Colours = FALSE) {
        heatmapClass@FeatureData <- FeatureData
        heatmapClass@PlotData$FeatureData <- reshape2::melt(heatmapClass@FeatureData, id.var = "Feature")
        heatmapClass@Plot$FeatureBar <- ggplot2::ggplot(heatmapClass@PlotData$FeatureData,
                                                        ggplot2::aes(x = Feature, y = variable, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::coord_flip()
        if (typeof(Colours) == "list" | typeof(Colours) == "character") {
                heatmapClass@Plot$FeatureBar <- heatmapClass@Plot$FeatureBar +
                        ggplot2::scale_fill_manual(position="right", values = Colours)
        }
        heatmapClass@PlotData$FeatureBarLegend <- ggpubr::get_legend(heatmapClass@Plot$FeatureBar)
        heatmapClass@Plot$FeatureBar <- heatmapClass@Plot$FeatureBar +
                ggplot2::theme(
                        legend.position = "none",
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_text(angle = 90,
                                                            vjust = 0.5),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_blank(),

                )
        heatmapClass
}
