#' Function to combine the plots
#' @param heatmapClass object of class heatmap (required)
#' @param PRINT TRUE/FALSE should the plot be displayed (default: TRUE)
#' @description Build all the plots together
#' @keywords plot heatmap
#' @export
#' @examples
#' heatmapClass <- plotHeatmap(heatmapClass, high = "red", mid = "white", low = "blue", midpoint = 0)

buildHeatmap <- function(heatmapClass, PRINT = TRUE) {
        if (heatmapClass@Rowv & heatmapClass@Colv) {
                print("Col and Row")
                ## Space and column Dendro
                heatmapClass@Plot$Combo <- (patchwork::plot_spacer() +
                                                    heatmapClass@Plot$Col_dend +
                                                    ## row Dendro and heatmap
                                                    heatmapClass@Plot$Row_dend + heatmapClass@Plot$Heatmap +
                                                    ## setlayout
                                                    patchwork::plot_layout(heights=c(1,5),
                                                                widths=c(1,5),
                                                                nrow=2)) &
                        ## remove padding
                        ggplot2::theme(plot.margin = ggplot2::unit(rep(0.01,4),"mm"))
        } else if (heatmapClass@Rowv) {
                print("Row")
                ## row Dendro and heatamp
                heatmapClass@Plot$Combo <- (z@Plot$Row_dend + z@Plot$Heatmap +
                                                    ## setlayout
                                                    patchwork::plot_layout(widths=c(1,5),
                                                                ncol=2)) &
                        ## remove padding
                        ggplot2::theme(plot.margin = ggplot2::unit(rep(0.01,4),"mm"))
        } else if (heatmapClass@Colv) {
                print("Col")
                ## column Dendro
                heatmapClass@Plot$Combo <- (heatmapClass@Plot$Col_dend +
                                                    ## heatmap
                                                    heatmapClass@Plot$Heatmap +
                                                    ## setlayout
                                                    patchwork::plot_layout(heights=c(1,5),
                                                                nrow=2)) &
                        ## remove padding
                        ggplot2::theme(plot.margin = ggplot2::unit(rep(0.01,4),"mm"))

        } else {
                heatmapClass@Plot$Combo <- heatmapClass@Plot$Heatmap &
                        ## remove padding
                        ggplot2::theme(plot.margin = ggplot2::unit(rep(0.01,4),"mm"))
        }
        if( PRINT ) {
                print(heatmapClass@Plot$Combo)
        }
        heatmapClass
}
