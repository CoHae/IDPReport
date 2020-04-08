#' 3 Dimensional Scatterplot for VLXT, VSL2 and PONDRFIT incl Disease and/or Mutagenesis Distinction
#'
#' @param dataset A dataset with columns VLXT, VSL2, PONDRFIT and DM
#' @return 3 dimensional scatterplot
#' @examples
#' scatterpl3DDM(TPRdataset)


scatterplot3dDM <- function(dataset) {
  dataset <- dataset[order(dataset$DM),]

  #PONDRFIT% is as 0.2 in the dataset, I make it 20.
  dataset$PONDRFIT <- dataset$PONDRFIT*100

  colors <- c("tomato", "steelblue", "palegreen3", "yellow")

  # "as.factor" necessary to designate $DM as categorical variable
  # that determines color indicators
  colors <- colors[as.factor(dataset$DM)]

  levels(dataset$DM) <- c("No Disease or Mutagenesis noted",
                          "Disease noted",
                          "Disease and Mutagenesis noted",
                          "Mutagenesis noted")

  scatterplot3d(dataset$PONDRFIT, dataset$VLXT, dataset$VSL2, pch = 16, color=colors,
                xlab = "PONDR_FIT",
                ylab = "VLXT",
                zlab = "VSL2",
                xlim=c(0,100),
                ylim=c(0,100),
                zlim=c(0,100))

  legend("topleft", legend = levels(dataset$DM), cex = 0.7,
      bty = "n", bg = "transparent",
      col = c("tomato", "steelblue", "palegreen3", "yellow"), pch = 16)
#      ,inset = -0.50, xpd = TRUE, horiz = TRUE)

}
