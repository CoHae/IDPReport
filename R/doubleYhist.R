#' Frequency plus Relative Frequency Histogram on a Double Y Axis Scale
#'
#' @param dataset A dataset
#' @return A double y scale histogram plus pie chart
#' @examples
#' doubleYhist(TPRdataset)

doubleYhist <- function(dataset) {
  VLXTVector <- dataset$VLXT
  # Cut data into sections
  # breaks = 0:10
  breaks <- c(0,10,20,30,40,50,60,70,80,90,100)

  # Cut data into sections
  VLXTVector.cut = cut(VLXTVector, breaks, right=FALSE)
  VLXTVector.freq = table(VLXTVector.cut)

  # Calculate relative frequency
  VLXTVector.relfreq = VLXTVector.freq / length(VLXTVector)

  # Parse to a list to use xyplot later and assigning x values
  VLXTVector.list <- list(x = c(10,20,30,40,50,60,70,80,90,100),
                          y = as.vector(VLXTVector.relfreq))
  #Build histogram and relative frequency curve
  hist1 <- histogram(VLXTVector, breaks = 10, freq = TRUE, col='skyblue',
                     xlab="VLXT % Disorder",
                     ylab="Frequency",
                     main="Frequency Distribution of the Percent Disorder Score",
                     plot=FALSE)
  relFreqCurve <- xyplot(y ~ x, VLXTVector.list, type="l",
                         ylab = "Relative frequency", ylim=c(0,1))

  doubleYScale(hist1, relFreqCurve, add.ylab2 = TRUE)
  # Build double objects plot
  # histgraph <- as.grob(doubleYScale(hist1, relFreqCurve, add.ylab2 = TRUE))
  #  piegraph <- base2grob(~pie(VLXTVector.freq, labels = breaks))
  # grid.arrange(histgraph, nrow = 1)
  #  grid.arrange(histgraph,piegraph, nrow = 1)
}
