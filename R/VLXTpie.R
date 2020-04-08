#' Displays VLXT as pie graph
#'
#' @param dataset A dataset containing the numeric column VLXT (values 0-100)
#' @return a pie graph
#' @examples
#' VLXTpie(TPRdataset)

VLXTpie <- function(dataset) {
  # grouped frequency. Displays VLXT, SL2 and PONDRFIT next to each other for the 3 groups
  predictor <- "VLXT"
  dataset$groupedVLXT <- cut(dataset$VLXT, c(-Inf, 10,30, Inf),
                             labels=c("0 to 10", "10 to 30", "greater 30"))
  groups <- "0 to 10"
  occurences <- sum(dataset$groupedVLXT == "0 to 10")
  groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "10 to 30"))
  groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "greater 30"))
  #
  #  groupedplot <- data.frame(groups, occurences, predictor)

  #save this info to make pie charts later
  pieVLXT <- occurences

  pct <- round(pieVLXT/sum(pieVLXT)*100)
  pielables <- paste(groups, pct) # add percents to labels
  pielables <- paste(pielables,"%",sep="") # ad % to labels

  pie(pieVLXT, labels = pielables, col=rainbow(length(pielables)),
      main = "VLXT % Disorder Score")
}
