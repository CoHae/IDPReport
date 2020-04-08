#' Displays VLXT, VSL2 and PONDRFIT next to each other for low, medium and high disorder categories
#'
#' @param dataset A dataset containing the columns VLXT, VSL2 and PONDRFIT
#' @return A side-by-side grouped histogram
#' @examples
#' groupedhist(TPRdataset)

groupedhist <- function(dataset) {

  # grouped frequency. Displays VLXT, VSL2 and PONDRFIT next to each other for the 3 groups
  predictor <- "VLXT"
  dataset$groupedVLXT <- cut(dataset$VLXT, c(-Inf, 10,30, Inf),
                             labels=c("0 to 10", "10 to 30", "greater 30"))
  groups <- "0 to 10"
  occurences <- sum(dataset$groupedVLXT == "0 to 10")
  groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "10 to 30"))
  groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedVLXT == "greater 30"))

  groupedplot <- data.frame(groups, occurences, predictor)

  #VSL2-------------------------------------------------------
  predictor <- "VSL2"

  dataset$groupedVSL2 <- cut(dataset$VSL2, c(-Inf, 10,30, Inf),
                             labels=c("0 to 10", "10 to 30", "greater 30"))
  #groups <- "0 to 10"
  occurences <- sum(dataset$groupedVSL2 == "0 to 10")
  #groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedVSL2 == "10 to 30"))
  #groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedVSL2 == "greater 30"))

  groupedVSL2 <- data.frame(groups, occurences, predictor)
  # rbind adds rows to data frame
  groupedplot <- rbind(groupedplot, groupedVSL2)

  #PONDR FIT-------------------------------------------
  predictor <- "PONDRFIT"

  # either cut at 0.1/0.3 or multiply PONDRFIT by 100 bc in PONDRFIT 30% is given as 0.3
  #dataset$groupedPONDRFIT <- cut(dataset$PONDRFIT, c(-Inf, 10,30, Inf),
  dataset$groupedPONDRFIT <- cut(dataset$PONDRFIT, c(-Inf, 0.1, 0.3, Inf),
                                 labels=c("0 to 10", "10 to 30", "greater 30"))
  #groups <- "0 to 10"
  occurences <- sum(dataset$groupedPONDRFIT == "0 to 10")
  #groups <- c(groups, "10 to 30")
  occurences <- c(occurences, sum(dataset$groupedPONDRFIT == "10 to 30"))
  #groups <- c(groups, "greater 30")
  occurences <- c(occurences, sum(dataset$groupedPONDRFIT == "greater 30"))

  groupedPONDRFIT <- data.frame(groups, occurences, predictor)
  # rbind adds rows to data frame
  groupedplot <- rbind(groupedplot, groupedPONDRFIT)

  plotdata <- groupedplot %>%
    mutate(predictor = factor(predictor), groups = factor(groups))

  ggplot(plotdata, aes(fill=predictor, y=occurences, x=groups)) +
       geom_bar(stat="identity", position="dodge")

}
