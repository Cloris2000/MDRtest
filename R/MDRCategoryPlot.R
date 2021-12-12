#' Plotting the percentage of each MDR, XDR, PDR category.
#'
#' A function that takes in a list of dataframes and convert the antimicrobial
#' resistance level to three categories: MDR, XDR, and PDR. The barplot indicates
#'  the percentage of MDR, XDR, and PDR isolates in the data.

#'
#' @param dataframe A dataframe that contains all sample IDs and their MDR categories.
#'
#' @return Returns a histogram.
#'
#' @examples
#' # Examples 1:
#' # plot the percentage
#' x <- classifyAllMDR(drug_resistance = DrugResistance[1:100,],
#'                                antimicrobial_agents = 'Antimicrobial.Agent')
#' MDRPlot(x)
#'
#'
#' @export
#' @importFrom graphics barplot
#'
MDRPlot <- function(dataframe){
  MDRPlot <- list()

  #check if the dataframe contains the neccessary columns
  if(! "Sample_ID" %in% colnames(dataframe) || ! "Category" %in% colnames(dataframe)){
    stop("missing sample ID or category")
  }

  arrangeData <- dataframe$Category[dataframe$Category != "NULL"]
  data <- table(arrangeData)
  data$Category <- factor(data$Category,levels = c("Not MDR", "MDR", "XDR"))
  MDRPlot <- graphics::barplot(data, ylab = "Number of Samples", xlab = "MDR Categories",
                                 main = "Distribution of Multidrug Resistance Categories",
                               col = "#a1e9f0", ylim=c(0,500))
  data$Category <- factor(data$Category,levels = c("Not MDR", "MDR", "XDR"))
  number <- as.matrix(data)
  text(MDRPlot, number+5, labels=as.character(number))

  return(MDRPlot)
}

# [END]
