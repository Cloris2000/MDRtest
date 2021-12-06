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
#' x <- data.frame(Sample_ID = c("1","2","3","4","5","6"),
#'                 Category = c("MDR", "PDR", "NULL", "XDR", "PDR", "NULL"))
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
  MDRPlot <- graphics::barplot(data, ylab = "Number of Samples", xlab = "MDR Categories",
                                 main = "Distribution of Multidrug Resistance Categories",
                               col = "darkred")
  number <- as.matrix(data)
  text(MDRPlot, number+5, labels=as.character(number))

  return(MDRPlot)
}

# [END]
