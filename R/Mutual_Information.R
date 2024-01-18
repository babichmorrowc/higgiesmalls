#' Calculate the mutual information between a variable and the label parameter, can be used to rank impact on classification.
#'
#' @param parameter Name of the column in [higgs_data_na] to calculate mutual information.
#'
#' @return The mutual information between [parameter] and [Label].
#' @import infotheo
#' @import dplyr
#' @export

#|Mutual Info-----------------------------------------|

Mutual_Information<- function(parameter){
  Variable = higgs_data_orig[parameter]
  Labels = higgs_data_orig["Label"]
  #take variable data form higgs orig
  Labels[Labels == "s"] <- "1"
  Labels[Labels == "b"] <- "0"
  Labels_Val <- apply(Labels,2,as.numeric)
  #replace with numeric for mutinfo
  ParaB <- discretize(Variable, nbins = 20000)
  ParaS <- discretize(Labels_Val,nbins = 20000)
  #mutinformation requires binned data
  return(mutinformation(ParaB,ParaS,method = "emp"))
}
#|----------------------------------------------------|


#' Plot the mutual information of a family of parameters with [Label]
#' 
#' @param MI_Data Dataframe consisting of two columns [data] and [numeric]
#' @return A ggplot of mutual information
Plot_Mutual_Information <- function(MI_Data){
    par(mar= c(10,12,10,5))
    bar_chart <- barplot(height = MI_Data$data, names.arg = MI_Data$names, col = "skyblue", las = 2, main = "Mutual Information Values", horiz = TRUE, xlab = "Mutual Information")
    return(bar_chart)
}