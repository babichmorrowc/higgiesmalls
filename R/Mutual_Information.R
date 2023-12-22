#' Calculate the mutual information.
#'
#' @param parameter Name of the column in [higgs_data_na] to calculate mutual information.
#'
#' @return The mutual information of [parameter].
#' @import infotheo
#' @import dplyr
#' @export
#'
Mutual_Information <- function(parameter){
  XYdata <- ggplot_build(plots(parameter))$data[[1]]
  Para_GivenB <- filter(XYdata, fill =="#F8766D")$y
  Para_GivenS <- filter(XYdata,fill != "#F8766D")$y
  ParaB <- discretize(Para_GivenB, nbins = 200)
  ParaS <- discretize(Para_GivenS,nbins = 200)
  return(infotheo::mutinformation(ParaS,ParaB))
}
