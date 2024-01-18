#' Produce a density plot for each variable, plotting the variable given b and s respectively
#'
#' @param Parameter Name of the column in [data] for which the plot will be created.
#' @param data Dataframe including column [Parameter]. Must also include column [Label].
#'
#' @return A ggplot object. This is a density plot for each variable, plotting the variable given b and s respectively.
#' @import ggplot2
#' @export
#'
plots <- function(Parameter, data = higgs_data_na){
  pl <- ggplot(data = data, aes(x= get(Parameter), fill = Label)) +
    geom_density(alpha = 0.5) +
    labs(x = Parameter)
  return(pl)
}

#' Produce the joint distribution of the var|b and var|s respectively
#'
#' @param P1 Name of the column in [data] for which the plot will be created.
#' @param data Dataframe including column [P1]. Must also include column [Label] with b and s values.
#'
#' @return A ggplot object. This shows the joint distributions of [P1] given b and s respectively.
#' @import ggplot2
#' @import dplyr
#' @exportroxy
#'
plots_pairwise <- function(P1, data = higgs_data_na){
  mydiff <- function(data, diff){return(c(data, rep(NA, diff)))}
  P1_b <- as.vector(unlist(filter(data,Label == "b")[P1]))
  P1_s <- as.vector(unlist(filter(data, Label == "s")[P1]))
  difference = abs(length(P1_b)-length(P1_s))
  smaller_P1 <- min(length(P1_b),length(P1_s))
  if (smaller_P1 == length(P1_b)){
    P1_b <- mydiff(P1_b,difference)
  } else{
    P1_s <- mydiff(P1_s,difference)}
  #processing variables for plotting^^^^
  pl <- ggplot() +
    stat_bin_2d(bins = 70,aes(x=P1_b,y = P1_s))+
    theme_bw()
  return(pl)
}
