#' Calculate the approximate median significance for a given prediction.
#'
#' @param predictions Vector of predicted signal vs. background values. Should contain "s" for signal and "b" for background.
#' @param labels Vector of true labels of signal vs. background values. Should contain "s" for signal and "b" for background.
#' @param weights Vector of weights for each observation. Note that if you are predicting for a subset of the data, you should be using `adjust_weights` to calculate the adjusted weight values.
#' @param b_reg Regularisation parameter as described here: http://opendata.cern.ch/record/329. The default value determined by CERN is 10.
#'
#' @return The value of the approximate median significance (AMS) of the prediction. AMS is used to measure the success of a classifier.
#' @export
#'
#' @examples
#' \dontrun{
#' # Test AMS if we predicted all background
#' predictions <- rep("b", nrow(higgs_data_orig))
#' labels <- higgs_data_orig$Label
#' weights <- higgs_data_orig$Weight
#' approx_mean_sig(predictions, labels, weights) # give us 0
#' }

approx_median_sig <- function(predictions, labels, weights, b_reg = 10) {
  true_pos <- sum(weights[predictions == "s" & labels == "s"])
  false_pos <- sum(weights[predictions == "s" & labels == "b"])
  ams <- sqrt(2 * ((true_pos + false_pos + b_reg) * log(1 + (true_pos / (false_pos + b_reg))) - true_pos))
  return(ams)
}
