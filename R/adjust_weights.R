#' Calculate adjusted weights for a subset of the data. See description at http://opendata.cern.ch/record/329.
#'
#' @param complete_data The complete dataframe. Must contain a weight column (see `unadjusted_weight_col`) and a label column (see `label_col`)
#' @param subset_data The subset of `complete_data` for which you want to compute adjusted weights.
#' @param unadjusted_weight_col The name of the column containing the unadjusted weight values.
#' @param label_col The name of the column containing the label values (signal vs. background). This column must contain "s" for signal and "b" for background.
#' @return A vector of adjusted weights for the observations in `subset_data`
#' @export
#' @examples
#' # Calculate adjusted weights for a subset of data used in the Kaggle challenge:
#' kaggle_adjust_weights_t <- adjust_weights(complete_data = higgs_data_orig,
#' subset_data = higgs_data_orig[higgs_data_orig$KaggleSet == "t",],
#' unadjusted_weight_col = "Weight",
#' label_col = "Label")

adjust_weights <- function(complete_data, subset_data, unadjusted_weight_col = "Weight", label_col = "Label") {
  # Get total values from complete dataset
  total_signal_weights <- sum(complete_data[[unadjusted_weight_col]][complete_data[[label_col]] == "s"])
  total_background_weights <- sum(complete_data[[unadjusted_weight_col]][complete_data[[label_col]] == "b"])
  # Get total values from subset dataset
  subset_signal_weights <- sum(subset_data[[unadjusted_weight_col]][subset_data[[label_col]] == "s"])
  subset_background_weights <- sum(subset_data[[unadjusted_weight_col]][subset_data[[label_col]] == "b"])

  adjusted_weights <- subset_data[[unadjusted_weight_col]] *
    case_when(subset_data[[label_col]] == "s" ~ total_signal_weights / subset_signal_weights,
              subset_data[[label_col]] == "b" ~ total_background_weights / subset_background_weights)
  return(adjusted_weights)

}
