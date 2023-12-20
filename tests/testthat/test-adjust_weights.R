test_that("KaggleSet t weights are equal", {
  higgs_data_orig <- read.csv(test_path("testdata", "atlas-higgs-challenge-test-data.csv"))
  kaggle_adjust_weights_t <- adjust_weights(complete_data = higgs_data_orig,
                                            subset_data = higgs_data_orig[higgs_data_orig$KaggleSet == "t",],
                                            unadjusted_weight_col = "Weight",
                                            label_col = "Label")
  expect_equal(kaggle_adjust_weights_t, higgs_data_orig$KaggleWeight[higgs_data_orig$KaggleSet == "t"])
})

test_that("KaggleSet b weights are equal", {
  higgs_data_orig <- read.csv(test_path("testdata", "atlas-higgs-challenge-test-data.csv"))
  kaggle_adjust_weights_b <- adjust_weights(complete_data = higgs_data_orig,
                                             subset_data = higgs_data_orig[higgs_data_orig$KaggleSet == "b",],
                                             unadjusted_weight_col = "Weight",
                                             label_col = "Label")
  expect_equal(kaggle_adjust_weights_b, higgs_data_orig$KaggleWeight[higgs_data_orig$KaggleSet == "b"])
})
