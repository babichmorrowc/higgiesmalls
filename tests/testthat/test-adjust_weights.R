test_that("KaggleSet t weights are equal", {
  kaggle_adjust_weights_t <- adjust_weights(complete_data = higgs_data_orig,
                                            subset_data = higgs_data_orig[higgs_data_orig$KaggleSet == "t",],
                                            unadjusted_weight_col = "Weight",
                                            label_col = "Label")
  expect_equal(kaggle_adjust_weights_t, higgs_data_orig$KaggleWeight[higgs_data_orig$KaggleSet == "t"])
})
