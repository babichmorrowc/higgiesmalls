test_that("Predicting all background gives AMS 0", {
  higgs_data_orig <- read.csv(test_path("testdata", "atlas-higgs-challenge-test-data.csv"))
  predictions <- rep("b", nrow(higgs_data_orig))
  labels <- higgs_data_orig$Label
  weights <- higgs_data_orig$Weight
  ams_all_background <- approx_median_sig(predictions, labels, weights)
  expect_equal(ams_all_background, 0)
})
