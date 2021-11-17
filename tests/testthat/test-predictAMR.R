context("predictAMR")
library(MDRClassifier)

test_that("precit the pca of samples for the sample at row 18 of PC1",{

  dataframe = pca_data[c(1:17), c(2,9,16)]
  new_data = pca_data[c(18:20), c(2,9,16)]

  pca_result <- predictAMR(dataframe = dataframe,
                          new_data = new_data)
  expect_type(pca_result, "double")
  result <- round(pca_result[1,1], digits = 2)
  expect_identical(result, 1.19)

})

test_that("precit the pca of samples for sample at row 19 of PC1",{

  dataframe = pca_data[c(1:17), c(2,9,16)]
  new_data = pca_data[c(18:20), c(2,9,16)]

  pca_result <- predictAMR(dataframe = dataframe,
                           new_data = new_data)
  expect_type(pca_result, "double")
  result <- round(pca_result[2,1], digits = 2)
  expect_identical(result, 2.35)

})
