library(MDRClassifier)

test_that("running correctly", {

  x <- data.frame(Sample_ID = c("1","2","3","4","5","6"),
                  Category = c("MDR", "PDR", "NULL", "XDR", "PDR", "NULL"))
  result <- MDRPlot(x)

  expect_type(result, "double")
  expect_length(result, 3)
})


test_that("error with invalid user input", {


  x <- data.frame(Sample_ID = c("1","2","3","4","5","6"),
                  category = c("MDR", "PDR", "NULL", "XDR", "PDR", "NULL")
                  )
  expect_error(result <- MDRPlot(x))

  #missing column
  x <- data.frame(Sample_ID = c("1","2","3","4","5","6"))
  expect_error(result <- MDRPlot(x))

  #no right column
  x <- data.frame(id = c("1","2","3","4","5","6"),
                  cat = c("MDR", "PDR", "NULL", "XDR", "PDR", "NULL"))
  expect_error(result <- MDRPlot(x))

})
