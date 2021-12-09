library(MDRClassifier)

test_that("classify sample with more than or equal to three categories (MDR)",{

  drug_resistance = DrugResistance
  sample_ID = "GFBCEDDN_00080"
  antimicrobial_agents = "Antimicrobial.Agent"

  MDRcategory <- classifyMDR(drug_resistance = drug_resistance,
                             sample_ID = sample_ID,
                             antimicrobial_agents = antimicrobial_agents)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "MDR")

})

test_that("classify sample with zero categories (S)",{

  drug_resistance = DrugResistance
  sample_ID = "JMFLPHLL_00316"
  antimicrobial_agents = "Antimicrobial.Agent"

  MDRcategory <- classifyMDR(drug_resistance = drug_resistance,
                             sample_ID = sample_ID,
                             antimicrobial_agents = antimicrobial_agents)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "S")

})

test_that("classify sample with number of categories equals to 1 or 2 (S)",{

  drug_resistance = DrugResistance
  sample_ID = "JMFLPHLL_00191"
  antimicrobial_agents = "Antimicrobial.Agent"

  MDRcategory <- classifyMDR(drug_resistance = drug_resistance,
                             sample_ID = sample_ID,
                             antimicrobial_agents = antimicrobial_agents)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "S")

})

test_that("classify sample with number of categories equals to 1 or 2 from RSI (R)",{

  RSI_table = RSI_table
  sample_ID = "PA1381"
  total_cat = 8

  MDRcategory <- classifyMDRfromRSI(RSI_table = RSI_table,
                             sample_ID = sample_ID,
                             total_cat = total_cat)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "R")

})

test_that("classify sample with number of categories more than 3 from RSI (MDR1)",{

  RSI_table = RSI_table
  sample_ID = "PA1387"
  total_cat = 8

  MDRcategory <- classifyMDRfromRSI(RSI_table = RSI_table,
                                    sample_ID = sample_ID,
                                    total_cat = total_cat)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "MDR1")

})

test_that("classify sample with number of categories less than 1 from RSI (S)",{

  RSI_table = RSI_table
  sample_ID = "PA1071"
  total_cat = 8

  MDRcategory <- classifyMDRfromRSI(RSI_table = RSI_table,
                                    sample_ID = sample_ID,
                                    total_cat = total_cat)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "S")

})
