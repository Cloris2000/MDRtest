context("classifyMDR")
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

test_that("classify sample with zero categories (NULL)",{

  drug_resistance = DrugResistance
  sample_ID = "JMFLPHLL_00316"
  antimicrobial_agents = "Antimicrobial.Agent"

  MDRcategory <- classifyMDR(drug_resistance = drug_resistance,
                             sample_ID = sample_ID,
                             antimicrobial_agents = antimicrobial_agents)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "NULL")

})

test_that("classify sample with number of categories equals to 1 or 2 (XDR)",{

  drug_resistance = DrugResistance
  sample_ID = "JMFLPHLL_00191"
  antimicrobial_agents = "Antimicrobial.Agent"

  MDRcategory <- classifyMDR(drug_resistance = drug_resistance,
                             sample_ID = sample_ID,
                             antimicrobial_agents = antimicrobial_agents)
  expect_type(MDRcategory, "character")
  expect_identical(MDRcategory, "XDR")

})
