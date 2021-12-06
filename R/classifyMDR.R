#12 Nov 2021
#' Classify the multi-drug resistance
#'
#' A function that classify the multi-drug resistance given
#' the sample ID and drug resistance information from RGI prediction.
#'
#' @param drug_resistance A list with each key representing the sample ID and its
#'                        value is the string of the drugs that the sample resistant.
#' @param sample_ID The string of sample ID that used for classifying multi-drug resistance.
#' @param antimicrobial_agents The string of the column name specifying sample's antimicrobial agents.
#'
#' @return Returns the category of the given sample's multi-drug resistance.
#'
#' @examples
#' # Example 1
#' # Using DrugResistance dataset available with package
#' dim(DrugResistance)
#' colnames(DrugResistance)
#' resultsExample1 <- classifyMDR(drug_resistance = DrugResistance,
#'                                sample_ID = "GFBCEDDN_00939",
#'                                antimicrobial_agents = "Antimicrobial.Agent")
#' # To obtain value from results
#' resultsExample1
#'
#' @references
#'
#' @export
#' @import hash
classifyMDR <- function(drug_resistance, sample_ID, antimicrobial_agents){

  #checking arguments
  if(!sample_ID %in% rownames(drug_resistance)) stop("The sample ID must have a corresponding row in the dataset.")
  if(!antimicrobial_agents %in% colnames(drug_resistance)) stop("The dataset must have a column for sample's antimicrobial agents.")
  #construct a hash for MDR reference
  MDR_table <- hash()
  MDR_table[['Aminoglycosides']] <- list('gentamicin C')
  MDR_table[['Ansamycins']] <- list('rifampin')
  MDR_table[['Anti-MRSA cephalosporins']] <- list('ceftaroline')
  MDR_table[['Cephamycins']] <- list(a='oxacillin', b='cefoxitin')
  MDR_table[['Fluoroquinolones']] <- list(a='ciprofloxacin', b='moxifloxacin')
  MDR_table[['Folate pathway inhibitors']] <- list('trimethoprim')
  MDR_table[['Fucidanes']] <- list('fusidic acid')
  MDR_table[['Glycopeptides']] <- list(a='vancomycin', b='teicoplanin', c='telavancin')
  MDR_table[['Glycylcyclines']] <- list('tigecycline')
  MDR_table[['Lincosamides']] <- list('clindamycin')
  MDR_table[['Lipopeptides']] <- list('daptomycin')
  MDR_table[['Macrolides']] <- list('erythromycin')
  MDR_table[['Oxazolidinones']] <- list('linezolid')
  MDR_table[['Phenicols']] <- list('chloramphenicol')
  MDR_table[['Phosphonic acids']] <- list('fosfomycin')
  MDR_table[['Streptogramins']] <- list('quinupristin')
  MDR_table[['Tetracyclines']] <- list(a='tetracycline', b='doxycycline', c='minocycline')
  #print(values(MDR_table, keys='Tetracyclines'))
  #print('tetracycline' %in% values(MDR_table, keys='Tetracyclines'))
  #Classify the MDR according to sample's antimicrobial agents and categories
  agents <- drug_resistance[sample_ID, antimicrobial_agents]
  #check type of agents is string
  if (is.character(agents)  & length(agents) == 1) {
    lst <- as.list(strsplit(agents, split=',')[[1]])
    #print(lst)
    count_category = 0
    category_lst <- list()
    for (agent in lst){
      #print(agent)
      for (key in keys(MDR_table)){
        if ((agent %in% values(MDR_table, keys=key)) & !(key %in% category_lst)){
          category_lst <- c(category_lst, key)
          count_category <- count_category + 1
        }
      }
    }
    #print(count_category)
    if(count_category >= 3){
      result <- 'MDR'
    }
    else if (1 <= count_category & count_category <= 2){
      result <- 'XDR'
    }
    else if (count_category == 17) {result <- 'PDR'}
    else{result <- 'NULL'} #No antimicrobial agent tested
  }
  else{stop("The data type of antimicrobial agents are not string.")}

  return(result)
}



#' Classify the multi-drug resistance for multiple samples
#'
#' A function that classify the multi-drug resistances of all samples in the csv file
#' given the all drug resistances information from RGI prediction.
#'
#' @param drug_resistance A list with each row representing each
#'                        sample IDs and one of the columns is the string of
#'                        drugs that the sample resistant.
#' @param antimicrobial_agents The string of the column name specifying sample's antimicrobial agents.
#'
#' @return Returns a dataframe with each row name representing the sample ID and its
#'         column representing the MDR category.
#'
#' @examples
#' # Example 2
#' # Using DrugResistance dataset available with package
#' dim(DrugResistance)
#'
#' #classify the category of all samples' multi-drug resistance
#' resultsExample2 <- classifyAllMDR(drug_resistance = DrugResistance,
#'                                antimicrobial_agents = 'Antimicrobial.Agent')
#' # To obtain value from results
#' resultsExample2
#'
#' @references
#'
#' @export
classifyAllMDR <- function(drug_resistance, antimicrobial_agents){
  Sample_ID <- ""
  Category <- ""
  #print(rownames(drug_resistance))
  for (sample in rownames(drug_resistance)){
    #print(sample)
    Sample_ID <- c(Sample_ID, sample)
    category <- classifyMDR(drug_resistance, sample, antimicrobial_agents)
    Category <- c(Category, category)

    #result[[sample]] <- category
  }
  df <- data.frame(Sample_ID, Category)
  return(df)
}




#'Classify the multi-drug resistance from RSI table
#'
#' A function that classify the multi-drug resistance from RSI table.
#'
#' @param RSI_table A list with each row representing the sample ID and each column
#'                         is the abbreviation of antimicrobial.
#'                         If the isolate is resistant to the agent, 'R' is in the cell.
#'                         Otherwise, 'S' is in the cell.
#'
#' @param sample_ID The string of sample ID that used for classifying multi-drug resistance.
#'
#' @param total_cat The integer number of total antimicrobial category of species.
#'
#'
#' @return Returns the category of the given sample's multi-drug resistance.
#'
#'
#' # Example 5
#' # Using RSI_table dataset available with package
#' dim(RSI_table)
#' colnames(RSI_table)
#' resultsExample5 <- classifyMDRfromRSI(RSI_table = RSI_table,
#'                                sample_ID = "PA1381", total_cat = 8)
#' # To obtain value from results
#' resultsExample5
#'
#' @references
#'
#' @export
#' @import hash
classifyMDRfromRSI <- function(RSI_table, sample_ID, total_cat){

  #construct a hash for MDR reference
  abb_table <- hash()
  abb_table[['Aminoglycosides']] <- list(a='GEN', b='AMK', c='TOB', d='NET')
  abb_table[['Antipseudomonal fluoroquinolones']] <- list(a='CIP', b='LEV')
  abb_table[['Polymyxins']] <- list(a='COL', b='PMB')
  abb_table[['Monobactams']] <- list(a='ATM', b='FEP')
  abb_table[['Antipseudomonal cephalosporins']] <- list(a='CAZ', b='CEF')
  abb_table[['Antipseudomonalcarbapenems']] <- list(a='MEM', b='IPM', c='DOR')
  abb_table[['Antipseudomonal penicillins + b-lactamase inhibitors']] <- list(a='TCC', b='TZP')
  abb_table[['Phosphonic acids']] <- list('FOS')

   count_category <- countCategory(RSI_table, sample_ID)
   count_max = 0
   lst <- list()
   for (agent in colnames(RSI_table)){
     for (key in keys(abb_table)){
       if ((agent %in% values(abb_table, keys=key)) & !(key %in% lst)){
         lst <- c(lst, key)
         count_max = count_max + 1
       }
     }
   }
    #print(count_category)
   if(count_max ==  total_cat){ #complete mode
     if(count_category >= 3){
        result <- 'MDR'
      }
      else if (3 < count_category & count_category <= total_cat-2){
        result <- 'XDR'
      }
      else if (count_category == total_cat) {result <- 'PDR'}
      else{result <- 'S'} #Susceptible to all antimicrobial agents
   }
   else if(count_max < total_cat){ #partial mode
     if(1 <= count_category &&  count_category < 3){
       result <- 'R'
     }
     else if(3 == count_category){
       result <- 'MDR'
     }
     else if (3 < count_category & count_category < count_max){
       result <- paste('MDR',as.character(count_category-3), sep='')
     }
     else if (count_category == count_max) {result <- 'PDR'} #A candidate for real PDR
     else{result <- 'S'} #Susceptible to all antimicrobial agents
   }
   else # (count_max > total_cat)
     {stop("The total number of antimicrobial categoties from ECDC criteria is not correct.")}


  return(result)
}


#'
#'helper function
#' count <- countCategory(RSI_table = RSI_table, sample_ID = "PA1381")
#' count
#'
countCategory <- function(RSI_table, sample_ID){

  #checking arguments
  if(!sample_ID %in% rownames(RSI_table)) stop("The sample ID must have a corresponding row in the dataset.")

  #construct a hash for MDR reference
  abb_table <- hash()
  abb_table[['Aminoglycosides']] <- list(a='GEN', b='AMK', c='TOB', d='NET')
  abb_table[['Antipseudomonal fluoroquinolones']] <- list(a='CIP', b='LEV')
  abb_table[['Polymyxins']] <- list(a='COL', b='PMB')
  abb_table[['Monobactams']] <- list(a='ATM', b='FEP')
  abb_table[['Antipseudomonal cephalosporins']] <- list(a='CAZ', b='CEF')
  abb_table[['Antipseudomonalcarbapenems']] <- list(a='MEM', b='IPM', c='DOR')
  abb_table[['Antipseudomonal penicillins + b-lactamase inhibitors']] <- list(a='TCC', b='TZP')
  abb_table[['Phosphonic acids']] <- list('FOS')
  #obtain the entire row of the isolate
  agents <- RSI_table[sample_ID,]
  #check type of agents is string

  count_category = 0
  category_lst <- list()
  for (abb in colnames(RSI_table)){
    #print(agent)
    for (key in keys(abb_table)){
      if ((abb %in% values(abb_table, keys=key)) & !(key %in% category_lst) & RSI_table[sample_ID, abb] == 'R'){
        category_lst <- c(category_lst, key)
        count_category <- count_category + 1
      }
    }
  }
  return(count_category)
}



#' Classify the multi-drug resistance for multiple samples
#'
#' A function that classify the multi-drug resistances of all samples in the csv file
#' given the all drug resistances information from RGI prediction. The number of antimicrobial
#' agents tested determines which version of the multi-drug resistance category to use.
#' If all antimicrobial agents are tested and given,then use the criteria given by ECDC.
#' Otherwise, we use more specific criteria by subdividing ECDC criteria to generate reasonable MDR categories.
#'
#' @param RSI_table A list with each row representing each
#'                        sample IDs and one of the columns is the string of
#'                        drugs that the sample resistant.
#'
#' @param total_cat The integer number of total antimicrobial category of species.

#'             mode is set to "complete" by default.
#'             If all antimicrobial agents are tested and given,then use "complete" mode.
#'             Otherwise, use "partial" mode to generate reasonable MDR categories.
#'
#' @return Returns a dataframe with each row name representing the sample ID and its
#'         column representing the MDR category.
#'
#' @examples
#' # Example 6
#' # Using DrugResistance dataset available with package
#'
#' #classify the category of all samples' multi-drug resistance
#' resultsExample6 <- classifyAllMDRfromRSI(RSI_table = RSI_table, total_cat = 8)
#' # To obtain value from results
#' resultsExample6
#'
#' @references
#'
#' @export
classifyAllMDRfromRSI <- function(RSI_table, total_cat){
  #print(rownames(drug_resistance))
  Sample_ID <- c()
  Category <- c()
  for (sample in rownames(RSI_table)){
    #print(sample)
    Sample_ID <- c(Sample_ID, sample)
    category <- classifyMDRfromRSI(RSI_table, sample, total_cat)
    Category <- c(Category, category)

    #result[[sample]] <- category
  }
  df <- data.frame(Sample_ID, Category)
  return(df)
}


#[END]


#devtools::load_all()
#devtools::document()
#devtools::check()


