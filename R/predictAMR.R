#12 Nov 2021
#' Predict AMR from PCA.
#'
#' A function that predict multi-drug resistance by principle component analysis (PCA)
#' to show the new sample's in the original cluster.
#'
#' @param dataframe A dataframe with rownames representing the sample ID and each column
#'                  is one of sample's antimicrobial features. If the value equals to 1,
#'                  the sample has this antimicrobial feature. If the value equals to 0,
#'                  the sample does not have this antimicrobial feature.
#' @param new_data The new data of dataframe must contain columns (variables) with the same
#'                 names and in the same order as the data used to compute PCA.
#' @return Returns the PCA result of the new sample.
#'
#' @examples
#'Example 3
#'Using pca_data available with package
#'dim(pca_data)
#'resultsExample3 <- predictAMR(pca_data[c(1:17), c(2,9,16)], pca_data[c(18:20), c(2,9,16)])
#'resultsExample3
#'
#' @references
#'
#' @export
#' @import factoextra


predictAMR <- function(dataframe, new_data){

  #checking arguments

  #dataframe.active <- dataframe
  #calculation
  data.pca <- prcomp(dataframe, scale = TRUE)
  #visualize eigenvalues
  #fivz_eig(data.pca)
  #get eigenvalue
  eig_val <- get_eigenvalue(data.pca)

  #result for individuals
  data_ind <-get_pca_ind(data.pca)
  ind_coordinates <- data_ind$coord
  ind_contribution <- data_ind$contrib
  ind_quality_rep <- data_ind$cos2

  #result for variables
  data_var <- get_pca_var(data.pca)
  var_coorinates <- data_var$coord
  var_contribution <- data_var$contrib
  var_quaility_rep <- data_var$cos2
  #Biplot of individuals and variables
  fviz_pca_biplot(data.pca, repel = TRUE,
                  col.var = "#2E9FDF", # Variables color
                  col.ind = "#696969"  # Individuals color
                   )
  #predict new_data
  new_ind_coord <- predict(data.pca, newdata = new_data)
  return(new_ind_coord)
}



#' Plot PCA.
#'
#' A function that generate PCA and dimension reduction plots.
#'
#' @param dataframe A dataframe with rownames representing the sample ID and each column
#'                  is one of sample's antimicrobial features. If the value equals to 1,
#'                  the sample has this antimicrobial feature. If the value equals to 0,
#'                  the sample does not have this antimicrobial feature.
#' @param new_data The new data of dataframe must contain columns (variables) with the same
#'                 names and in the same order as the data used to compute PCA.
#' @param plot_type The type of plot need to be generated. For individual PCA plot,
#'                  using "individual". For visualizing relationship between variables, using
#'                  "variables". For predict new data and its relationship with old data, using
#'                  "predict".
#' @return Returns the PCA plot.
#'
#' @examples
#'Example 4
#'Using pca_data available with package
#'dim(pca_data)
#'resultsExample4 <- plotPCA(pca_data[c(2:10), c(2,9)], pca_data[c(11:13), c(2,9)])
#'resultsExample4
#'
#' @references
#'
#' @export
#' @import factoextra
#'
plotPCA <- function(dataframe, new_data){
  #plot of active individuals
  data.pca <- prcomp(dataframe, scale = TRUE)
   p <- fviz_pca_ind(data.pca, repel = TRUE)


     # Add new supplementary individuals
     new_ind_coord <- predictAMR(dataframe, new_data)
     return(fviz_add(p, new_ind_coord, color = "blue"))
   }


#[END]


#devtools::load_all()
#devtools::check()
#devtools::document
