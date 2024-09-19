# Function to calculate normalized mutual information

normalized_MI <- function(continuous_feature, categorical_feature) {
  
  # Discretize continuous features for importance calculation
  numbers_of_bins = 4
  for (i in 1:ncol(continuous_feature)) {
    continuous_feature[, i] = cut(continuous_feature[, i], 
                                  breaks = unique(quantile(continuous_feature[, i],probs=seq.int(0,1, by=1/numbers_of_bins))), 
                                  include.lowest=TRUE)
    levels(continuous_feature[, i]) <- c(0, 1, 2, 3)
  }
  feature_all <- cbind(continuous_feature, categorical_feature)
  
  # calculate mutual information
  mutual_information_vector <- rep(0, ncol(feature_all))
  for (i in 1:ncol(feature_all)) {
    feature_all_but_i <- feature_all[,-i]
    feature_i <- feature_all[,i]
    weight_i <- 0
    for (j in 1:ncol(feature_all_but_i)){
      weight_i = weight_i + aricode::NMI(feature_i,feature_all_but_i[,j],variant="max")
    }
    weight_i <- weight_i / ncol(feature_all_but_i)
    mutual_information_vector[i] <- weight_i
  }
  feature_importance <- mutual_information_vector
  return(feature_importance)
}


