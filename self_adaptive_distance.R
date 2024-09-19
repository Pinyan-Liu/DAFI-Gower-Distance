# Function to get the adjusting factor w

self_adaptive_distance <- function(continuous_feature, categorical_feature) {
  vector_con_distance <- rep(0, ncol(continuous_feature))
  vector_cat_distance <- rep(0, ncol(categorical_feature))
  
  for (i in 1:nrow(continuous_feature)) {
    vector_con_distance = vector_con_distance + colSums(abs(matrix(rep(t(continuous_feature[i,]),nrow(continuous_feature)+1-i), ncol=ncol(continuous_feature), byrow=TRUE)-
                                                              continuous_feature[i:nrow(continuous_feature),]))
  }
  vector_con_distance_quantile <- rep(0, ncol(continuous_feature))
  for (i in 1:ncol(continuous_feature)) {
    vector_con_distance_quantile[i] = (vector_con_distance[i] / (quantile(continuous_feature[,i], 0.75) - quantile(continuous_feature[,i], 0.25)))
  }
  
  for (i in 1:nrow(categorical_feature)) {
    vector_cat_distance = vector_cat_distance + colSums(matrix(1, nrow(categorical_feature)-i+1, ncol(categorical_feature))-as.numeric(matrix(rep(t(categorical_feature[i,]),nrow(categorical_feature)+1-i), ncol=ncol(categorical_feature), byrow=TRUE) ==
                                                                                                                                         categorical_feature[i:nrow(categorical_feature),]))
  }
  
  vector_cat_level <- rep(0, ncol(categorical_feature))
  for (i in 1:ncol(categorical_feature)) {
    vector_cat_level[i] = length(levels(categorical_feature[,i]))
  }
  
  vector_cat_distance = vector_cat_distance / vector_cat_level * 2
  weight_cat <- rep(mean(vector_con_distance_quantile), ncol(categorical_feature)) / vector_cat_distance
  
  feature_importance <- c(rep(1,length(continuous_feature)),weight_cat)
  
  return(feature_importance)
}
