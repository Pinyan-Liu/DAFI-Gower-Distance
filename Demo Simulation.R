################################################################################################################
##################################### Simulate Datasets First ##################################################

library(clustMixType)
library(mclust)
library(ggplot2)
library(reshape2)
library(dplyr)


generate_x_cont <- function(cluster_sizes, mean_vec, sd_vec) {
  if (length(cluster_sizes) != length(mean_vec) | 
      length(cluster_sizes) != length(sd_vec)) {
    stop("Wrong input length.")
  } else {
    unlist(lapply(seq_along(cluster_sizes), function(i) {
      rnorm(n = cluster_sizes[i], mean = mean_vec[i], sd = sd_vec[i])
    }))
  }
}
generate_x_cat <- function(cluster_sizes, cats_vec, prob_list) {
  if (length(cluster_sizes) != length(prob_list)) {
    stop("Wrong input length.")
  }
  prob_len <- unlist(lapply(prob_list, length))
  if (length(unique(prob_len)) > 1) stop("Probability unequal length.")
  if (length(cats_vec) != prob_len[1]) {
    stop("Probability length unmatched with categories.")
  } else {
    factor(unlist(lapply(seq_along(cluster_sizes), function(i) {
      sample(x = cats_vec, size = cluster_sizes[i], replace = TRUE, 
             prob = prob_list[[i]])
    })))
  }
}

## Simulated data C1
generate_data <- function(cluster_sizes = c(60, 60, 80),
                          x1_mean_se = list(mean = rep(0, 3), sd = rep(1, 3)),
                          x2_mean_se = list(mean = c(20, 22, 19), sd = rep(3, 3)),
                          x3_mean_se = list(mean = c(0, -1, 2), sd = rep(1, 3)),
                          x4_mean_se = list(mean = c(3, 4, 6), sd = rep(1, 3)),
                          x5_mean_se = list(mean = c(-4, -2, 4), sd = rep(4, 3)),
                          x6_cats = c(0,1,2,3,4,5), 
                          x6_prob = list(cl1 = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1),
                                         cl2 = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1),
                                         cl3 = c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)),
                          x7_cats = c(1, 2, 3, 4, 5, 6), 
                          x7_prob = list(cl1 = c(0.01, 0.02, 0.94, 0.01, 0.01, 0.01),
                                         cl2 = c(0.94, 0.01, 0.01, 0.02, 0.01, 0.01),
                                         cl3 = c(0.02, 0.01, 0.01, 0.95, 0.01, 0.01)),
                          x8_cats = c(1, 2, 3, 4, 5, 6), 
                          x8_prob = list(cl1 = c(0.05, 0.06, 0.8, 0.05, 0.01, 0.03),
                                         cl2 = c(0.8, 0.05, 0.05, 0.06, 0.03, 0.01),
                                         cl3 = c(0.01, 0.03, 0.05, 0.8, 0.05, 0.06)),
                          x9_cats = c(0, 1, 2, 3, 4, 5), 
                          x9_prob = list(cl1 = c(0.01, 0.05, 0.85, 0.02, 0.03, 0.04),
                                         cl2 = c(0.05, 0.02, 0.01, 0.03, 0.04, 0.85),
                                         cl3 = c(0.85, 0.05, 0.01, 0.04, 0.03, 0.02)),
                          x10_cats = c(1, 2, 3, 4, 5, 6), 
                          x10_prob = list(cl1 = c(0.05, 0.05, 0.7, 0.06, 0.04, 0.1),
                                          cl2 = c(0.7, 0.05, 0.05, 0.04, 0.1, 0.06),
                                          cl3 = c(0.06, 0.04, 0.1, 0.05, 0.05, 0.7))) {
  x1 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x1_mean_se$mean, sd_vec = x1_mean_se$sd)
  x2 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x2_mean_se$mean, sd_vec = x2_mean_se$sd)
  x3 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x3_mean_se$mean, sd_vec = x3_mean_se$sd)
  x4 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x4_mean_se$mean, sd_vec = x4_mean_se$sd)
  x5 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x5_mean_se$mean, sd_vec = x5_mean_se$sd)
  x6 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x6_cats, 
                       prob_list = x6_prob)
  x7 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x7_cats, 
                       prob_list = x7_prob)
  x8 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x8_cats, 
                       prob_list = x8_prob)
  x9 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x9_cats, 
                       prob_list = x9_prob)
  x10 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x10_cats, 
                        prob_list = x10_prob)
  true_c <- unlist(lapply(seq_along(cluster_sizes), function(i) {
    rep(i, cluster_sizes[i])
  }))
  data.frame(true_c = true_c, 
             x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
             x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10)
}
# set.seed(123456)
# dataB1 <- generate_data()


# Generate 500 datasets and store ARIs using lapply
num_datasets <- 500
set.seed(123456)
datasetsC1 <- lapply(1:num_datasets, function(i) generate_data())
# Export datasets for future use
saveRDS(datasetsC1, file = "/Users/pinyan/Desktop/R scipt/Simulation datasets + real-world(NHANES)/dat_list_simC1_20240116.RDS")



#####################################################################################################
#####################################################################################################

## Simulated data C2
generate_data <- function(cluster_sizes = c(60, 60, 80),
                          x1_mean_se = list(mean = rep(0, 3), sd = rep(1, 3)),
                          x2_mean_se = list(mean = rep(2, 3), sd = rep(1, 3)),
                          x3_mean_se = list(mean = c(0, -1, 2), sd = rep(1, 3)),
                          x4_mean_se = list(mean = c(3, 4, 6), sd = rep(1, 3)),
                          x5_mean_se = list(mean = c(-4, -2, 4), sd = rep(4, 3)),
                          x6_cats = c(2, 1, 0, 3, 4, 5), 
                          x6_prob = list(cl1 = c(0.16, 0.16, 0.16, 0.16, 0.16, 0.2),
                                         cl2 = c(0.2, 0.16, 0.16, 0.16, 0.16, 0.16),
                                         cl3 = c(0.16, 0.16, 0.16, 0.2, 0.16, 0.16)),
                          x7_cats = c(4, 5, 6, 7, 8, 9), 
                          x7_prob = list(cl1 = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1),
                                         cl2 = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1),
                                         cl3 = c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)),
                          x8_cats = c(1, 2, 3, 4, 5, 6), 
                          x8_prob = list(cl1 = c(0.05, 0.06, 0.8, 0.05, 0.01, 0.03),
                                         cl2 = c(0.8, 0.05, 0.05, 0.06, 0.03, 0.01),
                                         cl3 = c(0.01, 0.03, 0.05, 0.8, 0.05, 0.06)),
                          x9_cats = c(0, 1, 2, 3, 4, 5), 
                          x9_prob = list(cl1 = c(0.01, 0.05, 0.85, 0.02, 0.03, 0.04),
                                         cl2 = c(0.05, 0.02, 0.01, 0.03, 0.04, 0.85),
                                         cl3 = c(0.85, 0.05, 0.01, 0.04, 0.03, 0.02)),
                          x10_cats = c(1, 2, 3, 4, 5, 6), 
                          x10_prob = list(cl1 = c(0.05, 0.05, 0.7, 0.06, 0.04, 0.1),
                                          cl2 = c(0.7, 0.05, 0.05, 0.04, 0.1, 0.06),
                                          cl3 = c(0.06, 0.04, 0.1, 0.05, 0.05, 0.7))) {
  x1 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x1_mean_se$mean, sd_vec = x1_mean_se$sd)
  x2 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x2_mean_se$mean, sd_vec = x2_mean_se$sd)
  x3 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x3_mean_se$mean, sd_vec = x3_mean_se$sd)
  x4 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x4_mean_se$mean, sd_vec = x4_mean_se$sd)
  x5 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x5_mean_se$mean, sd_vec = x5_mean_se$sd)
  x6 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x6_cats, 
                       prob_list = x6_prob)
  x7 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x7_cats, 
                       prob_list = x7_prob)
  x8 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x8_cats, 
                       prob_list = x8_prob)
  x9 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x9_cats, 
                       prob_list = x9_prob)
  x10 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x10_cats, 
                        prob_list = x10_prob)
  true_c <- unlist(lapply(seq_along(cluster_sizes), function(i) {
    rep(i, cluster_sizes[i])
  }))
  data.frame(true_c = true_c, 
             x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
             x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10)
}


# Generate 500 datasets and store ARIs using lapply
num_datasets <- 500
set.seed(123456)
datasetsC2 <- lapply(1:num_datasets, function(i) generate_data())
# Export datasets for future use
saveRDS(datasetsC2, file = "/Users/pinyan/Desktop/R scipt/Simulation datasets + real-world(NHANES)/dat_list_simC2_20240116.RDS")


#####################################################################################################
#####################################################################################################

## Simulated data C3
generate_data <- function(cluster_sizes = c(60, 60, 80),
                          x1_mean_se = list(mean = rep(0, 3), sd = rep(1, 3)),
                          x2_mean_se = list(mean = rep(2, 3), sd = rep(1, 3)),
                          x3_mean_se = list(mean = rep(1, 3), sd = rep(1, 3)),
                          x4_mean_se = list(mean = c(3, 4, 6), sd = rep(1, 3)),
                          x5_mean_se = list(mean = c(-4, -2, 4), sd = rep(4, 3)),
                          x6_cats = c(2, 1, 0, 3, 4, 5), 
                          x6_prob = list(cl1 = c(0.16, 0.16, 0.16, 0.16, 0.16, 0.2),
                                         cl2 = c(0.2, 0.16, 0.16, 0.16, 0.16, 0.16),
                                         cl3 = c(0.16, 0.16, 0.16, 0.2, 0.16, 0.16)),
                          x7_cats = c(4, 5, 6, 7, 8, 9), 
                          x7_prob = list(cl1 = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1),
                                         cl2 = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1),
                                         cl3 = c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)),
                          x8_cats = c(1, 2, 3, 4, 5, 6), 
                          x8_prob = list(cl1 = c(0.15, 0.16, 0.17, 0.16, 0.18, 0.18),
                                         cl2 = c(0.16, 0.17, 0.16, 0.18, 0.18, 0.15),
                                         cl3 = c(0.17, 0.16, 0.18, 0.18, 0.15, 0.16)),
                          x9_cats = c(0, 1, 2, 3, 4, 5), 
                          x9_prob = list(cl1 = c(0.01, 0.05, 0.85, 0.02, 0.03, 0.04),
                                         cl2 = c(0.05, 0.02, 0.01, 0.03, 0.04, 0.85),
                                         cl3 = c(0.85, 0.05, 0.01, 0.04, 0.03, 0.02)),
                          x10_cats = c(1, 2, 3, 4, 5, 6), 
                          x10_prob = list(cl1 = c(0.05, 0.05, 0.7, 0.06, 0.04, 0.1),
                                          cl2 = c(0.7, 0.05, 0.05, 0.04, 0.1, 0.06),
                                          cl3 = c(0.06, 0.04, 0.1, 0.05, 0.05, 0.7))) {
  x1 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x1_mean_se$mean, sd_vec = x1_mean_se$sd)
  x2 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x2_mean_se$mean, sd_vec = x2_mean_se$sd)
  x3 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x3_mean_se$mean, sd_vec = x3_mean_se$sd)
  x4 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x4_mean_se$mean, sd_vec = x4_mean_se$sd)
  x5 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x5_mean_se$mean, sd_vec = x5_mean_se$sd)
  x6 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x6_cats, 
                       prob_list = x6_prob)
  x7 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x7_cats, 
                       prob_list = x7_prob)
  x8 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x8_cats, 
                       prob_list = x8_prob)
  x9 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x9_cats, 
                       prob_list = x9_prob)
  x10 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x10_cats, 
                        prob_list = x10_prob)
  true_c <- unlist(lapply(seq_along(cluster_sizes), function(i) {
    rep(i, cluster_sizes[i])
  }))
  data.frame(true_c = true_c, 
             x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
             x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10)
}


# Generate 500 datasets and store ARIs using lapply
num_datasets <- 500
set.seed(123456)
datasetsC3 <- lapply(1:num_datasets, function(i) generate_data())
# Export datasets for future use
saveRDS(datasetsC3, file = "/Users/pinyan/Desktop/R scipt/Simulation datasets + real-world(NHANES)/dat_list_simC3_20240116.RDS")


#####################################################################################################
#####################################################################################################

## Simulated data C4
generate_data <- function(cluster_sizes = c(60, 60, 80),
                          x1_mean_se = list(mean = rep(0, 3), sd = rep(1, 3)),
                          x2_mean_se = list(mean = rep(2, 3), sd = rep(1, 3)),
                          x3_mean_se = list(mean = rep(1, 3), sd = rep(1, 3)),
                          x4_mean_se = list(mean = rep(3, 3), sd = rep(1, 3)),
                          x5_mean_se = list(mean = c(-4, -2, 4), sd = rep(4, 3)),
                          x6_cats = c(2, 1, 0, 3, 4, 5), 
                          x6_prob = list(cl1 = c(0.16, 0.16, 0.16, 0.16, 0.16, 0.2),
                                         cl2 = c(0.2, 0.16, 0.16, 0.16, 0.16, 0.16),
                                         cl3 = c(0.16, 0.16, 0.16, 0.2, 0.16, 0.16)),
                          x7_cats = c(4, 5, 6, 7, 8, 9), 
                          x7_prob = list(cl1 = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1),
                                         cl2 = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1),
                                         cl3 = c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)),
                          x8_cats = c(1, 2, 3, 4, 5, 6), 
                          x8_prob = list(cl1 = c(0.15, 0.16, 0.17, 0.16, 0.18, 0.18),
                                         cl2 = c(0.16, 0.17, 0.16, 0.18, 0.18, 0.15),
                                         cl3 = c(0.17, 0.16, 0.18, 0.18, 0.15, 0.16)),
                          x9_cats = c(0, 1, 2, 3, 4 ,5), 
                          x9_prob = list(cl1 = c(0.2, 0.16, 0.17, 0.16, 0.17, 0.14),
                                         cl2 = c(0.16, 0.17, 0.16, 0.17, 0.14, 0.2),
                                         cl3 = c(0.17, 0.16, 0.17, 0.14, 0.2, 0.16)),
                          x10_cats = c(1, 2, 3, 4, 5, 6), 
                          x10_prob = list(cl1 = c(0.05, 0.05, 0.7, 0.06, 0.04, 0.1),
                                          cl2 = c(0.7, 0.05, 0.05, 0.04, 0.1, 0.06),
                                          cl3 = c(0.06, 0.04, 0.1, 0.05, 0.05, 0.7))) {
  x1 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x1_mean_se$mean, sd_vec = x1_mean_se$sd)
  x2 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x2_mean_se$mean, sd_vec = x2_mean_se$sd)
  x3 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x3_mean_se$mean, sd_vec = x3_mean_se$sd)
  x4 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x4_mean_se$mean, sd_vec = x4_mean_se$sd)
  x5 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x5_mean_se$mean, sd_vec = x5_mean_se$sd)
  x6 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x6_cats, 
                       prob_list = x6_prob)
  x7 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x7_cats, 
                       prob_list = x7_prob)
  x8 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x8_cats, 
                       prob_list = x8_prob)
  x9 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x9_cats, 
                       prob_list = x9_prob)
  x10 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x10_cats, 
                        prob_list = x10_prob)
  true_c <- unlist(lapply(seq_along(cluster_sizes), function(i) {
    rep(i, cluster_sizes[i])
  }))
  data.frame(true_c = true_c, 
             x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
             x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10)
}


# Generate 500 datasets and store ARIs using lapply
num_datasets <- 500
set.seed(123456)
datasetsC4 <- lapply(1:num_datasets, function(i) generate_data())
# Export datasets for future use
saveRDS(datasetsC4, file = "/Users/pinyan/Desktop/R scipt/Simulation datasets + real-world(NHANES)/dat_list_simC4_20240116.RDS")





#####################################################################################################
#####################################################################################################

## Simulated data C5
generate_data <- function(cluster_sizes = c(60, 60, 80),
                          x1_mean_se = list(mean = rep(0, 3), sd = rep(1, 3)),
                          x2_mean_se = list(mean = rep(2, 3), sd = rep(1, 3)),
                          x3_mean_se = list(mean = rep(1, 3), sd = rep(1, 3)),
                          x4_mean_se = list(mean = c(3, 4, 6), sd = rep(1, 3)),
                          x5_mean_se = list(mean = c(-4, -2, 4), sd = rep(4, 3)),
                          x6_cats = c(0,1,2,3,4,5), 
                          x6_prob = list(cl1 = c(0.2, 0.2, 0.2, 0.2, 0.1, 0.1),
                                         cl2 = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1),
                                         cl3 = c(0.2, 0.1, 0.2, 0.2, 0.2, 0.1)),
                          x7_cats = c(1, 2, 3, 4, 5, 6), 
                          x7_prob = list(cl1 = c(0.01, 0.02, 0.94, 0.01, 0.01, 0.01),
                                         cl2 = c(0.94, 0.01, 0.01, 0.02, 0.01, 0.01),
                                         cl3 = c(0.02, 0.01, 0.01, 0.95, 0.01, 0.01)),
                          x8_cats = c(1, 2, 3, 4, 5, 6), 
                          x8_prob = list(cl1 = c(0.05, 0.06, 0.8, 0.05, 0.01, 0.03),
                                         cl2 = c(0.8, 0.05, 0.05, 0.06, 0.03, 0.01),
                                         cl3 = c(0.01, 0.03, 0.05, 0.8, 0.05, 0.06)),
                          x9_cats = c(0, 1, 2, 3, 4, 5), 
                          x9_prob = list(cl1 = c(0.01, 0.05, 0.85, 0.02, 0.03, 0.04),
                                         cl2 = c(0.05, 0.02, 0.01, 0.03, 0.04, 0.85),
                                         cl3 = c(0.85, 0.05, 0.01, 0.04, 0.03, 0.02)),
                          x10_cats = c(1, 2, 3, 4, 5, 6), 
                          x10_prob = list(cl1 = c(0.05, 0.05, 0.7, 0.06, 0.04, 0.1),
                                          cl2 = c(0.7, 0.05, 0.05, 0.04, 0.1, 0.06),
                                          cl3 = c(0.06, 0.04, 0.1, 0.05, 0.05, 0.7))) {
  x1 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x1_mean_se$mean, sd_vec = x1_mean_se$sd)
  x2 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x2_mean_se$mean, sd_vec = x2_mean_se$sd)
  x3 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x3_mean_se$mean, sd_vec = x3_mean_se$sd)
  x4 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x4_mean_se$mean, sd_vec = x4_mean_se$sd)
  x5 <- generate_x_cont(cluster_sizes = cluster_sizes, 
                        mean_vec = x5_mean_se$mean, sd_vec = x5_mean_se$sd)
  x6 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x6_cats, 
                       prob_list = x6_prob)
  x7 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x7_cats, 
                       prob_list = x7_prob)
  x8 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x8_cats, 
                       prob_list = x8_prob)
  x9 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x9_cats, 
                       prob_list = x9_prob)
  x10 <- generate_x_cat(cluster_sizes = cluster_sizes, cats_vec = x10_cats, 
                        prob_list = x10_prob)
  true_c <- unlist(lapply(seq_along(cluster_sizes), function(i) {
    rep(i, cluster_sizes[i])
  }))
  data.frame(true_c = true_c, 
             x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
             x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10)
}


# Generate 500 datasets and store ARIs using lapply
num_datasets <- 500
set.seed(123456)
datasetsC5 <- lapply(1:num_datasets, function(i) generate_data())
# Export datasets for future use
saveRDS(datasetsC5, file = "/Users/pinyan/Desktop/R scipt/Simulation datasets + real-world(NHANES)/dat_list_simC5_20240116.RDS")


################################################################################################################
##################################### Data Analysis of DAFI-Gower ##############################################
library(clustMixType)
library(mclust)
library(ggplot2)
library(reshape2)
library(dplyr)
library(infotheo)
library(cluster)
library(factoextra)


# Import list of datasets (simulation 5)
dat_list_simC5 <- readRDS('dat_list_simC5_20240116.RDS')
## Scaling/ Standardizing the numeric value in the data
# Min-Max Normalization
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

dat_list_simC5_norm <- lapply(dat_list_simC5, function(data) {
  data %>% dplyr::mutate(across(where(is.numeric),normalize))
})


# Normalized MI + Self adjustment +  modified Gower
source("self_adaptive_distance.R")
source("gower.dist.modify.R")
source("normalized_MI.R")

# Initialize a list to store feature importance
feature_importance_list <- list()

set.seed(123456)
ari_relevance_Wgt_results <- unlist(lapply(seq_along(dat_list_simC5_norm), function(i) {
  print(i)
  data <- dat_list_simC5_norm[[i]]
  data_without_label <- data[,-1]
  continuous_feature <- data[,2:6]
  categorical_feature <- data[,7:11]
  
  # Get feature importance
  feature_importance1 = self_adaptive_distance(continuous_feature=continuous_feature, categorical_feature=categorical_feature)
  feature_importance2 = normalized_MI(continuous_feature=continuous_feature, categorical_feature=categorical_feature)
  feature_importance <- feature_importance1*feature_importance2
  feature_importance <- feature_importance/sum(feature_importance)
  # Store the importance weights
  feature_importance_list[[i]] <<- feature_importance
  
  dist_matrix <- gower.dist.modify(data_without_label, var.weights = feature_importance, robcb = "iqr")   
  # Apply PAM clustering
  pam_res <- pam(dist_matrix, k = 3)  
  adjustedRandIndex(pam_res$clustering, data$true_c)
}))


# Compute median, 25th and 75th percentiles of ARIs
mean_ari <- mean(ari_relevance_Wgt_results)
median_ari <- median(ari_relevance_Wgt_results)
percentile_25 <- quantile(ari_relevance_Wgt_results, 0.25)
percentile_75 <- quantile(ari_relevance_Wgt_results, 0.75)
cat("Mean ARI:", mean_ari, "\n")
cat("Median ARI:", median_ari, "\n")
cat("25th Percentile ARI:", percentile_25, "\n")
cat("75th Percentile ARI:", percentile_75, "\n")


# Plot the average feature importance across all 500 data in the list
average_importances <- Reduce("+", feature_importance_list) / length(feature_importance_list)
average_importances








