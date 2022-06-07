# function to sum with NAs
sum_na <- function(x) sum(x,na.rm = T)

#### accuracy functions
get_conf_mat <- function(mapped, reference) {
  table(mapped, reference)
}

dim_check <- function(x, len = 2) { # DANGER - hard coded to 2 classes, use len = if error matrix is different size
  dim(x)[1] != len | dim(x)[2] != len
}

percentage_agreement <- function(conf_mat, len = 2) {
  # sum(as.character(data[true_id[[reps]], "veg_class"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
  if(dim_check(conf_mat, len)) {return(NA)}
  sum(diag(conf_mat)) / sum(conf_mat) # xtab method quicker?
}

sampled_accuracies_geo <- function(mapped, reference) {
  sample_data <- data.frame(mapped, reference) %>% #, strat = factor(mapped)) %>%
    #group_by(strat) %>%
    sample_frac(0.66, replace = F) %>% # this is the monte carlo
    rbind(data.frame(mapped = c(11,12,13,14,15,16,21,22,23,24), reference = c(11,12,13,14,15,16,21,22,23,24)))
  conf_mat <- get_conf_mat(sample_data$mapped, sample_data$reference)
  data.frame(perc_agr = percentage_agreement(conf_mat, 10),
             SL_c = conf_mat[1,1] / sum(conf_mat[1,]),
             DL_c = conf_mat[2,2] / sum(conf_mat[2,]),
             IRF_c = conf_mat[3,3] / sum(conf_mat[3,]),
             ORF_c = conf_mat[4,4] / sum(conf_mat[4,]),
             RC_c = conf_mat[5,5] / sum(conf_mat[5,]),
             TRF_c = conf_mat[6,6] / sum(conf_mat[6,]),
             SRS_c = conf_mat[7,7] / sum(conf_mat[7,]),
             RS_c = conf_mat[8,8] / sum(conf_mat[8,]),
             PL_c = conf_mat[9,9] / sum(conf_mat[9,]),
             BRS_c = conf_mat[10,10] / sum(conf_mat[10,]),
             SL_o = conf_mat[1,1] / sum(conf_mat[,1]),
             DL_o = conf_mat[2,2] / sum(conf_mat[,2]),
             IRF_o = conf_mat[3,3] / sum(conf_mat[,3]),
             ORF_o = conf_mat[4,4] / sum(conf_mat[,4]),
             RC_o = conf_mat[5,5] / sum(conf_mat[,5]),
             TRF_o = conf_mat[6,6] / sum(conf_mat[,6]),
             SRS_o = conf_mat[7,7] / sum(conf_mat[,7]),
             RS_o = conf_mat[8,8] / sum(conf_mat[,8]),
             PL_o = conf_mat[9,9] / sum(conf_mat[,9]),
             BRS_o = conf_mat[10,10] / sum(conf_mat[,10]))
}

sampled_accuracies_ben <- function(mapped, reference) {
  sample_data <- data.frame(mapped, reference) %>% #, strat = factor(mapped)) %>%
    #group_by(strat) %>%
    sample_frac(0.66, replace = F) %>% # this is the monte carlo
    rbind(data.frame(mapped = c(11,12,13,14,15,18), reference = c(11,12,13,14,15,18)))
  conf_mat <- get_conf_mat(sample_data$mapped, sample_data$reference)
  data.frame(perc_agr = percentage_agreement(conf_mat, 6),
             SA_c = conf_mat[1,1] / sum(conf_mat[1,]),
             RU_c = conf_mat[2,2] / sum(conf_mat[2,]),
             RO_c = conf_mat[3,3] / sum(conf_mat[3,]),
             SG_c = conf_mat[4,4] / sum(conf_mat[4,]),
             CA_c = conf_mat[5,5] / sum(conf_mat[5,]),
             BMA_c = conf_mat[6,6] / sum(conf_mat[6,]),
             SA_o = conf_mat[1,1] / sum(conf_mat[,1]),
             RU_o = conf_mat[2,2] / sum(conf_mat[,2]),
             RO_o = conf_mat[3,3] / sum(conf_mat[,3]),
             SG_o = conf_mat[4,4] / sum(conf_mat[,4]),
             CA_o = conf_mat[5,5] / sum(conf_mat[,5]),
             BMA_o = conf_mat[6,6] / sum(conf_mat[,6]))
}

get_geo_regions <- function(map_region, data, nboot = 1000) {
  region_data <- filter(data, region == map_region) %>%
    select(class_num, classification)
  region_distribution <- bind_rows(replicate(
    n = nboot,
    expr = {sampled_accuracies_geo(region_data$classification, region_data$class_num)},
    simplify = F))
  data.frame(
    region = map_region,
    lapply(region_distribution, mean),
    lapply(region_distribution, quantile, 0.025),
    lapply(region_distribution, quantile, 0.975),
    lapply(region_distribution, quantile, 0.05)
  )
}

get_ben_regions <- function(map_region, data, nboot = 1000) {
  region_data <- filter(data, region == map_region) %>%
    select(class_num, classification)
  region_distribution <- bind_rows(replicate(
    n = nboot,
    expr = {sampled_accuracies_ben(region_data$classification, region_data$class_num)},
    simplify = F))
  data.frame(
    region = map_region,
    lapply(region_distribution, mean),
    lapply(region_distribution, quantile, 0.025),
    lapply(region_distribution, quantile, 0.975),
    lapply(region_distribution, quantile, 0.05)
  )
}

make_error <- function(x) {
  1 - x
}

coralhab_ci <- function(country, region, country_totals, errors) {
  coralhab <- country_totals$coralhab_sum[country_totals$UNION==country]
  mean_o <- mean(c(errors$CA_o_05[errors$region == region], errors$RO_o_05[errors$region == region]))
  mean_c <- mean(c(errors$CA_c_05[errors$region == region], errors$RO_c_05[errors$region == region]))
  data.frame(country = country, coralhab = round(coralhab),
             lower = round(coralhab - (coralhab * mean_c)),
             upper = round(coralhab + (coralhab * mean_o)))
}
