library(dplyr)
library(tidyr)
library(ggplot2)

source("reef_functions.R")

geo_full <- read.csv("global_geo_val_5m.csv", stringsAsFactors = F) %>%
  select(class_num, classification, region)
geo_full$class_num[geo_full$class_num != 2] <- 1
geo_full$classification[geo_full$classification != 2] <- 1
geo_full <- rbind(geo_full, data.frame(region = 'dummy', class_num = 1, classification = 2))
table(geo_full$classification, geo_full$class_num)

geo_cases <- read.csv("global_geo_val_5m.csv", stringsAsFactors = F) %>%
  filter(class_num != 25, classification != 25, class_num != 2)

benthic_cases <- read.csv("global_benthic_val_5m.csv", stringsAsFactors = F) %>%
  filter(class_num != 16, class_num != 17)


#########################################################
### Accuracy measures, intervals and area multipliers ###
#########################################################
# Accuracy values are the mean of the sampling dsitribution
# Accuracy 95% intervals are the 2.5% - 97.5% interval of the sampling dsitribution
# Area lower limit is the area minus the 95% comission error (*_c_025)
# Area upper limit is the area plus the 95% omission error (*_c_025)
#########################################################
#########################################################


#### GEOMORPHIC
# geo_region_stats_full <- bind_rows(
#   lapply(unique(geo_cases$region), get_geo_regions, geo_cases, 1000)
# ) %>%
#   rowwise() %>%
#   mutate(across(contains("_o"), make_error)) %>%
#   mutate(across(contains("_c"), make_error))
# 
# rownames(geo_region_stats_full) <- NULL
# geo_stats_names <- names(geo_region_stats_full)
# geo_stats_names <- gsub(".1", "_025", geo_stats_names, fixed = T)
# geo_stats_names <- gsub(".2", "_975", geo_stats_names, fixed = T)
# geo_stats_names <- gsub(".3", "_05", geo_stats_names, fixed = T)
# names(geo_region_stats_full) <- geo_stats_names
# 
# saveRDS(geo_region_stats_full, file = "geo_region_stats_full.rds")
geo_region_stats_full <- readRDS("geo_region_stats_full.rds")

# plotting
geo_region_stats_pretty <- pivot_longer(geo_region_stats_full, perc_agr:BRS_o_975)

ggplot(filter(geo_region_stats_pretty, grepl("perc_agr$", name, fixed = F)),
       aes(y = value, x = region)) +
  geom_point()


#### BENTHIC
# ben_region_stats_full <- bind_rows(
#   lapply(unique(benthic_cases$region), get_ben_regions, benthic_cases, 1000)
# )  %>%
#   rowwise() %>%
#   mutate(across(contains("_o"), make_error)) %>%
#   mutate(across(contains("_c"), make_error))
# 
# rownames(ben_region_stats_full) <- NULL
# ben_stats_names <- names(ben_region_stats_full)
# ben_stats_names <- gsub(".1", "_025", ben_stats_names, fixed = T)
# ben_stats_names <- gsub(".2", "_975", ben_stats_names, fixed = T)
# ben_stats_names <- gsub(".3", "_05", ben_stats_names, fixed = T)
# names(ben_region_stats_full) <- ben_stats_names
# 
# saveRDS(ben_region_stats_full, file = "ben_region_stats_full.rds")
ben_region_stats_full <- readRDS("ben_region_stats_full.rds")

ben_region_stats_pretty <- pivot_longer(ben_region_stats_full, perc_agr:BMA_o_975)

ggplot(filter(ben_region_stats_pretty, grepl("perc_agr$", name, fixed = F)),
       aes(y = value, x = region)) +
  geom_point()


### EXPORT CSVs for PAPER SUPP MAT 

write.csv(ben_region_stats_full, file = "benthic_error_full.csv")
write.csv(geo_region_stats_full, file = "geomorphic_error_full.csv")



