library(foreign)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggtern)

source("reef_functions.R")
source("reef_error.R")

coral_grids <- read.dbf("global_5m.dbf", as.is = T)

# classes to ignore
# ignore_classes <- c(paste0("geo_", 0:10), paste0("geo_", 17:20), paste0("geo_", 25:26),
#                     paste0("ben_", 0:10), paste0("ben_", 19:20))

#ignore_classes <- c("geo_20") ## is there enough 25 to include?? What class to include in? Reef flat?

################
################

# TODO: DONT FORGET TO CHANGE NULL EEZ to 'international waters' or 'outside EEZ'
coral_grids$UNION[is.na(coral_grids$UNION)] <- "Outside EEZ"

# TODO: remove (choose first) duplicated grid IDs that have multiple distance matches (choose 1st for each ID)
#       - dealt with via group() + n()

###############
###############

# summarise variables of interest

coral_grid_processed <- coral_grids %>%
  #select(-ignore_classes) %>%
  group_by(id) %>%
  mutate(id_n = n()) %>% # so we can retain all rows for hte shapefile, but filter out dupes
  ungroup() %>%
  rowwise() %>%
  mutate(
    extent_sum = sum_na(reef_exten),
    geo_sum = sum_na(c_across(starts_with("geo_"))),
    pageo_sum = sum_na(c_across(starts_with("pageo_"))),
    ben_sum = sum_na(c_across(starts_with("ben_"))),
    paben_sum = sum_na(c_across(starts_with("paben_"))),
    reef_top = sum_na(c(geo_13, geo_14, geo_15)), ## should patch reef be in here?
    reef_slope = sum_na(c(geo_21, geo_22)),
    coral_realestate = sum_na(c(geo_13, geo_14, geo_15, geo_21, geo_22, geo_24, geo_25)),
    coral_habitat = sum_na(c(ben_13, ben_15)),
    pacoral_realestate = sum_na(c(pageo_13, pageo_14, pageo_15, pageo_21, pageo_22, pageo_24, pageo_25)),
    pacoral_habitat = sum_na(c(paben_13, paben_15)),
    flat_back = sum_na(c(geo_13, geo_14, geo_24)),
    ben_entropy = shannon_entropy(c_across(starts_with("ben_")))
  ) %>%
  ungroup() %>%
  mutate(
    pop_slo = reef_slope * 25,
    pop_fl_ba = flat_back * 15,
    pop_crest = geo_15 * 20,
    #total_pop = sum_na(c_across(slope_pop, flat_back_pop, crest_pop)),
    pop_low = ben_15 * 10,
    pop_high = ben_15 * 60,
    pop_med = ben_15 * 25,
    prot_geo = pageo_sum / geo_sum,
    prot_coralhab = pacoral_habitat / coral_habitat
  )

# back to .dbf
###############
#write.dbf(as.data.frame(coral_grid_processed), file = "global_5m_processed.dbf")
###############


## Stats/analysis

coral_grid_analysis <- coral_grid_processed %>% 
  filter(id_n == 1) %>%
  select(-top, -bottom, -left, -right, -id, -id_n) %>%
  filter(geo_sum > 0)



### total's statements
# totals
sum_na(coral_grid_analysis$extent_sum) / (1000*1000)
sum_na(coral_grid_analysis$geo_sum) / (1000*1000)

# increase in mapped areas vs. WCMC
sum_na(coral_grid_analysis$geo_sum) / sum_na(coral_grid_analysis$wcmc)
sum_na(coral_grid_analysis$coral_realestate) / sum_na(coral_grid_analysis$wcmc)

# total protected
sum_na(coral_grid_analysis$pageo_sum) / sum_na(coral_grid_analysis$geo_sum)
# total coral habitat protected
sum_na(coral_grid_analysis$pacoral_habitat) / sum_na(coral_grid_analysis$coral_habitat)
# total under threat protected
sum_na(coral_grid_analysis$pageo_sum[coral_grid_analysis$score_mean < -0.1]) / sum_na(coral_grid_analysis$geo_sum[coral_grid_analysis$score_mean < -0.1])
sum_na(coral_grid_analysis$pacoral_habitat[coral_grid_analysis$score_mean < -0.1]) / sum_na(coral_grid_analysis$coral_habitat[coral_grid_analysis$score_mean < -0.1])

# class totals
class_totals <- coral_grid_analysis %>%
  select(starts_with("geo_") | starts_with("ben_")) %>%
  summarise_all(sum_na) %>%
  t() %>% as.data.frame() %>%
  mutate(area = V1 / 1000000,
         class = rownames(.)) %>%
  select(area, class) %>%
  mutate(geo_perc = area/.$area[which(.$class == "geo_sum")],
         ben_perc = area/.$area[which(.$class == "ben_sum")])


# global seagrass
c(
  class_totals$area[class_totals$class=="ben_14"], #67236.4
  67236.4 - (67236.4 * mean(ben_region_stats_full$SG_c_05)),
  67236.4 + (67236.4 * mean(ben_region_stats_full$SG_o_05))
)


### figure out ref surface area based on generalised multipliers
# geo multipliers from Roelfsema et al 2021
geo_mults = data.frame(class = c(11,12,13,14,15,16,21,22,23,24,25),
                       multiplier = c(1.435252,2.023705,1.452008,2.139408,2.072318,2.139408,3.542818,4.115758,1.137139,2.183803,1.452008))

geo_class_totals <- class_totals %>%
  filter(grepl("geo_", class, fixed = T)) %>%
  filter(class != 'geo_sum') %>%
  mutate(class = as.integer(gsub("geo_", "", class, fixed = T))) %>%
  left_join(geo_mults, by = "class") %>%
  mutate(surface_area = area * multiplier)

sum_na(geo_class_totals$surface_area)


# coral habitat (CA + RO)
c(
  class_totals$area[class_totals$class=="ben_15"] + class_totals$area[class_totals$class=="ben_13"],
  49556.97 - (49556.97 * mean(ben_region_stats_full$CA_c_05)) + 31990.6 - (31990.6 * mean(ben_region_stats_full$RO_c_05)),
  49556.97 + (49556.97 * mean(ben_region_stats_full$CA_o_05)) + 31990.6 + (31990.6 * mean(ben_region_stats_full$RO_o_05))
)


# plot vs. WCMC
plot(log(coral_realestate) ~ log(wcmc), data = coral_grid_analysis)
# amount reef within distance to major land mass
land_dist = 0.1
100 * (coral_grid_analysis %>% filter(distance < land_dist) %>% summarise(sum = sum_na(geo_sum)) / coral_grid_analysis %>% summarise(sum = sum_na(geo_sum)))
100 * (coral_grid_analysis %>% filter(distance < land_dist) %>% summarise(sum = sum_na(coral_habitat)) / coral_grid_analysis %>% summarise(sum = sum_na(coral_habitat)))
# geomorphic based popultions
round(c(sum(coral_grid_analysis$pop_slo), sum(coral_grid_analysis$pop_fl_ba), sum_na(coral_grid_analysis$pop_crest)) / 1000000000)
# benthic based
round(c(sum_na(coral_grid_analysis$pop_low), sum_na(coral_grid_analysis$pop_med), sum_na(coral_grid_analysis$pop_high)) / 1000000000)
# benthic based under threat
round(c(sum_na(coral_grid_analysis$pop_low[coral_grid_analysis$score_mean < -0.1]), 
        sum_na(coral_grid_analysis$pop_med[coral_grid_analysis$score_mean < -0.1]), 
        sum_na(coral_grid_analysis$pop_high[coral_grid_analysis$score_mean < -0.1]))
      / 1000000000)





### THREAT LEVELS
# threat_plot_dat <- coral_grid_analysis %>%
#   select(score_mean, geo_sum, coral_habitat) %>%
#   pivot_longer(c(geo_sum, coral_habitat)) %>%
#   mutate(value = value/1000000)
# 
# threat_plot_dat$name <- factor(threat_plot_dat$name, levels = c("geo_sum", "coral_habitat"))

threat_plot_dat <- coral_grid_analysis %>%
  select(score_mean, coral_habitat, distance) %>%
  mutate(coral_habitat = coral_habitat/1000000)

ggplot(data = threat_plot_dat, aes(x = distance, y = coral_habitat)) +
  geom_point(aes(colour = score_mean), alpha = 0.5, shape = 16, stroke = 0) +
  scale_colour_continuous(type = "viridis")




### COUNTRY SUMMARIES
coral_country_summary <- coral_grid_analysis %>%
  group_by(UNION) %>%
  summarise(extent_sum = sum(extent_sum) / 1000000,
            geo_sum = sum(geo_sum) / 1000000,
            ben_sum = sum(ben_sum) / 1000000,
            realestate_sum = sum(coral_realestate) / 1000000,
            coralhab_sum = sum(coral_habitat) / 1000000,
            coralalg_sum = sum_na(ben_15) / 1000000,
            seagrass = sum_na(ben_14) / 1000000,
            wcmc_sum = sum(wcmc) / 1000000,
            mean_dist = mean(distance),
            mean_score = mean(score_mean, na.rm = T),
            wcmc_diff = geo_sum / wcmc_sum,
            perc_prot = sum(pageo_sum) / 1000000 / geo_sum
            ) %>%
  mutate(perc_prot = replace(perc_prot, is.infinite(perc_prot), 0))


# Bahamas seagrass CIs
c(
  coral_country_summary$seagrass[coral_country_summary$UNION == "Bahamas"], #26371.01
  26371.01 - (26371.01 * mean(ben_region_stats_full$SG_c_05)),
  26371.01 + (26371.01 * mean(ben_region_stats_full$SG_o_05))
)

# countries
coral_country_summary[coral_country_summary$UNION == "Micronesia",]
# top n %
sum(arrange(coral_country_summary, desc(geo_sum))$wcmc_sum[1:20]) / 
  sum(arrange(coral_country_summary, desc(geo_sum))$wcmc_sum)
# amount reef compared to ocean area (%)
sum(coral_country_summary$geo_sum) / (361900000) * 100
# amount reef compared to land area (%)
sum(coral_country_summary$geo_sum) / (148940000) * 100
# plot vs. WCMC
plot(log(realestate_sum) ~ log(wcmc_sum), data = coral_country_summary)
# distance plot
plot(geo_sum ~ mean_dist, data = coral_country_summary)

# country protected areas
top_protectors = arrange(coral_country_summary, desc(perc_prot), desc(geo_sum))


### Tables ###

# Table 1
table1_countries <- arrange(coral_country_summary, desc(coralhab_sum))$UNION[1:20]
table1_regions <- c('indo','gbrt','phil',
                    'nth_cari', 'pns','swp',
                    'rsga','swp','south_asia',
                    'pns', 'eastern_micronesia','eaf',
                    'nth_cari','cio','nth_cari',
                    'rsga','eaf','south_asia',
                    'central_south_pacific','eaf')

coralhab_ci("Indonesia", "indo", coral_country_summary, ben_region_stats_full)
table1_cis <- bind_rows(map2(table1_countries, table1_regions, coralhab_ci, coral_country_summary, ben_region_stats_full))

country_area_table1 <- coral_country_summary %>%
  filter(UNION %in% table1_countries) %>%
  select(UNION, extent_sum, geo_sum, coralhab_sum) %>%
  mutate(across(where(is.numeric), round)) %>%
  arrange(desc(coralhab_sum)) %>%
  left_join(table1_cis, by = c("UNION" = "country"))
write.csv(country_area_table1, file = "table1.csv", row.names = F)

# extended data table 1
country_area_edtable <- coral_country_summary %>%
  select(UNION, extent_sum, geo_sum, coralhab_sum, wcmc_sum, mean_dist, perc_prot) %>%
  mutate(across(c(geo_sum, coralhab_sum, wcmc_sum), round)) %>%
  arrange(desc(coralhab_sum))
write.csv(country_area_edtable, file = "supptable1.csv", row.names = F)  


### plots ###

# plot by longitude
coral_longitude_plot <- coral_grid_processed %>% 
  filter(id_n == 1) %>%
  select(coral_habitat, left) %>%
  mutate(longitude = ceiling(left)) %>%
  select(-left) %>%
  group_by(longitude) %>%
  summarise(reef = sum(coral_habitat) / 1000000000)

ggplot(coral_longitude_plot, aes(x = longitude, y = reef)) +
  geom_line() +
  theme_void()
  #theme_classic() +
  #scale_x_continuous(breaks = seq(-180,180,45)) +
  #scale_y_continuous(breaks = seq(0,16,8)) +
  #xlab("Longitude") + ylab("Shallow reef area")
  


## plot ordered by WCMC
focus_countries <- arrange(coral_country_summary, desc(coralhab_sum))$UNION[1:200]

country_area_plotdat <- coral_country_summary %>%
  filter(UNION %in% focus_countries) %>%
  #filter(!UNION %in% c("Bahamas", "Cuba")) %>%
  select(-mean_dist, -seagrass) %>%
  select(-ben_sum, -coralalg_sum, -realestate_sum) %>%
  #arrange(desc(wcmc_sum)) %>%
  pivot_longer(geo_sum:wcmc_sum)

country_area_plotdat$UNION <- factor(country_area_plotdat$UNION, 
                                     levels = focus_countries) # get in order for x axis
country_area_plotdat$name <- factor(country_area_plotdat$name, 
                                    levels = c("wcmc_sum","geo_sum","coralhab_sum")) # get in order for plot areas
country_area_plotdat$value <- country_area_plotdat$value # convert to km2

ggplot(data = country_area_plotdat, aes(x = UNION)) +
  geom_bar(aes(y = value, group = name, fill = name),
           stat = "identity", width=.5, position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(text = element_text(size=20)) +
  ggtitle("Coral reef area estimates - ordered by WCMCv4 estimate") +
  xlab("Jurisdiction") + ylab("Shallow coral reef area (km2)") +
  labs(fill = "Area (km^2)") +
  scale_fill_manual(labels = c("WCMCv4 spatial layer", "Shallow coral reef", "Coral habitat"),
                    values = c("gray", "orange", "blue")) + 
  theme(legend.position = c(0.8, 0.8))




## Plot reef area vs. distance ( by country)
country_dist <- coral_country_summary %>%
  select(UNION, mean_dist, coralhab_sum, geo_sum, perc_prot, wcmc_sum, wcmc_diff) %>%
  mutate(mean_dist = mean_dist * 111,
         country_label = UNION)
country_dist$country_label[!country_dist$country_label %in% focus_countries] <- "" 

ggplot(data = country_dist, aes(x = mean_dist, y = coralhab_sum, label = country_label)) +
  geom_point(shape = 16, size = 5, stroke = 0) +
  theme_classic() + 
  theme(text = element_text(size=20)) +
  geom_text(size = 6, hjust=-0.3, vjust=-0.1) +
  ylab("Shallow coral habitat (km2)") + xlab("Distance to major landmass (approx. km)") +
  ggtitle("Global coral reef remoteness remoteness")

## Plot reef area vs. distance (by grid)
ggplot(data = coral_grid_analysis, aes(x = distance, y = coral_habitat)) +
  geom_point(shape = 16, size = 1, stroke = 0, alpha = 0.3) +
  theme_classic() + 
  ylab("Shallow coral habitat (km2)") + xlab("Distance to major landmass (approx. km)") +
  ggtitle("Global coral reef remoteness remoteness")

## Plot reef area vs. percentage protected
ggplot(data = country_dist, aes(x = perc_prot, y = geo_sum, label = country_label)) +
  geom_point(shape = 16, size = 5, stroke = 0) +
  theme_classic() + 
  theme(text = element_text(size=20)) +
  geom_text(size = 6, hjust=-0.3, vjust=-0.1) +
  ylab("Shallow coral reef (km2)") + xlab("Percentage protected by active MPA") +
  ggtitle("Shallow coral reef protected status by country")

## plot showing area mapped comparison
country_dist$country_label[country_dist$wcmc_diff < 2 & country_dist$wcmc_diff > 0.5] <- "" 
ggplot(data = country_dist, aes(x = wcmc_sum, y = geo_sum, label = country_label)) +
  geom_point(shape = 16, size = 5, stroke = 0) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() + 
  theme(text = element_text(size=16)) +
  geom_text(size = 3, hjust=-0.3, vjust=-0.1) +
  scale_y_log10() + scale_x_log10() +
  ylab("Shallow coral reef area (log10 km2)") + xlab("WCMCv4 spatial layer (log10 km2)") +
  ggtitle("Shallow coral reef area mapped vs. existing WCMCv4 area")


## plot showing remoteness, area, threat
country_dist_threat <- coral_country_summary %>%
  select(UNION, mean_dist, coralhab_sum, mean_score, wcmc_sum, geo_sum, wcmc_diff) %>%
  mutate(mean_dist = mean_dist * 111,
         country_label = UNION)

country_dist_threat$country_label[!country_dist_threat$country_label %in% focus_countries] <- "" 
ggplot(data = country_dist_threat, aes(x = mean_dist, y = coralhab_sum, label = country_label)) +
  geom_point(aes(colour = mean_score), shape = 16, size = 5, stroke = 0) +
  scale_colour_continuous(type = 'viridis') +
  theme_classic() + 
  theme(text = element_text(size=20)) +
  geom_text(size = 6, hjust=-0.3, vjust=-0.1) +
  ylab("Coral habitat (km2)") + xlab("Distance to major landmass (approx. km)") +
  ggtitle("Coral habitat area, remoteness and threat level by country")

# plus the inset comparisons
benthic_summary <- coral_grid_analysis %>%
  group_by(UNION) %>%
  summarise(Sand = sum_na(ben_11) / 1000000,
            Rubble = sum_na(ben_12) / 1000000,
            Rock = sum_na(ben_13) / 1000000,
            `Coral Algae` = sum_na(ben_15) / 1000000,
            `Microalgal Mats` = sum_na(ben_18) / 1000000,
            Seagrass = sum_na(ben_14) / 1000000)

benthic_summary %>%
  filter(UNION == "Micronesia") %>%
  pivot_longer(-UNION, names_to = "benthic_class") %>%
  ggplot(aes(x = "", y = value, fill = benthic_class)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(fill = "Benthic composition") +
    scale_fill_manual(values = c("#ff6161","#9bcc4f","#b19c3a","#e0d05e","#ffffbe","#668438"))

benthic_summary %>%
  filter(UNION == "Bahamas") %>%
  pivot_longer(-UNION, names_to = "benthic_class") %>%
  ggplot(aes(x = "", y = value, fill = benthic_class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void()

## ternary plots

ternary_countries <- c(arrange(coral_country_summary, desc(coralhab_sum))$UNION[1:20], "Brazil", "Sri Lanka", "Mayotte")

ternary_data <- coral_grid_analysis %>%
  filter(UNION %in% ternary_countries) %>%
  mutate(rubble = ben_12,
         seagrass = ben_14,
         coral_habitat = ben_13 + ben_15,
         sand = ben_11,
         reef_area = geo_sum,
         label = UNION) %>%
  select(rubble,seagrass,coral_habitat,sand,reef_area,label) %>%
  group_by(label) %>%
  summarise(rubble = sum_na(rubble), seagrass = sum_na(seagrass), coral_habitat = sum_na(coral_habitat), sand = sum_na(sand), reef_area = sum_na(reef_area)/1000000)
ggtern(data = ternary_data, aes(x = rubble, y = coral_habitat, z = sand)) +
  theme_rgbw() +
  geom_point(aes(size = reef_area)) +
  theme_showgrid_minor() +
  scale_size(name = "") +
  labs(xarrow = "", x = "",
       yarrow = "", y = "",
       zarrow = "", z = "") +
  #scale_T_continuous() + scale_R_continuous() + scale_R_continuous() +
  geom_text(aes(label = label), hjust=0,vjust=0, size = 2, check_overlap = F)

Mayotte#ternary_country = "Australia"
ternary_data_grids <- coral_grid_analysis %>%
  filter(UNION %in% ternary_countries) %>%
  mutate(x = ben_12,
         y = ben_13 + ben_15,
         z = ben_11,
         reef_area = geo_sum,
         label = UNION) %>%
  select(x,y,z,reef_area,label)
ggtern(data = ternary_data_grids, aes(x = x, y = y, z = z)) +
  theme_rgbw() +
  geom_point(alpha = 0.5, stroke = 0) +
  labs(xarrow = "Rubble", x = "",
       yarrow = "Coral/Algae + Rock", y = "",
       zarrow = "Sand", z = "") +
  facet_wrap(~label, ncol = 4)
  
  
  
  