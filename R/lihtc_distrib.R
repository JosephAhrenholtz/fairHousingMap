
# manual check: passed
# last year comparison:


library(scales)


# STEP 1: LOAD AND REDUCE LIHTC DEVELS
year <- 2024
filepaths()

# load and reduce geocoded projects
locations <- st_read(pres_db_location, quiet = T) %>%
  st_transform(crs = 4326)
locations <- locations %>%
  select('id' = 'Universal', 'proj_name' = 'Name', 'address_clean' = 'Clean.Addr', 'city' = 'City', 'county_name' = 'County', 'zipcode' = 'Zip',
         'lowinc_units' = 'Affordable', 'active_programs' = 'Active.Pro', geometry) %>%
  # filter to only projects with LIHTC funding
  filter(grepl('LIHTC', active_programs))


# load additional lihtc attributes
details <- read.xlsx(pres_db_attributes)
details <- details %>%
  select('id' = 'Universal.ID', 'proj_name' = 'Name', 'address_clean' = 'Clean.Address', 'lihtc_award_year' = 'LIHTC.Award.Year', 'credit_type' = 'LIHTC.Tax.Credit.Type',
         'construct_type' = 'LIHTC.Construction.Type', 'population_served' = 'Population.Served', 'placed_in_service' = 'PIS?', 'active_programs' = 'Active.Program(s)') %>%
  # filter to only projects with LIHTC funding
  filter(grepl('LIHTC', active_programs)) %>%
  select(-active_programs)


# id at times is duplicated over multiple phases of the project so join by id, project name, and address
devels <- locations %>%
  left_join(details, by = c('id', 'proj_name', 'address_clean'))
# reduce by additonal params
devels_red <- devels %>%
  filter(grepl('new construction', construct_type, ignore.case=TRUE) &
           grepl('family|families', population_served, ignore.case=TRUE) &
           # cut to 2015 or after, will later cut to 2017 or after for 4% projects only
           lihtc_award_year >= 2015) # 416 projects that meet the criteria.  2 projects are funded by 9% and 4% so those will be counted in both credit types
rm(devels, locations, details)



# STEP 2A: JOIN TO OPP NEIGHBS
if (year == 2024){
  # load
  load('data/final_2024.rda')
  opp <- final_2024
  rm(final_2024)
}

# reduce
opp <- opp %>%
  select(fips, fips_bg, region, oppcat, pov_seg_flag)

# add opp cat distribution
opp <- opp %>%
  mutate(n_geo = n()) %>%
  group_by(oppcat) %>%
  mutate(pct_neighbcat = n()/n_geo) %>%
  ungroup()

# add share hps
opp <- opp %>%
  mutate(pct_hps = sum(pov_seg_flag, na.rm = T)/n_geo) %>%
  select(-n_geo)

# spatial join devels to opp cats
opp_devels <- opp %>%
  sf::st_join(devels_red)

# return to df and reduce to tracts with devels
opp_devels <- opp_devels %>%
  filter(!is.na(id)) %>%
  st_drop_geometry() # 416 minus 3 projects that had no geometry (no building number).  413 now in total.


# STEP 2B: JOIN TO SEG NEIGHBS
# load seg cat and make spatial
segcat <- read_zip(segcat, year = year, type = 'csv')
segcat <- segcat %>% select('fips' = 'geoid', segcat)
tract_2010 <- tracts(state = "CA", year = 2010, cb = T)
tract_2010 <- tract_2010 %>% unite(fips, c('STATE', 'COUNTY', 'TRACT'), sep = '') %>% select(fips, geometry)
segcat <- segcat %>% left_join(tract_2010, by = 'fips') %>% st_as_sf(crs = 4326)

# summarize segcat distribution
segcat <- segcat %>%
  mutate(n_geo = n()) %>%
  group_by(segcat) %>%
  mutate(pct_neighbcat = n()/n_geo) %>%
  ungroup() %>%
  select(-n_geo)

# spatial join devels to seg cats
segcat_devels <- segcat %>%
  sf::st_join(devels_red)

# return to df and reduce
segcat_devels <- segcat_devels %>%
  filter(!is.na(id)) %>%
  st_drop_geometry()

# write intermediate files
write_csv(opp_devels, paste0('data/intermediate/', year, '/opp_devels.csv'))
write_csv(segcat_devels, paste0('data/intermediate/', year, '/segcat_devels.csv'))



# STEP 3: WRANGLE DATA FOR PLOTTING
# load and factor
opp_devels <- read_csv(paste0("data/intermediate/", year, "/opp_devels.csv")) %>% rename('neighbcat' = oppcat)
segcat_devels <- read_csv(paste0("data/intermediate/", year, "/segcat_devels.csv")) %>% rename('neighbcat' = segcat)

opp_devels$neighbcat[which(is.na(opp_devels$neighbcat))] <- 'Insufficient Data'
opp_levels <- c('Insufficient Data', 'Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')
opp_devels$neighbcat <- factor(opp_devels$neighbcat, levels = opp_levels)

segcat_levels <- c('High POC Segregation', 'High White Segregation', 'Low-Medium Segregation', 'Racially Integrated')
segcat_devels$neighbcat <- factor(segcat_devels$neighbcat, levels = segcat_levels)

# separate neighb distribution
opp_neighbs <- opp_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)
hps_neighbs <- opp_devels %>% select('pct_neighbcat' = 'pct_hps') %>% distinct(pct_neighbcat) %>% mutate(neighbcat = 'High-Poverty & Segregated*')
opp_neighbs <- bind_rows(hps_neighbs, opp_neighbs)
opp_neighbs$neighbcat <- factor(opp_neighbs$neighbcat, c('High-Poverty & Segregated*', opp_levels))

seg_neighbs <- segcat_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)


# Separate 9% opp
opp_nine <- opp_devels %>% filter(grepl('9', credit_type)) %>%
  mutate(timeframe = ifelse(lihtc_award_year < 2019, 'Pre-incentive', 'Post-incentive'))
hps_nine <- opp_nine %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
hps_nine$neighbcat <- factor(hps_nine$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

# Separate 9% seg
seg_nine <- segcat_devels %>% filter(grepl('9', credit_type)) %>%
  mutate(timeframe = ifelse(lihtc_award_year < 2019, 'Pre-incentive', 'Post-incentive'))


# separate 4% opp
opp_four <- opp_devels %>% filter(grepl('4', credit_type), lihtc_award_year >= 2017) %>%
  mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))
hps_four <- opp_four %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
hps_four$neighbcat <- factor(hps_four$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

# Separate 4% seg
seg_four <- segcat_devels %>% filter(grepl('4', credit_type), lihtc_award_year >= 2017) %>%
  mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))

# move data frames to a list
opp_nine_list <- list(hps_nine, opp_nine)
seg_nine_list <- list(seg_nine)
opp_four_list <- list(hps_four, opp_four)
seg_four_list <- list(seg_four)

# summarize
summ <- function(x) x %>%
  group_by(timeframe) %>%
  mutate(tot_lowinc_units = sum(lowinc_units)) %>%
  group_by(neighbcat, timeframe) %>%
  reframe(
    pct_lowinc_units = sum(lowinc_units, na.rm = TRUE)/tot_lowinc_units) %>%
  distinct(neighbcat, timeframe, .keep_all = T) %>%
  complete(neighbcat, timeframe, fill = list(pct_lowinc_units = 0)) %>%
  ungroup()

final_opp_nine <- bind_rows(lapply(opp_nine_list, summ)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
final_seg_nine <- bind_rows(lapply(seg_nine_list, summ)) %>% left_join(seg_neighbs, by = 'neighbcat')
final_opp_four <- bind_rows(lapply(opp_four_list, summ)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
final_seg_four <- bind_rows(lapply(seg_four_list, summ)) %>% left_join(seg_neighbs, by = 'neighbcat')


# STEP FOUR: PLOT

# plot function
plot_func <- function(df, title, subtitle, caption){

  # lengthen and factor
  df <- df %>% pivot_longer(c(-neighbcat, -timeframe), names_to = "name", values_to = "value")
  df$timeframe[df$name == "pct_neighbcat"] <- "Neighborhoods"
  df$timeframe <- factor(df$timeframe, levels = c('Neighborhoods', 'Pre-incentive', 'Post-incentive'))

  df %>%
    ggplot(aes(x = neighbcat, y = value, fill = timeframe, label=scales::percent(value, accuracy = 1))) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(labels = c("% Neighborhoods", "% Pre-incentive units", "% Post-incentive units"), values = c("#999999", "#E69F00", "#43a2ca")) +
    guides(fill = guide_legend(title = NULL)) +
    geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 2.5, fontface = "bold") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = "", y = "") +
    scale_x_discrete(labels = label_wrap(14)) +
    theme(plot.caption = element_text(face = "italic"),
          legend.position = c(0.83, 0.86),
          legend.background = element_rect(fill = "white", color = "black")) +
    plot_theme()
}

opp_caption <- "Sources: CHPC Preservation Database, January 2024; and 2024 CTCAC/HCD Opportunity Map.  Note: *Neighborhoods that\nmeet the High-Poverty & Segregated definition are also assessed in terms of neighborhood resources."
seg_caption <- "Sources: CHPC Preservation Database, January 2024; and UC Berkeley Othering & Belonging Institute: The Roots of\nStructural Racism Project, 2021"

# save figures
fig_opp_nine <- plot_func(final_opp_nine, title = "9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                          subtitle = "Pre-incentive: 2015-2018; Post-incentive: 2019-2022",
                          caption = opp_caption)
fig_seg_nine <- plot_func(final_seg_nine, title = "9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Tract-Level Segregation",
                          subtitle = "Pre-incentive: 2015-2018; Post-incentive: 2019-2022",
                          caption = seg_caption)
fig_opp_four <- plot_func(final_opp_four, title = "4% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                          subtitle = "Pre-incentive: 2017-2020; Post-incentive: 2021-2022",
                          caption = opp_caption)
fig_seg_four <- plot_func(final_seg_four, title = "4% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Tract-Level Segregation",
                          subtitle = "Pre-incentive: 2017-2020; Post-incentive: 2021-2022",
                          caption = seg_caption)

# write
ggsave(plot = fig_opp_nine, paste0("output/", year, "/fig_opp_nine.tiff"), width = 7, height = 5, bg = "white")
ggsave(plot = fig_seg_nine, paste0("output/", year,"/fig_seg_nine.tiff"), width = 7, height = 5, bg = "white")
ggsave(plot = fig_opp_four, paste0("output/", year,"/fig_opp_four.tiff"), width = 7, height = 5, bg = "white")
ggsave(plot = fig_seg_four, paste0("output/", year,"/fig_seg_four.tiff"), width = 7, height = 5, bg = "white")


