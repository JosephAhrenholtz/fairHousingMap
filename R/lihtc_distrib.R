#' Plot 9% and 4% large-family, new construction LIHTC across opportunity/segregation categories pre- and post-policy adoption
#'
#'
#' @details Cutoff year is 2019 for 9% projects and 2021 for 4% projects.  Currently, we include projects
#'    with application years 2015 or newer for 9%, 2017 or newer for 4%.  opp_lihtc_devels() preps the lihtc data and performs
#'    the spatial join of lihtc developments to neighborhood categories.  plot_lihtc_devels() reads the output of the former,
#'    summarizes and plots the data, and the figures are saved as .tiff files in the products/charts directory.
#'
#'@note The code below was adjusted in March, 2024 per HCD request to treat High-Poverty & Segregated as an exclusive category,
#'    though in the 2024 map it was shifted to an overlay.  This will likely change in future iterations as the research
#'    partners identify an alternative way to present the data that is more aligned with the current mapping methodology.
#'
#'    Additionally, this code was written in parallel to CHPC updating their geocoding standardization process.  Once this process
#'    is finalized, the input files or fields may be adjusted.  Currently, CHPC is sharing one file for locations and a second file
#'    with project details.  The files can be difficult to join because of duplicate id's, project names, app id's, and addresses.
#'    OBI should request that they share a single file to avoid these issues.
#'
#'    The two function calls currently do not include parameters.  If the charts do not change significantly in 2025 than parameters can
#'    be written to specify the year.  If the 2025 charts change significantly than the 2024 code may need to be archived and a new script written.
#'    The charts produced in 2022 are not reproducible because many projects needed to be manually located, which spurred efforts in 2023 between
#'    CHPC and OBI to standardize and document the geocoding process .  There were no charts made using the 2023 map.
#'
#'
#'
#' @examples
#' opp_lihtc_devels() # call first
#' plot_lihtc_devels() # call second
#'
#'
#' @source Preservation Database extraction share by CHPC
#' @source 2024 CTCAC/HCD Opportunity Map
#' @source OBI Categorical segregation: https://berkeley.app.box.com/file/1298215376451
#'
#' @import dplyr tidyr stringr ggplot2 scales
#'
#' @export
#'

opp_lihtc_devels <- function(){
  # STEP 1: LOAD AND REDUCE LIHTC DEVELS
  year <- 2024
  filepaths()


  # prep details
  details <- read.xlsx(pres_db_attributes)
  details <- details %>%
    select('id' = 'Universal.ID', 'proj_name' = 'Name', 'lihtc_application_number' = LIHTC.Application.Number, 'address_orig' = 'Address_Original','lihtc_award_year' = 'LIHTC.Award.Year', 'credit_type' = 'LIHTC.Tax.Credit.Type',
           'construct_type' = 'LIHTC.Construction.Type', 'population_served' = 'Population.Served', 'placed_in_service' = 'PIS?', 'active_programs' = 'Active.Program(s)') %>%
    # filter to only projects with LIHTC app number
    filter(!is.na(lihtc_application_number) &
             grepl('new construction', construct_type, ignore.case=TRUE) &
             grepl('family|families', population_served, ignore.case=TRUE) &
             lihtc_award_year >= 2015)


  # prep locations
  locations <- read_csv(pres_db_locations)
  locations <- locations %>%
    select(
      'id' = 'universal_id',
      #'proj_name' = 'name',
      #lihtc_application_number,
      #'address_orig' = 'address_original',
      'city' = 'city',
      'county_name' = 'county',
      'lowinc_units' = 'affordable_units',
      final_lat,
      final_lon
    )

  # compute inner join
  loc_matched <- details %>%
    inner_join(locations, by = 'id')

  # make sure id, project name, address, and application number across each record are unique
  devels_red <- loc_matched %>%
    group_by(id, proj_name, lihtc_application_number, address_orig) %>%
    filter(n() == 1)

  # convert to spatial object
  devels_sp = st_as_sf(devels_red, coords = c("final_lon", "final_lat"),
                       crs = 4326, agr = "constant")


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

  # insert hps in oppcat for tracts that meet definition - per HCD's request but likely subject to change since does not match with methodology
  opp$pov_seg_flag[is.na(opp$pov_seg_flag)] <- 0
  opp <- opp %>%
    mutate(neighbcat = if_else(pov_seg_flag == 1, 'High-Poverty & Segregated*', oppcat))

  # add opp cat distribution
  # opp <- opp %>%
  #   mutate(n_geo = n()) %>%
  #   group_by(oppcat) %>%
  #   mutate(pct_neighbcat = n()/n_geo) %>%
  #   ungroup()
  opp <- opp %>%
    mutate(n_geo = n()) %>%
    group_by(neighbcat) %>%
    mutate(pct_neighbcat = n()/n_geo) %>%
    ungroup()

  # add share hps
  # opp <- opp %>%
  #   mutate(pct_hps = sum(pov_seg_flag, na.rm = T)/n_geo) %>%
  #   select(-n_geo)

  # spatial join devels to opp cats
  opp_devels <- opp %>%
    sf::st_join(devels_sp)

  # return to df and reduce to tracts with devels
  opp_devels <- opp_devels %>%
    filter(!is.na(id)) %>%
    st_drop_geometry() # 520


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
    sf::st_join(devels_sp)

  # return to df and reduce
  segcat_devels <- segcat_devels %>%
    filter(!is.na(id)) %>%
    st_drop_geometry()

  # write intermediate files
  write_csv(opp_devels, paste0('data/intermediate/', year, '/opp_devels.csv'))
  write_csv(segcat_devels, paste0('data/intermediate/', year, '/segcat_devels.csv'))
}


plot_lihtc_devels <- function(){
  year <- 2024

  # STEP 3: WRANGLE DATA FOR PLOTTING
  # load and factor
  #opp_devels <- read_csv(paste0("data/intermediate/", year, "/opp_devels.csv")) %>% rename('neighbcat' = oppcat)
  opp_devels <- read_csv(paste0("data/intermediate/", year, "/opp_devels.csv"))
  segcat_devels <- read_csv(paste0("data/intermediate/", year, "/segcat_devels.csv")) %>% rename('neighbcat' = segcat)

  opp_devels$neighbcat[which(is.na(opp_devels$neighbcat))] <- 'Insufficient Data'
  #opp_levels <- c('Insufficient Data', 'Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')
  opp_levels <- c('Insufficient Data', 'High-Poverty & Segregated*', 'Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')
  opp_devels$neighbcat <- factor(opp_devels$neighbcat, levels = opp_levels)

  segcat_levels <- c('High POC Segregation', 'High White Segregation', 'Low-Medium Segregation', 'Racially Integrated')
  segcat_devels$neighbcat <- factor(segcat_devels$neighbcat, levels = segcat_levels)

  # separate neighb distribution
  opp_neighbs <- opp_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)
  #hps_neighbs <- opp_devels %>% select('pct_neighbcat' = 'pct_hps') %>% distinct(pct_neighbcat) %>% mutate(neighbcat = 'High-Poverty & Segregated*')
  #opp_neighbs <- bind_rows(hps_neighbs, opp_neighbs)
  #opp_neighbs$neighbcat <- factor(opp_neighbs$neighbcat, c('High-Poverty & Segregated*', opp_levels))
  opp_neighbs$neighbcat <- factor(opp_neighbs$neighbcat, levels = opp_levels)

  seg_neighbs <- segcat_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)


  # Separate 9% opp
  opp_nine <- opp_devels %>% filter(grepl('9', credit_type)) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2019, 'Pre-incentive', 'Post-incentive'))
  # hps_nine <- opp_nine %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
  # hps_nine$neighbcat <- factor(hps_nine$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

  # Separate 9% seg
  seg_nine <- segcat_devels %>% filter(grepl('9', credit_type)) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2019, 'Pre-incentive', 'Post-incentive'))


  # separate 4% opp
  opp_four <- opp_devels %>% filter(grepl('4', credit_type), lihtc_award_year >= 2017) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))
  # hps_four <- opp_four %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
  # hps_four$neighbcat <- factor(hps_four$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

  # Separate 4% seg
  seg_four <- segcat_devels %>% filter(grepl('4', credit_type), lihtc_award_year >= 2017) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))

  # move data frames to a list
  #opp_nine_list <- list(hps_nine, opp_nine)
  opp_nine_list <- list(opp_nine)
  seg_nine_list <- list(seg_nine)
  #opp_four_list <- list(hps_four, opp_four)
  opp_four_list <- list(opp_four)
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

  #final_opp_nine <- bind_rows(lapply(opp_nine_list, summ)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
  final_opp_nine <- bind_rows(lapply(opp_nine_list, summ)) %>% left_join(opp_neighbs, by = 'neighbcat')
  final_seg_nine <- bind_rows(lapply(seg_nine_list, summ)) %>% left_join(seg_neighbs, by = 'neighbcat')
  # final_opp_four <- bind_rows(lapply(opp_four_list, summ)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
  final_opp_four <- bind_rows(lapply(opp_four_list, summ)) %>% left_join(opp_neighbs, by = 'neighbcat')
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

  opp_caption <- "Sources: CHPC Preservation Database, January 2024; and 2024 CTCAC/HCD Opportunity Map.  Note: *Though\nneighborhoods that meet the High-Poverty & Segregated definition are also assessed in terms of neighborhood resources\nin the 2024 Opportunity Map, High-Poverty & Segregated is treated as a mutually exclusive category for the purposes of this\nchart."
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
  ggsave(plot = fig_opp_nine, paste0("products/charts/", year, "/fig_opp_nine.tiff"), width = 7, height = 5, bg = "white")
  ggsave(plot = fig_seg_nine, paste0("products/charts/", year,"/fig_seg_nine.tiff"), width = 7, height = 5, bg = "white")
  ggsave(plot = fig_opp_four, paste0("products/charts/", year,"/fig_opp_four.tiff"), width = 7, height = 5, bg = "white")
  ggsave(plot = fig_seg_four, paste0("products/charts/", year,"/fig_seg_four.tiff"), width = 7, height = 5, bg = "white")
}






