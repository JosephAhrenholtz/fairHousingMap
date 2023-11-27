#' Plot 9% and 4% large-family, new construction LIHTC across opportunity/segregation categories pre- and post-policy adoption
#'
#'
#' @details Cutoff year is 2019 for 9% projects and 2021 for 4% projects.  Currently, we include projects
#'    with application years 2015 or newer.  geo_lihtc() loads, geocodes, and performs the spatial join of lihtc
#'    developments to neighborhood categories.  The output of geo_lihtc() is two intermediate csv files, which are
#'    read by plot_lihtc(), summarized, plotted, and the figures are saved as .tiff files in the output/year directory.
#'
#' @param year designates the appropriate filepaths
#'
#' @examples
#' geo_lihtc(year = current_year) # call first
#' plot_lihtc(year = current_year) # call second
#'
#'
#' @source TCAC Projects: https://www.treasurer.ca.gov/ctcac/projects.asp
#' @source OBI Categorical segregation: https://berkeley.app.box.com/file/1298215376451
#'
#' @import dplyr tidyr stringr tidygeocoder tigris ggplot2
#'
#' @export
#'


geo_lihtc <- function(year = current_year){


  # STEP 1: LOAD, REDUCE, FORMAT FIPS
  filepaths(year = year)

  devels <- read_zip(lihtc, year = year, type = 'excel', sheet = "California Mapped Projects")

  # rename and select necessary cols
  devels <- devels %>%
    rename(appid = `Application.Number`, app_stage = `Application.Stage`,
           PISdate = `Placed.in.Service.(PIS).Date`,
           address = `Project.Address`, housing_type = `Housing.Type`,
           lowinc_units = `Low.Income.Units`,
           county_name = `Project.County`, construct_type = `Construction.Type`,
           zipcode = `Project.Zip.Code`, credit_type = `Type.of.tax.credit.funding`,
           ) %>%
    # create application year column from appid
    mutate(app_year = substr(appid, 4,7)) %>%
    select(appid, app_year, app_stage, PISdate, address, zipcode, county_name,
           housing_type, construct_type, credit_type, lowinc_units)


  # ensure target variables are formatted consistently (could be written more elegantly)
  devels$housing_type[which(devels$housing_type == 'Large family')] <- 'Large Family'
  devels$housing_type[which(devels$construct_type == 'New Construction ')] <- 'New Construction'
  devels$credit_type <- str_replace_all(devels$credit_type, c("9% ARRA" = "9 %", "0.09" = "9 %"))
  devels$credit_type <- str_replace_all(devels$credit_type, c("4% ARRA" = "4 %", "0.04" = "4 %", "4%" = "4 %"))

  # filter to exclusively new construction, family-serving developments after 2014 and ensure there is an associated credit type
  devels <- devels %>%
    filter(housing_type == 'Large Family' & construct_type == 'New Construction' & as.numeric(app_year) >= 2015 & !is.na(credit_type))


  # NOTE: WE MAY NEED ADDITIONAL FILTER ON APPLICATION STAGE BUT NEED TO CONFIRM


  # END STEP 1

  # STEP 2: GEOCODE AND SPATIAL JOINS

  # temporarily taking sample for small geocode request
  devels <- devels[sample(nrow(devels), 50),] %>% tidygeocoder::geocode(address = address, method = 'arcgis')

  # ***add step to filter by performance***

  # add geometry
  devels <- devels %>%
    st_as_sf(coords = c("long","lat")) %>%
    st_sf(crs = 4326)

  # load opp and reduce to necessary columns
  opp <- final_opp(write = F, as_geo = T) %>%
    select(fips, fips_bg, region, oppcat, pov_seg_flag, nbrhood_chng)


  # summarize opp cat distribution
  opp <- opp %>%
    mutate(n_geo = n()) %>%
    group_by(oppcat) %>%
    mutate(pct_neighbcat = n()/n_geo) %>%
    ungroup()

  # spatial join devels to opp cats
  opp_devels <- opp %>%
    sf::st_join(devels)

  # return to df and reduce to tracts with devels
  opp_devels <- opp_devels %>%
    filter(!is.na(appid)) %>%
    st_drop_geometry()


  # repeat for seg cats
  # load seg cat and make spatial
  segcat <- read_zip(segcat, year = year, type = 'csv')
  segcat <- segcat %>% select(geoid, segcat) %>% rename('fips' = geoid)
  tract_2010 <- tracts(state = "CA", year = 2010) %>% select(GEOID10, geometry) %>% rename('fips' = GEOID10)
  segcat <- segcat %>%
    left_join(tract_2010, by = 'fips') %>%
    st_as_sf(crs = 4326)

  # summarize segcat distribution
  segcat <- segcat %>%
    mutate(n_geo = n()) %>%
    group_by(segcat) %>%
    mutate(pct_neighbcat = n()/n_geo)

  # spatial join devels to seg cats
  segcat_devels <- segcat %>%
    sf::st_join(devels)

  # return to df and reduce
  segcat_devels <- segcat_devels %>%
    filter(!is.na(appid)) %>%
    st_drop_geometry()

  # write intermediate files
  write_csv(opp_devels, paste0('data/intermediate/', year, '/opp_devels.csv'))
  write_csv(segcat_devels, paste0('data/intermediate/', year, '/segcat_devels.csv'))

  # END STEP 2


}


# summarize
plot_lihtc <- function(year = current_year){

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
  seg_neighbs <- segcat_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)

  # separate 9 %
  opp_nine <- opp_devels %>% filter(credit_type == '9 %') %>%
    mutate(timeframe = ifelse(as.numeric(app_year < 2019), 'Pre-incentive', 'Post-incentive'))
  opp_pre_nine <- opp_nine %>% filter(timeframe == 'Pre-incentive')
  opp_post_nine <- opp_nine %>% filter(timeframe == 'Post-incentive')

  seg_nine <- segcat_devels %>% filter(credit_type == '9 %') %>%
    mutate(timeframe = ifelse(as.numeric(app_year < 2019), 'Pre-incentive', 'Post-incentive'))
  seg_pre_nine <- seg_nine %>% filter(timeframe == 'Pre-incentive')
  seg_post_nine <- seg_nine %>% filter(timeframe == 'Post-incentive')

  # separate 4 %
  opp_four <- opp_devels %>% filter(credit_type == '4 %') %>%
    mutate(timeframe = ifelse(as.numeric(app_year < 2021), 'Pre-incentive', 'Post-incentive'))
  opp_pre_four <- opp_four %>% filter(timeframe == 'Pre-incentive')
  opp_post_four <- opp_four %>% filter(timeframe == 'Post-incentive')

  opp_nine_list <- list(opp_pre_nine, opp_post_nine)
  seg_nine_list <- list(seg_pre_nine, seg_post_nine)
  opp_four_list <- list(opp_pre_four, opp_post_four)

  # summarize
  summ <- function(x) x %>%
    mutate(tot_lowinc_units = sum(lowinc_units)) %>%
    group_by(neighbcat, timeframe) %>%
    reframe(pct_lowinc_units = sum(lowinc_units, na.rm = TRUE)/tot_lowinc_units) %>%
    distinct(neighbcat, .keep_all = T) %>%
    ungroup() %>%
    complete(neighbcat, timeframe, fill = list(pct_lowinc_units = 0))

  final_opp_nine <- bind_rows(lapply(opp_nine_list, summ)) %>% left_join(opp_neighbs, by = 'neighbcat')
  final_seg_nine <- bind_rows(lapply(seg_nine_list, summ)) %>% left_join(seg_neighbs, by = 'neighbcat')
  final_opp_four <- bind_rows(lapply(opp_four_list, summ)) %>% left_join(opp_neighbs, by = 'neighbcat')



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
      theme(plot.caption = element_text(face = "italic"),
            legend.position = c(0.83, 0.86),
            legend.background = element_rect(fill = "white", color = "black"),
            axis.text.x = element_text(angle = 20, hjust=1)) +
      plot_theme()
  }

  # save figures
  fig_opp_nine <- plot_func(final_opp_nine, title = "9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                             subtitle = "Pre-incentive: 2015-2018; Post-incentive: 2019-2023",
                             caption = "Sources: CTCAC database, November 2023; and 2024 CTCAC/HCD Opportunity Map")
  fig_seg_nine <- plot_func(final_seg_nine, title = "9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Tract-Level Segregation",
                             subtitle = "Pre-incentive: 2015-2018; Post-incentive: 2019-2023",
                             caption = "Sources: CTCAC database, November 2023; and UC Berkeley Othering & Belonging Institute: The Roots of Structural Racism Project, 2021")
  fig_opp_four <- plot_func(final_opp_four, title = "4% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                             subtitle = "Pre-incentive: 2017-2020; Post-incentive: 2021-2023",
                             caption = "Sources: CTCAC database, November 2023; and 2024 CTCAC/HCD Opportunity Map")


  # write
  ggsave(plot = fig_opp_nine, paste0("output/", year, "/fig_opp_nine.tiff"), width = 6, height = 5)
  ggsave(plot = fig_seg_nine, paste0("output/", year,"/fig_seg_nine.tiff"), width = 6, height = 5)
  ggsave(plot = fig_opp_four, paste0("output/", year,"/fig_opp_four.tiff"), width = 6, height = 5)


}






















