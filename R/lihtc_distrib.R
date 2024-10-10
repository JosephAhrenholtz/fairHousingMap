#' Plot 9% and 4% large-family, new construction LIHTC across opportunity/segregation categories pre- and post-policy adoption
#'
#'
#' @details Cutoff year is 2019 for 9% projects and 2021 for 4% projects.  Currently, we include projects
#'    with application years 2015 or newer for 9%, 2017 or newer for 4%.  opp_lihtc_devels() preps the lihtc data and performs
#'    the spatial join of lihtc developments to neighborhood categories.  plot_lihtc_devels() reads the output of the former,
#'    summarizes and plots the data, and the figures are saved as .tiff files in the products/charts directory.
#'
#'@note The code below was adjusted in September, 2024 for analysis to be included in the draft 2025 OM FAQ.
#'
#'
#' @examples
#' opp_lihtc_devels() # call first
#' plot_lihtc_devels() # call second
#'
#'
#' @source Preservation Database extraction share by CHPC in September, 2024.  The DSA with CHPC requires that we delete the data
#' once we run the analysis.  This code is only to be run within agreed upon timeframe of the DSA, which can be found here: https://berkeley.box.com/s/j5fhauebi4rw64f957ysdsvatzl08zib.
#' @source 2025 CTCAC/HCD Opportunity Map
#'
#' @import dplyr tidyr stringr ggplot2 scales
#'
#' @export
#'

opp_lihtc_devels <- function(){


  # STEP 1: PREPARE LIHTC
  year <- 2025
  filepaths()
  lf <- st_read(lf_lihtc)

  # CHPC provided large family developments but different construction types so filter for projects that included new construction
  lf <- lf %>%
    filter(grepl('new construction|nc/', lihtc_construction_type, ignore.case=TRUE))

  # Print unique values of housing type - each should include large family
  unique_housing_types <- shQuote(unique(lf$housing_type))
  message("Unique housing types: ", paste(unique_housing_types, collapse = ", "))

  # Print unique values of construction type
  unique_construction_types <- shQuote(unique(lf$lihtc_construction_type))
  message("Unique constuction types: ", paste(unique_construction_types, collapse = ", "))

  # check for duplicated projects and stop execution if found
  lf_grouped <- lf %>%
    group_by(universal_id, name, lihtc_application_number)

  duplicates <- lf_grouped %>%
    filter(n() > 1)

  if (nrow(duplicates) > 0) {
    stop("Error: There are duplicate projects in the LIHTC dataset.")
  } else {
    rm(lf_grouped, duplicates)
  }

  # convert to spatial object and make attribute-geometry relationship constant over geometry operations
  lf <-  st_as_sf(lf, crs = 4326, agr = "constant")


  # STEP 2: JOIN TO OPP NEIGHBS
  # load
  load('data/final_2025.rda')
  opp <- final_2025
  rm(final_2025)

  # reduce
  opp <- opp %>%
    select(fips, fips_bg, region, oppcat, pov_seg_flag)

  # make generic neighbcat column
  opp$pov_seg_flag[is.na(opp$pov_seg_flag)] <- 0
  opp <- opp %>%
    mutate(neighbcat = oppcat)

  # add opp cat distribution
  opp <- opp %>%
    mutate(n_geo = n()) %>%
    group_by(neighbcat) %>%
    mutate(pct_neighbcat = n()/n_geo) %>%
    ungroup()

  # add share hps
  opp <- opp %>%
    mutate(pct_hps = sum(pov_seg_flag, na.rm = T)/n_geo) %>%
    select(-n_geo)


  # spatial join devels to opp cats
  opp_devels <- opp %>%
    sf::st_join(lf)

  # return to df and reduce to tracts with devels
  opp_devels <- opp_devels %>%
    filter(!is.na(lihtc_application_number)) %>%
    st_drop_geometry()

  # write intermediate files
  write_csv(opp_devels, paste0('data/intermediate/', year, '/opp_devels.csv'))

}


plot_lihtc_devels <- function(){
  year <- 2025

  # STEP 3: WRANGLE DATA FOR PLOTTING
  # load and factor
  opp_devels <- read_csv(paste0("data/intermediate/", year, "/opp_devels.csv"))

  opp_devels$neighbcat[which(is.na(opp_devels$neighbcat))] <- 'Insufficient Data'
  opp_levels <- c('Insufficient Data', 'Low Resource', 'Moderate Resource', 'High Resource', 'Highest Resource')
  opp_devels$neighbcat <- factor(opp_devels$neighbcat, levels = opp_levels)

  # separate neighb distribution
  opp_neighbs <- opp_devels %>% select(neighbcat, pct_neighbcat) %>% distinct(neighbcat, .keep_all = T)
  hps_neighbs <- opp_devels %>% select('pct_neighbcat' = 'pct_hps') %>% distinct(pct_neighbcat) %>% mutate(neighbcat = 'High-Poverty & Segregated*')
  opp_neighbs <- bind_rows(hps_neighbs, opp_neighbs)
  opp_neighbs$neighbcat <- factor(opp_neighbs$neighbcat, c('High-Poverty & Segregated*', opp_levels))





  # STEP 4: PLOT LARGE-FAMILY PORTFOLIO (ALL YEARS)

  hps_devels <- opp_devels %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
  hps_devels$neighbcat <- factor(hps_devels$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))
  opp_devels_list <- list(opp_devels, hps_devels)

  # summarize the entire portfolio
  summ_all_devels <- function(x) x %>%
    mutate(tot_aff_units = sum(affordable_units)) %>%
    group_by(neighbcat) %>%
    reframe(
      pct_aff_units = sum(affordable_units, na.rm = TRUE)/tot_aff_units) %>%
    distinct(neighbcat, .keep_all = T) %>%
    complete(neighbcat, fill = list(pct_aff_units = 0)) %>%
    ungroup()

  final_opp_devels <- bind_rows(lapply(opp_devels_list, summ_all_devels)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
  final_opp_devels$neighbcat <- factor(final_opp_devels$neighbcat, c('High-Poverty & Segregated*', opp_levels))


  # plot function for entire portfolio
  plot_func_all <- function(df, title, subtitle, caption) {

    # lengthen and factor
    df <- df %>% pivot_longer(cols = -neighbcat, names_to = "name", values_to = "value")

    # ensure the order of the factors matches the intended labels
    df$name <- factor(df$name, levels = c("pct_neighbcat", "pct_aff_units"))

    df %>%
      ggplot(aes(x = neighbcat, y = value, fill = name, label = scales::percent(value, accuracy = 1))) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      scale_fill_manual(labels = c("% Neighborhoods", "% Affordable units"), values = c("#999999", "#1CAC78")) +
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


  # # create plot and write
  opp_caption <- "Sources: CHPC Preservation Database; and Draft 2025 CTCAC/HCD Opportunity Map.  Note: *Neighborhoods that meet\nthe High-Poverty & Segregated definition are also assessed in terms of neighborhood resources."
  fig_opp_devels <- plot_func_all(final_opp_devels, title = "4% and 9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                                              subtitle = "1990-2023",
                                              caption = opp_caption)
  ggsave(plot = fig_opp_devels, paste0("products/charts/", year, "/fig_opp_portfolio.tiff"), width = 7, height = 5, bg = "white")




  # STEP 5: PLOT 9% AND 4% SPECIFIC

  # Separate 9% opp
  opp_nine <- opp_devels %>% filter(grepl('9', lihtc_tax_credit_type), lihtc_award_year >= 2015) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2019, 'Pre-incentive', 'Post-incentive'))
  # is this right???
  hps_nine <- opp_nine %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
  hps_nine$neighbcat <- factor(hps_nine$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

  # separate 4% opp
  opp_four <- opp_devels %>% filter(grepl('4', lihtc_tax_credit_type), lihtc_award_year >= 2017) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))
  hps_four <- opp_four %>% mutate(neighbcat = case_when(pov_seg_flag == 1 ~ 'High-Poverty & Segregated*', TRUE ~ 'Does Not Meet Definition'))
  hps_four$neighbcat <- factor(hps_four$neighbcat, levels = c('High-Poverty & Segregated*', 'Does Not Meet Definition'))

  # seperate 4% with entire pre-incentive portfolio for data point in FAQ
  opp_four_allpre <- opp_devels %>% filter(grepl('4', lihtc_tax_credit_type)) %>%
    mutate(timeframe = ifelse(lihtc_award_year < 2021, 'Pre-incentive', 'Post-incentive'))


  # move data frames to a list
  opp_nine_list <- list(hps_nine, opp_nine)
  opp_four_list <- list(hps_four, opp_four)
  opp_four_allpre_list <- list(opp_four_allpre)


  summ_timeframe <- function(x) x %>%
    group_by(timeframe) %>%
    mutate(tot_aff_units = sum(affordable_units)) %>%
    group_by(neighbcat, timeframe) %>%
    reframe(
      pct_aff_units = sum(affordable_units, na.rm = TRUE)/tot_aff_units) %>%
    distinct(neighbcat, timeframe, .keep_all = T) %>%
    complete(neighbcat, timeframe, fill = list(pct_aff_units = 0)) %>%
    ungroup()

  final_opp_nine <- bind_rows(lapply(opp_nine_list, summ_timeframe)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
  final_opp_four <- bind_rows(lapply(opp_four_list, summ_timeframe)) %>% filter(neighbcat != 'Does Not Meet Definition') %>% left_join(opp_neighbs, by = 'neighbcat')
  final_opp_four_allpre <- bind_rows(lapply(opp_four_allpre_list, summ_timeframe)) %>% left_join(opp_neighbs, by = 'neighbcat')


  # plot function
  plot_func_timeframe <- function(df, title, subtitle, caption){

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

  # save figures
  fig_opp_nine <- plot_func_timeframe(final_opp_nine, title = "9% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                            subtitle = "Pre-incentive: 2015-2018; Post-incentive: 2019-2023",
                            caption = opp_caption)

  fig_opp_four <- plot_func_timeframe(final_opp_four, title = "4% LIHTC-Financed Affordable Units in Large-Family \nDevelopments by Level of Neighborhood Resources",
                            subtitle = "Pre-incentive: 2017-2020; Post-incentive: 2021-2023",
                            caption = opp_caption)

  # write
  ggsave(plot = fig_opp_nine, paste0("products/charts/", year, "/fig_opp_nine.tiff"), width = 7, height = 5, bg = "white")
  ggsave(plot = fig_opp_four, paste0("products/charts/", year,"/fig_opp_four.tiff"), width = 7, height = 5, bg = "white")



}




