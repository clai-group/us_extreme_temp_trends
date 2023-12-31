#### this script does an exploratory analysis of the extreme heat/cold events

## initial settings
Sys.setenv(R_MAX_NUM_DLLS = 999)
options("scipen"=100, "digits"=4)
seed <- 10000
set.seed(seed)


####  Install and load the required packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(stars, # spatiotemporal data handling
               #raster, # raster data handling
               #terra, # raster data handling
               sf, # vector data handling
               dplyr, # data wrangling
               stringr, # string manipulation
               lubridate, # dates handling
               data.table, # data wrangling
               patchwork, # arranging figures
               tigris, # county border
               colorspace, # color scale
               viridis, # arranging figures
               tidyr, # reshape
               ggspatial, # north arrow and scale bar
               ggplot2, # make maps
               broom, here)

#dat <- data.table::fread("data/us_extreme_events_by_county_year_by_type_2008_2022.csv")
dat_path <- file.path(here::here("trend_analysis_data/data",
                               "Counties_compiled_admin_geo_ehe_ece_sf_2008_2022.rds"))

file_size <- file.info(dat_path)$size

dat <- readRDS("data/Counties_compiled_admin_geo_ehe_ece_sf_2008_2022.rds")[[1]]

### we need to create annual/monthly aggregates by frequency (each row is an event in a county), maximum ratio acrea impacted, and maximum/minimum intensity

## create a non-spatial table of annual statistics from the counties spatial data
dat_table <- dat %>% st_drop_geometry()

ls(dat)

dat_table$abs_intensity = abs(ifelse(dat_table$event_type == "Extreme Cold Event", dat_table$min_intensity, dat_table$max_intensity))

#Annual
avg_by_county = dat_table %>% group_by(STATE_NAME,NAME, event_type,GEOID, year_numerical) %>% 
  summarise(count_event = n(),
            total_imapacted_area_hectare = sum(impacted_area_hectare),
            avg_impacted_area_hectare = mean(impacted_area_hectare),
            mean_intensity = mean(abs_intensity), 
            avg_impacted_area_ratio = mean(impacted_area)/total_area) 

## which counties have the highest frequency of each events?
freq_by_county = dat_table %>%
  dplyr::group_by(STATE_NAME,NAME, event_type,GEOID) %>%
  dplyr::summarise(total_event_days = n(), mean_days=sum(total_event_days)/15) 


freq_cold = freq_by_county %>% filter(event_type == "Extreme Cold Event") %>% 
  arrange(desc(mean_days)) %>% distinct() ## Cold

freq_heat = freq_by_county %>% filter(event_type == "Extreme Heat Event") %>% ## Cold
  arrange(desc(mean_days)) %>% distinct() ## Heat

## maximum ratio acrea impacted
ratio_cold = avg_by_county %>% filter(event_type == "Extreme Cold Event") %>% 
  select(GEOID, STATE_NAME,NAME, year_numerical,avg_impacted_area_ratio, event_type) %>% 
  arrange(desc(avg_impacted_area_ratio)) %>% 
  distinct() ## Cold

ratio_heat =avg_by_county %>% filter(event_type == "Extreme Heat Event") %>% 
  select(GEOID, STATE_NAME,NAME, year_numerical,avg_impacted_area_ratio, event_type) %>% 
  arrange(desc(avg_impacted_area_ratio)) %>% 
  distinct() ## Heat

## intensity
intensity_cold = avg_by_county %>% filter(event_type == "Extreme Cold Event") %>% 
  select(GEOID, STATE_NAME,NAME, year_numerical,mean_intensity, event_type) %>% 
  arrange(desc(mean_intensity)) %>% 
  distinct() ## Cold

intensity_heat =avg_by_county %>% filter(event_type == "Extreme Heat Event") %>% 
  select(GEOID, STATE_NAME,NAME, year_numerical,mean_intensity, event_type) %>% 
  arrange(desc(mean_intensity)) %>% 
  distinct() ## Heat


## Plots of absolute intensity of counties for each state
unique_states = unique(dat_table$STATE_NAME)
for (state_name in unique_states) {
  # Filter the dataset for each state
  filtered_data <- dat_table %>%
    filter(STATE_NAME == state_name) %>% 
    group_by(STATE_NAME,NAME, event_type,GEOID, year_numerical) %>%
    summarise(mean_intensity = mean(abs_intensity))
  
  # Plots by counties for each state
  intensity_plot = ggplot(filtered_data,aes(x=year_numerical,group=event_type,colour = factor(event_type)), size = 4) +
    geom_line(aes(y=mean_intensity)) + facet_grid(GEOID~event_type)
}



#
#
#
#
#
# # create a non-spatial table of annual statistics from the counties spatial data
# dat_table <- dat %>% st_drop_geometry()
#
# ls(dat)
# ## focus on a sample state
# dat_mas <- subset(dat_table, dat$STATE_NAME == "Massachusetts")
#
# ##how many counties?
# length(unique(dat_mas$GEOID))
#
# # which counties have the highest frequency of each events?
# by_county_count_average <- dat_table %>%
#   dplyr::group_by(STATE_NAME,NAME, event_type,GEOID) %>%
#   dplyr::summarise(mean_days=sum(total_event_days)/15,max_days=max(total_event_days)) %>%
#   arrange(desc(mean_days))
#
# # which counties have the largest area impacted by each events?
# by_county_acres_average <- dat_table %>%
#   dplyr::group_by(STATE_NAME,NAME, event_type,GEOID) %>%
#   dplyr::summarise(mean_acres=sum(average_impacted_area_hectare)/15,max_area=max(average_impacted_area_hectare)) %>%
#   arrange(desc(mean_acres))
#
# # ## look at a single county :25003
# # dat_mas_25003 <- subset(dat_mas,dat_mas$GEOID == "25003")
# # # dat_mas_25003$year_numerical <- lubridate::ymd(dat_mas_25003$year_numerical, truncated = 2L)
# # # simple plot
# # ggplot(dat_mas_25003,aes(x=year_numerical,group=event_type,colour = factor(event_type)), size = 4) +
# #   geom_line(aes(y=total_event_days)) +
# #   facet_grid(GEOID~event_type)
# #
# #
# # ggplot(dat_mas,aes(x=year_numerical,group=event_type,colour = factor(event_type)), size = 4) +
# #   geom_line(aes(y=total_event_days)) +
# #   facet_grid(GEOID~event_type)
#
#
#
#
# ###loop through states and plot --- TAKES TIME AND NOT VERY USEFUL!
# # for (i in 1:length(unique(dat$STATE_NAME))){
# #   dat_state <- subset(dat,dat$STATE_NAME == unique(dat$STATE_NAME)[i])
# #
# # pl <- ggplot(dat_state,aes(x=year_numerical,y=average_impacted_area_hectare,
# #                    group=event_type,colour = event_type), size = 4) +
# #   geom_point() +
# #   geom_line(alpha=0.3,color="black")+
# #   facet_grid(GEOID~event_type,scales = "free_y") +
# #   stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
# #   labs(y = "average impacted area (in hectare)", x = "year", title = paste0(dat_state$STATE_NAME)) +
# #   theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 18),
# #                           axis.title = element_text(size = 15),
# #                           axis.title.y = element_text(angle = 90, vjust = 0.5),
# #                           axis.text = element_text(size = 11),
# #                           legend.position = "none")
# #
# #
# #
# # # ggsave(pl,file=paste0("outputs/plots/",dat_state$STATE_NAME[1],".png"), width = 10, height = 20)
# #
# # }
#
#
# ###simple regression analysis by county
# ##standardized the variables? but interpretation will be really hard
# # dat$average_impacted_area_hectare <- scale(dat$average_impacted_area_hectare)
#
#
#
# out_hectar_US <- dat %>%  st_drop_geometry() %>%
#   group_by(event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(average_impacted_area_hectare ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
# out_frequency_US <- dat_table %>%
#   group_by(event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(total_event_days ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
# out_hectar_county <- dat_table %>%
#   group_by(GEOID,event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(average_impacted_area_hectare ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
#
# out_hectar_state <- dat_table %>%
#   group_by(STATE_NAME,event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(average_impacted_area_hectare ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
#
# out_frequency_county <- dat_table %>%
#   group_by(GEOID,event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(total_event_days ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
# out_frequency_state <- dat_table %>%
#   group_by(STATE_NAME,event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(total_event_days ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
#
# out_hectar_county$GEOID <- as.character(out_hectar_county$GEOID)
#
# ### map it
#
# library(sf)
# # read the spatial layer of census unit in sf class
# #counties_boundaries_sf <- readRDS("data/contiguous_us_counties_sf_2022.rds")
# event <- "Extreme Heat Event"
#
#
# #counties_boundaries_sf <- merge(counties_boundaries_sf,
# #                              subset(out_hectar_county,out_hectar_county$event_type == event), by="GEOID", all=T)
# counties_boundaries_sf <-  dat
#
# ggplot(data = counties_boundaries_sf) +
#   geom_sf(aes(fill=estimate),color=NA) +
#   theme_void() +
#   scale_fill_viridis(name="Hectars", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
#   labs(
#     title = paste0(event),
#     subtitle = "Estimated increase by Hectar impacted since 2008",
#     caption = "--"
#   ) +
#   theme(
#     text = element_text(color = "#22211d"),
#     plot.background = element_rect(fill = "#f5f5f2", color = NA),
#     panel.background = element_rect(fill = "#f5f5f2", color = NA),
#     legend.background = element_rect(fill = "#f5f5f2", color = NA),
#
#     plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
#     plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
#     plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
#
#     legend.position = c(0.7, 0.09)
#   ) +
#   coord_sf()
#
#
#
# # By state numbers
# by_state_count <- dat %>%
#   dplyr::group_by(STATE_NAME,event_type,year_numerical) %>%
#   dplyr::summarise(count=max(total_event_days))
#
# US_count <- by_state_count %>%
#   dplyr::group_by(event_type,year_numerical) %>%
#   dplyr::summarise(count=max(count))
#
# out_frequency_US <- US_count %>%
#   group_by(event_type) %>%
#   group_modify(
#     # Use `tidy`, `glance` or `augment` to extract different information from the fitted models.
#     ~ tidy(glm(count ~ year_numerical, data = .))
#   ) %>%
#   filter(term=="year_numerical")
#
#
#
#
#
