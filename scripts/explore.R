#### this script does an exploratory analysis of the extreme heat/cold events

## initial settings
Sys.setenv(R_MAX_NUM_DLLS = 999)
options("scipen"=100, "digits"=4)
seed <- 10000
set.seed(seed)


####  Install and load the required packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, devtools, backports, Hmisc, tidyr,dplyr,ggplot2,plyr,scales,readr,
               httr, DT, tidyverse,reshape2,lubridate,praznik,epitools,tcltk,ggpmisc,ggpubr)

dat <- read_csv("data/us_extreme_events_by_county_year_by_type_2008_2022.csv")


## focus on a sample state
dat_mas <- subset(dat,dat$STATE_NAME == "Massachusetts")

##how many counties?
length(unique(dat_mas$GEOID))

## look at a single county :25003
dat_mas_25003 <- subset(dat_mas,dat_mas$GEOID == "25003")
# dat_mas_25003$year_numerical <- lubridate::ymd(dat_mas_25003$year_numerical, truncated = 2L)
# simple plot
ggplot(dat_mas_25003,aes(x=year_numerical,group=event_type,colour = factor(event_type)), size = 4) +
  geom_line(aes(y=total_event_days)) +
  facet_grid(GEOID~event_type)


ggplot(dat_mas,aes(x=year_numerical,group=event_type,colour = factor(event_type)), size = 4) +
  geom_line(aes(y=total_event_days)) +
  facet_grid(GEOID~event_type)




###loop through states
for (i in 1:length(unique(dat$STATE_NAME))){
  dat_state <- subset(dat,dat$STATE_NAME == unique(dat$STATE_NAME)[i])

pl <- ggplot(dat_state,aes(x=year_numerical,y=average_impacted_area_hectare,
                   group=event_type,colour = event_type), size = 4) +
  geom_point() +
  geom_line(alpha=0.3,color="black")+
  facet_grid(GEOID~event_type,scales = "free_y") +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)+
  labs(y = "average impacted area (in hectare)", x = "year", title = paste0(dat_state$STATE_NAME)) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title = element_text(size = 15),
                          axis.title.y = element_text(angle = 90, vjust = 0.5),
                          axis.text = element_text(size = 11),
                          legend.position = "none")



ggsave(pl,file=paste0("outputs/plots/",dat_state$STATE_NAME[1],".png"), width = 10, height = 20)

}
