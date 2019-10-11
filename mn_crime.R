##################
# minnesota crime over time
##################

library(dplyr)
library(tidyr)
library(stringr)
library(tidycensus)
library(ggplot2)
library(gganimate)

census_api_key("ecda17575f4d914b502c70f2bae7a5f3d253792d")

mn <- get_decennial(geography = "county", 
                    state = "MN", 
                    variables = "P001001", 
                    year = 2010, 
                    output = "tidy", 
                    geometry = T) %>%
  mutate(county =  str_replace(NAME, ", Minnesota", ""))

crime <- read.csv("mn_compass_crime.csv") %>%
  gather(key = year, value = value, -c(county, measure)) %>%
  mutate(year = str_remove_all(year, pattern = "X"),
         value = as.numeric(str_remove_all(value, pattern=",")),
         measure = case_when(
           measure=="Serious crime rate per 100,000 residents"~"serious_crime_rate",
           measure=="Total serious crimes"~"serious_crime",
           measure=="Property crime rate per 100,000 residents"~"property_crime_rate",
           measure=="Number of property crimes"~"property_crime",
           measure=="Violent crime rate per 100,000 residents"~"violent_crime_rate",
           measure=="Number of violent crimes"~"violent_crime",
           measure=="Total population"~"total_pop"
         )) %>%
  spread(key = measure, value = value) %>%
  mutate(total_crime = property_crime+violent_crime,
         total_crime_rate = round((total_crime/total_pop)*100000, digits = 0))

mn_crime <- mn %>% inner_join(crime, by = "county") %>%
  select(-c("GEOID","NAME", "variable", "value"))

plot <- ggplot(mn_crime) +
  geom_sf(aes(geometry = geometry, fill=total_crime_rate, group = year)) +
  transition_states(year, transition_length = 1, state_length = 2) + 
  scale_fill_distiller(palette = "Spectral",
                       breaks = c(seq(0,6000,1000)),
                       limits = c(0,6000),
                       name = "Rate per 100,000")+
  labs(title = "Crime Rate in Minnesota Counties: {closest_state}",
          subtitle = "Source: MNBCA UCR")+
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),        panel.background = element_blank(),
        plot.subtitle = element_text(face="italic"),
        panel.grid.major = element_line(colour="transparent"),
        plot.title = element_text(face="bold")) 

animate(plot, fps = 5, end_pause = 10)
anim_save(filename = "mn_crime.gif", animation = last_animation())
