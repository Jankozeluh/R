library(tidyverse)
library(DataExplorer)

setwd("~/Documents/R/tel/imola_ks_ferrari_f138_practice_7_11_2021")

data <- read.table("imola_ks_ferrari_f138_practice_7_11_2021.csv",header = TRUE) %>% na.omit() %>% as_tibble()
info <- list(car=data$carId[1],track_name=data$trackId[1],track_length=data$trackLength[1])
data <- select(data, -c(1, 2, 3, 5, 6, 8, 9, 12, 29, 30, 41, 42, 43))

#damage <- tibble(Engine = data$engine_damage, Index3 = data$index_3_damage, Gear_box = data$gear_box_damage, Rear_bumper = data$rear_bumper_damage, Front_bumper = data$front_bumper_damage)

anyDamage <- function(){
  damage <- data %>% select(1,2,73:77) %>% filter_at(vars(contains("damage")), all_vars(.==-1))
  print(paste0("Car was damaged in these laps: ", damage %>% select(1) %>% distinct())) 
  print("If you would like to see more information type anything.")
  cc <- readline()
  if(cc != ""){
    print(damage)
  }
}

anyDamage()

#I just realized that this dataset is pretty weird, I dunno what did I recorded.

tyreTemp <- function(){
  full <- data %>% select(1,2,106:117)
  
  ggplot(data=full,aes(x=lapIndex, y=tyre_temp_I_0, color=binIndex)) +
    geom_line()
  
  ggplot(data=full, aes(x = lapIndex, color=binIndex)) + 
    geom_bar() +
    facet_wrap(vars(tyre_temp_O_1,tyre_temp_O_2,tyre_temp_O_3), ncol = 3) +
    labs(x = "Lap Index", y = "Temperature")
  #not workin
  print(full)
  #full %>% filter_at(vars(contains("temp")), all_vars(.!=-1))
}

