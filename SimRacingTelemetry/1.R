library(tidyverse)
library(DataExplorer)

setwd("~/Documents/R/tel/ks_brands_hatch_rss_formula_hybrid_2018_8_11_2021")

data <- read_csv("ks_brands_hatch_rss_formula_hybrid_2018_8_11_2021.csv") %>% na.omit() %>% as_tibble()
info <- list(car=data$carId[1],track_name=data$trackId[1],track_length=data$trackLength[1])
data <- select(data, -c(1, 2, 3, 5, 6, 8, 9, 12, 29, 30, 41, 42, 43))

######

#velocity <- data %>% select(velocity_X,velocity_Y,velocity_Z)
#gforce <- data %>% select(gforce_X,gforce_Y,gforce_Z)

#tyre_tempCore <- data %>% select(tyre_tempCore_0,tyre_tempCore_1,tyre_tempCore_2,tyre_tempCore_3)
#tyre_temp_M <- data %>% select(tyre_temp_M_0,tyre_temp_M_1,tyre_temp_M_2,tyre_temp_M_3)
#tyre_temp_I <- data %>% select(tyre_temp_I_0,tyre_temp_I_1,tyre_temp_I_2,tyre_temp_I_3)
#tyre_temp_O <- data %>% select(tyre_temp_O_0,tyre_temp_O_1,tyre_temp_O_2,tyre_temp_O_3)

#ers <- data %>% select(ers_recoverySetting,ers_charging,ers_deployed,ers_heatChargingSetting,ers_powerSetting)
#brakeTemp <- data %>% select(brake_temp_0,brake_temp_1,brake_temp_2,brake_temp_3)
#wind <- data %>% select(wind_speed,wind_dir)


######
str(data)
summary(data)
DataExplorer::create_report(select(data, 1:25))
######

#cc_lap_info <- function(){
#  
#}


lap_time <-  function (){
  print(distinct(as.data.frame(data),lapIndex),row.names = FALSE)
  lapp <- readline()
  if(lapp %in% data$lapIndex){
    prep <- data %>% filter(data$lapIndex==lapp) %>% filter(lap_time == max(lap_time)) %>% select(lap_time) %>% distinct()
    print(c(prep))
  }
  else{
    print("Enter valid lap index!")
  }
}

#

pit_time <- function(){
  print(data %>% filter(pit_status==1) %>% select(lapIndex) %>% distinct() %>% pull(lapIndex))
  pitt <- readline()
  if(pitt %in% pits){
    p_time <- data %>% filter(pit_status==1 & lapIndex==pitt) %>% select(lapIndex,lap_time)
    paste("Lap time : ", max(p_time$lap_time)-min(p_time$lap_time)) %>% print()
  }else{
    print("Enter valid lap index!")
  }
}

#

tyre_wear_lap <- function(){
  print(distinct(as.data.frame(data),lapIndex),row.names = FALSE)
  lapp <- readline()
  if(lapp %in% data$lapIndex){
    prep <- data %>% filter(data$lapIndex==lapp) %>% as.data.frame()
  
    mean <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,mean)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    min <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,min)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    max <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,max)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    sd <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,sd)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
  
    final <- rbind(mean,max,min,sd)
    rownames(final) <- c("Mean","Max","Min","Sd")
  
    print(final)
  }
  else{
    print("Enter valid lap index!")
  }
}

#

tyre_wear_ov <- function(){
    prep <- data %>% as.data.frame()
    
    mean <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,mean)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    min <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,min)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    max <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,max)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    sd <- prep %>% mutate(across(tyre_wear_0:tyre_wear_3,sd)) %>% select(tyre_wear_0:tyre_wear_3) %>% distinct()
    
    final <- rbind(mean,max,min,sd)
    rownames(final) <- c("Mean","Max","Min","Sd")
    
    print("Overall tyre wear")
    print(final)
}

#
tyre_wear_lap()
tyre_wear_ov()
#data %>% filter(lapIndex==1) %>% filter(lap_time == max(lap_time)) %>% select(lapIndex,lap_time)
prep <- data %>% filter(data$lapIndex==lapp) %>% filter(lap_time == max(lap_time)) %>% select(lapIndex,lap_time) %>% distinct()



#ggplot(data=data,aes(x=lap_time, y=lap_distance, color=lapIndex)) +
#  geom_line()+geom_smooth()

ggplot(data=data,aes(x=tyre_tempCore$tyre_tempCore_0, y=rpm, color=lapIndex)) +
  geom_line()

#Fuel over the laps
ggplot(data=data,aes(x=lapIndex, y=fuel, color=pit_status)) +  geom_line() + geom_smooth()
       
