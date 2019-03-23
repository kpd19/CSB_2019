###############################
#                             #
#       FUNCTION FILE         #
#         KP Dixon            #
#         CSB 2019            #
#                             #
###############################

library(tidyverse)
library(zoo)


# function that capitalizes the first letter in a string
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

########################### 
#
# FUNCTIONS + TESTS
#
########################### 

# function to plot daily snow depth or other metric for a given date range
# works great
plot_over_time <- function(data, timeA = 1990, timeB = 2000, var = "SNWD", metric = "Snow Depth"){
  SD_plot <- data %>% filter(year >= timeA) %>% filter(year <= timeB) %>% ggplot() + 
    aes_string(x = "DATE", y = var) + geom_line(color = "darkblue") +
    labs(title = paste0(metric, " - ", sitename),
         y = metric,
         x = "Date") + theme_bw(base_size = 15)
  
  print(SD_plot)
}



# function to find the temperature for the hottest day of the year 
# works but isn't malliable for variable
TMAX_over_time <- function(data, timeA = 1990, timeB = 2000){
  SD_plot <- data %>% filter(year >=timeA) %>% filter(year <= timeB) %>%  
    group_by(year) %>% summarize(max_var = max(TMAX, na.rm=TRUE)) %>% 
    ggplot() + aes(x = year, y = max_var) + geom_line(color = "red") +
    labs(title = paste0("Hottest Temp of the Year - ", sitename),
         y = "Temperature (F)",
         x = "Date") + theme_bw(base_size = 15)
  
  print(SD_plot)
}



# function to find the temperature for the coldest day of the year 
# works but isn't malliable for variable
TMIN_over_time <- function(data, timeA = 1990, timeB = 2000){
  SD_plot <- data %>% filter(year >= timeA) %>% filter(year <= timeB) %>%
    group_by(year) %>% summarize(min_var = min(TMIN, na.rm=TRUE)) %>% 
    ggplot() + aes(x = year, y = min_var) + geom_line(color = "darkblue") +
    labs(title = paste0("Coldest temperature of the year (F) - ", sitename),
         y = "Temperature (F)",
         x = "Date") + theme_bw(base_size = 15)
  
  print(SD_plot)
}



# function to find the Extremes for the year 
# works but isn't malliable for variable
EXTREMES_over_time <- function(data, timeA = 1990, timeB = 2000){
  SD_plot <- dat %>% filter(year >=timeA) %>% filter(year <= timeB) %>%
    group_by(year) %>% summarize(max_var = max(TMAX, na.rm=TRUE), min_var = min(TMIN, na.rm=TRUE)) %>% 
    ggplot() + aes(x = year) + geom_line(aes(y = max_var), color = "red") + geom_line(aes(y=min_var), color="darkblue") +
    labs(title = paste0("Extreme Temps of the Year - ", sitename),
         y = "Temperature (F)",
         x = "Date") + theme_bw(base_size = 15)
  
  print(SD_plot)
}


# function to find the last day with no snow
last_no_snow <- function(dataset, timeA = 1990, timeB = 2000){
  SD_plot <- dataset %>% filter(year >= timeA) %>% filter(year <= timeB) %>% group_by(year) %>% filter(!is.na(SNWD)) %>% filter(SNWD==0) %>% 
    select(DATE) %>% top_n(1) %>%  mutate(early = as.numeric(format(DATE, format="%j"))) %>% 
    ggplot() + aes(x=year, y= early) %>% geom_point(color="blue") +
    labs(title = paste0("Day of first accumulated snow - ", sitename),
         y = "Julian Day",
         x = "Year") + theme_bw(base_size = 15)
  
  print(SD_plot)
}


# function to find the first day with no snow
# errors with it being in early january
earliest_no_snow <- function(dataset, timeA = 1990, timeB = 2010){
  SD_plot <- dataset %>% filter(year >= timeA) %>%  filter(year <= timeB) %>% group_by(year) %>% filter(!is.na(SNWD)) %>% filter(SNWD==0) %>% 
    select(DATE) %>% top_n(1, desc(DATE)) %>%  mutate(early = as.numeric(format(DATE, format="%j"))) %>% 
    ggplot() + aes(x=year, y= early) %>% geom_point(color="blue") +
    labs(title = paste0("Extreme Temps of the Year - ", sitename),
         y = "Julian Day",
         x = "Year") + theme_bw(base_size = 15)
  
  print(SD_plot)
}


# function to find cumulative snowfall 
cumulative_snow <- function(dataset, timeA = 1990, timeB = 2010){
  snow_dat <- dataset %>% filter(year >= timeA) %>% filter(year <= timeB) %>% group_by(year) %>% summarize(cumulative = sum(SNOW, na.rm=TRUE))
  SD_plot <- snow_dat %>% ggplot() + aes(x=year, y= cumulative) %>% geom_point(color="blue") +
    labs(title = paste0("Extreme Temps of the Year - ", sitename),
         y = "Cumulative Inches of Snow",
         x = "Year") + theme_bw(base_size = 15)
  
  print(SD_plot)
}

# function to find the number of degree days relevant to DFTM
find_degree_days <- function(dataset, timeA=1991, threshold = 42, target = 400){
  first_date <- dataset %>% filter(year == timeA) %>%  mutate(average = (TMAX + TMIN)/2) %>% mutate(diff = average - threshold) %>% 
    mutate(d_days = ifelse(diff <=0,0,diff)) %>% mutate(j_date = as.numeric(format(DATE, format="%j"))) %>% drop_na(d_days) %>% 
    mutate(cumulative = cumsum(d_days)) %>% filter(cumulative>target) %>% top_n(1, desc(j_date)) %>% select(j_date)
  return(as.numeric(first_date))
}


# for all the years in the dataset find the julian date for degree days above a certain threshold
degree_days_time <- function(dataset, timeA = 1985, timeB = 2010, threshold=42, target=440, site = "Site Here", PLOT = TRUE){
  
  years <- seq(timeA, timeB, 1)
  values <- array(NA, dim=c(length(years),2))
  values[,1] <- years
  
  for(i in 1:length(years)){
    values[i,2] <- find_degree_days(dataset = dataset, timeA = years[i], threshold = threshold, target = target)
  }
  values <- data.frame(values)
  colnames(values) <- c("year", "julian_days")
  mean_j_date <- mean(values$julian_days, na.rm=TRUE)
  var_j_date <- var(values$julian_days, na.rm=TRUE)
  
  if (PLOT == TRUE){
    degree_day_plot <- values %>% ggplot() + aes(x = year, y = julian_days) + geom_point(color="blue") +
      geom_hline(yintercept = mean_j_date, color = "blue") + 
      labs(title = paste0("Degree days - ", sitename),
           y = paste0("Julian date ", DFTM_target, " degree days reached"),
           x = "Year") + theme_bw(base_size = 15)
    
    print(degree_day_plot)
  } else{
  print("no plot")
  }
  
  return(list(mean_j_date, var_j_date)) 
  
}


# to either plot the cumulative degree days or just how many degree days each calendar day added
plot_degree_days <- function(dataset, timeA=1991, threshold = 42, target = 440, var = "cumulative"){
  degree_days <- dat %>% filter(year == timeA) %>%  mutate(average = (TMAX + TMIN)/2) %>% mutate(diff = average - threshold) %>% 
    mutate(d_days = ifelse(diff <=0,0,diff)) %>% mutate(j_date = as.numeric(format(DATE, format="%j"))) %>% drop_na(d_days) %>% 
    mutate(cumulative = cumsum(d_days)) 
  
  date <- which(degree_days$cumulative >= target)[1]
  
  degree_day_plot <- degree_days %>% ggplot() + aes_string(x = "j_date", y = var)+ geom_point(color="blue") +
    labs(title = paste0("Daily Degree days - ", sitename, " - ", timeA),
         y = paste0("Degree days"),
         x = "Julian Date") + theme_bw(base_size = 15)
  
  if(var == "cumulative"){
  degree_day_plot <- degree_day_plot + geom_segment(aes(x=date,xend=date,y=0,yend=target)) + 
    geom_segment(aes(x=0,xend=date,y=target,yend=target)) + 
    geom_point(x = date, y = target) + annotate(geom="text", x=300, y=target, label=paste0(target, " reached at day ", date),
                                                   color="black")
  }
  print(degree_day_plot)
}

