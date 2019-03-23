###############################
#                             #
#      TEST DATA FILE         #
#         KP Dixon            #
#         CSB 2019            #
#                             #
###############################


rm(list=ls())

source("Functions.R")

# reads in the data
dat <- read_csv("Datasets/Leavenworth_daily_CSB.csv")

# takes a look at the dataframe
glimpse(dat)

# find the sitename from the datafile, used in functions to plot with name
sitename <- tolower(strsplit(unique(dat$NAME), " ")[[1]][1])
sitename <- firstup(sitename)

# add some information about months and years
dat$month <- format(dat$DATE, "%b")
dat$year <- as.numeric(format(dat$DATE,"%Y"))

# filter for years with only greater than 320 days of info
good_years <- dat %>% group_by(year) %>% summarize(numdays = n()) %>% filter(numdays >320) %>% select(year)
dat <- dat %>% filter(year %in% good_years$year)
# dat %>% group_by(year) %>% summarize(numdays = n()) %>% ggplot() + aes(x=year, y= numdays) + geom_line(color="dark green")

# set some global parameters for the dataset
min_date <- min(dat$year)
max_date <- max(dat$year)

DFTM_threshold <- 42 # the degree threshold for Douglas Fir Tussock Moths (DFTM)
DFTM_target <- 440 # the number of degree days for DFTM to start hatching

# plots the value for each day over time
# timeA is the lowest year and timeB is the highest year
# options are whichever columns data is available for e.g. TMAX, TMIN, SNWD (snowdepth), SNOW (snowfall)
# changing the metric value changes the xaxis label
# can print using sitename in the filename
var1 <- "TMAX"
metric1 <- "Temp Max"
pdf(paste0("Plots/", var1, "_", sitename, ".pdf"))
plot_over_time(data = dat, timeA=min_date, timeB=max_date, var = var1, metric = metric1)
dev.off()

# plot that finds the TMAX for each year and plots it
TMAX_over_time(data = dat, timeA=min_date, timeB=max_date)

# plot that finds the TMAX for each year and plots it
TMIN_over_time(data = dat, timeA=min_date, timeB=max_date)

# plots both TMAX and TMIN for year year over time
EXTREMES_over_time(data = dat, timeA=min_date, timeB=max_date)

# finds the last date with no snow on the ground
last_no_snow(dataset = dat, timeA=min_date, timeB = max_date)

# finds when the snow melts
# issue with finding it in January when there hasn't been a big snowfall yet
earliest_no_snow(dataset = dat, timeA=min_date, timeB = max_date)

# finds cumulative snowfall for the year
cumulative_snow(dataset = dat, timeA=min_date, timeB = max_date)

# function that finds number of degree days over time using another function
# uses threshold for degree days and calculates a value for each day
# finds how many julian days in a year it takes to get to a given threshold
# you can choose to plot and return values of mean and variance over the time or just to plot
plot_mod <- degree_days_time(dataset = dat, timeA = min_date, timeB = max_date, threshold = DFTM_threshold, target = DFTM_target, PLOT = TRUE)
degree_days_mean <- as.numeric(plot_mod[1])
degree_days_var <- as.numeric(plot_mod[2])

print(degree_days_mean)
print(degree_days_var)


# choose between var2 = "d_days" (shows how much each day gives) and var2 = "cumulative"

var2 <- "cumulative"
pdf(paste0("Plots/", var2, "_", sitename, ".pdf"))
plot_degree_days(dataset=dat, timeA = 2005, threshold = DFTM_threshold, target = DFTM_target, var = var2)
dev.off()

