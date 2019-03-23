###############################
#                             #
#      PREDICTIONS FOR        #
#       DEGREE DAYS           #
#         KP Dixon            #
#         CSB 2019            #
#                             #
###############################


rm(list=ls())

source("Functions.R")

# set some global parameters for the dataset
DFTM_threshold <- 42 # the degree threshold for Douglas Fir Tussock Moths (DFTM)
DFTM_target <- 440 # the number of degree days for DFTM to start hatching


file.dir <- "Datasets/"

# 4. loads filenames form directory
files <- c(list.files(file.dir, pattern = ".csv"))
length(files)

sites <- c()
means <- c()
var <- c()

for (i in 1:length(files)){
  filename <- paste(file.dir, files[i], sep="/")
  dat <- read_csv(filename) #, col_types=list(DATE = col_date()))
  
  sitename <- tolower(strsplit(unique(dat$NAME), " ")[[1]][1])
  sitename <- firstup(sitename)
  
  dat$month <- format(dat$DATE, "%b")
  dat$year <- as.numeric(format(dat$DATE,"%Y"))
  
  good_years <- dat %>% group_by(year) %>% summarize(numdays = n()) %>% filter(numdays >320) %>% select(year)
  dat <- dat %>% filter(year %in% good_years$year)
  
  plot_mod <- degree_days_time(dataset = dat, threshold = DFTM_threshold, target = DFTM_target, site = sitename, PLOT = FALSE)
  degree_days_mean <- as.numeric(plot_mod[1])
  degree_days_var <- as.numeric(plot_mod[2])
  
  
  sites <- c(sites, sitename)
  means <- c(means, degree_days_mean)
  var <- c(var, degree_days_var)
  
  
}

# rounding the mean and calculated the standard deviations from the data
means <- round(means)
sd <- sqrt(var)
sd <- round(sd)

# translating the julian days into dates
date_means <- as.Date(means, origin=as.Date("2019-01-01"))
sd_upper <- as.Date(means+sd, origin = as.Date("2019-01-01"))
sd_lower <- as.Date(means-sd, origin = as.Date("2019-01-01"))


plt <- ggplot() + aes(x=sites, y=date_means) + 
  geom_point( color="black") +
  geom_errorbar(aes(ymin=sd_lower, ymax=sd_upper), width=.2) + 
  labs(title = paste0("Predictions for when ", DFTM_target, " degree days is reached"),
       y = "Date",
       x = "Sites") + theme_bw(base_size = 15)


pdf("Plots/2019Pred.pdf")
print(plt)  
dev.off()

