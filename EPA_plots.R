library(ggplot2)
library(dplyr)

filter_by_date <- function(df, start_date, end_date) {
  if(missing(end_date)) {
    return(df %>% mutate(datesort = as.Date(Date, format= "%m/%d/%Y")) %>%
             filter(datesort >= as.Date(start_date)) %>% select(Site.ID, datesort, Daily.Mean.PM2.5.Concentration)) 
  } else { 
    return(df %>% mutate(datesort = as.Date(Date, format= "%m/%d/%Y")) %>%
             filter(datesort >= as.Date(start_date) & datesort <= as.Date(end_date)) 
           %>% select(Site.ID, datesort, Daily.Mean.PM2.5.Concentration)) 
  }
}

process_epa_data <- function(df, start_date, end_date) {
  df <- filter_by_date(df, start_date, end_date)
  colnames(df) = c('site_id', 'datesort', 'dm_PM25')
  df$site_id <- as.factor(df$site_id)
  return(df)
}

plot_epa_data <- function(df, location, year, linedata, outfile) {
  p <- ggplot(df, aes(x=datesort, y=dm_PM25, colour=site_id)) +
    geom_line() + 
    scale_color_brewer(palette="Paired") +
    ggtitle(paste("Daily Mean PM 2.5 Concentration", location, year, sep = ", ")) + 
    xlab("date") + 
    ylab("PM 2.5 concentration (\u03bcg/m\u00b3)") + 
    ylim(0,200) +
    geom_hline(aes(yintercept = yint, linetype = linenames), linedata)
  ggsave(p, file=paste(fpath_out, outfile, sep = ''))
  return(p)
}


fpath = '/Users/student/JelliffeWitte/EPA_data/2020/'
fname = '2020_SFarea_PM25.csv'
fpath_out <<- '/Users/student/JelliffeWitte/EPA_figures/'

pm25_line <- data.frame(yint = c(500, 250.4, 150.4, 55.4, 35.4, 12),
                          #c(12, 35.4, 55.4, 150.4, 250.4, 500), 
                            linenames = factor(c('Hazardous', 'Very Unhealthy', 'Unhealthy', 
                                                 'Unhealthy for Sensitive Groups', 'Moderate', 'Good')))
                        #linenames = factor(c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 
                                        #     'Unhealthy', 'Very Unhealthy', 'Hazardous')))


pm25_line$linenames <- factor(pm25_line$linenames, levels = c('Hazardous', 'Very Unhealthy', 'Unhealthy', 
                                                               'Unhealthy for Sensitive Groups', 'Moderate', 'Good'))

pm25_18sf = read.csv(paste(fpath, fname, sep = ''))


# filter by date
pm25_18sf <- process_epa_data(pm25_18sf, '2020-09-07', '2020-09-11')

myplot = p18sf <- plot_epa_data(pm25_18sf, "SF-Oakland-Hayward", '2020', pm25_line, 'sf_pm25_2020.png') 

myplot

