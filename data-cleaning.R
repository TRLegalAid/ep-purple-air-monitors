# just looking at primary sensor data

library(tidyverse)
library(data.table)
library(janitor)
library(easycsv)
library(lubridate)
library(scales)

data_dir <- path.expand("CSV Primary")
filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)

result <- lapply(filePaths, fread, sep=",")


# Function to clean up the .csv file names
rename <- function(filePath) {
  
  new_name <- str_sub(filePath,10,nchar(filePath)) %>%
    str_remove("Real(.+)")
  
  return(new_name)
}


# apply cleaning function to all filePaths and use these to rename the dfs in the list (result)
new_names <- lapply(filePaths, rename)
names(result) <- new_names


# Function to clean up primary sensor dataframe
prep_primary <- function(df) {
  
  # clean up column names, select columns of interest, remove duplicates
  df2 <- df %>% 
    clean_names() %>%
    select(entry_id, 
           created_at_primary = created_at, 
           pm2_5 = pm2_5_atm_ug_m3) %>%
    unique() %>%
    mutate(created_at_primary_dt = as.POSIXct(created_at_primary, format="%Y-%m-%d %H:%M:%S UTC"))
  
  # new field for just the time
  df2$time <- format(df2$created_at_primary_dt, format =  "%H:%M:%S")
  
  # convert back to posixct
  df2$time <- as.POSIXct(df2$time, format = "%H:%M:%S")
  
  # extract just the hour
  df2$hour <- hour(df2$time)
  
  return(df2)
  
}


# Apply function to all dfs in the list
results_clean <- lapply(result, prep_primary)


#### ------------ Get pm2.5 summary statistics for all monitors

# function to get summary of the pm2_5 column of a dataframe
get_pm2_5_summary <- function(df) {
  
  df_summary <- summary(df$pm2_5)
  
  start <- head(df$created_at_primary_dt, 1)
  end <- tail(df$created_at_primary_dt, 1)
  
  df_summary$start <- head(df$created_at_primary_dt, 1)
  df_summary$end <- tail(df$created_at_primary_dt, 1)
  
  return(df_summary)
}


# Apply function to all dfs in the list
results_summary <- lapply(results_clean, get_pm2_5_summary)

# bind all rows
pm2_5_summary <- bind_rows(results_summary, .id = "column_label")

# # write summary file to csv
# write.csv(pm2_5_summary, "output/pm2_5_summary.csv")



#### -------- Plot observations by time of day for Zavala, Douglas, Mesita, Hawkins

# Select Zavala primary sensors
dfz <- results_clean[[30]] # Zavala E.S. B (undefined)
dfz2 <- results_clean[[29]]  # Zavala E.S (outside) 
dfz3 <- results_clean[[28]] # Zavala 2 B (undefined)
dfz4 <- results_clean[[27]] # Zavala 2 (outside)


## Select Douglass primary sensors
dfd1 <- results_clean[[11]] # Douglass E.S. (outside)
dfd2 <- results_clean[[12]] # Douglass E.S. B (undefined)


## Select Mesita primary sensors
dfm1 <- results_clean[[15]] # Mesita ES (outside)
dfm2 <- results_clean[[16]] # Mesita ES B (undefined)


## Select Hawkins primary sensors
dfh1 <- results_clean[[13]] # Hawkins E.S. (outside)
dfh2 <- results_clean[[14]] # Hawkins E.S. B (undefined)



zavala_es_b <- dfz %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="purple") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))


zavala_es <- dfz2 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="purple") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))
  
  
zavala_2_b <- dfz3 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="purple") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))
  
  
zavala_2 <- dfz4 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="purple") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))


  
douglass_es <- dfd1 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="blue") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))
  
douglass_es_b <- dfd2 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="blue") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))



mesita_es <- dfm1 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="darkgreen") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))


mesita_es_b <- dfm2 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="darkgreen") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))



hawkins_es <- dfh1 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="darkblue") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))

hawkins_es_b <- dfh2 %>% filter(pm2_5 < 1000) %>% 
  ggplot(aes(x=time, y=pm2_5)) + 
  geom_point(alpha=0.2, pch = 16, colour="darkblue") + 
  scale_x_datetime(date_breaks = "6 hours", labels = date_format("%H:%M"))



#### ----------- Get pm2.5 summary metrics by hour

# function to return hourly summary data for cleaned dataframe
get_hourly_summary <- function(df) {
  
  df2 <- df %>% group_by(hour) %>% 
    summarise(mean_pm2_5 = mean(pm2_5), 
              median_pm2_5 = median(pm2_5)) %>%
    drop_na(hour)
    
  return(df2)
}


# Apply function to all cleaned dfs
results_hourly_summary <- lapply(results_clean, get_hourly_summary)


# bind all rows
pm2_5_hourly_summary <- bind_rows(results_hourly_summary, .id = "column_label")

# # write summary file to csv
# write.csv(pm2_5_hourly_summary, "output/pm2_5_hourly_summary.csv")

