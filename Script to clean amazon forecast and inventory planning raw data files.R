#Code to clean weekly forecast and inventory files

#packages to load
library(tidyverse)
library(lubridate)



#-------------- Loading data and creating some variables -------------------------------------------------

#loading weekly forecast and inventory file
filename <- file.choose()
amz <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, skipNul = TRUE)
amz <- amz[,1:134]

#changing the name of the product title column to match the other files
names(amz)[3] <- 'Product_Title'
names(amz)[names(amz) == 'ï»¿ASIN'] <- 'ASIN'  

#extracting month and year and day from filename
d <- str_sub(filename, -14, -5)
d <- str_split(d, '_', simplify = TRUE)
file_mon <- as.numeric(d[1])
file_day <- as.numeric(d[2])
file_yr <- as.numeric(d[3])

#creating some variables for date columns
mon_name <- month.name[file_mon]
file_date <- as.Date(paste(file_yr, file_mon, file_day, sep = "-"), "%Y-%m-%d")

#creating variables for the last day in the forecast range to get the date of the last complete month in the range and the start of the next full month
last_day <- file_date + days(181)
rollback_date <- rollback(last_day)
start_next_month <- floor_date(file_date, 'month') + months(1)



#--------------Code to clean the inventory portion of the file -------------------------------------------------

#pulling out inventory data to save in a separate file
amz_inv <- amz[1:17]

#changing column names
names(amz_inv) <- c('ASIN', 'UPC', 'Product_Title', 'Per_Rep_OOS', 'Rep_OOS_%_of_Total', 'Rep_OOS_Per_Prior_Period', 'Ordered_Units', 'Ordered_Units_Prior_Period_Per', 'Unfilled_Customer_Ordered_Units', 'Available_Inventory', 'Available_Inventory_Prior_Period_Per', 'Weeks_On_Hand', 'Open_Purchase_Order_Qty', 'Open_Purchase_Order_Qty_Prior_Period_Per', 'Receive_Fill_Rate_Per', 'Overall_Vendor_Lead_Time_Days', 'Replenishment_Category')

#cleaning data
#cleaning dollars and units columns
amz_inv[,c(7,9,10,12,13,16)]<- amz_inv[,c(7,9,10,12,13,16)] %>% mutate_all(function(x) as.numeric(gsub('[\\$,()]', '', x)))
#cleaning % columns
amz_inv[,c(4,5,6,8,11,14,15)]<- amz_inv[,c(4,5,6,8,11,14,15)] %>% mutate_all(function(x) as.numeric(gsub('[\\%,]', '', x))/100)

#adding date columns
amz_inv <- add_column(amz_inv, Year = file_yr, .before = 1)
amz_inv <- add_column(amz_inv, Day = file_day, .before = 1)
amz_inv <- add_column(amz_inv, Month_Num = file_mon, .before = 1)
amz_inv <- add_column(amz_inv, Month = mon_name, .before = 1)
amz_inv <- add_column(amz_inv, Date = file_date, .before = 1)





#--------------Code to clean and aggregate the Mean forecast -------------------------------------------------

#creating a dataframe with the 'mean' forecast at a monthly level
amz_mean <- amz %>%
  select(ASIN, UPC, Product_Title, contains('Mean Forecast'))

#getting rid of spaces in column names
names(amz_mean) <- gsub(' ', '', names(amz_mean))

#getting rid of commas in numbers
amz_mean[4:ncol(amz_mean)] <- as.numeric(apply(amz_mean[4:ncol(amz_mean)], 2, gsub, patt = ',', replace =  ''))

#looping through forecast columns and making 7 daily columns
for (i in 1:(ncol(amz_mean) - 3)) {
  for (j in 1:7) {
    new_col <- paste('Week', i, '.', j, sep = '')
    old_col <- paste('Week', i, '-MeanForecast', sep = '')
    amz_mean <- amz_mean %>%
      mutate(!!new_col := amz_mean[[old_col]]/7)
  }
}

#getting rid of the original columns
amz_mean <- amz_mean %>% select(-contains('MeanForecast'))

#creating new column names
new_col_dates <- seq(file_date, last_day, by = 'day')
new_col_names <- c('ASIN', 'UPC', 'Product_Title', as.character(new_col_dates))
names(amz_mean) <- new_col_names

#deleting columns that don't make complete months
cols_to_delete <- new_col_dates >= start_next_month & new_col_dates <= rollback_date
cols_to_delete <- c(TRUE, TRUE, TRUE, cols_to_delete)

#cutting out incomplete months so there are only complete months
amz_mean <- amz_mean[cols_to_delete]

#reshaping the data and creating a month column, then aggregating by month
amz_mean <- gather(amz_mean, key = 'Date', value = 'Value', -ASIN, -UPC, -Product_Title) %>%
  arrange(ASIN, Date)

#creating a month and year column
amz_mean <- amz_mean %>%
  mutate('Month_Num' = month(Date)) %>%
  mutate('Month' = month(Date, label = TRUE, abbr = FALSE)) %>%
  mutate('Year' = year(Date)) 

#grouping by month and year
amz_mean <- amz_mean %>%
  mutate('POS_Forecast_Type' = 'Mean_Fcst') %>%
  group_by(ASIN, UPC, Product_Title, Month_Num, Month, Year, POS_Forecast_Type) %>%
  summarise('POS_Forecast' = round(sum(Value), 0)) %>%
  arrange(ASIN, Year, Month_Num)





#--------------Code to clean and aggregate the P70 forecast -------------------------------------------------

#creating a dataframe with the 'P70' forecast at a monthly level
amz_p70 <- amz %>%
  select(ASIN, UPC, Product_Title, contains('P70 Forecast'))

#getting rid of spaces in column names
names(amz_p70) <- gsub(' ', '', names(amz_p70))

#getting rid of commas in numbers
amz_p70[4:ncol(amz_p70)] <- as.numeric(apply(amz_p70[4:ncol(amz_p70)], 2, gsub, patt = ',', replace =  ''))

#looping through forecast columns and making 7 daily columns
for (i in 1:(ncol(amz_p70) - 3)) {
  for (j in 1:7) {
    new_col <- paste('Week', i, '.', j, sep = '')
    old_col <- paste('Week', i, '-P70Forecast', sep = '')
    amz_p70 <- amz_p70 %>%
      mutate(!!new_col := amz_p70[[old_col]]/7)
  }
}

#getting rid of the original columns
amz_p70 <- amz_p70 %>% select(-contains('P70Forecast'))

#creating new column names
new_col_dates <- seq(file_date, last_day, by = 'day')
new_col_names <- c('ASIN', 'UPC', 'Product_Title', as.character(new_col_dates))
names(amz_p70) <- new_col_names

#deleting columns that don't make complete months
cols_to_delete <- new_col_dates >= start_next_month & new_col_dates <= rollback_date
cols_to_delete <- c(TRUE, TRUE, TRUE, cols_to_delete)

#cutting out incomplete months so there are only complete months
amz_p70 <- amz_p70[cols_to_delete]

#reshaping the data and creating a month column, then aggregating by month
amz_p70 <- gather(amz_p70, key = 'Date', value = 'Value', -ASIN, -UPC, -Product_Title) %>%
  arrange(ASIN, Date)

#creating a month and year column
amz_p70 <- amz_p70 %>%
  mutate('Month_Num' = month(Date)) %>%
  mutate('Month' = month(Date, label = TRUE, abbr = FALSE)) %>%
  mutate('Year' = year(Date)) 

#grouping by month and year
amz_p70 <- amz_p70 %>%
  mutate('POS_Forecast_Type' = 'P70_Fcst') %>%
  group_by(ASIN, UPC, Product_Title, Month_Num, Month, Year, POS_Forecast_Type) %>%
  summarise('POS_Forecast' = round(sum(Value), 0)) %>%
  arrange(ASIN, Year, Month_Num)




#--------------Code to clean and aggregate the P80 forecast -------------------------------------------------

#creating a dataframe with the 'P80' forecast at a monthly level
amz_p80 <- amz %>%
  select(ASIN, UPC, Product_Title, contains('P80 Forecast'))

#getting rid of spaces in column names
names(amz_p80) <- gsub(' ', '', names(amz_p80))

#getting rid of commas in numbers
amz_p80[4:ncol(amz_p80)] <- as.numeric(apply(amz_p80[4:ncol(amz_p80)], 2, gsub, patt = ',', replace =  ''))

#looping through forecast columns and making 7 daily columns
for (i in 1:(ncol(amz_p80) - 3)) {
  for (j in 1:7) {
    new_col <- paste('Week', i, '.', j, sep = '')
    old_col <- paste('Week', i, '-P80Forecast', sep = '')
    amz_p80 <- amz_p80 %>%
      mutate(!!new_col := amz_p80[[old_col]]/7)
  }
}

#getting rid of the original columns
amz_p80 <- amz_p80 %>% select(-contains('P80Forecast'))

#creating new column names
new_col_dates <- seq(file_date, last_day, by = 'day')
new_col_names <- c('ASIN', 'UPC', 'Product_Title', as.character(new_col_dates))
names(amz_p80) <- new_col_names

#deleting columns that don't make complete months
cols_to_delete <- new_col_dates >= start_next_month & new_col_dates <= rollback_date
cols_to_delete <- c(TRUE, TRUE, TRUE, cols_to_delete)

#cutting out incomplete months so there are only complete months
amz_p80 <- amz_p80[cols_to_delete]

#reshaping the data and creating a month column, then aggregating by month
amz_p80 <- gather(amz_p80, key = 'Date', value = 'Value', -ASIN, -UPC, -Product_Title) %>%
  arrange(ASIN, Date)

#creating a month and year column
amz_p80 <- amz_p80 %>%
  mutate('Month_Num' = month(Date)) %>%
  mutate('Month' = month(Date, label = TRUE, abbr = FALSE)) %>%
  mutate('Year' = year(Date)) 

#grouping by month and year
amz_p80 <- amz_p80 %>%
  mutate('POS_Forecast_Type' = 'P80_Fcst') %>%
  group_by(ASIN, UPC, Product_Title, Month_Num, Month, Year, POS_Forecast_Type) %>%
  summarise('POS_Forecast' = round(sum(Value), 0)) %>%
  arrange(ASIN, Year, Month_Num)




#--------------Code to clean and aggregate the P90 forecast -------------------------------------------------

#creating a dataframe with the 'P90' forecast at a monthly level
amz_p90 <- amz %>%
  select(ASIN, UPC, Product_Title, contains('P90 Forecast'))

#getting rid of spaces in column names
names(amz_p90) <- gsub(' ', '', names(amz_p90))

#getting rid of commas in numbers
amz_p90[4:ncol(amz_p90)] <- as.numeric(apply(amz_p90[4:ncol(amz_p90)], 2, gsub, patt = ',', replace =  ''))

#looping through forecast columns and making 7 daily columns
for (i in 1:(ncol(amz_p90) - 3)) {
  for (j in 1:7) {
    new_col <- paste('Week', i, '.', j, sep = '')
    old_col <- paste('Week', i, '-P90Forecast', sep = '')
    amz_p90 <- amz_p90 %>%
      mutate(!!new_col := amz_p90[[old_col]]/7)
  }
}

#getting rid of the original columns
amz_p90 <- amz_p90 %>% select(-contains('P90Forecast'))

#creating new column names
new_col_dates <- seq(file_date, last_day, by = 'day')
new_col_names <- c('ASIN', 'UPC', 'Product_Title', as.character(new_col_dates))
names(amz_p90) <- new_col_names

#deleting columns that don't make complete months
cols_to_delete <- new_col_dates >= start_next_month & new_col_dates <= rollback_date
cols_to_delete <- c(TRUE, TRUE, TRUE, cols_to_delete)

#cutting out incomplete months so there are only complete months
amz_p90 <- amz_p90[cols_to_delete]

#reshaping the data and creating a month column, then aggregating by month
amz_p90 <- gather(amz_p90, key = 'Date', value = 'Value', -ASIN, -UPC, -Product_Title) %>%
  arrange(ASIN, Date)

#creating a month and year column
amz_p90 <- amz_p90 %>%
  mutate('Month_Num' = month(Date)) %>%
  mutate('Month' = month(Date, label = TRUE, abbr = FALSE)) %>%
  mutate('Year' = year(Date)) 

#grouping by month and year
amz_p90 <- amz_p90 %>%
  mutate('POS_Forecast_Type' = 'P90_Fcst') %>%
  group_by(ASIN, UPC, Product_Title, Month_Num, Month, Year, POS_Forecast_Type) %>%
  summarise('POS_Forecast' = round(sum(Value), 0)) %>%
  arrange(ASIN, Year, Month_Num)



#--------------Combining and saving files -------------------------------------------------

#combining all forecast versions into one file
all_fcsts <- rbind(amz_mean, amz_p70, amz_p80, amz_p90) %>%
  ungroup()

#sorting 
all_fcsts <- all_fcsts %>%
  arrange(ASIN, Year, Month_Num, POS_Forecast_Type)

#adding forecast version date column
all_fcsts <- add_column(all_fcsts, POS_Fcst_Version_Date = file_date, .before = 1)

#spreading the data so there aren't so many rows because that adds up very quickly
all_fcsts <- spread(all_fcsts, key = POS_Forecast_Type, value = POS_Forecast)

#creating new filenames
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
new_fcst_filename <- gsub('Forecast and Inventory Planning', 'Weekly Forecast', new_filename)
new_fcst_filename <- gsub('Amazon US Weekly Weekly', 'Amazon US Weekly', new_fcst_filename)
new_inv_filename <- gsub('Weekly Forecast', 'Weekly Inventory', new_fcst_filename)

#creating filenames for the all weeks forecast files
all_fcst_filename <- str_extract(new_fcst_filename, '.*(?=Forecast\\\\)')
csv_all_fcst_filename <- gsub('Weekly ', 'Weekly Forecast\\\\Amazon US Weekly Forecast With UPC_All_Weeks.csv', all_fcst_filename)
r_all_fcst_filename <- gsub('Weekly ', 'Weekly Forecast\\\\Amazon US Weekly Forecast With UPC_All_Weeks.rds', all_fcst_filename)

#creating filenames for the all weeks inventory files
all_inv_filename <- str_extract(new_inv_filename, '.*(?=Inventory\\\\)')
csv_all_inv_filename <- gsub('Weekly ', 'Weekly Inventory\\\\Amazon US Weekly Inventory With UPC_All_Weeks.csv', all_inv_filename)
r_all_inv_filename <- gsub('Weekly ', 'Weekly Inventory\\\\Amazon US Weekly Inventory With UPC_All_Weeks.rds', all_inv_filename)

#reading in all weeks files
all_fcst_data <- readRDS(r_all_fcst_filename)
all_fcst_data <- rbind(all_fcst_data, all_fcsts)
all_inv_data <- readRDS(r_all_inv_filename)
all_inv_data <- rbind(all_inv_data, amz_inv)

#removing duplicated rows in case a file that has already been cleaned was selected
all_fcst_data <- unique(all_fcst_data)
all_inv_data <- unique(all_inv_data)

#saving files
saveRDS(all_fcst_data, r_all_fcst_filename)
saveRDS(all_inv_data, r_all_inv_filename)
write.csv(all_fcst_data, csv_all_fcst_filename, row.names = FALSE)
write.csv(all_inv_data, csv_all_inv_filename, row.names = FALSE)
write.csv(all_fcsts, new_fcst_filename, row.names = FALSE)
write.csv(amz_inv, new_inv_filename, row.names = FALSE)

#clearing workspace
rm(list = ls())

#printing finished so I know all loops have run
print('finished')

