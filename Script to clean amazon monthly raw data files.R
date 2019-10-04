#script to clean data from Amazon's vendor portal
#only for Sales Diagnostic, Traffic Diagnostic, Customer Reviews and Inventory Health

#packages to load
library(tidyverse)
library(lubridate)



#Instructions  ------------------------------------------------------------------------------------------------

#Script to clean *MONTHLY* files
#run each block of code separately 
#     - select each monthly file from the file in the raw data folder


#script cleans data, adds a date column, and saves a cleaned monthly file as well as a file (csv and R) that has every month combined



#--------------Code to clean monthly sales diagnostic files -------------------------------------------------

#loading sales diagnostic file
filename <- file.choose()
sales <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

#extracting month and year from filename
mon <- str_extract(filename, "(?<=UPC_)([A-z]*)(?=_)")
yr <- str_extract(filename, "([0-9]{4})")

#creating some variables for date columns
mon_num <- match(mon, month.name)
file_date <- as.Date(paste(yr, mon_num, 1, sep = "-"), "%Y-%m-%d")

#changing column names
names(sales) <- c('ASIN', 'UPC', 'Product_Title', 'Ordered_Revenue', 'Ordered_Revenue_Per_of_Total', 'Ordered_Revenue_Prior_Period_Per_Growth', 'Ordered_Revenue_LY_Per_Growth', 'Ordered_Units', 'Ordered_Units_Per_Total', 'Ordered_Units_Prior_Period_Per_Growth', 'Ordered_Units_LY_Per_Growth', 'Subcategory_Sales_Rank', 'Subcategory_Better_Worse_Per', 'Average_Sales_Price', 'Average_Sales_Price_Prior_Period_Per', 'Change_GV_Prior_Period_Per', 'Change_GV_LY_Per', 'Rep_OOS_Per', 'Rep_OOS_Per_Total', 'Rep_OOS_Prior_Period_Per_Growth', 'LBB_Price_Per')

#cleaning data
#cleaning dollars and units columns
sales[,c(4,8,12,14)]<- sales[,c(4,8,12,14)] %>% mutate_all(function(x) as.numeric(gsub('[\\$,()]', '', x)))
#cleaning % columns
sales[,-c(1,2,3,4,8,12,14)]<- sales[,-c(1,2,3,4,8,12,14)] %>% mutate_all(function(x) as.numeric(gsub('[\\%,]', '', x))/100)

#adding date columns
sales <- add_column(sales, Year = yr, .before = 1)
sales <- add_column(sales, Month_Num = mon_num, .before = 1)
sales <- add_column(sales, Month = mon, .before = 1)
sales <- add_column(sales, Date = file_date, .before = 1)

#creating a new filename to save cleaned data to a new folder
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
all_filename <- str_extract(new_filename, '.*(?=Monthly)')
csv_all_filename <- gsub('Diagnostic\\\\', 'Diagnostic\\\\Amazon US Sales Diagnostic With UPC_All_Months.csv', all_filename)
r_all_filename <- gsub('Diagnostic\\\\', 'Diagnostic\\\\Amazon US Sales Diagnostic With UPC_All_Months.rds', all_filename)

#reading in all months file
all_data <- readRDS(r_all_filename)
all_data <- rbind(all_data, sales)

#removing duplicated rows in case a file that has already been cleaned was selected
all_data <- unique(all_data)

#saving files
saveRDS(all_data, r_all_filename)
write.csv(sales, new_filename, row.names = FALSE)
write.csv(all_data, csv_all_filename, row.names = FALSE)

#clearning workspace
rm(list = ls())




#--------------Code to clean monthly sales diagnostic shipped files -------------------------------------------------

#loading sales diagnostic file
filename <- file.choose()
sales <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

#extracting month and year from filename
mon <- str_extract(filename, "(?<=UPC_)([A-z]*)(?=_)")
yr <- str_extract(filename, "([0-9]{4})")

#creating some variables for date columns
mon_num <- match(mon, month.name)
file_date <- as.Date(paste(yr, mon_num, 1, sep = "-"), "%Y-%m-%d")

#changing column names
names(sales) <- c('ASIN', 'UPC', 'Product_Title', 'Shipped_Revenue', 'Shipped_Revenue_Per_of_Total', 'Shipped_Revenue_Prior_Period_Per_Growth', 'Shipped_Revenue_LY_Per_Growth', 'Shipped_Units', 'Shipped_Units_Per_Total', 'Shipped_Units_Prior_Period_Per_Growth', 'Shipped_Units_LY_Per_Growth', 'Ordered_Units', 'Ordered_Units_Per_Total', 'Ordered_Prior_Period_Per_Growth', 'Ordered_Units_LY_Per_Growth', 'Customer_Returns', 'Free_Replacements', 'Subcategory_Sales_Rank', 'Subcategory_Better_Worse_Per', 'Average_Sales_Price', 'Average_Sales_Price_Prior_Period_Per', 'Change_GV_Prior_Period_Per', 'Change_GV_LY_Per', 'Rep_OOS_Per', 'Rep_OOS_Per_Total', 'Rep_OOS_Prior_Period_Per_Growth', 'LBB_Price_Per')

#cleaning data
#cleaning dollars and units columns
sales[,c(4,8,12,16,17,18,20)]<- sales[,c(4,8,12,16,17,18,20)] %>% mutate_all(function(x) as.numeric(gsub('[\\$,()]', '', x)))
#cleaning % columns
sales[,-c(1,2,3,4,8,12,16,17,18,20)]<- sales[,-c(1,2,3,4,8,12,16,17,18,20)] %>% mutate_all(function(x) as.numeric(gsub('[\\%,]', '', x))/100)

#adding date columns
sales <- add_column(sales, Year = yr, .before = 1)
sales <- add_column(sales, Month_Num = mon_num, .before = 1)
sales <- add_column(sales, Month = mon, .before = 1)
sales <- add_column(sales, Date = file_date, .before = 1)

#creating a new filename to save cleaned data to a new folder
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
all_filename <- str_extract(new_filename, '.*(?=Monthly)')
csv_all_filename <- gsub('Shipped\\\\', 'Shipped\\\\Amazon US Sales Diagnostic Shipped With UPC_All_Months.csv', all_filename)
r_all_filename <- gsub('Shipped\\\\', 'Shipped\\\\Amazon US Sales Diagnostic Shipped With UPC_All_Months.rds', all_filename)

#reading in all months file
all_data <- readRDS(r_all_filename)
all_data <- rbind(all_data, sales)

#removing duplicated rows in case a file that has already been cleaned was selected
all_data <- unique(all_data)

#saving files
saveRDS(all_data, r_all_filename)
write.csv(sales, new_filename, row.names = FALSE)
write.csv(all_data, csv_all_filename, row.names = FALSE)

#clearning workspace
rm(list = ls())


#--------------Code to clean monthly traffic diagnostic files -------------------------------------------------


#loading traffic diagnostic file
filename <- file.choose()
traf <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

#extracting month and year from filename
mon <- str_extract(filename, "(?<=UPC_)([A-z]*)(?=_)")
yr <- str_extract(filename, "([0-9]{4})")

#creating some variables for date columns
mon_num <- match(mon, month.name)
file_date <- as.Date(paste(yr, mon_num, 1, sep = "-"), "%Y-%m-%d")

#changing column names
names(traf) <- c('ASIN', 'UPC', 'Product_Title', 'Per_Total_GVs', 'Per_Change_GV_VS_Prior_Period', 'Per_Change_GV_VS_LY', 'Unique_Visitors_Prior_Period', 'Unique_Visitors_LY', 'Conversion_Percentile', 'Per_Change_In_Conversion_VS_Prior_Period', 'Per_Change_In_Conversion_VS_LY', 'Fast_Track_Glance_View_Per', 'Per_Change_Fast_Track_Glance_View_VS_Prior_Period', 'Per_Change_Glance_View_VS_LY')

#Cleaning data
#all data columns are % so same conversion for all
traf[,4:14]<- traf[,4:14] %>% mutate_all(function(x) as.numeric(gsub('%', '', x))/100)

#adding date columns
traf <- add_column(traf, Year = yr, .before = 1)
traf <- add_column(traf, Month_Num = mon_num, .before = 1)
traf <- add_column(traf, Month = mon, .before = 1)
traf <- add_column(traf, Date = file_date, .before = 1)

#creating a new filename to save cleaned data to a new folder
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
all_filename <- str_extract(new_filename, '.*(?=Monthly)')
csv_all_filename <- gsub('Diagnostic\\\\', 'Diagnostic\\\\Amazon US Traffic Diagnostic With UPC_All_Months.csv', all_filename)
r_all_filename <- gsub('Diagnostic\\\\', 'Diagnostic\\\\Amazon US Traffic Diagnostic With UPC_All_Months.rds', all_filename)

#reading in all months file
all_data <- readRDS(r_all_filename)
all_data <- rbind(all_data, traf)

#removing duplicated rows in case a file that has already been cleaned was selected
all_data <- unique(all_data)

#saving files
saveRDS(all_data, r_all_filename)
write.csv(traf, new_filename, row.names = FALSE)
write.csv(all_data, csv_all_filename, row.names = FALSE)

#clearning workspace
rm(list = ls())



#--------------Code to clean monthly inventory health files -------------------------------------------------


#loading inventory health file
filename <- file.choose()
inv_health <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

#extracting month and year from filename
mon <- str_extract(filename, "(?<=UPC_)([A-z]*)(?=_)")
yr <- str_extract(filename, "([0-9]{4})")

#creating some variables for date columns
mon_num <- match(mon, month.name)
file_date <- as.Date(paste(yr, mon_num, 1, sep = "-"), "%Y-%m-%d")

#changing column names
names(inv_health) <- c('ASIN', 'UPC', 'Product_Title', 'Net_Received_$', 'Net_Received_Units', 'Sell_Through_Rate', 'Open_Purchase_Order_Qty', 'Sellable_On_Hand_Inv_$', 'Sellable_On_Hand_Inv_$_Trailing_30_Day_Avg', 'Sellable_On_Hand_Units', 'Unsellable_On_Hand_Inv_$', 'Unsellable_On_Hand_Inv_$_Trailing_30_Day_Avg', 'Unsellable_On_Hand_Units', 'Aged_90_Days_Sellable_Inv_$', 'Aged_90_Days_Sellable_Inv_$_Trailing_30_Day_Avg', 'Aged_90_Days_Sellable_Inv_Units', 'Unhealthy_Inv_$', 'Unhealthy_Inv_Trailing_30_Day_Avg', 'Unhealthy_Units', 'Replenishment_Category')

#cleaning data
#cleaning dollars and units columns
inv_health[,-c(1,2,3,6,20)]<- inv_health[,-c(1,2,3,6,20)] %>% mutate_all(function(x) as.numeric(gsub('[\\$,()]', '', x)))
#cleaning % columns
inv_health[,6]<- as.numeric(gsub('[\\%,]', '', inv_health[,6]))/100

#adding date columns
inv_health <- add_column(inv_health, Year = yr, .before = 1)
inv_health <- add_column(inv_health, Month_Num = mon_num, .before = 1)
inv_health <- add_column(inv_health, Month = mon, .before = 1)
inv_health <- add_column(inv_health, Date = file_date, .before = 1)

#creating a new filename to save cleaned data to a new folder
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
all_filename <- str_extract(new_filename, '.*(?=Monthly)')
csv_all_filename <- gsub('Health\\\\', 'Health\\\\Amazon US Inventory Health With UPC_All_Months.csv', all_filename)
r_all_filename <- gsub('Health\\\\', 'Health\\\\Amazon US Inventory Health With UPC_All_Months.rds', all_filename)

#reading in all months file
all_data <- readRDS(r_all_filename)
all_data <- rbind(all_data, inv_health)

#removing duplicated rows in case a file that has already been cleaned was selected
all_data <- unique(all_data)

#saving files
saveRDS(all_data, r_all_filename)
write.csv(inv_health, new_filename, row.names = FALSE)
write.csv(all_data, csv_all_filename, row.names = FALSE)

#clearning workspace
rm(list = ls())



#--------------Code to clean monthly customer review files -------------------------------------------------


#loading customer reviews file
filename <- file.choose()
cust <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)

#extracting month and year from filename
mon <- str_extract(filename, "(?<=UPC_)([A-z]*)(?=_)")
yr <- str_extract(filename, "([0-9]{4})")

#creating some variables for date columns
mon_num <- match(mon, month.name)
file_date <- as.Date(paste(yr, mon_num, 1, sep = "-"), "%Y-%m-%d")

#changing column names
names(cust) <- c('ASIN', 'UPC', 'Product_Title', 'Number_Customer_Reviews', 'Number_Customer_Reviews_Prior_Period', 'Number_Customer_Reviews_Life_to_Date', 'Average_Customer_Review', 'Average_Customer_Review_Prior_Period', 'Average_customer_Review_Life_to_Date', '5_Stars', '4_Stars', '3_Stars', '2_Stars', '1_Star')

#cleaning data
#all columns are units
cust[,4:14]<- cust[,4:14] %>% mutate_all(function(x) as.numeric(gsub(',', '', x)))

#adding date columns
cust <- add_column(cust, Year = yr, .before = 1)
cust <- add_column(cust, Month_Num = mon_num, .before = 1)
cust <- add_column(cust, Month = mon, .before = 1)
cust <- add_column(cust, Date = file_date, .before = 1)

#creating a new filename to save cleaned data to a new folder
new_filename <- gsub('Raw', 'Cleaned', filename)
new_filename <- gsub('.csv', '_Cleaned.csv', new_filename)
all_filename <- str_extract(new_filename, '.*(?=Monthly)')
csv_all_filename <- gsub('Reviews\\\\', 'Reviews\\\\Amazon US Customer Reviews With UPC_All_Months.csv', all_filename)
r_all_filename <- gsub('Reviews\\\\', 'Reviews\\\\Amazon US Customer Reviews With UPC_All_Months.rds', all_filename)

#reading in all months file
all_data <- readRDS(r_all_filename)
all_data <- rbind(all_data, cust)

#removing duplicated rows in case a file that has already been cleaned was selected
all_data <- unique(all_data)

#saving files
saveRDS(all_data, r_all_filename)
write.csv(cust, new_filename, row.names = FALSE)
write.csv(all_data, csv_all_filename, row.names = FALSE)

#clearning workspace
rm(list = ls())


