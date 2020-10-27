#script to create DSR file

#loading packages
library(tidyverse)
library(lubridate)

#loading current month end data
me_data <- read_csv('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Current Month End Raw Data (reworked).csv')

#loading the DSR data to get current month data
dsr_data <- read_csv('S:/Fiskars Americas/Finance/Shared/Forecasting/Beg Forecast & LE/DSR_data.csv')

#converting a couple columns to dates
me_data$ME_Version_Date <- as.Date(me_data$ME_Version_Date, format = '%m/%d/%Y')
me_data$FC_Date <- as.Date(me_data$FC_Date, format = '%m/%d/%Y')

#getting current me_version date to use for filtering the current dsr data
cur_version <- max(me_data$ME_Version_Date)
cur_month <- month(cur_version)
cur_year <- year(cur_version)

#filtering month end data on only future months
me_data <- me_data %>%
  filter(FC_Date > ME_Version_Date)

#grouping and summarising 
me_data <- me_data %>%
  dplyr::select(Customer, Actl_TP_Fcst_Delta, Division, Country, Sales_Manager, Month_No, Year) %>%
  group_by(Customer, Division, Country, Sales_Manager, Month_No, Year) %>%
  summarise(Forecast_Plus_Delta = round(sum(Actl_TP_Fcst_Delta, na.rm = T), 0)) %>%
  ungroup()

#splitting for beginning of month data
BF <- me_data %>%
  mutate(Forecast_Type = 'BF')

#creating LE data
LE <- me_data %>%
  mutate(Forecast_Type = 'LE')

#appending BF and LE
me_data <- rbind(BF, LE) %>%
  dplyr::select(Customer, Division, Country, Sales_Manager, Month_No, Year, Forecast_Type, Forecast_Plus_Delta)

#changing name of all other if sales rep isn't all other
me_data <- me_data %>%
  mutate(Customer = if_else(Customer == 'ALL OTHER - (G45)' & 
                              Sales_Manager != 'ALL OTHER',
                             'ALL OTHER BY SALES MANAGER', Customer))

#filtering DSR data on current month only
dsr_data <- dsr_data %>%
  filter(Month_No <= cur_month & Year == cur_year)

#adding DSR data to month end data and sorting
me_data <- rbind(me_data, dsr_data) %>%
  arrange(Customer, Forecast_Type, Year, Month_No)

#writing file to S drive
write.csv(me_data, 'S:/Fiskars Americas/Finance/Shared/Forecasting/Beg Forecast & LE/DSR_data.csv', row.names = F)

#removing variables
rm(list = ls())

print('DSR_data file has been created')


