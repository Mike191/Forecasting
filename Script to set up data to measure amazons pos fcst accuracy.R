#script for setting up data to measure Amazon's POS forecast accuracy

#loading libraries
library(tidyverse)
library(lubridate)
library(ggthemes)

#loading data file
amz <- readRDS('S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Amazon Data/Cleaned/Weekly Forecast/Amazon US Weekly Forecast With UPC_All_Weeks.RDS')

#filtering on the second week of every month - the day (Monday) must be between 9 and 15
amz <- amz %>%
  filter(day(POS_Fcst_Version_Date) >=9 & day(POS_Fcst_Version_Date) <= 15)

#creating variables to filter forecast to only the last 12 months
last_month <- as.Date(cut(today(), 'month')) - months(1)
last_year <- last_month - months(11)

#calculating the lag and filtering on the last 12 months
amz <- amz %>%
  mutate(Fcst_Date = make_date(Year, Month_Num, 1),
         Lag = month(as.period(interval(POS_Fcst_Version_Date, Fcst_Date)))+1) %>%
  filter(Lag != 5) %>%
  filter(Fcst_Date >= last_year & Fcst_Date <= last_month) %>%
  select(Fcst_Date, ASIN:Lag)

#loading data to add SKU and our leadtime
lt <- read.csv('S://Fiskars Americas/Operations/Shared/SOP/Forecasting/R/RData/Amazon_ASINS_Leadtimes.csv', header = TRUE, stringsAsFactors = FALSE)

#adding SKU and our leadtime
amz <- amz %>%
  left_join(lt)

#creating a column to say if something is our leadtime or not
amz <- amz %>%
  mutate(Our_LT = if_else(Lag == Our_LT, 'TRUE', 'FALSE')) %>%
  select(1:3, SKU, 4:14)

#loading data for actual POS to compare to forecast
actuals <- readRDS('S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Amazon Data/Cleaned/Sales Diagnostic Shipped/Amazon US Sales Diagnostic Shipped With UPC_All_Months.RDS')

#filtering out unneeded columns and filtering on the last 12 months
actuals <- actuals %>%
  filter(Date >= last_year & Date <= last_month) %>%
  mutate(Lookup = paste0(Date, ASIN)) %>%
  select(Lookup, Shipped_Units)

#joining data to bring in actuals
amz <- amz %>%
  mutate(Lookup = paste0(Fcst_Date, ASIN)) %>%
  left_join(actuals, by = 'Lookup') %>%
  select(-Lookup)

#removing unneeded variables
rm(last_month)
rm(last_year)

#calculating absolute error
amz <- amz %>%
  mutate(Mean_ABS_Error = abs(Mean_Fcst - Shipped_Units),
         P70_Below = if_else(P70_Fcst >= Shipped_Units, 1, 0),
         P80_Below = if_else(P80_Fcst >= Shipped_Units, 1, 0),
         P90_Below = if_else(P90_Fcst >= Shipped_Units, 1, 0))

#calculating Lag 1 accuracy
lag1 <- amz %>%
  filter(Lag == 1) %>%
  group_by(Fcst_Date, Month, Year) %>%
  summarise(Mean_Fcst = sum(Mean_Fcst, na.rm = TRUE),
            Mean_ABS_Error = sum(Mean_ABS_Error, na.rm = TRUE),
            Shipped_Units = sum(Shipped_Units, na.rm = TRUE),
            Num_SKUs = n(),
            Mean_Acc = 1-(Mean_ABS_Error/Shipped_Units),
            P70_Below = sum(P70_Below, na.rm = TRUE)/Num_SKUs,
            P80_Below = sum(P80_Below, na.rm = TRUE)/Num_SKUs,
            P90_Below = sum(P90_Below, na.rm = TRUE)/Num_SKUs)

#calculating Lag 2 accuracy
lag2 <- amz %>%
  filter(Lag == 2) %>%
  group_by(Fcst_Date, Month, Year) %>%
  summarise(Mean_Fcst = sum(Mean_Fcst, na.rm = TRUE),
            Mean_ABS_Error = sum(Mean_ABS_Error, na.rm = TRUE),
            Shipped_Units = sum(Shipped_Units, na.rm = TRUE),
            Num_SKUs = n(),
            Mean_Acc = 1-(Mean_ABS_Error/Shipped_Units),
            P70_Below = sum(P70_Below, na.rm = TRUE)/Num_SKUs,
            P80_Below = sum(P80_Below, na.rm = TRUE)/Num_SKUs,
            P90_Below = sum(P90_Below, na.rm = TRUE)/Num_SKUs)

#calculating Lag 3 accuracy
lag3 <- amz %>%
  filter(Lag == 3) %>%
  group_by(Fcst_Date, Month, Year) %>%
  summarise(Mean_Fcst = sum(Mean_Fcst, na.rm = TRUE),
            Mean_ABS_Error = sum(Mean_ABS_Error, na.rm = TRUE),
            Shipped_Units = sum(Shipped_Units, na.rm = TRUE),
            Num_SKUs = n(),
            Mean_Acc = 1-(Mean_ABS_Error/Shipped_Units),
            P70_Below = sum(P70_Below, na.rm = TRUE)/Num_SKUs,
            P80_Below = sum(P80_Below, na.rm = TRUE)/Num_SKUs,
            P90_Below = sum(P90_Below, na.rm = TRUE)/Num_SKUs)

#calculating our leadtime accuracy
our_lt <- amz %>%
  filter(Our_LT == 'TRUE') %>%
  group_by(Fcst_Date, Month, Year) %>%
  summarise(Mean_Fcst = sum(Mean_Fcst, na.rm = TRUE),
            Mean_ABS_Error = sum(Mean_ABS_Error, na.rm = TRUE),
            Shipped_Units = sum(Shipped_Units, na.rm = TRUE),
            Num_SKUs = n(),
            Mean_Acc = 1-(Mean_ABS_Error/Shipped_Units),
            P70_Below = sum(P70_Below, na.rm = TRUE)/Num_SKUs,
            P80_Below = sum(P80_Below, na.rm = TRUE)/Num_SKUs,
            P90_Below = sum(P90_Below, na.rm = TRUE)/Num_SKUs)
            
            
#creating df for plotting our leadtime
lt_plot_df <- our_lt %>%
  ungroup() %>%
  select(Fcst_Date, Month, Year, 8:11) %>%
  mutate(Month = month.abb[match(Month, month.name)]) %>%
  gather(key = 'Type', value = 'Value', 4:7)

#reordering the month factor so everything plots correctly
lt_plot_df$Month <- as.character(lt_plot_df$Month)
lt_plot_df$Month <- factor(lt_plot_df$Month, levels = unique(lt_plot_df$Month))
lt_plot_df$Value <- lt_plot_df$Value * 100

#plotting
ggplot(lt_plot_df, aes(x = Month, y = Value, color = Type, group = Type)) +
  geom_line() +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x = element_blank(),
       y = 'Accuracy',
       title = 'Amazon\'s POS Forecast Accuracy at Our Leadtime',
       subtitle = 'Last 12 Months') +
  scale_y_continuous(breaks = pretty(lt_plot_df$Value, n = 10))


             
             

             
             
             
             