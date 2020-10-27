#remaking script for All Other month end slides

#loading libraries
library(tidyverse)
library(officer)
library(lubridate)
library(ggchicklet)
library(readxl)
library(ggthemes)

#setting current year
cur_year <- year(Sys.Date())

#loading data
rawdata <- read_excel("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Template - Reworking month end data.xlsx", sheet = "RawData")
deltas <- read_excel("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Template - Reworking month end data.xlsx", sheet = "Deltas")

#fixing monthyear column because it imports as a date
rawdata$Month_Year <- paste0(rawdata$Month, rawdata$Year)
deltas$Month_Year <- paste0(deltas$Month, deltas$Year)

#filtering out last month's data just in case in it's in the file
rawdata <- rawdata %>%
  filter(ME_Version_Date == max(ME_Version_Date))%>%
  select(-Director)

deltas <- deltas %>%
  filter(ME_Version_Date == max(ME_Version_Date)) %>%
  select(-Director)

#reordering columns in deltas df to match rawdata
deltas <- deltas[names(rawdata)]

#filling in NAs with zeros
deltas[is.na(deltas)] <- 0

#combining dataframes
rawdata <- rbind(rawdata, deltas)

#grouping and adding deltas to the rest of the numbers
rawdata <- rawdata %>%
  group_by_at(vars(-c(Delta, Forecast, Shipments, Total_Possible, Actl_TP, Actl_TP_Fcst))) %>%
  summarise_at(vars(c(Delta, Forecast, Shipments, Total_Possible, Actl_TP, Actl_TP_Fcst)), sum, na.rm = TRUE) %>%
  mutate(Actl_TP_Fcst_Delta = sum(c(Actl_TP_Fcst, Delta), na.rm = TRUE))

#rearranging columns
rawdata <- rawdata %>%
  select('ME_Version_Date', 'Country', 'Division', 'Sales_Manager', 'Customer', 'SRP5', 'SRP7', 'Year', 'Month', 'FC_Date', 'Month_No', 'Shipments', 'Forecast', 'Total_Possible', 'Delta', 'Actl_TP', 'Actl_TP_Fcst', 'Actl_TP_Fcst_Delta', 'Conversion_Rate', 'Month_Year', 'Key', 'Current_ME_Version', 'Current_Month')


#pulling in last month's data
lastmonth <- read_csv("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Current Month End Raw Data (reworked).csv") 


#temporary code for testing ------ must delete  -----------------
#lastmonth <- read_csv("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/08-Reworked Month End Raw Data - August 2020.csv")

lastmonth$Year <- as.numeric(lastmonth$Year)

#dropping director column
lastmonth <- lastmonth %>%
  select(-Director)

#reordering columns so they match the rawdata df
lastmonth <- lastmonth[names(rawdata)]


#filtering on all other and getting rid of gov and e-commerce
ao_tm <- rawdata %>%
  filter(Customer == 'ALL OTHER - (G45)' & Year ==  cur_year) %>%
  filter(!Sales_Manager %in% c('KATIE ROBERTS', 'OWN E-COMMERCE', 'TIM TEBBE', 'OPEN GOVERNMENT'))

#filtering last month's data
ao_lm <- lastmonth %>%
  filter(Customer == 'ALL OTHER - (G45)' & Year == cur_year) %>%
  filter(!Sales_Manager %in% c('KATIE ROBERTS', 'OWN E-COMMERCE', 'TIM TEBBE', 'OPEN GOVERNMENT'))

#grouping last month's data by sales manager
ao_lm <- ao_lm %>%
  group_by(Sales_Manager) %>%
  summarise(LM_Total = round(sum(Actl_TP_Fcst_Delta, na.rm = TRUE)/1000, 1))

#grouping this month by sales manager
ao_tm <- ao_tm %>%
  group_by(Sales_Manager) %>%
  summarise(TM_Total = round(sum(Actl_TP_Fcst_Delta, na.rm = TRUE)/1000, 1))


#creating changes df
changes <- left_join(ao_tm, ao_lm, by = 'Sales_Manager')
changes[is.na(changes)] <- 0

#removing the sales manager All Other
changes <- changes %>%
  filter(!Sales_Manager %in% c('ALL OTHER'))




changes <- changes %>%
  mutate(variance = TM_Total - LM_Total,
         abs_variance = abs(variance)) %>%
  arrange(abs_variance) %>%
  mutate(color = if_else(variance < 0, "below", "above"))

#filtering on top 10
changes <- tail(changes, 10)


#plotting current year changes
ggplot(changes, aes(x = factor(Sales_Manager, levels = changes$Sales_Manager), y = variance, fill = color, alpha = .6)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_solarized() +
  scale_fill_manual(values = c("above" = "darkgreen", "below" = "red")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank()) +
  geom_hline(yintercept = 0, alpha = .3) +
  geom_text(aes(label = round(changes$variance, 1)), color = 'black', alpha = 1,
           hjust = ifelse(changes$variance < 0, -.5, 1.5)) +
  labs(title = "Top 10 All Other Changes vs Last Month")



