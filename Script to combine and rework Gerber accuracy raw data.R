#Script to combine the last two years of Gerber forecast data and put it in the same format with the same column names as the Fiskars file to import into the SKU segmentation file

#INSTRUCTIONS
# Update the years below and run the file.  That's it.


# Assign years to last year and this year  ---------------------------------
ly <- 2019
ty <- 2020



#loading packages  -----------------------------------------------
library(tidyverse)
library(readxl)
#library(openxlsx)


#creating last year file name
ly_filename <- paste0("S://Portland/Operations/Shared/Forecast/^ Reports and Metrics/Forecast Accuracy/", ly,"/FA SUMMARY ", ly, ".xlsx")
ty_filename <- paste0("S://Portland/Operations/Shared/Forecast/^ Reports and Metrics/Forecast Accuracy/", ty,"/FA SUMMARY ", ty, ".xlsx")

#importing accuracy data for this year and last year
ly_acc <- read_excel(ly_filename, sheet = 'DATA')
ty_acc <- read_excel(ty_filename, sheet = 'DATA')



#Cleaning up this year's data   ------------------------------------------
ty_acc <- ty_acc %>%
  filter(`Promo/NPD?` == 'No') %>%
  separate(col = Month, into = c('Year', 'Month'), sep = '/') %>%
  mutate(Month = month.name[match(Month, month.abb)]) %>%
  mutate(MonthYear = paste(Month, Year)) %>%
  mutate(Country = ifelse(`US/INT` == 'United States', 'USA', 'CANADA')) %>%
  mutate('SKU Number' = gsub(' .*$', '', `Item Num/Desc`)) %>%
  mutate(Cost = round(`Order Cost $` / `Order Units`, 2)) %>%
  mutate(Division = 'OUR') %>%
  mutate('Category (SRP5)' = 'RECREATION') %>%
  mutate('Sales Rep Key' = paste0(Country, Customer, `Category (SRP5)`)) %>%
  select(Year, Month, MonthYear, Country, `SKU Number`, `Sales Rep Key`, Cost, Division, Customer, `Category (SRP5)`, Category, `Item Num/Desc`, `Forecast Units`, `Forecast Cost $`, `Order Units`, `Order Cost $`, `Reason For Adjustment`, `Forecast Unit Adjustment`, `Forecast Units Corrected`, `Forecast Cost Corrected`, `Order Unit Adjustment`, `Order Units Corrected`, `Order Cost Corrected`)

#updating column names
names(ty_acc) <- c('Year', 'Month', 'MonthYear', 'Country', 'SKU Number', 'Sales Rep Key', 'Cost', 'Division', 'Customer', 'Category (SRP 5)', 'Sub-Category (SRP 7)', 'SKU', 'LT Forecast - Units', 'LT Forecast - Cost', 'Orders - Units', 'Orders - Cost', 'Adjustment Request', 'Forecast Adjustments', 'Adjusted FC - Units', 'Adjusted FC - Cost', 'Order Adjustments', 'Adjusted Orders - Units', 'Adjusted Orders - Cost')

#cleaning up a few columns and calculated columns
ty_acc <- ty_acc %>%
  mutate('Adjusted FC - Cost' = round(`Adjusted FC - Cost`, 0),
         'Adjusted Orders - Cost' = round(`Adjusted Orders - Cost`, 0),
         'Error - Units' = `Adjusted Orders - Units` - `Adjusted FC - Units`,
         'ABS Error - Units' = abs(`Error - Units`),
         'ABS Percent Error - Units' = round((`ABS Error - Units` / `Adjusted Orders - Units`),2),
         'Accuracy - Units' = round(pmax(1-`ABS Percent Error - Units`, 0),2),
         'Bias - Units' = round((`Error - Units` / `Adjusted Orders - Units`) * -1, 2),
         'Error - Cost' = `Adjusted Orders - Cost` - `Adjusted FC - Cost`,
         'ABS Error - Cost' = abs(`Error - Cost`),
         'ABS Percent Error - Cost' = round(`ABS Error - Cost` / `Adjusted Orders - Cost`, 2),
         'Accuracy - Cost' = round(pmax(1 - `ABS Percent Error - Cost`, 0), 2),
         'Bias - Cost' = round((`Error - Cost` / `Adjusted Orders - Cost`) * -1, 2))
         
  

#Cleaning up last year's data    ----------------------------------------------
ly_acc <- ly_acc %>%
  filter(`Promo/NPD?` == 'No') %>%
  separate(col = Month, into = c('Year', 'Month'), sep = '/') %>%
  mutate(Month = month.name[match(Month, month.abb)]) %>%
  mutate(MonthYear = paste(Month, Year)) %>%
  mutate(Country = ifelse(`US/INT` == 'United States', 'USA', 'CANADA')) %>%
  mutate('SKU Number' = gsub(' .*$', '', `Item Num/Desc`)) %>%
  mutate(Cost = round(`Order Cost $` / `Order Units`, 2)) %>%
  mutate(Division = 'OUR') %>%
  mutate('Category (SRP5)' = 'RECREATION') %>%
  mutate('Sales Rep Key' = paste0(Country, Customer, `Category (SRP5)`)) %>%
  select(Year, Month, MonthYear, Country, `SKU Number`, `Sales Rep Key`, Cost, Division, Customer, `Category (SRP5)`, Category, `Item Num/Desc`, `Forecast Units`, `Forecast Cost $`, `Order Units`, `Order Cost $`, `Reason For Adjustment`, `Forecast Unit Adjustment`, `Forecast Units Corrected`, `Adjusted Forecast Cost`, `Order Unit Adjustment`, `Order Units Corrected`, `Adjusted Order Cost`)

#updating column names
names(ly_acc) <- c('Year', 'Month', 'MonthYear', 'Country', 'SKU Number', 'Sales Rep Key', 'Cost', 'Division', 'Customer', 'Category (SRP 5)', 'Sub-Category (SRP 7)', 'SKU', 'LT Forecast - Units', 'LT Forecast - Cost', 'Orders - Units', 'Orders - Cost', 'Adjustment Request', 'Forecast Adjustments', 'Adjusted FC - Units', 'Adjusted FC - Cost', 'Order Adjustments', 'Adjusted Orders - Units', 'Adjusted Orders - Cost')

#cleaning up a few columns and calculated columns
ly_acc <- ly_acc %>%
  mutate('Adjusted FC - Cost' = round(`Adjusted FC - Cost`, 0),
         'Adjusted Orders - Cost' = round(`Adjusted Orders - Cost`, 0),
         'Error - Units' = `Adjusted Orders - Units` - `Adjusted FC - Units`,
         'ABS Error - Units' = abs(`Error - Units`),
         'ABS Percent Error - Units' = round((`ABS Error - Units` / `Adjusted Orders - Units`),2),
         'Accuracy - Units' = round(pmax(1-`ABS Percent Error - Units`, 0),2),
         'Bias - Units' = round((`Error - Units` / `Adjusted Orders - Units`) * -1, 2),
         'Error - Cost' = `Adjusted Orders - Cost` - `Adjusted FC - Cost`,
         'ABS Error - Cost' = abs(`Error - Cost`),
         'ABS Percent Error - Cost' = round(`ABS Error - Cost` / `Adjusted Orders - Cost`, 2),
         'Accuracy - Cost' = round(pmax(1 - `ABS Percent Error - Cost`, 0), 2),
         'Bias - Cost' = round((`Error - Cost` / `Adjusted Orders - Cost`) * -1, 2))



#combining files into one   ----------------------------------------
final <- rbind(ly_acc, ty_acc)

#saving file
write.csv(final, "S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Gerber Accuracy Report Raw Data.csv", row.names = FALSE)





#-----------------------  Combining FA and Gerber into one raw data file for combined accuracy report ----------

#loading FA data
FA <- read_excel('S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Accuracy Report Raw Data.xlsx',
                 col_types = c(rep('text', 6), 'numeric', rep('text', 5), rep('numeric', 4), 'text', rep('numeric', 16)))

#combinging into one df
comb_df <- rbind(FA, final)

#fixing the customer column
comb_df$Customer <- toupper(comb_df$Customer)
comb_df$Customer[comb_df$Customer == "ALL OTHER - (G45)"] <- "ALL OTHER"
comb_df$Customer <- trimws(comb_df$Customer)

#additional data cleaning  
comb_df$Customer <- ifelse(grepl("DO IT BEST", comb_df$Customer), "DO IT BEST", comb_df$Customer)
comb_df$Customer <- ifelse(grepl("MEIJER", comb_df$Customer), "MEIJER", comb_df$Customer)
comb_df$Customer <- ifelse(grepl("ORGILL", comb_df$Customer), "ORGILL", comb_df$Customer)
comb_df$Customer <- ifelse(grepl("TRUE VALUE", comb_df$Customer), "TRUE VALUE", comb_df$Customer)
comb_df$Customer <- str_trim(comb_df$Customer, "both")

#saving file as excel file
#write.xlsx(comb_df, 'S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Accuracy Report Raw Data Combined.xlsx', sheetName = 'Raw Data')

write.csv(comb_df, 'S://Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Accuracy Report Raw Data Combined.csv', row.names = FALSE)



