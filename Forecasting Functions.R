#script to load forecasting functions

#loading packages
library(tidyverse)
library(forecast)
library(sweep)
library(timetk)
library(purrr)
library(lubridate)
library(zoo)
library(readxl)




# ========================================================================================

# ------------------------------ Function to convert to time series  ---------------------

# ========================================================================================

#will return a dataframe with a nested column with the history for each SKU as a time series
history_to_ts <- function(df) {
  #renaming column 1 to 'SKU'
  colnames(df)[1] <- 'SKU'
  
  #getting rid of column 2
  df <- df %>%
    dplyr::select(-2)
  
  #reshaping data
  df <- df %>%
    gather(key = Date,
           value = History,
           -SKU) 
  
  #filling in NAs with zeros because NAs can cause issues with some models
  df$History[is.na(df$History)] <- 0
  
  #converting the Date column to a date format
  df$Date <- as.Date(df$Date, format = '%m/%d/%Y') 
  
  #grabbing the start year and start month of our data
  start_year <- lubridate::year(min(df$Date))
  start_month <- lubridate::month(min(df$Date))  
  
  #nesting the demand history into a new column called History
  df <- df %>%
    group_by(SKU) %>%
    nest()  
  
  #code to convert the Nested_History column into a time series
  df <- df %>%
    mutate(Nested_History = map(.x = data,   
                                .f = tk_ts,           
                                select = -Date,       
                                start = c(start_year, start_month),  
                                freq = 12)) %>%
    dplyr::select(-data)
  
  return(df)
}




# ======================================================================================

# -------------------- Function to build models and create forecasts  ------------------

# ======================================================================================


build_models <- function(df, models = list(), model_names = list()) {

  #starting list of final column names
  final_col_names <- c('SKU', 'Nested_History')
  
  #looping through the list of models
  for (t in 1:length(models)) {
    
    #creating a new data frame with teh SKU column to add forecasts to
    new_df <- tibble(SKU = df$SKU, Nested_History = df$Nested_History)
    
    #getting the model name
    model <- models[t]
    
    #printing status
    print(paste('Building', model_names[t]))

    #building model and creating a forecast
    new_df <- new_df %>%
        mutate(fcst = map(.x = Nested_History,
                          .f = model))
      
    #creating column name list
    final_col_names <- append(final_col_names, model_names[t])
    
    #getting rid of SKU column on df to bind it with the final dataframe
    new_df <- new_df %>%
      dplyr::select(-Nested_History)

    #adding lead time forecast to the new df
    df <- merge(df, new_df, by = 'SKU')
    
    } 

    #changing column names
    colnames(df) <- final_col_names
    print('Building models complete.')
    return(df)
  }
  
 




# ======================================================================================

# -------------------- Function to create test set forecasts  --------------------------

# ======================================================================================

#function for creating historical lead time forecasts
#currently cannot handle xregs
create_test_set_forecasts <- function(df, models = list(), model_names = list(), months = 3, lead_time = 3) {

  #creating a new data frame with the SKU column to add forecasts to
  final_df <- data_frame(SKU = df$SKU)
  
  #starting list of final column names
  final_col_names <- c('SKU')

  #looping through the list of models
  for (t in 1:length(models)) {
    
    #creating a new data frame with teh SKU column to add forecasts to
    new_df <- data_frame(SKU = df$SKU)
    
    #getting the model name
    model <- models[t]
    
    #getting length of the time series and setting start and end for subsetting
    l <- length(df$Nested_History[[1]])
    s <- l - lead_time - months
    e <- l - sum(lead_time + 1)
    col_name_list <- c('SKU')
    
    #function to subset time series
    ts_subset <- function(ts, end) {
      subset(ts, end = end)
    }
    
    #function to grab the lead time forecast
    lt_func <- function(fcst) {
      sw_sweep(round(fcst$mean)[[sum(lead_time + 1)]], 0)
    }
    
    #looping through subsetting time series to create a lead time forecast
    for (i in s:e) {
      train <- df %>%
        mutate(Nested_History = map(.x = Nested_History,
                                    .f = ts_subset,
                                    end = i))
      

      #creating a forecast
      train <- train %>%
        mutate(fcst = map(.x = Nested_History,
                          .f = model))
      
      #creating column name
      c <- as.yearmon(time(train$fcst[[1]]$mean))[sum(lead_time + 1)]
      c <- as.character(c)
      c <- gsub(' ', '_', c)
      col_name_list <- append(col_name_list, c)
      
      #printing forecasts its working on
      print(paste('Creating forecasts for', model_names[t], 'for', c))
      
      #grabbing lead time forecast
      train <- train %>%
        mutate(fc = map(.x = fcst,
                       .f = lt_func)) %>%
        ungroup() %>%
        dplyr::select(fc)

      #adding lead time forecast to the new df
      new_df <- cbind(new_df, train)
    } 
    
    #getting rid of negatives
    for (j in 2:ncol(new_df)) {
      new_df[j][new_df[j] < 0] <- 0
      }

    #adding new col name for nested forecasts
    model_name <- model_names[t]
    nested_col_name <- paste0(model_name, '_Test_Set')
    final_col_names <- append(final_col_names, nested_col_name)
    
    #changing column names
    colnames(new_df) <- col_name_list
    
    #reshaping new_df
    new_df <- new_df %>%
      gather(key = Month, value = Forecast, -SKU)
    
    #converting forecast column to numbers instead of a list
    new_df$Forecast <- as.numeric(new_df$Forecast)
    
    #nesting
    new_df <- new_df %>%
      group_by(SKU) %>%
      nest() %>%
      ungroup() %>%
      dplyr::select(-SKU)
    
    #adding new nested test set to the final dataframe
    final_df <- cbind(final_df, new_df)
    
  }
  
  colnames(final_df) <- final_col_names
  return(final_df)
  print('Test set forecasts complete.')
  
}







# =====================================================================

# ---------------- Function to get accuracy data ----------------------

# =====================================================================

get_accuracy_data <- function(customer, division = list(), country) {
    acc <- read_excel('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Accuracy Report Raw Data.xlsx', sheet = 'Raw Data')
    stat_fc <- read_excel('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Accuracy Raw Data/Stat Accuracy Report Raw Data.xlsx', sheet = 'Raw Data')
    
    #selecting columns and filtering
    acc <- acc %>%
      dplyr::select(Year, Month, Country, `SKU Number`, Cost, Division, Customer, `Adjusted FC - Units`, `Adjusted FC - Cost`, `Adjusted Orders - Units`, `Adjusted Orders - Cost`) %>%
      filter(Customer == customer &
               Division %in% division &
               Country == country)
    
    #selecting columns and filtering for stat
    stat_fc <- stat_fc %>%
      dplyr::select(Year, Month, Country, `SKU Number`, Division, Customer, `Stat Forecast Units`, `Stat Forecast Cost`)
    
    #fixing column names
    colnames(acc) <- c('Year', 'Month', 'Country', 'SKU', 'Cost', 'Division', 'Customer', 'Adj_FC_Units', 'Adj_FC_Cost', 'Adj_Orders_Units', 'Adj_Orders_Cost')
    colnames(stat_fc) <- c('Year', 'Month', 'Country', 'SKU', 'Division', 'Customer', 'Stat_FC_Units', 'Stat_FC_Cost')
    
    #creating month_year column
    acc <- acc %>%
      mutate(Month_Year = paste0(substr(Month, 1, 1), tolower(substr(Month, 2, 3)), "_", as.character(Year))) %>%
      mutate(Cost = round(Cost, 2),
             Adj_FC_Cost = round(Adj_FC_Cost, 0),
             Adj_Orders_Cost = round(Adj_Orders_Cost, 0)) %>%
      dplyr::select(Year, Month, Month_Year, Country, SKU, Cost, Division, Customer, Adj_FC_Units, Adj_FC_Cost, Adj_Orders_Units, Adj_Orders_Cost) %>%
      mutate(Lookup = paste0(Month_Year, Country, Customer, SKU))
    
    #creating month_year column for stat
    stat_fc <- stat_fc %>%
      mutate(Month_Year = paste0(substr(Month, 1, 1), tolower(substr(Month, 2, 3)), "_", as.character(Year))) %>%
      mutate(Stat_FC_Units = round(Stat_FC_Units, 0),
             Stat_FC_Cost = round(Stat_FC_Cost, 0)) %>%
      mutate(Lookup = paste0(Month_Year, Country, Customer, SKU)) %>%
      dplyr::select(Lookup, Stat_FC_Units, Stat_FC_Cost)
    
    #joining tables and getting rid of the lookup column
    acc <- merge(acc, stat_fc, by = 'Lookup') %>%
      dplyr::select(-Lookup)
    
    #cleaning up any NAs in stat
    acc$Stat_FC_Units[is.na(acc$Stat_FC_Units)] <- 0
    acc$Stat_FC_Cost[is.na(acc$Stat_FC_Cost)] <- 0
    
    return(acc)
}






# ===================================================================================

# ---------------- Function to calculate test set accuracy  -------------------------

# ===================================================================================

#takes in dataframe with nested lead time forecasts and the accuracy data
#returns 2 dataframes - 1 with nested accuracy data by SKU and 1 overall summary of the model totals
test_set_accuracy <- function(test_set, accuracy) {

  #creating empty data frame for summary data frame
  accuracy_summary <- data_frame()
  
  #creating empty data frame for sku level data
  sku_accuracy_data <- data_frame(SKU = test_set$SKU)
  
  #creating empty data frame for sku summary data
  sku_summary_data <- data_frame(SKU = test_set$SKU)
  
  #starting list of column names for the sku_accuracy_data dataframe
  final_col_names <- c('SKU')
  
  #filtering sku accuracy data to only skus in the accuracy file
  sku_accuracy_data <- sku_accuracy_data %>%
    filter(SKU %in% acc$SKU)
  
  sku_summary_data <- sku_summary_data %>%
    filter(SKU %in% acc$SKU)
  
  
  #---------------------------loop -------------------------------
  
  #looping through the test set columns to measure accuracy
  num_test_cols <- ncol(test_set)
  
  for (i in 2:num_test_cols) {
    
    #grabbing column name and appending it to the final_col_name list
    col_name <- names(test_set[i])
    final_col_names <- append(final_col_names, col_name)
    
    #creating temporary dataframe with one model of data
    temp_test_df <- test_set %>%
      dplyr::select(c(1, i))
    
    colnames(temp_test_df) <- c('SKU', 'Forecasts')

    #unnesting model forecasts
    temp_test_df <- temp_test_df %>%
      unnest(cols = c(Forecasts))

    #changing column names
    colnames(temp_test_df) <- c('SKU', 'Month_Year', 'Test_Fcst_Units')
    
    #grabbing list of SKUs not in accuracy file
    not_in_acc <- setdiff(temp_test_df$SKU, accuracy$SKU)
    
    #filtering temp df on only SKUs in the accuracy file
    temp_test_df <- temp_test_df %>%
      filter(SKU %in% acc$SKU)
    
    #creating a SKU_monthyear column to join with accuracy data to make calculations
    temp_test_df <- temp_test_df %>%
      mutate(Lookup = paste0(SKU, "_", Month_Year))
    
    #creating the same column in the acc file
    accuracy_temp <- accuracy %>%
      mutate(Lookup = paste0(SKU, "_", Month_Year)) %>%
      dplyr::select(Lookup, Cost, Adj_FC_Units, Adj_FC_Cost, Adj_Orders_Units, Adj_Orders_Cost, Stat_FC_Units, Stat_FC_Cost)
    
    #joining accuracy data to do calculations
    temp_test_df <- temp_test_df %>%
      left_join(accuracy_temp, by = 'Lookup')
    
    #calculating accuracy measures by SKU
    temp_test_df <- temp_test_df %>%
      mutate(Test_Fcst_Cost = round(Test_Fcst_Units * Cost, 0),
             Sales_Fcst_Error_Units = Adj_Orders_Units - Adj_FC_Units,
             Sales_Fcst_ABS_Error_Units = abs(Sales_Fcst_Error_Units),
             Sales_Fcst_Acc_Units = round((1-(Sales_Fcst_ABS_Error_Units/Adj_Orders_Units)) * 100, 2),
             Sales_Fcst_Error_Cost = Adj_Orders_Cost - Adj_FC_Cost,
             Sales_Fcst_ABS_Error_Cost = abs(Sales_Fcst_Error_Cost),
             Sales_Fcst_Acc_Cost = round((1-(Sales_Fcst_ABS_Error_Cost/Adj_Orders_Cost)) * 100, 2),
             Dem_Fcst_Error_Units = Adj_Orders_Units - Stat_FC_Units,
             Dem_Fcst_ABS_Error_Units = abs(Dem_Fcst_Error_Units),
             Dem_Fcst_Acc_Units = round((1-(Dem_Fcst_ABS_Error_Units/Adj_Orders_Units)) * 100, 2),
             Dem_Fcst_Error_Cost = Adj_Orders_Cost - Stat_FC_Cost,
             Dem_Fcst_ABS_Error_Cost = abs(Dem_Fcst_Error_Cost),
             Dem_Fcst_Acc_Cost = round((1-(Dem_Fcst_ABS_Error_Cost/Adj_Orders_Cost)) * 100, 2),
             Test_Fcst_Error_Units = Adj_Orders_Units - Test_Fcst_Units,
             Test_Fcst_ABS_Error_Units = abs(Test_Fcst_Error_Units),
             Test_Fcst_Acc_Units = round((1-(Test_Fcst_ABS_Error_Units/Adj_Orders_Units)) * 100, 2),
             Test_Fcst_Error_Cost = Adj_Orders_Cost - Test_Fcst_Cost,
             Test_Fcst_ABS_Error_Cost = abs(Test_Fcst_Error_Cost),
             Test_Fcst_Acc_Cost = round((1-(Test_Fcst_ABS_Error_Cost/Adj_Orders_Cost)) * 100, 2)) 
    

    #adding total abs error cost and total acc for summary and comparison
    temp_test_df <- temp_test_df %>%
      group_by(SKU) %>%
      mutate(Total_Adj_Orders_Cost = sum(Adj_Orders_Cost, na.rm = T),
             Sales_Fcst_Total_ABS_Error_Cost = sum(Sales_Fcst_ABS_Error_Cost, na.rm = T),
             Sales_Fcst_Total_Acc_Cost = round((1-(Sales_Fcst_Total_ABS_Error_Cost/Total_Adj_Orders_Cost)) * 100, 2),
             Dem_Fcst_Total_ABS_Error_Cost = sum(Dem_Fcst_ABS_Error_Cost, na.rm = T),
             Dem_Fcst_Total_Acc_Cost = round((1-(Dem_Fcst_Total_ABS_Error_Cost/Total_Adj_Orders_Cost)) * 100, 2),
             Test_Fcst_Total_ABS_Error_Cost = sum(Test_Fcst_ABS_Error_Cost, na.rm = T),
             Test_Fcst_Total_Acc_Cost = round((1-(Test_Fcst_Total_ABS_Error_Cost/Total_Adj_Orders_Cost)) * 100, 2)) %>%
      ungroup()

    #pulling out total accuracy per SKU for sku summary
    temp_sku_summary <- temp_test_df %>%
      dplyr::select(SKU, Dem_Fcst_Total_Acc_Cost, Sales_Fcst_Total_Acc_Cost, Test_Fcst_Total_Acc_Cost)
    
    temp_sku_summary <- unique(temp_sku_summary)
    
    colnames(temp_sku_summary) <- c('SKU','Demantra_Acc_Cost', 'Sales_Acc_Cost', paste0(col_name, '_Acc_Cost'))
    
    temp_sku_summary <- temp_sku_summary %>%
      dplyr::select(-SKU)

    
    
    # -----------------  Creating summary for Test Set  ---------------------------------
    
    #creating overall total dataframe
    by_month_test <- temp_test_df %>%
      group_by(Month_Year) %>%
      mutate(ABS_Error_Units = sum(Test_Fcst_ABS_Error_Units, na.rm = T),
             Acc_Units = round((1-(ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T))) * 100, 2),
             Error_Cost = sum(Adj_Orders_Cost - Test_Fcst_Cost, na.rm = T),
             ABS_Error_Cost = sum(Test_Fcst_ABS_Error_Cost, na.rm = T),
             Acc_Cost = round((1-(ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T))) * 100, 2)) %>%
      ungroup()
    
    #test units by month dataframe
    test_units <- by_month_test %>%
      dplyr::select(Month_Year, Acc_Units)
    
    #changing column names and getting rid of duplicates
    colnames(test_units) <- c('Col', 'Value')
    test_units <- unique(test_units)
    test_units$Col <- paste0(test_units$Col, "_units")
      
    #creating totals for units df
    test_totals_units <- temp_test_df %>%
      mutate(Total_Error_Units = round(sum(Test_Fcst_Error_Units, na.rm = T), 0),
             Total_ABS_Error_Units = round(sum(Test_Fcst_ABS_Error_Units, na.rm = T), 0),
             Total_Acc_Units = round((1-Total_ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Units, Total_ABS_Error_Units, Total_Acc_Units) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
    
    #test cost by month
    test_cost <- by_month_test %>%
      dplyr::select(Month_Year, Acc_Cost)
    
    #changing column names and getting rid of duplicates
    colnames(test_cost) <- c('Col', 'Value')
    test_cost <- unique(test_cost) 
    test_cost$Col <- paste0(test_cost$Col, "_cost")
    
    #creating totals for cost df
    test_totals_cost <- temp_test_df %>%
      mutate(Total_Error_Cost = round(sum(Test_Fcst_Error_Cost, na.rm = T), 0),
             Total_ABS_Error_Cost = round(sum(Test_Fcst_ABS_Error_Cost, na.rm = T), 0),
             Total_Acc_Cost = round((1-Total_ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Cost, Total_ABS_Error_Cost, Total_Acc_Cost) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    test_units <- bind_rows(test_units, test_totals_units) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Units = 'Units', .before = 1) 
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    test_cost <- bind_rows(test_cost, test_totals_cost) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Cost = 'Cost', .before = 1) %>%
      add_column(Model = col_name, .before = 1)
    
    #joining units and cost into one dataframe
    test_units <- cbind(test_cost, test_units)
    
    
    # --------------   Creating summary data for Sales Fcst -------------------
    #creating temporary dataframes for sales forecast accuracy
    
    #creating overall total dataframe
    by_month_temp_sales <- temp_test_df %>%
      group_by(Month_Year) %>%
      mutate(ABS_Error_Units = sum(Sales_Fcst_ABS_Error_Units, na.rm = T),
             Acc_Units = round((1-(ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T))) * 100, 2),
             Error_Cost = sum(Adj_Orders_Cost - Adj_FC_Cost, na.rm = T),
             ABS_Error_Cost = sum(Sales_Fcst_ABS_Error_Cost, na.rm = T),
             Acc_Cost = round((1-(ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T))) * 100, 2)) %>%
      ungroup()
    
    #temp units by month dataframe
    sales_units <- by_month_temp_sales %>%
      dplyr::select(Month_Year, Acc_Units)
    
    #changing column names and getting rid of duplicates
    colnames(sales_units) <- c('Col', 'Value')
    sales_units <- unique(sales_units) 
    sales_units$Col <- paste0(sales_units$Col, "_units")
    
    #creating totals for units df
    sales_totals_units <- temp_test_df %>%
      mutate(Total_Error_Units = round(sum(Sales_Fcst_Error_Units, na.rm = T), 0),
             Total_ABS_Error_Units = round(sum(Sales_Fcst_ABS_Error_Units, na.rm = T), 0),
             Total_Acc_Units = round((1-Total_ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Units, Total_ABS_Error_Units, Total_Acc_Units) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
             
    #sales cost by month
    sales_cost <- by_month_temp_sales %>%
      dplyr::select(Month_Year, Acc_Cost)
    
    #changing column names and getting rid of duplicates
    colnames(sales_cost) <- c('Col', 'Value')
    sales_cost <- unique(sales_cost)  
    sales_cost$Col <- paste0(sales_cost$Col, "_cost")
    
    #creating totals for cost df
    sales_totals_cost <- temp_test_df %>%
      mutate(Total_Error_Cost = round(sum(Sales_Fcst_Error_Cost, na.rm = T), 0),
             Total_ABS_Error_Cost = round(sum(Sales_Fcst_ABS_Error_Cost, na.rm = T), 0),
             Total_Acc_Cost = round((1-Total_ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Cost, Total_ABS_Error_Cost, Total_Acc_Cost) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    sales_units <- bind_rows(sales_units, sales_totals_units) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Units = 'Units', .before = 1)
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    sales_cost <- bind_rows(sales_cost, sales_totals_cost) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Cost = 'Cost', .before = 1) %>%
      add_column(Model = 'Sales_Forecast', .before = 1)
    
    #joining units and cost into one dataframe
    sales_units <- cbind(sales_cost, sales_units)
    
    
    # --------------   Creating summary data for Demantra Stat Fcst -------------------
    #creating temporary dataframes for Demantra stat forecast accuracy
    
    #creating overall total dataframe
    by_month_temp_dem <- temp_test_df %>%
      group_by(Month_Year) %>%
      mutate(ABS_Error_Units = sum(Dem_Fcst_ABS_Error_Units, na.rm = T),
             Acc_Units = round((1-(ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T))) * 100, 2),
             Error_Cost = sum(Adj_Orders_Cost - Adj_FC_Cost, na.rm = T),
             ABS_Error_Cost = sum(Dem_Fcst_ABS_Error_Cost, na.rm = T),
             Acc_Cost = round((1-(ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T))) * 100, 2)) %>%
      ungroup()
    
    #temp units by month dataframe
    dem_units <- by_month_temp_dem %>%
      dplyr::select(Month_Year, Acc_Units)
    
    #changing column names and getting rid of duplicates
    colnames(dem_units) <- c('Col', 'Value')
    dem_units <- unique(dem_units) 
    dem_units$Col <- paste0(dem_units$Col, "_units")
    
    #creating totals for units df
    dem_totals_units <- temp_test_df %>%
      mutate(Total_Error_Units = round(sum(Dem_Fcst_Error_Units, na.rm = T), 0),
             Total_ABS_Error_Units = round(sum(Dem_Fcst_ABS_Error_Units, na.rm = T), 0),
             Total_Acc_Units = round((1-Total_ABS_Error_Units/sum(Adj_Orders_Units, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Units, Total_ABS_Error_Units, Total_Acc_Units) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
    
    #sales cost by month
    dem_cost <- by_month_temp_dem %>%
      dplyr::select(Month_Year, Acc_Cost)
    
    #changing column names and getting rid of duplicates
    colnames(dem_cost) <- c('Col', 'Value')
    dem_cost <- unique(dem_cost)
    dem_cost$Col <- paste0(dem_cost$Col, "_cost")
    
    #creating totals for cost df
    dem_totals_cost <- temp_test_df %>%
      mutate(Total_Error_Cost = round(sum(Dem_Fcst_Error_Cost, na.rm = T), 0),
             Total_ABS_Error_Cost = round(sum(Dem_Fcst_ABS_Error_Cost, na.rm = T), 0),
             Total_Acc_Cost = round((1-Total_ABS_Error_Cost/sum(Adj_Orders_Cost, na.rm = T)) * 100, 2)) %>%
      dplyr::select(Total_Error_Cost, Total_ABS_Error_Cost, Total_Acc_Cost) %>%
      slice(1) %>%
      gather(key = 'Col', value = 'Value')
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    dem_units <- bind_rows(dem_units, dem_totals_units) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Units = 'Units', .before = 1) 
    
    #combining all 4 dataframes into one and spreading so everything is in its own column
    dem_cost <- bind_rows(dem_cost, dem_totals_cost) %>%
      pivot_wider(names_from = Col, values_from = Value) %>%
      add_column(Cost = 'Cost', .before = 1) %>%
      add_column(Model = 'Demantra_Stat_Forecast', .before = 1)
    
    #joining units and cost into one dataframe
    dem_units <- cbind(dem_cost, dem_units)


    # ------------------ Combing Test and Sales into summary table -------------------------
    

    #adding data to the summary data frame
    accuracy_summary <- rbind(accuracy_summary, test_units)
    accuracy_summary <- rbind(accuracy_summary, sales_units)
    accuracy_summary <- rbind(accuracy_summary, dem_units)
    
    #nesting columns in sku level dataframe
    temp_test_df <- temp_test_df %>%
      dplyr::select(-Test_Fcst_Cost, - Lookup) %>%
      group_by(SKU) %>%
      nest() %>%
      ungroup() %>%
      dplyr::select(-SKU)
    
    #binding to the final sku level data dataframe
    sku_accuracy_data <- cbind(sku_accuracy_data, temp_test_df)
    
    sku_summary_data <- cbind(sku_summary_data, temp_sku_summary)
    
  }  #end of loop looping through each column in test set except sku column
  
  #getting rid of duplicate columns
  sku_summary_data <- sku_summary_data[, !duplicated(colnames(sku_summary_data))]
  
  #cleaning up any duplicates in the summary dataframe
  accuracy_summary <- unique(accuracy_summary) %>%
    arrange(desc(Total_Acc_Cost))
  
  #adding column names to the sku level dataframe
  colnames(sku_accuracy_data) <- final_col_names
  
  print(num_test_cols)
  
  #browser()
  
  #creating a best model column on the sku summary
  sku_summary_data <- sku_summary_data%>%
    mutate(Best_Model = colnames(sku_summary_data[,2:ncol(sku_summary_data)])[max.col(sku_summary_data[,2:ncol(sku_summary_data)])])
  
  #old code that caused error for Shaelyn
  # sku_summary_data <- sku_summary_data %>%
  #   mutate(Best_Model = colnames(sku_summary_data[,2:sum(num_test_cols,2)])[apply(sku_summary_data[, 2:sum(num_test_cols,2)], 1, which.max)])
  
  #assiging variables to the global environment
  assign('test_set_sku_details', sku_accuracy_data, .GlobalEnv)
  assign('test_set_model_summary', accuracy_summary, .GlobalEnv)
  assign('test_set_sku_summary', sku_summary_data, .GlobalEnv)
  assign('excluded_skus', not_in_acc, .GlobalEnv)

  #printing out list of SKUs that wwere excluded
  cat("======================================\n")
  cat("\n\n")
  print('The following SKUs were excluded because they weren\'t found in the accuracy report: ')
  cat("\n\n")
  print(not_in_acc)
  cat("\n\n====================================")
}
  




# ===================================================================================

# ---------------- Function to create an export file of final forecasts  ------------

# ===================================================================================

create_export <- function(df) {
  
  df <- df %>%
    dplyr::select(-Nested_History)
  
  #function to grab the mean forecast
  fc_func <- function(fcst) {
    sw_sweep(fcst$mean)
  }
  
  #mapping function to all rows
  df <- df %>%
    mutate(fc = map(.x = df[[2]],
                    .f = fc_func))
  
  #getting rid of negatives
  for (j in 1:nrow(df)) {
    df$fc[[j]][df$fc[[j]] < 0] <- 0
    }
  
  #creating column names
  col_list <- as.yearmon(time(df$fc[[1]]))
  x <- match(substr(col_list, 1, 3), month.abb)  #converting month to number
  y <- substr(col_list, 5, 9)  #grabbing year
  col_list <- paste0(x, '/1/', y)  #combining into a date format
  col_list <- c('Item Number', 'Series / Time', col_list)
  
  #creating dataframe for final output
  final_df <- tibble(SKU = df$SKU)
  final_df['Series / Time'] <- 'Custom Stat'


  #function to grab one month of forecast to put into a new column
  get_fc <- function(fc, start, end) {
    round(subset(fc, start = start, end = end), 0)
    }
  
  #looping through the length of the time series and getting each month one by one
  for (i in 1:length(df$fc[[1]])) {
    temp_df <- df %>%
      dplyr::select(SKU, fc)
    
    temp_df <- temp_df %>%
      mutate(new_fc = map(.x = fc, 
                       .f = get_fc,
                       start = i,
                       end = i))
    
    #selecting only the new column to add to final dataframe
    temp_df <- temp_df %>%
      dplyr::select(new_fc)
    
    final_df <- cbind(final_df, temp_df)
    
    
  }
  
  colnames(final_df) <- col_list
  final_df[3:ncol(final_df)] <- as.integer(unlist(final_df[3:ncol(final_df)]))
  return(final_df)
  
  }

