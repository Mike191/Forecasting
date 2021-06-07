#This script imports the RawData and Deltas sheets from the 'reworking the month end raw data' template
#It reworks the data and saves the following 5 files:
# 1. A version of the current month raw data in the monthly folders on the S drive  - ex filename: 08 - Reworked Month End Raw Data - August 2018 - new format.csv
# 2. Updates the "Current Month End Raw Data (reworked).csv" file on the S drive
# 3. Updates the "Current Month End Deltas (reworked).csv" file
# 4. Updates the "Last_Month_Raw_Data" file which feeds the last month data when the month end file is updated each month
# 5. Creates a file in the old format for the mid-month meeting named "ME Data for Mid Month Sheeet"

# - Will be adding a full year file in 2019, but that's not currently possible with half the data for 2018 in a different format (and with All Other not broken out by sales manager)

#Instructions:
#     - Must update the template by pulling the data from the month end file and pasting it into the template
#           - Template name = 'Template - Reworking month end data'
#           - Located in the 'Month End Raw Data' folder 
#           - There's an instructions tab in the template
#     - After template is updated and saved, run the script and the file will be created and saved on the S drive


# %% importing packages and data

#Importing Pandas
import pandas as pd
from datetime import datetime
import calendar

#Importing the RawData and delta tabs
rawdata = pd.read_excel("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Template - Reworking month end data.xlsx", sheet_name = "RawData")
deltas = pd.read_excel("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Template - Reworking month end data.xlsx", sheet_name = "Deltas")

#%%fixing data types and filtering prior month data if it's in the file

#fixing data types and getting rid of the time stamp
rawdata['ME_Version_Date'] = rawdata['ME_Version_Date'].dt.strftime('%m/%d/%Y')
rawdata['FC_Date'] = rawdata['FC_Date'].dt.strftime('%m/%d/%Y')
deltas['ME_Version_Date'] = deltas['ME_Version_Date'].dt.strftime('%m/%d/%Y')
deltas['FC_Date'] = deltas['FC_Date'].dt.strftime('%m/%d/%Y')

#Fixing the Month_Year column because it imports as a date
rawdata['Month_Year'] = rawdata['Month'] + rawdata.Year.map(str)
deltas['Month_Year'] = deltas['Month'] + deltas.Year.map(str)

#filtering out last month's data if it's in there (it shouldn't be but just in case)
rawdata = rawdata.loc[rawdata['ME_Version_Date'] == max(rawdata['ME_Version_Date'])]
deltas = deltas.loc[deltas['ME_Version_Date'] == max(deltas['ME_Version_Date'])]

# %% creating three temporary dataframes for lookups

#creating a temporary dataframe and filling in the director column in the delta table
temp_dir = rawdata[['Sales_Manager', 'Director']]
temp_dir = temp_dir.drop_duplicates()
temp_dir = temp_dir.set_index('Sales_Manager')
deltas['Director'] = deltas.Sales_Manager.map(temp_dir.Director)

#creating a temporary dataframe for updaing the conversion rate column in the total year file
#this code is for updating conversion rates on a full year file
#not currently being used
#temp_conv = rawdata[['Country', 'Month_Year', 'Conversion_Rate']]
#temp_conv['key'] = temp_conv['Country'] + temp_conv['Month_Year']
#temp_conv = temp_conv[['Conversion_Rate', 'key']]
#temp_conv = temp_conv.set_index(['key'])
#temp_conv = temp_conv.loc[~temp_conv.index.duplicated(keep = 'first')]

# %% reworking the data, saving the deltas file and saving the current month end file in both locations
#rearranging the deltas dataframe to match the rawdata column order
deltas = deltas[['ME_Version_Date', 'Country', 'Division', 'Sales_Manager', 'Director', 'Customer', 'SRP5', 'SRP7', 'Year', 'Month', 'FC_Date', 'Month_No', 'Shipments', 'Forecast', 'Total_Possible', 'Delta', 'Actl_TP', 'Actl_TP_Fcst', 'Conversion_Rate', 'Month_Year', 'Key', 'Current_ME_Version', 'Current_Month']]

#filling in nas with zeros in value columns
deltas = deltas.fillna(0)

#writing current month deltas file (#3 in the instructions at the top)
deltas.to_csv("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Current Month End Deltas (reworked).csv", index = False, encoding = 'utf-8-sig')

#adding the deltas to the raw data file
rawdata = rawdata.append(deltas)

#grouping data and adding deltas to the Actl_TP_Fcst line to get a final line with everything called Actl_TP_Fcst_Delta
rawdata = rawdata.groupby(['ME_Version_Date', 'Country', 'Division', 'Sales_Manager', 'Director', 'Customer', 'SRP5', 'SRP7', 'Year', 'Month', 'FC_Date', 'Month_Year', 'Key', 'Month_No', 'Current_ME_Version', 'Current_Month', 'Conversion_Rate'], as_index = False)[['Delta', 'Forecast', 'Shipments','Total_Possible','Actl_TP', 'Actl_TP_Fcst']].sum()
rawdata['Actl_TP_Fcst_Delta'] = rawdata['Actl_TP_Fcst'] + rawdata['Delta']

#rearranging columns
rawdata = rawdata[['ME_Version_Date', 'Country', 'Division', 'Sales_Manager', 'Director', 'Customer', 'SRP5', 'SRP7', 'Year', 'Month', 'FC_Date', 'Month_No', 'Shipments', 'Forecast', 'Total_Possible', 'Delta', 'Actl_TP', 'Actl_TP_Fcst', 'Actl_TP_Fcst_Delta', 'Conversion_Rate', 'Month_Year', 'Key', 'Current_ME_Version', 'Current_Month']]

#saving current month file (#2 in the instructions at the top) to the S drive
rawdata.to_csv("S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Current Month End Raw Data (reworked).csv", index = False, encoding = 'utf-8-sig')

#creating a file name for the monthly file that goes into the folder with the raw data by month
me_date = max(rawdata['ME_Version_Date'])
me_date = datetime.strptime(me_date, '%m/%d/%Y').date()
month_num = me_date.strftime('%m')
month_name = me_date.strftime('%B')
cur_year = me_date.strftime('%Y')
filename = month_num + str('-') + str('Reworked Month End Raw Data - ') + month_name + str(' ') + cur_year + str('.csv')
filepath = str('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/') + cur_year + str(' Monthly Raw Data/Reworked (csv)')
final_file_name = filepath + str('/') + filename

#writing current month data to the monthly folder (#1 in the instructions at the top)
rawdata.to_csv(final_file_name, index = False, encoding = 'utf-8-sig')

#removing the Actl_TP_Fcst_Delta column to create a file for the last month data for next month's update (#4 in the instructions at the top)
last_month_raw_data = rawdata
last_month_raw_data = last_month_raw_data.drop('Actl_TP_Fcst_Delta', 1)
last_month_raw_data.to_csv('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Raw Data for New Month End Sheet/Last_Month_Raw_Data.csv', index = False, encoding = 'utf-8-sig')


#creating a last month raw data file for Jennifer's report
last_mon_num = str(int(month_num) - 1).zfill(2)
last_mon_name = calendar.month_name[int(last_mon_num)]

#figuring out folder year
if int(month_num) > 1:
    folder_year = cur_year
else:
    folder_year = str(int(cur_year) -1)
    
#reading last month file
last_month_file_name = last_mon_num + str('-Reworked Month End Raw Data - ') + last_mon_name + str(' ') + folder_year + str('.csv')
last_month_file_path = str('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/') + folder_year + str(' Monthly Raw Data/Reworked (csv)/') + last_month_file_name
last_month_file = pd.read_csv(last_month_file_path)

#writing last month file to a different foler
last_month_file.to_csv('S:/Fiskars Americas/Operations/Shared/SOP/Forecasting/Month End Sheet/Month End Raw Data/Last Month End Raw Data (reworked).csv', index = False, encoding = 'utf-8-sig')



# %% block of code for creating a full year file
#All Other wasn't broken out until May so previous data isn't at the same level so not creating a full year file at the time
#Maybe add to this in 2019

##############################

