library(lubridate)
library(data.table)
library(dplyr)
library(beepr)

dirPath <- "datasets/"

#Delete files in folder if they exist 
do.call(file.remove, list(list.files(dirPath, full.names = TRUE)))

# Status of COVID-19 cases in Ontario
# This dataset compiles daily snapshots of publicly reported data on 2019 Novel Coronavirus (COVID-19) testing in Ontario.
# src: https://data.ontario.ca/en/dataset/status-of-covid-19-cases-in-ontario
#downloads the testing, hospitalization, and LTC data for the province wide response 
covidStatus <- read.csv(file = "https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv",
                        sep = ",", 
                        encoding = 'UTF-8')

#Clean by Date 

covidStatus$Reported.Date <- as.Date(covidStatus$Reported.Date)
#Sort by Date
covidStatus <- covidStatus %>% 
  dplyr::arrange(Reported.Date)

# Create URL for download
dirPath <- paste(getwd(), "/datasets/", sep = "")

# Export as RDS file 
fileName = paste(dirPath, 'Status of COVID-19 cases in Ontario.rds',sep = '')
saveRDS(covidStatus, file = fileName)


#print notification message to console 
# file.csv is downloaded, x rows by y columns
print(paste("Status of COVID-19 cases in Ontario has been dowloaded",
            format(nrow(covidStatus), big.mark = ","), "Rows x ", ncol(covidStatus), "Columns",  
            sep = " "))










#Confirmed positive cases of COVID19 in Ontario
# Compiled daily reported data from public health units on confirmed positive cases of COVID-19 in Ontario.
# src: https://data.ontario.ca/en/dataset/confirmed-positive-cases-of-covid-19-in-ontario
ConfirmedPositives <- fread(input = "https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv", 
                            sep = ",", 
                            encoding = "UTF-8", 
                            data.table = F)

ConfirmedPositives <- ConfirmedPositives %>% 
  dplyr::mutate(Accurate_Episode_Date = as.Date(Accurate_Episode_Date), 
                Case_Reported_Date = as.Date(Case_Reported_Date), 
                Test_Reported_Date = as.Date(Test_Reported_Date)) %>% 
  dplyr::arrange(Case_Reported_Date) %>% 
  dplyr::filter(!Accurate_Episode_Date == is.na(Accurate_Episode_Date)) %>% 
  dplyr::filter(!Accurate_Episode_Date < "2020-01-01")



# Set file name 
fileName = paste(dirPath, 'Confirmed positive cases of COVID-19 in Ontario.csv',sep = '')
# Write CSV File 
# write.csv(ConfirmedPositives, 
#           file = fileName, 
#           fileEncoding = 'UTF-8', 
#           row.names = F)

# Export as RDS file 
fileName = paste(dirPath, 'Confirmed positive cases of COVID-19 in Ontario.rds',sep = '')
saveRDS(ConfirmedPositives, file = fileName)

#print notification message to console 
# file.csv is downloaded, x rows by y columns
print(paste("Confirmed positive cases of COVID-19 in Ontario has been dowloaded",
            format(nrow(ConfirmedPositives), big.mark = ","), "Rows x ", ncol(ConfirmedPositives), "Columns",  
            sep = " "))





# Ontario COVID-19 outbreaks data
# This dataset compiles daily snapshots of publicly reported data on 2019 Novel Coronavirus (COVID-19) outbreaks in Ontario
# src: https://data.ontario.ca/en/dataset/ontario-covid-19-outbreaks-data
url <- "https://data.ontario.ca/dataset/5472ffc1-88e2-48ca-bc9f-4aa249c1298d/resource/66d15cce-bfee-4f91-9e6e-0ea79ec52b3d/download/ongoing_outbreaks.csv"
ongoingOutbreaks <- data.table::fread(input = url,
                                      sep = ",", 
                                      encoding = "UTF-8", 
                                      data.table = )
# format dates 
ongoingOutbreaks$date <- as.Date(ongoingOutbreaks$date, format="%m/%d/%Y")

# clean data 
ongoingOutbreaks <- ongoingOutbreaks %>% 
  
  # remove ID number codes from group strings 
  dplyr::mutate(outbreak_group = trimws(gsub('[0-9]+', '', outbreak_group), which = "both"), 
                outbreak_subgroup = trimws(gsub('[0-9]+', '', outbreak_subgroup), which = "both")) %>% 
  
  # arrange by date, group, then subgroup 
  dplyr::arrange(date, outbreak_group, outbreak_subgroup) %>% 
  
  # clean up subgroup names 
  dplyr::mutate(outbreak_subgroup = case_when(
    .$outbreak_subgroup == "Workplace - Other" ~ "Workplace (Other)", 
    .$outbreak_subgroup == "Workplace - Food Processing" ~ "Food Processing", 
    .$outbreak_subgroup == "Workplace - Farm" ~ "Farm",
    .$outbreak_subgroup == "School - Secondary" ~ "Secondary School",
    .$outbreak_subgroup == "School - Post-Secondary" ~ "Post-Secondary School",
    .$outbreak_subgroup == "School - Elementary/Secondary" ~ "Elementary/Secondary School",
    .$outbreak_subgroup == "School - Elementary" ~ "Elementary School", 
    TRUE ~ as.character(outbreak_subgroup)))


# Set file name 
fileName = paste(dirPath, 'Ongoing outbreaks.csv',sep = '')

# Write CSV File 
# write.csv(ongoingOutbreaks, 
#           file = fileName, 
#           fileEncoding = 'UTF-8', 
#           row.names = F)

# Export as RDS file 
fileName = paste(dirPath, 'Ongoing outbreaks.rds',sep = '')
saveRDS(ongoingOutbreaks, file = fileName)


#print notification message to console 
# file.csv is downloaded, x rows by y columns
print(paste("Ontario COVID-19 outbreaks data has been dowloaded",
            format(nrow(ongoingOutbreaks), big.mark = ","), "Rows x ", ncol(ongoingOutbreaks), "Columns",  
            sep = " "))












# Vaccine Data 
url <- "https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/8a89caa9-511c-4568-af89-7f2174b4378c/download/vaccine_doses.csv"
Vaccine_data <- data.table::fread(input = url,
                                  sep = ",", 
                                  encoding = "UTF-8", 
                                  data.table = FALSE, header = T)


Vaccine_data$previous_day_doses_administered  <-  as.numeric(gsub(",","",Vaccine_data$previous_day_total_doses_administered))
Vaccine_data$total_doses_administered  <-  as.numeric(gsub(",","",Vaccine_data$total_doses_administered))
Vaccine_data$total_doses_in_fully_vaccinated_individuals  <-  as.numeric(gsub(",","",Vaccine_data$total_doses_in_fully_vaccinated_individuals))
Vaccine_data$total_individuals_fully_vaccinated  <-  as.numeric(gsub(",","",Vaccine_data$total_individuals_fully_vaccinated))

# Set file name 
fileName = paste(dirPath, 'COVID-19 Vaccine Data.csv',sep = '')

# Write CSV File 
# write.csv(Vaccine_data, 
#           file = fileName, 
#           fileEncoding = 'UTF-8', 
#           row.names = F)


# Export as RDS file 
fileName = paste(dirPath, 'COVID-19 Vaccine Data.rds',sep = '')
saveRDS(Vaccine_data, file = fileName)

print(paste("Ontario vaccine data has been dowloaded",
            format(nrow(Vaccine_data), big.mark = ","), "Rows x ", ncol(Vaccine_data), "Columns",  
            sep = " "))













#this part beeps to notify the user the download is complete 
beep(sound = 1, expr = NULL)
print(paste("DOWNLOAD COMPLETE!"))

#garbage collector; removes global variables from the environment 
remove(list = ls())




