#R Script to analyze ambient ozone concentrations at US monitors
#and visualize the data as a web map using Leaflet for R
#Developed by Mark A. Prettyman, DE DNREC DAQ
#with contributions from Shane Cone

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(gmodels)
library(knitr)
library(kableExtra)
library(leaflet)
library(data.table)
library(htmlwidgets)
library(htmltools)
library(readxl)
library(geojsonsf)
library(geojsonio)
library(sp)
library(sf)
library(RCurl)
library(leafpop)
library(crosstalk)
library(leafsync)
library(shiny)
library(rgdal)
library(RAQSAPI)
library(keyring)

#--------------------------
# OBTAINING AMBIENT OZONE DATA FROM AIRNOW

#load data frame monitors and their associated nonattainment areas in the country
load('C:/airnow/NAA_list.RData')

#Enter the year of data you want to analyze
Year_to_analyze <- "2021"

#converts the year to a date at the start of the ozone season
ozone_start_date <- as.Date(paste0(Year_to_analyze,"-01-01"))

#gets the date of "today", which is actually for "yesterday" to ensure that the analysis is current UP TO the current day.
Date_Today <- Sys.Date()-1

#converts the date of "today" to a character value without dashes
Today_characters <- paste0(substr(Date_Today,6,7),substr(Date_Today,9,10))

#creates a list of dates between the start of the ozone season and "today"
#removes the dashes from the dates and converts them all to characters
day_list <- str_remove_all(as.character(seq(ozone_start_date,Date_Today,1)),"-")

#main url for airnow daily files
airnowtech_files <- "https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/"

#name used for daily data files from airnow, which list the max concentrations for pollutants at each monitor each day
fileName <- "daily_data_v2.dat"

#creates a default folder on the computer
airnow_folder <- "C:/airnow"

#creates default folder
ifelse(!dir.exists(file.path(airnow_folder)), dir.create(file.path(airnow_folder)), FALSE)

#creates a subfolder
ifelse(!dir.exists(file.path(airnow_folder,Year_to_analyze)), dir.create(file.path(airnow_folder,Year_to_analyze)), FALSE)

#creates a variable for the path of the subfolder
YOC_folder <- paste(airnow_folder,Year_to_analyze,sep = "/")

#new subfolder name
dat_folder_name <- "Daily_Data_Files"

#creates another subfolder
ifelse(!dir.exists(file.path(YOC_folder,dat_folder_name)), dir.create(file.path(YOC_folder,dat_folder_name)), FALSE)

#another variable for the path of the final folder
dat_folder <- paste(YOC_folder,dat_folder_name,sep = "/")

#downloads the daily dat files for the selected year for the ozone season and places them in the download folder created.
system.time(
  for (i in day_list){
    destination_file <- file.path(dat_folder,paste0(i,fileName,sep = ""))
    #only downloads new files which have not already been downloaded
    if(!file.exists(destination_file)){
      temp_url <- paste0(airnowtech_files,Year_to_analyze,"/",i,"/",fileName)
      download.file(temp_url,destination_file)
    }
  })

#list of dat files in folder
file_list <- list.files(dat_folder)

#empty data frame
dataset <- data.frame()

 #read and bind all dat files into the empty data frame
system.time(
  for (i in 1:length(file_list)){
    setwd(dat_folder)
    temp_data <- fread(file_list[i], sep = "|", header = F,  stringsAsFactors = F) 
    dataset <- rbindlist(list(dataset, temp_data), use.names = T) 
  })


#list of names for the header of the data frame
headers <- c("Date","Monitor_ID","SiteName","Param_Name","Units","Value","Averaging_Period","Data_Source",
             "AQI_Value","AQI_Category","Latitude","Longitude","AQSID")

#renaming header of data frame
colnames(dataset) <- headers

#--------------------------
# ANALYZING AIRNOW DATA TO OBTAIN 4TH MAX AT EVERY MONITOR

#copying data frame to a new working data frame
AQ2021 <- dataset

#converting Date field from a character to a date.
# AQ2021$POSIX.Date <- as.POSIXct(paste0(AQ2021$Date,"20"), format = '%m/%d/%Y')

#creates a field for "Country Code" as a 3 digit subset of the AQS ID field
AQ2021$CountryCode <- substring(AQ2021$AQSID,1,3)

#a uniqe list of the Country Codes
countryCodeList <- unique(AQ2021$CountryCode)

#specific Country Codes which are uses in the US
US_Country_Codes <- c('840','021','001','093','113')

#filtering the data frame for monitors in the US and their 8hr daily max values, 
#and renaming field for 8hr ozone average max values
US_daily_max <- AQ2021 %>% 
  filter(CountryCode %in% US_Country_Codes) %>%
  filter(Param_Name == "OZONE-8HR") %>%
  rename("Avg_8hr" = Value)

#filters records for days when monitors exceed 70ppb
num_exceedences <- US_daily_max %>% 
  filter(Avg_8hr > 70)

#calculates how many values are included for each monitor in the US_daily_max data frame
exceedences_by_mon <- plyr::ddply(US_daily_max,~Monitor_ID,summarise,'days_>70ppb'=sum(Avg_8hr > 70))

#intermediary data frame
US_mon_coords <- US_daily_max %>%
  select(Monitor_ID,SiteName,Longitude,Latitude) %>%
  group_by(Monitor_ID) %>% 
  slice(1) %>% 
  left_join(.,exceedences_by_mon, by = "Monitor_ID", keep = F)

#selecting the 4 highest 8hr ozone max values for each monitor
US_4_Highest <- AQ2021 %>% 
  filter(CountryCode %in% US_Country_Codes) %>%
  filter(Param_Name == "OZONE-8HR") %>%
  rename("Avg_8hr_4thMax" = Value) %>%
  group_by(Monitor_ID) %>% 
  arrange(desc(AQI_Value)) %>% 
  slice(1:4)


#calculates how many values are included for each monitor in the US_4thMax df
num_monitors <- plyr::ddply(US_4_Highest,~Monitor_ID,summarise,num_days=length(unique(Date)))

#empty vector
num_vec <- c()

#loop to create a list of values (1 through 4) to represent the n-highest values by monitor
#the values create this list based upon the monitors and numbers in num_monitors
for (i in 1:nrow(num_monitors)){
  z <- seq(1,num_monitors[i,2],1)
  end <- length(num_vec)+1
  num_vec <- append(num_vec,z,end)
}

#the num_vec of values representing the n-highest values are added to the df
US_4_Highest$n_highest <- num_vec

#a "wide" pivot table is created which includes columsn for the 4 highest ozone values
US_Pivot <- US_4_Highest %>%
  select(Monitor_ID,SiteName,Avg_8hr_4thMax,n_highest) %>%
  pivot_wider(names_from = n_highest,values_from = Avg_8hr_4thMax)

#renaming columns of pivot table
colnames(US_Pivot) <- c("Monitor_ID","SiteName","2021_Max","2021_2nd_High","2021_3rd_High","2021_4th_High")

#alternate data frame of US_Pivot to include coordinates
US_Pivot_Coords <- US_Pivot %>%
  left_join(.,US_mon_coords, by = c("Monitor_ID","SiteName"), keep = F)

#removes records with no coordinates
US_Pivot_Coords <- US_Pivot_Coords %>%
  filter(!is.na(Latitude))

#vector with repeating dates of the most recent date of data
today.date_vec_1 <- rep(as.character(format.Date(Date_Today, "%m/%d/%Y")),nrow(US_Pivot_Coords))

#the vector of the most recent date of data is added to the data frame in order to date the data
US_Pivot_Coords$Data_Date <- today.date_vec_1

#--------------------------
# OBTAINS HISTORIC 4TH MAX VALUES FOR PRIOR YEARS FROM AQS DATA MART

#set username as email
my.email <- "abc@def.com"

#keyring package and functions used to store and manage AQS credentials
keyring::key_set(service = "AQSDatamart", username = my.email)

#retrieve saved AQS credentials and pass them to aqs_credentials function of RAQSAPI
datamartAPI_user <- server <- "AQSDatamart"
key = key_get(service = server, username = my.email)
aqs_credentials(username = my.email, key)

#query Data Mart API by a nationwide bounding box, to get data for 2020 from all US monitors
aqs2020 <- aqs_annualsummary_by_box(parameter = "44201",
                                    bdate = as.Date("20200101",
                                                    format = "%Y%m%d"),
                                    edate = as.Date("20201231",
                                                    format = "%Y%m%d"),
                                    minlat = "24.2",
                                    maxlat = "49.5",
                                    minlon = "-125.0",
                                    maxlon = "-66.8"
)

#filter all data just for 8-hour ozone data associated with the 2015 ozone NAAQS
aqs2020.8hr <- aqs2020 %>%
  filter(pollutant_standard == "Ozone 8-hour 2015")

#filter out unnecessary fields
aqs2020.8hr2 <- aqs2020.8hr[,c(1:8,14,35,36,48,50,54)]

#create unique field for 9-digit Monitor ID
aqs2020.8hr2 <- aqs2020.8hr2 %>% unite("Monitor_ID",state_code:site_number,sep = "",remove = FALSE)

#shortened list of header names
headers2020 <- c("Monitor_ID","StateFIPS","CountyFIPS","SiteNumber","ParameterCode","POC","Latitude","Longitude",
                   "Datum","Year","2020_4thMax","2020_4thMax_Datetime","SiteName","State","CBSA")

#applying header names to data frame
colnames(aqs2020.8hr2) <- headers2020

#query Data Mart API by a nationwide bounding box, to get data for 2019 from all US monitors
aqs2019 <- aqs_annualsummary_by_box(parameter = "44201",
                                    bdate = as.Date("20190101",
                                                    format = "%Y%m%d"),
                                    edate = as.Date("20191231",
                                                    format = "%Y%m%d"),
                                    minlat = "24.2",
                                    maxlat = "49.5",
                                    minlon = "-125.0",
                                    maxlon = "-66.8"
)

#filter all data just for 8-hour ozone data associated with the 2015 ozone NAAQS
aqs2019.8hr <- aqs2019 %>%
  filter(pollutant_standard == "Ozone 8-hour 2015")

#filter out unnecessary fields
aqs2019.8hr2 <- aqs2019.8hr[,c(1:8,14,35,36,48,50,54)]

#create unique field for 9-digit Monitor ID
aqs2019.8hr2 <- aqs2019.8hr2 %>% unite("Monitor_ID",state_code:site_number,sep = "",remove = FALSE)

#shortened list of header names
headers2019 <- c("Monitor_ID","StateFIPS","CountyFIPS","SiteNumber","ParameterCode","POC","Latitude","Longitude",
                   "Datum","Year","2019_4thMax","2019_4thMax_Datetime","SiteName","State","CBSA")

#applying header names to data frame
colnames(aqs2019.8hr2) <- headers2019

#merging the 4th max values from 2019 and 2020, by monitor, along with data frame of monitors by nonattainment area
fourthMax_19_20 <- aqs2020.8hr2 %>%
  select(Monitor_ID,`2020_4thMax`,`2020_4thMax_Datetime`) %>%
  left_join(.,aqs2019.8hr2, by = "Monitor_ID", keep = F) %>%
  left_join(.,NAA_list,by = "Monitor_ID", keep = F)

#converts the ppm values to ppb values
fourthMax_19_20$`2020_4thMax` <- fourthMax_19_20$`2020_4thMax`*1000
fourthMax_19_20$`2019_4thMax` <- fourthMax_19_20$`2019_4thMax`*1000

#saves the data frame of 2019 and 2020 4th max values to an RData file
setwd(airnow_folder)
save(fourthMax_19_20, file = 'fourthMax_19_20.RData')

#--------------------------

#loads the 4th max data frame previously created.
setwd(airnow_folder)
load('fourthMax_19_20.RData')

#joins the 2021 4th Max value to the data fram for 2019 and 2020 4th max values, and the
US_2021DV <- fourthMax_19_20 %>%
  select(State,CountyFIPS,Monitor_ID,NAA_Name,Latitude,Longitude,`2019_4thMax`,`2020_4thMax`) %>%
  left_join(.,US_Pivot, by = "Monitor_ID", keep = F)  %>%
  left_join(.,exceedences_by_mon, by = "Monitor_ID", keep = F)

#calculates the draft 2019-2021 design value for ozone for the monitors
US_2021DV$Draft_DV_19_21 <- apply(US_2021DV[,c(7,8,13)], 1, function(x) trunc(mean(x)))

#calculates 4th max value to exceed 75ppb or 70ppb, in 2021
# US_2021DV$Max2021_4thMax_75 <- apply(US_2021DV[,c(8,13)], 1, function(x) trunc((76*3)-sum(x)-1))
# US_2021DV$Max2021_4thMax_70 <- apply(US_2021DV[,c(8,13)], 1, function(x) trunc((71*3)-sum(x)-1))

#removes records with no draft DV for 2021
US_2021DV <- US_2021DV %>%
  filter(!is.na(Draft_DV_19_21))

#vector with repeating dates of the most recent date of data
today.date_vec_2 <- rep(as.character(format.Date(Date_Today, "%m/%d/%Y")),nrow(US_2021DV))

#the vector of the most recent date of data is added to the data frame in order to date the data
US_2021DV$Data_Date <- today.date_vec_2

#---------------------------------
# MAPPING THE DATA

#bins the draft design value data, above and below the 0.070ppm ozone NAAQS
US_Pivot_Coords$O3_2021_4thMax <- cut(US_Pivot_Coords$`2021_4th_High`,c(0,60,65,70,75,80,150), include.lowest = T,labels = c('< 61','61-65','66-70','71-75','76-80','>80'))

#bins the draft design value data, above and below the 0.070ppm ozone NAAQS
US_2021DV$O3_NAAQS_Attainment <- cut(US_2021DV$Draft_DV_19_21,c(0,60,65,70,75,80,150), include.lowest = T,labels = c('< 61','61-65','66-70','71-75','76-80','>80'))

#bins the 4th max values to exceed, if incorporating these values in the map
# US_2021DV$Attain_75_in_2021 <- cut(US_2021DV$Max2021_4thMax_75,c(0,60,65,70,75,80,150), include.lowest = T,labels = c('< 61','61-65','66-70','71-75','76-80','>80'))
# US_2021DV$Attain_70_in_2021 <- cut(US_2021DV$Max2021_4thMax_70,c(0,60,65,70,75,80,150), include.lowest = T,labels = c('< 61','61-65','66-70','71-75','76-80','>80'))

#color pallette is set
monitorCol <- colorFactor(c('blue','purple','green','yellow','orange','red'), 
                          domain = c('< 61','61-65','66-70','71-75','76-80','>80'))

#alters the leaflet html options for styling the title of the map
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 8px; 
    padding-right: 8px; 
    background: rgba(192,192,192,1);
    font-weight: bold;
    font-size: 18px;
  }
"))

#specifies value of title
title <- tags$div(
  tag.map.title, HTML(paste0("AirNow Data From ",ozone_start_date," Through ",Date_Today))
)  

setwd(airnow_folder)

# reading JSON file for NAA boundaries as a "simple feature" object
#US_2015_O3_NAA <- geojson_sf("US_2015_ozone_NAA_4326.geojson")
US_2015_O3_NAA <- topojson_read("US_2015_ozone_NAA_4326.json")

#unique list of NAAs and the design value site for each, based upon highes draft 2021 DV
NAA_DV_Site <- US_2021DV %>%
  select(Monitor_ID,SiteName,NAA_Name,Draft_DV_19_21,Data_Date) %>%
  filter(!is.na(NAA_Name)) %>%
  group_by(NAA_Name) %>%
  arrange(desc(Draft_DV_19_21)) %>%
  slice(1)

#temporary simple feature object, using a subset of fields
temp_polygon <- US_2015_O3_NAA %>%
  select(c(1,2,3,4,5,8,9,12,26,28,32,63,64)) %>%
  mutate(Designation_DV = design_value*1000)

#design value data of NAA_DV_Site is merged with the spatial data of the NAA polygons, to create a new layer
new_NAA_Polygons <- merge(temp_polygon,NAA_DV_Site,by.x='area_name',by.y='NAA_Name')

# reading JSON file for state
#US_states <- geojson_sf("US_States_4326.geojson")
US_states <- topojson_read("US_States_4326.json")

#creates a orange color palette for the various NAA classifications
naa_pal <- colorFactor(
  palette = "Oranges",
  domain = US_2015_O3_NAA$classification)

# The Leaflet map widget is set to a variable "map_combined".
map_combined <- leaflet() %>% 
  
  # the zoom and center is set
  setView(-77, 39, zoom = 5) %>%
  
  #attribution data is added to the map
  addTiles(attribution = paste0('<a href=\"https://dnrec.alpha.delaware.gov/\">DE DNREC</a> - Mark Prettyman & Shane Cone, Created ',
                                Date_Today+1)) %>%
  
  #a basemap is added
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # a polygon layer is added for the US states object and its outline color and opacity are set
  addPolygons(data=US_states,
              stroke = TRUE,
              smoothFactor = 0.2,
              weight = 2,
              opacity = 1.0,
              color = "black",
              fillColor = "transparent",
              group = "State Outlines") %>%
  
  # a polygon layer is added for the US NAA object and its outline color and opacity are set
  addPolygons(data=new_NAA_Polygons,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.2,
              fillOpacity = .8,
              fillColor = ~naa_pal(classification),
              group = "2015 NAA Classifications",
              popup = as.character(paste0(new_NAA_Polygons$area_name," NAA","<br>",
                                          "Classification: ",new_NAA_Polygons$classification,"<br>",
                                          "Design Value Site Name: ",new_NAA_Polygons$SiteName,"<br>",
                                          "Design Value Site ID: ",new_NAA_Polygons$Monitor_ID,"<br>",
                                          "Design Value Site 2021 DV: ",new_NAA_Polygons$Draft_DV_19_21))) %>%

  
#this following two addCircleMarker objects are commented out, depending on the popup info to display
# add circle markers for the monitors and 4th max value
# addCircleMarkers(data = US_Pivot_Coords,
#                  ~Longitude,
#                  ~Latitude,
#                  popup = as.character(paste0("Site Name: ",US_Pivot_Coords$SiteName,"<br>",
#                                              "AQS ID: ",US_Pivot_Coords$Monitor_ID,"<br>",
#                                              "2021 4th Max: ",US_Pivot_Coords$`2021_4th_High`," ppb","<br>",
#                                              "# Days >70ppb: ",US_Pivot_Coords$`days_>70ppb`)),
#                  label = as.character(US_Pivot_Coords$SiteName),
#                  labelOptions = labelOptions(textsize = "15px"),
#                  color = ~monitorCol(O3_2021_4thMax),
#                  radius = 4,
#                  stroke = F, fillOpacity = 1,
#                  group = "2021 4th Max") %>%
# 
# # add circle markers for the monitors and draft DV
# addCircleMarkers(data = US_2021DV,
#                  ~Longitude,
#                  ~Latitude,
#                  popup = as.character(paste0("Site Name: ",US_2021DV$SiteName,"<br>",
#                                              "AQS ID: ",US_2021DV$Monitor_ID,"<br>",
#                                              "2021 Draft DV: ",US_2021DV$Draft_DV_19_21," ppb")),
#                  label = as.character(US_2021DV$SiteName),
#                  labelOptions = labelOptions(textsize = "15px"),
#                  color = ~monitorCol(O3_NAAQS_Attainment),
#                  radius = 4,
#                  stroke = F, fillOpacity = 1,
#                  group = "2021 DV") %>%

  # add circle markers for the monitors and 4th max value
  addCircleMarkers(data = US_Pivot_Coords,
                 ~Longitude,
                 ~Latitude,
                 popup = popupTable(US_Pivot_Coords,
                                    zcol = c("Monitor_ID","SiteName","days_>70ppb","2021_Max","2021_2nd_High",
                                             "2021_3rd_High","2021_4th_High")),
                 label = as.character(US_Pivot_Coords$SiteName),
                 labelOptions = labelOptions(textsize = "15px"),
                 color = ~monitorCol(O3_2021_4thMax),
                 radius = 4,
                 stroke = F, fillOpacity = 1,
                 group = "2021 4th Max") %>%
  
  # add circle markers for the monitors and DV value
  addCircleMarkers(data = US_2021DV,
                   ~Longitude,
                   ~Latitude,
                   popup = popupTable(US_2021DV,
                                      zcol = c("Monitor_ID","SiteName","NAA_Name","Draft_DV_19_21")),
                   label = as.character(US_2021DV$SiteName),
                   labelOptions = labelOptions(textsize = "15px"),
                   color = ~monitorCol(O3_NAAQS_Attainment),
                   radius = 4,
                   stroke = F, fillOpacity = 1,
                   group = "2021 DV") %>%
  
  # # add circle markers for the monitors and 4th max value in 2021 to exceed 75ppb standard
  # addCircleMarkers(data = US_2021DV,
  #                  ~Longitude,
  #                  ~Latitude,
  #                  popup = popupTable(US_2021DV,
  #                                     zcol = c("Monitor_ID","SiteName","NAA_Name","2018_4thMax","2019_4thMax","2021_4th_High","Max2021_4thMax_75")),
  #                  label = as.character(US_2021DV$SiteName),
  #                  labelOptions = labelOptions(textsize = "15px"),
  #                  color = ~monitorCol(Attain_75_in_2021),
  #                  radius = 4,
  #                  stroke = F, fillOpacity = 1,
  #                  group = "2021 4th Max to Attain (75ppb)") %>%
  # 
  # # add circle markers for the monitors and 4th max value in 2021 to exceed 70ppb standard
  # addCircleMarkers(data = US_2021DV,
  #                  ~Longitude,
  #                  ~Latitude,
  #                  popup = popupTable(US_2021DV,
  #                                     zcol = c("Monitor_ID","SiteName","NAA_Name","2018_4thMax","2019_4thMax","2021_4th_High","Max2021_4thMax_70")),
  #                  label = as.character(US_2021DV$SiteName),
  #                  labelOptions = labelOptions(textsize = "15px"),
  #                  color = ~monitorCol(Attain_70_in_2021),
  #                  radius = 4,
  #                  stroke = F, fillOpacity = 1,
  #                  group = "2021 4th Max to Attain (70ppb)") %>%
  
  # add legend for the ozone values
  addLegend('bottomleft', pal = monitorCol, values = US_2021DV$O3_NAAQS_Attainment,
            title = 'Ozone Value Bins',opacity = 1) %>%
  
  # adds radio buttons for switching layers between 4th max and DV values
  addLayersControl(baseGroups = c("2021 4th Max", "2021 DV"),
                   options = layersControlOptions(collapsed = FALSE),
                   overlayGroups = c("State Outlines","2015 NAA Classifications")) %>%
  
  # add legend for the NAA classifications
  addLegend('bottomright', pal = naa_pal, values = US_2015_O3_NAA$classification,
            title = '2015 Ozone NAA Classifications',opacity = 1) %>%
  
  # adds a title in the top of the map
  addControl(title, position = "topright", className="map-title") 

#calling the map object
#map_combined

#---------------------------------
# EXPORT MAP TO STANDALONE HTML FILE AND UPLOAD TO FTP SITE

# #creates a filename
htmlFileName <- paste0(airnow_folder,"/","2021_Draft_Ozone_Values",".html")

# exporting the map widget to a single html file
saveWidget(map_combined, htmlFileName, selfcontained = T)

uploadFileName <- "2021_Draft_Ozone_Values"

setwd(YOC_folder)

#export polygons of nonattainment areas to geoJSON file
fileName1 <- "US_NAA_2021_4thMax_DV.geojson"
geojson_write(new_NAA_Polygons, file = fileName1)
fileName2 <- "US_4thMax_Values.csv"
write.csv(US_Pivot_Coords, file = fileName2,row.names = FALSE)
fileName3 <- "US_DesignValues.csv"
write.csv(US_2021DV,file = fileName3,row.names = FALSE)

#uploads html file to FTP
login <- "login"
secret <- "password"
uploadSite <- "sftp://abc"

ftpUpload(htmlFileName,
          paste0(uploadSite,uploadFileName,".html"),
          verbose = TRUE,
          userpwd = paste0(login,":",secret))

ftpUpload(paste0(YOC_folder,"/",fileName1),
          paste0(uploadSite,fileName1),
          verbose = TRUE,
          userpwd = paste0(login,":",secret))

ftpUpload(paste0(YOC_folder,"/",fileName2),
          paste0(uploadSite,fileName2),
          verbose = TRUE,
          userpwd = paste0(login,":",secret))

ftpUpload(paste0(YOC_folder,"/",fileName3),
          paste0(uploadSite,fileName3),
          verbose = TRUE,
          userpwd = paste0(login,":",secret))





 
# #example code to export as a shapefile
# abc <- US_Pivot_Coords %>%
#   st_as_sf(coords = c("Longitude","Latitude"))
# CRS(abc)
# crs(abc)
# def <- st_set_crs(abc,4326)
# plot(def,main="Map")
# st_write(def,"def.shp",driver = "ESRI Shapefile")
