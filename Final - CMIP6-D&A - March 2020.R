#### Clear the Global Environment:
rm(list=ls())


# ipak function: install and load multiple R packages.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# packages to be installed?

packages <- c("chron", "RColorBrewer", "lattice", "ncdf4", "magrittr", 
              "dplyr", "rgdal", "raster", 
              "curl", "stringr", "magicfor", "lubridate", "gWidgets", 
              "ncdf4.helpers", "PCICt") 


ipak(packages)

####

require(rgdal)
require(raster)
library(curl)

library(rattle) # Must install only once before running the code
library(RGtk2)  # Must install only once before running the code
library(tcltk)  # Must install only once before running the code

library(gWidgets)
library(lubridate)
library(stringr)
library(magicfor)

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(magrittr) 
library(dplyr)    
library(ggplot2)
library(xlsx)
library(ncdf4.helpers)
library(PCICt)

########################## Creating the GUI:

main_win <- gwindow(" CMIP6 data Donwloader and Analyzer (CMIP6-D&A) ", visible= FALSE, toolkit = guiToolkit())
1
paned <- gpanedgroup ( cont = main_win ) 
group <- ggroup(horizontal = FALSE, container=paned) 

frame_1 <- gframe ( "" , cont = group , horizontal = FALSE ) 

## add logo:
URL_logo <- "https://www.dropbox.com/s/8412ihgpt34ga7z/SoftwareX_logo_m.png?dl=1"
download.file(URL_logo, destfile = "CMIP6_logo.png", mode = 'wb', quiet = TRUE)

logo <- list.files(pattern="CMIP6_logo.png$")
img <- gimage(logo, container=frame_1)

addSpring ( frame_1 ) 
addSpring ( frame_1 )

## add comments about:
frame_2 <- gframe ( "About CMIP6-D&A:" , cont = group , horizontal = FALSE ) 

text_logo <- "https://www.dropbox.com/s/w066pha4yw2wnij/text_m.png?dl=1"
download.file(text_logo, destfile = "text.png.png", mode = 'wb', quiet = TRUE)

text_logo <- list.files(pattern="text.png.png$")
img <- gimage(text_logo, container=frame_2)


Visit_button <- gbutton("Visit our website", 
                        container = frame_2,
                        handler = function(h,...) browseURL("https://ucalgary.academia.edu/BabakFarjad"))
addSpring(Visit_button)

addSpring ( frame_2 ) 
addSpring ( frame_2 )

## A group to select the Download Links in a TXT file by user:
frame3 <- gframe ( "Download Links? " , cont = group , horizontal = TRUE )
addSpring ( frame3 )
addSpring ( frame3 )

set_directory_2_1 <- ggroup(container = frame3)
obj_2_1 <- glabel("Select the TXT file where you have saved the download links: ", container =set_directory_2_1)
TXT_Link_file <- gfilebrowse ( text = "Select the TXT file ..." ,
                               quote = FALSE ,
                               type = "open" , cont = set_directory_2_1 )
addSpring ( frame3 )
addSpring ( frame3 )

## A group to select output directory by user:
frame4 <- gframe ( "Output directory? " , cont = group , horizontal = FALSE ) 
addSpring ( frame4 ) 
addSpring ( frame4 )

set_directory <- ggroup(container = frame4)
obj <- glabel("                             Select the output directory to save the reults: ", container =set_directory)
start_dir <- gfilebrowse ( text = "Select a directory ..." , 
                           quote = FALSE ,
                           type = "selectdir" , cont = set_directory )
addSpring ( frame4 ) 
addSpring ( frame4 )

## A group to organize the Run and Cancel buttons: 
button.group_run <- ggroup(container = group) 
## Push buttons to right 
addSpring(button.group_run) 
obj_run <- gbutton("Run", container=button.group_run,
                   handler = function(h,...) gmessage("Run the model?")) 
obj_cancel <- gbutton("Cancel", handler = function(h,...) dispose(main_win),
                      container=button.group_run)
obj_help <- gbutton("Help", container=button.group_run,
                    handler = function(h,...) gmessage(
                      "The Coupled Model Intercomparison Project Phase 6 Donwloader and Analyzer (CMIP6-D&A) is designed 
to obtain NetCDF files for precipitation (pr); minimum and maximum near surface air temperature (tasmin, tasmax)
from The World Climate Research Programme (WCRP), and generate statistical reports for the Alberta, Canada and its watersheds.
                      
                      The developers: 
                      Dr. Masoud Abdollahi; 
                      Dr. Babak Farjad; 
                      Dr. Anil Gupta; 
                      Dr. Quazi K. Hassan
                      University of Calgary - Sep:Dec. 2019.
                      
                      abdolahi@ucalgary.ca "
                    ))



## Display GUI 
visible ( main_win ) <- TRUE

####################################################

## R do these when user clicks on Run button:
addhandlerchanged(obj_run, handler=function(h,...) 
{
  ## recording the start time:
  start_time <- Sys.time()
  
  ## Set working directory:
  setwd(svalue(start_dir))
  getwd()
  
  ## Shape file Boundries:
  URL_shapefile <- "https://www.dropbox.com/s/oc3u1jd8dnl1e6i/shape_file_Alberta.zip?dl=1"
  FileName1 <- "shape_file_Alberta.zip"
  
  DIR_shapefile <- paste(svalue(start_dir),"/shape_file_Alberta", sep="")
  dir.create(DIR_shapefile, showWarnings = FALSE)
  
  download.file(URL_shapefile, destfile=paste0(DIR_shapefile,"/", FileName1), method="auto", mode="wb")
  
  ## Set working directory to Shape Files folder:
  setwd(DIR_shapefile)
  getwd()
  unzip(FileName1,exdir=DIR_shapefile)
  
  ## Loading the the New_GT_AB and Alberta shapefile:
  desired_area_Big <- readOGR(dsn = ".", layer = "New_GT_AB")
  desired_area_AB <- readOGR(dsn = ".", layer = "Alberta")
  
  ## Loading the Alberta Watersheds shapefile:
  ## 1 - Athabasca
  Athabasca <- readOGR(dsn = ".", layer = "Athabasca_m")
  ## 2 - Beaver
  Beaver <- readOGR(dsn = ".", layer = "Beaver")
  ## 3 - Hay_GreatSlave
  Hay_GreatSlave <- readOGR(dsn = ".", layer = "Hay_GreatSlave")
  ## 4 - Milk
  Milk <- readOGR(dsn = ".", layer = "Milk")
  ## 5 - NorthSaskatchewan
  NorthSaskatchewan <- readOGR(dsn = ".", layer = "NorthSaskatchewan")
  ## 6 - Peace_Slave
  Peace_Slave <- readOGR(dsn = ".", layer = "Peace_Slave")
  ## 7 - SouthSaskatchewan
  SouthSaskatchewan <- readOGR(dsn = ".", layer = "SouthSaskatchewan")
  
  ## Loop to download the NC files:
  i <- 1
  
  setwd("..")
  
  ## Reading the Download Links in the TXT file:
  download_links <- readLines(svalue(TXT_Link_file))
  print(class(download_links))
  
  for (i in 1:length(download_links)){
    if (i <= length(download_links)) {
      
      ## Set working directory:
      setwd(svalue(start_dir))
      getwd()
      
      ## Create a new directory where to download the HDF files
      ## Selecting a proper folder name:
      folder_name <- sub("\\.nc", "", basename(download_links[i]))
      
      print(folder_name)
      
      DIR <- paste(getwd(),folder_name, sep="/")
      dir.create(DIR)
      
      ## Set working directory to NC folder:
      setwd(DIR)
      getwd()
      
      ## Download the NC file
      M01 <- 'wget -c'
      M02 <- ' '
      
      Create_Wget_script <- paste0(M01, M02, download_links[i], 
                                   sep = "")
      
      Create_Wget_script
      write(Create_Wget_script, file = "Wget_script.bat")
      
      system("cmd.exe", input = "Wget_script.bat")
      
      ## Set working directory to NC folder:
      setwd(DIR)
      getwd()
      
      ## create a connetion to the downloaded NC file:
      fn <- basename(download_links[i])
      ncin <- nc_open(fn) 
      ncin
      
      nc_name <- substr(basename(download_links[i]), 0, 6)
      
      ## name of the downloaded product:
      if (substr(basename(download_links[i]), 0, 6) == "pr_day") {
        dname <- "pr"
      } else if (substr(basename(download_links[i]), 0, 6) == "tasmin") {
        dname <- "tasmin"
      } else {
        dname <- "tasmax"}
      print(dname)

      ## getting the fill value of the image (data):
      fillvalue <- ncatt_get(ncin, dname, "_FillValue") 
      fillvalue <- fillvalue$value
      fillvalue
      
      ## get the variable and its attributes 
      variable_units <<- ncatt_get(ncin, dname, "units") # getting the variable attribute (or variable scale)
      variable_units
      
      ## make it a Raster:
      fn_brick <- brick(fn) 
      fn_brick
      
      ## clip nc file for greater than Alberta and save it as a new nc file:
      ## get longitude and latitude from original nc file:
      lon <- ncvar_get(ncin,"lon")
      lat <- ncvar_get(ncin,"lat")

      ## get the units of longitude and latitude from original nc file:
      latunits <-ncatt_get(ncin,"lat","units")
      longunits <-ncatt_get(ncin,"lon","units")

      ## get the time and its unit from original nc file:
      time <- ncvar_get(ncin,"time")
      tunits_nc <- ncatt_get(ncin,"time","units")

      ## defining the lat and long for our study area of interest (greater than Alberta):
      LonIdx <- which( ncin$dim$lon$vals >= 239 & ncin$dim$lon$vals <= 253)
      LatIdx <- which( ncin$dim$lat$vals >= 47 & ncin$dim$lat$vals <= 62)

      ## clip the original nc with our study area of interest (greater than Alberta):
      MyVariable <- ncvar_get(ncin, dname)[LonIdx, LatIdx,]
      dim(MyVariable)

      ## Define the AB dimensions
      mv = fillvalue #for missing value
      lonAB <-ncdim_def( "Lon", longunits$value,lon[LonIdx[1]:LonIdx[length(LonIdx)]] )
      latAB <- ncdim_def( "Lat", latunits$value, lat[LatIdx[1]:LatIdx[length(LatIdx)]])
      t <- ncdim_def( "Time", tunits_nc$value, time)

      ## Make a variable with those dimensions.
      prdata <- ncvar_def(dname, variable_units[[2]], list(lonAB, latAB, t), mv)

      ## Set working directory to NC folder:
      setwd(DIR)
      getwd()

      ## Create a netCDF file with this variable:
      nctest <- nc_create(paste(folder_name, "clipped_AB.nc", sep = "_"), prdata )

      ## place the data in the new nc file:
      ncvar_put(nctest, prdata, MyVariable)

      ## close the connection to the new nc file for greater than Alberta:
      nc_close(nctest)
      
      ## check the extent of the raster file:
      extent(fn_brick)
      
      ## rotate the raster file to be like a normal lat long map:
      fn_brick = rotate(fn_brick)
      
      ## clip the NetCDF file with our desired polygons:
      ## check the coordinates:
      ## for New_GT_AB
      crs(desired_area_Big)
      crs(fn_brick)
      
      desired_area_Big = spTransform(desired_area_Big, crs(fn_brick))
      
      ## for Alberta
      crs(desired_area_AB)
      crs(fn_brick)
      
      desired_area_AB = spTransform(desired_area_AB, crs(fn_brick))
      
      ## An extent object defines a box .e.g a region of interest
      ROI <- extent(-130,-100,40,65)
      ROI
      
      fn_brick_crop <- crop(fn_brick,ROI)
      
      ## Mask the brick file with desired area
      fn_brick_mask_Big = mask(fn_brick_crop, desired_area_Big)

      ## convert fill values to NA
      fn_brick_mask_Big[fn_brick_mask_Big == fillvalue] <- NA
      
      ## Converting the variable's unit (i.e., from: "kg m-2 s-1" to mm OR : "K" to "C" ) :
      if (variable_units[[2]] == "K") {
        fn_brick_mask_Big_converted_unit <<- (fn_brick_mask_Big - 273.15)
      } else if (variable_units[[2]] == "kg m-2 s-1") {
        fn_brick_mask_Big_converted_unit <<- (fn_brick_mask_Big*86400)
      } 
      print(fn_brick_mask_Big_converted_unit)
      
      ## Mask the brick file with Alberta
      fn_brick_mask_Alberta = mask(fn_brick_mask_Big_converted_unit, desired_area_AB)

      ## get the time variable and Convert it to GOOD Dates : 
      time_variable <- nc.get.time.series(ncin, time.dim.name = "time")
      head(time_variable)
      class(time_variable)
      
      # converting "PCICt" class of time_variable to date:
      ex_char <- as.character.PCICt(time_variable)
      tori_2 <- as.Date(ex_char, tz = "UTC") 
      head(tori_2)
      class(tori_2)
      length(tori_2)
      
      ## Mask the brick file with each watershed:
      Athabasca_area = mask(fn_brick_mask_Alberta, Athabasca)
      Beaver_area = mask(fn_brick_mask_Alberta, Beaver)
      Hay_GreatSlave_area = mask(fn_brick_mask_Alberta, Hay_GreatSlave)
      Milk_area = mask(fn_brick_mask_Alberta, Milk)
      NorthSaskatchewan_area = mask(fn_brick_mask_Alberta, NorthSaskatchewan)
      Peace_Slave_area = mask(fn_brick_mask_Alberta, Peace_Slave)
      SouthSaskatchewan_area = mask(fn_brick_mask_Alberta, SouthSaskatchewan)
      
      ## calculate the mean image () mean:
      avg_Alberta = cellStats(fn_brick_mask_Alberta, stat='mean')
      avg_Athabasca_area = cellStats(Athabasca_area, stat='mean')
      avg_Beaver_area = cellStats(Beaver_area, stat='mean')
      avg_Hay_GreatSlave_area = cellStats(Hay_GreatSlave_area, stat='mean')
      avg_Milk_area = cellStats(Milk_area, stat='mean')
      avg_NorthSaskatchewan_area = cellStats(NorthSaskatchewan_area, stat='mean')
      avg_Peace_Slave_area = cellStats(Peace_Slave_area, stat='mean')
      avg_SouthSaskatchewan_area = cellStats(SouthSaskatchewan_area, stat='mean')
      
      ## prepare things as a data frame to save NC as an Excel file:
      year_avg_df <- data.frame(tori_2,
                                avg_Alberta,
                                avg_Athabasca_area, 
                                avg_Beaver_area, 
                                avg_Hay_GreatSlave_area,
                                avg_Milk_area,
                                avg_NorthSaskatchewan_area,
                                avg_Peace_Slave_area,
                                avg_SouthSaskatchewan_area)
      
      ## select the excel headlines based on the downloaded product:
      if (substr(basename(download_links[i]), 0, 6) == "pr_day") {
        names(year_avg_df) <- c("Date/Precipitation_mm", "Alberta", "Athabasca", "Beaver", "Hay_GreatSlave", "Milk", "NorthSaskatchewan", "Peace_Slave", "SouthSaskatchewan")
      } else if (substr(basename(download_links[i]), 0, 6) == "tasmin") {
        names(year_avg_df) <- c("Date/Min_Air_Temp_C", "Alberta", "Athabasca", "Beaver", "Hay_GreatSlave", "Milk", "NorthSaskatchewan", "Peace_Slave", "SouthSaskatchewan")
      } else
        names(year_avg_df) <- c("Date/Max_Air_Temp_C", "Alberta", "Athabasca", "Beaver", "Hay_GreatSlave", "Milk", "NorthSaskatchewan", "Peace_Slave", "SouthSaskatchewan")
      
      head(year_avg_df)
      dim(year_avg_df)
      
      year_avg_df$Date <- ymd(year_avg_df$Date)
      
      ## Calculate the annual statistics (mean):
      Annual_results <- year_avg_df %>%
        group_by(year(Date)) %>%
        summarize(Annual_Mean_Alberta = mean(Alberta, na.rm = TRUE),
                  Annual_Mean_Athabasca = mean(Athabasca, na.rm = TRUE),
                  Annual_Mean_Beaver = mean(Beaver, na.rm = TRUE),
                  Annual_Mean_Hay_GreatSlave = mean(Hay_GreatSlave, na.rm = TRUE),
                  Annual_Mean_Milk = mean(Milk, na.rm = TRUE),
                  Annual_Mean_NorthSaskatchewan = mean(NorthSaskatchewan, na.rm = TRUE),
                  Annual_Mean_Peace_Slave = mean(Peace_Slave, na.rm = TRUE),
                  Annual_Mean_SouthSaskatchewan = mean(SouthSaskatchewan, na.rm = TRUE))
      
      head(Annual_results)
      
      daily_results <- year_avg_df 
      Annual_results <- as.data.frame(Annual_results)
      class(Annual_results)
      class(daily_results)
      
      ## a name for Excel file:
      excel_nc_name <- sub("\\.nc", "", basename(fn))
      Excel_file_daily <- paste(excel_nc_name,"_daily_results", ".csv", sep = "")  
      Excel_file_annual <- paste(excel_nc_name,"_Annual_results", ".csv", sep = "")  
      
      ## save all statistics to an Excel file:
      write.csv(daily_results, file = Excel_file_daily)
      write.csv(Annual_results, file = Excel_file_annual)

      ## cleaning the unwanted files:
      file.remove("Wget_script.bat")
      setwd("..")
      
      ## close the connection to the nc file:
      nc_close(ncin)
      
      ## to release R memory form big files:
      gc() 
      
      i <- i+1
    }
  }
  
  ## Delete shape file folder:
  unlink("shape_file_Alberta", recursive = TRUE)
  
  ## recording the start time:
  end_time  <- Sys.time()
  
  ## Done message:
  gmessage(paste("******************** Done! ********************", "\n", "\n",
                 "Please check your selected folder.", "\n", "\n",
                 "Start Time was: ", start_time, "\n", 
                 "End   Time was: ", end_time, "\n",
                 "\n -----  Thanks for using CMIP6-D&A  -----", sep =" "))
  
})
########################################################################## M.A - December 2019


