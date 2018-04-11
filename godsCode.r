###############################################################################
# GODS CODE- RUNS MY DISSERTATION ANALYSIS FROM RAW TEXT FILES TO ANALYSIS AND
# RESULTS PLOTTING, WITH THE EXCEPTION OF THE MARSH ISLAND PAPER BECAUSE THAT
# WAS ALREADY DONE WHEN I STARTED WRITING THIS IN APRIL 2014.
#
# Created:  D.S. Stich, 04.18.2014
# Modified: D.S. Stich, 04.17.2015
#
################################################################################
# PART 1. PACKAGE INSTALL AND LOADING------------------------------------------
{ # Code folding marker}
################################################################################
# Install and load necessary packages
#
# Packages can all be installed up-front, but the install and loading code is
# also present in each of the sub-sections below
################################################################################
# Install data manipulation and GUI toolkit packages
  #install.packages("Rcpp") # Uncomment start of line to run
#   install.packages("lubridate") # Uncomment start of line to run
#   install.packages("tcltk2") # Uncomment start of line to run
#   install.packages("reshape") # Uncomment start of line to run
#   install.packages("reshape2") # Uncomment start of line to run
# 	install.packages("geosphere") # Uncomment start of line to run
# 	install.packages('glmmADMB') # Uncomment start of line to run
# 	install.packages('MASS') # Uncomment start of line to run
# 	install.packages('car') # Uncomment start of line to run
# 	install.packages('lme4') # Uncomment start of line to run
# 	install.packages('RMark') # Uncomment start of line to run
# 	install.packages('R2WinBUGS') # Uncomment start of line to run
# 	install.packages('rjags') # Uncomment start of line to run
# 	install.packages('R2jags') # Uncomment start of line to run
# 	install.packags('plyr') # Uncomment start of line to run
# 	install.packages('AICcmodavg', deps=TRUE) # Uncomment to install
#   install.packages('StreamMetabolism') # Uncomment to install
#   install.packages('lmerTest') # Uncomment to install
#   install.packages('plotrix') # Uncomment to install
# 	install.packages('snow') # Uncomment start of line to run
# Install and load spatial data packages; uncomment to install
#   install.packages(c("rgdal","raster","R.utils","maptools","spsurvey","rgeos",
#   "tcltk2"), dep=TRUE)

# Load the packages into R session
  require(Rcpp) # C++ package
  require(lubridate) # Package for handling Date/Time
  require(tcltk2) # Toolkit commmands for file mgmt and GUI windows
  require(reshape) # Data manipulation package
  require(reshape2) # Data manipulation package
	require(rgdal) # Geospatial Data Abstraction Library
  require(R.utils) # Programming utilities in R
  require(spsurvey) # Spatial Survey Design and Analysis
  require(maptools) # Tools for reading and handling spatial objects
  require(raster) # Gridded spatial data package
  require(sp) # Classes and methods for spatial data
  require(rgeos) # Interface to Geometry Engine - Open Source (GEOS)
	require(geosphere) # Geodetic tools
	require(glmmADMB) # AD Model builder
	require(MASS) # Stats tools
	require(car) # Stats tools
	require(lme4) # Linear modeling tools
	require(RMark) # R to MARK language
	require(R2WinBUGS) # R to WinBUGS language package
	require(rjags) # R to JAGS language package (1)
	require(R2jags) # R to JAGS language package (2)
	require(plyr) # Data manipulation and 'apply' tools
	require(AICcmodavg) # Model selection and averaging package
  require(StreamMetabolism) # Photoperiod calculator, etc.
  require(lmerTest) # Lmer statistical testing tools
	require(plotrix) # Plotting tools
	require(snow) # Parallel processing package
################################################################################
} # Code folding marker
# END PART 1. PACKAGE INSTALL AND LOADING---------------------------------------


# PART 2. DATA COMPILATION------------------------------------------------------
{ # Code folding marker
################################################################################
# Make a coherent surgery file for all years with standardized names, etc.
#
# INPUT FILENAMES:  '2005-2013 Acoustic Smolt Tagging.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'surgery0506...surgery13','surg'
# OUTPUT FILENAMES: '2005-2013 Surgery.txt'
################################################################################
# NOTE: Right now, the code replaces NA and impossible values of individual fish
# covariates with the mean across years.  It is, however, conceivable that one
# might want to replace the NA values with annual averages...
# Packages
	#install.packages('tcltk') # Uncomment start of line to run
	#install.packages('tcltk2') # Uncomment start of line to run
	require(tcltk)
	require(tcltk2)

# Read in surgery file
  surgery <- read.csv(file.choose()) # '2005-2013 Acoustic smolt tagging.txt'

# Fix the date format
  surgery$ReleaseDate<-ymd_hms(as.character(surgery$ReleaseDate))

# Add ATPase data to the 2005 & 2006 surgery data
  # Select only those records from the appropriate year(s)
    surgery0506<-surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2005|
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2006,]
  # Read in ATPase data for the year
	  atp0506 <- read.csv(file.choose())
	# Add ATPase activity to the surgery0506 table
    surgery0506 <- merge(surgery0506, atp0506, by="TagID", all.x=TRUE)
	# Rename the ATPase column to make more sense
	  names(surgery0506)[names(surgery0506)=="Activity"] <- "ATPaseActivity"

# Add ATPase data to 2009 surgery data
  # Select only those records from the appropriate year(s)
    surgery09<-surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2009,]
  # Read in ATPase data for the year
	  atp09 <- read.csv(file.choose())
	# Add ATPase activity to the surgery09 table
    surgery09 <- merge(surgery09,atp09, by="TagID", all.x=TRUE)
	# Rename the ATPase column to make more sense
	  names(surgery09)[names(surgery09)=="Activity"] <- "ATPaseActivity"

# Add ATPase data to 2010 surgery data
  # Select only those records from the appropriate year(s)
    surgery10 <- surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2010,]
  # Read in ATPase data for the year
	  atp10 <- read.csv(file.choose())
	# Add ATPase activity to the surgery10 table
    surgery10 <- merge(surgery10,atp10, by="TagID", all.x=TRUE)
  # Rename the ATPase column to make more sense
	  names(surgery10)[names(surgery10)=="Activity"] <- "ATPaseActivity"

# Add ATPase data to 2011 surgery data
  # Select only those records from the appropriate year(s)
    surgery11<-surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2011,]
  # Read in ATPase data for the year
	  atp11 <- read.csv(file.choose())
	# Add ATPase activity to the surgery11 table
    surgery11 <- merge(surgery11, atp11, by="TagID", all.x=TRUE)
	# Rename the ATPase column to make more sense
	  names(surgery11)[names(surgery11)=="Activity"] <- "ATPaseActivity"

# Add ATPase data to 2012 surgery data
  # Select only those records from the appropriate year(s)
    surgery12<-surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2012,]
  # Read in ATPase data for the year
	  atp12 <- read.csv(file.choose())
	# Add ATPase activity to the surgery12 table
    surgery12 <- merge(surgery12, atp12, by="TagID", all.x=TRUE)
	# Rename the ATPase column to make more sense
	  names(surgery12)[names(surgery12)=="Activity"] <- "ATPaseActivity"

# Add ATPase data to 2013 surgery data
  # Select only those records from the appropriate year(s)
    surgery13<-surgery[
    	as.numeric(year(as.character(surgery$ReleaseDate)))==2013,]
  # Read in ATPase data for the year
	  atp13 <- read.csv(file.choose())
	# Add ATPase activity to the surgery13 table
    surgery13 <- merge(surgery13, atp13, by="TagID", all.x=TRUE)
	# Rename the ATPase column to make more sense
	  names(surgery13)[names(surgery13)=="Activity"] <- "ATPaseActivity"

# Combine all of the surgery dfs into a single file
  # First, make all of the columns into chr to avoid factor-based errors
    for(i in 1:ncol(surgery0506)){
      surgery0506[,i] <- as.character(surgery0506[,i])
    	surgery09[,i] <- as.character(surgery09[,i])
  	  surgery10[,i] <- as.character(surgery10[,i])
    	surgery11[,i] <- as.character(surgery11[,i])
     	surgery12[,i] <- as.character(surgery12[,i])
     	surgery13[,i] <- as.character(surgery13[,i])
    }
	# Remove the column with vial number from the 2012 tagging df
	  surgery12 <- surgery12[,-10]
  # Combine all years of tagging data
    surg <- rbind(surgery0506, surgery09, surgery10, surgery11, surgery12,
   	  surgery13)
  # Re-assign names to the length and mass columns
    names(surg)[7:8] <- c("ForkLength.mm","Mass.g")

# Rename release sites to standardize among years and avoid duplication later on
  # Replace weldon release sites for 2005-2013 with EastBranch
	  surg$ReleaseSite <- as.character(surg$ReleaseSite)
	  for(i in 1:nrow(surg)){
	  	if((year(surg$ReleaseDate[i])==2010 |
	  		  year(surg$ReleaseDate[i])==2011 |
	  		  year(surg$ReleaseDate[i])==2012 |
	  		  year(surg$ReleaseDate[i])==2013) &
	  		  surg$ReleaseSite[i]=="Weldon"){
	  		surg$ReleaseSite[i]<-"EastBranch"
	  	} else {
	  		next;
	  	}
	  }
	# Replace BikeClub release sites for 2006 with Weldon
	  surg$ReleaseSite <- as.character(surg$ReleaseSite)
	  for(i in 1:nrow(surg)){
	  	if(( year(surg$ReleaseDate[i])==2006) &
	  		  surg$ReleaseSite[i]=="BikeClub"){
	  		surg$ReleaseSite[i]<-"Weldon"
	  	} else {
	  		next;
	  	}
	  }
	# Rename Abbott release site so it is spelled correctly-- oops!
  	surg$ReleaseSite[surg$ReleaseSite=="Abbot"] <- "Abbott"
	# OH NO!!! I went to Abbot shortly after finishing the WHOLE scrip, and
	# realized that it was spelled correctly in the first place!!! Google Earth
	# had it spelled wrong >:|

# Replace NA values of individual fish covariates with mean across years
	# First, make vars numeric
	  surg$ForkLength.mm <- as.numeric(surg$ForkLength.mm) # Fork length
    surg$Mass.g <- as.numeric(surg$Mass.g) # Fish mass
    surg$ATPaseActivity <- as.numeric(surg$ATPaseActivity) # ATPase

	# Replace negative values with mean of ATPaseActivity, simultaneously
	# replace ridiculously large values of ATPaseActivity with mean value, and
	# then replace NA values of ATPase Activity with adjusted mean value (i.e.
	# excluding negative and ridiculous values)
	  for(i in 1:nrow(surg)){
	    if(!is.na(surg$ATPaseActivity[i]) &
	    	(surg$ATPaseActivity[i]<1 | surg$ATPaseActivity[i]>15)){
	      surg$ATPaseActivity[i]<-mean(surg$ATPaseActivity, na.rm=TRUE)
	    } else {
	      if(is.na(surg$ATPaseActivity[i])){
	        surg$ATPaseActivity[i]<-mean(surg$ATPaseActivity, na.rm=TRUE)
	      } else {
	      	next;
	      }
	    }
	  }

  # Replace NA values with mean of ForkLength.mm, replace cm with mm first so
	# NA values are not replaced with biased estimates of the mean value
    for(i in 1:nrow(surg)){
      if(!is.na(surg$ForkLength.mm[i]) & (surg$ForkLength.mm[i] < 100)){
      	surg$ForkLength.mm[i] <- surg$ForkLength.mm[i]*10
      } else {
      	if(is.na(surg$ForkLength.mm[i])){
	    	  surg$ForkLength.mm[i]<-mean(surg$ForkLength.mm, na.rm=TRUE)
      	} else {
      		surg$ForkLength.mm[i] <- surg$ForkLength.mm[i]
        }
      }
    }

  # Replace NA values with mean of Mass.g
      for(i in 1:nrow(surg)){
        if(is.na(surg$Mass.g[i]))
        	surg$Mass.g[i] <- mean(surg$Mass.g, na.rm=TRUE)
      }

# Give the table the name 'surgery' in for reading in as a .rda in the future
	  surgery <- surg

# Write the new table to a file
    write.table(surgery ,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    row.names=FALSE)
################################################################################

################################################################################
# Compile the 2005 & 2006 Smolt data and create final 0506 database
#
# INPUT FILENAMES:  '2005 & 2006 ATS Smolt Detections.rda' (or .txt),
#                   '2005-2013 Surgery.txt',
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future
#                   'd0506'
# OUTPUT FILENAMES: 'Final 2005 & 2006 Database.txt' (d0506),
#                   'Final 2005 & 2006 Database.rda' (d0506); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  library(lubridate)
  library(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery <- read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg0506<-surgery[as.numeric(year(as.character(surgery$ReleaseDate)))==2005|
    		as.numeric(year(as.character(surgery$ReleaseDate)))==2006,]
    surg0506$ReleaseDate<-ymd_hms(as.character(surg0506$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers <- read.csv(file.choose())
  receivers$Deployed <- ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved <- ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers) <- c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data ('XXXX ATS Smolt detections.txt' or .csv)
  #d <- read.csv(file.choose(), header=F)
	#detections0506 <- d
	#save(detections0506, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Read in 2005 & 2006 ATS Smolt detections.rda
	names(detections0506) <- c("ReceiverSN","TagID","D","T","Sensor.1","Units.1")
  # May need to change based on VUE export formats
    #d<-data.frame(d$Receiver.S.N, d$ID, d$Date.Time, d$Sensor.1, d$Units.1)
    #names(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  # Format the dates in d (if it's legacy data with split timestamp)
    DateTime <- do.call(paste, c(detections0506[c("D", "T")], sep= " "))
    d <- data.frame(detections0506[ ,1:2], DateTime,
    	detections0506[ ,5:ncol(detections0506)])
    #names(d) <- c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  # Change time format
    d$DateTime <- ymd_hms(as.character(d$DateTime))
    rm(DateTime) # Get rid of this, it's huge (>200 MB)
  # Add a column for numeric timestamp
    d$DetNum<-as.numeric(d$DateTime)

# Make df with detections and fish/tag data; about 10 seconds
  fish.detections <- merge(d, surg0506, by="TagID", all.y=TRUE)

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary, takes a couple minutes
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN", all.x=TRUE)
	#Rawdata$ReleaseDate <- as.character(Rawdata$ReleaseDate)
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata
  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(qc$ReleaseDate)
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate < qc$DetNum |
        qc$NumericReleaseDate==qc$DetNum, ]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum > qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate > qc2$DepNum |
    	qc2$NumericReleaseDate==qc2$DepNum, ]

  # Rename the df for lazy loading later on as .rda filetype
    d0506<-qc3
	    qc <- d0506[year(d0506$DateTime)==year(d0506$Deployed) &
  		year(d0506$DateTime)==year(d0506$Retrieved) &
  	  year(d0506$DateTime)==year(d0506$ReleaseDate), ]
    d0506<-qc
	  nrow(d0506)

# Add year to the TagID field so that there are no tag overlaps possible
  d0506$TagID <- paste(d0506$TagID, "-", as.integer(substr(d0506$DateTime,1,4)),
  sep="")

# Get the desired columns from the data
  d0506<-data.frame(d0506$TagID, d0506$ReceiverSN, d0506$Location,
  	d0506$LocationCode, d0506$RKM, d0506$Easting, d0506$Northing,
  	d0506$DateTime, d0506$Origin, d0506$ReleaseSite, d0506$R.Easting,
  	d0506$R.Northing, d0506$ReleaseDate, d0506$ForkLength.mm,
  	d0506$Mass.g, d0506$ATPaseActivity, d0506$Surgeon)

# Assign standardized column names
  names(d0506)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
  	"RKM", "Easting", "Northing",	"DateTime", "Origin",
  	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
   	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

# Write the final database to a file
  # This will save it as a comma separated text file ('Final XXXX Database.txt')
    #write.table(d0506,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    #row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
  # ('Final XXXX Database.rda)
    save(d0506, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile the 2009 Smolt data and create final 2009 database
#
# INPUT FILENAMES:  '2009 ATS Smolt Detections.rda' (or .txt),
#                   '2005-2013 Surgery.txt
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code
#                   'd09'
# OUTPUT FILENAMES: 'Final 2009 Database.txt' (d09),
#                   'Final 2009 Database.rda' (d09); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  library(lubridate)
  library(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery<-read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg09<-surgery[year(surgery$ReleaseDate)==2009,]
    surg09$ReleaseDate<-ymd_hms(as.character(surg09$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers)<-c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data
  #d<-read.csv(file.choose())
	#detections09 <- d
	#save(detections09, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Read in 2009 ATS Smolt detections.rda

# May need to change based on VUE export formats
  d<-detections09
  colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  d$DateTime<-ymd_hms(as.character(d$DateTime))
  d$DetNum<-as.numeric(d$DateTime)
  colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange",
 	  "DetNum")

# Make file with detections and fish/tag data
  fish.detections<-merge(d,surg09, by="TagID")

# Now that the dataframe is smaller, combine with receiver diary
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN", all.x=TRUE)
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(as.POSIXct(qc$ReleaseDate))
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
    	qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Rename the df for lazy loading later on as .rda filetype
    d09<-qc3
    nrow(d09)

# Add year to the TagID field so that there are no tag overlaps possible
  d09$TagID <- paste(d09$TagID, "-", as.integer(substr(d09$DateTime, 1, 4)),
  sep="")

# Get the desired columns from the data
  d09<-data.frame(d09$TagID, d09$ReceiverSN, d09$Location, d09$LocationCode,
  	d09$RKM, d09$Easting, d09$Northing,	d09$DateTime, d09$Origin,
  	d09$ReleaseSite, d09$R.Easting, d09$R.Northing, d09$ReleaseDate,
   	d09$ForkLength.mm, d09$Mass.g, d09$ATPaseActivity, d09$Surgeon)

# Assign standardized column names
  names(d09)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
  	"RKM", "Easting", "Northing",	"DateTime", "Origin",
  	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
   	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

# Write the final database to a file
  # This will save it as a comma separated text file ('Final XXXX Database.txt')
    #write.table(d09,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    #row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
    save(d09, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile the 2010 Smolt data and create final 2010 database
#
# INPUT FILENAMES:  '2010 ATS Smolt Detections.txt',
#                   '2005-2013 Surgery.txt',
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code
#                   'd10', 'detections10'
# OUTPUT FILENAMES: 'Final 2010 Database.txt' (d10),
#                   'Final 2010 Database.rda' (d10); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  require(lubridate)
  require(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery<-read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg10<-surgery[year(as.character(surgery$ReleaseDate))==2010,]
    surg10$ReleaseDate<-ymd_hms(as.character(surg10$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers)<-c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data
  #d<-read.csv(file.choose(), header=FALSE)
	#detections10 <- d
	#save(detections10, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Choose '2010 ATS Smolt detections.rda'
  d <- detections10
# May need to change based on VUE export formats
  #d<-d[,c(1,4,5,6,7,3)]
  #colnames(d)<-c("ReceiverSN","TagID","D","T","SensorValue","SensorRange")
  #d$DateTime<-paste(d$D, d$T, sep=" ")
# Re-order fields
  #d<-d[,c(1,2,7,5,6)]
# Change to POSIXct objec
  #d$DateTime <- as.POSIXct(d$DateTime)
  #d$DetNum<-as.numeric(d$DateTime)
  #colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange",
 	  #"DetNum")

# Make df with detections and fish/tag data
  fish.detections <- merge(d, surg10, by="TagID")

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN")
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(qc$ReleaseDate)
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
    	qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Rename the df for lazy loading later on as .rda filetype
    d10<-qc3
	  nrow(d10)

# Add year to the TagID field so that there are no tag overlaps possible
  d10$TagID <- paste(d10$TagID, "-", as.integer(substr(d10$DateTime, 1, 4)),
  sep="")

# Get the desired columns from the data
  d10<-data.frame(d10$TagID, d10$ReceiverSN, d10$Location, d10$LocationCode,
  	d10$RKM, d10$Easting, d10$Northing,	d10$DateTime, d10$Origin,
  	d10$ReleaseSite, d10$R.Easting, d10$R.Northing, d10$ReleaseDate,
   	d10$ForkLength.mm, d10$Mass.g, d10$ATPaseActivity, d10$Surgeon)

# Assign standardized column names
  names(d10)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
  	"RKM", "Easting", "Northing",	"DateTime", "Origin",
  	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
   	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")


# Write the final database to a file
  # This will save it as a comma separated text file ('Final XXXX Database.txt')
    #write.table(d10,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    #row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
    save(d10, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile the 2011 Smolt data and create final 2011 database
#
# INPUT FILENAMES:  '2011 ATS Smolt Detections.rda' (or .txt),
#                   '2005-2013 Surgery.txt,
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code
#                   'd11'
# OUTPUT FILENAMES: 'Final 2011 Database.txt' (d11),
#                   'Final 2011 Database.rda' (d11); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  library(lubridate)
  library(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery<-read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg11<-surgery[year(as.character(surgery$ReleaseDate))==2011,]
    surg11$ReleaseDate<-ymd_hms(as.character(surg11$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers)<-c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data ('2011 ATS Smolt detections.txt')
  #d<-read.csv(file.choose())
	#detections11 <- d
	#save(detections11, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Choose '2011 ATS Smolt detections.rda'
  d <- detections11
# May need to change based on VUE export formats
  d<-data.frame(d$Receiver.S.N, d$ID, d$Date.Time, d$Sensor.1, d$Units.1)
  colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  d$DateTime<-ymd_hms(as.character(d$DateTime))
  d$DetNum<-as.numeric(d$DateTime)

# Make file with detections and fish/tag data
  fish.detections<-merge(d, surg11, all.y=TRUE)

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN")
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(qc$ReleaseDate)
    # Remove detections that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
    	qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Rename the df for lazy loading later on as .rda filetype
    d11<-qc3
    nrow(d11)

# Add year to the TagID field so that there are no tag overlaps possible
  d11$TagID <- paste(d11$TagID, "-", as.integer(substr(d11$DateTime, 1, 4)),
  sep="")

# Get the desired columns from the data
  d11<-data.frame(d11$TagID, d11$ReceiverSN, d11$Location, d11$LocationCode,
  	d11$RKM, d11$Easting, d11$Northing,	d11$DateTime, d11$Origin,
  	d11$ReleaseSite, d11$R.Easting, d11$R.Northing, d11$ReleaseDate,
   	d11$ForkLength.mm, d11$Mass.g, d11$ATPaseActivity, d11$Surgeon)

# Assign standardized column names
  names(d11)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
  	"RKM", "Easting", "Northing",	"DateTime", "Origin",
  	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
   	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

# Write the final database to a file
  # This will save it as a comma separated text file ('Final XXXX Database.txt')
    #write.table(d11,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    #row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
  # ('Final 2011 Database.rda)
    save(d11, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile the 2012 Smolt data and create final 2012 database
#
# INPUT FILENAMES:  '2012 ATS Smolt Detections.rda' (or .txt),
#                   '2005-2013 Surgery.txt'
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code
#                   'd12'
# OUTPUT FILENAMES: 'Final 2012 Database.txt' (d12),
#                   'Final 2012 Database.rda' (d12); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  library(lubridate)
  library(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery<-read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg12<-surgery[year(as.character(surgery$ReleaseDate))==2012,]
    surg12$ReleaseDate<-ymd_hms(as.character(surg12$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers)<-c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data ('XXXX ATS Smolt detections.txt')
  #d <- read.csv(file.choose())
  #detections12 <- d
	#save(detections12, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Choose '2012 ATS Smolt detections.rda'
  d <- detections12

# Make file with detections and fish/tag data
  fish.detections<-merge(d, surg12, all.y=TRUE)

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN")
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(qc$ReleaseDate)
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
    	qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Get rid of the numeric date columns that are not needed
    qc3<-qc3[,(-c(6,26:28))]

  # Rename the df for lazy loading later on as .rda filetype
    d12<-qc3
    nrow(d12)

# Add year to the TagID field so that there are no tag overlaps possible
  d12$TagID <- paste(d12$TagID, "-", as.integer(substr(d12$DateTime, 1, 4)),
  sep="")

# Get the desired columns from the data
  d12<-data.frame(d12$TagID, d12$ReceiverSN, d12$Location, d12$LocationCode,
  	d12$RKM, d12$Easting, d12$Northing,	d12$DateTime, d12$Origin,
  	d12$ReleaseSite, d12$R.Easting, d12$R.Northing, d12$ReleaseDate,
   	d12$ForkLength.mm, d12$Mass.g, d12$ATPaseActivity, d12$Surgeon)

# Assign standardized column names
  names(d12)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
  	"RKM", "Easting", "Northing",	"DateTime", "Origin",
  	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
   	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

# Write the final database to a file
  # This will save it as a comma separated text file ('Final XXXX Database.txt')
    #write.table(d12,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    #row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
  # ('Final XXXX Database.rda)
    save(d12, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile the 2013 Smolt data and create final 2013 database
#
# INPUT FILENAMES:  '2013 ATS Smolt Detections.rda' (or .txt),
#                   '2005-2013 Surgery.txt
#                   '2005-2013 Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code
#                    'd13', d13DepthTags'
# OUTPUT FILENAMES: 'Final 2013 Database.txt' (d13),
#                   'Final 2013 Database.rda' (d13); for faster loading
#                   'DepthDataForDepth-TaggedFish.rda'
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  library(lubridate)
  library(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-2013 Surgery.txt'
    surgery<-read.csv(file.choose()) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg13<-surgery[year(as.character(surgery$ReleaseDate))==2013,]
    surg13$ReleaseDate<-ymd_hms(as.character(surg13$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))
  receivers<-data.frame(receivers[,1],receivers[,4:(ncol(receivers))])
  colnames(receivers)<-c("ReceiverSN", "Location", "LocationCode", "Easting",
 	   "Northing", "Deployed", "Retrieved", "RKM", "DepNum", "RetNum")

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data ('XXXX ATS Smolt detections.txt' or .csv)
  #d <- read.csv(file.choose())
  #detections13 <- d
	#save(detections13, file = tclvalue(tcl("tk_getSaveFile")))
	load(file.choose()) # Choose '2013 ATS Smolt detections.rda'
  d <- detections13
# May need to change based on VUE export formats
  d<-data.frame(d$Receiver.S.N, d$ID, d$Date.Time, d$Sensor.1, d$Units.1)
  colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  d$DateTime<-ymd_hms(as.character(d$DateTime))
  d$DetNum<-as.numeric(d$DateTime)

# Make file with detections and fish/tag data
  fish.detections<-merge(d, surg13, by="TagID",all.y=TRUE)

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary
  Rawdata<-merge(fish.detections, receivers, by="ReceiverSN")
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(qc$ReleaseDate)
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
    	qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Rename the df for lazy loading later on as .rda filetype
    d13<-qc3
    nrow(d13)

# Add year to the TagID field so that there are no tag overlaps possible
  d13$TagID <- paste(d13$TagID, "-", as.integer(substr(d13$DateTime, 1, 4)),
  sep="")

# Create the database for depth-tagged fish from 2013
	# Get data for just the depth tags
  	d13DepthTags <- data.frame(d13[is.na(d13$SensorValue)==FALSE,])
	# Calculate the depth of each fish at each timestamp
  	d13DepthTags$Depth<- 0.8794+0.2918*d13DepthTags$SensorValue
	# Put the columns in the desired order
    d13DepthTags<-data.frame(d13DepthTags$TagID, d13DepthTags$ReceiverSN,
    	d13DepthTags$Location, d13DepthTags$LocationCode,	d13DepthTags$RKM,
    	d13DepthTags$Easting, d13DepthTags$Northing,	d13DepthTags$DateTime,
    	d13DepthTags$Origin, d13DepthTags$ReleaseSite, d13DepthTags$R.Easting,
    	d13DepthTags$R.Northing, d13DepthTags$ReleaseDate,
    	d13DepthTags$ForkLength.mm,	d13DepthTags$Mass.g,
    	d13DepthTags$ATPaseActivity, d13DepthTags$Surgeon,	d13DepthTags$Depth)
	# Assign meaningful names to the columns of the dataframe
  	names(d13DepthTags)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
    	"RKM", "Easting", "Northing",	"DateTime", "Origin",
    	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
     	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon","Depth")
	# Write it to a file
	  save(d13DepthTags, file = tclvalue(tcl("tk_getSaveFile")))

# Compile the location data (without depth info) for 2013 tags
  # Get the desired columns from the data
    d13<-data.frame(d13$TagID, d13$ReceiverSN, d13$Location, d13$LocationCode,
    	d13$RKM, d13$Easting, d13$Northing,	d13$DateTime, d13$Origin,
    	d13$ReleaseSite, d13$R.Easting, d13$R.Northing, d13$ReleaseDate,
     	d13$ForkLength.mm, d13$Mass.g, d13$ATPaseActivity, d13$Surgeon)
  # Assign standardized column names
    names(d13)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
    	"RKM", "Easting", "Northing",	"DateTime", "Origin",
    	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
     	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

  # Write the final database to a file
    # This will save it as a comma separated text file
	  # ('Final XXXX Database.txt')
      #write.table(d13,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	      #row.names=FALSE)
    # This one will save it as an R-data file, which will load faster later on!
    # ('Final XXXX Database.rda)
      save(d13, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Compile all Atlantic salmon smolt detection data for 2005-2013
#
# NOTE: THIS DATABASE DOES NOT CONTAIN ANY DEPTH/TEMPERATURE DATA FROM THE
# 2013 DEPTH-TAGGED SMOLTS. THOSE DATA ARE STORED ELSEWHERE TO FACILITATE A
# LESS-COMPLEX HANDLING OF THEIR ANALYSIS
#
# INPUT FILENAME(S): 'Final 2005 & 2006 Database.rda','Final 2009 Database.rda',
#                    'Final 2010 Database.rda','Final 2011 Database.rda',
#                    'Final 2012 Database.rda','Final 2013 Database.rda',
#                    '2005-2013 Surgery.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:      'AllData', 'Releases'
# OUTPUT FILENAME:   'Final 2005-2013 Database.txt' (AllData),
#                    'Final 2005-2013 Database.rda' (AllData)
################################################################################
# Install and load packages
	#install.packages('lubridate')
	#install.packages('tcltk')
	#install.packages('tcltk2')
	require(lubridate)
	require(tcltk)
	require(tcltk2)

# Set system time to UTC so everything is standardized in capture histories
  Sys.setenv( TZ = 'UTC' )

# Read in the files as saved .rda databases for faster handling time
  # Choose 'Final 2005 & 2006 Database.rda'
    load(file.choose())
  # Choose 'Final 2009 Database.rda'
    load(file.choose())
  # Choose 'Final 2010 Database.rda'
    load(file.choose())
  # Choose 'Final 2011 Database.rda'
    load(file.choose())
  # Choose 'Final 2012 Database.rda'
    load(file.choose())
  # Choose 'Final 2013 Database.rda'
    load(file.choose())

# Smash the annual dbases into a single file, about 10 seconds
  unix.time(AllData <- rbind(d0506, d09, d10, d11, d12, d13))

# Give each fish a location at its release site, to be combined with 'AllData'
# Include the Release event for each fish in the capture history, put the
# information about the release site in the df in place of the 'normal' data
# for each location in the river.  This will create a location event for each
# of the fish upon release.
	# Load surgery data
	  surgery <- read.csv(file.choose())
  # Create a dataframe to hold release 'detections'
	  Releases <- as.data.frame(matrix(ncol=ncol(AllData), nrow=nrow(surgery)))
  # Assign names to the df
	  names(Releases) <- names(AllData)
  # Populate the df with 'detections' and the information from the surgery tab
	  Releases$TagID <- paste(surgery$TagID, "-",
	  	as.integer(substr(surgery$ReleaseDate, 1, 4)), sep="")
    Releases$ReceiverSN <- surgery$ReleaseSite
    Releases$Location <- surgery$ReleaseSite
    Releases$LocationCode <- surgery$ReleaseSite
    Releases$Easting <- surgery$R.Easting
    Releases$Northing <- surgery$R.Northing
    Releases$DateTime <- surgery$ReleaseDate
	  Releases$Origin <- surgery$Origin
	  Releases$ReleaseSite <- surgery$ReleaseSite
	  Releases$R.Easting <- surgery$R.Easting
	  Releases$R.Northing <- surgery$R.Northing
	  Releases$ReleaseDate <- surgery$ReleaseDate
	  Releases$ForkLength.mm <- surgery$ForkLength.mm
	  Releases$Mass.g <- surgery$Mass.g
    Releases$ATPaseActivity <- surgery$ATPaseActivity
	  Releases$Surgeon <- surgery$Surgeon
  # Get RKM for each of the release sites so it can be added to 'Releases'
    # Get the unique values of release site
      ReleaseSites <- c(sort(unique(as.character(surgery$ReleaseSite))))
    # Make a vector of corresponding rkms for release sites
      ReleaseRKMs <- c(187, 43.5, 162, 99, 144, 142,  92.3, 9.5, 149)
  # Add RKM to 'releases' that way release locations have RKM associated w/ them
    Releases$ReleaseSite <- as.character(Releases$ReleaseSite)
    ReleaseSites <- as.character(ReleaseSites)
    for(i in 1:nrow(Releases)){
    	for(t in 1:length(ReleaseSites)){
      	if(Releases$ReleaseSite[i] == ReleaseSites[t]){
    	  	Releases$RKM[i] <- ReleaseRKMs[t]
    	  } else {
    	  	next;
    	  }
      }
    }

# Combine the release history with the 'AllData' dataframe to create
# comprehensive histories for each fish from release to final location
  AllData <- rbind(AllData, Releases) # About 10 seconds to combine

# Remove the weird NA-NA tag from the final database
	AllData <- AllData[(AllData$TagID!="NA-NA"),]

#	Check the number of tags in the dataframe- should be 2212 if everything worked
	length(unique(AllData$TagID))

# Write the final database to a file
  # This will save it as a comma-separated text file
  # ('Final 2005-2013 Database.txt')
    write.table(AllData, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    row.names=FALSE)
  # This one will save it as an R-data file, which will load faster later on!
  # ('Final 2005-2013 Database.rda)
    save(AllData, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################
} # Code folding marker
# END PART 2. DATA COMPILATION--------------------------------------------------


# PART 3. DATA MANIPULATION FOR ESTUARY CAPTURE HISTORIES-----------------------
{ # Code Folding marker
################################################################################
# Get min of DateTime for each fish at each location using C++ loop(s)
#
# INPUT FILENAME(S): 'Final 2005-2013 database.rda' (or .txt)
# MY FUNCTIONS:      'DTAgain' created, 'timesC' created
# NAMED OUTPUT:      'timestamp', 'caps'
# OUTPUT FILENAME:   'MinDateTimeGod.csv', 'MinDateTimeGod.rda'
################################################################################
# Install and load necessary packages
	#install.packages('Rcpp') # Uncomment to run, may require restart of R
	#install.packages('lubridate') # Uncomment to run
	#install.packages('reshape') # Uncomment to run
	#install.packages('reshape2') # Uncomment to run
	require(Rcpp)
  require(lubridate)
	require(reshape)
	require(reshape2)
	require(tcltk2)

# Set system time to UTC for ease of handling time/number conversions later on
  Sys.setenv(TZ='UTC')

# Read in 'Final 2005-2013 database.rda'
	load(file.choose())

# Prep timestamp data from the main database and put it in necessary format
  dt <- as.POSIXct(AllData$DateTime, tz="UTC")

# Make fxn for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

# Variable definition for use in c++ fxns
   y <- as.character(AllData$TagID) # Make TagID character var
   w  <- as.numeric(dt, origin=1970-01-01) # Make date numeric for ease
   v <- as.character(AllData$LocationCode) # Make LocationCode character var
   data2 <- unique(data.frame(y,v,w)[,1:2]) # Make df of unique combinations
   y2 <- data2[ , 1] # Vectorize values of TagID in data2
   v2 <- data2[ , 2] # Vectorize values of LocationCode in data2

# Create function to get min DateTime for each fish at each location
# Includes a progress meter that reports % complete for external ('i') loop
  cppFunction('
    NumericVector timesC(CharacterVector v, CharacterVector y,
    NumericVector w, CharacterVector v2, CharacterVector y2){
      int n = v.size();
      NumericVector z(n);
      int m = v2.size();
      NumericVector Timestamp(m);
      float length = m;
      std::cout << std::endl;

      for(int i=0; i < m; ++i) {
        for(int t=0; t < n; ++t) {
          if(y[t]==y2[i] && v[t]==v2[i]) {
            z[t] = w[t];
          } else {
            z[t] = 1e16;
          }
        }
        std::cout<<"\\r"
        <<100*(float)(i+1)/(float)length
        <<"% "
        <<std::flush;
        Timestamp[i] = min(z);
      }
      return Timestamp;
    }
  ')

# Get min Date time for each fish at each location (run the function)
  unix.time(timestamp <- timesC(v,y,w,v2,y2)) # Call fxn & time it, over 1 hr
  head(DTAgain(timestamp)) # View results to check datatype
  Dates<-DTAgain(timestamp) # Convert results back to DateTime from numeric
  head(Dates) # Data QC- spotcheck

# Make df with TagID, LocationCode, min date for each tag at each location
  caps<-data.frame(data2,Dates) # Create the object
  names(caps)<-c("TagID","LocationCode","DateTime") # Rename the columns

# Write the min DateTime for each fish at each location to a .csv file
# this will prevent data loss when I pull a dumb-dumb and close out!
# Requires tcltk2 package
# This is called 'MinDateTimeGod.csv'
  write.table(caps, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    row.names=FALSE)
# Write it to an Rdata file for faster future loading
# ('MinDateTimeGod.rda')
	save(caps, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Get RKM for each location in the caps df
#
# INPUT FILENAME(S): '2005-2013 Acoustic receiver diary.txt',
#                    'MinDateTimeGod.csv' (or 'caps' df)
# MY FUNCTIONS:      'rkmC' created/used
# NAMED OUTPUT:      'rk', 'newCaps'
# OUTPUT FILENAME:   'MinDateTimeGodWithRKM.csv', 'MinDateTimeGodWithRKM.rda'
################################################################################
# Install and load necessary packages
	#install.packages('Rcpp') # Uncomment to run, may require restart of R
	#install.packages('lubridate') # Uncomment to run
	#install.packages('reshape') # Uncomment to run
	#install.packages('reshape2') # Uncomment to run
	require(Rcpp)
  require(lubridate)
	require(reshape)
	require(reshape2)
	require(tcltk2)

# Read data file 'Final 2005-2013 Database.rda'
  #load(file.choose())
# Create a vector of River kilometers for each location in the receiver diary
  rkms <- unique(AllData[ , c(4, 5)])

# Make sure variable type is not factor
	rkms$LocationCode <- as.character(rkms$LocationCode)

# Read in 'DateTimeGod.csv' if caps is not in workspace
  #caps <- read.csv(file.choose()) # Uncomment to run
  caps$LocationCode <- as.character(caps$LocationCode) # Format data

# Create variables for C++ Loop used to get RKM in each
  rkm <- rkms$RKM # Vectorize unique values of rkm
  receiversLocations <- rkms$LocationCode # Vectorize unique location code
  capsLocations <- caps$LocationCode # Vectorize location from caps df above

# Make c++ loop to get rkm for each location in caps df above
# Includes a progress meter that reports % complete for external ('i') loop
  cppFunction('
    NumericVector rkmC(CharacterVector capsLocations,
    NumericVector rkm, CharacterVector receiversLocations){
      int n = capsLocations.size();
      NumericVector z(n);
      int m = receiversLocations.size();
      NumericVector RKM(m);
      float length = m;
      std::cout << std::endl;

        for(int i=0; i < m; ++i) {
          for(int t=0; t < n; ++t) {
            if(capsLocations[t]==receiversLocations[i]) {
              z[t] = rkm[i];
            } else {
              continue;
            }
          }
          std::cout<<"\\r"
          <<100*(float)(i+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return z;
      }
    ')
    rk<-rkmC(capsLocations, rkm, receiversLocations) # Run the function
    newCaps<-data.frame(caps,rk) # Add rkm to the caps df
    names(newCaps)[4]<-"RKM" # Change the name of the RKM column
    head(newCaps) # Data QC- spotcheck

  # Write the newCaps DF to a csv for easy reloading
  # This file is called MinDateTimeGodWithRKM
    write.table(newCaps, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
      row.names=FALSE)
	# Write to an r-data file
	  save(newCaps, file=tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Make timestamp matrix for time-varying covariates
#
# INPUT FILENAME(S): 'MinDateTimeGodWithRKM.csv' (or 'newCaps' df)
# MY FUNCTIONS:      'DTAgain' used
# NAMED OUTPUT:      'Date'
# OUTPUT FILENAME:   'DateTimeMatrix.csv', 'DateTimeMatrix.rda'
################################################################################
# Install and load necessary packages
	#install.packages('lubridate') # Uncomment to run
	#install.packages('tcltk2') # Uncomment to run
  require(lubridate)
	require(tcltk2)

# Read in 'PNRMarkRecaptureData2005-2013.csv' if 'newCaps' is not in workspace
  newCaps <- read.csv(file.choose())
# Convert timestamp to POSIXct object
  newCaps$DateTime  <- as.POSIXct(newCaps$DateTime, tz="UTC")

# Create a matrix of dates for each fish at each location, takes a few seconds
  Date <- cast(newCaps, TagID ~ RKM + LocationCode, value="DateTime",
  	fun.aggregate=min)

	# Put the columns in order of decreasing rkm
	  Date <- Date[,c(1,rev(2:ncol(Date)))]

  # Compile the last few detections into a single column
    Bay <- apply(Date[,(ncol(Date)-5):ncol(Date)], 1, min)

# Rearrange date matrix
	Date <- cbind(Date[ , c(1:(ncol(Date)-6))],Bay)

# Change the name for the fort point location
	names(Date)[(ncol(Date))] <- "-6_BAY"

# Get rid of 'infinity' values in the DateTime matrix
  for(i in 2:ncol(Date)){
  	Date[,i][is.infinite(Date[,i])] <- 0
  	Date[,i] <- DTAgain(Date[,i])
  }

# Write the DF with temporal matrix to a csv file
# This is called DateTimeMatrix.csv and holds Date/Time each fish was seen
  write.table(Date, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    row.names=FALSE)
# Write it to an r-data file
	save(Date, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Make matrix of the DAY that each fish was located at each receiver location
# by getting rid of the timestamps.  This will form the dates used in getting
# time-varying values of Temperature and flow for time-varying covariates
# used in mark-recapture survival analysis.
#
# INPUT FILENAME(S): 'DateTimeMatrix.csv' (or 'Date' df)
# MY FUNCTIONS
# NAMED OUTPUT:      'DateOnly', 'DateOnly2', 'DateOnly3'
# OUTPUT FILENAME:   'DateOnly3.csv'
################################################################################
# Package loading and install
	#install.packages("tcltk2") # Uncomment start of line to run
	#install.packages("lubridate") # Uncomment start of line to run
	require(tcltk2)
	require(lubridate)

# Get date into 'date only' format
  # Read in 'DateTimeMatrix.csv' if 'Date' is not already in memory.
    Date <- load(file.choose())
  # Make two empty matrices to hold Character strings of Timestamps from 'Date'
  # above (DateOnly), and Substrings that only have the date from those
  # timestamps (DateOnly2)
    DateOnly<-matrix(ncol=ncol(Date), nrow=nrow(Date))
    DateOnly2<-matrix(ncol=ncol(Date), nrow=nrow(Date))

  # Create progress bar to monitor percent complete
    total <- nrow(Date) # Total number of interations to monitor progress over
    pb <- tkProgressBar(title = "DateOnly3 Matrix build progress",
    	min = 0, max = total, width = 400)

  # Make sure the time matches that in 'Date'
    Sys.setenv(TZ='UTC')

  # Make nested loop to get new df with dates as char
    for (i in 1:nrow(Date)){ # For each tag
    	for (t in 1:ncol(Date)){ # At each location
    		DateOnly[i, t] <- as.character(Date[i, t]) # Get the date chars
    		DateOnly2[i, t] <- substr(DateOnly[i, t], # Strip the date out
          nchar(DateOnly[i, t])-nchar(DateOnly[i, t]),
          nchar(DateOnly[i, t])-nchar(DateOnly[i, t])+10)
  	  } # Close t
      # Don't go to sleep
  		  Sys.sleep(0.1)
      # Print progress
        setTkProgressBar(pb, i,
          label = paste( round(i/total*100, 0), "% Completed"))
  	} # Close i loop
    close(pb) # Close progress bar

  # Remove TagID for processing in C++ loops below, no need to write to a file
    DateOnly3 <- DateOnly2[ , 2:ncol(DateOnly2)]

  # Save 'DateOnly3' as 'DateOnlyMatrix.csv'
    write.table(DateOnly3, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    row.names=FALSE)
	# Save it as an r-data file
	  save(DateOnly3, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Extract temperature and precipitation data for the entire watershed using
# shapefile of watershed and weather data from the PRISM database at Oregon
# State University:
# browseURL(http://www.prism.oregonstate.edu/) # Uncomment to run this line
#
# INPUT FILENAME(S): 'Penob.shp',
#                     directory '*/PRISM_TempData/',
#                     directory '*/PRISM_PrecipData/'
# MY FUNCTIONS:      'substrRight'
# NAMED OUTPUT:      'caps'
# OUTPUT FILENAME:   'PRISM_PrecipData.csv', 'PRISM_PrecipData.rda',
#                    'PRISM_TempData.csv', 'PRISM_TempData.rda'
################################################################################
# Install and load spatial data packages; uncomment to install
  #install.packages(c("rgdal","raster","R.utils","maptools","spsurvey","rgeos",
  #"tcltk2", "sp"), dep=TRUE)
  require(rgdal)
  require(R.utils)
  require(spsurvey)
  require(maptools)
  require(raster)
  require(sp)
  require(rgeos)
  require(tcltk2)

# Reset the coordinate system for the PNR watershed using one of the rasters
  # Read in shape file with watershed boundaries
  # (*/'Penob.shp')
    s<-readShapePoly(file.choose())
  # Read in a raster (e.g. '*/PRISM_ppt_stable_4kmD1_20050101_bil.bil')
    r <- raster(file.choose())
  # Change coordinate system for HUC
    projection(s)<-projection(r)

# Set a directory for working with the Precip Data
  setwd(choose.dir())

# List the files in the directory that have a '.bil' extension
  files<-list.files(pattern = "\\.bil$")
  head(files) # Spot check to make sure it worked

# Write fxn to strip date out of each of the filenames
  substrRight <- function(x, n){
    substr(x, nchar(x)-2*n+1, nchar(x)-n)
  }

# PRECIPITATION DATA ROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{ # Code-folding marker
  # Make empty columns to hold the results
  DTReference <- c() # Date and time
  Precip.sum <- c() # Sum of precipitation in watershed on each date
  Precip.mean <- c() # Mean precip by 4-km squares for each date
  Precip.min <- c() # Minimum precip in watershed on each date
  Precip.max <- c() # Maximum precip in watershed on each date
  Precip.sd <- c() # Standard deviation of precip in watershed on each date
  Precip.quants <- matrix(nrow=length(files), ncol=11) # Empty matrix for quants
  probabilities<-c(0.025,0.05,0.10,0.20,0.25,0.50,0.75,0.80,0.90,0.95,0.975)

# Create progress bar
  total <- length(files) # Get the finish line for the GUI bar
  pb <- tkProgressBar(title = "Precipitation data retrieval progress",
  	min = 0, max = total, width = 400)

# Monster loop to do every file
  for(i in 1:total){
  # Get the spatial extent of the shapefile
    e <- extent(s)
  # Read in the raster for cropping
  	r<- raster(files[i])
  # Change coordinate system for HUC
    projection(s)<-projection(r)
  # Cropping the raster to the shapefile spatial extent
    myraster.crop <- crop(r, e, snap="out")
  # Dummy raster with a spatial extension equal to the cropped raster,
  # but full of NA values
    crop <- setValues(myraster.crop, NA)
  # Rasterize the catchment boundaries, with NA outside the catchment
  # waterhed boundaries
    myshp.r <- rasterize(s, crop, silent=TRUE)
  # Putting NA values in all the raster cells outside the shapefile boundaries
    myraster.masked <- mask(x=myraster.crop, mask=myshp.r)
  # Plot to make sure it worked
    #par(bg="gray87")
    #plot(myraster.masked)
  # Get statistics of interest
    DTReference[i] <- substrRight(files[i], 8)
  	Precip.sum[i] <- sum(myraster.masked$values[,], na.rm=TRUE)
    Precip.mean[i] <- mean(myraster.masked$values[,], na.rm=TRUE)
    Precip.min[i] <- min(myraster.masked$values[,], na.rm=TRUE)
    Precip.max[i] <- max(myraster.masked$values[,], na.rm=TRUE)
    Precip.sd[i] <- sd(myraster.masked$values[,], na.rm=TRUE)
    Precip.quants[i,1:11] <- t(quantile(myraster.masked$values[,], na.rm=TRUE,
    	probs=probabilities))
  # Do a printout of progress
    Sys.sleep(0.001)
    setTkProgressBar(pb, i, label=paste(round(i/total*100, 0),
    	"% Precipitation data retrieved"))
  } # Close out the i loop
  close(pb) # Close out the progress bar

# Collect data into a data.frame
  Precip.data <- data.frame(DTReference,Precip.sum,Precip.mean, Precip.sd,
  	Precip.min, Precip.max,	Precip.quants)

# Give the dataframe names
  names(Precip.data) <- c("Date","Precip.sum","Precip.mean","Precip.sd",
  	"Precip.min","Precip.max","0.025","0.05","0.10","0.20","0.25","0.50","0.75",
  	"0.80","0.90","0.95","0.975")

# Write the Penobscot River precipitation data to a file
  # As text file, uncomment to run, name it '
    #write.table(Precip.data, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    #row.names=FALSE)
  # As an R-data file:
    save(Precip.data, file = tclvalue(tcl("tk_getSaveFile")))

} # Code-folding marker
# END PRECIPITATION ROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TEMPERATURE DATA ROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{ # Code-folding marker
# Set a directory for working with the Precip Data
  setwd(choose.dir())
  files<-list.files(pattern = "\\.bil$")
  #head(files)

# Make empty columns to hold the results
  DTReference <- c()
  Temp.mean <- c()
  Temp.min <- c()
  Temp.max <- c()
  Temp.sd <- c()
  Temp.quants <- matrix(nrow=length(files), ncol=11)
  probabilities<-c(0.025,0.05,0.10,0.20,0.25,0.50,0.75,0.80,0.90,0.95,0.975)

# Create progress bar
  total <- length(files) # Get the finish line for the GUI bar
  pb <- tkProgressBar(title = "Temperature data retrieval progress",
  	min = 0, max = total, width = 400)

# Monster loop to do every file
  for(i in 1:total){
  # Get the spatial extent of the shapefile
    e <- extent(s)
  # Read in the raster for cropping
  	r<- raster(files[i])
  # Change coordinate system for HUC
    projection(s)<-projection(r)
  # Cropping the raster to the shapefile spatial extent
    myraster.crop <- crop(r, e, snap="out")
  # Dummy raster with a spatial extension equal to the cropped raster,
  # but full of NA values
    crop <- setValues(myraster.crop, NA)
  # Rasterize the catchment boundaries, with NA outside the catchment
  # waterhed boundaries
    myshp.r <- rasterize(s, crop, silent=TRUE)
  # Putting NA values in all the raster cells outside the shapefile boundaries
    myraster.masked <- mask(x=myraster.crop, mask=myshp.r)
  # Plot to make sure it worked
    #par(bg="gray87")
    #plot(myraster.masked)
  # Get statistics of interest
    DTReference[i] <- substrRight(files[i], 8)
    Temp.mean[i] <- mean(myraster.masked$values[,], na.rm=TRUE)
    Temp.min[i] <- min(myraster.masked$values[,], na.rm=TRUE)
    Temp.max[i] <- max(myraster.masked$values[,], na.rm=TRUE)
    Temp.sd[i] <- sd(myraster.masked$values[,], na.rm=TRUE)
    Temp.quants[i,1:11] <- t(quantile(myraster.masked$values[,], na.rm=TRUE,
    	probs=probabilities))
  # Do a printout of progress
    Sys.sleep(0.1)
    setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),
    	"% Temperature data retrieved"))
  } # Close out the i loop
  close(pb) # Close out the progress bar

# Collect data into a data.frame
  Temp.data <- data.frame(DTReference,Temp.mean, Temp.sd,
  	Temp.min, Temp.max,	Temp.quants)

# Give the dataframe names
  names(Temp.data) <- c("Date","Temp.mean","Temp.sd",
  	"Temp.min","Temp.max","0.025","0.05","0.10","0.20","0.25","0.50","0.75",
  	"0.80","0.90","0.95","0.975")

# Write the data to a file
  # As text file, uncomment to run
    #write.table(Temp.data, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    #row.names=FALSE)
  # As an R-data file:
    save(Temp.data, file = tclvalue(tcl("tk_getSaveFile")))

} # Code-folding marker
# END TEMPERATURE ROUTINE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################################################################################

################################################################################
# Get temp for each day at each location for each fish
#
# INPUT FILENAME(S): 'PRISMTempDataPNR.csv' 'DateTimeMatrix.csv' (or 'Date' df),
#                     (or 'DateOnly3' df)
# MY FUNCTIONS:      'TimeTempC', 'TempCovariateC'
# NAMED OUTPUT:      'TempC', 'TempDate', 'Temp', 'time.Temp', 'TempMatrix',
#                    'TempMean', 'temp.Covariate'
# OUTPUT FILENAME:   'TimeVaryingTemperatureCovariate.csv'
################################################################################
# Package install and load
	#install.packages('Rcpp')
	#install.packages('tcltk2')
	require(Rcpp)
	require(tcltk2)

# Create matrix with temperature for each day
  # Read in 'PrismTempDataPNR.csv' or 'PRISMTempDataPNR.rda'
	  #Temp.data <- read.csv(file.choose()) # This is for .csv OR
    load(file.choose()) # This is for .rda

  # Read in 'DateOnlyMatrix.csv' if 'DateOnly3' df not in workspace
    #DateOnly3 <- read.csv(file.choose()) # Uncomment to run for .csv file
	  load(file.choose()) # Uncomment to load the .rda version

  # Strip date from timestamp that was included in the file name of each
  # raster file that estimated mean temp in 4-km blocks
    TempC <- Temp.data[ ,1:2] # Get date and mean temp in watershed

  # Strip year out of string
    TempC[ ,3] <- substr(TempC[ ,1],nchar(TempC[ ,1])-nchar(TempC[ ,1]),
    	nchar(TempC[ ,1])-nchar(TempC[ ,1])+4)

  # Strip month out of string
    TempC[ ,4] <- substr(TempC[ ,1],nchar(TempC[ ,1])-nchar(TempC[ ,1])+5,
      nchar(TempC[ ,1])-nchar(TempC[ ,1])+6)

  # Strip day out of string
    TempC[ ,5] <- substr(TempC[ ,1],nchar(TempC[ ,1])-nchar(TempC[ ,1])+7,
    	nchar(TempC[ ,1]))

  # Paste year, month, and day together; separate with '-'
    TempC[ ,6] <- as.character( paste(TempC$V3,TempC$V4, TempC$V5, sep="-") )

  # Make a dataframe containing new date column and mean Temp.data in
  # watershed for all days 01/01/05-08/31/13
    TempC <- data.frame(TempC$V6,TempC$Temp.mean)

  # Assign names to the columns of the dataframes
    names(TempC) <- c("Date" ,"MeanTempC")

  # Make the 'Dates' column into variable type=Character
    TempC[ ,1] <- as.character(TempC[ ,1])

  # Make a character vector of dates for use in timeTempC
    TempDate <- TempC[ ,1]

  # Make a numeric vector of mean Temp.datas in watershed for use in timeTempC
    Temp <- TempC[ ,2]

# Make a matrix that holds temperature experienced by each fish at each
# location on the firt date that it was located there from a matrix and
# two vectors using a C++ function
  cppFunction('
    NumericMatrix timeTempC(CharacterMatrix DateOnly3,
    CharacterVector TempDate, NumericVector Temp){
      int n = DateOnly3.nrow();
      int m = DateOnly3.ncol();
      int q = Temp.size();
      NumericMatrix timeTemp(DateOnly3.nrow(),DateOnly3.ncol());
      float length = q;
      std::cout << std::endl;

      for(int j=0; j < q; ++j){
        for(int i=0; i < n; ++i) {
          for(int t=0; t < m; ++t) {
             if(DateOnly3(i,t)==TempDate[j]) {
                timeTemp(i,t)=Temp[j];
             } else {
               if(DateOnly3(i,t)=="1970-01-01"){
                  timeTemp(i,t)=-999;
               } else {
                 continue;
               }
             }
           }
         }
         std::cout<<"\\r"
         <<100*(float)(j+1)/(float)length
         <<"% "
         <<std::flush;
       }
       return timeTemp;
     }
  ')
  # Run function timeTempC, time it, and store the output for use below
    unix.time(time.Temp<-timeTempC(DateOnly3,TempDate,Temp))
  # Spot check the data to make sure it worked correctly
    time.Temp[600:610,1:5]

# Put the TagIDs back on the front end of time-varying temp covariate and get
# rid of -999 values by replacing with mean temperature in watershed.
# Temperature is mean temperature in the watershed each day
  # Make a matrix of temperatures to work with
    TempMatrix <- time.Temp
  # Make a vector of the matrix for summary stats
    TempVector <- as.vector(TempMatrix)
  # Take mean of TempVector with which to replace -999 values
  # Exclude the -999 values from the mean
    TempMean <- mean(TempVector[TempVector!=-999])
  # Make nested for loop to replace -999 values in TempMatrix with mean
    cppFunction('
      NumericMatrix TempCovariateC(NumericMatrix TempMatrix){
        int n = TempMatrix.nrow();
        int m = TempMatrix.ncol();
        NumericMatrix TempCovariate(TempMatrix.nrow(),TempMatrix.ncol());
        float length = n;
        std::cout << std::endl;

          for(int i=0; i < n; ++i){
            for(int t=0; t < m; ++t){
              if(TempMatrix(i,t)==-999){
                TempCovariate(i,t)=9.854804;
              } else {
                TempCovariate(i,t)=TempMatrix(i,t);
              }
            }
            std::cout<<"\\r"
            <<100*(float)(i+1)/(float)length
            <<"% "
            <<std::flush;
          }
          return TempCovariate;
        }
      ')

  # Run temp.Covariate fxn to get the final time-varying covariate
    temp.Covariate <- TempCovariateC(TempMatrix)
  # Create a dataframe containing TagID and temperature covariate
    temp.Covariate <- data.frame(Date[,1], temp.Covariate)
  # Assign names to the dataframe
    names(temp.Covariate) <- names(Date)
	  names(temp.Covariate)
  # Write it to a .csv file named 'TimeVaryingTemperatureCovariate.csv'
  	setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
    write.table(temp.Covariate, file = 'TimeVaryingTemperatureCovariate.csv',
    	sep=",", row.names=FALSE)
	# Save it as r-data file
	  save(temp.Covariate, file = 'TimeVaryingTemperatureCovariate.rda')
################################################################################

################################################################################
# Get total precipitation in the watershed for the day each fish was observed
# at each location throughout the system
#
# INPUT FILENAME(S): 'PrismPrecipDataPNR.rda', 'DateOnlyMatrix.rda'
#                    (or 'DateOnly3' df)
# MY FUNCTIONS:      'timePrecipC', 'precipCovariateC'
# NAMED OUTPUT:      'Precip', 'PrecipDate', 'PrecipMM', 'time.Precip',
#                    'PrecipMatrix', 'PrecipVector', 'PrecipMean',
#                    'precip.Covariate'
# OUTPUT FILENAME:   'TimeVaryingPrecipitationCovariate.csv'
################################################################################
# Package install and load
	#install.packages('Rcpp')
	#install.packages('tcltk2')
	require(Rcpp)
	require(tcltk2)

# Read in Precipitation data ('PrismPrecipDataPNR.rda')
  #Precipitation<-read.csv(file.choose()) # Uncomment to use .csv format
	load(file.choose())

# Strip date from timestamp based on the name of each raster file u sed in
# calculating precipitation statistics for each day.
  # Get df of Date string and total precip in the watershed for each day
    Precip<-Precip.data[,1:2] # Just take first two columns of file
	  Precip$Date <- as.character(Precip$Date) # Make date column chr

  # Strip year
    Precip[,3] <- substr(Precip[,1],nchar(Precip[,1])-nchar(Precip[,1]),
    	            nchar(Precip[,1])-nchar(Precip[,1])+4)

  # Strip month
    Precip[,4] <- substr(Precip[,1],nchar(Precip[,1])-nchar(Precip[,1])+5,
                  nchar(Precip[,1])-nchar(Precip[,1])+6)

  # Strip day
    Precip[,5] <- substr(Precip[,1],nchar(Precip[,1])-nchar(Precip[,1])+7,
    	            nchar(Precip[,1]))

  # Paste year, month, and day together; separate with '-'
    Precip[,6] <- as.character( paste(Precip$V3,Precip$V4, Precip$V5,
  	              sep="-") )

# Make a dataframe of dates and total precip in watershed for each day
  Precip<-data.frame(Precip$V6,Precip$Precip.sum)

# Give the dataframe names
  names(Precip) <- c("Date" ,"MeanPrecip")

# Make 'Date' column into type=Character
  Precip[,1] <- as.character(Precip[,1])

# Make character vector of dates for use in timePrecipC
  PrecipDate <- Precip[,1]

# Make numeric vector of total precip in watershed for use in timePrecipC
  PrecipMM <- Precip[,2]

# Make a matrix that holds the sum of all precipitation in the watershed as
# estimated across 4-km square blocks. This represents the total precip in the
# watershed when a fish was at each location in the estuary.
  cppFunction('
    NumericMatrix timePrecipC(CharacterMatrix DateOnly3,
      CharacterVector PrecipDate, NumericVector PrecipMM){
        int n = DateOnly3.nrow();
        int m = DateOnly3.ncol();
        int q = PrecipMM.size();
        NumericMatrix timePrecip(DateOnly3.nrow(),DateOnly3.ncol());
        float length = q;
        std::cout << std::endl;

        for(int j=0; j < q; ++j){
          for(int i=0; i < n; ++i) {
            for(int t=0; t < m; ++t) {
              if(DateOnly3(i,t)==PrecipDate[j]) {
                timePrecip(i,t)=PrecipMM[j];
              } else {
                if(DateOnly3(i,t)=="1970-01-01"){
                  timePrecip(i,t)=-999;
                } else {
                  continue;
                }
              }
            }
          }
          std::cout<<"\\r"
          <<100*(float)(j+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return timePrecip;
      }
    ')
    # Run the function to get total precip in the watershed for each fish when
    # they were seen at each location.
    # NOTE: will contain NA values (-999) that need to be replaced with means
      unix.time(time.Precip<-timePrecipC(DateOnly3,PrecipDate,PrecipMM))

    # Spot check the data to make sure it worked correctly
      time.Precip[601:610,1:5]

# Make time-varying covariate for precipitation by replacing -999 values with
# mean total precipitation
  # Make new precip matrix for data processing
    PrecipMatrix <- time.Precip

  # Convert to a matrix and take the average for use in precip.Covariate
    PrecipVector <- as.vector(PrecipMatrix)

  # Calculate mean of precipitation vector, leaving out NA values (-999)
    mean(PrecipVector[PrecipVector!=-999])

  # Make nested for loop to replace -999 values in PrecipMatrix with mean
    cppFunction('
      NumericMatrix PrecipCovariateC(NumericMatrix PrecipMatrix){
        int n = PrecipMatrix.nrow();
        int m = PrecipMatrix.ncol();
        NumericMatrix PrecipCovariate(PrecipMatrix.nrow(), PrecipMatrix.ncol());
        float length = n;
        std::cout << std::endl;

        for(int i=0; i < n; ++i){
          for(int t=0; t < m; ++t){
            if(PrecipMatrix(i,t)==-999){
              PrecipCovariate(i,t)=5597.622;
            } else {
              PrecipCovariate(i,t)=PrecipMatrix(i,t);
            }
          }
          std::cout<<"\\r"
          <<100*(float)(i+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return PrecipCovariate;
      }
    ')
    # Run the function (no time b/c it's wicked fast) to create the time-varying
    # precipitation covariate for mark-recapture analyses
      precip.Covariate <- PrecipCovariateC(PrecipMatrix)

    # Make a dataframe that contains tagIDs and the covariate
      precip.Covariate <- data.frame(Date[,1], precip.Covariate)

    # Assign names to the columns of the dataframe
      names(precip.Covariate) <- names(Date)
	    names(precip.Covariate)

  # Write the dataframe to a file; name it
    # 'TimeVaryingPrecipitationCovariate.csv'
	    setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
      write.table(precip.Covariate, file = 'TimeVaryingPrecipitationCovariate.csv',
      	sep=",", row.names=FALSE)
	  # Write it to an r-data file
	    save(precip.Covariate, file = 'TimeVaryingPrecipitationCovariate.rda')
################################################################################

################################################################################
# Get freshwater discharge at the west enfield gage for each day that each fish
# was located
#
# INPUT FILENAME(S): 'WestEnfieldFlowClean.txt', 'DateOnlyMatrix.rda'
#                    'DateTimeMatrix.rda'
# MY FUNCTIONS:      'timeDischargeC', 'DischargeCovariateC'
# NAMED OUTPUT:      'Discharge', 'DischargeDate', 'DischargeMM',
#                    'time.Discharge', 'DischargeMatrix', 'DischargeVector',
#                    'DischargeMean', 'Discharge.Covariate'
# OUTPUT FILENAME:   'TimeVaryingDischargeCovariate.csv'
################################################################################
# Package install and load
	#install.packages('Rcpp')
	#install.packages('tcltk2')
	require(Rcpp)
	require(tcltk2)

# Read in Flow data ('WestEnfieldFlowDataClean.txt'), replace NA with 0 b/c ice
  Discharge <- read.table(file.choose( ), header=TRUE)[ , 3:4]
	names(Discharge) <- c('Date', 'Discharge')
	Discharge[ , 2] <- as.character(Discharge[ , 2])
	for(i in 1:nrow(Discharge)){
		if(is.na(Discharge[i , 2])){
	    Discharge[i , 2] <- 0
		} else {
			next;
		}
	}
	Discharge[ , 2] <- as.numeric(Discharge[ , 2])

# Read in the date matrix for fish locations
	load(file.choose( )) # DateOnlyMatrix.rda

# Load the dateTimeMatrix
	load(file.choose( )) # DateTimeMatrix.rda

# Make 'Date' column into type=Character
  Discharge [ , 1] <- as.character(Discharge[ , 1])

# Make character vector of dates for use in timeDischargeC
  DischargeDate <- Discharge[,1]

# Make numeric vector of total Discharge in watershed for use in timeDischargeC
  DischargeCFS <- Discharge[,2]

# Make a matrix that holds the sum of all Dischargeitation in the watershed as
# estimated across 4-km square blocks. This represents the total Discharge in the
# watershed when a fish was at each location in the estuary.
  cppFunction('
    NumericMatrix timeDischargeC(CharacterMatrix DateOnly3,
      CharacterVector DischargeDate, NumericVector DischargeCFS){
        int n = DateOnly3.nrow();
        int m = DateOnly3.ncol();
        int q = DischargeCFS.size();
        NumericMatrix timeDischarge(DateOnly3.nrow(), DateOnly3.ncol());
        float length = q;
        std::cout << std::endl;

        for(int j=0; j < q; ++j){
          for(int i=0; i < n; ++i) {
            for(int t=0; t < m; ++t) {
              if(DateOnly3(i,t)==DischargeDate[j]) {
                timeDischarge(i,t)=DischargeCFS[j];
              } else {
                if(DateOnly3(i,t)=="1970-01-01"){
                  timeDischarge(i,t)=-999;
                } else {
                  continue;
                }
              }
            }
          }
          std::cout<<"\\r"
          <<100*(float)(j+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return timeDischarge;
      }
    ')
    # Run the function to get total Discharge in the watershed for each fish when
    # they were seen at each location.
    # NOTE: will contain NA values (-999) that need to be replaced with means
      unix.time(time.Discharge <- timeDischargeC(DateOnly3, DischargeDate,
      	DischargeCFS))

    # Spot check the data to make sure it worked correctly
      time.Discharge[601:610,1:5]

# Make time-varying covariate for Dischargeitation by replacing -999 values with
# mean total Dischargeitation
  # Make new Discharge matrix for data processing
    DischargeMatrix <- time.Discharge

  # Convert to a matrix and take the average for use in Discharge.Covariate
    DischargeVector <- as.vector(DischargeMatrix)

  # Calculate mean of Dischargeitation vector, leaving out NA values (-999)
    mean(DischargeVector[DischargeVector!=-999])

  # Make nested for loop to replace -999 values in DischargeMatrix with mean
    cppFunction('
      NumericMatrix DischargeCovariateC(NumericMatrix DischargeMatrix){
        int n = DischargeMatrix.nrow();
        int m = DischargeMatrix.ncol();
        NumericMatrix DischargeCovariate(DischargeMatrix.nrow(), DischargeMatrix.ncol());
        float length = n;
        std::cout << std::endl;

        for(int i=0; i < n; ++i){
          for(int t=0; t < m; ++t){
            if(DischargeMatrix(i,t)==-999){
              DischargeCovariate(i,t)=22769.8;
            } else {
              DischargeCovariate(i,t)=DischargeMatrix(i,t);
            }
          }
          std::cout<<"\\r"
          <<100*(float)(i+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return DischargeCovariate;
      }
    ')
    # Run the function (no time b/c it's wicked fast) to create the time-varying
    # Dischargeitation covariate for mark-recapture analyses
      Discharge.Covariate <- DischargeCovariateC(DischargeMatrix)

    # Make a dataframe that contains tagIDs and the covariate
      Discharge.Covariate <- data.frame(Date[ , 1], Discharge.Covariate)

    # Assign names to the columns of the dataframe
      names(Discharge.Covariate) <- names(Date)
	    names(Discharge.Covariate)

  # Write the dataframe to a file; name it
    # 'TimeVaryingDischargeitationCovariate.csv'
	    setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
      write.table(Discharge.Covariate,
      	file = 'TimeVaryingDischargeCovariate.csv', sep=",", row.names=FALSE)
	  # Write it to an r-data file
	    save(Discharge.Covariate, file = 'TimeVaryingDischargeCovariate.rda')
################################################################################

################################################################################
# Make a photoperiod covariate
#
# INPUT FILENAME(S): 'DateOnlyMatrix.rda' (or .csv)-- DateOnly3 df
# MY FUNCTIONS:
# NAMED OUTPUT:      photo.Cov (vector); photo.Covariate (matrix)
# OUTPUT FILENAME:   'TimeVaryingPhotoperiodCovariate.rda' (or .csv)
################################################################################
# Install and load necessary packages
	#install.packages("geosphere") # Uncomment to run
	require(geosphere)

# Calculate photoperiod for each day in the DateOnlyMatrix using geosphere
 	photo.Cov <- daylength(44.821614, yday(DateOnly3))

# Turn the resulting vector into a matrix
  photo.Covariate <- matrix(photo.Cov, nrow=nrow(DateOnly3),
   	ncol=ncol(DateOnly3))

# Assign column names to the matrix that match the other time-varying covs
  colnames(photo.Covariate) <- names(
  	precip.Covariate)[2:length(names(precip.Covariate))]

# Replace the photoperiod for 1970-01-01 with mean of study period
  # Create a text-based progress meter
	  pb <- txtProgressBar(min = 0, max = nrow(photo.Covariate), style = 3)
	# Replace photoperiod values and track progress
  	for(i in 1:nrow(photo.Covariate)){
  		Sys.sleep(0.1)
      for(t in 1:ncol(photo.Covariate)){
    		if(photo.Covariate[i, t]==min(photo.Covariate)){
    			photo.Covariate[i, t] <- mean(photo.Cov[photo.Cov!=min(photo.Cov)])
    		} else {
    			next;
    		}
    	} # t
      setTxtProgressBar(pb, i) # Update progress meter
    } # i

# Write the dataframe to a file; name it
  # 'TimeVaryingPhotoperiodCovariate.csv'
    #write.table(photo.Covariate, file = tclvalue(tcl("tk_getSaveFile")),
    	#sep=",", row.names=FALSE)
	# Write it to an r-data file
	  #save(photo.Covariate, file=tclvalue(tcl("tk_getSaveFile")))

	# OR...automate the file-writing process
	  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
 	  write.table(photo.Covariate, file = "TimeVaryingPhotoperiodCovariate.csv",
 	  	sep=",", row.names=FALSE)
 	  save(photo.Covariate,file = "TimeVaryingPhotoperiodCovariate.rda")
################################################################################

################################################################################
# Make whole-river (GodModel) capture histories for all of the fish
#
# INPUT FILENAME(S):  'Final 2005-2013 Database.rda' ('AllData'),
# MY FUNCTIONS:
# NAMED OUPUT:        'ch'
# OUTPUT FILENAME:    'GodFull.rda' (or .csv)
################################################################################
# Install packages
  #install.packages("tcltk2")
  #install.packages("reshape")
  #install.packages("reshape2")
  #install.packages("lubridate")
  require(reshape)
  require(tcltk2)
  require(lubridate)
  require(reshape2)

# Load 'Final 2005-2013 Database.rda' (object name = 'AllData') into workspace
  load(file.choose())
  # Or, read it in as a csv instead (no difference, just takes longer!)
    #AllData <- read.csv(file.choose())# Uncomment to read this way instead

# Make the capture histories out of the comprehensive location histories for
# each fish.
  # Use the cast function to make the capture-history matrix for processing
  # Takes about 5 minutes to complete
    #unix.time( # Uncomment to time the cast fxn
    ch <- cast(AllData,
    	TagID + ForkLength.mm + Mass.g + ATPaseActivity + ReleaseSite +
    	ReleaseDate + Origin ~ RKM + LocationCode)
    #) # Uncomment to time the cast fxn

  # Reverse the order of the Location columns to sort by decreasing rkms
    ch<-ch[ , c(1:7, rev(8:ncol(ch)))]

  # Use a neseted for loop to make a 1/0 matrix of captures at each site
  # Takes about 20 seconds to run
    #unix.time( # uncomment to time
    for(i in 1:nrow(ch)){
      for (t in 8:ncol(ch)){
        if(ch[i, t] > 0){
          ch[i, t] <- 1
        } else {
          ch[i, t] <- 0
        }
      } # t
    } # i
    #) # uncomment to time

  # Spot-check the capture history to make sure it worked ok
    # First ten rows and columns
      ch[1:10, 1:10]
    # First ten rows and last ten columns
      ch[1:10, (ncol(ch)-10):ncol(ch)]

  # Add 'Year' to the capture history for each fish as a grouping variable that
  # can be used to help model year/release-group random effects later on
    ch$Year <- substr(as.character(ch$ReleaseDate), 1, 4)

  # Rearrange the df so that 'Year' is in the front with other covariates
    ch <- ch[ , c(1:7, ncol(ch), 8:(ncol(ch)-1))]

  # Now combine all detections after FortPoint to create a 'Final' detection
  # array at the end of the river
    for(i in 1:nrow(ch)){
	    if(sum(ch[i , (ncol(ch)-5):ncol(ch)]) > 0){
        ch[i, (ncol(ch)-5)] <- 1
      } else {
      	ch[i, (ncol(ch)-5)] <- 0
      }
    } # i
	# Take out the unecessary columns
    ch <- ch[ , 1:(ncol(ch)-5)]
	# Rename the final interval to be 'Bay'
	  colnames(ch)[ncol(ch)] <- "-6_BAY"

# Write to a datafile, either as .csv format or as .rda
	# Uncomment to automate file writing to a specific directory
	# Set working directory
 	  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
 	  write.table(ch, file = "GodFull.csv", sep=",", row.names=FALSE)
 	  save(ch, file = "GodFull.rda")

# GodFull.csv
  #write.table(ch, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
  	#row.names=FALSE)
# Save as r-data file ('GodFull.rda')
  #save(ch,file = tclvalue(tcl("tk_getSaveFile")))
################################################################################
} # Code-folding marker
# END PART 3. DATA MANIPULATION FOR ESTUARY CAPTURE HISTORIES-------------------


# PART 5. ESTUARY BEHAVIOR ANALYSIS---------------------------------------------
{ # Code-folding marker
################################################################################
# Create matrices for estuary behavioral analysis
#
# INPUT FILENAME(S):  'GodFull.rda' (or ch df), 'DateTimeMatrix.rda' (Date df)
# MY FUNCTIONS:       'DTAgain', 'positiveMin'
# NAMED OUPUT:        'mvmt' (df), 'timing' (df)
# OUTPUT FILENAME(S): 'SmoltEstuaryMovements.rda' (and .csv)
#	                    'SmoltEstuaryTiming.rda' (and .csv)
################################################################################
# Install and load necessary packages
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  #install.packages("geosphere") # Uncomment to run
  #install.packages("plyr") # Uncomment to run
  require(plyr)
  require(geosphere)
  require(lubridate)
  require(tcltk2)

# Set system time to UTC for ease of handling time/number conversions later on
  Sys.setenv(TZ='UTC')

# Import DateTimeMatrix
  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
  load('DateTimeMatrix.rda')
  Date$TagID <- as.character(Date$TagID)

# Make fxn for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

# Change all of the date/time into numeric data and create a new dataframe
  # Use apply to convert the entire dataframe at once
    DetNum <- mapply(as, Date[,2:ncol(Date)], c("numeric"))
  # Make a new dataframe that also has tag ID
    num.date <- data.frame(Date[,1], DetNum)
    #num.date <- mapply(as, num.date[ , 2:ncol(num.date)], c("numeric"))

  # Now select only the estuary data for movement analysis
    num.date <- num.date[ , c(1, 48:ncol(num.date))]
  # Give the TagID column a name
    names(num.date) <- c("TagID", colnames(DetNum)[47:length(colnames(DetNum))])
  # Sort the matrix by TagID
    num.date <- num.date[with(num.date, order(TagID)),]
    num.date[, 2:ncol(num.date)] <- mapply(as, num.date[,2:ncol(num.date)],
      c("numeric"))

# Get some covariates for the analysis
  # Load the God captures, 'GodFull.rda', object name = 'ch'
    load('GodFull.rda')
    # Change data type for TagID
      ch$TagID <- as.character(ch$TagID)
    # Sort the dataframe by TagID
      ch <- ch[with(ch, order(TagID)),]

  # Create variable for Stillwater or Mainstem, unknown gets mean 0.5
  # Numbers in this loop don't need to change unless ch changes in the future
  	for(i in 1:nrow(ch)){
		  if(sum(ch[i,c(45,48,49,52)])>0){
			  ch$Stillwater[i] <- 1
		  } else {
		  	if(sum(ch[i,c(46,47)])>0){
			  	ch$Stillwater[i] <- 0
			  } else {
			  	if(sum(ch[i,c(45:49, 52)])==0){
			    	ch$Stillwater[i] <- 0.5
			    } else {
				    next;
			    }
		    }
	    }
	  } # i

  # Create variable to hold release rkm
	# Get RKM for each of the release sites so it can be added to 'Releases'
    # Get the unique values of release site
      ReleaseSites <- c(sort(unique(as.character(ch$ReleaseSite))))

    # Make a vector of corresponding rkms for release sites
      ReleaseRKMs <- c(187, 43.5, 162, 99, 144, 142,  92.3, 9.5, 149)

	# Convert the vector data type for ReleaseSite in ch to chr from factor
	  ch$ReleaseSite <- as.character(ch$ReleaseSite)

  # Create a column in the capture history to hold release RKM and fill it in
	  ch$ReleaseRKM <- 0
	  for(i in 1:length(ReleaseSites)){
      for(t in 1:nrow(ch)){
	      if(ch$ReleaseSite[t]==ReleaseSites[i]){
	      	ch$ReleaseRKM[t] <- ReleaseRKMs[i]
	      } else {
	      	next;
	      } # else
	    } # t
    } # i

  # Create a variable for number of dams passed: first make a progress meter and
  # then run the if-else statement from hell.
	pb <- txtProgressBar(min = 0, max = nrow(ch), style = 3)
	for(i in 1:nrow(ch)){
    Sys.sleep(0.1)
		if(ch$ReleaseSite[i]=="Brewer" | ch$ReleaseSite[i]=="Verona"){
			ch$DamsPassed[i] <- 0
		} else {
			if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]!=2013 & ch$Stillwater[i]==1){
				ch$DamsPassed[i] <- 9
			} else {
				if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]!=2013 & ch$Stillwater[i]==0){
					ch$DamsPassed[i] <- 8
				} else {
					if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]!=2013 & ch$Stillwater[i]==0.5){
						ch$DamsPassed[i] <- 8.5
				    } else {
					    if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]==2013 & ch$Stillwater[i]==1){
					  	  ch$DamsPassed[i] <- 9
					    } else {
					  	    if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]==2013 & ch$Stillwater[i]==0){
					  	      ch$DamsPassed[i] <- 7
					        } else {
						        if(ch$ReleaseSite[i]=="Abbott" & ch$Year[i]==2013 & ch$Stillwater[i]==0.5){
						          ch$DamsPassed[i] <- 8
					            } else {
					      	        if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]!=2013 & ch$Stillwater[i]==1){
					      		      ch$DamsPassed[i] <- 6
					      	        } else {
					      		        if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]!=2013 & ch$Stillwater[i]==0){
					      		          ch$DamsPassed[i] <- 5
					                    } else {
					      		            if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]!=2013 & ch$Stillwater[i]==0.5){
					      		              ch$DamsPassed[i] <- 5.5
					      	                } else {
					                            if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]==2013 & ch$Stillwater[i]==1){
					      		                  ch$DamsPassed[i] <- 6
					      	                    } else {
					      		                    if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]==2013 & ch$Stillwater[i]==0){
					      		                      ch$DamsPassed[i] <- 4
					      		                    } else {
					      			                    if(ch$ReleaseSite[i]=="EastBranch" & ch$Year[i]==2013 & ch$Stillwater[i]==0.5){
					      				                  ch$DamsPassed[i] <- 5
					      			                    } else {
					      			                        if((ch$ReleaseSite[i]=="Weldon" | ch$ReleaseSite[i]=="BikeClub") & ch$Stillwater[i]==1){
					      			                          ch$DamsPassed[i] <- 5
					      			                        } else{
					      			                            if((ch$ReleaseSite[i]=="Weldon" | ch$ReleaseSite[i]=="BikeClub") & ch$Stillwater[i]==0){
					      			      	                      ch$DamsPassed[i] <- 4
					      			                            } else {
					      			      	                        if((ch$ReleaseSite[i]=="Weldon" | ch$ReleaseSite[i]=="BikeClub") & ch$Stillwater[i]==0.5){
					      			      		                      ch$DamsPassed[i] <- 4.5
					      			      	                        } else {
					      			      	   	                        if(ch$ReleaseSite[i]=="Milo" & ch$Stillwater[i]==1){
					      			      			                      ch$DamsPassed[i] <- 5
					      			      	                            } else {
					      			      			                        if(ch$ReleaseSite[i]=="Milo" & ch$Stillwater[i]==0){
					      			      				                      ch$DamsPassed[i] <- 4
					      			      			                        } else {
					      			      				                        if(ch$ReleaseSite[i]=="Milo" & ch$Stillwater[i]==0.5){
					      			      					                      ch$DamsPassed[i] <- 4.5
					      			      				                        } else {
					      			      					                        if((ch$ReleaseSite[i]=="Passadumkeag" | ch$ReleaseSite[i]=="Howland" )& ch$Stillwater[i]==1){
					      			      						                      ch$DamsPassed[i] <- 4
					      			      					                        } else {
					      			      						                        if((ch$ReleaseSite[i]=="Passadumkeag" | ch$ReleaseSite[i]=="Howland") & ch$Stillwater[i]==0){
					      			      						                          ch$DamsPassed[i] <- 3
					      			      						                        } else {
					      			      							                        if((ch$ReleaseSite[i]=="Passadumkeag" | ch$ReleaseSite[i]=="Howland") & ch$Stillwater[i]==0.5){
					      			      								                      ch$DamsPassed[i] <- 3.5
					      			      							                        } else {
					      			      								                        next;
					      			      							                        }
					      			      						                        }
					      			      					                        }
					      			      				                        }
					      			      			                        }
					      			      		                        }
					      			      	                        }
					      			                            }
					      			                        }
					      			                    }
					      			                }
					      		                }
					      	                }
					                    }
					                }
					            }
					        }
				        }
			        }
		        }
	        }
	    }
	  setTxtProgressBar(pb, i)
	} # i (folding line)

# Compile the estuary smolt Movement data
  mvmt <- data.frame(ch[ , c(1:8, 95:97)], num.date[ , c(2:ncol(num.date))])

  # Rename the columns to make sure they match the survival caps
    names(mvmt) <- c(names(ch)[c(1:8, 95:97)],
      names(num.date)[(2:ncol(num.date))])

  # Keep only the receiver locations that were used in the estuary survival
  # analysis
    mvmt <- mvmt[ , c(1:11,14,18,22,25,27,30,39,45,48:50)]
    mvmt <- mvmt[mvmt[, 5]!= "Brewer" & mvmt[, 5]!= "Verona", ]

  # Remove any fish that were never located in the estuary
    mvmt <- mvmt[apply(mvmt[ ,12:22], 1, sum)!=0, ]
    nrow(mvmt)

  # Write the movement data to a file for later analysis
    write.table(mvmt, "SmoltEstuaryMovements.csv", row.names=FALSE, sep=',')
    save(mvmt, file="SmoltEstuaryMovements.rda")

# Compile the estuary timing data
  # Create a function to get minimum of numeric date greater than zero
    positiveMin <- function(x){
      min(x[x > 0])
    }

  # Get minimum of date and time for each fish located on the array
    minStamp <- apply(mvmt[ , 12:ncol(mvmt)], 1, positiveMin)
    # Convert it to date and time to make sure it worked
      DTAgain(minStamp)[1:25]

	# Get median of date and time for each fish located on the array
	  # Load the 2005-2013 database
	    load('C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/2005-present Penobscot ATS/Final 2005-2013 database.rda')

	  # Keep only the estuary data
	    EstData <- AllData[AllData$RKM < 45,]

	  # Correct data format and just keep tags and timestamps
  	  Times <- data.frame(EstData$TagID, EstData$DateTime)
      names(Times) <- c("TagID", "DateTime")
      Times$TagID <- as.character(Times$TagID)
      Times$DateTime <- as.numeric(Times$DateTime, origin=1970-01-01)

	  # Calculate median time of estuary occupancy for each fish
  	  MedTime <- ddply(EstData, "TagID", summarize, outVal = median(DateTime))
	    names(MedTime) <- c("TagID", "MedDate")
	    MedTime$TagID <- as.character(MedTime$TagID)
	    MedTime <- MedTime[MedTime$TagID %in% timing$TagID==TRUE,]
	    MedTime <- MedTime[with(MedTime, order(TagID)),]

  # Convert min date and time to ordinal date for analysis
    ArrivalMin <- DTAgain(minStamp)
    OrdinalMin <- yday(DTAgain(minStamp))

  # Make a data frame of the estuary timing data
    timing <- data.frame(ArrivalMin, OrdinalMin, mvmt[,1:11])
    timing$MedDate <- MedTime$MedDate
	  timing$OrdinalMedDate <- yday(MedTime$MedDate)

  # Add a column for condition factor (k)
    timing$k <- (timing$Mass.g/(timing$ForkLength.mm^3))*100000

  # Write the timing data to a file for later analysis
    write.table(timing, "SmoltEstuaryTiming.csv", row.names=FALSE, sep=',')
    save(timing, file="SmoltEstuaryTiming.rda")
################################################################################

################################################################################
# Conduct estuary timing analysis (Thallo model) and coefficient plots
#
# INPUT FILENAME(S):  'SmoltEstuaryTiming.rda' (or .csv)
# MY FUNCTIONS:       'approx.chat', 'standardize', 'aic.table'
# NAMED OUPUT:        'suns', 'Cand.models' (list), 'ranks'
# OUTPUT FILENAME(S): 'SunriseSunsetTimes2005-2015.csv',
#                     'TimingModelSelection.csv'
################################################################################
# Install and load the necessary packages
  #install.packages("AICcmodavg", deps=TRUE) # Uncomment to install
  #install.packages("StreamMetabolism") # Uncomment to install
	#install.packages("plyr") # Uncomment to install
	#install.packages("geosphere") # Uncomment to install
  require(AICcmodavg)
  require(StreamMetabolism)
	require(plyr)
	require(geosphere)

# Define functions
  # 'aic.table'
  # Function to create AIC Model selection table
    # Write a fxn to do it
      aic.table <- function(x, y){
        # Create a column to hold number of parameters
          k <- c()
        # Get number of parameters
          for (i in 1:length(x)){
            k[i] <- (length(x[[i]]$coefficients))
            #k[i]<-nrow(data.frame((summary(Cand.models[[i]])[10]))) + 1
          }
        # Make blank columns to hold statistics
          AICc <- c(rep(0, length(k)))
          DeltaAIC <- c(rep(0, length(k)))
          AICWeight <- c(rep(0, length(k)))
          Deviance <- c(rep(0, length(k)))
          Relative.likelihood<-c(rep(0, length(k)))
        # Make dataframe to hold results
          ranks<-data.frame(y, k, AICc, DeltaAIC, AICWeight)
          names(ranks)[1] <- "Model"
        # Get AIC for each model
          for(i in 1:nrow(ranks)){
            ranks$AICc[i] <- AICc(x[[i]])
          }
        # Sort the table by AICc
          ranks <- ranks[with(ranks, order(AICc)), ]
        # Calculate delta AIC
          for(i in 1:nrow(ranks)){
            ranks$DeltaAIC[i] <- ranks$AICc[i]-ranks$AICc[1]
          }
        # Calculate relative likelihoods
          for(i in 1:nrow(ranks)){
            Relative.likelihood[i] <- exp(-.5*ranks$DeltaAIC[i])
          }
        # Calculate AIC weights
          for(i in 1:length(Relative.likelihood)){
            ranks$AICWeight[i] <- Relative.likelihood[i]/
             sum(Relative.likelihood[1:length(Relative.likelihood)])
          }
        # Round off the calculated columns
          ranks[ , 3:5] <- round(ranks[ , 3:5], 2)
        # Print the model selection results
          return(ranks)
    }

  # 'Standardize'
  # Create fxn to standardize covariates
  	standardize=function(x,z=NULL){
      if(is.null(z)){
        return((x-mean(x))/sqrt(var(x)))
      } else {
          return((x-mean(z))/sqrt(var(z)))
        }
    }

  # 'approx.chat'
  # Create a fxn to calculate c-hat from Ben Bolker
    approx.chat <- function ( object ){
      with(object, sum((weights*residuals^2)[weights>0])/df.residual)
    }

# Import the timing matrix, it's a dataframe called 'timing'
  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
  load('SmoltEstuaryTiming.rda')

# Change the column names to be shorter
  names(timing) <- c("ArrivalMin","ArrivalOrd", "Tag", "FL", "Mass", "ATP",
    "RelSite", "RelDate", "Org", "Year", "SW", "RelKM", "Dams", "MedDate",
    "MedDateOrd","k")

# Change the release date variable to ordinal
    timing$RelDate <- substr(timing$RelDate, start=1, stop=10)

# Calculate cumulative degree day covariate for each fish's arrival time in est
# as well as total precipitation in watershed from release to arrival
  # Load the temp and precip data for the Penobscot Watershed- to be used in
  # creating cumulative degree day covariate and precip covariate
    setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
    load('PrismTempDataPNR.rda')

  # Read in Flow data ('WestEnfieldFlowDataClean.txt'), replace NA with 0 b/c ice
    Discharge <- read.table('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/WestEnfieldFlowDataClean.txt',
    	header=TRUE)[ , 3:4]
	  names(Discharge) <- c('Date', 'Discharge')
	  Discharge[ , 2] <- as.character(Discharge[ , 2])
	  for(i in 1:nrow(Discharge)){
		  if(is.na(Discharge[i , 2])){
	      Discharge[i , 2] <- 0
		  } else {
			  next;
		  }
	  }
	  Discharge[ , 2] <- as.numeric(Discharge[ , 2])

  # Get temperature for each day
    # Strip date from timestamp that was included in the file name of each
    # raster file that estimated mean temp in 4-km blocks
      TempC <- Temp.data[ , 1:2] # Get date and mean temp in watershed

    # Strip year out of string
      TempC[ ,3] <- substr(TempC[ , 1],nchar(TempC[ , 1])-nchar(TempC[ , 1]),
      	nchar(TempC[ , 1])-nchar(TempC[ , 1])+4)

    # Strip month out of string
      TempC[ ,4] <- substr(TempC[ ,1], nchar(TempC[ ,1])-nchar(TempC[ , 1])+5,
        nchar(TempC[ , 1])-nchar(TempC[ ,1])+6)

    # Strip day out of string
      TempC[ ,5] <- substr(TempC[ ,1], nchar(TempC[ ,1])-nchar(TempC[ , 1])+7,
      	nchar(TempC[ ,1]))

    # Paste year, month, and day together; separate with '-'
      TempC[ ,6] <- as.character( paste(TempC$V3, TempC$V4, TempC$V5, sep="-") )

    # Make a dataframe containing new date column and mean Temp.data in
    # watershed for all days 01/01/05-08/31/13
      TempC <- data.frame(TempC$V6, TempC$Temp.mean)

      # Assign names to the columns of the dataframes
        names(TempC) <- c("Date", "MeanTempC")

      # Make the 'Dates' column into variable type=Character
        TempC[ , 1] <- as.character(TempC[ , 1])

      # Make a vector to hold the year for each temperature measurement
        TempC$Year <- year(as.POSIXct(TempC[,1]))

  # Get a column with just dates for timing df
    timing$Day <- substr(timing$MedDate, start=1, stop=10)

  # Calculate cumulative degree days for each fish prior to release
	  # Read in GLNFH temperature data
	    GLNFH <- read.csv('GLNFHTemps.csv', header=TRUE)
	    # Create a year variable
	      GLNFH$Year <- substr(GLNFH$Date, start=1, stop=4)
	    # Rename the temperature column
	      names(GLNFH)[2]<-'Temp'

	  # Read in river temperature data
	    river.t <- read.csv('RiverTemps_2005-2011.csv', header=TRUE)
	    # Get daily averages for river temps
	      rtp <- tapply(river.t$Temp, river.t$Date, mean)
	    # Get date
	      rds <- sort(unique(river.t$Date))
	    # Get year
	      ry <- year(rds)
	    # Make the daily means into a df
	      rivers <- data.frame(ry, rds, rtp)
	      names(rivers) <- c('Year', 'Date', 'Temp')

	  # Change year and date vars into char
	    GLNFH[ , c(1, 3)] <- apply(GLNFH[ , c(1, 3)], 2, FUN=as.character)
	    rivers[ , c(1, 2)] <- apply(rivers[ , c(1, 2)], 2, FUN=as.character)

  # Do the calculation for ATU
    pb <- txtProgressBar(min = 0, max = nrow(timing), style = 3)
    for(i in 1:nrow(timing)){
      Sys.sleep(0.1)
    	if(timing$Org[i]=='Hatchery'){
        timing$ATU[i] <- sum(GLNFH$Temp[GLNFH$Year==timing$Year[i] &
          (GLNFH$Date < timing$RelDate[i] | GLNFH$Date==timing$RelDate[i])])
    	} else {
    	  if(timing$Org[i]=='Wild'){
          timing$ATU[i] <- sum(rivers$Temp[rivers$Year==timing$Year[i] &
            (rivers$Date < timing$RelDate[i] | rivers$Date==timing$RelDate[i])])
    	  } else {
    		  next;
    	  }
    	}
      setTxtProgressBar(pb, i)
    } # i
    timing$ATU2 <- timing$ATU^2

  # Get discharge for each day
    # Change datatype for date
      Discharge$Date <- as.character(Discharge$Date)

    # Make a vector to hold the year for each temperature measurement
      Discharge$Year <- year(as.POSIXct(Discharge[,1]))

  # Calculate mean discharge for each fish
    pb <- txtProgressBar(min = 0, max = nrow(timing), style = 3)
    for(i in 1:nrow(timing)){
      Sys.sleep(0.1)
      timing$Dis[i] <- mean(Discharge$Discharge[Discharge$Year==timing$Year[i] &
        (Discharge$Date < timing$Day[i] | Discharge$Date==timing$Day[i]) &
        (Discharge$Date > timing$RelDate[i] | Discharge$Date==timing$RelDate[i])])
      setTxtProgressBar(pb, i)
    } # i

# Change variable type for release date
  timing$RelDate <- yday(timing$RelDate)

# Create a variable for BINARY ORIGIN
	for(i in 1:nrow(timing)){
		if(timing$Org[i] == "Wild"){
			timing$BinaryOrigin[i] <- 1
		} else {
			timing$BinaryOrigin[i] <- 0
		}
	}	# i
  #timing$Org <- timing$BinaryOrigin
	#timing$Org <- standardize(timing$Org)

# Calculate a photoperiod variable
	timing$photo <- daylength(44.821614, timing$RelDate)

# Create a quadratic for ATPase
	timing$ATP2<-timing$ATP^2

# Create df that keeps vars on original scales for model predictions
	time <- timing

# Standardize individual covariates
  timing[ , c(4:6, 11:13, 16, 18:23)] <-
    apply(timing[ , c(4:6, 11:13, 16, 18:23)], 2, standardize)

# Remove ridiculous arrival times (July!)
  timing$ArrivalOrd[timing$ArrivalOrd > 160] <- round(mean(timing$ArrivalOrd[timing$ArrivalOrd < 160]))
  timing$MedDateOrd[timing$MedDateOrd > 160] <- round(mean(timing$MedDateOrd[timing$MedDateOrd < 160]))

# Conduct analysis of timing
  # Build candidate model list
    Cand.models=list()
      Cand.models[[1]]=glm(ArrivalOrd ~ photo + RelKM + SW, family=poisson, data=timing)
      Cand.models[[2]]=glm(ArrivalOrd ~ photo + RelKM + FL, family=poisson, data=timing)
      Cand.models[[3]]=glm(ArrivalOrd ~ photo + RelKM + SW + FL, family=poisson, data=timing)
      Cand.models[[4]]=glm(ArrivalOrd ~ photo + RelKM + k, family=poisson, data=timing)
      Cand.models[[5]]=glm(ArrivalOrd ~ photo + RelKM + SW + k, family=poisson, data=timing)
      Cand.models[[6]]=glm(ArrivalOrd ~ photo + RelKM + ATP + ATP2, family=poisson, data=timing)
      Cand.models[[7]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATP + ATP2, family=poisson, data=timing)
      Cand.models[[8]]=glm(ArrivalOrd ~ photo + RelKM + Org, family=poisson, data=timing)
      Cand.models[[9]]=glm(ArrivalOrd ~ photo + RelKM + SW + Org, family=poisson, data=timing)
      Cand.models[[10]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATU, family=poisson, data=timing)
      Cand.models[[11]]=glm(ArrivalOrd ~ photo + RelKM + FL + ATU, family=poisson, data=timing)
      Cand.models[[12]]=glm(ArrivalOrd ~ photo + RelKM + SW + FL + ATU, family=poisson, data=timing)
      Cand.models[[13]]=glm(ArrivalOrd ~ photo + RelKM + k + ATU, family=poisson, data=timing)
      Cand.models[[14]]=glm(ArrivalOrd ~ photo + RelKM + SW + k + ATU, family=poisson, data=timing)
      Cand.models[[15]]=glm(ArrivalOrd ~ photo + RelKM + ATP + ATP2 + ATU, family=poisson, data=timing)
      Cand.models[[16]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATP + ATP2 + ATU, family=poisson, data=timing)
      Cand.models[[17]]=glm(ArrivalOrd ~ photo + RelKM + Org + ATU, family=poisson, data=timing)
      Cand.models[[18]]=glm(ArrivalOrd ~ photo + RelKM + SW + Org + ATU, family=poisson, data=timing)
      Cand.models[[19]]=glm(ArrivalOrd ~ photo + RelKM + SW + Dis, family=poisson, data=timing)
      Cand.models[[20]]=glm(ArrivalOrd ~ photo + RelKM + FL + Dis, family=poisson, data=timing)
      Cand.models[[21]]=glm(ArrivalOrd ~ photo + RelKM + SW + FL + Dis, family=poisson, data=timing)
      Cand.models[[22]]=glm(ArrivalOrd ~ photo + RelKM + k + Dis, family=poisson, data=timing)
      Cand.models[[23]]=glm(ArrivalOrd ~ photo + RelKM + SW + k + Dis, family=poisson, data=timing)
      Cand.models[[24]]=glm(ArrivalOrd ~ photo + RelKM + ATP + ATP2 + Dis, family=poisson, data=timing)
      Cand.models[[25]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATP + ATP2 + Dis, family=poisson, data=timing)
      Cand.models[[26]]=glm(ArrivalOrd ~ photo + RelKM + Org + Dis, family=poisson, data=timing)
      Cand.models[[27]]=glm(ArrivalOrd ~ photo + RelKM + SW + Org + Dis, family=poisson, data=timing)
      Cand.models[[28]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATU + Dis, family=poisson, data=timing)
      Cand.models[[29]]=glm(ArrivalOrd ~ photo + RelKM + FL + ATU + Dis, family=poisson, data=timing)
      Cand.models[[30]]=glm(ArrivalOrd ~ photo + RelKM + SW + FL + ATU + Dis, family=poisson, data=timing)
      Cand.models[[31]]=glm(ArrivalOrd ~ photo + RelKM + k + ATU + Dis, family=poisson, data=timing)
      Cand.models[[32]]=glm(ArrivalOrd ~ photo + RelKM + SW + k + ATU + Dis, family=poisson, data=timing)
      Cand.models[[33]]=glm(ArrivalOrd ~ photo + RelKM + ATP + ATP2 + ATU + Dis, family=poisson, data=timing)
      Cand.models[[34]]=glm(ArrivalOrd ~ photo + RelKM + SW + ATP + ATP2 + ATU + Dis, family=poisson, data=timing)
      Cand.models[[35]]=glm(ArrivalOrd ~ photo + RelKM + Org + ATU + Dis, family=poisson, data=timing)
      Cand.models[[36]]=glm(ArrivalOrd ~ photo + RelKM + SW + Org + ATU + Dis, family=poisson, data=timing)
      Cand.models[[37]]=glm(ArrivalOrd ~ photo + Dams + FL, family=poisson, data=timing)
      Cand.models[[38]]=glm(ArrivalOrd ~ photo + Dams + k, family=poisson, data=timing)
      Cand.models[[39]]=glm(ArrivalOrd ~ photo + Dams + ATP + ATP2, family=poisson, data=timing)
      Cand.models[[40]]=glm(ArrivalOrd ~ photo + Dams + Org, family=poisson, data=timing)
      Cand.models[[41]]=glm(ArrivalOrd ~ photo + Dams + FL + ATU, family=poisson, data=timing)
      Cand.models[[42]]=glm(ArrivalOrd ~ photo + Dams + k + ATU, family=poisson, data=timing)
      Cand.models[[43]]=glm(ArrivalOrd ~ photo + Dams + ATP + ATP2 + ATU, family=poisson, data=timing)
      Cand.models[[44]]=glm(ArrivalOrd ~ photo + Dams + Org + ATU, family=poisson, data=timing)
      Cand.models[[45]]=glm(ArrivalOrd ~ photo + Dams + FL + Dis, family=poisson, data=timing)
      Cand.models[[46]]=glm(ArrivalOrd ~ photo + Dams + k + Dis, family=poisson, data=timing)
      Cand.models[[47]]=glm(ArrivalOrd ~ photo + Dams + ATP + ATP2 + Dis, family=poisson, data=timing)
      Cand.models[[48]]=glm(ArrivalOrd ~ photo + Dams + Org + Dis, family=poisson, data=timing)
      Cand.models[[49]]=glm(ArrivalOrd ~ photo + Dams + FL + ATU + Dis, family=poisson, data=timing)
      Cand.models[[50]]=glm(ArrivalOrd ~ photo + Dams + k + ATU + Dis, family=poisson, data=timing)
      Cand.models[[51]]=glm(ArrivalOrd ~ photo + Dams + ATP + ATP2 + ATU + Dis, family=poisson, data=timing)
      Cand.models[[52]]=glm(ArrivalOrd ~ photo + Dams + Org + ATU + Dis, family=poisson, data=timing)
      Cand.models[[53]]=glm(ArrivalOrd ~ photo + SW, family=poisson, data=timing)
      Cand.models[[54]]=glm(ArrivalOrd ~ photo + FL, family=poisson, data=timing)
      Cand.models[[55]]=glm(ArrivalOrd ~ photo + SW + FL, family=poisson, data=timing)
      Cand.models[[56]]=glm(ArrivalOrd ~ photo + k, family=poisson, data=timing)
      Cand.models[[57]]=glm(ArrivalOrd ~ photo + SW + k, family=poisson, data=timing)
      Cand.models[[58]]=glm(ArrivalOrd ~ photo + ATP + ATP2, family=poisson, data=timing)
      Cand.models[[59]]=glm(ArrivalOrd ~ photo + SW + ATP + ATP2, family=poisson, data=timing)
      Cand.models[[60]]=glm(ArrivalOrd ~ photo + Org, family=poisson, data=timing)
      Cand.models[[61]]=glm(ArrivalOrd ~ photo + SW + Org, family=poisson, data=timing)
      Cand.models[[62]]=glm(ArrivalOrd ~ photo + SW + ATU, family=poisson, data=timing)
      Cand.models[[63]]=glm(ArrivalOrd ~ photo + FL + ATU, family=poisson, data=timing)
      Cand.models[[64]]=glm(ArrivalOrd ~ photo + SW + FL + ATU, family=poisson, data=timing)
      Cand.models[[65]]=glm(ArrivalOrd ~ photo + k + ATU, family=poisson, data=timing)
      Cand.models[[66]]=glm(ArrivalOrd ~ photo + SW + k + ATU, family=poisson, data=timing)
      Cand.models[[67]]=glm(ArrivalOrd ~ photo + ATP + ATP2 + ATU, family=poisson, data=timing)
      Cand.models[[68]]=glm(ArrivalOrd ~ photo + SW + ATP + ATP2 + ATU, family=poisson, data=timing)
      Cand.models[[69]]=glm(ArrivalOrd ~ photo + Org + ATU, family=poisson, data=timing)
      Cand.models[[70]]=glm(ArrivalOrd ~ photo + SW + Org + ATU, family=poisson, data=timing)
      Cand.models[[71]]=glm(ArrivalOrd ~ photo + SW + Dis, family=poisson, data=timing)
      Cand.models[[72]]=glm(ArrivalOrd ~ photo + FL + Dis, family=poisson, data=timing)
      Cand.models[[73]]=glm(ArrivalOrd ~ photo + SW + FL + Dis, family=poisson, data=timing)
      Cand.models[[74]]=glm(ArrivalOrd ~ photo + k + Dis, family=poisson, data=timing)
      Cand.models[[75]]=glm(ArrivalOrd ~ photo + SW + k + Dis, family=poisson, data=timing)
      Cand.models[[76]]=glm(ArrivalOrd ~ photo + ATP + ATP2 + Dis, family=poisson, data=timing)
      Cand.models[[77]]=glm(ArrivalOrd ~ photo + SW + ATP + ATP2 + Dis, family=poisson, data=timing)
      Cand.models[[78]]=glm(ArrivalOrd ~ photo + Org + Dis, family=poisson, data=timing)
      Cand.models[[79]]=glm(ArrivalOrd ~ photo + SW + Org + Dis, family=poisson, data=timing)
      Cand.models[[80]]=glm(ArrivalOrd ~ photo + SW + ATU + Dis, family=poisson, data=timing)
      Cand.models[[81]]=glm(ArrivalOrd ~ photo + FL + ATU + Dis, family=poisson, data=timing)
      Cand.models[[82]]=glm(ArrivalOrd ~ photo + SW + FL + ATU + Dis, family=poisson, data=timing)
      Cand.models[[83]]=glm(ArrivalOrd ~ photo + k + ATU + Dis, family=poisson, data=timing)
      Cand.models[[84]]=glm(ArrivalOrd ~ photo + SW + k + ATU + Dis, family=poisson, data=timing)
      Cand.models[[85]]=glm(ArrivalOrd ~ photo + ATP + ATP2 + ATU + Dis, family=poisson, data=timing)
      Cand.models[[86]]=glm(ArrivalOrd ~ photo + SW + ATP + ATP2 + ATU + Dis, family=poisson, data=timing)
      Cand.models[[87]]=glm(ArrivalOrd ~ photo + Org + ATU + Dis, family=poisson, data=timing)
      Cand.models[[88]]=glm(ArrivalOrd ~ photo + SW + Org + ATU + Dis, family=poisson, data=timing)

	# Create vector of model names
    ModNames <- c()
      ModNames[[1]] <- 'photo + RelKM + SW '
      ModNames[[2]] <- 'photo + RelKM + FL '
      ModNames[[3]] <- 'photo + RelKM + SW + FL '
      ModNames[[4]] <- 'photo + RelKM + k '
      ModNames[[5]] <- 'photo + RelKM + SW + k '
      ModNames[[6]] <- 'photo + RelKM + ATP '
      ModNames[[7]] <- 'photo + RelKM + SW + ATP '
      ModNames[[8]] <- 'photo + RelKM + Org '
      ModNames[[9]] <- 'photo + RelKM + SW + Org '
      ModNames[[10]] <- 'photo + RelKM + SW + ATU '
      ModNames[[11]] <- 'photo + RelKM + FL + ATU '
      ModNames[[12]] <- 'photo + RelKM + SW + FL + ATU '
      ModNames[[13]] <- 'photo + RelKM + k + ATU '
      ModNames[[14]] <- 'photo + RelKM + SW + k + ATU '
      ModNames[[15]] <- 'photo + RelKM + ATP + ATU '
      ModNames[[16]] <- 'photo + RelKM + SW + ATP + ATU '
      ModNames[[17]] <- 'photo + RelKM + Org + ATU '
      ModNames[[18]] <- 'photo + RelKM + SW + Org + ATU '
      ModNames[[19]] <- 'photo + RelKM + SW + Dis '
      ModNames[[20]] <- 'photo + RelKM + FL + Dis '
      ModNames[[21]] <- 'photo + RelKM + SW + FL + Dis '
      ModNames[[22]] <- 'photo + RelKM + k + Dis '
      ModNames[[23]] <- 'photo + RelKM + SW + k + Dis '
      ModNames[[24]] <- 'photo + RelKM + ATP + Dis '
      ModNames[[25]] <- 'photo + RelKM + SW + ATP + Dis '
      ModNames[[26]] <- 'photo + RelKM + Org + Dis '
      ModNames[[27]] <- 'photo + RelKM + SW + Org + Dis '
      ModNames[[28]] <- 'photo + RelKM + SW + ATU + Dis '
      ModNames[[29]] <- 'photo + RelKM + FL + ATU + Dis '
      ModNames[[30]] <- 'photo + RelKM + SW + FL + ATU + Dis '
      ModNames[[31]] <- 'photo + RelKM + k + ATU + Dis '
      ModNames[[32]] <- 'photo + RelKM + SW + k + ATU + Dis '
      ModNames[[33]] <- 'photo + RelKM + ATP + ATU + Dis '
      ModNames[[34]] <- 'photo + RelKM + SW + ATP + ATU + Dis '
      ModNames[[35]] <- 'photo + RelKM + Org + ATU + Dis '
      ModNames[[36]] <- 'photo + RelKM + SW + Org + ATU + Dis '
      ModNames[[37]] <- 'photo + Dams + FL '
      ModNames[[38]] <- 'photo + Dams + k '
      ModNames[[39]] <- 'photo + Dams + ATP '
      ModNames[[40]] <- 'photo + Dams + Org '
      ModNames[[41]] <- 'photo + Dams + FL + ATU '
      ModNames[[42]] <- 'photo + Dams + k + ATU '
      ModNames[[43]] <- 'photo + Dams + ATP + ATU '
      ModNames[[44]] <- 'photo + Dams + Org + ATU '
      ModNames[[45]] <- 'photo + Dams + FL + Dis '
      ModNames[[46]] <- 'photo + Dams + k + Dis '
      ModNames[[47]] <- 'photo + Dams + ATP + Dis '
      ModNames[[48]] <- 'photo + Dams + Org + Dis '
      ModNames[[49]] <- 'photo + Dams + FL + ATU + Dis '
      ModNames[[50]] <- 'photo + Dams + k + ATU + Dis '
      ModNames[[51]] <- 'photo + Dams + ATP + ATU + Dis '
      ModNames[[52]] <- 'photo + Dams + Org + ATU + Dis '
      ModNames[[53]] <- 'photo + SW '
      ModNames[[54]] <- 'photo + FL '
      ModNames[[55]] <- 'photo + SW + FL '
      ModNames[[56]] <- 'photo + k '
      ModNames[[57]] <- 'photo + SW + k '
      ModNames[[58]] <- 'photo + ATP '
      ModNames[[59]] <- 'photo + SW + ATP '
      ModNames[[60]] <- 'photo + Org '
      ModNames[[61]] <- 'photo + SW + Org '
      ModNames[[62]] <- 'photo + SW + ATU '
      ModNames[[63]] <- 'photo + FL + ATU '
      ModNames[[64]] <- 'photo + SW + FL + ATU '
      ModNames[[65]] <- 'photo + k + ATU '
      ModNames[[66]] <- 'photo + SW + k + ATU '
      ModNames[[67]] <- 'photo + ATP + ATU '
      ModNames[[68]] <- 'photo + SW + ATP + ATU '
      ModNames[[69]] <- 'photo + Org + ATU '
      ModNames[[70]] <- 'photo + SW + Org + ATU '
      ModNames[[71]] <- 'photo + SW + Dis '
      ModNames[[72]] <- 'photo + FL + Dis '
      ModNames[[73]] <- 'photo + SW + FL + Dis '
      ModNames[[74]] <- 'photo + k + Dis '
      ModNames[[75]] <- 'photo + SW + k + Dis '
      ModNames[[76]] <- 'photo + ATP + Dis '
      ModNames[[77]] <- 'photo + SW + ATP + Dis '
      ModNames[[78]] <- 'photo + Org + Dis '
      ModNames[[79]] <- 'photo + SW + Org + Dis '
      ModNames[[80]] <- 'photo + SW + ATU + Dis '
      ModNames[[81]] <- 'photo + FL + ATU + Dis '
      ModNames[[82]] <- 'photo + SW + FL + ATU + Dis '
      ModNames[[83]] <- 'photo + k + ATU + Dis '
      ModNames[[84]] <- 'photo + SW + k + ATU + Dis '
      ModNames[[85]] <- 'photo + ATP + ATU + Dis '
      ModNames[[86]] <- 'photo + SW + ATP + ATU + Dis '
      ModNames[[87]] <- 'photo + Org + ATU + Dis '
      ModNames[[88]] <- 'photo + SW + Org + ATU + Dis '

# Check for model overdispersion
  # Calculate c-hat for each of the models
    Chat <- c()
    for(i in 1:length(Cand.models)){
      Chat[i] <- approx.chat(Cand.models[[i]])
    }
    Chat # Print the results and do a quick visual check...LOOKS GOOD!!

# Run the aic.table function
  TimingResults <- head(aic.table(Cand.models, ModNames), 10)
  TimingResults

# Look at the results of the best model
  summary(Cand.models[[as.numeric(row.names(TimingResults)[1])]])

# Save the model-selection table to a file
  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Figures & Tables")
  write.table(TimingResults, "TimingModelSelection.csv", row.names=FALSE,
    sep=",")

# Make a dataframe of the coefficient estimates to work with
	timing.res <- summary(Cand.models[[
	  as.numeric(row.names(TimingResults)[1])]])$coefficients
	timing.res # View the results

# Calculate effects of the important covariates
	# Release date
	  # Min of standardized photo
	    MiRD <-  min(timing$photo)
	  # Max of standardized photo
      MaRD <- max(timing$photo)
	  # New values of standardized photo for making predictions
	    nRD <- seq(MiRD, MaRD, 0.01)
	  # Real-scale variable
      RnRD <- nRD*sd(time$photo)+mean(time$photo)
		  # Calculate predicted means
	      RD.preds <- exp(timing.res[1, 1] +
	      	timing.res[2, 1] * nRD +
	  		  timing.res[3, 1] * median(timing$RelKM) +
	  	    timing.res[4, 1] * .5 +
	  	    timing.res[5, 1] * median(timing$Dis))
		  # Calculate predicted upper CL
	      RD.predsL <- exp((timing.res[1, 1] - 1.96*timing.res[1, 2])+
	    	  (timing.res[2, 1] - 1.96 * timing.res[2, 2]) * nRD +
	  		  (timing.res[3, 1] - 1.96 * timing.res[3, 2]) * median(timing$RelKM) +
	  	    (timing.res[4, 1] - 1.96 * timing.res[4, 2]) * .5 +
	  	    (timing.res[5, 1] - 1.96 * timing.res[5, 2]) * median(timing$Dis))
	    # Calculate predicted lower CL
	      RD.predsU <- exp((timing.res[1, 1] + 1.96*timing.res[1, 2])+
	    	  (timing.res[2, 1] + 1.96 * timing.res[2, 2]) * nRD +
	  		  (timing.res[3, 1] + 1.96 * timing.res[3, 2]) * median(timing$RelKM) +
	  	    (timing.res[4, 1] + 1.96 * timing.res[4, 2]) * .5 +
	  	    (timing.res[5, 1] + 1.96 * timing.res[5, 2]) * median(timing$Dis))

	# Release kilometer
	  # Min of standardized RelKM
	    Mirkm<-  min(timing$RelKM)
	  # Max of standardized RelKM
      Markm <- max(timing$RelKM)
	  # New values of standardized and logit-transformed RelKM for making
	  # predictions
	    nrkm<- seq(Mirkm, Markm, 0.01)
		# Real-scale values of RelKM that correspond to the new standardized
	  # values that were created for making predictions ('nrkm')
	    Rnrkm <- nrkm*sd(time$RelKM)+mean(time$RelKM)

	  # Calculate predicted means
		  # Calculate predicted means
	      Rk.preds <- exp(timing.res[1, 1] +
	      	timing.res[2,1] * median(timing$photo) +
	  		  timing.res[3, 1] * nrkm +
	  	    timing.res[4, 1] * .5 +
	  	    timing.res[5, 1] * median(timing$Dis))
		  # Calculate predicted upper CL
	      Rk.predsL <- exp((timing.res[1, 1] - 1.96*timing.res[1, 2])+
	    	  (timing.res[2, 1] - 1.96 * timing.res[2, 2]) * median(timing$photo) +
	  		  (timing.res[3, 1] - 1.96 * timing.res[3, 2]) * nrkm +
	  	    (timing.res[4, 1] - 1.96 * timing.res[4, 2]) * .5 +
	  	    (timing.res[5, 1] - 1.96 * timing.res[5, 2]) * median(timing$Dis))
	    # Calculate predicted lower CL
	      Rk.predsU <- exp((timing.res[1, 1] + 1.96*timing.res[1, 2])+
	    	  (timing.res[2, 1] + 1.96 * timing.res[2, 2]) * median(timing$photo) +
	  		  (timing.res[3, 1] + 1.96 * timing.res[3, 2]) * nrkm +
	  	    (timing.res[4, 1] + 1.96 * timing.res[4, 2]) *.5 +
	  	    (timing.res[5, 1] + 1.96 * timing.res[5, 2]) * median(timing$Dis))

	# Discharge
	  # Min of standardized Dis
	    MiD<-  min(timing$Dis)
	  # Max of standardized Dis
      MaD <- max(timing$Dis)
	  # New values of standardized and logit-transformed Dis for making
	  # predictions
	    nD<- seq(MiD, MaD, 0.01)
		# Real-scale values of Dis that correspond to the new standardized
	  # values that were created for making predictions ('nA')
	    RnF<- nD*sd(time$Dis)+mean(time$Dis)

	  # Calculate predicted means
		  # Calculate predicted means
	      RF.preds <- exp(timing.res[1, 1] +
	      	timing.res[2,1] * median(timing$photo) +
	  		  timing.res[3, 1] * median(timing$RelKM) +
	  	    timing.res[4, 1] * .5 +
	  	    timing.res[5, 1] * nD)
		  # Calculate predicted upper CL
	      RF.predsL <- exp((timing.res[1, 1] - 1.96*timing.res[1, 2])+
	    	  (timing.res[2, 1] - 1.96 * timing.res[2, 2]) * median(timing$photo) +
	  		  (timing.res[3, 1] - 1.96 * timing.res[3, 2]) * median(timing$RelKM) +
	  	    (timing.res[4, 1] - 1.96 * timing.res[4, 2]) * .5 +
	  	    (timing.res[5, 1] - 1.96 * timing.res[5, 2]) * nD)
	    # Calculate predicted lower CL
	      RF.predsU <- exp((timing.res[1, 1] + 1.96*timing.res[2, 2])+
	    	  (timing.res[2, 1] + 1.96 * timing.res[2, 2]) * median(timing$photo) +
	  		  (timing.res[3, 1] + 1.96 * timing.res[3, 2]) * median(timing$RelKM) +
	  	    (timing.res[4, 1] + 1.96 * timing.res[4, 2]) * .5 +
	  	    (timing.res[5, 1] + 1.96 * timing.res[5, 2]) * nD)

	# NKA activity
	  # Min of standardized NKA
	    MiNKA<-  min(timing$ATP)
	  # Max of standardized NKA
      MaNKA <- max(timing$ATP)
	  # New values of standardized and logit-transformed NKA for making
	  # predictions
	    nNKA<- seq(MiNKA, 3, 0.01)
		  nNKA2<- nNKA^2
		# Real-scale values of NKAthat correspond to the new standardized
	  # values that were created for making predictions ('nNKA')
	    RnNKA<- nNKA*sd(time$ATP)+mean(time$ATP)

	  # Calculate predicted means
	    NKA.res <- summary(Cand.models[[16]])$coefficients
		  # Calculate predicted means
	      RN.preds <- exp(NKA.res[1, 1] +
	      	NKA.res[2,1] * median(timing$photo) +
	  		  NKA.res[3, 1] * median(timing$RelKM) +
	  	    NKA.res[5, 1] * nNKA +
	  	    NKA.res[6, 1] * nNKA2 +
	  	    NKA.res[7, 1] * median(timing$ATU))
		  # Calculate predicted lower CL
	      RN.predsL <- exp((NKA.res[1, 1] - 1.96*NKA.res[1, 2])+
	    	  (NKA.res[2, 1] - 1.96 * NKA.res[2, 2]) * median(timing$photo) +
	  		  (NKA.res[3, 1] - 1.96 * NKA.res[3, 2]) * median(timing$RelKM) +
	  	    (NKA.res[5, 1] - 1.96 * NKA.res[4, 2]) * nNKA +
	  	    (NKA.res[6, 1] - 1.96 * NKA.res[5, 2]) * nNKA2 +
	  	    (NKA.res[7, 1] - 1.96 * NKA.res[6, 2]) * median(timing$ATU))
	    # Calculate predicted upper CL
	      RN.predsU <- exp((NKA.res[1, 1] + 1.96*NKA.res[1, 2])+
	    	  (NKA.res[2, 1] + 1.96 * NKA.res[2, 2]) * median(timing$photo) +
	  		  (NKA.res[3, 1] + 1.96 * NKA.res[3, 2]) * median(timing$RelKM) +
	  	    (NKA.res[5, 1] + 1.96 * NKA.res[4, 2]) * nNKA +
	  	    (NKA.res[6, 1] + 1.96 * NKA.res[5, 2]) * nNKA2 +
	  	    (NKA.res[7, 1] + 1.96 * NKA.res[6, 2]) * median(timing$ATU))

	# Thermal history
	  # Min of standardized ATU
	    MiATU<-  min(timing$ATU)
	  # Max of standardized ATU
      MaATU <- max(timing$ATU)
	  # New values of standardized and logit-transformed NKA for making
	  # predictions
	    nATU<- seq(MiATU, MaATU, 0.01)
		# Real-scale values of ATU that correspond to the new standardized
	  # values that were created for making predictions ('nATU')
	    RnATU<- nATU*sd(time$ATU)+mean(time$ATU)

	  # Calculate predicted means
	    NKA.res <- summary(Cand.models[[16]])$coefficients
		  # Calculate predicted means
	      ATU.preds <- exp(NKA.res[1, 1] +
	      	NKA.res[2, 1] * median(timing$photo) +
	  		  NKA.res[3, 1] * median(timing$RelKM) +
	  	    NKA.res[5, 1] * median(timing$ATP) +
	  	    NKA.res[6, 1] * median(timing$ATP2) +
	  	    NKA.res[7, 1] * nATU)
		  # Calculate predicted lower CL
	      ATU.predsL <- exp((NKA.res[1, 1] - 1.96*NKA.res[1, 2])+
	    	  (NKA.res[2, 1] - 1.96 * NKA.res[2, 2]) * median(timing$photo) +
	  		  (NKA.res[3, 1] - 1.96 * NKA.res[3, 2]) * median(timing$RelKM) +
	  	    (NKA.res[5, 1] - 1.96 * NKA.res[4, 2]) * median(timing$ATP) +
	  	    (NKA.res[6, 1] - 1.96 * NKA.res[5, 2]) * median(timing$ATP2) +
	  	    (NKA.res[7, 1] - 1.96 * NKA.res[6, 2]) * nATU)
	    # Calculate predicted upper CL
	      ATU.predsU <- exp((NKA.res[1, 1] + 1.96*NKA.res[1, 2])+
	    	  (NKA.res[2, 1] + 1.96 * NKA.res[2, 2]) * median(timing$photo) +
	  		  (NKA.res[3, 1] + 1.96 * NKA.res[3, 2]) * median(timing$RelKM) +
	  	    (NKA.res[5, 1] + 1.96 * NKA.res[4, 2]) * median(timing$ATP) +
	  	    (NKA.res[6, 1] + 1.96 * NKA.res[5, 2]) * median(timing$ATP2)+
	  	    (NKA.res[7, 1] + 1.96 * NKA.res[6, 2]) * nATU)

# Plot covariates of estuary arrival date
	 # Plot photoperiod results
     # Name the file
 	     tiff( file = "C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Figures & Tables/Figure2.tiff" ,
          width = 6000 , height = 6000 , pointsize = 18 ,
          compression = "lzw" , res = 400 )
# 		 # Plot the mean predictions
#    	 # Set up a plotting window
# 	     par(mar=c(4, 10, 6.5, 0.5), mfrow=c(2,2))
# 	     plot(RnRD, RD.preds, type="l", ylim=c(110, 155), xlab="",
# 	       ylab="", lwd=3, cex.lab=3, yaxt="n", xaxt="n", cex.axis=2,
# 	     	 col='black')
# 	    # Plot the LCL
# 	      par(new=TRUE)
# 	      plot(RnRD, RD.predsL, type = "l", lty=2, ylim=c(110, 155), ylab="",
# 	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
#           xaxt="n")
# 	    # Plot the UCL predictions
# 	      par(new=TRUE)
# 	      plot(RnRD, RD.predsU, type = "l", lty=2, ylim=c(110, 155), ylab="",
# 	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
#           xaxt="n")
# 		  # Give the whole graph axes and labels
# 	      mtext(side=2, "Estuary arrival date", cex=3, line=6.5, adj=2.5)
# 		    axis(side=2, at=c(110, 121, 130, 140, 150),
# 		  	  c('25 Apr', " 1 May", "10 May", "20 May",
# 		    	"30 May"), las=2, cex.axis=1.5)
# 	      mtext(side=1, "Photoperiod at tagging", cex=2, line=4)
# 			  axis(side=1, at=c(seq(13, 15.5, 0.5)), as.character(c(seq(13, 15.5,
# 			  	0.5))), cex.axis=1.5)
# 	      mtext(side=3, "Date of tagging", cex=2, line=3.5)
# 		    axis(side=3, at=c(daylength(44.821614 ,c(105, 115, 130, 145))),
# 		    	c("15 Apr", "25 Apr", "10 May", "25 May"), cex.axis=1.5)
# 	      text(x=13.35, y=154, '(a)', cex=2)

	 # Plot RelKM results
		 # Plot the raw data
	     par(mar=c(4, 10, 6.5, 0.5), mfrow=c(2,2))
	     plot(time$RelKM, time$ArrivalOrd, col='gray87',
	       ylim=c(110, 155), ylab="",
	       xlab="", cex.lab=3, yaxt="n", cex.axis=2,
         xaxt="n")
		 # Plot the mean predictions
		   par(new=TRUE)
	      plot(Rnrkm, Rk.preds, type = "l", lty=1, ylim=c(110, 155), ylab="",
	      	xlab="", col="black", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(90, 190))
		 # Plot the LCL predictions
		   par(new=TRUE)
	      plot(Rnrkm, Rk.predsL, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(90, 190))
	    # Plot the UCL predictions
	      par(new=TRUE)
	      plot(Rnrkm, Rk.predsU, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(90, 190))
		  # Give the whole graph axes and labels
        mtext(side=2, "Estuary arrival date", cex=3, line=6.5, adj=3)
 		    axis(side=2, at=c(110, 121, 130, 140, 150),
 		  	  c('25 Apr', " 1 May", "10 May", "20 May",
		    	"30 May"), las=2, cex.axis=1.5)
	      mtext(side=1, "Release rkm", cex=2, line=3.5)
			  axis(side=1, at=c(seq(100, 180, 20)), as.character(c(seq(100, 180,
			  	20))), cex.axis=1.5)
	      text(x=93, y=154, '(a)', cex=2)

	 # Plot the Discharge results
	   # Plot the raw data
	     par(mar=c(4, 10, 6.5, 0.5))
	     plot(time$Dis, time$ArrivalOrd, col='gray87',
	      ylim=c(110, 155), ylab="",
	      xlab="", cex.lab=3, yaxt="n", cex.axis=2,
         xaxt="n")
		 # Plot the mean predictions
		   par(new=TRUE)
	     plot(RnF, RF.preds, type="l", ylim=c(110, 155), xlab="",
	       ylab="", lwd=3, cex.lab=3, yaxt="n", xaxt="n", cex.axis=2,
	     	 col='black')
	    # Plot the LCL
	      par(new=TRUE)
	      plot(RnF, RF.predsL, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n")
	    # Plot the UCL predictions
	      par(new=TRUE)
	      plot(RnF, RF.predsU, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n")
		  # Give the whole graph axes and labels
	      #mtext(side=2, "Estuary arrival date", cex=2, line=6.5)
		    axis(side=2, at=c(110, 121, 130, 140, 150),
		  	  c('25 Apr', " 1 May", "10 May", "20 May",
		    	"30 May"), las=2, cex.axis=1.5)
	      mtext(side=1, expression(paste('Discharge (m'^'3',plain('\u00b7'),
	      	's'^'-1',')')), cex=2, line=4)
			  axis(side=1, at=c(17857, 35714, 53571, 71428),
			  	as.character(c(500, 1000, 1500, 2000)),
			  	cex.axis=1.5)
	      text(x=11000, y=154, '(b)', cex=2)

	 # Plot ATU results
	    # Plot the raw data
	      par(mar=c(6, 10, 5.5, 0.5))
	      plot(time$ATU, time$ArrivalOrd, col='gray87',
	      	ylim=c(110, 155), ylab="",
	      	xlab="", cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(220, 500))
		 # Plot the mean predictions
		   par(new=TRUE)
	     plot(RnATU, ATU.preds, type="l", ylim=c(110, 155), xlab="",
	       ylab="", lwd=3, cex.lab=3, yaxt="n", xaxt="n", cex.axis=2,
	     	 col='black', xlim=c(220, 500))
	    # Plot the LCL
	      par(new=TRUE)
	      plot(RnATU, ATU.predsL, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(220, 500))
	    # Plot the UCL predictions
	      par(new=TRUE)
	      plot(RnATU, ATU.predsU, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n", xlim=c(220, 500))
		  # Give the whole graph axes and labels
 	      #mtext(side=2, "Estuary arrival date", cex=2, line=7.5)
 		    axis(side=2, at=c(110, 121, 130, 140, 150),
 		  	  c('25 Apr', " 1 May", "10 May", "20 May",
		    	"30 May"), las=2, cex.axis=1.5)
	      mtext(side=1, "Accumulated thermal units (ATU)", cex=2, line=3.5)
			  axis(side=1, at=c(seq(200, 500, 50)),
			  	as.character(c(seq(200, 500, 50))), cex.axis=1.5)
	      text(x=230, y=154, '(c)', cex=2)


	 # Plot NKA results
		 # Plot the raw data
	     par(mar=c(6, 10, 5.5, 0.5))
	     plot(time$ATP, time$ArrivalOrd, col='gray87',
	      	ylim=c(110, 155), ylab="",
	      	xlab="", cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n")
		 # Plot the mean predictions
		   par(new=TRUE)
	     plot(RnNKA, RN.preds, type="l", ylim=c(110, 155), xlab="",
	       ylab="", lwd=3, cex.lab=3, yaxt="n", xaxt="n", cex.axis=2,
	     	 col='black')
	    # Plot the LCL
	      par(new=TRUE)
	      plot(RnNKA, RN.predsL, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n")
	    # Plot the UCL predictions
	      par(new=TRUE)
	      plot(RnNKA, RN.predsU, type = "l", lty=2, ylim=c(110, 155), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=2,
          xaxt="n")
		  # Give the whole graph axes and labels
 	      #mtext(side=2, "Estuary arrival date", cex=2, line=7.5)
		    axis(side=2, at=c(110, 121, 130, 140, 150),
		  	  c('25 Apr', " 1 May", "10 May", "20 May",
		    	"30 May"), las=2, cex.axis=1.5)
	      mtext(side=1, "Gill NKA activity", cex=2, line=3.5)
			  axis(side=1, at=c(seq(2, 12, 2)), as.character(c(seq(2, 12, 2))),
			  	cex.axis=1.5)
	      text(x=1.3, y=154, '(d)', cex=2)

	  #Turn off graphics device to print to file
	    dev.off()
################################################################################

################################################################################
# Conduct estuary movements analysis (Hermes model)
#
# INPUT FILENAME(S):  'SmoltEstuaryMovements.rda' (or .csv)
# MY FUNCTIONS:       'DTAgain','approx.chat', 'standardize', 'aic.table'
# NAMED OUPUT:
# OUTPUT FILENAME(S): 'MovModelSelection.csv'
################################################################################
# Install and load necessary packages
  #install.packages("AICcmodavg", deps=TRUE) # Uncomment to install
  #install.packages("reshape")
  #install.packages("reshape2")
  #install.packages("lmerTest")
  require(reshape)
  require(reshape2)
  require(AICcmodavg)
	require(lme4)
  require(lmerTest)
	require(plotrix)
	require(lubridate)
	require(car)
	require(tcltk)
	require(tcltk2)
	require(geosphere)

# Set working directory and read in the movement data
  #setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
  load('SmoltEstuaryMovements.rda') # This is a df called mvmt

# FUNCTION DEFINITION
  # 'aic.tableR'
  # Function to create AIC Model selection table for mixed models
    # Write a fxn to do it
      aic.tableR <- function(x, y){
        # Create a column to hold number of parameters
          k <- c()
        # Get number of parameters
          for (i in 1:length(x)){
            #k[i] <- (length(x[[i]]$coefficients))
            k[i]<-nrow(data.frame(summary(x[[i]])[10]))+1
          }
        # Make blank columns to hold statistics
          AICc <- c(rep(0, length(k)))
          DeltaAIC <- c(rep(0, length(k)))
          AICWeight <- c(rep(0, length(k)))
          Deviance <- c(rep(0, length(k)))
          Relative.likelihood<-c(rep(0, length(k)))
        # Make dataframe to hold results
          ranks<-data.frame(y, k, AICc, DeltaAIC, AICWeight)
          names(ranks)[1] <- "Model"
        # Get AIC for each model
          for(i in 1:nrow(ranks)){
            ranks$AICc[i] <- AIC(x[[i]])+(((2*k[i])*(k[i]+1))/
              (nrow(est.mvmt)-k[i]-1))
          }
        # Sort the table by AICc
          ranks <- ranks[with(ranks, order(AICc)), ]
        # Calculate delta AIC
          for(i in 1:nrow(ranks)){
            ranks$DeltaAIC[i] <- ranks$AICc[i]-ranks$AICc[1]
          }
        # Calculate relative likelihoods
          for(i in 1:nrow(ranks)){
            Relative.likelihood[i] <- exp(-.5*ranks$DeltaAIC[i])
          }
        # Calculate AIC weights
          for(i in 1:length(Relative.likelihood)){
            ranks$AICWeight[i] <- Relative.likelihood[i]/
             sum(Relative.likelihood[1:length(Relative.likelihood)])
          }
        # Round off the calculated columns
          ranks[ , 3:5] <- round(ranks[ , 3:5], 2)
        # Print the model selection results
          return(ranks)
    }

  # 'DTAgain'
  # fxn for getting dates back from numeric format using UTC, default origin
    DTAgain<- function(x){
      as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
        strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
    }

  # 'standardize'
  # Create fxn to standardize covariates
  	standardize=function(x,z=NULL){
      if(is.null(z)){
        return((x-mean(x))/sqrt(var(x)))
      } else {
          return((x-mean(z))/sqrt(var(z)))
        }
    }
# END FUNCTION DEFINITIONS

# # Check to make sure any ridiculous dates are not in the data (July/August!)
#   # Replace them with zero if they are present so we're not calculating movement
#   # rates for dead fish
#     for(i in 12:ncol(mvmt)){
#       for(t in 1:nrow(mvmt)){
#         if(yday(DTAgain(mvmt[t,i])) > 160){
#           mvmt[t,i] <- 0
#         } else {
#           next;
#         }
#       }
#     }
#
# Calculate movement rates between consecutive relocations for each fish
  # Remove any old files from the working directory so the data aren't added on
  # if the loop is run again!
    file.remove('fish.rates.txt')
  # Create a text-based progress bar and initialize it
    pb <- txtProgressBar(min = 0, max = nrow(mvmt), style = 3)
    for(t in 1:nrow(mvmt)){
      Sys.sleep(0.01) # Length of time-out for each progress bar write
      # Define a row for each fish
        FishRow <- c(mvmt[t,12:ncol(mvmt)])
      # Get the sites with detections
        ListSites <- names(FishRow[FishRow!=0])
      # Get the rkm values for those sites
        RKMs <- colsplit(ListSites, "_", names=c("RKM","Site"))[,1]
      # Define a vector over which movement rates will be calculated
        MoVector <- mvmt[t, 12:ncol(mvmt)][mvmt[t, 12:ncol(mvmt)]!=0]

      # Define empty vectors to hold:
        Time <- c() # Amount of time elapsed between min det at each relocation
        Dists <- c() # Distance between relocation events
        Rates <- c() # Rate of movement between relocation events

      # Now calculate speeds, distances, and movement rates
        for(i in 2:length(MoVector)){ # For each fish calculate at each location
          Time[i-1] <- (MoVector[i]-MoVector[i-1]) # Time elapsed during move
          Dists[i-1] <- RKMs[i-1]-RKMs[i] # Distance traveled in RKM
          Rates[i-1] <- Dists[i-1]/Time[i-1] # Rate of movement (km/second)
        } # Close i loop

      # Repeat the fish data so the rate measurements can be matched up with
      # individuals and individual covariates (this is just formatting).
        FishD <- mvmt[c(rep(t,(length(Rates)))), 1:11]

      # Make a dataframe holding fish data and movement rates for all sites at
      # at which each fish was located
        d <- data.frame(FishD, ListSites[2:length(ListSites)],
          Rates, MoVector[2:length(MoVector)])

        # Write the data to a file, append all fish after first to the end
          write.table(d, "fish.rates.txt", row.names=FALSE, col.names=FALSE,
            append=TRUE, sep=",")

      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
    } # Close t loop

    # Read in the new estuary movement data (created in previous loop)
	    setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData")
      est.mvmt <- read.csv('fish.rates.txt', header=F)

    # Give the data column headings
      names(est.mvmt) <- c(names(mvmt[1:11]), "RKM_Loc", "Rate", "MovDate")

    # Look at the data to make sure everything looks right
      head(est.mvmt)

	  # Convert km/s to km/h
	    est.mvmt$Rate <- est.mvmt$Rate*3600

    # Remove fish that were only ever located once
      est.mvmt <- est.mvmt[est.mvmt$Rate > 0, ]

    # Convert movement rates to body lengths per second based on the length of
    # the fish for which movement rate was calculated; last step of movement
    # rate calculation
      #est.mvmt$BLS <- est.mvmt$Rate*1000000/est.mvmt$ForkLength.mm
    # Finally, standardize and center movement rate for analysis to facilitate
    # models that make biological sense (i.e. relax constraint on response var
    # that made it [0,inf]
      #est.mvmt$CBLS <- standardize(log(est.mvmt$BLS))

# Add the rest of the covariates
  # Add a column for condition factor (k)
    est.mvmt$k <- (est.mvmt$Mass.g/(est.mvmt$ForkLength.mm^3))*100000
  # Change release date to ordinal
    est.mvmt$ReleaseDate <- yday(est.mvmt$ReleaseDate)
  # Change MovDate to ordinal
    est.mvmt$MovDate <- yday(DTAgain(est.mvmt$MovDate))
    # Calculate a second-order term for optimum movement date
      est.mvmt$MovDate2 <- est.mvmt$MovDate^2

# Get river kilometer in at which movement began
	est.mvmt$RKM_Loc <- as.character(est.mvmt$RKM_Loc)
  est.mvmt$RKM <- colsplit(est.mvmt$RKM_Loc, pattern="_", names=c("RKM", "Loc"))[,1]
  # Calculate second-order term for river kilometer
    est.mvmt$RKM2 <- est.mvmt$RKM^2

# Change the column names to be shorter
  names(est.mvmt) <- c("TagID", "FL", "Mass", "ATP", "RelSite", "RelDate",
    "Org", "Year", "SW", "RelKM", "Dams", "RKM_Loc", "Rate", "MovDate", "k",
  	"MovDate2", "RKM", "RKM2")

# Go back and get flow for the estuary movement data
  # Read in Flow data ('WestEnfieldFlowDataClean.txt'), replace NA with 0 b/c ice
    Discharge <- read.table('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/WestEnfieldFlowDataClean.txt',
    	header=TRUE)[ , 3:4]
	  names(Discharge) <- c('Date', 'Discharge')
	  Discharge[ , 2] <- as.character(Discharge[ , 2])
	  for(i in 1:nrow(Discharge)){
		  if(Discharge[i , 2]=='Ice'){
	      Discharge[i , 2] <- '0'
		  } else {
			  next;
		  }
	  }
	  Discharge[ , 2] <- as.numeric(Discharge[ , 2])

  # Get discharge for each day
    # Make a vector to hold the year for each discharge measurement
      Discharge$Year <- year(as.POSIXct(Discharge[,1]))

  # Calculate mean discharge for each fish
    pb <- txtProgressBar(min = 0, max = nrow(est.mvmt), style = 3)
    for(i in 1:nrow(est.mvmt)){
      Sys.sleep(0.1)
      est.mvmt$Dis[i] <- mean(Discharge$Discharge[Discharge$Year==est.mvmt$Year[i] &
        (yday(Discharge$Date) < est.mvmt$MovDate[i] | yday(Discharge$Date)==est.mvmt$MovDate[i]) &
        (yday(Discharge$Date) > est.mvmt$RelDate[i] | yday(Discharge$Date)==est.mvmt$RelDate[i])])
      setTxtProgressBar(pb, i)
    } # i

# Make a photoperiod covariate to test
  est.mvmt$PhS <- scale(daylength(44.821614, est.mvmt$MovDate))
  est.mvmt$Ph2S <- scale(daylength(44.821614, est.mvmt$MovDate))^2

# Remove the NA values of Rate that result from fish not being relocated more
# than once
	est.mvmt <- na.omit(est.mvmt)

# Save a version of the dataframe with the real-scale values of variables that
# can be used later for un-standardizing variables so that the model effects can
# be plotted
	est.mvmtR <- est.mvmt
	est.mvmtR$PhS <- (daylength(44.821614, est.mvmtR$MovDate))

# Standardize covariates
   est.mvmt[ , c(2:4 ,10:11, 14:19)] <-
     apply(est.mvmt[ , c(2:4 ,10:11, 14:19)], 2, scale)

# Make a column  for km/hr to check this out for John and to facilitate reporting
# 	est.mvmt$Rate <- (est.mvmtR$BLS*est.mvmtR$FL)/(60*60)
# 	est.mvmtR$Rate <- (est.mvmtR$BLS*est.mvmtR$FL)/(60*60)

# Create a candidate model set
  Mov.mods = list()
    Mov.mods[[1]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[2]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[3]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + k + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[4]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + ATP + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[5]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + Org + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[6]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[7]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + k + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[8]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + ATP + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[9]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + Org + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[10]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[11]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + k + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[12]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + ATP + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[13]] = lmer(log(Rate) ~ (1|TagID) +  RKM + RKM2 + Dis + Org + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[14]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[15]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[16]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + k + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[17]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + ATP + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[18]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + Org + SW + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[19]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[20]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + k + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[21]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + ATP + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[22]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + Org + Dams + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[23]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[24]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + k + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[25]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + ATP + RelKM + PhS + Ph2S, data = est.mvmt)
    Mov.mods[[26]] = lmer(log(Rate) ~ (1|TagID) +  RKM + Dis + Org + RelKM + PhS + Ph2S, data = est.mvmt)

# Create a vector of names for the models
  MovNames <- c()
    MovNames[1] <- 'RKM + RKM 2 + Dis + PhS + PhS2'
    MovNames[2] <- 'RKM + RKM 2 + Dis + SW + PhS + PhS2'
    MovNames[3] <- 'RKM + RKM 2 + Dis + k + SW + PhS + PhS2'
    MovNames[4] <- 'RKM + RKM 2 + Dis + ATP + SW + PhS + PhS2'
    MovNames[5] <- 'RKM + RKM 2 + Dis + Org + SW + PhS + PhS2'
    MovNames[6] <- 'RKM + RKM 2 + Dis + Dams + PhS + PhS2'
    MovNames[7] <- 'RKM + RKM 2 + Dis + k + Dams + PhS + PhS2'
    MovNames[8] <- 'RKM + RKM 2 + Dis + ATP + Dams + PhS + PhS2'
    MovNames[9] <- 'RKM + RKM 2 + Dis + Org + Dams + PhS + PhS2'
    MovNames[10] <- 'RKM + RKM 2 + Dis + RelKM + PhS + PhS2'
    MovNames[11] <- 'RKM + RKM 2 + Dis + k + RelKM + PhS + PhS2'
    MovNames[12] <- 'RKM + RKM 2 + Dis + ATP + RelKM + PhS + PhS2'
    MovNames[13] <- 'RKM + RKM 2 + Dis + Org + RelKM + PhS + PhS2'
    MovNames[14] <- 'RKM + Dis + PhS + PhS2'
    MovNames[15] <- 'RKM + Dis + SW + PhS + PhS2'
    MovNames[16] <- 'RKM + Dis + k + SW + PhS + PhS2'
    MovNames[17] <- 'RKM + Dis + ATP + SW + PhS + PhS2'
    MovNames[18] <- 'RKM + Dis + Org + SW + PhS + PhS2'
    MovNames[19] <- 'RKM + Dis + Dams + PhS + PhS2'
    MovNames[20] <- 'RKM + Dis + k + Dams + PhS + PhS2'
    MovNames[21] <- 'RKM + Dis + ATP + Dams + PhS + PhS2'
    MovNames[22] <- 'RKM + Dis + Org + Dams + PhS + PhS2'
    MovNames[23] <- 'RKM + Dis + RelKM + PhS + PhS2'
    MovNames[24] <- 'RKM + Dis + k + RelKM + PhS + PhS2'
    MovNames[25] <- 'RKM + Dis + ATP + RelKM + PhS + PhS2'
    MovNames[26] <- 'RKM + Dis + Org + RelKM + PhS + PhS2'

# Create a model-selection table
  MovResults <- aic.tableR(Mov.mods, MovNames)
# Look at model-selection results
	MovResults

# Save the model-selection table to a file
   setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Figures & Tables")
   write.table(MovResults, "MovementModelSelection.csv", row.names=FALSE,
     sep=",")

# Look at results of best model
  best.mov <- summary(Mov.mods[[as.numeric(row.names(MovResults)[1])]])
	best.mov

# Look at the coefficient estimates
	mov.res <- best.mov$coefficients
  write.table(mov.res, "MovementCoeffs.csv", row.names=FALSE,
    sep=",")
	mov.res

# Make covariate predictions from the best model
	# Calculate effect of location in estuary on movement
	  # Get new values of RKM for prediction
  	  # Min of standardized estuary RKM
	      MiRKM <- min(est.mvmt$RKM)
	    # Max of standardized estuary RKM
	      MaRKM <- max(est.mvmt$RKM)
	    # New values of standardized estuary RKM for prediction
	      nRKM <- seq(MiRKM, MaRKM, 0.01)
      # Real-scale values of RKM that can be used for plotting predictions
	      RnRKM <- nRKM * sd(est.mvmtR$RKM) + mean(est.mvmtR$RKM)
	  # Predict movement rate based on new values of RKM and median values for the
	  # rest of the explanatory variables
	    # Mean predicted movement rate
	      RKM.preds <- mov.res[1, 1] +
		      mov.res[2, 1] * nRKM +
		      mov.res[3, 1] * median(est.mvmt$Dis) +
		      mov.res[4, 1] * median(est.mvmt$RelKM) +
		      mov.res[5, 1] * median(est.mvmt$PhS) +
		      mov.res[6, 1] * median(est.mvmt$Ph2S)
	    # Unstandardized mean predicted movement rates
	      uRKM.preds <- exp(RKM.preds)
	    # Upper CL for predicted movement rate
	      RKM.predsU <- (mov.res[1, 1] + (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] + 1.96 * mov.res[2, 2]) * nRKM +
		      (mov.res[3, 1] + 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis)+
		      (mov.res[4, 1] + 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] + 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] + 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
		  # Unstandardized upper CL predicted movement rates
	      uRKM.predsU <- exp(RKM.predsU + 1.22)
	    # Lower CL for predicted movement rate
	      RKM.predsL <- (mov.res[1, 1] - (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] - 1.96 * mov.res[2, 2]) * nRKM +
		      (mov.res[3, 1] - 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis) +
		      (mov.res[4, 1] - 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] - 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] - 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
	    # Unstandardized upper CL predicted movement rates
	      uRKM.predsL <- exp(RKM.predsL - 1.22)

	# Calculate effect of timing on movement rates
	  # Get new values of date for prediction
  	  # Min of standardized date
	      MiPh <- min(est.mvmt$PhS)
	    # Max of standardized date
	      MaPh<- max(est.mvmt$PhS)
	    # New values of standardized date for prediction
	      nPh<- seq(MiPh, MaPh, 0.01)
      # Real-scale values of date that can be used for plotting predictions
	      RnPh <- nPh * sd(est.mvmtR$PhS) + mean(est.mvmtR$PhS)
	  # Predict movement rate based on new values of date and median values for
	  # rest of the explanatory variables
	    # Mean predicted movement rate
	      Ph.preds <- mov.res[1, 1] +
		      mov.res[2, 1] * median(est.mvmt$RKM) +
		      mov.res[3, 1] * median(est.mvmt$Dis) +
		      mov.res[4, 1] * median(est.mvmt$RelKM) +
		      mov.res[5, 1] * nPh +
		      mov.res[6, 1] * nPh^2
	    # Unstandardized mean predicted movement rates
	      uPh.preds <- exp(Ph.preds)
	    # Upper CL for predicted movement rate
	      Ph.predsU <- (mov.res[1, 1] + (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] + 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] + 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis)+
		      (mov.res[4, 1] + 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] + 1.96 *mov.res[5, 2]) * nPh +
		      (mov.res[6, 1] + 1.96 *mov.res[6, 2]) * nPh^2
		  # Unstandardized upper CL predicted movement rates
	      uPh.predsU <- exp(Ph.predsU+1.22)
	    # Lower CL for predicted movement rate
	      Ph.predsL <- (mov.res[1, 1] - (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] - 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] - 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis) +
		      (mov.res[4, 1] - 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] - 1.96 * mov.res[5, 2]) * nPh +
		      (mov.res[6, 1] - 1.96 * mov.res[6, 2]) * nPh^2
			# Unstandardized upper CL predicted movement rates
	      uPh.predsL <- exp(Ph.predsL-1.22)

	# Caclulate effect of release location
	  # Get new values of release RKM
  	  # Min of standardized release RKM
	    MiRel <- min(est.mvmt$RelKM)
	  # Max of standardized release RKM
	    MaRel <- max(est.mvmt$RelKM)
	  # New values of standardized release RKM
	    nRel <- seq(MiRel, MaRel, 0.01)
      # Real-scale values of RKM that can be used for plotting predictions
	    RnRel <- nRel * sd(est.mvmtR$RelKM) + mean(est.mvmtR$RelKM)
	  # Predict movement rate based on new values of RKM and median values for the
	  # rest of the explanatory variables
	    # Mean predicted movement rate
	      Rel.preds <- mov.res[1, 1] +
		      mov.res[2, 1] * median(est.mvmt$RKM) +
		      mov.res[3, 1] * median(est.mvmt$Dis) +
		      mov.res[4, 1] * nRel +
		      mov.res[5, 1] * median(est.mvmt$PhS) +
		      mov.res[6, 1] * median(est.mvmt$Ph2S)
	    # Unstandardized mean predicted movement rates
	      uRel.preds <- exp(Rel.preds)
	    # Upper CL for predicted movement rate
	      Rel.predsU <- (mov.res[1, 1] + (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] + 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] + 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis)+
		      (mov.res[4, 1] + 1.96 * mov.res[4, 2]) * nRel +
		      (mov.res[5, 1] + 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] + 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
		  # Unstandardized upper CL predicted movement rates
	      uRel.predsU <- exp(Rel.predsU+1.22)
	    # Lower CL for predicted movement rate
	      Rel.predsL <- (mov.res[1, 1] - (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] - 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] - 1.96 * mov.res[3, 2]) * median(est.mvmt$Dis) +
		      (mov.res[4, 1] - 1.96 * mov.res[4, 2]) * nRel +
		      (mov.res[5, 1] - 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] - 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
			# Unstandardized upper CL predicted movement rates
	      uRel.predsL <- exp(Rel.predsL-1.22)

	# Caclulate effect of discharge
	  # Get new values of discharge
  	  # Min of standardized discharge
	    MiDi <- min(est.mvmt$Dis)
	  # Max of standardized  discharge
	    MaDi <- max(est.mvmt$Dis)
	  # New values of standardized  discharge
	    nDi <- seq(MiDi, MaDi, 0.01)
      # Real-scale values of  discharge that for plotting predictions
	    RnDi <- nDi * sd(est.mvmtR$Dis) + mean(est.mvmtR$Dis)
	  # Predict movement rate based on new  discharge and median values for the
	  # rest of the explanatory variables
	    # Mean predicted movement rate
	      Di.preds <- mov.res[1, 1] +
		      mov.res[2, 1] * median(est.mvmt$RKM) +
		      mov.res[3, 1] * nDi +
		      mov.res[4, 1] * median(est.mvmt$RelKM) +
		      mov.res[5, 1] * median(est.mvmt$PhS) +
		      mov.res[6, 1] * median(est.mvmt$Ph2S)
	    # Unstandardized mean predicted movement rates
	      uDi.preds <- exp(Di.preds)
	    # Upper CL for predicted movement rate
	      Di.predsU <- (mov.res[1, 1] + (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] + 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] + 1.96 * mov.res[3, 2]) * nDi +
		      (mov.res[4, 1] + 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] + 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] + 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
		  # Unstandardized upper CL predicted movement rates
	      uDi.predsU <- exp(Di.predsU + 1.22)
	    # Lower CL for predicted movement rate
	      Di.predsL <- (mov.res[1, 1] - (1.96 * mov.res[1, 2])) +
		      (mov.res[2, 1] - 1.96 * mov.res[2, 2]) * median(est.mvmt$RKM) +
		      (mov.res[3, 1] - 1.96 * mov.res[3, 2]) * nDi +
		      (mov.res[4, 1] - 1.96 * mov.res[4, 2]) * median(est.mvmt$RelKM) +
		      (mov.res[5, 1] - 1.96 * mov.res[5, 2]) * median(est.mvmt$PhS) +
		      (mov.res[6, 1] - 1.96 * mov.res[6, 2]) * median(est.mvmt$Ph2S)
			# Unstandardized upper CL predicted movement rates
	      uDi.predsL <- exp(Di.predsL - 1.22)

# Plot covariate predictions from best model
	  # Save the file
 		  tiff( file = "Figure3.tiff" ,
        width = 7000 , height = 6000 , pointsize = 18 ,
        compression = "lzw" , res = 500 )
	  # Timing vs Movement
	  # Raw data
	    par(mar=c(5, 8, 6, 1), mfrow=c(2, 2))
		  plot(est.mvmtR$PhS, est.mvmtR$Rate, col="gray87",
	      ylab="", ylim=c(0, 8), xlab="", yaxt="n",
        xaxt="n", xlim=c(13.9, 15.6))
	    par(new=TRUE)
	  # Predictions
	    plot(RnPh, (uPh.preds), type="l", lwd=3, lty=1, col="black",
	      ylab="", ylim=c(0, 8), xlab="", yaxt="n",
        xaxt="n", xlim=c(13.9, 15.6))
	    par(new=TRUE)
	    plot(RnPh, uPh.predsU, type="l", lwd=3, lty=2, col="gray40",
	      ylab="", ylim=c(0, 8), xlab="", yaxt="n",
        xaxt="n", xlim=c(13.9, 15.6))
	    par(new=TRUE)
	    plot(RnPh, (uPh.predsL), type="l", lwd=3, lty=2, col="gray40",
	      ylab="",ylim=c(.5, 8), xlab="", yaxt="n",
        xaxt="n", xlim=c(13.9, 15.6))
	    axis(2, at=c(.5,seq(0, 8, 2)), sprintf("%.1f",c(0,seq(0, 8, 2))), las=2,
	    	cex.axis=1.5)
	    #axis.break(axis=2, bgcol='white', breakcol='black', style='slash',
	    	#breakpos=.75, brw=0.05)
	    mtext(side=2, expression(paste("Movement rate (km ", plain('\u00b7'),
	      	' h'^'-1', ')')), cex=2.5, line=4, adj=1.25)
		  mtext(side=1, "Photoperiod (hours)", cex=2, line=4)
			axis(side=1, at=c(seq(14.0, 15.5, 0.5)), c('14.0', '14.5', '15.0',
				'15.5'), cex.axis=1.5)
	    mtext(side=3, "Date", cex=2, line=3, adj=0.5)
		  axis(side=3, at=round(daylength(44.821614,
		  	c(115, 130, 161)),	1),
		  	c("25 Apr", "10 May", '10 Jun'),
		  	cex.axis=1.5)
	    text(x=14, y=7, '(a)', cex=2)

    # RKM vs Movement
      par(mar=c(5, 6, 6, 3))
		    plot(est.mvmtR$RKM, est.mvmtR$Rate, col="gray87",
	      ylab="", ylim=c(0, 8), xlab="", yaxt="n",
        xaxt="n", xlim=c(45, -15))
	    par(new=TRUE)
	    plot(-(RnRKM), uRKM.preds, type="l", lwd=3, lty=1, col="black",
	      ylab="", xlab="", xlim=c(-45, 15), xaxt="n", ylim=c(0, 8),
	      cex.axis=1.5, yaxt="n")
	    par(new=TRUE)
	    plot(-(RnRKM), uRKM.predsU, type="l", lwd=3, lty=2, col="gray40",
	      ylab="", xlab="", xlim=c(-45,15), xaxt="n", ylim=c(0, 8),
	      cex.axis=1.5, yaxt="n")
	    par(new=TRUE)
	    plot(-(RnRKM), uRKM.predsL, type="l", lwd=3, lty=2, col="gray40",
	      ylab="", xlab="", xlim=c(-45, 15), cex.lab=3, xaxt="n",  ylim=c(0, 8),
	      cex.axis=1.5, yaxt="n")
	    axis(1,pretty(c(-40,10)),bg='black', labels=c("40", "30", "20", "10", "0",
        "-10"),xaxs="i",yaxs="i", cex.axis=1.5, las=1)
	    axis(2, at=c(0,seq(0, 8, 2)), sprintf("%.1f",c(0,seq(0, 8, 2))), las=2,
	    	cex.axis=1.5)
	    #axis.break(axis=2, bgcol='white', breakcol='black', style='slash',
	    	#breakpos=.75, brw=0.05)
      mtext("Upstream", 3, cex=1.5, line=1, adj=0)
      mtext("Downstream", 3, cex=1.5, line=1, adj=1)
	    mtext("Location in estuary (rkm)", 1, cex=2, line=4, adj=0.5)
 		  text(x=11, y=7, '(b)', cex=2)

	  # Release rkm vs Movement
	    par(mar=c(6, 8, 4, 1))
			plot(est.mvmtR$RelKM, est.mvmtR$Rate, col="gray87",
	      ylab="", ylim=c(0, 8), xlab="", yaxt="n",
        xaxt="n")
	    par(new=TRUE)
	    plot(RnRel, uRel.preds, type="l", lwd=3, lty=1, col="black",
	      ylab="", ylim=c(0, 8), xlab="", cex.axis=1.5, yaxt="n")
	    par(new=TRUE)
	    plot(RnRel, uRel.predsU, type="l", lwd=3, lty=2, col="gray40",
	      ylab="", ylim=c(0, 8), xlab="", cex.axis=1.5, yaxt="n")
	    par(new=TRUE)
	    plot(RnRel, uRel.predsL, type="l", lwd=3, lty=2, col="gray40",
	      ylab="", ylim=c(0, 8), xlab="", cex.lab=3, cex.axis=1.5, yaxt="n")
	    axis(2, at=c(0,seq(0, 8, 2)), sprintf("%.1f",c(0,seq(0, 8, 2))), las=2,
	    	cex.axis=1.5)
	    #axis.break(axis=2, bgcol='white', breakcol='black', style='slash',
	    	#breakpos=.75, brw=0.05)
	    mtext("Release rkm", 1, cex=2, line=4, adj=0.5)
 		  text(x=97, y=7, '(c)', cex=2)

	  # Plot the Discharge results
	    # Raw data
		    par(mar=c(6, 6, 4, 3))
				plot(est.mvmtR$Dis, est.mvmtR$Rate, col="gray87",
	        ylab="", ylim=c(0, 8), xlab="", yaxt="n",
          xaxt="n")
	      par(new=TRUE)
		  # Plot the mean predictions
	      plot(RnDi, uDi.preds, type="l", ylim=c(0, 8), xlab="",
	        ylab="", lwd=3, cex.lab=3, yaxt="n", xaxt="n", cex.axis=1.5,
	     	  col='black')
	    # Plot the LCL
	      par(new=TRUE)
	      plot(RnDi, uDi.predsL, type = "l", lty=2, ylim=c(0, 8), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=1.5,
          xaxt="n")
	    # Plot the UCL predictions
	      par(new=TRUE)
	      plot(RnDi, uDi.predsU, type = "l", lty=2, ylim=c(0, 8), ylab="",
	      	xlab="", col="gray40", lwd=3, cex.lab=3, yaxt="n", cex.axis=1.5,
          xaxt="n")
		  # Give the whole graph axes and labels
	      axis(2, at=c(0, seq(0, 8, 2)), sprintf("%.1f",c(0,seq(0, 8, 2))), las=2,
	    	  cex.axis=1.5)
		    #axis.break(axis=2, bgcol='white', breakcol='black', style='slash',
	    	  #breakpos=.75, brw=0.05)
	      mtext(side=1, expression(paste('Discharge (m'^'3',plain('\u00b7'),
	      	's'^'-1',')')), cex=2, line=4)
			  axis(side=1, at=c(8928, 26786, 44643, 62500),
			  	as.character(c(250, 750, 1250, 1750)),
			  	cex.axis=1.5)
	      text(x=0.95*max(RnDi), y=7, '(d)', cex=2)
	  # Turn off graphics device
 	    dev.off()

	# Rearing history
	  # Make a boxplot of Arrival time vs rearing origin
# 	    tiff( file = "MvmtVsOrigin.tiff" ,
#         width = 6000 , height = 5000 , pointsize = 18 ,
#         compression = "lzw" , res = 400 )
# 	    par(mar=c(7,6,1,1))
#     	boxplot(est.mvmt$BLS~est.mvmt$Org, col="gray50",
#         ylab=expression(paste("Movement rate ", "(bl", plain('\u00b7'), "s"^-1,
# 	      ")")), cex.lab=1.5, xaxt="n", yaxt="n")
# 	    mtext(side=1, at=c(1,2), c("Hatchery", "Wild"), cex=1.5, line=1.5)
# 	    axis(side=2, at=c(5,10,15,20), c("5","10","15","20"),	las=2)
# 	    dev.off()
################################################################################
} # Code-folding marker
# END PART 5. ESTUARY BEHAVIOR ANALYSIS-----------------------------------------


# PART 4. ESTUARY SURVIVAL ANALYSIS---------------------------------------------
{ # Code-folding marker
################################################################################
# Create estuary capture history for estuary survival analysis and add (most of)
# the individual covariates for survival and detection probability
#
# INPUT FILENAME(S):  'GodFull.rda' (or 'ch')
#                     'TimeVaryingTemperatureCovariate.rda' ('temp.Covariate'),
#                     'TimeVaryingPrecipitationCovariate.rda'
#                     ('precip.Covariate')
#                     'TimeVaryingPhotoperiodCovariate.rda' (or .csv)
#                     'TimeVaryingPhotoperiodCovariate.rda'
# MY FUNCTIONS
# NAMED OUPUT:        'est.caps' (df)
# OUTPUT FILENAME:    'EstuarySurvivalCaptureHistory.rda' (and .csv)
################################################################################
# Read in the capture history file 'GodFull.rda'
	setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary analysis/ProcessedData")
  load('GodFull.rda')

# Remove EBPR,  KEND, SOUD, OR, GWTR, WINN, PNMR03, MRSH because hardly any
# fish were ever detected at those locations. Remove DSR, BMILL, LGREEN, TGC,
# and CMPLX b/c those receivers were placed in 2013 for depth-tagged fish.
# Remove Verona Isle receivers for Demographic estimation
# Remove FW release sites for estuary analysis- they will be given one release
	ch2 <- ch[,c(1:8, 57, 61, 65, 68, 70, 73, 82, 89, 92, 93, 94)]

# Sort the ch by TagID so the rows line up with covariate matrices
	ch2 <- ch2[with(ch2, order(as.character(TagID))),]

# Load temp and precip covariates if they are not already in workspace as
# temp.Covariate, precip.Covariate, and photo.Covariate dfs
	load('TimeVaryingTemperatureCovariate.rda')
	load('TimeVaryingPrecipitationCovariate.rda')
	load('TimeVaryingPhotoperiodCovariate.rda')

# Trim down the temp, precip, and photoperiod matrices to match cols in ch2
  temp.Covariate2 <- temp.Covariate[ , c(names(temp.Covariate) %in% names(ch2))]
  precip.Covariate2 <- precip.Covariate[ ,
  	c(names(temp.Covariate) %in% names(ch2))]
	photo.Covariate2 <- photo.Covariate[ ,
		c(colnames(photo.Covariate) %in% names(ch2))]

# Combine capture histories for estuary analysis w/ time-varying covariates
  est.caps <- cbind(ch2,
  	temp.Covariate2[ , 2:ncol(temp.Covariate2)],
  	precip.Covariate2[ , 2:ncol(precip.Covariate2)],
  	photo.Covariate2)

# Create variable for Stillwater or Mainstem, unknown gets mean 0.5
# Numbers in this loop don't need to change unless ch changes in the future
	for(i in 1:nrow(est.caps)){
		if(sum(ch[i,c(45,48,49,52)])>0){
			est.caps$Stillwater[i] <- 1
		} else {
			if(sum(ch[i,c(46,47)])>0){
				est.caps$Stillwater[i] <- 0
			} else {
				if(sum(ch[i,c(45:49, 52)])==0){
				est.caps$Stillwater[i] <- 0.5
			  } else {
				  next;
			  }
		  }
	  }
	} # i

# Create variable to hold release rkm
	# Get RKM for each of the release sites so it can be added to 'Releases'
  # Get the unique values of release site
    ReleaseSites <- c(sort(unique(as.character(est.caps$ReleaseSite))))

  # Make a vector of corresponding rkms for release sites
    ReleaseRKMs <- c(187, 43.5, 162, 99, 144, 142,  92.3, 9.5, 149)

	# Convert the vector data type for ReleaseSite in est.caps to chr from factor
	  est.caps$ReleaseSite <- as.character(est.caps$ReleaseSite)

  # Create a column in the capture history to hold release RKM and fill it in
	  est.caps$ReleaseRKM <- 0
	  for(i in 1:length(ReleaseSites)){
    	for(t in 1:nrow(est.caps)){
	    	if(est.caps$ReleaseSite[t]==ReleaseSites[i]){
	    		est.caps$ReleaseRKM[t] <- ReleaseRKMs[i]
	    	}
	    }
  	}

# Create a variable for number of dams passed: first make a progress meter and
# then run the if-else statement from hell.
	pb <- txtProgressBar(min = 0, max = nrow(photo.Covariate), style = 3)
	for(i in 1:nrow(est.caps)){
    Sys.sleep(0.1)
		if(est.caps$ReleaseSite[i]=="Brewer" | est.caps$ReleaseSite[i]=="Verona"){
			est.caps$DamsPassed[i] <- 0
		} else {
			if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==1){
				est.caps$DamsPassed[i] <- 9
			} else {
				if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==0){
					est.caps$DamsPassed[i] <- 8
				} else {
					if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==0.5){
						est.caps$DamsPassed[i] <- 8.5
				  } else {
						if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==1){
					  	est.caps$DamsPassed[i] <- 9
					  } else {
					  	if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==0){
					  	  est.caps$DamsPassed[i] <- 7
					    } else {
						    if(est.caps$ReleaseSite[i]=="Abbott" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==0.5){
						      est.caps$DamsPassed[i] <- 8
					      } else {
					      	if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==1){
					      		est.caps$DamsPassed[i] <- 6
					      	} else {
					      		if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==0){
					      			est.caps$DamsPassed[i] <- 5
					      		} else {
					      			if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]!=2013 & est.caps$Stillwater[i]==0.5){
					      				est.caps$DamsPassed[i] <- 5.5
					      			} else {
					            	if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==1){
					      		      est.caps$DamsPassed[i] <- 6
					      	      } else {
					      		      if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==0){
					      		       	est.caps$DamsPassed[i] <- 4
					      		      } else {
					      			      if(est.caps$ReleaseSite[i]=="EastBranch" & est.caps$Year[i]==2013 & est.caps$Stillwater[i]==0.5){
					      				     est.caps$DamsPassed[i] <- 5
					      			      } else {
					      			      	if((est.caps$ReleaseSite[i]=="Weldon" | est.caps$ReleaseSite[i]=="BikeClub") & est.caps$Stillwater[i]==1){
					      			      		est.caps$DamsPassed[i] <- 5
					      			      	} else{
					      			      		if((est.caps$ReleaseSite[i]=="Weldon" | est.caps$ReleaseSite[i]=="BikeClub") & est.caps$Stillwater[i]==0){
					      			      			est.caps$DamsPassed[i] <- 4
					      			      		} else {
					      			      			if((est.caps$ReleaseSite[i]=="Weldon" | est.caps$ReleaseSite[i]=="BikeClub") & est.caps$Stillwater[i]==0.5){
					      			      				est.caps$DamsPassed[i] <- 4.5
					      			      			} else {
					      			      				if(est.caps$ReleaseSite[i]=="Milo" & est.caps$Stillwater[i]==1){
					      			      					est.caps$DamsPassed[i] <- 5
					      			      				} else {
					      			      					if(est.caps$ReleaseSite[i]=="Milo" & est.caps$Stillwater[i]==0){
					      			      						est.caps$DamsPassed[i] <- 4
					      			      					} else {
					      			      						if(est.caps$ReleaseSite[i]=="Milo" & est.caps$Stillwater[i]==0.5){
					      			      							est.caps$DamsPassed[i] <- 4.5
					      			      						} else {
					      			      							if((est.caps$ReleaseSite[i]=="Passadumkeag" | est.caps$ReleaseSite[i]=="Howland" )& est.caps$Stillwater[i]==1){
					      			      								est.caps$DamsPassed[i] <- 4
					      			      							}	else {
					      			      								if((est.caps$ReleaseSite[i]=="Passadumkeag" | est.caps$ReleaseSite[i]=="Howland") & est.caps$Stillwater[i]==0){
					      			      									est.caps$DamsPassed[i] <- 3
					      			      								} else {
					      			      									if((est.caps$ReleaseSite[i]=="Passadumkeag" | est.caps$ReleaseSite[i]=="Howland") & est.caps$Stillwater[i]==0.5){
					      			      										est.caps$DamsPassed[i] <- 3.5
					      			      									} else {
					      			      										next;
					      			      									}
					      			      								}
					      			      							}
					      			      						}
					      			      					}
					      			      				}
					      			      			}
					      			      		}
					      			      	}
					      			      }
					      			    }
					      		    }
					      	    }
					      		}
					      	}
					      }
					    }
					  }
					}
				}
			}
		}
	  setTxtProgressBar(pb, i)
	} # i (folding line)

# Reorganize the capture data and covariates to make things easy, take the last
# three columns that hold new covariates up front with others
	est.caps <- est.caps[ ,c(1:8, (ncol(est.caps)-2):ncol(est.caps),
		9:(ncol(est.caps)-3))]

# Remove fish for which there are no observations
  total.recaps <- c() # Empty vector to hold results of for loop
	for(i in 1:nrow(est.caps)){
  	total.recaps[i] <- sum(est.caps[i,12:22])
  }
  est.caps <- est.caps[total.recaps!=0,]
	nrow(est.caps) # Spot check: should be a lot less than 2212!

# Add a freshwater release interval
	# Make a loop to add fw release event if not released at Brewer or Verona
  	for(i in 1:nrow(est.caps)){
  		if((est.caps$ReleaseRKM[i]) > 45){
  			est.caps$FWRel[i] <- 1
  		} else {
  			est.caps$FWRel[i] <- 0
  		}
  	}	# i

	# Now give time-varying covariates the mean value for that release event
  	for(i in 1:nrow(est.caps)){
  	  est.caps$FWRel.1[i] <- est.caps[1, 22]
  	  est.caps$FWRel.2[i] <- est.caps[1, 33]
  	  est.caps$FWRel.3[i] <- est.caps[1, 44]
  	} # i

# Reorganize everything for book keeping
	est.caps <- est.caps[ ,c(1:11, (ncol(est.caps)-3), 12:22, (ncol(est.caps)-2),
		23:33, (ncol(est.caps)-1), 34:44, (ncol(est.caps)), 45:(ncol(est.caps)-4))]

# Create a variable for BINARY ORIGIN
	for(i in 1:nrow(est.caps)){
		if(est.caps$Origin[i] == "Wild"){
			est.caps$BinaryOrigin[i] <- 0
		} else {
			est.caps$BinaryOrigin[i] <- 1
		}
	}	# i

# Now keep only the data from the capture history that is needed for analysis
	est.caps <- est.caps[ , c(12:(ncol(est.caps)-1),1:11, ncol(est.caps))]

# Write to a datafile, either as .csv format or as .rda
  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary analysis/ProcessedData")
  write.table(est.caps, "EstuarySurvivalCaptureHistory.csv", sep=",",
  	row.names=FALSE)
# Save as r-data file ('EstuarySurvivalCaptureHistory.rda')
  save(est.caps, file = "EstuarySurvivalCaptureHistory.rda")
################################################################################

################################################################################
# Finish creating and standardizing individual covariates and make the data for
# the survival analysis
#
# Create estuary INP file for survival analysis in RMARK and GOF in MARK
#
# INPUT FILENAME(S):  'EstuarySurvivalCaptureHistory.rda' (or .csv),
#                      est.caps df'estData.inp' (MARK input file-created inline)
#
# MY FUNCTIONS:       'standardize' created and used
# NAMED OUPUT:        'est.data' (df) 'History' (vector), 'est.caps' (df)
# OUTPUT FILENAME(S): 'EstuarySurvivalCaptureHistory.rda' (and .csv)
#                     'estData.inp' (MARK input file)
################################################################################
# Install and load necessary packages
  #install.packages("RMark") # Uncomment start of line to run
	#install.packages("snow") # Uncomment start of line to run
	#install.packages("reshape") # Uncomment start of line to run
	#install.packages("reshape2") # Uncomment start of line to run
	#install.packages("plotrix") # Uncomment start of line to run
  require(RMark)
	require(snow)
	require(reshape)
	require(reshape2)
	require(plotrix)
	require(matrixStats)
	require(lubridate)
	require(geosphere)

# Load the estuary capture histories (est.caps df)
	setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary analysis/ProcessedData")
	load("EstuarySurvivalCaptureHistory.rda") #	'EstuarySurvivalCaptureHistory.rda'
  est.caps <- as.data.frame(est.caps)

# On second thought, remove the columns for covariates in FW Release interval,
# they are not necessary
  est.caps <- est.caps[, -c(13,25,37)]

# Create a vector for the interval lengths for input during process.data in
# Rmark code, UNCOMMENT THE FOLLOWING LINES TO GET PER-KM SURVIVAL RATES
	# Get the first 11 colnames of the capture history
	  intervals <- names(est.caps)[1:12]
	# Split by the underscore, "_", to get RKM
  	intervaldf <- colsplit(intervals, "_", names = c("rkm","loc"))
	# Change the first rkm to 45 (VZ)
	  intervaldf[1,1] <- 45
	  intervaldf
	# Make for loop to calculate interval lengths
	  # Define an empty vector to hold results
  	  interval.length <- c()
	    intervaldf$rkm <- as.numeric(intervaldf$rkm)
	  # Fill the empty vector with interval lenghts (these are km, not time!)
	    for(i in 2: nrow(intervaldf)){
	    	interval.length[i] <- intervaldf[(i-1), 1]-intervaldf[i, 1]
	    }
	# Remove the n/a value in the first element of the vector, replace with one
	  interval.length<- interval.length[2:length(interval.length)]
	# Data check
	  interval.length
#   # Get final interval lengths
	  interval.length2 <- cumsum(interval.length)
 	  interval.names <- c(interval.length2[1:(length(interval.length2))])
    interval.names

# Create a vector holding the actual capture history (just ones and zeros)
  History<-do.call(paste, c(est.caps[ , c(1:12)], sep=""))
	head(History) # Data check

# Add individual covariates to the INP file
	hists <- data.frame(History, est.caps[ , c(44:46,50:ncol(est.caps))])
	# OR...WITH TIME-VARYING COVARIATES
	hists <- data.frame(History, est.caps[ , c(13:45, 47:49,53:ncol(est.caps))])
  head(hists) # Spot check the first few rows of data

# Format the capture history with covariates for MARK
	ForInpFile <- data.frame(hists[ , 1], 1, hists[ , 2:ncol(hists)], ";")
  names(ForInpFile)

# Create a vector holding the covariate names
	covs <- names(est.caps)[c(13:45, 47:49,53:ncol(est.caps))]
  covs  # Data check

# Need to set a wd for working within RMark because it automates a lot of the
# file-writing, most will be automated from here out to speed things up...
	setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/Analysis")
# Write the estuary capture history to an INP file with correct formatting
	write.table(ForInpFile, "estData.inp", row.names=FALSE, col.names=FALSE,
		quote=FALSE)

# Read in the data that will be used for MARK
  # First, setwd
	  setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/Analysis")
  # Read capture history that is now MARK-friendly
  	est.data <- convert.inp('estData.inp', covariates = covs)
	  head(est.data) # Spot check the input data

	# Make the names shorter for individual covariates
	  names(est.data)[c(36:ncol(est.data))] <- c("FL", "Mass", "ATP", "Year",
	  	"SW", "RelKM", "Dams", "Origin")

	# Create a variable for Photoperiod and Temperature squared and add them
	  PhS <- matrix(ncol=11, nrow=nrow(est.data), est.data[,25:25]^2)
	  TS  <- matrix(ncol=11, nrow=nrow(est.data), est.data[,3:13]^2)
	  est.data <- data.frame(est.data[, c(1:13)], TS, est.data[, c(14:35)], PhS,
	  	est.data[ , c(36:ncol(est.data))])

	# Make names for time-varying individual covariates
    names(est.data)[3:57] <- paste(c(rep("Tc", 11), rep("TcS", 11),
    	rep("Pr", 11), rep("Ph", 11), rep("PhS", 11)),
	    rep(c(0,interval.names[1:(length(interval.names)-1)]),5), sep="")

	# Create a factor variable for tagtype
	  for(i in 1:nrow(est.data)){
	  	if((est.data$RelKM[i] == 187 | est.data$RelKM[i] ==162) &
	  			est.data$Year[i] == 2011){
	  		est.data$TagType[i]  <- 0
	  	} else {
	  		est.data$TagType[i] <- 1
	  	}
	  }
		for(i in 1:nrow(est.data)){
	  	if((est.data$RelKM[i] == 43.5) & est.data$Year[i] == 2005){
	  		est.data$TagType[i]  <- 0
	  	} else {
	  		next;
	  	}
	  }

	# Create a variable for Fulton Condition factor (k)
	  est.data$k <- (est.data$Mass/(est.data$FL^3))*100000

	# Create a tag x year group structure for release GOF check
	  est.data$group <- paste(est.data$Year, est.data$TagType, est.data$RelKM,
	  	sep="")
	  est.data$group <- as.factor(est.data$group)

	# Make year into a factor
	  est.data$Year <- as.factor(est.data$Year)

	# Create a covariate for movement rate
	# Get TagIDs
	  est.data$TagID <- as.character(est.caps$TagID)
	# Read in the median movement rates
    mvrt <- read.csv('MedianMovementRates.csv')
	# Convert TagID to var char
    mvrt$TagID <- as.character(mvrt$TagID)
	# Get Median movement rate for each fish
		pb <- txtProgressBar(min = 0, max = nrow(est.data), style = 3)
	  for(i in 1:nrow(est.data)){
      Sys.sleep (0.001)
	  	for(t in 1:nrow(mvrt)){
	  	  if(est.data$TagID[i]==unique(mvrt[t,2])){
	  		  est.data$Mvmt[i] <- mvrt[t, 3]
	  	  } else {
	  	  	next;
	  	  }
	  	}
	  	if(is.na(est.data$Mvmt[i])){
	  		est.data$Mvmt[i] <- mean(mvrt[ , 2])
	  	} else {
	  		next;
	  	}
	  }
	  close(pb)

  # Take the Brewer and Verona Fish out of the survival analysis
		est.data <- est.data[est.data$RelKM!=9.5 & est.data$RelKM!=43.5,]

# Save a real-scale version of all covariates
	est.data2 <- est.data

# Standardize individual covariates	in the df for analysis
  # Create fxn to standardize covariates
  	standardize=function(x,z=NULL){
      if(is.null(z)){
        return((x-mean(x))/sqrt(var(x)))
      } else {
          return((x-mean(z))/sqrt(var(z)))
        }
    }

  # Standardize individual covariates
   	est.data[ , c(58:60, 62:67, ncol(est.data))] <-
		  apply(est.data[ , c(58:60, 62:67, ncol(est.data))], 2, standardize)

  # Standardize temperature and temperature squared
    # Temperature
	    meanTc <- mean(as.matrix(est.data[,3:13]))
	    sdTc <- sd(as.matrix(est.data[,3:13]))
      for(i in 1:nrow(est.data)){
        for(t in 3:13){
          est.data[i, t] <- (est.data[i,t]-meanTc)/sdTc
        }
      }
    # Temperature squared
	    meanTcS <- mean(as.matrix(est.data[,14:24]))
	    sdTcS <- sd(as.matrix(est.data[,14:24]))
      for(i in 1:nrow(est.data)){
        for(t in 14:24){
        	est.data[i, t] <- (est.data[i,t]-meanTcS)/sdTcS
        }
      }

	# Standardize precipitation
    # Precipitation
	    meanPr <- mean(as.matrix(est.data[,25:35]))
		  sdPr <- sd(as.matrix(est.data[,25:35]))
      for(i in 1:nrow(est.data)){
        for(t in 25:35){
          est.data[i, t] <- (est.data[i,t]-meanPr)/sdPr
        }
      }

	# Standardize photoperiod and photoperiod squared
    # Photoperiod
	    meanPh <- mean(as.matrix(est.data[,36:46]))
		  sdPh <- sd(as.matrix(est.data[,36:46]))
      for(i in 1:nrow(est.data)){
        for(t in 36:46){
          est.data[i, t] <- (est.data[i,t]-meanPh)/sdPh
        }
      }
    # Photoperiod squared
	    meanPhS <-mean(as.matrix(est.data[,47:57]))
		  sdPhS <- sd(as.matrix(est.data[,47:57]))
      for(i in 1:nrow(est.data)){
        for(t in 47:57){
          est.data[i, t] <- (est.data[i, t]-meanPhS)/sdPhS
        }
      }

# Go back and get flow for the survival data
  # Read in Flow data ('WestEnfieldFlowDataClean.txt'), replace NA with 0 b/c ice
    Discharge <- read.table('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/WestEnfieldFlowDataClean.txt',
    	header=TRUE)[ , 3:4]
	  names(Discharge) <- c('Date', 'Discharge')
	  Discharge[ , 2] <- as.character(Discharge[ , 2])
	  for(i in 1:nrow(Discharge)){
		  if(Discharge[i , 2]=='Ice'){
	      Discharge[i , 2] <- '0'
		  } else {
			  next;
		  }
	  }
	  Discharge[ , 2] <- as.numeric(Discharge[ , 2])

    # Make a vector to hold the year for each temperature measurement
      Discharge$Year <- year(as.POSIXct(Discharge[,1]))
	  # Change discharge date to character
	    Discharge$Date <- as.character(Discharge$Date)

  # Get discharge for each day
	  # Get matrix of timestamps for fish that are in est.data
      NewDateTime <- Date[Date$TagID %in% est.data$TagID,]

	  # Now keep only the sites that are in est.data
	    NewDateTime2 <- NewDateTime[ , names(NewDateTime) %in% names(est.caps)]

	  # Get matrix of dates for fish that are in est.data
	    # Make a substring function to get dates inside apply fxn
	      stripDMY <- function(x){ substr(x, start=1, stop=10)}
	    # Now apply stripDMY to all dateTime cells in NewDateTime2
	      NewDates <- apply(NewDateTime2[, 2:ncol(NewDateTime2)], 2, stripDMY)

	  # Make est.data year into numeric
	    est.data$Year <- as.numeric(as.character(est.data$Year))

  # Calculate mean discharge for each fish following arrival in the estuary
  # Calculate mean discharge for each fish following arrival in the estuary
    pb <- txtProgressBar(min = 0, max = nrow(est.data), style = 3)
	  Dis <- c()
    for(i in 1:nrow(NewDates)){
      Sys.sleep(0.1)
        Dis[i] <- mean(
        	Discharge$Discharge[
        	Discharge$Year==est.data$Year[i] &
            (yday(Discharge$Date) > min(yday(NewDates[i, NewDates[i, ]!='1970-01-01'])) |
          	  yday(Discharge$Date)==min(yday(NewDates[i, NewDates[i, ]!='1970-01-01']))) &
            (yday(Discharge$Date) < max(yday(NewDates[i, NewDates[i, ]!='1970-01-01'])) |
          	  yday(Discharge$Date)==max(yday(NewDates[i, NewDates[i, ]!='1970-01-01'])))
          ])
        setTxtProgressBar(pb, i)
    } # i

	# Add it to the dataframe for analysis as well as real-scale df
	  est.data$Dis <- Dis
  	est.data2$Dis<- Dis

	# Standardize it
	  est.data$Dis <- standardize(est.data$Dis)

	# Convert year back to a factor
	  est.data$Year <- as.factor(est.data$Year)
################################################################################

################################################################################
# Prep data for goodness of fit, do quick GOF test in release (not very useful)
#
# INPUT FILENAME(S):
# MY FUNCTIONS:
# NAMED OUPUT:        'est.data' (df)
# OUTPUT FILENAME(S): 'DesignMatrix.csv' (input for MARK DM)
#                     'GOFdata.inp' (MARK input file for GOF)
################################################################################
# Do release gof just to check things out. Will get a good estimate of c-hat
# manually from the median c-hat procedure in program MARK using the output of
# the next section.
  # Process data
		est.processed=process.data(est.data, groups="group",
			time.intervals=interval.length[1:length(interval.length)],
    	begin.time=2.5)
	# Do RELEASE GOF (not really that useful...it's always a high estimate)
	  release.gof(est.processed)

# Create a table of histories and years for output to MARK GOF File
  GOFdata <- data.frame(est.data$ch, est.processed$freq, ";")
  names(GOFdata) <- c("ch", "year")
  # Dummy code the year groups using nested if-else
  # Also, add '1' at the start of each group for freq and semicolon at line
	# breaks
    for(i in 1:nrow(GOFdata)){
      if(GOFdata$year[i]==2005){
      	GOFdata$group[i]<-"1 0 0 0 0 0 0;"
      } else {
        if(GOFdata$year[i]==2006){
      	  GOFdata$group[i]<-"0 1 0 0 0 0 0;"
        }	else {
        	if(GOFdata$year[i]==2009){
      	    GOFdata$group[i]<-"0 0 1 0 0 0 0;"
        	} else {
        		if(GOFdata$year[i]==2010){
      	      GOFdata$group[i]<-"0 0 0 1 0 0 0;"
         		} else {
        			if(GOFdata$year[i]==2011){
       	        GOFdata$group[i]<-"0 0 0 0 1 0 0;"
        	    } else {
        			  if(GOFdata$year[i]==2012){
      	          GOFdata$group[i]<-"0 0 0 0 0 1 0;"
        		    } else {
        			    if(GOFdata$year[i]==2013){
      	            GOFdata$group[i]<-"0 0 0 0 0 0 1;"
        		      } else {
        		      	next;
        		      }
        		    }
        	    }
            }
        	}
        }
      }
    } # i loop

  # Keep only the ch and the freq/group/semicolon column
    GOFdata <- GOFdata[c(1,3)]
    head(GOFdata) # Data check

  # Write the GOF data to a file
  write.table(GOFdata, "GOFdata.inp", row.names=FALSE, col.names=FALSE,
		quote=FALSE)

# Export the design matrix for our fave model to make the MARK less painful
	# Process data
	  est.processed=process.data(est.data, groups="Year",
			time.intervals=interval.length[1:length(interval.length)],
      begin.time=2.5)
    # Create default design data for CJS model
      est.ddl=make.design.data(est.processed)
	  # Define the parameters for an additive model
	    Phi.tY=list(formula=~time+Year)
	    p.tY=list(formula=~time+Year)
	  # Create the model, but don't run it
      model=make.mark.model(est.processed, est.ddl,
        parameters=list(Phi=Phi.tY, p=p.tY))
	# Export the design matrix, run the full model in MARK to get Medium C-hat
    designdata <- model$simplify$design.matrix
	  write.table(designdata, file='DesignMatrix.csv', row.names=F, col.names=F,
	   quote=F)
################################################################################

################################################################################
# Run candidate model structures with regard to temporal and spatial variation
# in survival and detection, as well as year effects and covariates on detection
#
# INPUT FILENAME(S):  'estData.inp' (MARK input file)
# MY FUNCTIONS:       'run.CandDets' created and used
# NAMED OUPUT:        'DetRanks' (df)
# OUTPUT FILENAME(S): 'DetRanking.csv' (Model-seleciton table)
################################################################################
# Run a set of candidate model structures to determine whether or not Release
# site and tag type are important for detection, and if survival varies between
# years and reaches
	  run.CandDets <- function(){
	# Process data
		est.processed=process.data(est.data, groups="Year",
			time.intervals=interval.length[1:length(interval.length)],
    	begin.time=0)

  # create default design data for CJS model
    est.ddl=make.design.data(est.processed)

    # Define range of models to be tested, in our case define Phi and p
	    # Parameterizations for Phi
	  	  Phi.t=list(formula=~ time)
	  	  Phi.y=list(formual=~ Year)
	  	  Phi.t.y=list(formula=~ time + Year)
        Phi.phS=list(formula=~ Ph + PhS)
	  	  Phi.t.phS=list(formula=~ time + Ph + PhS)
	  	  Phi.y.phS=list(formual=~ Year + Ph + PhS)
	  	  Phi.t.y.phS=list(formula=~ time + Year + Ph + PhS)
	  	  Phi.ph=list(formula=~ Ph)
	  	  Phi.t.ph=list(formula=~ time + Ph)
	  	  Phi.y.ph=list(formual=~ Year + Ph)
	  	  Phi.t.y.ph=list(formula=~ time + Year + Ph)

     	# Parameterizations for p
        p.t.y=list(formula=~ time + Year)
  	    p.t.y.TT.Dis=list(formula=~ time + Year + TagType + Dis)
  	    p.t.y.Dis=list(formula=~ time +Year + Dis)
  	    p.t.y.TT=list(formula=~ time + Year + TagType)
  	    #p.t.y.TT.Dis.Mvmt=list(formula=~ time + Year + TagType + Dis + Mvmt)
  	    #p.t.y.TT.Mvmt=list(formula=~ time + Year + TagType + Mvmt)
  	    #p.t.y.Dis=list(formula=~ time + Year + Mvmt)

		# Create the list of models to run
	  	model.list <- create.model.list("CJS")

		# Run the models and package the results
	    mark.wrapper(model.list, data=est.processed, ddl=est.ddl)
	}

  # Run the models
    CandDets <- run.CandDets()

	# Create a dataframe of model-selection statistics
	  DetRanks <- data.frame(model.table(adjust.chat(chat=1.26, CandDets)))
	  DetRanks
	# Create an object of the best detection model formula
	  BestDets <- as.character(DetRanks[1,2])
	  BestDets

	# Write the model-selection table to a file
		write.table(DetRanks, "DetRanking.csv", row.names=FALSE, col.names=FALSE,
	  	quote=FALSE, sep=",")

# # Do an a posteriori check to make sure that movement rate should not have been
# # accounted for in the best detection model!!
# 	run.CandDets2 <- function(){
# 	# Process data
# 		est.processed2=process.data(est.data, groups="Year",
# 			time.intervals=interval.length[1:length(interval.length)],
#     	begin.time=0)
#   # create default design data for CJS model
#     est.ddl2=make.design.data(est.processed2)
#     # Define range of models to be tested, in our case define Phi and p
# 		  # Parameterizations for Phi
# 	  	  Phi.t.y.phS=list(formula=~ time + Year + Ph + PhS)
#       # Parameterizations for p
#   	    p.t.y.TT.rkm=list(formula=~ time + Year + TagType + RelKM)
#   	    p.t.y.TT.mvmt=list(formula=~ time + Year + TagType + Mvmt)
# 		# Create the list of models to run
# 	  	model.list <- create.model.list("CJS")
# 		# Run the models and package the results
# 	    mark.wrapper(model.list, data=est.processed2, ddl=est.ddl2)
# 	}
#
#   # Run the models
#     CandDets2 <- run.CandDets2()
# 	# Create a dataframe of model-selection statistics
# 	  DetRanks2 <- data.frame(model.table(adjust.chat(chat=1.26, CandDets2)))
# 	  DetRanks2
# 	# Create an object of the best detection model formula
# 	  BestDets2 <- as.character(DetRanks2[1,2])
# 	  BestDets2
# 	# Write the model-selection table to a file
# 		write.table(DetRanks2, "DetRankingCHECK.csv", row.names=FALSE, col.names=FALSE,
# 	  	quote=FALSE, sep=",")
################################################################################

################################################################################
# Run models that include individual covariates for fish characteristics and
# migratory history (Poseidon models)
#
# INPUT FILENAME(S):  'estData.inp' (MARK input file)
# MY FUNCTIONS:       'run.Covariates' created and used
# NAMED OUPUT:        'CovModsBig', CovBigRanks' (both are dfs)
# OUTPUT FILENAME(S): 'CovModsRanking.csv' (Model-seleciton table)
################################################################################
# MUST RUN THE 'CandYearTime' FUNCTION ABOVE FIRST!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Set working directory
	setwd("C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/Analysis")

# Change the name of the Origin variable to match formulas
	est.data$Org <- est.data$Origin

# Change movement rate to km/h
	est.data$Mvmt <- (est.data2$Mvmt*est.data2$FL)/(60*60)
	est.data2$Mvmt <- est.data$Mvmt
	est.data$Mvmt <- scale(est.data$Mvmt)

# Create a function to run the candidate covariate model set
	run.Covariates <- function(){
	# Process data
		est.processed=process.data(est.data, groups="Year",
			time.intervals=interval.length[1:length(interval.length)],
    	begin.time=0)

  # create default design data for CJS model
    est.ddl=make.design.data(est.processed)

    # Define range of models to be tested, in this case define Phi and p
	    # Parameterizations for Phi
        Phi.t.Y.PhS.FL.=list(formula=~time + Year + Ph + PhS + FL)
        Phi.t.Y.PhS.k.=list(formula=~time + Year + Ph + PhS + k)
        Phi.t.Y.PhS.ATP.=list(formula=~time + Year + Ph + PhS + ATP)
        Phi.t.Y.PhS.Org.=list(formula=~time + Year + Ph + PhS + Org)
        Phi.t.Y.PhS.FL.SW.=list(formula=~time + Year + Ph + PhS + FL + SW)
        Phi.t.Y.PhS.k.SW.=list(formula=~time + Year + Ph + PhS + k + SW)
        Phi.t.Y.PhS.ATP.SW.=list(formula=~time + Year + Ph + PhS + ATP + SW)
        Phi.t.Y.PhS.Org.SW.=list(formula=~time + Year + Ph + PhS + Org + SW)
        Phi.t.Y.PhS.SW.=list(formula=~time + Year + Ph + PhS + SW)
        Phi.t.Y.PhS.Dams.=list(formula=~time + Year + Ph + PhS + Dams)
        Phi.t.Y.PhS.FL.Dams.=list(formula=~time + Year + Ph + PhS + FL + Dams)
        Phi.t.Y.PhS.k.Dams.=list(formula=~time + Year + Ph + PhS + k + Dams)
        Phi.t.Y.PhS.ATP.Dams.=list(formula=~time + Year + Ph + PhS + ATP + Dams)
        Phi.t.Y.PhS.Org.Dams.=list(formula=~time + Year + Ph + PhS + Org + Dams)
        Phi.t.Y.PhS.RelKM.=list(formula=~time + Year + Ph + PhS + RelKM)
        Phi.t.Y.PhS.FL.RelKM.=list(formula=~time + Year + Ph + PhS + FL + RelKM)
        Phi.t.Y.PhS.k.RelKM.=list(formula=~time + Year + Ph + PhS + k + RelKM)
        Phi.t.Y.PhS.ATP.RelKM.=list(formula=~time + Year + Ph + PhS + ATP + RelKM)
        Phi.t.Y.PhS.Org.RelKM.=list(formula=~time + Year + Ph + PhS + Org + RelKM)
        Phi.t.Y.PhS.Mvmt.=list(formula=~time + Year + Ph + PhS + Mvmt)
        Phi.t.Y.PhS.FL.Mvmt.=list(formula=~time + Year + Ph + PhS + FL + Mvmt)
        Phi.t.Y.PhS.k.Mvmt.=list(formula=~time + Year + Ph + PhS + k + Mvmt)
        Phi.t.Y.PhS.ATP.Mvmt.=list(formula=~time + Year + Ph + PhS + ATP + Mvmt)
        Phi.t.Y.PhS.Org.Mvmt.=list(formula=~time + Year + Ph + PhS + Org + Mvmt)
        Phi.t.Y.PhS.FL.SW.Mvmt.=list(formula=~time + Year + Ph + PhS + FL + SW + Mvmt)
        Phi.t.Y.PhS.k.SW.Mvmt.=list(formula=~time + Year + Ph + PhS + k + SW + Mvmt)
        Phi.t.Y.PhS.ATP.SW.Mvmt.=list(formula=~time + Year + Ph + PhS + ATP + SW + Mvmt)
        Phi.t.Y.PhS.Org.SW.Mvmt.=list(formula=~time + Year + Ph + PhS + Org + SW + Mvmt)
        Phi.t.Y.PhS.SW.Mvmt.=list(formula=~time + Year + Ph + PhS + SW + Mvmt)
        Phi.t.Y.PhS.Dams.Mvmt.=list(formula=~time + Year + Ph + PhS + Dams + Mvmt)
        Phi.t.Y.PhS.FL.Dams.Mvmt.=list(formula=~time + Year + Ph + PhS + FL + Dams + Mvmt)
        Phi.t.Y.PhS.k.Dams.Mvmt.=list(formula=~time + Year + Ph + PhS + k + Dams + Mvmt)
        Phi.t.Y.PhS.ATP.Dams.Mvmt.=list(formula=~time + Year + Ph + PhS + ATP + Dams + Mvmt)
        Phi.t.Y.PhS.Org.Dams.Mvmt.=list(formula=~time + Year + Ph + PhS + Org + Dams + Mvmt)
        Phi.t.Y.PhS.RelKM.Mvmt.=list(formula=~time + Year + Ph + PhS + RelKM + Mvmt)
        Phi.t.Y.PhS.FL.RelKM.Mvmt.=list(formula=~time + Year + Ph + PhS + FL + RelKM + Mvmt)
        Phi.t.Y.PhS.k.RelKM.Mvmt.=list(formula=~time + Year + Ph + PhS + k + RelKM + Mvmt)
        Phi.t.Y.PhS.ATP.RelKM.Mvmt.=list(formula=~time + Year + Ph + PhS + ATP + RelKM + Mvmt)
        Phi.t.Y.PhS.Org.RelKM.Mvmt.=list(formula=~time + Year + Ph + PhS + Org + RelKM + Mvmt)

		# Parameterizations for p
      p.BEST=list(formula=~ time + Year + TagType + Dis)

		# Create the list of models to run
	  	model.list <- create.model.list("CJS")

		# Run the models and package the results
	    mark.wrapper(model.list, data=est.processed, ddl=est.ddl)
	}

  # Run the models
    CovModsBig <- run.Covariates()

  # View results of model ranking
    #print(CovModsBig)
	  CovModsAdjusted <- adjust.chat(chat=1.2657, CovModsBig)
	  CovModsAdjusted
	  CovRanks <- data.frame(model.table(CovModsAdjusted))
		write.table(CovRanks, "CovModsRanking.csv", row.names=FALSE,
			col.names=TRUE, quote=FALSE, sep=",")

  # Get rid of the pile of MARK files that just got dumped on the PC
  # DO NOT RUN THIS COMMAND IF YOU WANT TO SAVE THE FILES OF THE FINAL MODEL
  # RUN.  THIS LINE IS ONLY TO CLEAN UP JUNK FILES CREATED DURING CODE DEBUGGING
    #cleanup(ask=FALSE, prefix = "mark")
################################################################################

################################################################################
# Plot the coefficients of covariates included in the best survival model
#
# INPUT FILENAME(S):
# MY FUNCTIONS:
# NAMED OUPUT:
# OUTPUT FILENAME(S): 'Figure2...Figure4'
################################################################################
# Install necessary packages
	#install.packages('plotrix')
	#install.packages('geosphere')
	require(plotrix) # Uncomment to install package
	require(geosphere) # Uncomment to install package

# Get the betas from the best model for plotting
  betas <- data.frame(
  	CovModsAdjusted$Phi.t.Y.PhS.ATP.Dams.Mvmt..p.BEST$results$beta
  )
  betas

# Get the probability-scale estimates of apparent survival
	reals <- data.frame(
  	CovModsAdjusted$Phi.t.Y.PhS.ATP.Dams.Mvmt..p.BEST$results$real[c(1:77), ])
  reals

# # Plot point estimates of mean and 95% CL for apparent survival in the estuary
# # for each year
# 	# Select and/or create a .tiff file to which the plot will be written
#   	tiff(file = tclvalue(tcl("tk_getSaveFile")), width=6000, height=5000,
# 	    pointsize=18, compression="lzw", res=500)
# 	# Get reach lengths for each year
# 	  lengths <- interval.length[1:10]
# 	# Set graphical parameters
# 	  par(mar=c(8,6,1,1))
# 	# 2005 survival estimates
#     plotCI(exp(-(1-reals[1:10,1])*lengths),
#   	  liw=c(exp(-(1-reals[1:10,1])*lengths)-exp(-(1-reals[1:10,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[1:10,4])*lengths)-exp(-(1-reals[1:10,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=21, pt.bg="black",
#     	cex=1.5, xaxt='n')
# 	# 2006 survival estimates
# 	  par(new=TRUE)
# 	  plotCI(exp(-(1-reals[12:21,1])*lengths),
#   	  liw=c(exp(-(1-reals[12:21,1])*lengths)-exp(-(1-reals[12:21,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[12:21,4])*lengths)-exp(-(1-reals[12:21,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=21, pt.bg="white",
#     	cex=1.5, xaxt='n')
# 	# 2009 survival estimates
# 		par(new=TRUE)
#     plotCI(exp(-(1-reals[23:32,1])*lengths),
#   	  liw=c(exp(-(1-reals[23:32,1])*lengths)-exp(-(1-reals[23:32,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[23:32,4])*lengths)-exp(-(1-reals[23:32,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=21, pt.bg="grey40",
#     	cex=1.5, xaxt='n')
# 	# 2010 survival estimates
# 	  par(new=TRUE)
# 	  plotCI(exp(-(1-reals[34:43,1])*lengths),
#    	  liw=c(exp(-(1-reals[34:43,1])*lengths)-exp(-(1-reals[34:43,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[34:43,4])*lengths)-exp(-(1-reals[34:43,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=24, pt.bg="black",
#     	cex=1.5, xaxt='n')
# 	# 2011 survival estimates
# 	  par(new=TRUE)
# 	  plotCI(exp(-(1-reals[45:54,1])*lengths),
#    	  liw=c(exp(-(1-reals[45:54,1])*lengths)-exp(-(1-reals[45:54,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[45:54,4])*lengths)-exp(-(1-reals[45:54,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=24, pt.bg="white",
#     	cex=1.5, xaxt='n')
# 	# 2012 survival estimates
# 	  par(new=TRUE)
# 	  plotCI(exp(-(1-reals[56:65,1])*lengths),
#    	  liw=c(exp(-(1-reals[56:65,1])*lengths)-exp(-(1-reals[56:65,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[56:65,4])*lengths)-exp(-(1-reals[56:65,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=24, pt.bg="gray40",
#     	cex=1.5, xaxt='n')
# 	# 2013 survival estimates
# 	  par(new=TRUE)
# 	  plotCI(exp(-(1-reals[67:76,1])*lengths),
#    	  liw=c(exp(-(1-reals[67:76,1])*lengths)-exp(-(1-reals[67:76,3])*lengths)),
#   	  uiw=c(exp(-(1-reals[67:76,4])*lengths)-exp(-(1-reals[67:76,1])*lengths)),
#   	  ylim=c(0.70, 1.00), ylab='', xlab='', yaxt='n', pch=22, pt.bg="black",
#     	cex=1.5, xaxt='n')
# 	# Axes, labels, and legends
# 	  # Axes
# 	    axis(side=1, at=c(seq(1,10,1)), cex.axis=1,
# 		    c(as.character(paste(round(intervaldf$rkm[1:10]),
# 		    c(round(intervaldf$rkm[2:10])," -4"), sep="\226"))))
# 	    axis.break(axis=2, style='slash', brw=.03, pos=0.71)
#       axis(side=2, at=c(seq(0.75, 1.00, 0.05)), c("0.75","0.80", "0.85",
#         "0.90", "0.95", "1.00"), cex.axis=1, las=2)
# 	  # Labels
# 	    mtext(side=1, c("Upstream","Downstream"), adj=c(0,1), line=3, cex=1.6)
# 	    mtext(side=1, "Reach rkm (from mouth of estuary)", line=6,
# 	    	cex=2)
# 	    mtext(side=2, expression(paste("Apparent survival ( ",Phi, " )")),
# 	      line=4, cex=2, xaxt='n')
# 	  # Legend
# 	    legend(pt.bg=c("black", "white", "gray40", "black", "white", "gray40",
# 		    "black"), pch=c(rep(21,3), rep(24,3), 22), pt.cex=1.5, bty='i',
# 		    legend=c("2005","2006", as.character(seq(2009,2013,1))), x=1, y=0.90 )
# 	 # Turn of the graphics device and save/update the file
#      dev.off()

# Plot the covariate predictions for the best model
  # Calculate effect of Photoperiod
	  # Calculate a new range of standardized values for survival that can be used
	  # to calculate predicted survival
    # Get minimum and maximum of the standardized covariate
      minPh <- min(as.matrix(est.data[ ,36:46]))
      maxPh <- max(as.matrix(est.data[ ,36:46]))
	  # Calculate new vector of photoperiods from which survival can be
	  # predicted
    	newPh <- seq(minPh, maxPh, 0.01)
	  # Make a vector of back-standardized values for plotting
  	  RnewPh <- newPh*sdPh + meanPh
	  # Estimate predicted values of survival from photoperiod
       newSp <- exp(betas[18,1]*newPh+betas[19,1]*(newPh^2))/
	  	   (1+exp(betas[18,1]*newPh+betas[19,1]*(newPh^2)))
	     newSpupper <- exp(betas[18,4]*newPh+betas[19,4]*(newPh^2))/
		     (1+exp(betas[18,4]*newPh+betas[19,4]*(newPh^2)))
	     newSplower <- exp(betas[18,3]*newPh+betas[19,3]*(newPh^2))/
		     (1+exp(betas[18,3]*newPh+betas[19,3]*(newPh^2)))
  # Calculate effects of dams on survival
    # Get minimum and maximum of the standardized covariate
      minD <- min(est.data$Dams)
      maxD <- max(est.data$Dams)
    # Calculate new vector of n.dams from which survival can be predicted
      newD <- seq(-2, maxD, 0.01)
    # Calculate a vector of non-standardized values for dams
  	  RnewD <- newD*sd(est.data2$Dams)+ mean(est.data2$Dams)
    # Predict values of survival based on dams
      newSd <- exp(betas[21,1]*newD)/(1 + exp(betas[21,1]*newD))
      newDupper <- exp(betas[21,4]*newD)/(1 + exp(betas[21,4]*newD))
      newDlower <- exp(betas[21,3]*newD)/(1 + exp(betas[21,3]*newD))
  # Calculate effects of ATPase on survival
    # Get minimum and maximum of the standardized covariate
      minA <- min(est.data$ATP)
      maxA <- max(est.data$ATP)
    # Calculate new vector of n.ATP from which survival can be predicted
      newA <- seq(minA, maxA, 0.01)
    # Calculate a vector of non-standardized values for ATP
  	  RnewA <- newA*sd(est.data2$ATP)+ mean(est.data2$ATP)
    # Predict values of survival based on ATP
      newSA <- exp(betas[20,1]*newA)/(1 + exp(betas[20,1]*newA))
      newSAupper <- exp(betas[20,4]*newA)/(1 + exp(betas[20,4]*newA))
      newSAlower <- exp(betas[20,3]*newA)/(1 + exp(betas[20,3]*newA))
	 # Plot effects of linear covariates
	   # Save the plot as a tiff
	     tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Figures & Tables/Figure5.tif',
	     	 width=6000, height=10000,
	       pointsize=18, compression="lzw", res=500)
	   # Effects of photoperiod (timing) on survival
  	   par(mar=c(5,10,6,1), par(mfrow=c(3, 1)))
  	   plot(RnewPh, newSp, type="l", ylim=c(0,1), lwd=3, xlab='',
  	   	 xlim=c(13.7,15.6), ylab='', cex.lab=3, cex.axis=2, yaxt='n')
       par(new=TRUE)
   	   plot(RnewPh, newSpupper, type="l", ylim=c(0,1), lwd=3, xlab='',
  		   xlim=c(13.7,15.6), ylab="", col='gray40', lty=2, cex.axis=2,
  	   	 yaxt='n')
	     par(new=TRUE)
	     plot(RnewPh, newSplower, type="l", ylim=c(0,1), lwd=3, xlab='',
  		   xlim=c(13.7,15.6), ylab="", col='gray40', lty=2, cex.axis=2,
  	   	 yaxt='n')
	     mtext(side=2, expression(paste("Apparent survival (", phi, ")")),
	     	 line=5, cex=3, adj=3)
	     mtext(side=1, expression(paste("Photoperiod (hours)")),
	     	 line=5, cex=2)
		   mtext(side=3, expression(paste("Date")),
	     	 line=3.7, cex=2)
       axis(side=2, at=c(seq(0.20, 1.00, 0.20)), c("0.20", "0.40",
       	 "0.60", "0.80", "1.00"), cex.axis=2, las=2)
	     axis(side=3, at=c( daylength(44.821614, c(100, 110, 120, 130, 140, 161))),
	     	c("Apr 10", "Apr 20", "Apr 30", "May 10", "May 20",
	    	"Jun 10"), cex.axis=2)
	     axis.break(axis=2, style='slash', brw=.02)
	     text(x=13.7, y=.95, '(a)', cex=2)
	   # Effects of dams on survival
  	   par(mar=c(6,10,4,1))
  	   plot(RnewD, newSd, type="l", ylim=c(0,1), lwd=3, ylab='', cex.lab=3,
  	 	   xlim=c(0,10), cex.axis=2, xlab='', yaxt='n')
       par(new=TRUE)
   	   plot(RnewD, newDupper, type="l", ylim=c(0,1), lwd=3, xlab='',
  		   xlim=c(0,10), ylab='', col='gray40', lty=2, cex.axis=2, yaxt='n')
	     par(new=TRUE)
	     plot(RnewD, newDlower, type="l", ylim=c(0,1), lwd=3, xlab='',
  		   xlim=c(0,10), ylab='', col='gray40', lty=2, cex.axis=2, yaxt='n')
	     mtext(side=1, 'Number of dams passed', line=5, cex=2)
	     axis(side=2, at=c(seq(0.20, 1.00, 0.20)), c("0.20", "0.40",
       	 "0.60", "0.80", "1.00"), cex.axis=2, las=2)
	     axis.break(axis=2, style='slash', brw=.02)
		   text(x=0, y=.95, '(b)', cex=2)
	   # Effects of ATPase activity
  	   par(mar=c(6,10,4,1))
  	   plot(RnewA, newSA, type="l", ylim=c(0,1), lwd=3, xlab="",	xlim=c(0,14),
  	   	 cex.axis=2, ylab='', yaxt='n')
       par(new=TRUE)
   	   plot(RnewA, newSAupper, type="l", ylim=c(0,1), lwd=3, xlab='',
  		   xlim=c(0,14), ylab="", col='gray40', lty=2, cex.axis=2, yaxt='n')
	     par(new=TRUE)
	     plot(RnewA, newSAlower, type="l", ylim=c(0,1), lwd=2, xlab='',
  		   xlim=c(0,14), ylab="", col='gray40', lty=2, cex.axis=2, yaxt='n')
       mtext('gill NKA activity', side=1, cex=2, line=4.3)
	     axis(side=2, at=c(seq(0.20, 1.00, 0.20)), c("0.20", "0.40",
       	 "0.60", "0.80", "1.00"), cex.axis=2, las=2)
	     axis.break(axis=2, style='slash', brw=.02)
			 text(x=0, y=.95, '(c)', cex=2)
	   # Turn off graphics
	     dev.off()

# Plot mean survival and detection across all years as a box plot
  # Get a df of estimated survival
    estimates <- reals[1:77, 1:4]
  # Get rid of the sloppy rownames
    row.names(estimates) <- NULL
  # Get groups (by year)
    groups <- substr(row.names(reals[1:77, ]), start=6, stop=10)
  # Get ending rkm for each interval
    interval <- rep(c(intervaldf[2:nrow(intervaldf) ,1]), 7)
  # Make estimates, groups, and intervals into a df
    estimates <- data.frame(groups, interval, estimates)
    estimates <- estimates[-c(11, 22, 33, 44, 55, 66, 77), ]

  # Make a boxplot of mean survival estimates by rkm
	  # Calculate effects of detection covariates in each reach
      detRes<-betas[23:nrow(betas),]
      detRes
	    # First do big tags
		    calc <- c()
		    calcUp <- c()
		    calcLow <- c()
		    for(i in 2:10){
		    	calc[i] <- exp(detRes[1,1]+detRes[i,1]+1.99+.404*(.2811))/
		    		(1+exp(detRes[1,1]+detRes[i,1]+1.99+.404*(.2811)))
		    	calcUp[i] <- exp(detRes[1,4]+detRes[i,4]+1.99+.404*(.2811))/
		    		(1+exp(detRes[1,4]+detRes[i,4]+1.99+.404*(.2811)))
		    	calcLow[i] <- exp(detRes[1,3]+detRes[i,3]+1.99+.404*(.2811))/
		    		(1+exp(detRes[1,3]+detRes[i,3]+1.99+.404*(.2811)))
		    }
		    calc[1]<-exp(detRes[1,1]+1.99+.404*(.2811))/
		      (1+exp(detRes[1,1]+1.99+.404*(.2811)))
		    calcUp[1] <- exp(detRes[1,4]+1.99+.404*(.2811))/
		    		(1+exp(detRes[1,4]+1.99+.404*(.2811)))
		    calcLow[1] <- exp(detRes[1,3]+1.99+.404*(.2811))/
		    		(1+exp(detRes[1,3]+1.99+.404*(.2811)))

	    # Next, do small tags
		    calc1 <- c()
		    calcUp1 <- c()
		    calcLow1 <- c()
		    for(i in 2:10){
		    	calc1[i] <- exp(detRes[1,1]+detRes[i,1]+1.99+.404*(-3.553))/
		    		(1+exp(detRes[1,1]+detRes[i,1]+1.99+.404*(-3.553)))
		    	calcUp1[i] <- exp(detRes[1,4]+detRes[i,4]+1.99+.404*(-3.553))/
		    		(1+exp(detRes[1,4]+detRes[i,4]+1.99+.404*(-3.553)))
		    	calcLow1[i] <- exp(detRes[1,3]+detRes[i,3]+1.99+.404*(-3.553))/
		    		(1+exp(detRes[1,3]+detRes[i,3]+1.99+.404*(-3.553)))
		    }
		    calc1[1]<-exp(detRes[1,1]+1.99+.404*(-3.553))/
		      (1+exp(detRes[1,1]+1.99+.404*(-3.553)))
		    calcUp1[1] <- exp(detRes[1,4]+1.99+.404*(-3.553))/
		    		(1+exp(detRes[1,4]+1.99+.404*(-3.553)))
		    calcLow1[1] <- exp(detRes[1,3]+1.99+.404*(-3.553))/
		    		(1+exp(detRes[1,3]+1.99+.404*(-3.553)))

	  # Select and/or create a .tiff file to which the plot will be written
   	  tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Figures & Tables/Figure4.tif',
   	  	width=6000, height=6000, pointsize=18, compression="lzw", res=500)
    # Create the plot
      par(mfrow=c(2,1), oma=c(6,6,.5,1))
	    # Plot mean detection probability for big tags
	      par(mar=c(1,1,1,1))
	      plotCI(x=seq(1,10,1), y=calc, uiw=c(calcUp-calc), liw=c(calc-calcLow),
	      	ylim=c(0,1), ylab='', xlab='', xaxt='n', yaxt='n', pch=21,
	      	pt.bg='white')
	    # Plot mean detection probability for small tags
	      par(new=TRUE)
	      plotCI(x=seq(1,10,1), y=calc1, uiw=c(calcUp1-calc1),
	      	liw=c(calc1-calcLow1),	pch=19, col='black', ylim=c(0,1), ylab='',
	      	xlab='', xaxt='n',	yaxt='n')
		    mtext(side=2, 'Detection probability (p)', line=4.5, cex=1.5)
		    axis(side=2, at=c(seq(0, 1.00, 0.2)), sprintf('%.2f', seq(0,1,.2)),
		    	cex.axis=1.25, las=2)
	      text(x=1, y=.95, '(a)', cex=1.25)
	    # Plot mean survival across years
	      par(mar=c(1,1,1,1))
	      boxplot(estimate~interval, ylab='', yaxt='n', xlim=c(10,1),	col='grey',
	    	  xlab = '', xaxt= 'n', boxwex=1.0, cex.axis=1.5, ylim=c(0.945, 1.00),
	    	  data=estimates, width=round(interval.length[10:1], 0))
	      axis(side=1, at=c(10:1), as.character(round(interval[1:10])),
	    	  cex.axis=1.25)
	      mtext(side=1, 'Upstream', adj=0, line=3, cex=1.25)
	      mtext(side=1, 'Downstream', adj=1, line=3, cex=1.25)
	      mtext(side=1, 'River kilometer (rkm)', line=5, cex=1.5)
	      axis(side=2, at=c(seq(0.95, 1.00, 0.01)), as.character(c(seq(0.95, .99,
	    	  0.01), '1.00')), cex.axis=1.5, las=2)
	      mtext(side=2, expression(paste('Apparent survival (', phi, ')',
	    	  plain('\u00b7'), 'km'^-1)), line=4.5, cex=1.5)
		    axis.break(axis=2, style="slash", brw=0.02)
	      text(x=10, y=.95, '(b)', cex=1.25)
    # Turn off graphics window
      dev.off()

    # Calculate effects of discharge on detection probability
	    MiDis <- min(est.data$Dis)
	    MaDis <- max(est.data$Dis)
	    nDis <- seq(MiDis, MaDis, .1)
	    RnDis <-(nDis*sd(est.data2$Dis) + mean(est.data2$Dis))/35.3146662127
	    # Mean change in detection over range of discharges
		    predPl <- mean(detRes[1:10, 1]) + mean(detRes[12:17, 1]) + detRes[19, 1]*nDis
		    predP <- exp(predPl)/(1+ exp(predPl))
		    max(predP)-min(predP)
	    # Upper CI for difference
		    predPl <- mean(detRes[1:10, 3]) + mean(detRes[12:17, 3]) + detRes[19, 3]*nDis
		    predP <- exp(predPl)/(1+ exp(predPl))
	      max(predP)-min(predP)
	    # Lower CI for difference
		    predPl <- mean(detRes[1:10, 4]) + mean(detRes[12:17, 4]) + detRes[19, 4]*nDis
		    predP <- exp(predPl)/(1+ exp(predPl))
	      max(predP)-min(predP)
	      #plot(RnDis, predP)

	  # Save mean estuary survival estimates for all years to use in plotting
	  # for diss talk
      estuary_ests <- boxplot(estimate~interval, data=estimates)
	    save(estuary_ests, file="C:/Users/Dan/Desktop/estuary_ests.rda")
################################################################################
} # Code-folding marker
# END PART 4. ESTUARY SURVIVAL ANALYSIS-----------------------------------------


# PART 5. DESCRIPTIVE STATISTICS FOR ESTUARY ANALYSES---------------------------
{ # Code-folding marker
################################################################################
# Create table of summary statistics for all fish tagged in all years by all
# USGS and UMaine researchers
#
# INPUT FILENAME(S):  '2005-present Surgery.txt', 'time' df from PART 5
# MY FUNCTIONS
# NAMED OUPUT:        taggingTable (df)
# OUTPUT FILENAME:    'TaggingTable.csv'
################################################################################
# Install and load necessary packages
  install.packages("tcltk2")
  require(tcltk2)

# Read in the '2005-present Surgery.txt'
	surgery <- read.csv(file.choose( ))
	surgery <- surgery[year(surgery$ReleaseDate)!='2014', ]
	surgery$Year <- year(surgery$ReleaseDate)

# Create a table holding the unique values of year, stocking site, and origin
  parms <- unique(time[,c(10,7,9)])
  parms <- parms[with(parms, order(Year, RelSite, Org)), ]

# Use a loop to calculate mean Forklength, mass, and ATPase activity for each
# release group each year, as well as to summarize release RKMs
  for(i in 1:nrow(parms)){
    parms$LF[i] <-
		  paste(
		    round(mean(time$FL[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),0),
		  	" (",
		  	round(sd(time$FL[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),0),
		  	")",
		  	sep=""
		  )

    parms$Mass[i] <-
		  paste(
		    round(mean(time$Mass[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),0),
		  	" (",
		  	round(sd(time$Mass[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),0),
		  	")",
		  	sep=""
		  )

    parms$NKA[i] <-
		  paste(
		    round(mean(time$ATP[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),2),
		  	" (",
		  	round(sd(time$ATP[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),2),
		  	")",
		  	sep=""
		  )

  	parms$n[i] <-
  		paste(
		    length(time$ATP[
    	    time$Year==parms$Year[i] &
    	 	  time$RelSite==parms$RelSite[i] &
    		  time$Org==parms$Org[i]]),
  			" (",
  			length(surgery$TagID[
    	    surgery$Year==parms$Year[i] &
    	 	  surgery$ReleaseSite==parms$RelSite[i] &
    		  surgery$Origin==parms$Org[i]]),
  			")",
  			sep=""
  			)

  	parms$RKM[i] <-
		  mean(time$RelKM[
    	  time$Year==parms$Year[i] &
    	 	time$RelSite==parms$RelSite[i] &
    		time$Org==parms$Org[i]])

  }
  taggingTable <- parms

# Write the new table to a file
    write.table(taggingTable, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	    row.names=FALSE)
################################################################################

################################################################################
# Descriptive statistics for the estuary timing, mvmt, and survival results
#
# INPUT FILENAME(S):  'SmoltEstuaryTiming.rda'
# MY FUNCTIONS:
# NAMED OUPUT:
# OUTPUT FILENAME:
################################################################################
# Timing
	# Load the timing data
    setwd(choose.dir())
    load('SmoltEstuaryTiming.rda')

  # Calculate mean and standard deviation of estuary arrival date
    mean(timing$OrdinalMin)
    sd(timing$OrdinalMin)

  # Get mean and sd of arrival for hatchery and wild fish
    # Hatchery fish
      mean(timing$OrdinalMin[timing$Origin=="Hatchery"])
      sd(timing$OrdinalMin[timing$Origin=="Hatchery"])
    # Wild fish
      mean(timing$OrdinalMin[timing$Origin=="Wild"])
      sd(timing$OrdinalMin[timing$Origin=="Wild"])

  # Get max and min for effects of release date on arrival date over the range
  # of release dates observed for the release date effect
    mean(yday(timing$ReleaseDate))
    sd(yday(timing$ReleaseDate))

    min(yday(timing$ReleaseDate))
    max(yday(timing$ReleaseDate))
################################################################################
} # Code-folding marker
# END PART 5. DESCRIPTIVE STATISTICS FOR ESTUARY ANALYSES-----------------------


# PART 6. SALINITY PREFERENCE LABORATORY STUDY----------------------------------
{ # Code-folding marker
################################################################################
# Data manipulation for salinity preference study:  takes data from raw text
# files after QC based on lab notes (e.g. renaming files and correcting tagIDs)
# and compiles everything into one big database
#
# INPUT FILENAME(S):  'FishData.csv', 176 Raw data files from shuttlesoft
# MY FUNCTIONS:       'rd', file.dt'
# NAMED OUPUT:        'dats' (df; compiled raw data), fish.files (df; fish data)
#                     'tank' (df; compiled raw data merged with fish data)
# OUTPUT FILENAME(S): 'SalPrefData.txt' (dats), 'FishTank.rda' (tank)
################################################################################
# Install and load necessary packages
	#install.packages('lubridate')
	#install.packages('reshape')
	#install.packages('reshape2')
	#install.packages('tcltk')
	#install.packages('tcltk2')
	require(lubridate)
	require(reshape)
	require(reshape2)
	require(tcltk)
	require(tcltk2)

# Set working directory
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/TaggingData")

# Read in the fish data
  fish <- read.csv('FishData.csv')[ ,-c(13)]
  head(fish) # Look at it to make sure everything is okay

# Read in Osmolality data
	osm <- read.csv('Osmolality2014.csv', header=TRUE)

# Add osmolality to fish data
	fish <- merge(fish, osm, by='OsmVial')
	fish <- fish[ , 2:ncol(fish)]

# Add in the ATPase data now that the analysis is done
	# Read in the file
 	  atpase14 <- read.csv('2014ATPaseActivity.csv', header=TRUE)
	# Give the df new names
	  names(atpase14)[1] <- 'Vial'
	# Get the initial and finishing ATPaseActivity for each fish
	  for(i in 1:nrow(fish)){
	  	fish$ATPaseActivity1[i] <- mean(atpase14$ATPaseActivity[atpase14$Vial==fish$ATPaseVial1[i]])
	  	fish$ATPaseActivity2[i] <- mean(atpase14$ATPaseActivity[atpase14$Vial==fish$ATPaseVial2[i]])
	  } # i

# Reset working directory to get raw data files
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/RawData")

# Make a vector to hold the filenames for raw data
  rd <- function (path = getwd(), pattern = NULL, all.files = FALSE,
  	full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
  	include.dirs = FALSE, no.. = FALSE) .Internal(list.files(path, pattern,
  	all.files, full.names, recursive, ignore.case, include.dirs, no..))

# Create a function to read in files and strip the date and time out of the
# names so the raw data can be associated with the tagging data
# Get date
  file.dt <- function(x){
  	paste(
  	# Strip date from file name
  		substr(
  			as.POSIXct(
          gsub("\\.", "-",
          	substr(colsplit(paste(substr(x, start=1, stop=5), "-",
    	  		  substr(x, start=7, stop=nchar(x)), sep="") ,"_",
    	  	  	names=c("1","2"))[,1],
  	        start=7,
          	stop=nchar(colsplit(paste(
  	      	  substr(x, start=1, stop=5), "-",
          		substr(x, start=7, stop=nchar(x)), sep="")
          		,"_", names=c("1","2"))[,1]))),
    	    format="%m- %d- %Y"),
  			start = 1, stop = 10
  	  ),

   # Strip time from filename
     paste(
       paste(

    	 # Strip hour out of the filename
         substr(
      	   substr(
             colsplit(
      			   paste(
      				   substr(x, start=1, stop=5), "-",
                 substr(x, start=7, stop=nchar(x)), sep=""),
      			       "_", names=c("1","2"))[,2],start=1, stop=4),
          start=1, stop=2
        ),

      # Strip minutes from the filename
        substr(
          substr(
        	  colsplit(
        		  paste(
        			  substr(x, start=1, stop=5), "-",
        			  substr(x, start=7, stop=nchar(x)), sep=""),
        		      "_", names=c("1","2"))[,2],start=1, stop=4),
      	  start=3, stop=4),

    	# Separate the hh and mm with a colon
        sep=":"),

    	# Add zero seconds to the end for formatting
        ":00", sep=""), sep=" "
   )
 }

  file.time <- function(x){
    paste(
    	paste(
    	# Strip hour out of the filename
        substr(
      	  substr(
      		  colsplit(
      			  paste(
      				  substr(x, start=1, stop=5), "-",
                substr(x, start=7, stop=nchar(x)), sep=""),
      			      "_", names=c("1","2"))[,2],start=1, stop=4), start=1, stop=2
        ),
      # Strip minutes from the filename
        substr(
          substr(
        	  colsplit(
        		  paste(
        			  substr(x, start=1, stop=5), "-",
        			  substr(x, start=7, stop=nchar(x)), sep=""),
        		      "_", names=c("1","2"))[,2],start=1, stop=4),
      	    start=3, stop=4),

    	# Separate them with a colon
        sep=":"),

    ":00", sep="")
  }

# Make a dataframe of tags, run dates, and filename
  file.PIT <- substr(rd(), start=1, stop=5) # Tag
  file.DT <- file.dt(rd()) # Date/time
  file.NAME <- c(rd()) # Name of the file

  file.data <- data.frame(file.PIT, file.DT, file.NAME) # Make df
  names(file.data) <- substr(names(file.data), start=6,
  	stop=nchar(names(file.data)))
  head(file.data) # Take a look

# Do a quick data qc
  file.data[!(file.data[,1] %in% fish$PIT),] # Make sure all files are in fish
  fish$PIT[!(fish$PIT %in% file.data[,1])] # Likewise

# Merge the fish data with the data about the files
  fish.files <- merge(fish, file.data, by="PIT")

  # Check the data
    head(fish.files)

# Smash all of the salinity pref tank data into a single file
  pb <- txtProgressBar(min = 0, max = length(rd()), style = 3)
  for(i in 1:length(rd())){
   dats <- read.table(file=as.character(rd())[i], skip=6, header=FALSE )
   dats$TagID <- substr(as.character(rd())[i], start=1, stop=5)
   write.table(dats,
   	 file='C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/SalPrefData.txt',
   	 append=TRUE, sep=",", col.names=F, row.names=F)
   Sys.sleep(0.1)
   setTxtProgressBar(pb, i)
  }
  close(pb)

  # Read it back
  # Reset working directory to get the file
    setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis")
    dats <- read.csv('SalPrefData.txt', header=F)
  # Give the table new names
    names(dats) <- c('Time', 'AM.PM.','Zone',	'Object salinity',
    'Preferred salinity',	'INCR salinity', 'DECR salinity',	'x pos',
  	'y pos',	'Velocity [mm/s]',	'Distance moved [mm]', 'Time INCR [s]',
  	'Time DECR [s]', 'Delta', 'Hysteresis',	'HI zone setpoint',
    'HI zone hysteresis',	'LO zone setpoint',	'LO zone hysteresis',	'k [min-1]',
    'Max INCR',	'Min DECR',	'Max Rate [Sal/hr]',	'UNK','Avoidance', 'Upper',
    'mean', 'PIT')

# Combine the fish and files data with the tank data...takes a few seconds
  tank <- merge(fish.files, dats[,c(1:13,ncol(dats))], by="PIT")

  # Save it out as an r-data file
    save(tank, file='FishTank.rda')
################################################################################

################################################################################
# Data analysis for salinity preference study
#
# INPUT FILENAME(S):  'FishTank.rda' (tank), need fish.files object created
#                      above!!
# MY FUNCTIONS:
# NAMED OUPUT:        'clean' (df; fish data with salinity choice result)
# OUTPUT FILENAME(S):
################################################################################
# Install and load necessary packages
	#install.packages('plotrix')
	#install.packages('tcltk')
	#install.packages('tcltk2')
	#install.packages('AICcmodavg')
	#install.packages('MASS')
	require(plotrix)
	require(tcltk)
	require(tcltk2)
	require(AICcmodavg)
	require(MASS)

# Define functions
  # 'aic.table.nb' *** SPECIALLY MADE FOR GLM.NB func ********
  # Function to create AIC Model selection table
    # Write a fxn to do it
      aic.table.nb <- function(x, y){
        # Create a column to hold number of parameters
          k <- c()
        # Get number of parameters
          for (i in 1:length(x)){
            k[i] <- (length(x[[i]]$coefficients))
            #k[i]<-nrow(data.frame((summary(Cand.models[[i]])[10]))) + 1
          }
        # Make blank columns to hold statistics
          AICc <- c(rep(0, length(k)))
          DeltaAIC <- c(rep(0, length(k)))
          AICWeight <- c(rep(0, length(k)))
          Deviance <- c(rep(0, length(k)))
          Relative.likelihood<-c(rep(0, length(k)))
        # Make dataframe to hold results
          ranks<-data.frame(y, k, AICc, DeltaAIC, AICWeight)
          names(ranks)[1] <- "Model"
        # Get AIC for each model
          for(i in 1:nrow(ranks)){
            ranks$AICc[i] <- (x[[i]])$aic
          }
        # Sort the table by AICc
          ranks <- ranks[with(ranks, order(AICc)), ]
        # Calculate delta AIC
          for(i in 1:nrow(ranks)){
            ranks$DeltaAIC[i] <- ranks$AICc[i]-ranks$AICc[1]
          }
        # Calculate relative likelihoods
          for(i in 1:nrow(ranks)){
            Relative.likelihood[i] <- exp(-.5*ranks$DeltaAIC[i])
          }
        # Calculate AIC weights
          for(i in 1:length(Relative.likelihood)){
            ranks$AICWeight[i] <- Relative.likelihood[i]/
             sum(Relative.likelihood[1:length(Relative.likelihood)])
          }
        # Round off the calculated columns
          ranks[ , 3:5] <- round(ranks[ , 3:5], 2)
        # Print the model selection results
          return(ranks)
    }

  # 'approx.chat'
  # Create a fxn to calculate c-hat
    approx.chat <- function ( object ){
      with(object, sum((weights*residuals^2)[weights>0])/df.residual)
    }

# Set working directory
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis")

# Load the data, it's a df called 'tank'
	load('FishTank.rda')

# Give it new names
	names(tank) <- c(names(tank)[1:20], "ObjSalinity", "PrefSalinity",
		"INCRSalinity", "DECRSalinity", "X", "Y", "Velocity", "Dist", "TimeINCR",
		"TimeDECR")

# First attempt to summarize preference data by fish
	 tank$pref <- tank$TimeINCR/(tank$TimeINCR + tank$TimeDECR)
	 tank <- tank[with(tank, order(PIT, TimeINCR, TimeDECR)), ]

	  # Create an empty vector to hold salinity preferences
  	  pref<-c()

	  # Calculate final preference for each fish
		  pb <- txtProgressBar(min = 0, max = length(unique(tank$PIT)), style = 3)
	      for(i in 1:length(unique(tank$PIT))){
	        pref[i] <- tank$pref[tank$PIT==unique(tank$PIT)[i]][length(tank$pref[tank$PIT==unique(tank$PIT)[i]])]
  	    Sys.sleep(0.01)
        setTxtProgressBar(pb, i)
  	    }
	    close(pb)

	# Make sure the PIT column is in the same order as the pref vector
	  unique(tank$PIT)==fish.files$PIT

	# Create a dataframe with preference for each fish and fish data
	  clean <- data.frame(fish.files, pref)

  # Change ordering of factor assignment for plotting later on
  	clean$Stage=factor(clean$Stage, c('Presmolt', 'Smolt', 'Postsmolt'))

	# Logit preference for constraint
	  clean$LogtPref <- log((pref)/(1-pref))

	# Binary preference
	  for(i in 1:nrow(clean)){
	  	if(clean$pref[i] > 0.50){
	  		clean$Binary[i] <- 1
	  	} else {
	  		clean$Binary[i] <- 0
	  	}
	  }

	# Calculate time after handling
	  clean$DT <- as.POSIXct(clean$DT) # Reformat trial date
	  clean$TaggingDate <- as.POSIXct(clean$TaggingDate)	# Reformat tagging date
	  clean$handler <- yday(clean$DT) - yday(clean$TaggingDate)

	# Calculate change in ATPase Activity for each fish
	  clean$DeltaATP <- clean$ATPaseActivity2-clean$ATPaseActivity1

	# Calculate number of switches
	  # First, get a binary column for switches-- about 30 minutes
	    tank$Zone <- as.character(tank$Zone)
		  pb <- txtProgressBar(min = 0, max = nrow(tank), style = 3)
	    for(i in 1:nrow(tank)){
        if(tank$Zone[i]=="OFF"){
        	tank$OffCount[i] <- 1
        } else {
        	tank$OffCount[i] <- 0
        }
        Sys.sleep(0.00001)
        setTxtProgressBar(pb, i) # Update progress bar
	    } # i
	    close(pb) # Close progress bar
	  # Now get cumulative number of switches for each second of each trial
		  Switches <- c()
			pb <- txtProgressBar(min = 0, max = length(unique(tank$PIT)), style = 3)
	    for(i in 1:length(unique(tank$PIT))){
	    	Switches <- append(Switches, cumsum(tank$OffCount[tank$PIT==unique(tank$PIT)[i]]))
        Sys.sleep(0.00001)
        setTxtProgressBar(pb, i) # Update progress bar
	    }
	    tank <- data.frame(tank, Switches)


	# Calculate maximum continuous time spent in SW by each fish (in seconds)
	  pb <- txtProgressBar(min = 0, max = length(unique(tank$PIT)), style = 3)
	  for(i in 1:length(unique(tank$PIT))){
	  	for(t in 1:nrow(clean)){
	  		if(clean$PIT[t]==unique(tank$PIT)[i]){
	  	    clean$MaxTime[t] <- max(rle(as.vector(tank$Zone[tank$PIT==unique(tank$PIT)[i]]))$lengths[
	  		    rle(as.vector(tank$Zone[tank$PIT==unique(tank$PIT)[i]]))$values=="INCR"])
	  		} else {
	  			next;
	  		} # else
	    } # t
	  	Sys.sleep(0.01)
      setTxtProgressBar(pb, i)
	  } # i
	  close(pb)
	  head(clean$MaxTime, 25)

	# Calculate number of movements between compartments
	  pb <- txtProgressBar(min = 0, max = nrow(clean), style = 3)
	  for(t in 1:nrow(clean)){
	  	clean$Switches[t] <- length(tank$Zone[tank$Zone=='OFF' & tank$PIT==clean$PIT[t]])
	  	Sys.sleep(0.01)
      setTxtProgressBar(pb, t)
	  } # t
	  close(pb)
	  head(clean$Switches, 25)

# Finish formatting factors appropriately
	clean$DayNight <- as.factor(clean$DayNight)
  clean$Start <- as.factor(clean$Start)

# RUN MODELS
  # PREFERENCE
	  # Create candidate model list for preference
	    prefMods=list()
        prefMods[[1]] <- lm(LogtPref ~ Stage, data=clean)
        prefMods[[2]] <- lm(LogtPref ~ Stage + DayNight, data=clean)
        prefMods[[3]] <- lm(LogtPref ~ Stage + Start, data=clean)
        prefMods[[4]] <- lm(LogtPref ~ Stage + handler, data=clean)
        prefMods[[5]] <- lm(LogtPref ~ Stage + DayNight + Start, data=clean)
        prefMods[[6]] <- lm(LogtPref ~ Stage + DayNight + handler, data=clean)
        prefMods[[7]] <- lm(LogtPref ~ Stage + DayNight + Start + handler, data=clean)
        prefMods[[8]] <- lm(LogtPref ~ Stage + Start + handler, data=clean)
        prefMods[[9]] <- lm(LogtPref ~ Stage + ATPaseActivity1, data=clean)
        prefMods[[10]] <- lm(LogtPref ~ Stage + DayNight + ATPaseActivity1, data=clean)
        prefMods[[11]] <- lm(LogtPref ~ Stage + Start + ATPaseActivity1, data=clean)
        prefMods[[12]] <- lm(LogtPref ~ Stage + handler + ATPaseActivity1, data=clean)
        prefMods[[13]] <- lm(LogtPref ~ Stage + DayNight + Start + ATPaseActivity1, data=clean)
        prefMods[[14]] <- lm(LogtPref ~ Stage + DayNight + handler + ATPaseActivity1, data=clean)
        prefMods[[15]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + ATPaseActivity1, data=clean)
        prefMods[[16]] <- lm(LogtPref ~ Stage + Start + handler + ATPaseActivity1, data=clean)
        prefMods[[17]] <- lm(LogtPref ~ Stage + Osmolality, data=clean)
        prefMods[[18]] <- lm(LogtPref ~ Stage + DayNight + Osmolality, data=clean)
        prefMods[[19]] <- lm(LogtPref ~ Stage + Start + Osmolality, data=clean)
        prefMods[[20]] <- lm(LogtPref ~ Stage + handler + Osmolality, data=clean)
        prefMods[[21]] <- lm(LogtPref ~ Stage + DayNight + Start + Osmolality, data=clean)
        prefMods[[22]] <- lm(LogtPref ~ Stage + DayNight + handler + Osmolality, data=clean)
        prefMods[[23]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + Osmolality, data=clean)
        prefMods[[24]] <- lm(LogtPref ~ Stage + Start + handler + Osmolality, data=clean)
        prefMods[[25]] <- lm(LogtPref ~ Stage + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[26]] <- lm(LogtPref ~ Stage + DayNight + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[27]] <- lm(LogtPref ~ Stage + Start + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[28]] <- lm(LogtPref ~ Stage + handler + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[29]] <- lm(LogtPref ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[30]] <- lm(LogtPref ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[31]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[32]] <- lm(LogtPref ~ Stage + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        prefMods[[33]] <- lm(LogtPref ~ Stage + DeltaATP, data=clean)
        prefMods[[34]] <- lm(LogtPref ~ Stage + DayNight + DeltaATP, data=clean)
        prefMods[[35]] <- lm(LogtPref ~ Stage + Start + DeltaATP, data=clean)
        prefMods[[36]] <- lm(LogtPref ~ Stage + handler + DeltaATP, data=clean)
        prefMods[[37]] <- lm(LogtPref ~ Stage + DayNight + Start + DeltaATP, data=clean)
        prefMods[[38]] <- lm(LogtPref ~ Stage + DayNight + handler + DeltaATP, data=clean)
        prefMods[[39]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + DeltaATP, data=clean)
        prefMods[[40]] <- lm(LogtPref ~ Stage + Start + handler + DeltaATP, data=clean)
        prefMods[[41]] <- lm(LogtPref ~ Stage + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[42]] <- lm(LogtPref ~ Stage + DayNight + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[43]] <- lm(LogtPref ~ Stage + Start + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[44]] <- lm(LogtPref ~ Stage + handler + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[45]] <- lm(LogtPref ~ Stage + DayNight + Start + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[46]] <- lm(LogtPref ~ Stage + DayNight + handler + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[47]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[48]] <- lm(LogtPref ~ Stage + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        prefMods[[49]] <- lm(LogtPref ~ Stage + Osmolality + DeltaATP, data=clean)
        prefMods[[50]] <- lm(LogtPref ~ Stage + DayNight + Osmolality + DeltaATP, data=clean)
        prefMods[[51]] <- lm(LogtPref ~ Stage + Start + Osmolality + DeltaATP, data=clean)
        prefMods[[52]] <- lm(LogtPref ~ Stage + handler + Osmolality + DeltaATP, data=clean)
        prefMods[[53]] <- lm(LogtPref ~ Stage + DayNight + Start + Osmolality + DeltaATP, data=clean)
        prefMods[[54]] <- lm(LogtPref ~ Stage + DayNight + handler + Osmolality + DeltaATP, data=clean)
        prefMods[[55]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + Osmolality + DeltaATP, data=clean)
        prefMods[[56]] <- lm(LogtPref ~ Stage + Start + handler + Osmolality + DeltaATP, data=clean)
        prefMods[[57]] <- lm(LogtPref ~ Stage + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[58]] <- lm(LogtPref ~ Stage + DayNight + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[59]] <- lm(LogtPref ~ Stage + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[60]] <- lm(LogtPref ~ Stage + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[61]] <- lm(LogtPref ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[62]] <- lm(LogtPref ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[63]] <- lm(LogtPref ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        prefMods[[64]] <- lm(LogtPref ~ Stage + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)

	  # Create vector of model names for preference models
	    prefModsNames<-c()
        prefModsNames[1] <- 'Stage'
        prefModsNames[2] <- 'Stage.Light'
        prefModsNames[3] <- 'Stage.Start'
        prefModsNames[4] <- 'Stage.handler'
        prefModsNames[5] <- 'Stage.Light.Start'
        prefModsNames[6] <- 'Stage.Light.handler'
        prefModsNames[7] <- 'Stage.Light.Start.handler'
        prefModsNames[8] <- 'Stage.Start.handler'
        prefModsNames[9] <- 'Stage.NKA'
        prefModsNames[10] <- 'Stage.Light.NKA'
        prefModsNames[11] <- 'Stage.Start.NKA'
        prefModsNames[12] <- 'Stage.handler.NKA'
        prefModsNames[13] <- 'Stage.Light.Start.NKA'
        prefModsNames[14] <- 'Stage.Light.handler.NKA'
        prefModsNames[15] <- 'Stage.Light.Start.handler.NKA'
        prefModsNames[16] <- 'Stage.Start.handler.NKA'
        prefModsNames[17] <- 'Stage.Osm'
        prefModsNames[18] <- 'Stage.Light.Osm'
        prefModsNames[19] <- 'Stage.Start.Osm'
        prefModsNames[20] <- 'Stage.handler.Osm'
        prefModsNames[21] <- 'Stage.Light.Start.Osm'
        prefModsNames[22] <- 'Stage.Light.handler.Osm'
        prefModsNames[23] <- 'Stage.Light.Start.handler.Osm'
        prefModsNames[24] <- 'Stage.Start.handler.Osm'
        prefModsNames[25] <- 'Stage.NKA.Osm'
        prefModsNames[26] <- 'Stage.Light.NKA.Osm'
        prefModsNames[27] <- 'Stage.Start.NKA.Osm'
        prefModsNames[28] <- 'Stage.handler.NKA.Osm'
        prefModsNames[29] <- 'Stage.Light.Start.NKA.Osm'
        prefModsNames[30] <- 'Stage.Light.handler.NKA.Osm'
        prefModsNames[31] <- 'Stage.Light.Start.handler.NKA.Osm'
        prefModsNames[32] <- 'Stage.Start.handler.NKA.Osm'
        prefModsNames[33] <- 'Stage.DNKA'
        prefModsNames[34] <- 'Stage.Light.DNKA'
        prefModsNames[35] <- 'Stage.Start.DNKA'
        prefModsNames[36] <- 'Stage.handler.DNKA'
        prefModsNames[37] <- 'Stage.Light.Start.DNKA'
        prefModsNames[38] <- 'Stage.Light.handler.DNKA'
        prefModsNames[39] <- 'Stage.Light.Start.handler.DNKA'
        prefModsNames[40] <- 'Stage.Start.handler.DNKA'
        prefModsNames[41] <- 'Stage.NKA.DNKA'
        prefModsNames[42] <- 'Stage.Light.NKA.DNKA'
        prefModsNames[43] <- 'Stage.Start.NKA.DNKA'
        prefModsNames[44] <- 'Stage.handler.NKA.DNKA'
        prefModsNames[45] <- 'Stage.Light.Start.NKA.DNKA'
        prefModsNames[46] <- 'Stage.Light.handler.NKA.DNKA'
        prefModsNames[47] <- 'Stage.Light.Start.handler.NKA.DNKA'
        prefModsNames[48] <- 'Stage.Start.handler.NKA.DNKA'
        prefModsNames[49] <- 'Stage.Osm.DNKA'
        prefModsNames[50] <- 'Stage.Light.Osm.DNKA'
        prefModsNames[51] <- 'Stage.Start.Osm.DNKA'
        prefModsNames[52] <- 'Stage.handler.Osm.DNKA'
        prefModsNames[53] <- 'Stage.Light.Start.Osm.DNKA'
        prefModsNames[54] <- 'Stage.Light.handler.Osm.DNKA'
        prefModsNames[55] <- 'Stage.Light.Start.handler.Osm.DNKA'
        prefModsNames[56] <- 'Stage.Start.handler.Osm.DNKA'
        prefModsNames[57] <- 'Stage.NKA.Osm.DNKA'
        prefModsNames[58] <- 'Stage.Light.NKA.Osm.DNKA'
        prefModsNames[59] <- 'Stage.Start.NKA.Osm.DNKA'
        prefModsNames[60] <- 'Stage.handler.NKA.Osm.DNKA'
        prefModsNames[61] <- 'Stage.Light.Start.NKA.Osm.DNKA'
        prefModsNames[62] <- 'Stage.Light.handler.NKA.Osm.DNKA'
        prefModsNames[63] <- 'Stage.Light.Start.handler.NKA.Osm.DNKA'
        prefModsNames[64] <- 'Stage.Start.handler.NKA.Osm.DNKA'

   # Print model-selection results
	   prefRes <- aictab(prefMods, prefModsNames)
	   head(aictab(prefMods, prefModsNames), 10)

	# TOLERANCE
	  # Create candidate model list
	    tolMods <- list()
        tolMods[[1]] <- glm.nb(MaxTime ~ Stage, data=clean)
        tolMods[[2]] <- glm.nb(MaxTime ~ Stage + DayNight, data=clean)
        tolMods[[3]] <- glm.nb(MaxTime ~ Stage + Start, data=clean)
        tolMods[[4]] <- glm.nb(MaxTime ~ Stage + handler, data=clean)
        tolMods[[5]] <- glm.nb(MaxTime ~ Stage + DayNight + Start, data=clean)
        tolMods[[6]] <- glm.nb(MaxTime ~ Stage + DayNight + handler, data=clean)
        tolMods[[7]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler, data=clean)
        tolMods[[8]] <- glm.nb(MaxTime ~ Stage + Start + handler, data=clean)
        tolMods[[9]] <- glm.nb(MaxTime ~ Stage + ATPaseActivity1, data=clean)
        tolMods[[10]] <- glm.nb(MaxTime ~ Stage + DayNight + ATPaseActivity1, data=clean)
        tolMods[[11]] <- glm.nb(MaxTime ~ Stage + Start + ATPaseActivity1, data=clean)
        tolMods[[12]] <- glm.nb(MaxTime ~ Stage + handler + ATPaseActivity1, data=clean)
        tolMods[[13]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + ATPaseActivity1, data=clean)
        tolMods[[14]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + ATPaseActivity1, data=clean)
        tolMods[[15]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + ATPaseActivity1, data=clean)
        tolMods[[16]] <- glm.nb(MaxTime ~ Stage + Start + handler + ATPaseActivity1, data=clean)
        tolMods[[17]] <- glm.nb(MaxTime ~ Stage + Osmolality, data=clean)
        tolMods[[18]] <- glm.nb(MaxTime ~ Stage + DayNight + Osmolality, data=clean)
        tolMods[[19]] <- glm.nb(MaxTime ~ Stage + Start + Osmolality, data=clean)
        tolMods[[20]] <- glm.nb(MaxTime ~ Stage + handler + Osmolality, data=clean)
        tolMods[[21]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + Osmolality, data=clean)
        tolMods[[22]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + Osmolality, data=clean)
        tolMods[[23]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + Osmolality, data=clean)
        tolMods[[24]] <- glm.nb(MaxTime ~ Stage + Start + handler + Osmolality, data=clean)
        tolMods[[25]] <- glm.nb(MaxTime ~ Stage + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[26]] <- glm.nb(MaxTime ~ Stage + DayNight + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[27]] <- glm.nb(MaxTime ~ Stage + Start + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[28]] <- glm.nb(MaxTime ~ Stage + handler + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[29]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[30]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[31]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[32]] <- glm.nb(MaxTime ~ Stage + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        tolMods[[33]] <- glm.nb(MaxTime ~ Stage + DeltaATP, data=clean)
        tolMods[[34]] <- glm.nb(MaxTime ~ Stage + DayNight + DeltaATP, data=clean)
        tolMods[[35]] <- glm.nb(MaxTime ~ Stage + Start + DeltaATP, data=clean)
        tolMods[[36]] <- glm.nb(MaxTime ~ Stage + handler + DeltaATP, data=clean)
        tolMods[[37]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + DeltaATP, data=clean)
        tolMods[[38]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + DeltaATP, data=clean)
        tolMods[[39]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + DeltaATP, data=clean)
        tolMods[[40]] <- glm.nb(MaxTime ~ Stage + Start + handler + DeltaATP, data=clean)
        tolMods[[41]] <- glm.nb(MaxTime ~ Stage + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[42]] <- glm.nb(MaxTime ~ Stage + DayNight + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[43]] <- glm.nb(MaxTime ~ Stage + Start + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[44]] <- glm.nb(MaxTime ~ Stage + handler + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[45]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[46]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[47]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[48]] <- glm.nb(MaxTime ~ Stage + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        tolMods[[49]] <- glm.nb(MaxTime ~ Stage + Osmolality + DeltaATP, data=clean)
        tolMods[[50]] <- glm.nb(MaxTime ~ Stage + DayNight + Osmolality + DeltaATP, data=clean)
        tolMods[[51]] <- glm.nb(MaxTime ~ Stage + Start + Osmolality + DeltaATP, data=clean)
        tolMods[[52]] <- glm.nb(MaxTime ~ Stage + handler + Osmolality + DeltaATP, data=clean)
        tolMods[[53]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + Osmolality + DeltaATP, data=clean)
        tolMods[[54]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + Osmolality + DeltaATP, data=clean)
        tolMods[[55]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + Osmolality + DeltaATP, data=clean)
        tolMods[[56]] <- glm.nb(MaxTime ~ Stage + Start + handler + Osmolality + DeltaATP, data=clean)
        tolMods[[57]] <- glm.nb(MaxTime ~ Stage + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[58]] <- glm.nb(MaxTime ~ Stage + DayNight + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[59]] <- glm.nb(MaxTime ~ Stage + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[60]] <- glm.nb(MaxTime ~ Stage + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[61]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[62]] <- glm.nb(MaxTime ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[63]] <- glm.nb(MaxTime ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        tolMods[[64]] <- glm.nb(MaxTime ~ Stage + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)

	  # Create a vector of model names for the tolerance models
	    tolModsNames <- c()
	      tolModsNames[1] <- 'Stage'
        tolModsNames[2] <- 'Stage.Light'
        tolModsNames[3] <- 'Stage.Start'
        tolModsNames[4] <- 'Stage.handler'
        tolModsNames[5] <- 'Stage.Light.Start'
        tolModsNames[6] <- 'Stage.Light.handler'
        tolModsNames[7] <- 'Stage.Light.Start.handler'
        tolModsNames[8] <- 'Stage.Start.handler'
        tolModsNames[9] <- 'Stage.NKA'
        tolModsNames[10] <- 'Stage.Light.NKA'
        tolModsNames[11] <- 'Stage.Start.NKA'
        tolModsNames[12] <- 'Stage.handler.NKA'
        tolModsNames[13] <- 'Stage.Light.Start.NKA'
        tolModsNames[14] <- 'Stage.Light.handler.NKA'
        tolModsNames[15] <- 'Stage.Light.Start.handler.NKA'
        tolModsNames[16] <- 'Stage.Start.handler.NKA'
        tolModsNames[17] <- 'Stage.Osm'
        tolModsNames[18] <- 'Stage.Light.Osm'
        tolModsNames[19] <- 'Stage.Start.Osm'
        tolModsNames[20] <- 'Stage.handler.Osm'
        tolModsNames[21] <- 'Stage.Light.Start.Osm'
        tolModsNames[22] <- 'Stage.Light.handler.Osm'
        tolModsNames[23] <- 'Stage.Light.Start.handler.Osm'
        tolModsNames[24] <- 'Stage.Start.handler.Osm'
        tolModsNames[25] <- 'Stage.NKA.Osm'
        tolModsNames[26] <- 'Stage.Light.NKA.Osm'
        tolModsNames[27] <- 'Stage.Start.NKA.Osm'
        tolModsNames[28] <- 'Stage.handler.NKA.Osm'
        tolModsNames[29] <- 'Stage.Light.Start.NKA.Osm'
        tolModsNames[30] <- 'Stage.Light.handler.NKA.Osm'
        tolModsNames[31] <- 'Stage.Light.Start.handler.NKA.Osm'
        tolModsNames[32] <- 'Stage.Start.handler.NKA.Osm'
        tolModsNames[33] <- 'Stage.DNKA'
        tolModsNames[34] <- 'Stage.Light.DNKA'
        tolModsNames[35] <- 'Stage.Start.DNKA'
        tolModsNames[36] <- 'Stage.handler.DNKA'
        tolModsNames[37] <- 'Stage.Light.Start.DNKA'
        tolModsNames[38] <- 'Stage.Light.handler.DNKA'
        tolModsNames[39] <- 'Stage.Light.Start.handler.DNKA'
        tolModsNames[40] <- 'Stage.Start.handler.DNKA'
        tolModsNames[41] <- 'Stage.NKA.DNKA'
        tolModsNames[42] <- 'Stage.Light.NKA.DNKA'
        tolModsNames[43] <- 'Stage.Start.NKA.DNKA'
        tolModsNames[44] <- 'Stage.handler.NKA.DNKA'
        tolModsNames[45] <- 'Stage.Light.Start.NKA.DNKA'
        tolModsNames[46] <- 'Stage.Light.handler.NKA.DNKA'
        tolModsNames[47] <- 'Stage.Light.Start.handler.NKA.DNKA'
        tolModsNames[48] <- 'Stage.Start.handler.NKA.DNKA'
        tolModsNames[49] <- 'Stage.Osm.DNKA'
        tolModsNames[50] <- 'Stage.Light.Osm.DNKA'
        tolModsNames[51] <- 'Stage.Start.Osm.DNKA'
        tolModsNames[52] <- 'Stage.handler.Osm.DNKA'
        tolModsNames[53] <- 'Stage.Light.Start.Osm.DNKA'
        tolModsNames[54] <- 'Stage.Light.handler.Osm.DNKA'
        tolModsNames[55] <- 'Stage.Light.Start.handler.Osm.DNKA'
        tolModsNames[56] <- 'Stage.Start.handler.Osm.DNKA'
        tolModsNames[57] <- 'Stage.NKA.Osm.DNKA'
        tolModsNames[58] <- 'Stage.Light.NKA.Osm.DNKA'
        tolModsNames[59] <- 'Stage.Start.NKA.Osm.DNKA'
        tolModsNames[60] <- 'Stage.handler.NKA.Osm.DNKA'
        tolModsNames[61] <- 'Stage.Light.Start.NKA.Osm.DNKA'
        tolModsNames[62] <- 'Stage.Light.handler.NKA.Osm.DNKA'
        tolModsNames[63] <- 'Stage.Light.Start.handler.NKA.Osm.DNKA'
        tolModsNames[64] <- 'Stage.Start.handler.NKA.Osm.DNKA'

	  # Print model-selection table for tolerance models
	    tolRes <- aic.table.nb(tolMods, tolModsNames)
	    head(aic.table.nb(tolMods, tolModsNames), 10)

	# MOVEMENT MODELS
	  # Create a list of candidate models for movement
	    movMods <- list()
        movMods[[1]] <- glm.nb(Switches ~ Stage, data=clean)
        movMods[[2]] <- glm.nb(Switches ~ Stage + DayNight, data=clean)
        movMods[[3]] <- glm.nb(Switches ~ Stage + Start, data=clean)
        movMods[[4]] <- glm.nb(Switches ~ Stage + handler, data=clean)
        movMods[[5]] <- glm.nb(Switches ~ Stage + DayNight + Start, data=clean)
        movMods[[6]] <- glm.nb(Switches ~ Stage + DayNight + handler, data=clean)
        movMods[[7]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler, data=clean)
        movMods[[8]] <- glm.nb(Switches ~ Stage + Start + handler, data=clean)
        movMods[[9]] <- glm.nb(Switches ~ Stage + ATPaseActivity1, data=clean)
        movMods[[10]] <- glm.nb(Switches ~ Stage + DayNight + ATPaseActivity1, data=clean)
        movMods[[11]] <- glm.nb(Switches ~ Stage + Start + ATPaseActivity1, data=clean)
        movMods[[12]] <- glm.nb(Switches ~ Stage + handler + ATPaseActivity1, data=clean)
        movMods[[13]] <- glm.nb(Switches ~ Stage + DayNight + Start + ATPaseActivity1, data=clean)
        movMods[[14]] <- glm.nb(Switches ~ Stage + DayNight + handler + ATPaseActivity1, data=clean)
        movMods[[15]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + ATPaseActivity1, data=clean)
        movMods[[16]] <- glm.nb(Switches ~ Stage + Start + handler + ATPaseActivity1, data=clean)
        movMods[[17]] <- glm.nb(Switches ~ Stage + Osmolality, data=clean)
        movMods[[18]] <- glm.nb(Switches ~ Stage + DayNight + Osmolality, data=clean)
        movMods[[19]] <- glm.nb(Switches ~ Stage + Start + Osmolality, data=clean)
        movMods[[20]] <- glm.nb(Switches ~ Stage + handler + Osmolality, data=clean)
        movMods[[21]] <- glm.nb(Switches ~ Stage + DayNight + Start + Osmolality, data=clean)
        movMods[[22]] <- glm.nb(Switches ~ Stage + DayNight + handler + Osmolality, data=clean)
        movMods[[23]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + Osmolality, data=clean)
        movMods[[24]] <- glm.nb(Switches ~ Stage + Start + handler + Osmolality, data=clean)
        movMods[[25]] <- glm.nb(Switches ~ Stage + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[26]] <- glm.nb(Switches ~ Stage + DayNight + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[27]] <- glm.nb(Switches ~ Stage + Start + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[28]] <- glm.nb(Switches ~ Stage + handler + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[29]] <- glm.nb(Switches ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[30]] <- glm.nb(Switches ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[31]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[32]] <- glm.nb(Switches ~ Stage + Start + handler + ATPaseActivity1 + Osmolality, data=clean)
        movMods[[33]] <- glm.nb(Switches ~ Stage + DeltaATP, data=clean)
        movMods[[34]] <- glm.nb(Switches ~ Stage + DayNight + DeltaATP, data=clean)
        movMods[[35]] <- glm.nb(Switches ~ Stage + Start + DeltaATP, data=clean)
        movMods[[36]] <- glm.nb(Switches ~ Stage + handler + DeltaATP, data=clean)
        movMods[[37]] <- glm.nb(Switches ~ Stage + DayNight + Start + DeltaATP, data=clean)
        movMods[[38]] <- glm.nb(Switches ~ Stage + DayNight + handler + DeltaATP, data=clean)
        movMods[[39]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + DeltaATP, data=clean)
        movMods[[40]] <- glm.nb(Switches ~ Stage + Start + handler + DeltaATP, data=clean)
        movMods[[41]] <- glm.nb(Switches ~ Stage + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[42]] <- glm.nb(Switches ~ Stage + DayNight + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[43]] <- glm.nb(Switches ~ Stage + Start + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[44]] <- glm.nb(Switches ~ Stage + handler + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[45]] <- glm.nb(Switches ~ Stage + DayNight + Start + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[46]] <- glm.nb(Switches ~ Stage + DayNight + handler + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[47]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[48]] <- glm.nb(Switches ~ Stage + Start + handler + ATPaseActivity1 + DeltaATP, data=clean)
        movMods[[49]] <- glm.nb(Switches ~ Stage + Osmolality + DeltaATP, data=clean)
        movMods[[50]] <- glm.nb(Switches ~ Stage + DayNight + Osmolality + DeltaATP, data=clean)
        movMods[[51]] <- glm.nb(Switches ~ Stage + Start + Osmolality + DeltaATP, data=clean)
        movMods[[52]] <- glm.nb(Switches ~ Stage + handler + Osmolality + DeltaATP, data=clean)
        movMods[[53]] <- glm.nb(Switches ~ Stage + DayNight + Start + Osmolality + DeltaATP, data=clean)
        movMods[[54]] <- glm.nb(Switches ~ Stage + DayNight + handler + Osmolality + DeltaATP, data=clean)
        movMods[[55]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + Osmolality + DeltaATP, data=clean)
        movMods[[56]] <- glm.nb(Switches ~ Stage + Start + handler + Osmolality + DeltaATP, data=clean)
        movMods[[57]] <- glm.nb(Switches ~ Stage + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[58]] <- glm.nb(Switches ~ Stage + DayNight + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[59]] <- glm.nb(Switches ~ Stage + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[60]] <- glm.nb(Switches ~ Stage + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[61]] <- glm.nb(Switches ~ Stage + DayNight + Start + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[62]] <- glm.nb(Switches ~ Stage + DayNight + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[63]] <- glm.nb(Switches ~ Stage + DayNight + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)
        movMods[[64]] <- glm.nb(Switches ~ Stage + Start + handler + ATPaseActivity1 + Osmolality + DeltaATP, data=clean)

   # Create a vector of model names for movements
	   movModsNames <- c()
        movModsNames[1] <- 'Stage'
        movModsNames[2] <- 'Stage.Light'
        movModsNames[3] <- 'Stage.Start'
        movModsNames[4] <- 'Stage.handler'
        movModsNames[5] <- 'Stage.Light.Start'
        movModsNames[6] <- 'Stage.Light.handler'
        movModsNames[7] <- 'Stage.Light.Start.handler'
        movModsNames[8] <- 'Stage.Start.handler'
        movModsNames[9] <- 'Stage.NKA'
        movModsNames[10] <- 'Stage.Light.NKA'
        movModsNames[11] <- 'Stage.Start.NKA'
        movModsNames[12] <- 'Stage.handler.NKA'
        movModsNames[13] <- 'Stage.Light.Start.NKA'
        movModsNames[14] <- 'Stage.Light.handler.NKA'
        movModsNames[15] <- 'Stage.Light.Start.handler.NKA'
        movModsNames[16] <- 'Stage.Start.handler.NKA'
        movModsNames[17] <- 'Stage.Osm'
        movModsNames[18] <- 'Stage.Light.Osm'
        movModsNames[19] <- 'Stage.Start.Osm'
        movModsNames[20] <- 'Stage.handler.Osm'
        movModsNames[21] <- 'Stage.Light.Start.Osm'
        movModsNames[22] <- 'Stage.Light.handler.Osm'
        movModsNames[23] <- 'Stage.Light.Start.handler.Osm'
        movModsNames[24] <- 'Stage.Start.handler.Osm'
        movModsNames[25] <- 'Stage.NKA.Osm'
        movModsNames[26] <- 'Stage.Light.NKA.Osm'
        movModsNames[27] <- 'Stage.Start.NKA.Osm'
        movModsNames[28] <- 'Stage.handler.NKA.Osm'
        movModsNames[29] <- 'Stage.Light.Start.NKA.Osm'
        movModsNames[30] <- 'Stage.Light.handler.NKA.Osm'
        movModsNames[31] <- 'Stage.Light.Start.handler.NKA.Osm'
        movModsNames[32] <- 'Stage.Start.handler.NKA.Osm'
        movModsNames[33] <- 'Stage.DNKA'
        movModsNames[34] <- 'Stage.Light.DNKA'
        movModsNames[35] <- 'Stage.Start.DNKA'
        movModsNames[36] <- 'Stage.handler.DNKA'
        movModsNames[37] <- 'Stage.Light.Start.DNKA'
        movModsNames[38] <- 'Stage.Light.handler.DNKA'
        movModsNames[39] <- 'Stage.Light.Start.handler.DNKA'
        movModsNames[40] <- 'Stage.Start.handler.DNKA'
        movModsNames[41] <- 'Stage.NKA.DNKA'
        movModsNames[42] <- 'Stage.Light.NKA.DNKA'
        movModsNames[43] <- 'Stage.Start.NKA.DNKA'
        movModsNames[44] <- 'Stage.handler.NKA.DNKA'
        movModsNames[45] <- 'Stage.Light.Start.NKA.DNKA'
        movModsNames[46] <- 'Stage.Light.handler.NKA.DNKA'
        movModsNames[47] <- 'Stage.Light.Start.handler.NKA.DNKA'
        movModsNames[48] <- 'Stage.Start.handler.NKA.DNKA'
        movModsNames[49] <- 'Stage.Osm.DNKA'
        movModsNames[50] <- 'Stage.Light.Osm.DNKA'
        movModsNames[51] <- 'Stage.Start.Osm.DNKA'
        movModsNames[52] <- 'Stage.handler.Osm.DNKA'
        movModsNames[53] <- 'Stage.Light.Start.Osm.DNKA'
        movModsNames[54] <- 'Stage.Light.handler.Osm.DNKA'
        movModsNames[55] <- 'Stage.Light.Start.handler.Osm.DNKA'
        movModsNames[56] <- 'Stage.Start.handler.Osm.DNKA'
        movModsNames[57] <- 'Stage.NKA.Osm.DNKA'
        movModsNames[58] <- 'Stage.Light.NKA.Osm.DNKA'
        movModsNames[59] <- 'Stage.Start.NKA.Osm.DNKA'
        movModsNames[60] <- 'Stage.handler.NKA.Osm.DNKA'
        movModsNames[61] <- 'Stage.Light.Start.NKA.Osm.DNKA'
        movModsNames[62] <- 'Stage.Light.handler.NKA.Osm.DNKA'
        movModsNames[63] <- 'Stage.Light.Start.handler.NKA.Osm.DNKA'
        movModsNames[64] <- 'Stage.Start.handler.NKA.Osm.DNKA'

	# Print a table of model-selection results for movements
	  movRes <- aic.table.nb(movMods, movModsNames)
		head(aic.table.nb(movMods, movModsNames), 10)

# Approximate c-hat for each of the models in each set
  # Create a fxn to calculate c-hat
    approx.chat <- function ( object ){
      with(object, sum((weights*residuals^2)[weights>0])/df.residual)
    }

# PRINT THE BEST MODELS FOR EACH SET
	# Preference
	  prefC <- summary(prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]])
	  #Anova(prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]])
	# Tolerance
	  tolC <- summary(tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]])
	  #Anova(tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]])
	# Movements
	  movC <- summary(movMods[[which(movModsNames==as.character(movRes[1,1]))]])
	  #Anova(movMods[[which(movModsNames==as.character(movRes[1,1]))]])

	# Make the coefficients into a single table
	  coefficients <- rbind(
	    summary(prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]])$coefficients,
	    summary(tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]])$coefficients,
		  summary(movMods[[which(movModsNames==as.character(movRes[1,1]))]])$coefficients
	  )
    coefficients <- data.frame(c(rep('Preference', 5),
    	rep('Tolerance', 7), rep('Movement', 7)), row.names(coefficients),
    	coefficients, row.names=NULL)
	  names(coefficients) <- c('Response', 'Parameter', 'Estimate', 'S.E.',
	  	't-statistic', 'p')
	  coefficients

# PAIRWISE COMPARISONS OF TREATMENT LEVEL MEANS
  # Behavior
	  # Preference by treatment group
	    TukeyHSD(aov(LogtPref ~ Stage, data=clean))
	  # Tolerance by treatment group
	    TukeyHSD(aov(log(MaxTime) ~ Stage, data=clean))
	  # Movement by treatment group
	    TukeyHSD(aov(log(Switches) ~ Stage, data=clean))

	# Physiology
	  # ATPase activity by treatment group
 	    summary(lm(ATPaseActivity1 ~ Stage, data=clean))
    # Change in ATPase activity by treatment group
	    summary(lm(DeltaATP ~ Stage, data=clean2))
	  # Osmolality by treatment group
	    summary(lm(Osmolality ~ Stage, data=clean2))

	  # ATPase activity by treatment group
 	    TukeyHSD(aov(ATPaseActivity1 ~ Stage, data=clean))
    # Change in ATPase activity by treatment group
	    TukeyHSD(aov(DeltaATP ~ Stage, data=clean2))
	  # Osmolality by treatment group
	    TukeyHSD(aov(Osmolality ~ Stage, data=clean2))

# DESCRIPTIVE STATISTICS FOR PHYSIOLOGY
	# ATPase activity
	  # Early group
	    mean(clean$ATPaseActivity1[clean$Stage=='Presmolt'])
	    sd(clean$ATPaseActivity1[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$ATPaseActivity1[clean$Stage=='Smolt'])
	    sd(clean$ATPaseActivity1[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$ATPaseActivity1[clean$Stage=='Postsmolt'])
	    sd(clean$ATPaseActivity1[clean$Stage=='Postsmolt'])

	# Change in ATPase activity
	  # Early group
	    mean(clean$DeltaATP[clean$Stage=='Presmolt'])
	    sd(clean$DeltaATP[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$DeltaATP[clean$Stage=='Smolt'])
	    sd(clean$DeltaATP[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$DeltaATP[clean$Stage=='Postsmolt'])
	    sd(clean$DeltaATP[clean$Stage=='Postsmolt'])

	# Plasma osmolality
	  # Early group
	    mean(clean$Osmolality[clean$Stage=='Presmolt'])
	    sd(clean$Osmolality[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$Osmolality[clean$Stage=='Smolt'])
	    sd(clean$Osmolality[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$Osmolality[clean$Stage=='Postsmolt'])
	    sd(clean$Osmolality[clean$Stage=='Postsmolt'])

# DESCRIPTIVE STATISTICS FOR BEHAVIOR
	# PREFERENCE
	  # Early group
	    mean(clean$pref[clean$Stage=='Presmolt'])
	    sd(clean$pref[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$pref[clean$Stage=='Smolt'])
	    sd(clean$pref[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$pref[clean$Stage=='Postsmolt'])
	    sd(clean$pref[clean$Stage=='Postsmolt'])

	# TOLERANCE
	  # Early group
	    mean(clean$MaxTime[clean$Stage=='Presmolt'])
	    sd(clean$MaxTime[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$MaxTime[clean$Stage=='Smolt'])
	    sd(clean$MaxTime[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$MaxTime[clean$Stage=='Postsmolt'])
	    sd(clean$MaxTime[clean$Stage=='Postsmolt'])

	# MOVEMENTS
	  # Early group
	    mean(clean$Switches[clean$Stage=='Presmolt'])
	    sd(clean$Switches[clean$Stage=='Presmolt'])
	  # Middle group
	    mean(clean$Switches[clean$Stage=='Smolt'])
	    sd(clean$Switches[clean$Stage=='Smolt'])
	  # Late group
	    mean(clean$Switches[clean$Stage=='Postsmolt'])
	    sd(clean$Switches[clean$Stage=='Postsmolt'])

# PLOTTING CODE FOR FIGURES IN PAPER
# FIGURE 3- TREATMENT EFFECTS ON PHYSIOLOGY
	# PHYSIOLOGY
	  # Select and/or create a .tiff file to which the plot will be written
  	  tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/Figure3.tiff',
  	  	width=3600, height=6000, pointsize=18, compression="lzw", res=500)
	  # Plot atpase activity by treatment group
	    # Create the plot
  	    par(mar=c(3, 9, 2, 1), mfrow=c(3,1))
  	    boxplot(clean$ATPaseActivity1~clean$Stage, col="gray87", ylim=c(0,12),
		      ylab="", xlab="",yaxt='n',	xaxt='n')
	      axis(side=2, at=c(0, 2, 4, 6, 8, 10, 12),
          c("0.0","2.0", "4.0", "6.0", "8.0", "10.0", "12.0"), las=2, cex.axis=1.5)
	      #axis(side=3, at=c(1,2,3), c("A", "B", "AB"), cex.axis=1.5, tck=F,
	      	#padj=1)
	      mtext(side=2, line=6, expression(paste('NKA activity')), cex=1.5)
		    text(.5 ,0.90*12, "(a)", cex=2, adj=c(0,0))

	  # Plot DELTA atpase activity by treatment group
	    # Create the plot
  	    par(mar=c(4,9,1,1))
  	    boxplot(clean$DeltaATP~clean$Stage, col="gray87",
		      ylab="", xlab="",yaxt='n',	xaxt='n')
	      axis(side=2, at=c(-6, -4, -2, 0, 2, 4, 6),
          c('-6.0', '-4.0', '-2.0', '0.0', "2.0", "4.0", "6.0"), las=2,
	      	cex.axis=1.5)
	      #axis(side=3, at=c(1,2,3), c("A", "B", "AB"), cex.axis=1.5, tck=F,
	      	#padj=1)
	      mtext(side=2, line=6, expression(paste(Delta, 'NKA activity')), cex=1.5)
			  text(.5 ,0.90*6, "(b)", cex=2, adj=c(0,0))
	  # Plot final osmolality by treatment group
	    # Create the plot
  	    par(mar=c(5,9,0,1))
  	    boxplot(clean$Osmolality~clean$Stage, col="gray87",
		      ylab="", xlab="",yaxt='n',	xaxt='n', ylim=c(300, 500))
	      axis(side=2, at=c(300, 350, 400, 450, 500),
          c('300', '350', '400', '450', '500'), las=2, cex.axis=1.5)
	      axis(side=1, at=c(1,2,3), c("Presmolt", "Smolt", "Postsmolt"), cex.axis=1.5)
	      #axis(side=3, at=c(1,2,3), c("A", "B", "C"), cex.axis=1.5, tck=F, padj=1)
	      mtext(side=1, line=3.5, "Treatment group", cex=1.5)
	      mtext(side=2, line=6, expression(paste('Osmolality')), cex=1.5)
				text(.5, 300+0.90*200, "(c)", cex=2, adj=c(0,0))
	  # Turn off graphics and save the file
	    dev.off()

# FIGURE 4- PREFERENCE AND RESIDENCE BY DEVELOPMENTAL STAGE
  tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/Figure4.tiff',
  	width=6000, height=10000, pointsize=18, compression="lzw", res=500)
# Make a boxplot of the model predictions for preference
  prefM <- prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]]
  prefP <- predict(prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]])
  par(mar=c(3,7,3,1), mfrow=c(2,1))
  boxplot(prefP~clean$Stage, yaxt='n', cex.axis=1.5, col='gray87')
  axis(2, at=c(-.82, -.6, -.4, -.2, 0, .2), c('0.30', '0.35', '0.40',
    '0.45', '0.50', '0.55'), las=2, cex.axis=1.5)
  mtext(side=2, 'SW preference (proportion)', cex=2, line=5)
  #mtext(side=1, 'Developmental stage', cex=2, line=4)
# Make a boxplot of the model predictions for residence
  tolM <- tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]]
  tolP <- exp(predict(tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]]
  	))
  par(mar=c(7,7,0,1))
  boxplot(tolP~clean$Stage, cex.axis=1.5, col='gray87', yaxt='n')
  axis(2, at=c(seq(100, 500, 100)), c('100','200','300','400','500'),
  	las=2, cex.axis=1.5)
  mtext(side=2, 'SW residence (seconds)', cex=2, line=5)
  mtext(side=1, 'Developmental stage', cex=2, line=4)
  dev.off()

# FIGURE 5- Effect of NKA on residence
	# Get new values from range of observed values
    nA <- seq(min(clean$ATPaseActivity1), max(clean$ATPaseActivity1), .01)
	# Make predictions
	  meanTA <- exp(tolC$coefficients[1,1] + tolC$coefficients[6,1]*nA)
	  upperTA <- exp((tolC$coefficients[1,1] + 1.96*tolC$coefficients[1,2]) +
	    (tolC$coefficients[6,1]+1.96*tolC$coefficients[6,2])*nA)
	  lowerTA <- exp((tolC$coefficients[1,1] - 1.96*tolC$coefficients[1,2]) +
	    (tolC$coefficients[6,1]-1.96*tolC$coefficients[6,2])*nA)
  # Calculate effect size of NKA over range of observations
    max(meanT)-min(meanT) # Mean effect size
	  max(lowerT)-min(lowerT) # Lower C.I.
	  max(upperT)-min(upperT) # Upper C.I.

  # Effect of change in NKA on residence
	# Get new values from range of observed values
    nDA <- seq(min(clean$DeltaATP), max(clean$DeltaATP), .01)
	# Make predictions
	  meanT <- exp(tolC$coefficients[1, 1] +
	  	tolC$coefficients[5, 1] * median(clean$handler) +
	  	tolC$coefficients[6, 1] * median(clean$ATPaseActivity1) +
	  	tolC$coefficients[7, 1] * nDA)

	  upperT <- exp((tolC$coefficients[1, 1] + 1.96*tolC$coefficients[1, 2]) +
	    (tolC$coefficients[5, 1] +
	      1.96 * tolC$coefficients[5,2]) * median(clean$handler) +
	  	(tolC$coefficients[6, 1] +
	  	  1.96 * tolC$coefficients[6,2]) * median(clean$ATPaseActivity1) +
	    (tolC$coefficients[7, 1] +
	    	1.96 * tolC$coefficients[7,2]) * nDA)

	  lowerT <- exp((tolC$coefficients[1, 1] - 1.96*tolC$coefficients[1, 2]) +
	    (tolC$coefficients[5, 1] - 1.96 * tolC$coefficients[5, 2]) *
	  		median(clean$handler) +
	  	(tolC$coefficients[6, 1] - 1.96 * tolC$coefficients[6, 2]) *
	  		median(clean$ATPaseActivity1) +
	    (tolC$coefficients[7, 1] - 1.96 * tolC$coefficients[7, 2]) * nDA)

	# Calculate effect size of NKA over range of observations
    max(meanT)-min(meanT) # Mean effect size
		max(lowerT)-min(lowerT) # Lower C.I.
	  max(upperT)-min(upperT) # Upper C.I.

 # Plot nka activity vs fitted values
   tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/Figure5.tiff',
  	 width=12000, height=5000, pointsize=18, compression="lzw", res=500)
   par(mar=c(5,7,1,1), mfrow=c(1,2))
   plot(tolP~clean$ATPaseActivity1, xaxt='n', xlim=c(1, 10), yaxt='n',
   	ylim=c(0,500), ylab='', xlab='')
   par(new=TRUE)
   plot(nA, meanTA, type='l', xlab='', ylab='', yaxt='n', xaxt='n', lty=1,
	   lwd=3, ylim=c(0, 500), xlim=c(1, 10))
	 par(new=TRUE)
	 plot(nA, upperTA, type='l', xlab='', ylab='', yaxt='n', xaxt='n',
	   lty=2, lwd=3, ylim=c(0, 500), xlim=c(1, 10))
	 par(new=TRUE)
	 plot(nA, lowerTA, type='l', xlab='', ylab='', yaxt='n', xaxt='n',
	   lty=2, lwd=3, ylim=c(0, 500), xlim=c(1, 10))
	 axis(side=1, at=c(seq(1, 10, 1)), c('1', '2', '3', '4', '5', '6', '7',
		 '8', '9', '10'), cex.axis=1.5)
	 mtext(side=1, line=3, "gill NKA activity", cex=1.5)
	 axis(side=2, at=c(seq(100,500,100)), as.character(c(seq(100,500,100))),
	 	las=2, cex.axis=1.5)
	 mtext(side=2, 'Residence', cex=2, line=5)
   text(x=1.3, y=490, '(a)', cex=1.75)
  # Plot Delta nka activity vs fitted values
    par(mar=c(5,6,1,2))
    plot(tolP~clean$DeltaATP, xaxt='n', xlim=c(-6,6), yaxt='n',
   	  ylim=c(0,500), ylab='', xlab='')
    par(new=TRUE)
	  plot(nDA, meanT, type='l', xlab='', ylab='', yaxt='n', xaxt='n',
	    lty=1, lwd=3, ylim=c(0, 500))
	  par(new=TRUE)
	  plot(nDA, upperT, type='l', xlab='', ylab='', yaxt='n', xaxt='n',
	    lty=2, lwd=3, ylim=c(0, 500))
	  par(new=TRUE)
	  plot(nDA, lowerT, type='l', xlab='', ylab='', yaxt='n', xaxt='n',
	    lty=2, lwd=3, ylim=c(0, 500))
		axis(side=1, at=c(seq(-6, 6, 1)), c(as.character(seq(-6, 6, 1))),
		  cex.axis=1.5)
		mtext(side=1, line=3, expression(paste(Delta,"NKA activity")),
		  cex=1.5)
	  axis(side=2, at=c(seq(0, 500, 100)),
	    c(as.character(seq(0, 500, 100))), las=2, cex.axis=1.5)
    text(x=-5.7, y=490, '(b)', cex=1.75)
    dev.off()

# MODEL CROSS VALIDATION
# Get coefficients for the best model in each set and plot predictive R-squared
# values from cross-validations
	# Preference
	  prefC <- summary(prefMods[[which(prefModsNames==as.character(prefRes[1,1]))]])
    # Cross validation for the preference model
      r = c()
      pred = c()
      obs = c()
      for ( i in 1:1000){
      # Test predictions
        # Subset the data
          splits = sample(1:nrow(clean),nrow(clean)*.90)
          data1 = clean[splits,]
          data2 = clean[-splits,]
          obs = append(obs,data2$Switches)
        # Run mod again with no random effect
          m=movMods[[which(prefModsNames==as.character(prefRes[1,1]))]]
          preds = c(as.numeric(predict(m,newdata=data2)))
          pred = append(pred,preds)
          r[i] = c(as.numeric(summary(lm(data2$pref~exp(preds)))[8]))
      }
	    # Plot histogram of predictive R-squared
        hist(r)
	    # Get quantiles for predictive R-squared
	      quantile(r)

	# Tolerance
	  tolC <- summary(tolMods[[which(tolModsNames==as.character(tolRes[1,1]))]])
    # Cross validation for the tolerance model
      r = c()
      pred = c()
      obs = c()
      for ( i in 1:1000){
      # Test predictions
        # Subset the data
          splits = sample(1:nrow(clean),nrow(clean)*.90)
          data1 = clean[splits,]
          data2 = clean[-splits,]
          obs = append(obs,data2$Switches)
        # Run mod again with no random effect
          m=movMods[[which(tolModsNames==as.character(tolRes[1,1]))]]
          preds = c(as.numeric(predict(m,newdata=data2)))
          pred = append(pred,preds)
          r[i] = c(as.numeric(summary(lm(data2$MaxTime~exp(preds)))[8]))
      }
	    # Plot histogram of predictive R-squared
        hist(r)
	    # Get quantiles for predictive R-squared
	      quantile(r)

	# Movements
	  movC <- summary(movMods[[which(movModsNames==as.character(movRes[1,1]))]])
    # Cross validation for the movement model
      r = c()
      pred = c()
      obs = c()
      for ( i in 1:1000){
      # Test predictions
        # Subset the data
          splits = sample(1:nrow(clean),nrow(clean)*.90)
          data1 = clean[splits,]
          data2 = clean[-splits,]
          obs = append(obs,data2$Switches)
        # Run mod again with no random effect
          m=movMods[[which(movModsNames==as.character(movRes[1,1]))]]
          preds = c(as.numeric(predict(m,newdata=data2)))
          pred = append(pred,preds)
          r[i] = c(as.numeric(summary(lm(data2$Switches~exp(preds)))[8]))
      }
	    # Plot histogram of predictive R-squared
        hist(r)
	    # Get quantiles for predictive R-squared
	      quantile(r)
################################################################################

################################################################################
# Descriptive statistics for salinity preference study
#
# INPUT FILENAME(S):  'FishTank.rda' (tank), need fish.files object created
#                      above!!
# MY FUNCTIONS:
# NAMED OUPUT:         Who cares
# OUTPUT FILENAME(S):  Nope
################################################################################
# FL statistics
  mean(clean$FL[clean$Stage=='Presmolt'])
  mean(clean$FL[clean$Stage=='Smolt'])
  mean(clean$FL[clean$Stage=='Postsmolt'])

  sd(clean$FL[clean$Stage=='Presmolt'])
  sd(clean$FL[clean$Stage=='Smolt'])
  sd(clean$FL[clean$Stage=='Postsmolt'])

# Mass statistics
  mean(clean$Mass[clean$Stage=='Presmolt'])
  mean(clean$Mass[clean$Stage=='Smolt'])
  mean(clean$Mass[clean$Stage=='Postsmolt'])

  sd(clean$Mass[clean$Stage=='Presmolt'])
  sd(clean$Mass[clean$Stage=='Smolt'])
  sd(clean$Mass[clean$Stage=='Postsmolt'])

# K statistics
  mean(clean$K[clean$Stage=='Presmolt'])
  mean(clean$K[clean$Stage=='Smolt'])
  mean(clean$K[clean$Stage=='Postsmolt'])

  sd(clean$K[clean$Stage=='Presmolt'])
  sd(clean$K[clean$Stage=='Smolt'])
  sd(clean$K[clean$Stage=='Postsmolt'])

# ATPaseActivity1 statistics
  median(clean$ATPaseActivity1[clean$Stage=='Presmolt'])
  median(clean$ATPaseActivity1[clean$Stage=='Smolt'])
  median(clean$ATPaseActivity1[clean$Stage=='Postsmolt'])

  sd(clean$ATPaseActivity1[clean$Stage=='Presmolt'])
  sd(clean$ATPaseActivity1[clean$Stage=='Smolt'])
  sd(clean$ATPaseActivity1[clean$Stage=='Postsmolt'])

# ATPaseActivity2 statistics
  median(clean$ATPaseActivity2[clean$Stage=='Presmolt'])
  median(clean$ATPaseActivity2[clean$Stage=='Smolt'])
  median(clean$ATPaseActivity2[clean$Stage=='Postsmolt'])

  sd(clean$ATPaseActivity2[clean$Stage=='Presmolt'])
  sd(clean$ATPaseActivity2[clean$Stage=='Smolt'])
  sd(clean$ATPaseActivity2[clean$Stage=='Postsmolt'])

# Osmolality statistics
  median(clean$Osmolality[clean$Stage=='Presmolt'])
  median(clean$Osmolality[clean$Stage=='Smolt'])
  median(clean$Osmolality[clean$Stage=='Postsmolt'])

  sd(clean$Osmolality[clean$Stage=='Presmolt'])
  sd(clean$Osmolality[clean$Stage=='Smolt'])
  sd(clean$Osmolality[clean$Stage=='Postsmolt'])

# Delta NKA
  median((clean$ATPaseActivity2[clean$Stage=='Presmolt'])-
  	(clean$ATPaseActivity1[clean$Stage=='Presmolt']))
  median((clean$ATPaseActivity2[clean$Stage=='Smolt'])-
  	(clean$ATPaseActivity1[clean$Stage=='Smolt']))
  median((clean$ATPaseActivity2[clean$Stage=='Postsmolt'])-
  	(clean$ATPaseActivity1[clean$Stage=='Postsmolt']))
################################################################################
} # Code-folding marker
# END PART 6. SALINITY PREFERENCE LABORATORY STUDY------------------------------


# PART 7. DEPTH-TAGGED FISH ANALYSIS--------------------------------------------
{ # Code-folding marker
################################################################################
# Data manipulation and analysis for 2013 depth-tagged fish, with respect to
# depth use vs river kilometer, and diurnal/tidal cycle
#
# INPUT FILENAME(S):  'DepthDataForDepth-TaggedFish.rda'
# MY FUNCTIONS:
# NAMED OUPUT:        'suns', 'dtTags',
#
# OUTPUT FILENAME(S): 'dtTagsWithLD.rda'
################################################################################
# Install and load necessary packages
	#install.packages('reshape')
	#install.packages('reshape2')
	#install.packages('tcltk')
	#install.packages('tcltk2')
	#install.packages('StreamMetabolism)
	#install.packages('lubridate')
	require(reshape)
	require(reshape2)
	require(tcltk)
	require(tcltk2)
	require(StreamMetabolism)
	require(lubridate)
	require(glmmADMB)

# Set working directory
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/DepthTagData")

# Read in the data file for depth-tagged fish released in the estuary in 2013
# The df is named d13DepthTags
	load('DepthDataForDepth-TaggedFish.rda')

# Sort the dataframe by TagID then RKM
  dtTags <- d13DepthTags
  TagsRKM<- dtTags[with(dtTags, order(TagID,-(RKM))), ]

# Make dfs for each fish with all of the data
  T8906<-TagsRKM[TagsRKM$TagID=="8906-2013",]
  T8907<-TagsRKM[TagsRKM$TagID=="8907-2013",]
  T8908<-TagsRKM[TagsRKM$TagID=="8908-2013",]
  T8909<-TagsRKM[TagsRKM$TagID=="8909-2013",]
  T8910<-TagsRKM[TagsRKM$TagID=="8910-2013",]
  T8911<-TagsRKM[TagsRKM$TagID=="8911-2013",]
  T8912<-TagsRKM[TagsRKM$TagID=="8912-2013",]
  T8913<-TagsRKM[TagsRKM$TagID=="8913-2013",]
  T8914<-TagsRKM[TagsRKM$TagID=="8914-2013",]
  T8915<-TagsRKM[TagsRKM$TagID=="8915-2013",]

# Create a file to hold the graphic once it's finished
  tiff( "C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/RKMvsDepthbox.tiff",
  	width = 6000 , height = 5000 , pointsize = 18, compression = "lzw" ,
  	res = 400 )
	  par(mar=c(8,7,1,1))
    boxplot(TagsRKM$Depth~TagsRKM$RKM,ylim=c(32,0),
    	at=c(-sort(unique(TagsRKM$RKM))), ylab="", xlab="",
    	yaxt='n', cex.lab=1.5, cex.axis=1.5, cex = 1.2, xaxt="n", col="gray87",
    	pch=21, bg="gray50")
    axis(1,pretty(c(-40,20)),bg='black', labels=c("40", "30", "20", "10", "0",
      "-10", "-20"),xaxs="i",yaxs="i", cex.axis=2,las=1)
	  axis(2, at=c(0, 5, 10, 15, 20, 25, 30), c('0', '5', '10', '15', '20', '25',
	  	'30'), las=2, cex.axis=2)
    mtext("Upstream",1, cex=2.5, line=4, adj=0)
    mtext("Downstream",1, cex=2.5, line=4, adj=1)
	  mtext(side=1, "River kilometer", cex=3, line=6)
	  mtext(side=2, "Fish depth (m)", line=4.5, cex=3)
  dev.off()

# Do a regression to see if rkm is related to depth use
  # Create a second-order term for river kilometer in estuary
	  dtTags$RKM2<- dtTags$RKM^2

  # Create fxn to standardize covariates, 'standardize'
  	standardize=function(x,z=NULL){
      if(is.null(z)){
        return((x-mean(x))/sqrt(var(x)))
      } else {
          return((x-mean(z))/sqrt(var(z)))
        }
    }

  # Run GLMM that explores relation between depth and rkm in the estuary, do not
	# use the standardized values of rkm for now because they produce gibberish
  	res.rkm <- summary(glmmadmb(Depth ~ RKM + RKM2 +(1|TagID),
		  family='gaussian', link='inverse', data=dtTags))$coefficients

# Plot predicted depths per rkm for GLMM used to describe relationship
	# Create new sequence of standardized RKMs for prediction if the
	  nRKM <- seq(min(dtTags$RKM), max(dtTags$RKM), 0.01)

	# Create new, unstandardize RKMs for prediction and plotting
    newRKM <- nRKM

	# Predict mean depth based on RKM
    newDepth <- 1/(res.rkm[1,1] + newRKM * res.rkm[2,1] + res.rkm[3,1] *
      (newRKM^2))

  # Predict upper 95% CL for depth based on RKM
    newDepthU <- 1/((res.rkm[1,1] + res.rkm[1,2] * 1.96 ) +
	    newRKM * (res.rkm[2,1] + 1.96 *res.rkm[2,2]) +
		  (newRKM^2) * (res.rkm[3,1] + 1.96 *res.rkm[3,2]))

  # Predict lower 95% CL for depth based on RKM
	  newDepthL <- 1/((res.rkm[1,1] - res.rkm[1,2] * 1.96 ) +
	    newRKM * (res.rkm[2,1] - 1.96 *res.rkm[2,2]) +
		  (newRKM^2) * (res.rkm[3,1] - 1.96 *res.rkm[3,2]))

# Plot results of GLMM used to explore relation between depth and rkm in estuary
  tiff( "C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/RKMvsDepthFit.tiff",
  	width = 6000 , height = 5000 , pointsize = 18, compression = "lzw" ,
  	res = 400 )
	par(mar=c(8,7,1,1))
  plot(newRKM, newDepth, type='l', ylim = c(15,0), yaxt='n', xlim=c(45,-16),
  	cex.axis=2, xlab='', ylab='', lwd=3)
  par(new=TRUE)
  plot(newRKM, newDepthL, type='l', ylim = c(15,0), lty=2, ylab='', xlab='',
  	yaxt='n', xaxt='n', xlim=c(45,-16), lwd=3, col='gray40')
  par(new=TRUE)
  plot(newRKM, newDepthU, type='l', ylim = c(15,0), lty=2, ylab='', xlab='',
  	yaxt='n', xaxt='n', xlim=c(45,-16), lwd=3, col='gray40')
	axis(2, at=c(0, 5, 10, 15, 20, 25, 30), c('0', '5', '10', '15', '20', '25',
	  	'30'), las=2, cex.axis=2)
  mtext("Upstream",1, cex=2.5, line=4, adj=0)
  mtext("Downstream",1, cex=2.5, line=4, adj=1)
	mtext(side=1, "River kilometer", cex=3, line=6)
	mtext(side=2, "Fish depth (m)", line=4.5, cex=3)
	dev.off()

# Do analysis to see if depth use differed between day and night/ BY TIDES
	# Create a variable for light or dark
	  # Calculate sunrise and sunset times..THESE ARE IN UTC!!!!
      suns <- sunrise.set(lat=44.818418, long=-68.722211, timezone="EST",
      date="2005/01/01", num.days=3650)

    # Write it to a table so it's saved
      write.table(suns, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
        row.names=FALSE)

	  # Read in sunrise/sunset times
	    SS <- read.csv('C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/Environmental Data/SunriseSunsetTimes2005-2015_EST_ Veazie_Calculated.csv')

  	# Change var type for sunrise and sunset times
	    SS[ ,1] <- as.POSIXct(as.character(SS[ , 1]))
	    SS[ ,2] <- as.POSIXct(as.character(SS[ , 2]))

	  # Make a column for date in the SS table
	    SS$Date <- substr(SS[,1], start=1, stop=10)

	  # Make a column for date in the depth tags table
	    dtTags$DateTimeEST <- as.POSIXct(dtTags$DateTime + 5*60*60)
	    dtTags$Date <- substr(dtTags$DateTime, start=1, stop=10)

   # Calculate light or dark for each fish's arrival time in the estuary
   # This takes quite a while, about 8 hours!!!!!
     pb <- txtProgressBar(min = 0, max = nrow(dtTags), style = 3)
     for(i in 1:nrow(dtTags)){
       for(t in 1:nrow(SS)){
         if(dtTags$DateTime[i] > SS[SS[,3]==dtTags$Date[i], 1] &
         	 dtTags$DateTime[i] < SS[SS[,3]==dtTags$Date[i], 2]){
         	 dtTags$LD[i] <- "light"
         } else {
           dtTags$LD[i] <- "dark"
         }
       } # t
     	 Sys.sleep(0.0001)
       setTxtProgressBar(pb, i)
     } # i
  	 close(pb)

    # Write this out b/c it took so damn long!!!
	    # Set working directory
        setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/DepthTagData")

     # The df is named d13DepthTags
	     save(dtTags, file='dtTagsWithLD.rda')
	     load(file='dtTagsWithLD.rda')

	   # Get DateTime in EST as well
	     dtTags$DT2 <- dtTags$DateTime + 4*60*60

# Calculate tidal stage for each relocation event
	# Set working directory to environmental data
  	setwd("C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/Environmental Data/Penobscot River Tides")

	# Read in the table with station data
	  Stations <- read.csv('PenobscotTidalStationData.csv', header=TRUE)

	# Read in Bangor Tides for 2013...THESE DATA ARE IN UTC SO THEY MATCH DATETIME
	  BGR<- read.table('BangorTides_2013.Annual', skip=14, sep="\t", )[ , -c(5,7)]

	# Give the new df column names
	  names(BGR) <- c('Date', 'Day', 'Time', 'Pred.ft', 'Pred.cm', 'Stage')

	# Add station name to the table
	  BGR$Station <- 'BGR'

	# Create a new field for DateTime
	  BGR$DateTime <- paste(
	  	substr(BGR$Date, start=1, stop=4), "-",
	  	substr(BGR$Date, start=6, stop=7), "-",
	  	substr(BGR$Date, start=9, stop=10), " ",
	  	BGR$Time, ":", "00", sep=""
	  	)
	  BGR$DateTime <- as.POSIXct(BGR$DateTime, tz='UTC')

	# Remove all of the unecessary fields
	  BGR <- BGR[ , c(8, 6)]

	# Add date and time back to the df in useable formats
	  BGR$Date <- substr(BGR$DateTime, start=1, stop=10)
	  BGR$Time <- substr(BGR$DateTime, start=12, stop=19)

	# Clip the tidal data to match the extent of the fish data
	  BGR <- BGR[(BGR$DateTime>(min(dtTags$DateTime)-(60*60*24)) |
	    BGR$DateTime==(min(dtTags$DateTime)-(60*60*24))) &
	    (BGR$DateTime<(max(dtTags$DateTime)+(60*60*24)) |
	    BGR$DateTime==(max(dtTags$DateTime)+(60*60*24))), ]

	# Check the data, it's a small df
	  BGR

	# Change the variable type for tidal stage to char
    BGR$Stage <- as.character(BGR$Stage)

	# Add tidal stage to each line of the depth-tagged fish data
	  # Get a timestamp for the dtTags df
	    dtTags$Time <- substr(dtTags$DateTime, start=12, stop=19)

    # Convert time to numeric for quick processing
      dtTags$TimeNum <- sapply(strsplit(dtTags$Time,":"),
        function(x) {
          x <- as.numeric(x)
          x[1]+x[2]/60+x[2]/3600
        })

	  # Convert tide times to numeric, too
      BGR$TimeNum <- sapply(strsplit(BGR$Time,":"),
        function(x) {
          x <- as.numeric(x)
          x[1]+x[2]/60+x[2]/3600
        })

	  # Do a conditional loop to get tidal stage if date of fish detection matches
	  # date of tidal measurement and time of fish detection is between two tidal
	  # measurement times
	    # Set up a progress bar
	      pb <- txtProgressBar(min = 0, max = nrow(dtTags), style = 3)

	    # Run the loop
	      for(i in 1:nrow(dtTags)){ # For each detection
	    	  for(t in 1:nrow(BGR)){ # And for each tidal measurement
            if((as.numeric(dtTags$DateTime[i])>as.numeric(BGR$DateTime[t])) &
          	  (as.numeric(dtTags$DateTime[i])<as.numeric(BGR$DateTime[t+1]))
            	){

            	tide[i] <- BGR$Stage[t]

	    	    } else {
              next;
	    	    } # else
         } # t
     	   Sys.sleep(0.0001) # Time out to print progress bar
         setTxtProgressBar(pb, i)	# Update progress bar

       } # i

  	   close(pb) # Close the progress bar

# Graph vertical and horizontal movements of fish with respect to tides across
# the range of dates
  tiff( "C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables/STST.tiff",
  	width = 3000 , height = 5000 , pointsize = 18, compression = "lzw" ,
  	res = 400 )
  par(mar=c(3, 7, 3, 7), mfrow=c(3,1))
  # Fish 8908
  plot(dtTags$DateTime[tide=='H'], as.numeric(as.factor(tide[tide=='H'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='red', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[tide=='L'], as.numeric(as.factor(tide[tide=='L'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='blue', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8908-2013']+5*60*60,
	  dtTags$RKM[dtTags$TagID=='8908-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='',
	  xlab='', cex.lab=2, yaxt='n', xaxt='n', pch=19, col='black')
  axis(2, at=c(seq(-10, 40, 10)), as.character(c(seq(-10, 40, 10))),
  	las=2, cex.axis=1.5)
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8908-2013']+5*60*60,
	  dtTags$Depth[dtTags$TagID=='8908-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='', xlab='',
  	col='grey', ylim=c(32, 0), yaxt='n', xaxt='n')
  axis(4, at=c(seq(30, 0, -5)), as.character(c(seq(30, 0, -5))), las=2,
  	cex.axis=1.5)
  #mtext(side=2, 'River kilometer', line=6, cex=2)
  mtext(side=2, 'Upstream', line=3.5, adj=1, cex=1)
  #mtext(side=2, 'Downstream', line=3.5, adj=0, cex=1)
  #mtext(side=4, 'Depth (m)', line=4.5, cex=2)

	# Fish 8912
  par(mar=c(4, 7, 0, 7))
  plot(dtTags$DateTime[tide=='H'], as.numeric(as.factor(tide[tide=='H'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='red', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[tide=='L'], as.numeric(as.factor(tide[tide=='L'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='blue', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8912-2013']+5*60*60,
	  dtTags$RKM[dtTags$TagID=='8912-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='',
	  xlab='', cex.lab=2, yaxt='n', xaxt='n', pch=19, col='black')
  axis(2, at=c(seq(-10, 40, 10)), as.character(c(seq(-10, 40, 10))),
  	las=2, cex.axis=1.5)
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8912-2013']+5*60*60,
	  dtTags$Depth[dtTags$TagID=='8912-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='', xlab='',
  	col='grey', ylim=c(32, 0), yaxt='n', xaxt='n')
  axis(4, at=c(seq(30, 0, -5)), as.character(c(seq(30, 0, -5))), las=2,
  	cex.axis=1.5)
  mtext(side=2, 'River kilometer', line=3.5, cex=2)
  #mtext(side=2, 'Upsstream', line=3.5, adj=1, cex=1)
  #mtext(side=2, 'Downstream', line=3.5, adj=0, cex=1)
  mtext(side=4, 'Depth (m)', line=4.5, cex=2)

	# Fish 8915
  par(mar=c(5, 7, 0, 7))
  plot(dtTags$DateTime[tide=='H'], as.numeric(as.factor(tide[tide=='H'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='red', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[tide=='L'], as.numeric(as.factor(tide[tide=='L'])),
  	ylab='', yaxt='n', xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)),
  	pch=19, col='blue', xlab='', xaxt='n')
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8915-2013']+5*60*60,
	  dtTags$RKM[dtTags$TagID=='8915-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='', xlab='',
  	cex.lab=2, yaxt='n', pch=19, col='black', xaxt='n')
  axis(2, at=c(seq(-10, 40, 10)), as.character(c(seq(-10, 40, 10))),
  	las=2, cex.axis=1.5)
  par(new=TRUE)
  plot(dtTags$DateTime[dtTags$TagID=='8915-2013']+5*60*60,
	  dtTags$Depth[dtTags$TagID=='8915-2013'],
	  xlim=c(min(dtTags$DateTime), max(dtTags$DateTime)), ylab='', xlab='',
  	col='grey', ylim=c(32, 0), yaxt='n', xaxt='n')
	axis.POSIXct(side=1, x=sort(unique(dtTags$Date)), format="%m-%d",
		cex.axis=1.5)
  axis(4, at=c(seq(30, 0, -5)), as.character(c(seq(30, 0, -5))),
  	las=2, cex.axis=1.5)
  mtext(side=1, 'Date', line=3.5, cex=2)
  #mtext(side=2, 'River kilometer', line=6, cex=2)
  #mtext(side=2, 'Upsstream', line=3.5, adj=1, cex=1)
  mtext(side=2, 'Downstream', line=3.5, adj=0, cex=1)
  #mtext(side=4, 'Depth (m)', line=4.5, cex=2)

# Turn off graphics device
	dev.off()

# ESTUARY MOVEMENTS VERSUS NKA ACTIVITY
# Add the tidal data to the dtTags data frame
  dtTags <- data.frame(dtTags, tide)

# Write this out AGAIN b/c it took so damn long!!!
	# Set working directory
    setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/DepthTagData")

  # The df is named d13DepthTags
	  save(dtTags, file='dtTagsWithLD.rda')

# See if fish with higher ATPase activity arrived at full SW earlier
	# Make a df with TagID and ATPase activity
    fish.stats <- data.frame(unique(dtTags$TagID),
    	unique(dtTags$ATPaseActivity))
	# Give it reasonable names now
    names(fish.stats)<-c('TagID', 'ATP')
  # Make an empty vector to hold travel time
    Arrival.hrs <- c()
  # Caclulate travel time through upper estuary for each fish
    for(i in 1:nrow(fish.stats)){
		  Arrival.hrs[i] <- (min(as.numeric(
		  	dtTags$DateTime[dtTags$TagID==fish.stats$TagID[i] &
		    dtTags$RKM==30]))-as.numeric(dtTags$ReleaseDate[1]))/60/60
	  }
  # Combine TagID, ATP, and travel time into a single df
    fish.stats <- data.frame(fish.stats, Arrival.hrs)

  # Run a model to see if travel time to rkm 30 is related to NKA activity
	  ATP.arrival <- lm(Arrival.hrs~ATP, data=fish.stats)
	  summary(ATP.arrival)

  # Make an empty vector to hold travel time
    MidTraverse.hrs <- c()
  # Caclulate travel time through upper estuary for each fish
    for(i in 1:nrow(fish.stats)){
		  MidTraverse.hrs[i] <-
    		(min(as.numeric(dtTags$DateTime[dtTags$TagID==fish.stats$TagID[i] &
		    (dtTags$RKM<6 | dtTags$RKM==6)]))-
    		(min(as.numeric(dtTags$DateTime[dtTags$TagID==fish.stats$TagID[i] &
		    dtTags$RKM==30]))))/60/60
	  }
  # Combine TagID, ATP, and travel time into a single df
    fish.stats <- data.frame(fish.stats, MidTraverse.hrs)
  # Run a model to see if travel time through lower est is related to NKA activity
	  ATP.traverse <- lm(MidTraverse.hrs~ATP, data=fish.stats)

# See if fish were deeper on incoming than outgoing tides using a t-test
	t.test(dtTags$Depth[dtTags$tide=='H'], dtTags$Depth[dtTags$tide=='L'])

# Get mean and SD number of reversals by fish during migration
# These are ordered by TagID
  mean(c(5,3,1,3,1,2,3,3,2,4))
  sd(c(5,3,1,3,1,2,3,3,2,4))
################################################################################

################################################################################
# Sonde data manipulation for depth-tagged fish
#
# INPUT FILENAME(S):  '2013SondeCasts.csv', 'allcastsXXXprocessed.cnv'
# MY FUNCTIONS:       'rdCNV'
# NAMED OUPUT:        'sondes', 'sondeData'
#
# OUTPUT FILENAME(S): 'SondeData.txt'
################################################################################
# FUNCTION DEFINITION
# Write function to scan file directory and get list of .DAT file names
  rd <- function (path = getwd(), pattern = NULL, all.files = FALSE,
  	full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
  	include.dirs = FALSE, no.. = FALSE) .Internal(list.files(path, pattern,
  	all.files, full.names, recursive, ignore.case, include.dirs, no..))

# DATA MANIPULATION
# Read in the data for sonde casts
  ctdCasts <- read.csv('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/2013SondeCasts.csv')

# Change name for cast ID
  names(ctdCasts)[4] <- 'Cast'

# Import Sonde data
  # Make a vector to hold the filenames for raw data
    setwd('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/SondeData')

  # Read in the Sonde data
    pb <- txtProgressBar(min = 0, max = length(rd()), style = 3) # Progress
    for(i in 1:length(rd())){
      sondes <- read.table(file=as.character(rd())[i], skip=6, header=FALSE)
      sondes$Cast <- as.numeric(substr(as.character(rd())[i], start=9,
      	stop=11))
      write.table(sondes,
   	    file='C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/SondeData.txt',
   	    append=TRUE, sep=",", col.names=F, row.names=F)
      Sys.sleep(0.001) # Time out for updating meter
      setTxtProgressBar(pb, i) # Update progress meter
    }
    close(pb) # Close progress meter
    head(sondes) # Check that it worked

    # Read in the whole file now
		  setwd('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis')
      sondes <- read.csv('SondeData.txt', header=FALSE)

    # Give the sonde data new names
      names(sondes) <- c('Conductivity.Sm', 'Conductivity.mScm',
      	'Conductivity.uScm', 'DepthSW', 'DepthFW', 'DescentRate', 'Temp',
      	'SoundVelocity.Chen', 'Sound Velocity.Delgrosso',
      	'Sound Velocity.Wilson', 'Salinity.PSU', 'TimeElapsed.seconds', 'Flag',
        'Cast')

# Merge the sonde data with the casts data
  sondeData <- merge(ctdCasts, sondes, by='Cast')

# Remove values from the surface or in the air
  sondeData <- sondeData[sondeData$DepthFW > 1 | sondeData$DepthFW == 1, ]

# Save the data as an rda
  save(sondeData, file='2013sondeData.rda')

# Get rid of the text file b/c it's not needed
  file.remove('SondeData.txt')
################################################################################

################################################################################
# DST-CTD data manipulation for depth-tagged fish analysis
#
# INPUT FILENAME(S):  'CTD Deployment.csv', '*.DAT' (CTD data files)
# MY FUNCTIONS:       'rd'
# NAMED OUPUT:        'ctds', 'ctdDeps'
#
# OUTPUT FILENAME(S): 'ctdData.txt'
################################################################################
# FUNCTION DEFINITION
# Write fxn to strip date out of each of the filenames
  substrRight <- function(x, n){
    substr(x, nchar(x)-2*n+1, nchar(x)-n)
  }

# Write function to scan file directory and get list of all file names
  rdDAT <- function (path = getwd(), pattern = '.DAT', all.files = FALSE,
  	full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
  	include.dirs = FALSE, no.. = FALSE) .Internal(list.files(path, pattern,
  	all.files, full.names, recursive, ignore.case, include.dirs, no..))

# Read in the data CTD Deployments
  ctdDeps <- read.csv('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/CTD Deployment.csv')

# Change names for CTD deployments
  names(ctdDeps)[4:8] <- c('Location', 'LocationCode', 'Position', 'Northing',
  	'Easting')

# Import CTD data
  # Make a vector to hold the filenames for raw data
    setwd('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/DSTCTDdata')
  # Read in the CTD data
    pb <- txtProgressBar(min = 0, max = length(rdDAT()), style = 3) # Progress
    for(i in 1:length(rdDAT())){
      ctds <- read.table(file=as.character(rdDAT())[i], header=FALSE)[, -1]
    	if(ncol(ctds) < 5){ ctds[ , 5] <- 'NA'}
      ctds$CTD<- as.integer(substrRight(rdDAT()[i], 4))
    	ctds$DatTimeEST <- paste('20', substr(ctds[, 1], start=7, stop=8 ), '-',
    		substr(ctds[ , 1], start=4, stop=5 ), '-',
    		substr(ctds[ , 1], start=1, stop=2 ), ' ',
    		ctds[ , 2], sep='')
    	ctds$DateTimeUTC <- as.POSIXct(ctds$DatTimeEST, tz='EST') + 4*60*60
    	ctds <- ctds[ , c(6, 7, 8, 3:5)]
      write.table(ctds,
   	    file='C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/ctdData.txt',
   	    append=TRUE, sep=",", col.names=F, row.names=F)
      Sys.sleep(0.001) # Time out for updating meter
      setTxtProgressBar(pb, i) # Update progress meter
    }
    close(pb) # Close progress meter
    head(ctds) # Check that it worked

    # Read in the whole file now
		  setwd('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis')
      ctds <- read.csv('ctdData.txt', header=FALSE)

    # Give the sonde data new names
      names(ctds) <- c('CTD', 'DateTimeEST', 'DateTimeUTC', 'Temp', 'Depth',
      	'Salinity')

# Merge the sonde data with the casts data
  ctdDeps$CTD <- as.character(ctdDeps$CTD)
  ctds$CTD <- as.character(ctds$CTD)
  ctdData <- merge(ctdDeps, ctds, by='CTD')

# Remove values from the surface or in the air
  #ctdData <- ctdData[ctdData$Depth > 0, ]

# Remove values greater than possible (17 out of 180K)
  ctdData <- ctdData[ctdData$Salinity < 35 | ctdData$Salinity==35,]

# Remove values outside the range of dates for deployment
	ctdData$Deployed <- as.POSIXct(as.character(ctdData$Deployed))
	ctdData$Retrieved <- as.POSIXct(as.character(ctdData$Retrieved))
	ctdData$DateTimeUTC <- as.POSIXct(as.character(ctdData$DateTimeUTC))
  ctdData <- ctdData[ctdData$DateTimeUTC < (ctdData$Retrieved + 24*60*60) &
  		ctdData$DateTimeUTC > (ctdData$Deployed - 24*60*60),]
	nrow(ctdData)
  str(ctdData)
  ctdData <- ctdData[ctdData$DateTimeUTC > (min(dtTags$DT2)-1) &
  		ctdData$DateTimeUTC < (max(dtTags$DT2)+1),]

# Save the data as an rda
  save(ctdData, file='2013ctdData.rda')

# Get rid of the text file b/c it is no longer needed
  file.remove('ctdData.txt')
################################################################################

################################################################################
# DST-CTD data manipulation for depth-tagged fish analysis
#
# INPUT FILENAME(S):  'CTD Deployment.csv', '*.DAT' (CTD data files)
# MY FUNCTIONS:       'rd'
# NAMED OUPUT:        'ctds', 'ctdDeps'
#
# OUTPUT FILENAME(S): 'ctdData.txt'
################################################################################
# Read in the CTD data
  setwd('C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis')
  load(file='2013ctdData.rda')

# Read in the depth-tagged fish data
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Analysis/DepthTagData")
	load(file='dtTagsWithLD.rda')

# Create a file to hold graph
  setwd("C:/Users/Dan/Desktop/Manuscripts/Preference study/Figures & Tables")
   tiff( "Figure6.tiff",
   	width = 3250 , height = 5000 , pointsize = 18, compression = "lzw" ,
   	res = 400 )

# Make plot of probabilities that [SW] at estuary locations is greater than
# XX PSU
  # Read in depth vs RKM data
    rdepth <- read.csv('DepthAtRKM.csv')
  # SALINITY DATA (OBSERVED)
	  par(mar=c(4,7,3,8), mfrow=c(3,1))
    plot(x=ctdData$RKM[ctdData$Position=='BOTTOM'],
    	y=ctdData$Salinity[ctdData$Position=='BOTTOM'],
    	yaxt='n', pch=19, col='black', ylim=c(0,30),
  	  xlim=c(43,-16), xaxt='n', xlab='', ylab='', yaxt='n')
		par(new=TRUE)
    plot(x=(ctdData$RKM[ctdData$Position=='TOP']-.25),
    	y=ctdData$Salinity[ctdData$Position=='TOP'],
    	yaxt='n', pch=19, xlim=c(43,-16), xaxt='n', xlab='', ylab='', yaxt='n',
    	col='grey', ylim=c(0,30))
	  axis(side=2, at=seq(0,35,5), as.character(seq(0,35,5)), las=2, cex.axis=1.5)
	# PREDICTED PROBABILITIES
    sals <- c(0.000, 0.156, 0.313, .469)
    for(t in 1:length(sals)){
      ctdData$prSal <- ctdData$Salinity/32
		  tester <- na.omit(ctdData)
	    for(i in 1:nrow(tester)){
			  if(tester$prSal[i] > sals[t]){
				  tester$LprSal[i] <- 1
			  } else {
				  tester$LprSal[i] <- 0;
			  }
		  }
      coefs <- summary(glm(LprSal~RKM, data=tester,
      	family='binomial'))$coefficients
		  nRKM  <- seq(-45, 43, 0.01)
      nSal <- 32 * (exp(coefs[1, 1] + nRKM * coefs[2, 1]) /
      		(1 + exp(coefs[1, 1] + nRKM * coefs[2, 1])))
		  par(new=TRUE)
		  plot(nRKM, nSal, type='l', xlim=c(43, -16), yaxt='n', ylab='',
		  	cex.axis=1.5, xlab='')
    }
    mtext(side=2, 'Salinity', line=4, cex=1.5)
		axis(side=4, at=c(0, 6.4, 12.8, 19.2, 25.6, 32), c('0.00', '0.20', '0.40',
			'0.60', '0.80', '1.00'), las=2, cex.axis=1.5)
	  mtext(side=4, 'p(Salinity)', line=5, cex=1.5)
		text(x=23.5, y=17, '0.0')
		text(x=19.5, y=17, '5.0')
		text(x=10, y=17, '10.0')
		text(x=3.5, y=17, '15.0')
		text(x=40, y=4, '(a)', cex=2)

# Add FISH DEPTH (OBSERVED)
	par(mar=c(5,7,2,8))
	ygap <- ifelse(rdepth$Depth > 40, rdepth$Depth-45, rdepth$Depth)
  plot(rdepth$RKM, ygap+10, xlim=c(45,-15), xaxt='n', ylim=c(55,0), type='l',
  	lwd=3, lty=2, col='grey87', yaxt='n', ylab='', xlab='')
		par(new=TRUE)
  boxplot(TagsRKM$Depth~TagsRKM$RKM, ylim=c(55,0),
    at=c(-sort(unique(TagsRKM$RKM))), ylab="", xlab="",
    yaxt='n', cex.lab=2, cex.axis=1.5, cex = 1, xaxt="n", col="gray87",
    pch=21, bg="gray50", main='')
	axis(1, pretty(c(-40,20)),bg='black', labels=c("40", "30", "20", "10", "0",
  "-10", "-20"),xaxs="i",yaxs="i", cex.axis=1.5,las=1)
	axis(2, at=c(0, 10, 20, 30, 40, 50), c('0', '10',
		'20', '30', '40', '80'), las=2, cex.axis=1.5)
	axis.break(axis=2, breakpos=45, brw=.02)
 	mtext(side=2, "Depth (m)", line=4.5, cex=1.5)
# Add graph showing probability that mean salinity is more than isosmotic
    ctdData$prSal <- ctdData$Salinity/32
		ctdData$LprSal <- exp(ctdData$prSal/(1-ctdData$prSal))
		tester <- na.omit(ctdData)
	  for(i in 1:nrow(tester)){
			if(tester$prSal[i] > 0.3125){
				tester$LprSal[i] <- 1
			} else {
				tester$LprSal[i] <- 0;
			}
		}
    coefs <- summary(glm(LprSal~RKM, data=tester,
    	family='binomial'))$coefficients
		nRKM  <- seq(-45, 43, 0.01)
    nSal <- (exp(coefs[1, 1] + nRKM * coefs[2, 1])/
    		(1 + exp(coefs[1, 1] + nRKM * coefs[2, 1])))
		uSal <- (exp((coefs[1, 1] + 1.96 * coefs[1, 2]) +
				(nRKM*coefs[2, 1] + 1.96 * coefs[2, 2]))/
				(1 + exp((coefs[1, 1] + 1.96 * coefs[1, 2]) +
				(nRKM*coefs[2, 1] + 1.96 * coefs[2, 2]))))
		lSal <- (exp((coefs[1, 1] - 1.96 * coefs[1, 2]) +
				(nRKM*coefs[2, 1] - 1.96 * coefs[2, 2]))/
				(1 + exp((coefs[1, 1] - 1.96 * coefs[1, 2]) +
				(nRKM*coefs[2, 1] - 1.96 * coefs[2, 2]))))
		par(new=TRUE)
		plot(nRKM, nSal, type='l', xlim=c(43, -16), yaxt='n', ylab='', xaxt='n',
			lwd=2, lty=1, xlab='')
		par(new=TRUE)
		plot(nRKM, lSal, type='l', xlim=c(43, -16), yaxt='n', ylab='', xaxt='n',
			lwd=2, lty=2, col='black', xlab='')
		par(new=TRUE)
		plot(nRKM, uSal, type='l', xlim=c(43, -16), yaxt='n', ylab='', xaxt='n',
			lwd=2, lty=2, col='black', xlab='')
		axis(side=4, at=seq(0.00,1.00,0.20), c('0.00', '0.20', '0.40', '0.60',
			'0.80', '1.00'), las=2, cex.axis=1.5)
	  mtext(side=4, 'p(Salinity > 10)', line=5, cex=1.5)
    text(x=40, y=.1, '(b)', cex=2)

# Modeled fish depths
	par(mar=c(8,7,0,8))
  plot(newRKM, newDepth, type='l', ylim = c(15,0), yaxt='n', xlim=c(43,-16),
  	cex.axis=1.5, xlab='', ylab='', lwd=3)
  par(new=TRUE)
  plot(newRKM, newDepthL, type='l', ylim = c(15,0), lty=2, ylab='', xlab='',
  	yaxt='n', xaxt='n', xlim=c(43,-16), lwd=3, col='gray40')
  par(new=TRUE)
  plot(newRKM, newDepthU, type='l', ylim = c(15,0), lty=2, ylab='', xlab='',
  	yaxt='n', xaxt='n', xlim=c(43,-16), lwd=3, col='gray40')
	axis(2, at=c(0, 5, 10, 15, 20, 25, 30), c('0', '5', '10', '15', '20', '25',
	  	'30'), las=2, cex.axis=1.5)
  mtext("Upstream",1, cex=1.5, line=3.5, adj=0)
  mtext("Downstream",1, cex=1.5, line=3.5, adj=1)
	mtext(side=1, "River kilometer", cex=2, line=6)
	mtext(side=2, "Fish depth (m)", line=4.5, cex=1.5)
  text(x=40, y=14, '(c)', cex=2)
 # Turn off graphics to save
	 dev.off()
################################################################################
} # Code-folding marker
# END PART 7. DEPTH-TAGGED FISH ANALYSIS----------------------------------------


# PART 8. 2014 DATA FOR FRESHWATER ANALYSIS-------------------------------------
{ # Code-folding marker
################################################################################
# Add the 2014 Acoustic tagging and receiver data to the appropriate db files
#
# INPUT FILENAMES:  '2005-2013 Surgery.txt
#                   '2005-present Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'receivers', 'surgery'
# OUTPUT FILENAMES: '2005-present Surgery.txt',
#                   '2005-present Receiver Diary.txt'
################################################################################
# Install and load necessary packages
  #install.packages('reshape') # Uncomment to run
  #install.packages('reshape2') # Uncomment to run
  #install.packages('tcltk') # Uncomment to run
  #install.packages('tcltk2') # Uncomment to run
  require(reshape)
  require(reshape2)
  require(tcltk)
  require(tcltk2)

# First, need to add the 2014 data to the surgery and receiver files from the
# rest of the years
  # Set the working directory to the file tree with the compiled data
    setwd('C:/Users/Dan/Desktop/Atlantic salmon smolts/smolt data/2005-present Penobscot ATS')
  # Read in the surgery data
    # The 2005-2013 surgery data
      surgery <- read.csv('2005-present Surgery.txt')

    # Change the surgeon names for the last time
      surgery$Surgeon <- as.character(surgery$Surgeon)
      surgery$Surgeon[surgery$Surgeon=='CH'] <- 'Chris Holbrook'
      surgery$Surgeon[surgery$Surgeon=='JZ'] <- 'Joe Zydlewski'
      surgery$Surgeon[surgery$Surgeon=='MK'] <- 'Mike Kinnison'

    # The 2014 surgery data
      surg14 <- read.csv(file.choose())

    # Add the 2014 surgery data to the rest of the surgery data
      surgery <- rbind(surgery, surg14)

    # Write the new surgery file to the main directory and remove the file
    # '2005-2014 Surgery.txt'
    # NOTE:  THIS WILL OVERWRITE THE OLD SURGERY FILE.  IF YOU ARE NOT SURE
    # ABOUT WHAT YOU ARE DOING MAKE SURE YOU HAVE A BACKUP COPY OF THE FILE
    # SOMEWHERE...Otherwise, <ctrl + r> the shit out of this puppy
      write.table(surgery, file='2005-present Surgery.txt', sep=",",
        row.names=FALSE, col.names=TRUE)

# Read in the receiver data...STILL NEED TO ADD NOAA DEPLOYMENTS!!!!!!!!!!!!!!!!
  # 2005-present receiver diary
    receivers <- read.csv('2005-present Receiver Diary.txt')

  # 2014 receiver diary
    rec2014 <- read.csv(file.choose())

  # Remove 'notes column'
    rec2014 <- rec2014[ , 1:(ncol(rec2014)-1)]
    # Start QC for names of the location codes so everything can get
    # standardized
      check <- rec2014$LocationCode %in% receivers$LocationCode
      qc <- data.frame(rec2014$LocationCode, check)
      qc[qc[ , 2]=='FALSE', ]

      # Change the names to standardized names, with the exception of STHP03,
      # because that location was new in 2014
        rec2014$LocationCode <- as.character(rec2014$LocationCode)
        rec2014$LocationCode[rec2014$LocationCode=='BMTR'] <- 'PISC04'
        rec2014$LocationCode[rec2014$LocationCode=='DPWEST'] <- 'DP'
        rec2014$LocationCode[rec2014$LocationCode=='DPEAST'] <- 'DP'
        rec2014$LocationCode[rec2014$LocationCode=='BHCOV'] <- 'BHCV'
        rec2014$LocationCode[rec2014$LocationCode=='BART02'] <- 'BART'
        rec2014$LocationCode[rec2014$LocationCode=='BART01'] <- 'BART'
        rec2014$LocationCode[rec2014$LocationCode=='RTE395'] <- 'RT395'
        rec2014$LocationCode[rec2014$LocationCode=='PWRST'] <- 'GSTAT'
        rec2014$LocationCode[rec2014$LocationCode=='WBER'] <- 'WEBBER'

    # Get RKM for 2014 deployments
      # Get RKMs from the main receiver diary
        for(i in 1:nrow(rec2014)){
          rec2014$RKM[i] <- receivers$RKM[
            which(rec2014$LocationCode[i]==receivers$LocationCode)[1]]
        }

      # Assign RKM to the STHP03 receiver location
        rec2014$RKM[rec2014$LocationCode=='STHP03'] <- 61.5

  # Add the 2014 receiver diary to the rest of them
    receivers <- rbind(receivers, rec2014)
    try2 <- rec2014[ , ] %in% receivers [ , ]

	  # Fix the RKMS for OR and ORTR b/c they were backward
    	receivers$LocationCode <- as.character(receivers$LocationCode)
      for(i in 1:nrow(receivers)){
	      if(receivers$LocationCode[i]=='OR'){
		      receivers$RKM[i] <- 7.21
	      } else {
		      if(receivers$LocationCode[i]=='ORTR'){
			      receivers$RKM[i] <- 55.39
		      } else {
			      next;
		      }
	      }
      }

  # Write the receiver diary back out to a new file
    write.table(receivers, file='2005-present Receiver Diary.txt', sep=",",
      row.names=FALSE, col.names=TRUE)

    # Add a numeric date for deployment and retrieval to the rec2014 df
      rec2014$DepNum <- as.numeric(as.POSIXct(rec2014$Deployed))
      rec2014$RetNum <- as.numeric(as.POSIXct(rec2014$Retrieved))

	  # Read in the NOAA receiver diary from 2014 and add it on
	    NOAAR14 <- read.csv(file.choose())
      # Add a numeric date for deployment and retrieval to NOAAR14 df
        NOAAR14$DepNum <- as.numeric(as.POSIXct(NOAAR14$Deployed))
        NOAAR14$RetNum <- as.numeric(as.POSIXct(NOAAR14$Retrieved))

    # Add NOAA 2014 data to the rest of the receiver diary
	    receivers <- rbind(receivers, NOAAR14)

  # Write the receiver diary back out to a new file
    write.table(receivers, file='2005-present Receiver Diary.txt', sep=",",
      row.names=FALSE, col.names=TRUE)
	  # Make it a csv, too for the next person
      write.table(receivers, file='2005-present Receiver Diary.csv', sep=",",
        row.names=FALSE, col.names=TRUE)
################################################################################

################################################################################
# Compile the 2014 Smolt data and create final 2014 database
#
# INPUT FILENAMES:  '2014 ATS Smolt Detections.rda' (or .txt),
#                   '2005-present Surgery.txt
#                   '2005-present Receiver Diary.txt'
# MY FUNCTIONS:
# NAMED OUTPUT:     'qc'...'qc3' (data QC steps), not used in the future,
#                    overwritten in subsequent code,'d14'
# OUTPUT FILENAMES: 'Final 2014 Database.txt' (d14),
#                   'Final 2014 Database.rda' (d14); for faster loading
################################################################################
# Install necessary packages and load them into the workspace
  #install.packages("lubridate") # Uncomment to run
  #install.packages("tcltk2") # Uncomment to run
  require(lubridate)
  require(tcltk2)

# Read in tagging file
  # Had to add an arbitrary timestamp to the datetime field in receiver diary so
  # data would parse later in code. To get the ymd_hms command to work properly,
  # the date and time must be formatted as yyyy-mm-dd hh:mm:ss in the receiver
  # diary and time cannot be 00:00:00.
  # Choose file '2005-present Surgery.txt'
    surgery<-read.csv(file.choose( )) # Browse to the tagging data file, select
  # Select only those records from the appropriate year(s)
    surg14<-surgery[year(as.character(surgery$ReleaseDate))==2014,]
    surg14$ReleaseDate<-ymd_hms(as.character(surg14$ReleaseDate))

# Read in receiver diary
# Had to add an arbitrary timestamp to the timestamps so data would parse
# To get the ymd_hms command to work properly, the date and time must be
# formatted as yyyy-mm-dd hh:mm:ss in receiver diary, time can't be 00:00:00
  receivers<-read.csv(file.choose())
  receivers$Deployed<-ymd_hms(as.character(receivers$Deployed))
  receivers$Retrieved<-ymd_hms(as.character(receivers$Retrieved))

# Read detection file, change the formatting of date/time and rename TagID
# To get the ymd_hms command to work, the date and time must be formatted as
# yyyy-mm-dd hh:mm:ss in the detection files
# Maximize the amount of memory that R can access
  #memory.limit(size=800000000000) # Uncomment to run

# Read in the detection data ('XXXX ATS Smolt detections.txt' or .csv)
  d <- read.csv(file.choose())
  detections14 <- d
  # Save it as an r-data file, then use that from now on
    save(detections14, file = tclvalue(tcl("tk_getSaveFile")))
  # Load the detections as an r-data file
    load(file.choose()) # Choose '2013 ATS Smolt detections.rda'
    d <- detections14

# May need to change based on VUE export formats
  d<-data.frame(d$Receiver.S.N, d$ID, d$Date.Time, d$Sensor.1, d$Units.1)
  colnames(d)<-c("ReceiverSN","TagID","DateTime","SensorValue","SensorRange")
  d$DateTime<-ymd_hms(as.character(d$DateTime))
  d$DetNum<-as.numeric(d$DateTime)

# Make file with detections and fish/tag data
  fish.detections<-merge(d, surg14, by="TagID", all.y=TRUE)

# Now that the dataframe is smaller, assign date/time class to DateTime field,
# then combine with receiver diary
  Rawdata <- merge(fish.detections, receivers, by="ReceiverSN")
  nrow(Rawdata)

# QC the data and remove any false detections
  # Make sure that the year of detection matches tagging & receiver data
    qc <- Rawdata[year(Rawdata$DateTime)==year(Rawdata$Deployed) &
  		year(Rawdata$DateTime)==year(Rawdata$Retrieved) &
  	  year(Rawdata$DateTime)==year(Rawdata$ReleaseDate), ]

  # Make sure release date happens prior to detection
    # Make a numeric release date column
      qc$NumericReleaseDate <- as.numeric(as.POSIXct(qc$ReleaseDate))
    # Remove detectinos that occur before release
      qc1 <- qc[qc$NumericReleaseDate<qc$DetNum|
        qc$NumericReleaseDate==qc$DetNum,]

  # Make sure the detection happens after receiver deployment and before
  # retrieval for the receiver that year
    qc2 <- qc1[(qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum) &
      (qc1$DetNum>qc1$DepNum|qc1$DetNum==qc1$DepNum), ]

  # Make sure the release date for detected fish happens after the deployment
  # for the receiver on which it is detected
    qc3<-qc2[qc2$NumericReleaseDate>mean(qc2$DepNum)|
      qc2$NumericReleaseDate==mean(qc2$DepNum),]

  # Rename the df for lazy loading later on as .rda filetype
    d14<-qc3
    nrow(d14)

# Add year to the TagID field so that there are no tag overlaps possible
  d14$TagID <- paste(d14$TagID, "-", as.integer(substr(d14$DateTime, 1, 4)),
    sep="")

# Compile the location data (without depth info) for 2014 tags
  # Get the desired columns from the data
    d14<-data.frame(d14$TagID, d14$ReceiverSN, d14$Location, d14$LocationCode,
    	d14$RKM, d14$Easting, d14$Northing,	d14$DateTime, d14$Origin,
    	d14$ReleaseSite, d14$R.Easting, d14$R.Northing, d14$ReleaseDate,
     	d14$ForkLength.mm, d14$Mass.g, d14$ATPaseActivity, d14$Surgeon)

  # Assign standardized column names
    names(d14)<-c("TagID", "ReceiverSN", "Location", "LocationCode",
    	"RKM", "Easting", "Northing",	"DateTime", "Origin",
    	"ReleaseSite", "R.Easting", "R.Northing", "ReleaseDate",
     	"ForkLength.mm", "Mass.g", "ATPaseActivity", "Surgeon")

# Change factors to type char
	d14[, c(1, 3:4, 9:10, 13, ncol(d14))] <- apply(d14[, c(1, 3:4, 9:10, 13,
		ncol(d14))], 2, as.character)

# Give each fish a location at its release site, to be combined with 'AllData'
# Include the Release event for each fish in the capture history, put the
# information about the release site in the df in place of the 'normal' data
# for each location in the river.  This will create a location event for each
# of the fish upon release.
	# Load surgery data
	  surgery <- surg14

  # Create a dataframe to hold release 'detections'
	  Releases <- as.data.frame(matrix(ncol=ncol(d14), nrow=nrow(surgery)))

  # Assign names to the df
	  names(Releases) <- names(d14)

  # Populate the df with 'detections' and the information from the surgery tab
	  Releases$TagID <- paste(surgery$TagID, "-",
	    as.integer(substr(surgery$ReleaseDate, 1, 4)), sep="")
    Releases$ReceiverSN <- surgery$ReleaseSite
    Releases$Location <- surgery$ReleaseSite
    Releases$LocationCode <- surgery$ReleaseSite
    Releases$Easting <- surgery$R.Easting
    Releases$Northing <- surgery$R.Northing
    Releases$DateTime <- surgery$ReleaseDate
	  Releases$Origin <- surgery$Origin
	  Releases$ReleaseSite <- surgery$ReleaseSite
	  Releases$R.Easting <- surgery$R.Easting
	  Releases$R.Northing <- surgery$R.Northing
	  Releases$ReleaseDate <- surgery$ReleaseDate
	  Releases$ForkLength.mm <- surgery$ForkLength.mm
	  Releases$Mass.g <- surgery$Mass.g
    Releases$ATPaseActivity <- surgery$ATPaseActivity
	  Releases$Surgeon <- surgery$Surgeon

  # Get RKM for each of the release sites so it can be added to 'Releases'
    # Get the unique values of release site
      ReleaseSites <- c(sort(unique(as.character(surgery$ReleaseSite))))

    # Make a vector of corresponding rkms for release sites
      ReleaseRKMs <- c(187, 162, 63)

  # Add RKM to 'releases' that way release locations have RKM associated w/ them
    Releases$ReleaseSite <- as.character(Releases$ReleaseSite)
    ReleaseSites <- as.character(ReleaseSites)
    for(i in 1:nrow(Releases)){
    	for(t in 1:length(ReleaseSites)){
      	if(Releases$ReleaseSite[i] == ReleaseSites[t]){
    	  	Releases$RKM[i] <- ReleaseRKMs[t]
    	  } else {
    	  	next;
    	  }
      }
    }

# Change var type for character vars so no factor-based errors occur
  Releases[, c(2:4, 8:9, 13, ncol(Releases))] <-
		apply(Releases[, c(2:4, 8:9, 13, ncol(Releases))], 2, as.character)

# Add releases to the final 2014 database
  d14 <- rbind(Releases, d14)

# Last, add NKA activity data to the df
  atp14<- read.csv(file.choose( ))
	atp14$TagID <- as.character(atp14$TagID)
	d14 <- merge(d14, atp14, by='TagID')

  # Write the final database to a file
    # This will save it as a comma separated text file
	  # ('Final XXXX Database.txt')
      write.table(d14,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	      row.names=FALSE)
    # This one will save it as an R-data file, which will load faster later on!
    # ('Final XXXX Database.rda)
      save(d14, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Add 2014 data to the 2005-present database
#
# INPUT FILENAMES:  'Final 2005-2013 Database.rda',
# MY FUNCTIONS:
# NAMED OUTPUT:     'AllData'
# OUTPUT FILENAMES: 'Final 2005-present Database.txt' (d14),
################################################################################
# Package install and load
	#install.packages('tcltk2')
  require(tcltk2)

# Load the big database for 2005-2013
  load(file.choose()) # ('Final 2005-2013 Database.rda)
# Reformat some of the columns to char, takes a few minutes!
  AllData[, c(1, 3:4, 8:10, 13, ncol(AllData))] <- apply(
  	AllData[, c(1, 3:4, 8:10, 13, ncol(AllData))], 2, as.character)

# Load the 2014 database
  load(file.choose()) # ('Final 2014 Database.rda)
	d14 <- d14[, c(1:15, 18, 17)]
  names(d14)[ncol(d14)-1]<- 'ATPaseActivity'

# Make fxn for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

	# Fix the timestamps!


# Add the 2014 data to the Final 2005-present database table
  AllData <- rbind(AllData, d14)

# Save the final database out to a file  # Write the final database to a file
    # This will save it as a comma separated text file
	  # ('Final 2005-present Database.txt')
      write.table(AllData,file = tclvalue(tcl("tk_getSaveFile")), sep=",",
 	      row.names=FALSE)

    # This one will save it as an R-data file, which will load faster later on!
    # ('Final 2005-present Database.rda)
      save(AllData, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################
} # Code-folding marker
# END PART 8. 2014 DATA FOR FRESHWATER ANALYSES---------------------------------


# PART 9. FRESHWATER SURVIVAL ANALYSIS------------------------------------------
{ # Code-folding marker
################################################################################
# Make whole-river (GodModel) capture histories for all of the fish ever
# released for MS mark-recapture survival analysis in BUGS or JAGS
#
# INPUT FILENAME(S):  'Final 2005-present Database.rda' ('AllData'),
# MY FUNCTIONS:
# NAMED OUPUT:        'ch'...'ch9', 'msGodCH' (Wholelotta capture histories)
# OUTPUT FILENAME:    'GodCapturesReady.csv', 'GodlyCovariates.csv'
################################################################################
# Install packages
  #install.packages("tcltk2")
  #install.packages("reshape")
  #install.packages("reshape2")
  #install.packages("lubridate")
  require(reshape)
  require(tcltk2)
  require(lubridate)
  require(reshape2)
	require(geosphere)

# Read in the 2005-present acoustic telemetry detection data
  load(file.choose())

# Make the capture histories out of the comprehensive location histories for
# each fish.
  # Use the cast function to make the capture-history matrix for processing.
  # Takes about 5 minutes to complete...
    #unix.time( # Uncomment to time the cast fxn
    ch <- cast(AllData,
    	TagID + ForkLength.mm + Mass.g + ATPaseActivity + ReleaseSite +
    	ReleaseDate + Origin ~ RKM + LocationCode)
    #) # Uncomment to time the cast fxn

  # Remove duplicated tag IDs in ch
    ch <- ch[!duplicated(ch$TagID),]

  # Reverse the order of the Location columns to sort by decreasing rkms
    ch <- ch[ , c(1:7, rev(8:ncol(ch)))]

  # Use a nested for loop to make a 1/0 matrix of captures at each site
  # Takes about 20 seconds to run
    #unix.time( # uncomment to time
    for(i in 1:nrow(ch)){
      for (t in 8:ncol(ch)){
        if(ch[i, t] > 0){
          ch[i, t] <- 1
        } else {
          ch[i, t] <- 0
        }
      } # t
    } # i
    #) # uncomment to time

  # Spot-check the capture history to make sure it worked ok
    # First ten rows and columns
      ch[1:10, 1:10]
    # First ten rows and last ten columns
      ch[1:10, (ncol(ch) - 10):ncol(ch)]

  # Add 'Year' to the capture history for each fish as a grouping variable that
  # can be used to help model year/release-group random effects later on
    ch$Year <- substr(as.character(ch$ReleaseDate), 1, 4)

  # Rearrange the df so that 'Year' is in the front with other covariates
    ch <- ch[ , c(1:7, ncol(ch), 8:(ncol(ch)-1))]

  # Now combine all detections after FortPoint to create a 'Final' detection
  # array at the end of the river
    for(i in 1:nrow(ch)){
	  if(sum(ch[i , (ncol(ch) - 5):ncol(ch)]) > 0){
        ch[i, (ncol(ch) - 5)] <- 1
      } else {
      	ch[i, (ncol(ch) - 5)] <- 0
      }
    } # i

    # Get rid of the columns for those intervals
      ch <- ch[ , 1:(ncol(ch) - 5)]

    # Rename the final interval to be 'Bay'
	  colnames(ch)[ncol(ch)] <- "-6_BAY"

  # Now combine all detections from Graham Station through FortPoint to create a
  # detection event for the estuary
  # array at the end of the river
    for(i in 1:nrow(ch)){
	  if(sum(ch[i , (ncol(ch) - 39):(ncol(ch)-1)]) > 0){
        ch[i, (ncol(ch) - 39)] <- 1
      } else {
      	ch[i, (ncol(ch) - 39)] <- 0
      }
    } # i

    # Get rid of the extra columns
      ch <- ch[ , c(1:(ncol(ch) - 39), ncol(ch))]

    # Rename the final interval to be 'Estuary'
	  colnames(ch)[ncol(ch) - 1] <- "44_Estuary"

  # Get rid of the release site columns
    ch2 <- ch[ , c(1:8, 10:16, 19:22, 24:25, 27:36, 38, 40:44, 46:ncol(ch))]

  # Get rid of columns that suck for detection or are not shared across years
    ch3 <- ch2[ , c(1:14, 16:22, 26:ncol(ch2))]

  # Combine locations that are one
    # First, do Stillwater
      for(i in 1:nrow(ch3)){
        if(sum(ch3[i , c(37:39)]) > 0) {
          ch3[i , c(37)] <- 1
        } else {
          ch3[i, 37] <- 0
        }
      }
      ch4 <- ch3[ , c(1:37, 40:ncol(ch3))] # Take out the extra rows
      colnames(ch4)[37]<- '61.5_STHP'

    # Next, do Milford
      for(i in 1:nrow(ch4)){
        if(sum(ch4[i , c(35:36)]) > 0) {
          ch4[i , c(35)] <- 1
        } else {
          ch4[i, 35] <- 0
        }
      }
      ch5 <- ch4[ , c(1:35, 37:ncol(ch4))] # Take out the extra rows
      colnames(ch5)[35] <- '62.2_MLHPW'

    # Next, do Orson Island and Gilman Falls
      for(i in 1:nrow(ch5)){
        if(sum(ch5[i , c(32, 34)]) > 0) {
          ch5[i , c(32)] <- 1
        } else {
          ch5[i, 32] <- 0
        }
      }
      ch6 <- ch5[ , c(1:33, 35:ncol(ch5))] # Take out the extra rows
      colnames(ch6)[32] <- '63_GFHP'

    # Combine PISC02 and PISC03 for the 2005-2006 data...or don't
      for(i in 1:nrow(ch6)){
        if(ch6$Year[i]==2005|ch6$Year[i]==2005){
          ch6[i, 20] <- sum(ch6[i , 20], ch6[ , 22])
        } else {
          ch6[i, 20] <- ch6[i, 20]
        }
      }
      for(i in 1:nrow(ch6)){
        if(ch6[i, 20] > 0){
          ch6[i, 20] <- 1
        } else {
          ch6[i, 20] <- 0
        }
      }

    # Get rid of the crappy sites I forgot
      ch7 <- ch6[ , c(1:21, 23:29, 31:36, 38:ncol(ch6))]

    # Reorganize the capture history
      ch8 <- ch7[ , c(1:14, 20, 24, 26, 15:19,21:23, 25, 27:30, 33, 35, 31:32,
        34, 36:ncol(ch7))]

    # Fix detection at GDHP to 1.00 for 2014...or don't
       ch8[ch8$Year==2014 & ch8$ReleaseSite=='Abbott' , 10] <-
         ch8[ch8$Year==2014 & ch8$ReleaseSite=='Abbott' , 9]

    # Make the states for the final capture history
      # First, re-code the piscataquis
        for (i in 1:nrow(ch8)){
          for(t in 9:17){
            if(ch8[i, t] > 0){
              ch8[i, t] <- 2
            } else {
              ch8[i, t] <- 0
            }
          }
        }

      # Now, re-code the Stillwater Branch
        for (i in 1:nrow(ch8)){
          for(t in 30:32){
            if(ch8[i, t] > 0){
              ch8[i, t] <- 3
            } else {
              ch8[i, t] <- 0
            }
          }
        }

    # Concatenate the actual capture histories (just the ones and zeros,
    # based on release site and migratory route in the lower river
      # Create variable for Stillwater or Mainstem, unknown gets mean 0.5
      # Numbers in this loop don't need to change unless ch changes in the
      # future
        Stillwater <- c()
	      for(i in 1:nrow(ch8)){
	        if( sum(ch8[i, c(31:32)]) > 0){
	          Stillwater[i] <- 1
	        } else {
            Stillwater[i] <- 0
	        }
	      } # i

      # Rename it
        ch9 <- ch8

      # Now use the Stillwater variable to determine which columns to use, in
      # conjunction with the release sites
        msGodCH <- matrix(nrow=nrow(ch9), ncol=20)
        for(i in 1:nrow(ch9)){
          if((ch9$ReleaseSite[i]=='Abbott' | ch9$ReleaseSite[i]=='Milo' |
            ch9$ReleaseSite[i]=='Howland') & (Stillwater[i] > 0)){
              msGodCH[i,] <- paste(ch9[i, c(9:17, 27:32, 36:(ncol(ch9)))])
          } else {
            if((ch9$ReleaseSite[i]=='Abbott' | ch9$ReleaseSite[i]=='Milo' |
              ch9$ReleaseSite[i]=='Howland') & (Stillwater[i]==0)){
                msGodCH[i,] <-  paste(ch9[i, c(9:17, 27:29, 33:(ncol(ch9)))])
            } else {
              if((ch9$ReleaseSite[i]!='Abbott' | ch9$ReleaseSite[i]!='Milo' |
                ch9$ReleaseSite[i]!='Howland') & (Stillwater[i]!=0)){
                  msGodCH[i,] <-  paste(ch9[i, c(18:32, 36:(ncol(ch9)))])
              } else {
                msGodCH[i,] <-  paste(ch9[i, c(18:29, 33:(ncol(ch9)))])
              }
            }
          }
        }
        msGodCH <- apply(msGodCH, 2, as.numeric)

      # Combine VZHP03 and VZHP02
	      for(i in 1:nrow(msGodCH)){
          msGodCH[i , (ncol(msGodCH)-4)] <- sum(msGodCH[i, (ncol(msGodCH)-4)],
            msGodCH[i, (ncol(msGodCH)-3)])
	      }

      # Now put all of those detections at the VZHP03 location
        for(i in 1:nrow(msGodCH)){
          if(msGodCH[i, (ncol(msGodCH)-4)] > 0){
            msGodCH[i, (ncol(msGodCH)-4)] <- 1
          } else {
            msGodCH[i, (ncol(msGodCH)-4)] <- 0
          }
        }

      # Remove the VZHP02 location
        msGodCH <- msGodCH[ , -(ncol(msGodCH)-3)]

    # Make covariates for the god model
      msGodCovs <- ch9[ , 1:8]

# Set working directory to the GodModel manuscript directory
  setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

# Write the capture histories to a csv that can be used in BUGS or JAGS
  write.table(msGodCH, 'GodCapturesReady.csv',  sep=",", row.names=FALSE,
    col.names=FALSE)

# Write the covariates to a csv, too
  write.table(msGodCovs, 'GodlyCovariates.csv',  sep=",", row.names=FALSE,)

# Finish collecting covariate information
# DISHCHARGE COVARIATE
# Go back and get flow for the survival data
  # Read in Flow data ('WestEnfieldFlowDataClean.txt'), replace NA with 0 b/c ice
    Discharge <- read.table('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/WestEnfieldFlowDataClean.txt',
    	header=TRUE)[ , 3:4]
	  names(Discharge) <- c('Date', 'Discharge')
	  Discharge[ , 2] <- as.character(Discharge[ , 2])
	  for(i in 1:nrow(Discharge)){
		  if(Discharge[i , 2]=='Ice'){
	      Discharge[i , 2] <- '0'
		  } else {
			  next;
		  }
	  }
	  Discharge[ , 2] <- as.numeric(Discharge[ , 2])

    # Make a vector to hold the year for each temperature measurement
      Discharge$Year <- year(as.POSIXct(Discharge[,1]))
	  # Change discharge date to character
	    Discharge$Date <- as.character(Discharge$Date)

	# Get discharge for each day
	# Get matrix of timestamps for fish that are in fw survival data
	  # Read in the Date Matrix for fw data
	    load('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis/DateTimeMatrix05-14.rda')
      # Get dates and times for fish used
        NewDateTime <- Date
      # Get the maximum date for each fish in the river
        # Get rid of 'infinity' values in the DateTime matrix
          for(i in 2:ncol(NewDateTime)){
  	        NewDateTime[ , i][is.infinite(NewDateTime[ , i])] <- 0
          }
        endGame <- c()
        for(i in 1:nrow(NewDateTime)){
          endGame[i] <- max(NewDateTime[i, 2:ncol(NewDateTime)])
        }

      # Set system time to UTC for ease of handling time/number conversions later on
        Sys.setenv(TZ='UTC')

      # Make fxn for getting dates back from numeric format using UTC, default origin
        DTAgain<- function(x){
          as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
          strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
        }
        endGame <- DTAgain(endGame)
        head(endGame)

      # Take average discharge from release to endGame for each fish
        msGodCovs$Year <- as.numeric(msGodCovs$Year)
        pb <- txtProgressBar(min = 0, max = nrow(msGodCovs), style = 3)
	      Dis <- c()
        for(i in 1:nrow(msGodCovs)){
          Sys.sleep(0.1)
          Dis[i] <- mean(
        	Discharge$Discharge[
        	Discharge$Year==msGodCovs$Year[i] &
            (yday(Discharge$Date) > yday(msGodCovs$ReleaseDate[i]) |
          	 yday(Discharge$Date)==yday(msGodCovs$ReleaseDate[i])) &
            (yday(Discharge$Date) < yday(endGame[i]) |
          	 yday(Discharge$Date)==yday(endGame[i]))
          ])
          setTxtProgressBar(pb, i)
        } # i
	    # Convert discharge to cms
        Dis <- Dis*0.028316847
      # Add discharge to covariates list
        msGodCovs$Discharge <- Dis
        msGodCovs$Discharge2 <- Dis^2

# PHOTOPERIOD COVARIATE
  msGodCovs$Ph <- daylength(45.239006, substr(msGodCovs$ReleaseDate, start=1,
    stop=10))
  msGodCovs$Ph2 <- msGodCovs$Ph^2

 # Calculate cumulative degree days for each fish prior to release
	  # Read in GLNFH temperature data
	    GLNFH <- read.csv('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData/GLNFHTemps.csv', header=TRUE)
	    # Create a year variable
	      GLNFH$Year <- substr(GLNFH$Date, start=1, stop=4)
	    # Rename the temperature column
	      names(GLNFH)[2]<-'Temp'

	  # Read in river temperature data
	    river.t <- read.csv('C:/Users/Dan/Desktop/Manuscripts/ATSPenobscotEstuary/Estuary Analysis/ProcessedData/RiverTemps_2005-2011.csv', header=TRUE)
	    # Get daily averages for river temps
	      rtp <- tapply(river.t$Temp, river.t$Date, mean)
	    # Get date
	      rds <- sort(unique(river.t$Date))
	    # Get year
	      ry <- year(rds)
	    # Make the daily means into a df
	      rivers <- data.frame(ry, rds, rtp)
	      names(rivers) <- c('Year', 'Date', 'Temp')

	  # Change year and date vars into char
	    GLNFH[ , c(1, 3)] <- apply(GLNFH[ , c(1, 3)], 2, FUN=as.character)
	    rivers[ , c(1, 2)] <- apply(rivers[ , c(1, 2)], 2, FUN=as.character)

  # Do the calculation for ATU
    pb <- txtProgressBar(min = 0, max = nrow(msGodCovs), style = 3)
    for(i in 1:nrow(msGodCovs)){
      Sys.sleep(0.1)
    	if(msGodCovs$Origin[i]=='Hatchery'){
        msGodCovs$ATU[i] <- sum(GLNFH$Temp[GLNFH$Year==msGodCovs$Year[i] &
          (GLNFH$Date < msGodCovs$ReleaseDate[i] | GLNFH$Date==msGodCovs$ReleaseDate[i])])
    	} else {
    	  if(msGodCovs$Origin[i]=='Wild'){
          msGodCovs$ATU[i] <- sum(rivers$Temp[rivers$Year==msGodCovs$Year[i] &
            (rivers$Date < msGodCovs$ReleaseDate[i] | rivers$Date==msGodCovs$ReleaseDate[i])])
    	  } else {
    		next;
    	  }
    	}
      setTxtProgressBar(pb, i)
    } # i
    msGodCovs$ATU2 <- msGodCovs$ATU^2

  # Do the calculation for Temperature
    # Get a vector of dates only from the release date
      msGodCovs$Date <- substr(msGodCovs$ReleaseDate, start=1, stop=10)
    # Get a vector of max date (without time) on which each fish was located
      endGameD <- substr(endGame, start=1, stop=10)

    pb <- txtProgressBar(min = 0, max = nrow(msGodCovs), style = 3)
    for(i in 1:nrow(msGodCovs)){
      Sys.sleep(0.1)
    	if(msGodCovs$Origin[i]=='Hatchery'){
        msGodCovs$T[i] <- mean(GLNFH$Temp[GLNFH$Year==msGodCovs$Year[i] &
          (GLNFH$Date > msGodCovs$Date[i] | GLNFH$Date==msGodCovs$Date[i]) &
          (GLNFH$Date < endGameD[i] | GLNFH$Date==endGameD[i])
          ])
    	} else {
    	  if(msGodCovs$Origin[i]=='Wild'){
          msGodCovs$T[i] <- mean(rivers$Temp[rivers$Year==msGodCovs$Year[i] &
            (rivers$Date > msGodCovs$Date[i] | rivers$Date==msGodCovs$Date[i]) &
            (rivers$Date < endGameD[i] | rivers$Date==endGameD[i])
            ])
    	  } else {
            if(msGodCovs$Origin[i]=='Hatchery' & msGodCovs$Date[i]==endGameD[i]){
              msGodCovs$T[i] <- GLNFH$Temp[rivers$Year==msGodCovs$Year[i] &
              GLNFH$Date==msGodCovs$Date[i]
              ]
            } else {
              if(msGodCovs$Origin[i]=='Wild' & msGodCovs$Date[i]==endGameD[i]){
                msGodCovs$T[i] <- rivers$Temp[rivers$Year==msGodCovs$Year[i] &
                rivers$Date==msGodCovs$Date[i]
                ]
              } else {
    		    next;
              }
            }
    	  }
    	}
      setTxtProgressBar(pb, i)	# Progress update
    } # i
    msGodCovs$T <- as.numeric(msGodCovs$T)
    msGodCovs$T2 <- msGodCovs$T^2 # Quadratic term for temperature

# Write the covariates to a csv, too
  write.table(msGodCovs, 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv',  sep=",", row.names=FALSE,)
################################################################################

################################################################################
# Get min of DateTime for each fish at each location in the FW survival model
# using C++ loop(s).  This is the same code as for the data manipulation routine
# in PART 3
#
# INPUT FILENAME(S): 'Final 2005-2013 database.rda' (or .txt)
# MY FUNCTIONS:      'DTAgain' created, 'timesC' created
# NAMED OUTPUT:      'timestamp', 'caps'
# OUTPUT FILENAME:   'MinDateTimeGod.csv', 'MinDateTimeGod.rda'
################################################################################
# Install and load necessary packages
	#install.packages('Rcpp') # Uncomment to run, may require restart of R
	#install.packages('lubridate') # Uncomment to run
	#install.packages('reshape') # Uncomment to run
	#install.packages('reshape2') # Uncomment to run
	#install.packages('stringr') # Uncomment to run
	require(Rcpp)
  require(lubridate)
	require(reshape)
	require(reshape2)
	require(tcltk2)


# Read in the data for all of the years 2005-2014
	load('C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/2005-present Penobscot ATS/Final 2005-present Database.rda')

# Set system time to UTC for ease of handling time/number conversions later on
  Sys.setenv(TZ='UTC')

# Make fxn for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

# # Correct the timestamps for the 2014 data--NO NEED TO RUN THIS SECTION AGAIN!!
# 	# Extract the data for 2014 with effed up time stamps
#   	d14 <- AllData[(which(substr(AllData$DateTime, start=1,
# 		  stop=3)=='139'))[1]:nrow(AllData),]
# 	# Remove 2014 data from the multi-annual database
# 		AllData2 <- AllData[AllData$ReleaseDate!='2014-05-02', ]
# 	# Change the release dates and times into POSIXctObjects by adding an
# 	# arbitrary timestamp and tz, then convert to datetime
# 	  d14$DateTime[substr(d14$DateTime, start=1, stop=4)==2014] <-
# 		  as.character(as.numeric(as.POSIXct(paste(d14$DateTime[substr(d14$DateTime,
# 		  	start=1, stop=4)==2014], ' 00:00:01', ' UTC', sep=''))))
# 	# Now convert the rows that were stored as var type numeric back to POSIXct
# 	# format
# 	  d14$DateTime <- DTAgain(as.numeric(d14$DateTime[substr(d14$DateTime,
# 	  	start=1, stop=4)!=2014]))
# 	# Now smash all the data back together...AGAIN!
#     # First, convert the AllData DateTime field to POSIXct
# 	    AllData2$DateTime <- as.POSIXct(AllData2$DateTime)
# 	  # Put them together and do a data check before saving over the bigdf
# 	    AllData3 <- rbind (AllData2, d14)
# 	    nrow(AllData3) # should be 7240964
# 	    AllData <- AllData3
# 	    rm(AllData2, AllData3, d14)
#   # Save the data for all of the years 2005-2014...AGAIN!
# 	# This will save it as a comma separated text file
# 	# ('Final 2005-present Database.txt')
#     write.table(AllData, file = 'C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/2005-present Penobscot ATS/Final 2005-present Database.txt',
#       sep=",", row.names=FALSE)
#   # This one will save it as an R-data file, which will load faster later on!
#   # ('Final 2005-present Database.rda)
#     save(AllData, file='C:/Users/Dan/Desktop/Atlantic salmon smolts/Smolt data/2005-present Penobscot ATS/Final 2005-present Database.rda')

# Read in the GodModelCH
  GD <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodCapturesReady.csv', header=F)

# Read in covariates for the god model
  covs <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv')

# Remove fish released at Verona and Brewer (estuary releases)
  GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
  covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

# Remove fish that were never located
  GD <- GD[rowSums(GD)!=0, ]
  covs <- covs[rowSums(GD)!=0, ]

# Prep timestamp data from the main database and put it in necessary format
  dt <- as.POSIXct(AllData$DateTime, tz="UTC")

# Variable definition for use in c++ fxns
   y <- as.character(AllData$TagID) # Make TagID character var
   w  <- as.numeric(dt, origin=1970-01-01) # Make date numeric for ease
   v <- as.character(AllData$LocationCode) # Make LocationCode character var
   data2 <- unique(data.frame(y,v,w)[,1:2]) # Make df of unique combinations
   y2 <- data2[ , 1] # Vectorize values of TagID in data2
   v2 <- data2[ , 2] # Vectorize values of LocationCode in data2

# Create function to get min DateTime for each fish at each location
# Includes a progress meter that reports % complete for external ('i') loop
  cppFunction('
    NumericVector timesC(CharacterVector v, CharacterVector y,
    NumericVector w, CharacterVector v2, CharacterVector y2){
      int n = v.size();
      NumericVector z(n);
      int m = v2.size();
      NumericVector Timestamp(m);
      float length = m;
      std::cout << std::endl;

      for(int i=0; i < m; ++i) {
        for(int t=0; t < n; ++t) {
          if(y[t]==y2[i] && v[t]==v2[i]) {
            z[t] = w[t];
          } else {
            z[t] = 1e16;
          }
        }
        std::cout<<"\\r"
        <<100*(float)(i+1)/(float)length
        <<"% "
        <<std::flush;
        Timestamp[i] = min(z);
      }
      return Timestamp;
    }
  ')

# Get min Date time for each fish at each location (run the function)
  unix.time(timestamp <- timesC(v,y,w,v2,y2)) # Call fxn & time it, over 1 hr
  head(DTAgain(timestamp)) # View results to check datatype
  Dates <- DTAgain(timestamp) # Convert results back to DateTime from numeric
  head(Dates) # Data QC- spotcheck

# Make df with TagID, LocationCode, min date for each tag at each location
  caps <- data.frame(data2, Dates) # Create the object
  names(caps)<-c("TagID", "LocationCode", "DateTime") # Rename the columns

# Write the min DateTime for each fish at each location to a .csv file
# this will prevent data loss when I pull a dumb-dumb and close out!
# Requires tcltk2 package
# This is called 'MinDateTimeGod.csv'
  write.table(caps, file = tclvalue(tcl("tk_getSaveFile")), sep=",",
    row.names=FALSE)
# Write it to an Rdata file for faster future loading
# ('MinDateTimeGod.rda')
	save(caps, file = tclvalue(tcl("tk_getSaveFile")))
################################################################################

################################################################################
# Run multi-annual survival model that standardizes survival across years as
# well as by reach lengths.
#
# INPUT FILENAME(S):  'GodCapturesReady.csv', 'GodlyCovariates.csv'
# MY FUNCTIONS:        Inits, ms.init.z, known.state.ms,
# NAMED OUPUT:        'GD', 'CH', 'ms.god', 'msGOD'
#                     'GodMultiAnnRes',
# OUTPUT FILENAME:    'MultiAnnualSurvivalEstimates.rda', 'Figure 4.tiff'
################################################################################
# Install and load necessary packages
  #install.packages('rjags')
  #install.packages('R2jags')
  #install.packages('R2WinBUGS')
  #install.packages('erer')
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(reshape2)
	require(erer)

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Set bugs directory for WinBUGS
    bugs.dir = 'C:/WinBUGS14/'

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

  # Get rid of Piscataquis detections for fish released below Howland in 2005
    for(i in 1:nrow(GD)){
      if(covs$Year[i]==2005 & covs$ReleaseSite[i]=='Howland'){
        GD[i, 1:9] <- 0
      } else {
        next;
      }
    }

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

# DATA DEFINITIONS FOR MODEL CODE
  # Define number of occasions in CH
    n.occasions <- ncol(CH)

  # Define number of states, including dead
    n.states <- 4

  # Define number of observable states (one for unobserved)
    n.obs <- 4

  # Define number of individuals
    nind <- nrow(CH)

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

# RUN THE MODEL
# Set a new working directory to separate the files for different models
  if(!file.exists('C:/Time')) dir.create('C:/Time')
  setwd('C:/Time')

# Specify the model in JAGS language
  sink("ms.god")
  cat("
    model{

      # GROUPS
        # 1 2005
        # 2 2006
        # 3 2009
        # 4 2010
        # 5 2011
        # 6 2012
        # 7 2013
        # 8 2014

      # Parameters:
        # S.Mainstem[t]
        # S.Piscataquis[t]
        # S.Stillwater[t]
        # psi.PiscToMain[t]
        # psi.PiscToStill[t]
        # psi.MainToPisc[t]
        # psi.MainToStill[t]
        # psi.StillToPisc[t]
        # psi.StillToMain[t]
        # p.Mainstem[t]
        # p.Pisquatis[t]
        # p.Stillwater[t]

      # States:
        # 1 Alive in Mainstem
        # 2 Alive in Piscataquis
        # 3 Alive in Stillwater
        # 4 Alive in Dead

      # Observations:
        # 1 Seen alive
        # 2 Seen dead
        # 3 Not seen

      # PRIORS & CONSTRAINTS####################################################
          # Detection covariates
            # Prior on fixed effect of tag type and data transform
              betaT <- log(beta.T / (1 - beta.T))
              beta.T ~ dunif(0,1)

            # Prior on fixed effect of flow for detection and data transform
              betaFp <- log(beta.Fp / (1 - beta.Fp))
              beta.Fp ~ dunif(0,1)

        # MAINSTEM PENOBSCOT RIVER SURVIVAL & DETECTION ########################
          # Survival per kilometer
            for(t in 1:(n.occasions-1)){
              # Survival probabilities
                S.Mainstem[t] <- pow(Smain[t],1/dMain[t])
            } # t

          # Covariate effects on detection
            for(i in 1:nind){
              for(t in 1:(n.occasions-1)){
                # Detection probabilities
                  logit(p.Mainstem[i,t]) <- pMainstem[t]  +
                    betaT*Tag[i] + betaFp*Discharge[i]
              } # t
            } # i
            # Detection probability on logit scale
              for(t in 1:(n.occasions-1)){
                pMainstem[t] <- log(pMain[t]/(1-pMain[t]))
              }

          # Prior distributions for survival and detection probs
              for(t in 1:(n.occasions-1)){
                # Group-specific survival
                  Smain[t] ~ dunif(0,1)
                # Detection
                  pMain[t] ~ dunif(0,1)
              } # t

        # PISCATAQUIS RIVER SURVIVAL & DETECTION ###############################
          # PISC params above confluence
            # Parameters being monitored
              for(t in 1:(CONF)){
                # Survival probabilities standardized by distance
                  S.Piscataquis[t] <- pow(Spisc[t],1/dPisc[t])
              } # t

            # Detection probability with covariates
              for(i in 1:nind){
                for(t in 1:(CONF-1)){
                  logit(p.Piscataquis[i,t]) <- pPiscataquis[t] +
                  betaT*Tag[i] + betaFp*Discharge[i]
                } # t
                for(t in CONF:(n.occasions-1)){
                  p.Piscataquis[i,t] <- 1
                } # t
              } # i
              # Detection probabilities on logit scale
                for(t in 1:(CONF-1)){
                  pPiscataquis[t] <- log(pPisc[t]/(1-pPisc[t]))
                } # t

            # Priors and constants for survival and detection in Piscataquis
              # Detection
                  for(t in 1:CONF-1){
                    pPisc[t] ~ dunif(0,1)
                  } # t
                  for(t in CONF:(n.occasions-1)){
                    pPisc[t] <- 1
                  } # t
              # Survival
                for(t in 1:CONF){
                  Spisc[t] ~ dunif(0,1)
                } # t
                for(t in (CONF+1):(n.occasions-1)){
                  Spisc[t] <- 1
                } # t

          # STILLWATER BRANCH SURVIVAL AND DETECTION ###########################
            # Parameters being monitored
              # Survival
                for(t in (STILL+1):(CONFB)){
                    S.Stillwater[t] <- pow(Sstill[t],1/dstill[t])
                }
              # Detection
                for(i in 1:nind){
                  for(t in 1:(n.occasions-1)){
                   logit(p.Stillwater[i,t]) <- pStillwater[t] + betaT*Tag[i] +
                   betaFp*Discharge[i]
                  } # t
                } # i

            # Logit-transformation of detection probability
              for(t in 1:(n.occasions-1)){
                pStillwater[t] <- log(pStill[t]/(1-pStill[t]))
              } # t

            # Priors for Stillwater survival and detection probabilities
              # Survival probabilities
                  for(t in 1:STILL){
                    Sstill[t] <- 1
                  }  # t
                  for(t in (STILL+1):(CONFB)){
                    Sstill[t] ~ dunif(0,1)
                  } # t
                  for(t in (CONFB+1):(n.occasions-1)){
                    Sstill[t] <- 1
                  } # t
              # Detection probabilities
                for(t in 1:STILL){
                  pStill[t] <- 1
                } # t
                for(t in (STILL+1):(CONFB)){
                  pStill[t] ~ dunif(0,1)
                } # t
                for(t in (CONFB+1):(n.occasions-1)){
                  pStill[t] <- 1
                } # t

        # STATE-TRANSITION PROBABILITIES #######################################
            # Mainstem transitions #############################################
              for(t in 1:(STILL-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

              psiM[1,STILL] ~ dunif(0, 1)

              psiM[2,STILL] <- 0
              for(t in (STILL+1):(n.occasions-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

            # Piscataquis transitions ##########################################
              for(t in 1:(CONF-1)){
                  psiP[1,t] <- 0
                  psiP[2,t] <- 1
              }
              psiP[1,CONF] <- 1
              psiP[2,CONF] <- 0
              for(t in (CONF+1):(n.occasions-1)){
                  psiP[1,t] <-0
                  psiP[2,t] <-1
               }

            # Stillwater transitions ###########################################
              for(t in 1:(CONFB-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }
              psiS[1,CONFB] <- 1
              psiS[2,CONFB] <- 0
              for(t in (CONFB+1):(n.occasions-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }

            # Final stat-transition for each ###################################
              for(t in 1:(n.occasions-1)){
              # Calculate last transition probability
                psiM[3,t] <- 1-psiM[1,t]-psiM[2,t]
                psiP[3,t] <- 1-psiP[1,t]-psiP[2,t]
                psiS[3,t] <- 1-psiS[1,t]-psiS[2,t]
              } # t

        # Four-dimensional state-transition and detection probability matrices
        # conditional on individual i being in state h at time t
          for(i in 1:nind){
            for(t in f[i]:n.occasions-1){
              # Define probability of state h(t+1) given h(t)
                ps[1,i,t,1] <- Smain[t]*psiM[1,t]
                ps[1,i,t,2] <- Smain[t]*psiM[2,t]
                ps[1,i,t,3] <- Smain[t]*psiM[3,t]
                ps[1,i,t,4] <- 1-Smain[t]
                ps[2,i,t,1] <- Spisc[t]*psiP[1,t]
                ps[2,i,t,2] <- Spisc[t]*psiP[2,t]
                ps[2,i,t,3] <- Spisc[t]* psiP[3,t]
                ps[2,i,t,4] <- 1-Spisc[t]
                ps[3,i,t,1] <- Sstill[t]*psiS[1,t]
                ps[3,i,t,2] <- Sstill[t]*psiS[2,t]
                ps[3,i,t,3] <- Sstill[t]*psiS[3,t]
                ps[3,i,t,4] <- 1-Sstill[t]
                ps[4,i,t,1] <- 0
                ps[4,i,t,2] <- 0
                ps[4,i,t,3] <- 0
                ps[4,i,t,4] <- 1
              # Define probability of detection given h(t)
                po[1,i,t,1] <- p.Mainstem[i,t]
                po[1,i,t,2] <- 0
                po[1,i,t,3] <- 0
                po[1,i,t,4] <- 1-p.Mainstem[i,t]
                po[2,i,t,1] <- 0
                po[2,i,t,2] <- p.Piscataquis[i,t]
                po[2,i,t,3] <- 0
                po[2,i,t,4] <- 1-p.Piscataquis[i,t]
                po[3,i,t,1] <- 0
                po[3,i,t,2] <- 0
                po[3,i,t,3] <- p.Stillwater[i,t]
                po[3,i,t,4] <- 1-p.Stillwater[i,t]
                po[4,i,t,1] <- 0
                po[4,i,t,2] <- 0
                po[4,i,t,3] <- 0
                po[4,i,t,4] <- 1
            } # t
          } # i

      # Likelihood
        for(i in 1:nind){
          # Define latent state at first capture
            z[i,f[i]] <- y[i,f[i]]
            for (t in (f[i]+1):n.occasions){
              # State process: draw S(t) given S(t-1)
                z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
              # Observation process: draw O(t) given S(t)
                y[i,t] ~ dcat(po[z[i,t],i,t-1,])
            } #t
        } #i
    }
  ",fill = TRUE)
  sink()
# End model specification

# Function to create known latent states z
  known.state.ms <- function(ms, notseen){
   # notseen: label for not seen
   state <- ms
   state[state==notseen] <- NA
   for (i in 1:dim(ms)[1]){
      m <- min(which(!is.na(state[i,])))
      state[i,m] <- NA
      }
   return(state)
   }

# Function to create initial values for unknown z
  ms.init.z <- function(ch, f){
    for (i in 1:dim(ch)[1]){
      ch[i,1:f[i]] <- NA
    }
    states <- max(ch, na.rm = TRUE)
    known.states <- 1:(states-1)
    v <- which(ch==states)
    ch[-v] <- NA
    ch[v] <- sample(known.states, length(v), replace = TRUE)
    return(ch)
  }

# Bundle data
  bugs.data <- list(y = CH, f = f, n.occasions = dim(CH)[2],
    nind = dim(CH)[1], z = known.state.ms(CH, 4), CONF=9, STILL=12, CONFB=15,
  	dMain=dMain, dPisc=dPisc, dstill=dstill, Tag=Tag, Discharge=Discharge)

# Initial values
  inits <- function(){
    list(
      Smain=c(runif((n.occasions-1), 0, 1)),
      Spisc=c(runif(9, 0, 1), rep(NA, 9)),
      Sstill=c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3)),
      pMain=c(runif((n.occasions-1), 0, 1)),
      pPisc=c(runif(8, 0, 1), rep(NA,10)),
      pStill=c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3)),
      beta.T=c(runif(1, 0, 1)),
      beta.Fp=c(runif(1, 0, 1)),
      z=ms.init.z(CH, f)
    )
  }

# Parameters monitored
  parameters <- c("S.Mainstem", "S.Piscataquis", "S.Stillwater", "pMain",
     "pPisc","pStill", "psiM")

# MCMC settings
  ni <- 33000
  nt <- 3
  nb <- 3000
  nc <- 3

# Call BUGS from R and run the model
  msGOD.MA <- bugs(bugs.data, inits, parameters, "ms.god", n.chains = nc,
    n.thin = nt, n.iter = ni, n.burnin = nb, bugs.directory=bugs.dir,
    working.directory=getwd(), debug=T)

# Print the model results
  GodMultiAnnRes <- data.frame(msGOD.MA$summary)
  GodMultiAnnRes

# Write the annual survival estimates to an rdata file
  parmsMA <- data.frame(msGOD.MA$sims.list)
  save(parmsMA, file='MultiAnnualSurvivalEstimates.rda')

# Read the file back in to get reach-specific estimates
  load('C:/Time/MultiAnnualSurvivalEstimates.rda')

  # Get distances
    distances <- c(dMain[1:17], dPisc, dStill)

  # Get reach-specific estimates
    # Reaches of interest
      reach <- data.frame(parmsMA[ , c(1:17, 19:30)])
    # Raise per-km rate to distance
      for(i in 1:ncol(reach)){
        reach[ ,i] <- reach[ ,i]^distances[i]
      }
      head(reach)

  # Calculate cumulative survival for Pisc
    SpisC <- c()
    pb <- txtProgressBar(min = 0, max = nrow(reach), style = 3)
    for(i in 1:nrow(reach)){
      Sys.sleep(0.001)
      SpisC[i] <- prod(reach[i,18:26])*prod(reach[i,10:12])*
        ((1-parmsMA[i,61])*prod(reach[i,13:15]) +
        	parmsMA[i,61]*prod(reach[i,27:29]))*
        prod(reach[i,16:17])
      setTxtProgressBar(pb, i)
    }
    close(pb)

    mean(unlist(SpisC))
    sd(unlist(SpisC))
    quantile(unlist(SpisC), probs=c(0.025, 0.975))

  # Calculate cumulative survival for Mainstem
    SmainC <- c()
    pb <- txtProgressBar(min = 0, max = nrow(reach), style = 3)
    for(i in 1:nrow(reach)){
      Sys.sleep(0.001)
      SmainC[i] <- prod(reach[i,1:12])*
        ((1-parmsMA[i,61])*prod(reach[i,13:15]) +
        	parmsMA[i,61]*prod(reach[i,27:29]))*
        prod(reach[i,16:17])
      setTxtProgressBar(pb, i)
    }
    close(pb)

    mean(unlist(SmainC))
    sd(unlist(SmainC))
    quantile(unlist(SmainC), probs=c(0.025, 0.975))

# Read the file back in to get reach-specific estimates
  load('C:/Time/MultiAnnualSurvivalEstimates.rda')

# Get distances for each reach
  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival.
	# It's called ch9.
    load('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda')
  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

  # Get distances
    distances <- c(dMain[1:17], dPisc, dStill)

# Reach type for each interval
  freeFlowing <- c(parmsMA[ , c(1, 2, 5, 6, 7, 10, 11, 12, 19, 24, 25)])
  freeFlowing2 <- data.frame(parmsMA[ , c(1, 2, 5, 6, 7, 10,
    11, 12, 24, 25)])

	dams <- c(parmsMA[ , c(4, 9, 14, 15, 17, 20, 22, 23, 27, 28, 29, 30)])
	dams2 <- data.frame(parmsMA[ , c(4, 9, 14, 15, 17, 20, 22, 23, 27, 28, 29,
		30)])

	heads <- c(parmsMA[ , c(3, 8, 13, 16, 19, 21, 26)])
	heads2 <- data.frame(parmsMA[ , c(3, 8, 13, 16, 19, 21, 26)])


# Calculate cumulative survival in the absence of dams
# Create a df in which headponds and dams are replaced with freeFlowing
  repl <- parmsMA
# Create a vector of mean survival through unimpounded reaches
  refs <- apply(freeFlowing2, 1, mean)
  refs2 <- refs
  for(i in 1:ncol(repl)){
    if(names(repl)[i]%in%names(dams2) | names(repl)[i]%in%names(heads2)){
      repl[ ,i] <- refs2
    } else {
      next;
    }
  }

# Get average survival in reaches with dams
	meanDams = mean(matrix(unlist(dams), ncol = length(names(dams)), byrow=TRUE))
	sdDams = sd(matrix(unlist(dams), ncol = length(names(dams)), byrow=TRUE))

# Get average survival through free-flowing reaches
	meanFF = mean(unlist(freeFlowing))
	sdFF = sd(unlist(freeFlowing))

  # Get reach-specific estimates
    # Reaches of interest
      reach <- data.frame(repl[ , c(1:17, 19:30)])
    # Raise per-km rate to distance
      for(i in 1:ncol(reach)){
        reach[ ,i] <- reach[ ,i]^distances[i]
      }
      head(reach)

  # Calculate cumulative survival for Pisc
    SpisC <- c()
    pb <- txtProgressBar(min = 0, max = nrow(reach), style = 3)
    for(i in 1:nrow(reach)){
      Sys.sleep(0.001)
      SpisC[i] <- prod(reach[i,18:26])*prod(reach[i,10:12])*
        ((1-repl[i,61])*prod(reach[i,13:15]) + repl[i,61]*prod(reach[i,27:29]))*
        prod(reach[i,16:17])
      setTxtProgressBar(pb, i)
    }
    close(pb)

    mean(unlist(SpisC))
    sd(unlist(SpisC))
    quantile(unlist(SpisC), probs=c(0.025, 0.975))

  # Calculate cumulative survival for Mainstem
    SmainC <- c()
    pb <- txtProgressBar(min = 0, max = nrow(reach), style = 3)
    for(i in 1:nrow(reach)){
      Sys.sleep(0.001)
      SmainC[i] <- prod(reach[i,1:12])*
        ((1-repl[i,61])*prod(reach[i,13:15]) + repl[i,61]*prod(reach[i,27:29]))*
        prod(reach[i,16:17])
      setTxtProgressBar(pb, i)
    }
    close(pb)

    mean(unlist(SmainC))
    sd(unlist(SmainC))
    quantile(unlist(SmainC), probs=c(0.025, 0.975))

# Compile posteriors by reach type
  freeFlowing <- c(parmsMA[ , c(1, 2, 5, 6, 7, 10, 11, 12, 19, 24, 25)])
  freeFlowing2 <- data.frame(parmsMA[ , c(1, 2, 5, 6, 7, 10,
    11, 12, 24, 25)])

	dams <- c(parmsMA[ , c(4, 9, 14, 15, 17, 20, 22, 23, 27, 28, 29, 30)])
	dams2 <- data.frame(parmsMA[ , c(4, 9, 14, 15, 17, 20, 22, 23, 27, 28, 29,
		30)])

	heads <- c(parmsMA[ , c(3, 8, 13, 16, 19, 21, 26)])
	heads2 <- data.frame(parmsMA[ , c(3, 8, 13, 16, 19, 21, 26)])

# Make grouped posteriors for Penobscot River assessments 04/17/2015
	# Mean and quantiles for ALL free flowing reaches
		allFreeFlowing = freeFlowing

	# Mean and quantiles for PISC free flowing reaches
    piscFree = (freeFlowing[substr(names(freeFlowing), start =3, stop=3)=='P'])

	# Mean and quantiles for UpperMainstem free flowing reaches
    upperMainFree = (freeFlowing[1:5])

	# Mean and quantiles for UpperMainstem free flowing reaches
    lowerMainFree = (freeFlowing[6:8])

	# Create a list out of all of these and write it to a csv
    freeFlowingReaches = list(unlist(allFreeFlowing), unlist(piscFree),
    	unlist(upperMainFree), unlist(lowerMainFree))

	# Create a function to get quantiles that can be applied to the list
	  myQuants <- function(x) quantile(x,
	  	probs=c(0.01, 0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975,
	  		0.99))
	# Apply the quantile function to the list
	  lapply(freeFlowingReaches, myQuants)

	# Write a file with the posteriors for each section of the river
	  write.list(freeFlowingReaches, 'freeFlowingPosteriors.csv')

# FIGURE 4.- MULTIANNUAL SURVIVAL ESTIMATES BY REACH
# Graph the results of the God Model
	tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & tables/Figure3.tiff',
  width=3500, height=5000, pointsize=18, compression="lzw", res=500)

 # Free flowing reaches
	 par(mfrow=c(3,1), oma=c(5,6,.5,1))
	 par(mar=c(1,1,.25,1))
   plot(x=NULL, y=NULL,ylim=c(0,150), xlim=c(0.90,1.00), yaxt='n', xaxt='n',
     ylab='', xlab='')
   cols=grey.colors(16)
   for(i in 1:ncol(freeFlowing2)){
     par(new=TRUE)
     plot(density(freeFlowing2[,i], bw=0.002), xlab='', ylab='', yaxt='n',
       xlim=c(0.90, 1.00), ylim=c(0, 200), main='', col=cols[i], xaxt='n')
   }
   #mtext(side=2, 'Density', line=5, cex=2, adj=-.75)
# 	 axis(side=1, at=c(seq(0.90, 1.00, 0.02)), c('0.90', '0.92', '0.94', '0.96',
# 	 	 '0.98', '1.00'), cex.axis=1.5)
	 axis(side=2, at=c(seq(0, 200, 50)), c('0', '50', '100', '150', '200'),
	 	cex.axis=1.5, las=2)
	 text(x=0.8965, y=190, '(a) Free-flowing', adj=0, cex=1.5)
	 box()

 # Head ponds
	 par(mar=c(1,1,.25,1))
   plot(x=NULL, y=NULL,ylim=c(0,150), xlim=c(0.90,1.00), yaxt='n', xaxt='n',
     ylab='', xlab='')
   cols=grey.colors(16)
   for(i in 1:ncol(heads2)){
     par(new=TRUE)
     plot(density(heads2[,i], bw=0.002), xlab='', ylab='', yaxt='n',
       xlim=c(0.90, 1.00), ylim=c(0, 200), main='', col=cols[i], xaxt='n')
   }
   mtext(side=2, 'Density', line=4.5, cex=1.5)
# 	 axis(side=1, at=c(seq(0.90, 1.00, 0.02)), c('0.90', '0.92', '0.94', '0.96',
# 	 	 '0.98', '1.00'), cex.axis=1.5)
	 axis(side=2, at=c(seq(0, 200, 50)), c('0', '50', '100', '150', '200'),
	 	cex.axis=1.5, las=2)
	 text(x=0.8965, y=190, '(b) Head ponds', adj=0, cex=1.5)
	 box()

 # Dams
	 par(mar=c(1,1,.25,1))
   plot(x=NULL, y=NULL,ylim=c(0, 150), xlim=c(0.90, 1.00), yaxt='n', xaxt='n',
     ylab='', xlab='')
   for(i in 1:ncol(dams2)){
     par(new=TRUE)
     plot(density(dams2[,i], bw=0.002), xlab='', ylab='', yaxt='n',
       xlim=c(0.90, 1.00), ylim=c(0, 200), main='', col=cols[i], xaxt='n')
   }
   mtext(side=1, expression(paste('Apparent survival per km (',
   	widehat(italic(S))[D[t]],')')), line=4.5, cex=1.5)
	 axis(side=1, at=c(seq(0.90, 1.00, 0.02)), c('0.90', '0.92', '0.94', '0.96',
	 	 '0.98', '1.00'), cex.axis=1.5)
	 axis(side=2, at=c(seq(0, 200, 50)), c('0', '50', '100', '150', '200'),
	 	cex.axis=1.5, las=2)
	 text(x=0.8965, y=190, '(c) Dams', adj=0, cex=1.5)
	 box()
 # Turn off the graphics device and save the tiff file
   dev.off()
################################################################################

################################################################################
# Run multi-annual survival model with environmental covariates of survival and
# tag/flow effects on detection probability
#
# INPUT FILENAME(S): 'GodCapturesReady.csv', 'GodlyCovariates.csv'
# MY FUNCTIONS:       Inits, ms.init.z, known.state.ms,
# NAMED OUPUT:       'GD', 'CH', 'ms.god', 'msGOD.Tag'
#                    'GodTagRes', 'parmsdis' (df of posterior estimates)
# OUTPUT FILENAME:   'pseudoPriorsForRoundTwo.r', 'GodModelPrelimResults.csv'
################################################################################
# PACKAGE INSTALL AND LOAD
  #install.packages('rjags')
  #install.packages('R2jags')
  #install.packages('R2WinBUGS')
  #install.packages('reshape2')
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(reshape2)

# FUNCTION DEFINITIONS
  # Make logit transform fxn
    logit<-function(x){
      log(x/(1-x))
    }

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Set bugs directory for WinBUGS
    bugs.dir = 'C:/WinBUGS14/'

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]


  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

# DATA DEFINITIONS FOR MODEL CODE
  # Define number of occasions in CH
    n.occasions <- ncol(CH)

  # Define number of states, including dead
    n.states <- 4

  # Define number of observable states (one for unobserved)
    n.obs <- 4

  # Define number of individuals
    nind <- nrow(CH)

  # Define groups based on year
    #group <- as.numeric(as.factor(covs$Year))

  # Define number of groups
    #n.groups <- length(unique(group))

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

    # Accumulated thermal units
      ATU <- scale(covs$ATU)[,1]
      ATUS <- c((scale(covs$ATU))[,1]^2)

    # Accumulated thermal units
      Ph <- scale(covs$Ph)[,1]
      PhS <- c((scale(covs$Ph))[,1]^2)

    # Accumulated thermal units
      T <- scale(covs$T)[,1]
      TS <- c((scale(covs$T))[,1]^2)

    # Rearing history
      Origin <- as.numeric(factor(covs$Origin,
        levels=c("Wild", "Hatchery")))-1

# RUN THE MODEL
# Set a new working directory to separate the files for different models
  dir.create('C:/Tag')
  setwd('C:/Tag')

# Specify the model in JAGS language
  sink("ms.god")
  cat("
    model{

      # Years
        # 1 2005
        # 2 2006
        # 3 2009
        # 4 2010
        # 5 2011
        # 6 2012
        # 7 2013
        # 8 2014

      # Tags
        # 0 V7
        # 1 V9

      # Origin
        # 1 Wild
        # 2 Hatchery

      # Parameters:
        # S.Mainstem
        # S.Piscataquis
        # S.Stillwater
        # psiM
        # psiP
        # psiS
        # p.Mainstem
        # p.Pisquatis
        # p.Stillwater

      # States:
        # 1 Alive in Mainstem
        # 2 Alive in Piscataquis
        # 3 Alive in Stillwater
        # 4 Dead

      # Observations:
        # 1 Detected in Mainstem
        # 2 Detected in Piscataquis
        # 3 Detected in Stillwater
        # 4 Not Detected

      # PRIORS & CONSTRAINTS ###################################################
        # COVARIATE EFFECTS ####################################################
          # State-transition covariates
            # Prior on fixed effect of flow for SW transition and data transform
              betaFm <- log(beta.Fm / (1 - beta.Fm))
              beta.Fm ~ dunif(0,1)
            # Effect of origin on Stillwater Branch usage and data transform
              betaOm <- log(beta.Om / (1 - beta.Om))
              beta.Om ~ dunif(0,1)

          # Detection covariates
            # Prior on fixed effect of tag type and data transform
              betaT <- log(beta.T / (1 - beta.T))
              beta.T ~ dunif(0,1)

            # Prior on fixed effect of flow for detection and data transform
              betaFp <- log(beta.Fp / (1 - beta.Fp))
              beta.Fp ~ dunif(0,1)

          # Survival covariates
            # Fixed effect of discharge on survival
              # Linear effect of discharge on survival and data transform
                betaFs <- log(beta.Fs / (1 - beta.Fs))
                beta.Fs ~ dunif(0,1)
              # Second-order term for discharge and data transform
                betaFs2 <- log(beta.Fs2 / (1 - beta.Fs2))
                beta.Fs2 ~ dunif(0,1)

            # Fixed effect of ATU
              # Linear effect of ATU on survival and data transform
                betaAs <- log(beta.As / (1 - beta.As))
                beta.As ~ dunif(0,1)
              # Second-order term for ATU and data transform
                betaAs2 <- log(beta.As2 / (1 - beta.As2))
                beta.As2 ~ dunif(0,1)

            # Fixed effect of photoperiod
              # Linear effect of photoperiod on survival and data transform
                betaPs <- log(beta.Ps / (1 - beta.Ps))
                beta.Ps ~ dunif(0,1)
              # Second-order term for photoperiod and data transform
                betaPs2 <- log(beta.Ps2 / (1 - beta.Ps2))
                beta.Ps2 ~ dunif(0,1)

            # Fixed effect of temperature
              # Linear effect of temperature on survival and data transform
                betaTs <- log(beta.Ts / (1 - beta.Ts))
                beta.Ts ~ dunif(0,1)
              # Second-order term for temperature and data transform
                betaTs2 <- log(beta.Ts2 / (1 - beta.Ts2))
                beta.Ts2 ~ dunif(0,1)

        # MAINSTEM PENOBSCOT RIVER SURVIVAL & DETECTION ########################
          # Survival and detection modeled as functions of covariates
            for(i in 1:nind){ # For each individual
              for(t in 1:(n.occasions-1)){ # In each reach
                # Define logit-scale detection probability WRT covariates
                  logit(p.Mainstem[i,t]) <- (pMainstem[t] + betaT*Tag[i] +
                    betaFp*Discharge[i])
                # Define logit-scale survival probability WRT covariates
                  logit(SMain[i,t]) <- (SMainstem[t] + betaFs*Discharge[i] +
                    betaFs2*DischargeS[i] + betaAs*ATU[i] + betaAs2*ATUS[i] +
                    betaPs*Ph[i] + betaPs2*PhS[i] + betaTs*T[i] + betaTs2*TS[i])
              } # t
            } # i

          # Data transformation for model output (parameters monitored)
            for(t in 1:(n.occasions-1)){ # In each reach
              # Survival probabilities standardized by reach length in state 1
                S.Mainstem[t]<-pow(Smain[t], 1/dMain[t])
              # Derived quantities for detection probability
                V7pMain[t] <- exp(pMainstem[t])/(1 + exp(pMainstem[t]))
                V9pMain[t] <- exp(pMainstem[t] + betaT)/
                  (1 + exp(pMainstem[t] + betaT))
            } # t

          # Prior distributions for mainstem detection and survival
            # Logit-scale priors for survival and detection
              for(t in 1:(n.occasions-1)){ # In each reach
                SMainstem[t] <- log(Smain[t]/(1-Smain[t])) # Survival intercept
                pMainstem[t] <- log(pMain[t]/(1-pMain[t])) # Detection intercept
              }
            # Naive prior distributions for detection and survival probabilities
            # on real probability scale
              for(t in 1:(n.occasions-1)){ # In each reach
                # Detection probability
                  pMain[t] ~ dunif(0,1)
                # Survival probability
                  Smain[t] ~ dunif(0,1)
              } # t

        # PISCATAQUIS RIVER SURVIVAL & DETECTION ###############################
          # Detection and survival modeled as functions of covariates
            for(i in 1:nind){ # For each individual
              for(t in 1:(CONF-1)){ # In each reach
                # Define logit-scale detection probability WRT covariates
                  logit(p.Piscataquis[i,t]) <- (pPiscataquis[t] + betaT*Tag[i] +
                    betaFp*Discharge[i])
              } # t
              for(t in 1:(CONF)){ # In each reach
                # Define logit-scale survival probability WRT covariates
                  logit(SPisc[i,t]) <- (SPiscataquis[t] + betaFs*Discharge[i] +
                    betaFs2*DischargeS[i] + betaAs*ATU[i] + betaAs2*ATUS[i] +
                    betaPs*Ph[i] + betaPs2*PhS[i] + betaTs*T[i] + betaTs2*TS[i])
              } # t

          # Detection probability downstream from the confluence of the
            # Piscataquis and Penobscot Rivers is constrained to be one.
              for(t in (CONF):(n.occasions-1)){ # In each reach
                p.Piscataquis[i,t] <- 1 # Detection constraint
              } # t
              for(t in (CONF+1):(n.occasions-1)){ # In each reach
                SPisc[i,t] <- 1 # Survival constraint
              } # t
            } # i

          # Data transformation for model output
            # Survival standardized by reach length in the Piscataquis River
              for(t in 1:(CONF)){ # In each reach
                S.Piscataquis[t] <- pow(Spisc[t], 1/dPisc[t])
              } # t
            # Derived quantities for detection probability
              for(t in 1:(CONF-1)){ # In each reach
                V7pPisc[t] <- exp(pPiscataquis[t])/(1 + exp(pPiscataquis[t]))
                V9pPisc[t] <- exp(pPiscataquis[t] + betaT)/
                  (1 + exp(pPiscataquis[t] + betaT))
              } # t

          # Prior distributions for survival and detection probability in
          # the Piscataquis River upstream of the confluence
            # Logit-scale transformation for detection probability in the
              # Piscataquis River
                for(t in 1:(CONF-1)){ # In each reach
                  pPiscataquis[t] <- log(pPisc[t] / (1 - pPisc[t]))
                } # t
                for(t in 1:(CONF)){ # In each reach
                  SPiscataquis[t] <- log(Spisc[t] / (1 - Spisc[t]))
                } # t
              # Probability-scale priors for detection and survival
                # Detection probability in the Piscataquis above confluence
                  for(t in 1:(CONF-1)){ # In each reach
                    pPisc[t] ~ dunif(0,1)
                  } # t
                # Survival in the Piscataquis River state above confluence
                  for(t in 1:(CONF)){ # In each reach
                    Spisc[t] ~ dunif(0,1)
                  } # t

          # STILLWATER BRANCH SURVIVAL AND DETECTION ###########################
            # Detection and survival probability in Stillwater Branch
              for(i in 1:nind){ # For each individual
                # Detection and survival probabilities in the Stillwater Branch
                # upstream of where it starts are constrained to be one
                  for(t in 1:STILL){ # In each reach
                    p.Stillwater[i,t] <- 1
                    SStill[i,t] <- 1
                  } # t
                # Detection and survival probabilities in the Stillwater Branch
                # modeled as functions of covariates
                  for(t in (STILL+1):(CONFB)){ # In each reach
                    # Define logit-scale detection probability WRT covariates
                      logit(p.Stillwater[i,t]) <- (pStillwater[t] +
                        betaT*Tag[i] + betaFp*Discharge[i])
                    # Define logit-scale survival probability WRT covariates
                      logit(SStill[i,t]) <- (SStillwater[t] +
                        betaFs*Discharge[i] + betaFs2*DischargeS[i] +
                        betaAs*ATU[i] + betaAs2*ATUS[i] +
                        betaPs*Ph[i] + betaPs2*PhS[i] +
                        betaTs*T[i] + betaTs2*TS[i])
                  } # t
                # Detection and survival downstream of the confluence of the
                # Stillwater Branch and the Penobscot River
                  for(t in (CONFB+1):(n.occasions-1)){ # In each reach
                    p.Stillwater[i,t] <- 1
                    SStill[i,t] <- 1
                  } # t
              } # i

            # Data transformation for model outputs
              for(t in (STILL+1):CONFB){ # In each reach
                # Survival in the Stillwater Branch standardized by reach
                # lengths
                  S.Stillwater[t] <- pow(Sstill[t],1/dstill[t])
                # Derived quantities for detection probability
                  V7pStill[t] <- exp(pStillwater[t])/(1 + exp(pStillwater[t]))
                  V9pStill[t] <- exp(pStillwater[t] + betaT)/
                    (1 + exp(pStillwater[t] + betaT))
                }

            # Prior distributions and parameter for detection and survival
            # probabilities in the Stillwater Branch
              # Logit-scale survival and detection probability
                for(t in (STILL+1):CONFB){ # In each reach
                  pStillwater[t] <- log(pStill[t] / (1 - pStill[t]))
                  SStillwater[t] <- log(Sstill[t] / (1 - Sstill[t]))
                }
              # Probablity-scale priors for survival and detection probability
              # in the Stillwater Branch
                for(t in (STILL+1):CONFB){ # In each reach
                  # Detection probabilities in the Stillwater Branch
                    pStill[t] ~ dunif(0,1)
                  # Survival probabilities through reaches in the Stillwater
                    Sstill[t] ~ dunif(0,1)
                } # t

        # STATE-TRANSITION PROBABILITIES #######################################
            # Mainstem transitions #############################################
              for(i in 1:nind){
                # Mainstem to mainstem constraint:
                # Always one except at head of Stillwater
                  for(t in 1:(n.occasions-1)){
                    PsiM1[i,t] <- 1-PsiM2[i,t]-PsiM3[i,t]
                  }

                # Mainstem to Piscataquis constraint:
                # Fish can't move from the mainstem into the Piscataquis
                  for(t in 1:(n.occasions-1)){
                    PsiM2[i,t] <- 0
                  }

                # Mainstem to Stillwater constraint:
                  # Fish can't move into Stillwater before the fork
                    for(t in 1:(STILL-1)){ # In each reach
                      PsiM3[i,t] <- 0
                    } # t

                # Covariate effects on transition into the Stillwater Branch
                # from the main-stem Penobscot River
                  logit(PsiM3[i, STILL]) <- (psi.M3[STILL] +
                    betaFm*Discharge[i] + betaOm*Origin[i])

                # Mainstem to Stillwater constraint:
                # Fish can't move into the Stillwater Branch downstream of it!
                  for(t in (STILL+1):(n.occasions-1)){ # In each reach
                    PsiM3[i,t] <- 0
                  } # t
                }

                # Prior distribution and data transform for probability of
                # moving into the Stillwater Branch from the main-stem
                  psi.M3[STILL] <- log(psiM3[STILL]/(1-psiM3[STILL]))
                  psiM3[STILL] ~ dunif(0, 1)

            # Piscataquis transitions ##########################################
              # Above the confluence of Piscataquis and Penosbcot Rivers
                for(t in 1:(CONF-1)){ # In each reach
                  psiP[1,t] <- 0
                  psiP[2,t] <- 1
                } # t

              # Fish must move into Penobscot from Piscataquis at the confluence
                psiP[1,CONF] <- 1
                psiP[2,CONF] <- 0

              # Fish can't get back into the 'squis after they leave
                for(t in (CONF+1):(n.occasions-1)){ # In each reach
                  psiP[1,t] <-0
                  psiP[2,t] <-1
                } # t

            # Stillwater transitions ###########################################
              # Fish can't move out of the Stillwater until the conlfluence of
              # the Stillwater Branch and the Mainstem Penobscot River
                for(t in 1:(CONFB-1)){
                  psiS[1,t] <- 0
                } # t

              # Fish must move out of the Stillwater at the confluence with the
              # Penobscot River
                psiS[1,CONFB] <- 1

              # Fish can't move into the Penobscot in any time other than at
              # the confluence of the Stillwater Branch and Penobscot River
                for(t in (CONFB+1):(n.occasions-1)){ # In each reach
                  psiS[1,t] <- 0
                } # t

              # Fish can never move into the Piscataquis River from the
              # Stillwater Branch
                for(t in 1:(n.occasions-1)){
                  psiS[2,t] <- 0
                } # t

            # Final stat-transition for each ###################################
              for(t in 1:(n.occasions-1)){ # In each reach
                # Calculate last transition probability
                # Note that the three types of transition probs for Mainstem are
                # coded above.
                  psiP[3,t] <- 1-psiP[1,t]-psiP[2,t] # Piscataquis to Stillwater
                  psiS[3,t] <- 1-psiS[1,t]-psiS[2,t] # Stillwater to Stillwater
              } # t

        # Four-dimensional state-transition and detection probability matrices
        # conditional on individual i being in state h at time t
          for(i in 1:nind){ # For each individual
            for(t in f[i]:n.occasions-1){ # In each reach after initial capture
              # Define probability of state h(t+1) given h(t)
                ps[1,i,t,1] <- SMain[i,t]*PsiM1[i,t]
                ps[1,i,t,2] <- SMain[i,t]*PsiM2[i,t]
                ps[1,i,t,3] <- SMain[i,t]*PsiM3[i,t]
                ps[1,i,t,4] <- 1-SMain[i,t]
                ps[2,i,t,1] <- SPisc[i,t]*psiP[1,t]
                ps[2,i,t,2] <- SPisc[i,t]*psiP[2,t]
                ps[2,i,t,3] <- SPisc[i,t]* psiP[3,t]
                ps[2,i,t,4] <- 1-SPisc[i,t]
                ps[3,i,t,1] <- SStill[i,t]*psiS[1,t]
                ps[3,i,t,2] <- SStill[i,t]*psiS[2,t]
                ps[3,i,t,3] <- SStill[i,t]*psiS[3,t]
                ps[3,i,t,4] <- 1-SStill[i,t]
                ps[4,i,t,1] <- 0
                ps[4,i,t,2] <- 0
                ps[4,i,t,3] <- 0
                ps[4,i,t,4] <- 1
              # Define probability of detection given h(t)
                po[1,i,t,1] <- p.Mainstem[i,t]
                po[1,i,t,2] <- 0
                po[1,i,t,3] <- 0
                po[1,i,t,4] <- 1-p.Mainstem[i,t]
                po[2,i,t,1] <- 0
                po[2,i,t,2] <- p.Piscataquis[i,t]
                po[2,i,t,3] <- 0
                po[2,i,t,4] <- 1-p.Piscataquis[i,t]
                po[3,i,t,1] <- 0
                po[3,i,t,2] <- 0
                po[3,i,t,3] <- p.Stillwater[i,t]
                po[3,i,t,4] <- 1-p.Stillwater[i,t]
                po[4,i,t,1] <- 0
                po[4,i,t,2] <- 0
                po[4,i,t,3] <- 0
                po[4,i,t,4] <- 1
            } # t
          } # i

      # Likelihood
        for(i in 1:nind){ # For each individual
          # Define latent state at first capture
            z[i,f[i]] <- y[i,f[i]]
            for (t in (f[i]+1):n.occasions){ # In each reach after initial
              # State process: draw S(t) given S(t-1)
                z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
              # Observation process: draw O(t) given S(t)
                y[i,t] ~ dcat(po[z[i,t],i,t-1,])
            } #t
        } #i
    }
  ",fill = TRUE)
  sink()
# End model specification

# Function to create known latent states z
  known.state.ms <- function(ms, notseen){
   # notseen: label for not seen
   state <- ms
   state[state==notseen] <- NA
   for (i in 1:dim(ms)[1]){
      m <- min(which(!is.na(state[i,])))
      state[i,m] <- NA
      }
   return(state)
   }

# Function to create initial values for unknown z
  ms.init.z <- function(ch, f){
    for (i in 1:dim(ch)[1]){
      ch[i,1:f[i]] <- NA
    }
    states <- max(ch, na.rm = TRUE)
    known.states <- 1:(states-1)
    v <- which(ch==states)
    ch[-v] <- NA
    ch[v] <- sample(known.states, length(v), replace = TRUE)
    return(ch)
  }
  head(ms.init.z(CH,f), 10)
  head(CH,10)

# Bundle data
  bugs.data <- list(y = CH, f = f, n.occasions = dim(CH)[2],
    nind = dim(CH)[1], z = known.state.ms(CH, 4), CONF=9, STILL=12, CONFB=15,
  	dMain=dMain, dPisc=dPisc, dstill=dstill, Tag=Tag, Discharge=Discharge,
    DischargeS=DischargeS, ATU=ATU, ATUS=ATUS, Ph=Ph, PhS=PhS, T=T, TS=TS,
    Origin=Origin)#,
    #group=as.numeric(as.factor(covs$Year)), n.groups=length(unique(group)))

# Initial values
  inits <- function(){
    list(
      # Survival and detection probabilities
        Smain=runif((n.occasions-1),0,1),   # Mainstem survival
        Spisc=c(runif(9,0,1)),              # Piscataquis survival
        Sstill=c(rep(NA,12), runif(3,0,1)), # Stillwater survival
        pMain=runif((n.occasions-1),0,1),   # Mainstem detection
        pPisc=c(runif(8,0,1)),              # Piscataquis detection
        pStill=c(rep(NA,12), runif(3,0,1)), # Stillwater detection
      # State-transition probabilities
        psiM3=c(rep(NA,11), runif(1,0,1)),  # Mainstem to Stillwater
      # Covariate effects
        beta.Fp=c(runif(1,0,1)),  # Effect of flow on detection
        beta.T=c(runif(1,0,1)),   # Effect of Tag model on detection
        beta.Fs=c(runif(1,0,1)),  # Effect of flow on survival, linear
        beta.Fs2=c(runif(1,0,1)), # Effect of flow on survival, quadratic
        beta.As=c(runif(1,0,1)),  # Effect of ATU on survival, linear
        beta.As2=c(runif(1,0,1)), # Effect of ATU on survival, quadratic
        beta.Ps=c(runif(1,0,1)),  # Effect of photoperiod on survival linear
        beta.Ps2=c(runif(1,0,1)), # Effect of photoperiod on survival, quadratic
        beta.Ts=c(runif(1,0,1)),  # Effect of temperature on survival, linear
        beta.Ts2=c(runif(1,0,1)), # Effect of temp on survival, quadratic
        beta.Fm=c(runif(1,0,1)),  # Effect of flow on use of Stillwater
        beta.Om=c(runif(1,0,1)),  # Effect of origin on use of Stillwater
      # Latent state matrix
        z=ms.init.z(CH, f)
    )
  }

# Parameters monitored
  parameters <- c("S.Mainstem", "S.Piscataquis", "S.Stillwater", "V7pMain",
   "V9pMain", "V7pPisc", "V9pPisc", "V7pStill", "V9pStill", "psiM3",
   "betaT", "betaFp", "betaFs", "betaFs2", "betaAs", "betaAs2", "betaPs",
   "betaPs2", "betaTs", "betaTs2", "betaFm", "betaOm")

# MCMC settings
  ni <- 330
  nt <- 3
  nb <- 30
  nc <- 3

# Call BUGS from R and run the model
  msGOD.Tag <- bugs(bugs.data, inits, parameters, "ms.god", n.chains = nc,
    n.thin = nt, n.iter = ni, n.burnin = nb, bugs.directory=bugs.dir,
    working.directory=getwd(), debug=T)

 # Print the model results
   GodTagRes <- data.frame(msGOD.Tag$summary)
   GodTagRes
   write.table(GodTagRes, 'GodModelPrelimResults.csv', row.names=FALSE,
     sep=',', quote=FALSE)

 # Now create a table containing the data for the posterior distributions of
 # the preliminary run
   parmdis <- data.frame(msGOD.Tag$sims.list)
   save(parmdis, file='pseudoPriorsForRoundTwo.rda')
################################################################################

################################################################################
# Estimate the probability that each of the covariates in the previous model
# are 'in' the model based on a Bernoulli inclusion parameter for each of them.
#
# INPUT FILENAME(S): 'GodCapturesReady.csv', 'GodlyCovariates.csv',
#                    'pseudoPriorsForRoundTwo.r' (parmdis df),
# MY FUNCTIONS:       Inits, ms.init.z, known.state.ms,
# NAMED OUPUT:       'GD', 'CH', 'ms.god', 'msGOD.PIP'
#
# OUTPUT FILENAME:
################################################################################
# PACKAGE INSTALL AND LOAD
  #install.packages('rjags')
  #install.packages('R2jags')
  #install.packages('R2WinBUGS')
  #install.packages('reshape2')
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(reshape2)

# Read in the posteriors from the first run for pseudopriors
  load('C:/Tag/pseudoPriorsForRoundTwo.rda')
  #names(parmdis)

# FUNCTION DEFINITIONS
  # Make logit transform fxn
    logit<-function(x){
      log(x/(1-x))
    }

  # Backtransform from logit
    plogis <- function(x){
      exp(x)/(1+exp(x))
    }

  # Make function for calculating parameters of beta distributions fr method of
  # moments and get hyperparameters for distribution describing survival in
  # free-flowing reaches of the river
    beta.mom<-function(mean,v){
      x<-mean
      a<-x*(x*(1-x)/v-1)
      b<-(1-x)*(x*(1-x)/v-1)
      c(a,b)
    }

# PSEUDOPRIORS FROM THE INITIAL COVARIATE RUN stored in parmdis df
  # Pseudoprior for mainstem to stillwater transition
    psi.still <- beta.mom(mean(parmdis$psiM3),sd(parmdis$psiM3))
  # Pseudopriors for mainstem survival
    sMu <- c() # Parameter a for pseudopriors on mainstem survival
    sMv <- c() # Parameter b for pseudopriors on mainstem survival
    for(i in 1:18){
      sMu[[i]] <- beta.mom(mean(parmdis[,i]),sd(parmdis[,i]))[1]
    }
    for(i in 1:18){
      sMv[[i]] <- beta.mom(mean(parmdis[,i]),sd(parmdis[,i]))[2]
    }
  # Pseudopriors for piscataquis survival
    sPu <- c() # Parameter a for pseudopriors on mainstem survival
    sPv <- c() # Parameter b for pseudopriors on mainstem survival
    for(i in 1:9){
      sPu[[i]] <- beta.mom(mean(parmdis[,(i+18)]),sd(parmdis[,(i+18)]))[1]
    }
    for(i in 1:9){
      sPv[[i]] <- beta.mom(mean(parmdis[,(i+18)]),sd(parmdis[,(i+18)]))[2]
    }

  # Pseudopriors on betas
    # First: Transforms posteriors to the probability scale
    # Then: Derives parameters of the beta distribution describing the posterior
    # estimates on the probability scale.
    # Next: stores estimates of parameters for that beta in a list.
    # Finally: creates individual vectors that hold each of the list elements
    # so that the data can be used in the bugs.data function (which will not
    # allow for recursive indexing from a list for variable definition).
    name <- vector('list', 12)
    for(i in 1:12){
      names(name)[i] <- (paste(names(parmdis)[i+89], ".i", sep=''))
      name[[i]] <- c(beta.mom(mean(plogis(parmdis[,(i+89)])),
       sd(plogis(parmdis[,(i+89)]))))
    }
    for(i in 1:length(name)){
      assign(paste(names(name)[i], sep=''), name[[i]]) # Create vector for bugs
    }

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Set bugs directory for WinBUGS
    bugs.dir = 'C:/WinBUGS14/'

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

# DATA DEFINITIONS FOR MODEL CODE
  # Define number of occasions in CH
    n.occasions <- ncol(CH)

  # Define number of states, including dead
    n.states <- 4

  # Define number of observable states (one for unobserved)
    n.obs <- 4

  # Define number of individuals
    nind <- nrow(CH)

  # Define groups based on year
    #group <- as.numeric(as.factor(covs$Year))

  # Define number of groups
    #n.groups <- length(unique(group))

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

    # Accumulated thermal units
      ATU <- scale(covs$ATU)[,1]
      ATUS <- c((scale(covs$ATU))[,1]^2)

    # Accumulated thermal units
      Ph <- scale(covs$Ph)[,1]
      PhS <- c((scale(covs$Ph))[,1]^2)

    # Accumulated thermal units
      T <- scale(covs$T)[,1]
      TS <- c((scale(covs$T))[,1]^2)

    # Rearing history
      Origin <- as.numeric(factor(covs$Origin,
        levels=c("Wild", "Hatchery")))-1

# RUN THE MODEL
# Set a new working directory to separate the files for different models
  if(!file.exists('C:/Tag/Inclusion')) dir.create('C:/Tag/Inclusion')
  setwd('C:/Tag/Inclusion')

# Specify the model in JAGS language
  sink("ms.god")
  cat("
    model{

      # Years
        # 1 2005
        # 2 2006
        # 3 2009
        # 4 2010
        # 5 2011
        # 6 2012
        # 7 2013
        # 8 2014

      # Tags
        # 0 V7
        # 1 V9

      # Origin
        # 1 Wild
        # 2 Hatchery

      # Parameters:
        # S.Mainstem
        # S.Piscataquis
        # S.Stillwater
        # psiM
        # psiP
        # psiS
        # p.Mainstem
        # p.Pisquatis
        # p.Stillwater

      # States:
        # 1 Alive in Mainstem
        # 2 Alive in Piscataquis
        # 3 Alive in Stillwater
        # 4 Dead

      # Observations:
        # 1 Detected in Mainstem
        # 2 Detected in Piscataquis
        # 3 Detected in Stillwater
        # 4 Not Detected

      # PRIORS & CONSTRAINTS ###################################################
        # COVARIATE EFFECTS ####################################################
          # State-transition covariates
            # Prior on fixed effect of flow for SW transition and data transform
              betaFm <- log(beta.Fm / (1 - beta.Fm)) # Logit scale
              beta.Fm ~ dbeta(betaFm.i[1], betaFm.i[2]) # Pseudoprior
              betaFmi ~ dbern(.5) # Prior for inclusion

            # Effect of origin on Stillwater Branch usage and data transform
              betaOm <- log(beta.Om / (1 - beta.Om)) # Logit scale
              beta.Om ~ dbeta(betaOm.i[1], betaOm.i[2]) # Pseudoprior
              betaOmi ~ dbern(.5) # Prior for inclusion

          # Detection covariates
            # Prior on fixed effect of tag type and data transform
              betaT <- log(beta.T / (1 - beta.T)) # Logit scale
              beta.T ~ dbeta(betaT.i[1], betaT.i[2]) # Pseudoprior
              betaTi ~ dbern(.5) # Prior for inclusion

            # Prior on fixed effect of flow for detection and data transform
              betaFp <- log(beta.Fp / (1 - beta.Fp)) # Logit scale
              beta.Fp ~ dbeta(betaFp.i[1], betaFp.i[2]) # Pseudoprior
              betaFpi ~ dbern(.5) # Prior for inclusion

          # Survival covariates
            # Fixed effect of discharge on survival
              # Linear effect of discharge on survival and data transform
                betaFs <- log(beta.Fs / (1 - beta.Fs)) # Logit scale
                beta.Fs ~ dbeta(betaFs.i[1],betaFs.i[2]) # Pseudoprior
                betaFsi ~ dbern(.5) # Prior for inclusion

              # Second-order term for discharge and data transform
                betaFs2 <- log(beta.Fs2 / (1 - beta.Fs2)) # Logit scale
                beta.Fs2 ~ dbeta(betaFs2.i[1],betaFs2.i[2]) # Pseudoprior
                betaFs2i ~ dbern(.5) # Prior for inclusion

            # Fixed effect of ATU
              # Linear effect of ATU on survival and data transform
                betaAs <- log(beta.As / (1 - beta.As)) # Logit scale
                beta.As ~ dbeta(betaAs.i[1],betaAs.i[2]) # Pseudoprior
                betaAsi ~ dbern(.5) # Prior for inclusion

              # Second-order term for ATU and data transform
                betaAs2 <- log(beta.As2 / (1 - beta.As2)) # Logit scale
                beta.As2 ~ dbeta(betaAs2.i[1],betaAs2.i[2]) # Pseudoprior
                betaAs2i ~ dbern(.5) # Prior for inclusion

            # Fixed effect of photoperiod
              # Linear effect of photoperiod on survival and data transform
                betaPs <- log(beta.Ps / (1 - beta.Ps)) # Logit scale
                beta.Ps ~ dbeta(betaPs.i[1],betaPs.i[2]) # Pseudoprior
                betaPsi ~ dbern(.5) # Prior for inclusion

              # Second-order term for photoperiod and data transform
                betaPs2 <- log(beta.Ps2 / (1 - beta.Ps2)) # Logit scale
                beta.Ps2 ~ dbeta(betaPs2.i[1],betaPs2.i[2]) # Pseudoprior
                betaPs2i ~ dbern(.5) # Prior for inclusion

            # Fixed effect of temperature
              # Linear effect of temperature on survival and data transform
                betaTs <- log(beta.Ts / (1 - beta.Ts)) # Logit scale
                beta.Ts ~ dbeta(betaTs.i[1],betaTs.i[2]) # Pseudoprior
                betaTsi ~ dbern(.5) # Prior for inclusion

              # Second-order term for temperature and data transform
                betaTs2 <- log(beta.Ts2 / (1 - beta.Ts2)) # Logit scale
                beta.Ts2 ~ dbeta(betaTs2.i[1], betaTs2.i[2]) # Pseudoprior
                betaTs2i ~ dbern(.5) # Prior for inclusion

        # MAINSTEM PENOBSCOT RIVER SURVIVAL & DETECTION ########################
          # Survival and detection modeled as functions of covariates
            for(i in 1:nind){ # For each individual
              for(t in 1:(n.occasions-1)){ # In each reach
                # Define logit-scale detection probability WRT covariates
                  logit(p.Mainstem[i,t]) <- (pMainstem[t] + betaTi*betaT*Tag[i] +
                    betaFpi*betaFp*Discharge[i])
                # Define logit-scale survival probability WRT covariates
                  logit(SMain[i,t]) <- (SMainstem[t] + betaFsi*betaFs*Discharge[i] +
                    betaFs2i*betaFs2*DischargeS[i] + betaAsi*betaAs*ATU[i] + betaAs2i*betaAs2*ATUS[i] +
                    betaPsi*betaPs*Ph[i] + betaPs2i*betaPs2*PhS[i] + betaTsi*betaTs*T[i] + betaTs2i*betaTs2*TS[i])
              } # t
            } # i

          # Data transformation for model output (parameters monitored)
            for(t in 1:(n.occasions-1)){ # In each reach
              # Survival probabilities standardized by reach length in state 1
                S.Mainstem[t]<-pow(Smain[t], 1/dMain[t])
              # Derived quantities for detection probability
                V7pMain[t] <- exp(pMainstem[t])/(1 + exp(pMainstem[t]))
                V9pMain[t] <- exp(pMainstem[t] + betaT)/
                  (1 + exp(pMainstem[t] + betaT))
            } # t

          # Prior distributions for mainstem detection and survival
            # Logit-scale priors for survival and detection
              for(t in 1:(n.occasions-1)){ # In each reach
                SMainstem[t] <- log(Smain[t]/(1-Smain[t])) # Survival intercept
                pMainstem[t] <- log(pMain[t]/(1-pMain[t])) # Detection intercept
              }
            # Naive prior distributions for detection and survival probabilities
            # on real probability scale
              for(t in 1:(n.occasions-1)){ # In each reach
                # Detection probability
                  pMain[t] ~ dunif(0,1)
                # Survival probability
                  Smain[t] ~ dunif(0,1)
              } # t

        # PISCATAQUIS RIVER SURVIVAL & DETECTION ###############################
          # Detection and survival modeled as functions of covariates
            for(i in 1:nind){ # For each individual
              for(t in 1:(CONF-1)){ # In each reach
                # Define logit-scale detection probability WRT covariates
                  logit(p.Piscataquis[i,t]) <- (pPiscataquis[t] + betaTi*betaT*Tag[i] +
                    betaFpi*betaFp*Discharge[i])
              } # t
              for(t in 1:(CONF)){ # In each reach
                # Define logit-scale survival probability WRT covariates
                  logit(SPisc[i,t]) <- (SPiscataquis[t] + betaFsi*betaFs*Discharge[i] +
                    betaFs2i*betaFs2*DischargeS[i] + betaAsi*betaAs*ATU[i] + betaAs2i*betaAs2*ATUS[i] +
                    betaPsi*betaPs*Ph[i] + betaPs2i*betaPs2*PhS[i] + betaTs*T[i] + betaTs2*TS[i])
              } # t

          # Detection probability downstream from the confluence of the
            # Piscataquis and Penobscot Rivers is constrained to be one.
              for(t in (CONF):(n.occasions-1)){ # In each reach
                p.Piscataquis[i,t] <- 1 # Detection constraint
              } # t
              for(t in (CONF+1):(n.occasions-1)){ # In each reach
                SPisc[i,t] <- 1 # Survival constraint
              } # t
            } # i

          # Data transformation for model output
            # Survival standardized by reach length in the Piscataquis River
              for(t in 1:(CONF)){ # In each reach
                S.Piscataquis[t] <- pow(Spisc[t], 1/dPisc[t])
              } # t
            # Derived quantities for detection probability
              for(t in 1:(CONF-1)){ # In each reach
                V7pPisc[t] <- exp(pPiscataquis[t])/(1 + exp(pPiscataquis[t]))
                V9pPisc[t] <- exp(pPiscataquis[t] + betaT)/
                  (1 + exp(pPiscataquis[t] + betaT))
              } # t

          # Prior distributions for survival and detection probability in
          # the Piscataquis River upstream of the confluence
            # Logit-scale transformation for detection probability in the
              # Piscataquis River
                for(t in 1:(CONF-1)){ # In each reach
                  pPiscataquis[t] <- log(pPisc[t] / (1 - pPisc[t]))
                } # t
                for(t in 1:(CONF)){ # In each reach
                  SPiscataquis[t] <- log(Spisc[t] / (1 - Spisc[t]))
                } # t
              # Probability-scale priors for detection and survival
                # Detection probability in the Piscataquis above confluence
                  for(t in 1:(CONF-1)){ # In each reach
                    pPisc[t] ~ dunif(0,1)
                  } # t
                # Survival in the Piscataquis River state above confluence
                  for(t in 1:(CONF)){ # In each reach
                    Spisc[t] ~ dunif(0,1)
                  } # t

          # STILLWATER BRANCH SURVIVAL AND DETECTION ###########################
            # Detection and survival probability in Stillwater Branch
              for(i in 1:nind){ # For each individual
                # Detection and survival probabilities in the Stillwater Branch
                # upstream of where it starts are constrained to be one
                  for(t in 1:STILL){ # In each reach
                    p.Stillwater[i,t] <- 1
                    SStill[i,t] <- 1
                  } # t
                # Detection and survival probabilities in the Stillwater Branch
                # modeled as functions of covariates
                  for(t in (STILL+1):(CONFB)){ # In each reach
                    # Define logit-scale detection probability WRT covariates
                      logit(p.Stillwater[i,t]) <- (pStillwater[t] +
                        betaTi*betaT*Tag[i] + betaFpi*betaFp*Discharge[i])
                    # Define logit-scale survival probability WRT covariates
                      logit(SStill[i,t]) <- (SStillwater[t] +
                        betaFsi*betaFs*Discharge[i] + betaFs2i*betaFs2*DischargeS[i] +
                        betaAsi*betaAs*ATU[i] + betaAs2i*betaAs2*ATUS[i] +
                        betaPsi*betaPs*Ph[i] + betaPs2i*betaPs2*PhS[i] +
                        betaTsi*betaTs*T[i] + betaTs2i*betaTs2*TS[i])
                  } # t
                # Detection and survival downstream of the confluence of the
                # Stillwater Branch and the Penobscot River
                  for(t in (CONFB+1):(n.occasions-1)){ # In each reach
                    p.Stillwater[i,t] <- 1
                    SStill[i,t] <- 1
                  } # t
              } # i

            # Data transformation for model outputs
              for(t in (STILL+1):CONFB){ # In each reach
                # Survival in the Stillwater Branch standardized by reach
                # lengths
                  S.Stillwater[t] <- pow(Sstill[t],1/dstill[t])
                # Derived quantities for detection probability
                  V7pStill[t] <- exp(pStillwater[t])/(1 + exp(pStillwater[t]))
                  V9pStill[t] <- exp(pStillwater[t] + betaT)/
                    (1 + exp(pStillwater[t] + betaT))
                }

            # Prior distributions and parameter for detection and survival
            # probabilities in the Stillwater Branch
              # Logit-scale survival and detection probability
                for(t in (STILL+1):CONFB){ # In each reach
                  pStillwater[t] <- log(pStill[t] / (1 - pStill[t]))
                  SStillwater[t] <- log(Sstill[t] / (1 - Sstill[t]))
                }
              # Probablity-scale priors for survival and detection probability
              # in the Stillwater Branch
                for(t in (STILL+1):CONFB){ # In each reach
                  # Detection probabilities in the Stillwater Branch
                    pStill[t] ~ dunif(0,1)
                  # Survival probabilities through reaches in the Stillwater
                    Sstill[t] ~ dunif(0,1)
                } # t

        # STATE-TRANSITION PROBABILITIES #######################################
            # Mainstem transitions #############################################
              for(i in 1:nind){
                # Mainstem to mainstem constraint:
                # Always one except at head of Stillwater
                  for(t in 1:(n.occasions-1)){
                    PsiM1[i,t] <- 1-PsiM2[i,t]-PsiM3[i,t]
                  }

                # Mainstem to Piscataquis constraint:
                # Fish can't move from the mainstem into the Piscataquis
                  for(t in 1:(n.occasions-1)){
                    PsiM2[i,t] <- 0
                  }

                # Mainstem to Stillwater constraint:
                  # Fish can't move into Stillwater before the fork
                    for(t in 1:(STILL-1)){ # In each reach
                      PsiM3[i,t] <- 0
                    } # t

                # Covariate effects on transition into the Stillwater Branch
                # from the main-stem Penobscot River
                  logit(PsiM3[i, STILL]) <- (psi.M3[STILL]  +
                    betaFmi*betaFm*Discharge[i] + betaOmi*betaOm*Origin[i])

                # Mainstem to Stillwater constraint:
                # Fish can't move into the Stillwater Branch downstream of it!
                  for(t in (STILL+1):(n.occasions-1)){ # In each reach
                    PsiM3[i,t] <- 0
                  } # t
                }

                # Prior distribution and data transform for probability of
                # moving into the Stillwater Branch from the main-stem
                  psi.M3[STILL] <- log(psiM3[STILL]/(1-psiM3[STILL]))
                  psiM3[STILL] ~ dbeta(psi.still[1], psi.still[2])

            # Piscataquis transitions ##########################################
              # Above the confluence of Piscataquis and Penosbcot Rivers
                for(t in 1:(CONF-1)){ # In each reach
                  psiP[1,t] <- 0
                  psiP[2,t] <- 1
                } # t

              # Fish must move into Penobscot from Piscataquis at the confluence
                psiP[1,CONF] <- 1
                psiP[2,CONF] <- 0

              # Fish can't get back into the 'squis after they leave
                for(t in (CONF+1):(n.occasions-1)){ # In each reach
                  psiP[1,t] <-0
                  psiP[2,t] <-1
                } # t

            # Stillwater transitions ###########################################
              # Fish can't move out of the Stillwater until the conlfluence of
              # the Stillwater Branch and the Mainstem Penobscot River
                for(t in 1:(CONFB-1)){
                  psiS[1,t] <- 0
                } # t

              # Fish must move out of the Stillwater at the confluence with the
              # Penobscot River
                psiS[1,CONFB] <- 1

              # Fish can't move into the Penobscot in any time other than at
              # the confluence of the Stillwater Branch and Penobscot River
                for(t in (CONFB+1):(n.occasions-1)){ # In each reach
                  psiS[1,t] <- 0
                } # t

              # Fish can never move into the Piscataquis River from the
              # Stillwater Branch
                for(t in 1:(n.occasions-1)){
                  psiS[2,t] <- 0
                } # t

            # Final stat-transition for each ###################################
              for(t in 1:(n.occasions-1)){ # In each reach
                # Calculate last transition probability
                # Note that the three types of transition probs for Mainstem are
                # coded above.
                  psiP[3,t] <- 1-psiP[1,t]-psiP[2,t] # Piscataquis to Stillwater
                  psiS[3,t] <- 1-psiS[1,t]-psiS[2,t] # Stillwater to Stillwater
              } # t

        # Four-dimensional state-transition and detection probability matrices
        # conditional on individual i being in state h at time t
          for(i in 1:nind){ # For each individual
            for(t in f[i]:n.occasions-1){ # In each reach after initial capture
              # Define probability of state h(t+1) given h(t)
                ps[1,i,t,1] <- SMain[i,t]*PsiM1[i,t]
                ps[1,i,t,2] <- SMain[i,t]*PsiM2[i,t]
                ps[1,i,t,3] <- SMain[i,t]*PsiM3[i,t]
                ps[1,i,t,4] <- 1-SMain[i,t]
                ps[2,i,t,1] <- SPisc[i,t]*psiP[1,t]
                ps[2,i,t,2] <- SPisc[i,t]*psiP[2,t]
                ps[2,i,t,3] <- SPisc[i,t]* psiP[3,t]
                ps[2,i,t,4] <- 1-SPisc[i,t]
                ps[3,i,t,1] <- SStill[i,t]*psiS[1,t]
                ps[3,i,t,2] <- SStill[i,t]*psiS[2,t]
                ps[3,i,t,3] <- SStill[i,t]*psiS[3,t]
                ps[3,i,t,4] <- 1-SStill[i,t]
                ps[4,i,t,1] <- 0
                ps[4,i,t,2] <- 0
                ps[4,i,t,3] <- 0
                ps[4,i,t,4] <- 1
              # Define probability of detection given h(t)
                po[1,i,t,1] <- p.Mainstem[i,t]
                po[1,i,t,2] <- 0
                po[1,i,t,3] <- 0
                po[1,i,t,4] <- 1-p.Mainstem[i,t]
                po[2,i,t,1] <- 0
                po[2,i,t,2] <- p.Piscataquis[i,t]
                po[2,i,t,3] <- 0
                po[2,i,t,4] <- 1-p.Piscataquis[i,t]
                po[3,i,t,1] <- 0
                po[3,i,t,2] <- 0
                po[3,i,t,3] <- p.Stillwater[i,t]
                po[3,i,t,4] <- 1-p.Stillwater[i,t]
                po[4,i,t,1] <- 0
                po[4,i,t,2] <- 0
                po[4,i,t,3] <- 0
                po[4,i,t,4] <- 1
            } # t
          } # i

      # Likelihood
        for(i in 1:nind){ # For each individual
          # Define latent state at first capture
            z[i,f[i]] <- y[i,f[i]]
            for (t in (f[i]+1):n.occasions){ # In each reach after initial
              # State process: draw S(t) given S(t-1)
                z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
              # Observation process: draw O(t) given S(t)
                y[i,t] ~ dcat(po[z[i,t],i,t-1,])
            } #t
        } #i
    }
  ",fill = TRUE)
  sink()
# End model specification

# Function to create known latent states z
  known.state.ms <- function(ms, notseen){
   # notseen: label for not seen
   state <- ms
   state[state==notseen] <- NA
   for (i in 1:dim(ms)[1]){
      m <- min(which(!is.na(state[i,])))
      state[i,m] <- NA
      }
   return(state)
   }

# Function to create initial values for unknown z
  ms.init.z <- function(ch, f){
    for (i in 1:dim(ch)[1]){
      ch[i,1:f[i]] <- NA
    }
    states <- max(ch, na.rm = TRUE)
    known.states <- 1:(states-1)
    v <- which(ch==states)
    ch[-v] <- NA
    ch[v] <- sample(known.states, length(v), replace = TRUE)
    return(ch)
  }
  #head(ms.init.z(CH,f), 10)
  #head(CH,10)

# Bundle data
  bugs.data <- list(y = CH, f = f, n.occasions = dim(CH)[2],
    nind = dim(CH)[1], z = known.state.ms(CH, 4), CONF=9, STILL=12, CONFB=15,
  	dMain=dMain, dPisc=dPisc, dstill=dstill, Tag=Tag, Discharge=Discharge,
    DischargeS=DischargeS, ATU=ATU, ATUS=ATUS, Ph=Ph, PhS=PhS, T=T, TS=TS,
    psi.still=psi.still, #sMu=sMu, sMv=sMv, sPu=sPu, sPv=sPv,
    betaFp.i=betaFp.i,
    betaT.i=betaT.i,
    betaFs.i=betaFs.i,
    betaFs2.i=betaFs2.i,
    betaAs.i=betaAs.i,
    betaAs2.i=betaAs2.i,
    betaPs.i=betaPs.i,
    betaPs2.i=betaPs2.i,
    betaTs.i=betaTs.i,
    betaTs2.i=betaTs2.i,
    betaFm.i=betaFm.i,
    betaOm.i=betaOm.i,
    Origin=Origin)#,
    #group=as.numeric(as.factor(covs$Year)), n.groups=length(unique(group)))

# Initial values
  inits <- function(){
    list(
      # Survival and detection probabilities
        Smain=runif((n.occasions-1),0,1),   # Mainstem survival
        Spisc=c(runif(9,0,1)),              # Piscataquis survival
        Sstill=c(rep(NA,12), runif(3,0,1)), # Stillwater survival
        pMain=runif((n.occasions-1),0,1),   # Mainstem detection
        pPisc=c(runif(8,0,1)),              # Piscataquis detection
        pStill=c(rep(NA,12), runif(3,0,1)), # Stillwater detection

      # State-transition probabilities
        psiM3=c(rep(NA,11), runif(1,0,1)),  # Mainstem to Stillwater

      # Covariate effects
        beta.Fp=c(runif(1,0,1)),  # Effect of flow on detection
        beta.T=c(runif(1,0,1)),   # Effect of Tag model on detection
        beta.Fs=c(runif(1,0,1)),  # Effect of flow on survival, linear
        beta.Fs2=c(runif(1,0,1)), # Effect of flow on survival, quadratic
        beta.As=c(runif(1,0,1)),  # Effect of ATU on survival, linear
        beta.As2=c(runif(1,0,1)), # Effect of ATU on survival, quadratic
        beta.Ps=c(runif(1,0,1)),  # Effect of photoperiod on survival linear
        beta.Ps2=c(runif(1,0,1)), # Effect of photoperiod on survival, quadratic
        beta.Ts=c(runif(1,0,1)),  # Effect of temperature on survival, linear
        beta.Ts2=c(runif(1,0,1)), # Effect of temp on survival, quadratic
        beta.Fm=c(runif(1,0,1)),  # Effect of flow on use of Stillwater
        beta.Om=c(runif(1,0,1)),  # Effect of origin on use of Stillwater
      # Parameter inclusion probabilities
        betaFpi=c(rbinom(1,1,.5)),  # Inclusion of discharge for detection
        betaTi=c(rbinom(1,1,.5)),   # Inclusion of tag type for detection
        betaFsi=c(rbinom(1,1,.5)),  # Inclusion of discharge2 on survival
        betaFs2i=c(rbinom(1,1,.5)), # Inclusion of ATU on survival
        betaAsi=c(rbinom(1,1,.5)),  # Inclusion of ATU on survival
        betaAs2i=c(rbinom(1,1,.5)), # Inclusion of ATU2 on survival
        betaPsi=c(rbinom(1,1,.5)),  # Inclusion of Photoperiod on survival
        betaPs2i=c(rbinom(1,1,.5)), # Inclusion of Photoperiod2 on survival
        betaTsi=c(rbinom(1,1,.5)),  # Inclusion of Temperature on survival
        betaTs2i=c(rbinom(1,1,.5)), # Inclusion of Temperature2 on survival
        betaFmi=c(rbinom(1,1,.5)),  # Inclusion of Flow on mainstem
        betaOmi=c(rbinom(1,1,.5)),  # Inclusion of Origin on mainstem
      # Latent state matrix
        z=ms.init.z(CH, f)
    )
  }

# Parameters monitored
  parameters <- c(#"S.Mainstem", "S.Piscataquis", "S.Stillwater",
    #"V7pMain", #"V9pMain", "V7pPisc", "V9pPisc", "V7pStill", "V9pStill",
    #"psiM3",
    #"betaT", "betaFp", "betaFs", "betaFs2", "betaAs", "betaAs2", "betaPs",
    #"betaPs2", "betaTs", "betaTs2", #"betaFm", "betaOm",
    "betaTi", "betaFpi", "betaFsi", "betaFs2i", "betaAsi", "betaAs2i",
    "betaPsi", "betaPs2i", "betaTsi", "betaTs2i", "betaFmi", "betaOmi"
    )

# MCMC settings
  ni <- 33000
  nt <- 3
  nb <- 3000
  nc <- 3

# Call BUGS from R and run the model
  msGOD.PIP <- bugs(bugs.data, inits, parameters, "ms.god", n.chains = nc,
    n.thin = nt, n.iter = ni, n.burnin = nb, bugs.directory=bugs.dir,
    working.directory=getwd(), debug=T)

# Print model results
  print(msGOD.PIP$summary, digits=3)
################################################################################

################################################################################
# Run annual survival models to assess changes in survival between years, while
# accounting for detection heterogeneity due to tag model and discharge
#
# INPUT FILENAME(S): 'GodCapturesReady.csv', 'GodlyCovariates.csv',
# MY FUNCTIONS:       Inits, ms.init.z, known.state.ms,
# NAMED OUPUT:       'GD', 'CH', 'ms.god', 'msGOD.Y', GodYearRes
#                    'parmsYear' (df of posterior estimates)
# OUTPUT FILENAME:   'GodModelAnnualSurvivalEstimates.rda'
################################################################################
# Install and load necessary packages
  #install.packages('rjags')
  #install.packages('R2jags')
  #install.packages('R2WinBUGS')
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(reshape2)

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Set bugs directory for WinBUGS
    bugs.dir = 'C:/WinBUGS14/'

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]


  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

# DATA DEFINITIONS FOR MODEL CODE
  # Define number of occasions in CH
    n.occasions <- ncol(CH)

  # Define number of states, including dead
    n.states <- 4

  # Define number of observable states (one for unobserved)
    n.obs <- 4

  # Define number of individuals
    nind <- nrow(CH)

  # Define groups based on year
    group <- as.numeric(as.factor(covs$Year))

  # Define number of groups
    n.groups <- length(unique(group))

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

    # Accumulated thermal units
      ATU <- scale(covs$ATU)[,1]
      ATUS <- c((scale(covs$ATU))[,1]^2)

    # Accumulated thermal units
      Ph <- scale(covs$Ph)[,1]
      PhS <- c((scale(covs$Ph))[,1]^2)

    # Accumulated thermal units
      T <- scale(covs$T)[,1]
      TS <- c((scale(covs$T))[,1]^2)

    # Rearing history
      Origin <- as.numeric(factor(covs$Origin,
        levels=c("Wild", "Hatchery")))-1

# RUN THE MODEL
# Set a new working directory to separate the files for different models
  if(!file.exists('C:/Year')) dir.create('C:/Year')
  setwd('C:/Year')

# Specify the model in JAGS language
  sink("ms.god")
  cat("
    model{

      # GROUPS
        # 1 2005
        # 2 2006
        # 3 2009
        # 4 2010
        # 5 2011
        # 6 2012
        # 7 2013
        # 8 2014

      # Parameters:
        # S.Mainstem[t]
        # S.Piscataquis[t]
        # S.Stillwater[t]
        # psi.PiscToMain[t]
        # psi.PiscToStill[t]
        # psi.MainToPisc[t]
        # psi.MainToStill[t]
        # psi.StillToPisc[t]
        # psi.StillToMain[t]
        # p.Mainstem[t]
        # p.Pisquatis[t]
        # p.Stillwater[t]

      # States:
        # 1 Alive in Mainstem
        # 2 Alive in Piscataquis
        # 3 Alive in Stillwater
        # 4 Alive in Dead

      # Observations:
        # 1 Seen alive
        # 2 Seen dead
        # 3 Not seen

      # PRIORS & CONSTRAINTS####################################################
          # Detection covariates
            # Prior on fixed effect of tag type and data transform
              betaT <- log(beta.T / (1 - beta.T))
              beta.T ~ dunif(0,1)

            # Prior on fixed effect of flow for detection and data transform
              betaFp <- log(beta.Fp / (1 - beta.Fp))
              beta.Fp ~ dunif(0,1)


        # MAINSTEM PENOBSCOT RIVER SURVIVAL & DETECTION ########################
          # Group-level estimation of survival and detection probabilities
          # that will be monitored.  S.mainstem[i,t] is standardized by distance
            for(i in 1:n.groups){
              for(t in 1:(n.occasions-1)){
                # Survival probabilities
                  S.Mainstem[i,t] <- pow(Smain[i,t],1/dMain[t])
              } # t
            } # i

          # Covariate effects on survival and detection
          # Individual-random effect of group on survival
            for(i in 1:nind){
              for(t in 1:(n.occasions-1)){
                # Survival probabilities
                  SMainstem[i,t] <- Smain[group[i],t]
                # Detection probabilities
                  logit(p.Mainstem[i,t]) <- pMainstem[t]  + betaT*Tag[i] + betaFp*Discharge[i]
              } # t
            } # i

            # Detection probability on logit scale
              for(t in 1:(n.occasions-1)){
                pMainstem[t] <- log(pMain[t]/(1-pMain[t]))
              }

          # Prior distributions for survival and detection probs
            # Group-specific survival
              for(g in 1:n.groups){
                for(t in 1:(n.occasions-1)){
                  Smain[g,t] ~ dunif(0,1)
                } # t
              } # g
            # Detection
              for(t in 1:(n.occasions-1)){
                pMain[t] ~ dunif(0,1)
               }

        # PISCATAQUIS RIVER SURVIVAL & DETECTION ###############################
          # PISC params above confluence
            # Parameters being monitored
              for(i in 1:n.groups){
                for(t in 1:(CONF)){
                  # Group-level survival probabilities standardized by distance
                    S.Piscataquis[i,t] <- pow(Spisc[i,t],1/dPisc[t])
                }
              }

            # Covariate effects
              for(i in 1:nind){
                for(t in 1:(n.occasions-1)){
                # Group-specific survival
                  SPiscataquis[i,t] <- Spisc[group[i],t]
                } # t
                # Detection probability with covariates
                  for(t in 1:(CONF-1)){
                    logit(p.Piscataquis[i,t]) <- pPiscataquis[t] + betaT*Tag[i] + betaFp*Discharge[i]
                  } # t
                  for(t in CONF:(n.occasions-1)){
                    p.Piscataquis[i,t] <- 1
                  } # t
                } # i

              # Detection probabilities on logit scale
                for(t in 1:(CONF-1)){
                  pPiscataquis[t] <- log(pPisc[t]/(1-pPisc[t]))
                }

            # Priors and constants for survival and detection in Piscataquis
              # Prior on detection probability
                for(t in 1:CONF-1){
                  pPisc[t] ~ dunif(0,1)
                }
                for(t in CONF:(n.occasions-1)){
                  pPisc[t] <- 1
                }
              # Prior on survival probability
                for(g in 1:n.groups){
                  for(t in 1:CONF){
                    Spisc[g,t] ~ dunif(0,1)
                  }
                  for(t in (CONF+1):(n.occasions-1)){
                    Spisc[g,t] <- 1
                  }
                }

          # STILLWATER BRANCH SURVIVAL AND DETECTION ###########################
            # Parameters being monitored
              # Survival
                for(i in 1:n.groups){
                  for(t in (STILL+1):(CONFB)){
                    S.Stillwater[i,t] <- pow(Sstill[i,t],1/dstill[t])
                  }
                }

              # Detection
                for(i in 1:nind){
                  for(t in 1:(n.occasions-1)){
                   logit(p.Stillwater[i,t]) <- pStillwater[t] + betaT*Tag[i] + betaFp*Discharge[i]
                  } # t
                } # i

            # Logit-transformation of detection probability
              for(t in 1:(n.occasions-1)){
                pStillwater[t] <- log(pStill[t]/(1-pStill[t]))
              } # t

            # Survival probabilities for likelihood
              for(i in 1:nind){
                for(t in 1:(n.occasions-1)){
                  SStillwater[i,t] <- Sstill[group[i],t]
                } # t
              } # i

            # Priors for Stillwater survival and detection probabilities
              # Survival probabilities
                for(i in 1:n.groups){
                  for(t in 1:STILL){
                    Sstill[i,t] <- 1
                  }  # t
                  for(t in (STILL+1):(CONFB)){
                    Sstill[i,t] ~ dunif(0,1)
                  } # t
                  for(t in (CONFB+1):(n.occasions-1)){
                    Sstill[i,t] <- 1
                  } # t
                } # i

              # Detection probabilities
                for(t in 1:STILL){
                  pStill[t] <- 1
                } # t
                for(t in (STILL+1):(CONFB)){
                  pStill[t] ~ dunif(0,1)
                } # t
                for(t in (CONFB+1):(n.occasions-1)){
                  pStill[t] <- 1
                } # t

        # STATE-TRANSITION PROBABILITIES #######################################
            # Mainstem transitions #############################################
              for(t in 1:(STILL-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

              psiM[1,STILL] ~ dunif(0, 1)

              psiM[2,STILL] <- 0
              for(t in (STILL+1):(n.occasions-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

            # Piscataquis transitions ##########################################
              for(t in 1:(CONF-1)){
                  psiP[1,t] <- 0
                  psiP[2,t] <- 1
              }
              psiP[1,CONF] <- 1
              psiP[2,CONF] <- 0
              for(t in (CONF+1):(n.occasions-1)){
                  psiP[1,t] <-0
                  psiP[2,t] <-1
               }

            # Stillwater transitions ###########################################
              for(t in 1:(CONFB-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }
              psiS[1,CONFB] <- 1
              psiS[2,CONFB] <- 0
              for(t in (CONFB+1):(n.occasions-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }

            # Final stat-transition for each ###################################
              for(t in 1:(n.occasions-1)){
              # Calculate last transition probability
                psiM[3,t] <- 1-psiM[1,t]-psiM[2,t]
                psiP[3,t] <- 1-psiP[1,t]-psiP[2,t]
                psiS[3,t] <- 1-psiS[1,t]-psiS[2,t]
              } # t

        # Four-dimensional state-transition and detection probability matrices
        # conditional on individual i being in state h at time t
          for(i in 1:nind){
            for(t in f[i]:n.occasions-1){
              # Define probability of state h(t+1) given h(t)
                ps[1,i,t,1] <- SMainstem[i,t]*psiM[1,t]
                ps[1,i,t,2] <- SMainstem[i,t]*psiM[2,t]
                ps[1,i,t,3] <- SMainstem[i,t]*psiM[3,t]
                ps[1,i,t,4] <- 1-SMainstem[i,t]
                ps[2,i,t,1] <- SPiscataquis[i,t]*psiP[1,t]
                ps[2,i,t,2] <- SPiscataquis[i,t]*psiP[2,t]
                ps[2,i,t,3] <- SPiscataquis[i,t]* psiP[3,t]
                ps[2,i,t,4] <- 1-SPiscataquis[i,t]
                ps[3,i,t,1] <- SStillwater[i,t]*psiS[1,t]
                ps[3,i,t,2] <- SStillwater[i,t]*psiS[2,t]
                ps[3,i,t,3] <- SStillwater[i,t]*psiS[3,t]
                ps[3,i,t,4] <- 1-SStillwater[i,t]
                ps[4,i,t,1] <- 0
                ps[4,i,t,2] <- 0
                ps[4,i,t,3] <- 0
                ps[4,i,t,4] <- 1
              # Define probability of detection given h(t)
                po[1,i,t,1] <- p.Mainstem[i,t]
                po[1,i,t,2] <- 0
                po[1,i,t,3] <- 0
                po[1,i,t,4] <- 1-p.Mainstem[i,t]
                po[2,i,t,1] <- 0
                po[2,i,t,2] <- p.Piscataquis[i,t]
                po[2,i,t,3] <- 0
                po[2,i,t,4] <- 1-p.Piscataquis[i,t]
                po[3,i,t,1] <- 0
                po[3,i,t,2] <- 0
                po[3,i,t,3] <- p.Stillwater[i,t]
                po[3,i,t,4] <- 1-p.Stillwater[i,t]
                po[4,i,t,1] <- 0
                po[4,i,t,2] <- 0
                po[4,i,t,3] <- 0
                po[4,i,t,4] <- 1
            } # t
          } # i

      # Likelihood
        for(i in 1:nind){
          # Define latent state at first capture
            z[i,f[i]] <- y[i,f[i]]
            for (t in (f[i]+1):n.occasions){
              # State process: draw S(t) given S(t-1)
                z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
              # Observation process: draw O(t) given S(t)
                y[i,t] ~ dcat(po[z[i,t],i,t-1,])
            } #t
        } #i
    }
  ",fill = TRUE)
  sink()
# End model specification

# Function to create known latent states z
  known.state.ms <- function(ms, notseen){
   # notseen: label for not seen
   state <- ms
   state[state==notseen] <- NA
   for (i in 1:dim(ms)[1]){
      m <- min(which(!is.na(state[i,])))
      state[i,m] <- NA
      }
   return(state)
   }

# Function to create initial values for unknown z
  ms.init.z <- function(ch, f){
    for (i in 1:dim(ch)[1]){
      ch[i,1:f[i]] <- NA
    }
    states <- max(ch, na.rm = TRUE)
    known.states <- 1:(states-1)
    v <- which(ch==states)
    ch[-v] <- NA
    ch[v] <- sample(known.states, length(v), replace = TRUE)
    return(ch)
  }

# Bundle data
  bugs.data <- list(y = CH, f = f, n.occasions = dim(CH)[2],
    nind = dim(CH)[1], z = known.state.ms(CH, 4), CONF=9, STILL=12, CONFB=15,
  	dMain=dMain, dPisc=dPisc, dstill=dstill, Tag=Tag, Discharge=Discharge,
    group=as.numeric(as.factor(covs$Year)), n.groups=n.groups)

# Initial values
  inits <- function(){
    list(
      Smain=structure(runif(n.groups * (n.occasions-1), 0, 1),
        Dim=c(n.groups, (n.occasions-1))),
      Spisc=structure(c(runif(9, 0, 1), rep(NA, 9), runif(9, 0, 1), rep(NA, 9)),
        Dim=c(n.groups, (n.occasions-1))),
      Sstill=structure(c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3), rep(NA, 12),
        runif(3, 0, 1), rep(NA, 3)), Dim=c(2, (n.occasions-1))),

      #pPisc=c(runif(8, 0, 1), rep(NA, 9)),
      pMain=c(runif((n.occasions-1), 0, 1)),
      pStill=c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3)),
      z=ms.init.z(CH, f)
    )
  }

# Parameters monitored
  parameters <- c("S.Mainstem",  "S.Piscataquis", "Sstill")#, "p.Mainstem",
     #"p.Piscataquis","pStill", "psiM")

# MCMC settings
  ni <- 33000
  nt <- 3
  nb <- 3000
  nc <- 3

# Call BUGS from R and run the model
  msGOD.Y <- bugs(bugs.data, inits, parameters, "ms.god", n.chains = nc,
    n.thin = nt, n.iter = ni, n.burnin = nb, bugs.directory=bugs.dir,
    working.directory=getwd(), debug=T)

# Print the model results
  GodYearRes <- data.frame(msGOD.Y$summary)
  GodYearRes

# Write the annual survival estimates to an rdata file
  parmsYear <- data.frame(msGOD.Y$sims.list)
  head(parmsYear)
  save(parmsYear, file='GodModelAnnualSurvivalEstimates.rda')
################################################################################

################################################################################
# Run multi-annual survival models with random group effect of rearing history
# (hatchery or wild) to describe patterns in survival among rearing histories
#
# INPUT FILENAME(S): 'GodCapturesReady.csv', 'GodlyCovariates.csv',
# MY FUNCTIONS:       Inits, ms.init.z, known.state.ms,
# NAMED OUPUT:       'GD', 'CH', 'ms.god', 'msGOD.O', GodOrgRes
#                    'parmsOrg' (df of posterior estimates)
# OUTPUT FILENAME:   'GodModelOriginEstimates.rda'
################################################################################
# Install and load necessary packages
  #install.packages('rjags')
  #install.packages('R2jags')
  #install.packages('R2WinBUGS')
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(reshape2)

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Set bugs directory for WinBUGS
    bugs.dir = 'C:/WinBUGS14/'

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

	# Now only use years for which there were both hatchery and wild fish
	  GD <- GD[covs$Year==2005 | covs$Year==2006 | covs$Year==2010 | covs$Year==2011, ]
	  covs <- covs[covs$Year==2005 | covs$Year==2006 | covs$Year==2010 | covs$Year==2011, ]

  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

# DATA DEFINITIONS FOR MODEL CODE
  # Define number of occasions in CH
    n.occasions <- ncol(CH)

  # Define number of states, including dead
    n.states <- 4

  # Define number of observable states (one for unobserved)
    n.obs <- 4

  # Define number of individuals
    nind <- nrow(CH)

  # Define groups based on year
    group <- as.numeric(as.factor(covs$Origin))

  # Define number of groups
    n.groups <- length(unique(group))

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

    # Accumulated thermal units
      ATU <- scale(covs$ATU)[,1]
      ATUS <- c((scale(covs$ATU))[,1]^2)

    # Accumulated thermal units
      Ph <- scale(covs$Ph)[,1]
      PhS <- c((scale(covs$Ph))[,1]^2)

    # Accumulated thermal units
      T <- scale(covs$T)[,1]
      TS <- c((scale(covs$T))[,1]^2)

    # Rearing history
      #Origin <- as.numeric(factor(covs$Origin,
        #levels=c("Wild", "Hatchery")))-1

# RUN THE MODEL
# Set a new working directory to separate the files for different models
  if(!file.exists('C:/Origin')) dir.create('C:/Origin')
  setwd('C:/Origin')

# Specify the model in JAGS language
  sink("ms.god")
  cat("
    model{

      # GROUPS
        # 1 2005
        # 2 2006
        # 3 2009
        # 4 2010
        # 5 2011
        # 6 2012
        # 7 2013
        # 8 2014

      # Parameters:
        # S.Mainstem[t]
        # S.Piscataquis[t]
        # S.Stillwater[t]
        # psi.PiscToMain[t]
        # psi.PiscToStill[t]
        # psi.MainToPisc[t]
        # psi.MainToStill[t]
        # psi.StillToPisc[t]
        # psi.StillToMain[t]
        # p.Mainstem[t]
        # p.Pisquatis[t]
        # p.Stillwater[t]

      # States:
        # 1 Alive in Mainstem
        # 2 Alive in Piscataquis
        # 3 Alive in Stillwater
        # 4 Alive in Dead

      # Observations:
        # 1 Seen alive
        # 2 Seen dead
        # 3 Not seen

      # PRIORS & CONSTRAINTS####################################################
          # Detection covariates
            # Prior on fixed effect of tag type and data transform
              betaT <- log(beta.T / (1 - beta.T))
              beta.T ~ dunif(0,1)

            # Prior on fixed effect of flow for detection and data transform
              betaFp <- log(beta.Fp / (1 - beta.Fp))
              beta.Fp ~ dunif(0,1)


        # MAINSTEM PENOBSCOT RIVER SURVIVAL & DETECTION ########################
          # Group-level estimation of survival and detection probabilities
          # that will be monitored.  S.mainstem[i,t] is standardized by distance
            for(i in 1:n.groups){
              for(t in 1:(n.occasions-1)){
                # Survival probabilities
                  S.Mainstem[i,t] <- pow(Smain[i,t],1/dMain[t])
              } # t
            } # i

          # Covariate effects on survival and detection
          # Individual-random effect of group on survival
            for(i in 1:nind){
              for(t in 1:(n.occasions-1)){
                # Survival probabilities
                  SMainstem[i,t] <- Smain[group[i],t]
                # Detection probabilities
                  logit(p.Mainstem[i,t]) <- pMainstem[t]  + betaT*Tag[i] + betaFp*Discharge[i]
              } # t
            } # i

            # Detection probability on logit scale
              for(t in 1:(n.occasions-1)){
                pMainstem[t] <- log(pMain[t]/(1-pMain[t]))
              }

          # Prior distributions for survival and detection probs
            # Group-specific survival
              for(g in 1:n.groups){
                for(t in 1:(n.occasions-1)){
                  Smain[g,t] ~ dunif(0,1)
                } # t
              } # g
            # Detection
              for(t in 1:(n.occasions-1)){
                pMain[t] ~ dunif(0,1)
               }

        # PISCATAQUIS RIVER SURVIVAL & DETECTION ###############################
          # PISC params above confluence
            # Parameters being monitored
              for(i in 1:n.groups){
                for(t in 1:(CONF)){
                  # Group-level survival probabilities standardized by distance
                    S.Piscataquis[i,t] <- pow(Spisc[i,t],1/dPisc[t])
                }
              }

            # Covariate effects
              for(i in 1:nind){
                for(t in 1:(n.occasions-1)){
                # Group-specific survival
                  SPiscataquis[i,t] <- Spisc[group[i],t]
                } # t
                # Detection probability with covariates
                  for(t in 1:(CONF-1)){
                    logit(p.Piscataquis[i,t]) <- pPiscataquis[t] + betaT*Tag[i] + betaFp*Discharge[i]
                  } # t
                  for(t in CONF:(n.occasions-1)){
                    p.Piscataquis[i,t] <- 1
                  } # t
                } # i

              # Detection probabilities on logit scale
                for(t in 1:(CONF-1)){
                  pPiscataquis[t] <- log(pPisc[t]/(1-pPisc[t]))
                }

            # Priors and constants for survival and detection in Piscataquis
              # Prior on detection probability
                for(t in 1:CONF-1){
                  pPisc[t] ~ dunif(0,1)
                }
                for(t in CONF:(n.occasions-1)){
                  pPisc[t] <- 1
                }
              # Prior on survival probability
                for(g in 1:n.groups){
                  for(t in 1:CONF){
                    Spisc[g,t] ~ dunif(0,1)
                  }
                  for(t in (CONF+1):(n.occasions-1)){
                    Spisc[g,t] <- 1
                  }
                }

          # STILLWATER BRANCH SURVIVAL AND DETECTION ###########################
            # Parameters being monitored
              # Survival
                for(i in 1:n.groups){
                  for(t in (STILL+1):(CONFB)){
                    S.Stillwater[i,t] <- pow(Sstill[i,t],1/dstill[t])
                  }
                }

              # Detection
                for(i in 1:nind){
                  for(t in 1:(n.occasions-1)){
                   logit(p.Stillwater[i,t]) <- pStillwater[t] + betaT*Tag[i] + betaFp*Discharge[i]
                  } # t
                } # i

            # Logit-transformation of detection probability
              for(t in 1:(n.occasions-1)){
                pStillwater[t] <- log(pStill[t]/(1-pStill[t]))
              } # t

            # Survival probabilities for likelihood
              for(i in 1:nind){
                for(t in 1:(n.occasions-1)){
                  SStillwater[i,t] <- Sstill[group[i],t]
                } # t
              } # i

            # Priors for Stillwater survival and detection probabilities
              # Survival probabilities
                for(i in 1:n.groups){
                  for(t in 1:STILL){
                    Sstill[i,t] <- 1
                  }  # t
                  for(t in (STILL+1):(CONFB)){
                    Sstill[i,t] ~ dunif(0,1)
                  } # t
                  for(t in (CONFB+1):(n.occasions-1)){
                    Sstill[i,t] <- 1
                  } # t
                } # i

              # Detection probabilities
                for(t in 1:STILL){
                  pStill[t] <- 1
                } # t
                for(t in (STILL+1):(CONFB)){
                  pStill[t] ~ dunif(0,1)
                } # t
                for(t in (CONFB+1):(n.occasions-1)){
                  pStill[t] <- 1
                } # t

        # STATE-TRANSITION PROBABILITIES #######################################
            # Mainstem transitions #############################################
              for(t in 1:(STILL-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

              psiM[1,STILL] ~ dunif(0, 1)

              psiM[2,STILL] <- 0
              for(t in (STILL+1):(n.occasions-1)){
                psiM[1,t] <- 1
                psiM[2,t] <- 0
              } # t

            # Piscataquis transitions ##########################################
              for(t in 1:(CONF-1)){
                  psiP[1,t] <- 0
                  psiP[2,t] <- 1
              }
              psiP[1,CONF] <- 1
              psiP[2,CONF] <- 0
              for(t in (CONF+1):(n.occasions-1)){
                  psiP[1,t] <-0
                  psiP[2,t] <-1
               }

            # Stillwater transitions ###########################################
              for(t in 1:(CONFB-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }
              psiS[1,CONFB] <- 1
              psiS[2,CONFB] <- 0
              for(t in (CONFB+1):(n.occasions-1)){
                  psiS[1,t] <- 0
                  psiS[2,t] <- 0
              }

            # Final stat-transition for each ###################################
              for(t in 1:(n.occasions-1)){
              # Calculate last transition probability
                psiM[3,t] <- 1-psiM[1,t]-psiM[2,t]
                psiP[3,t] <- 1-psiP[1,t]-psiP[2,t]
                psiS[3,t] <- 1-psiS[1,t]-psiS[2,t]
              } # t

        # Four-dimensional state-transition and detection probability matrices
        # conditional on individual i being in state h at time t
          for(i in 1:nind){
            for(t in f[i]:n.occasions-1){
              # Define probability of state h(t+1) given h(t)
                ps[1,i,t,1] <- SMainstem[i,t]*psiM[1,t]
                ps[1,i,t,2] <- SMainstem[i,t]*psiM[2,t]
                ps[1,i,t,3] <- SMainstem[i,t]*psiM[3,t]
                ps[1,i,t,4] <- 1-SMainstem[i,t]
                ps[2,i,t,1] <- SPiscataquis[i,t]*psiP[1,t]
                ps[2,i,t,2] <- SPiscataquis[i,t]*psiP[2,t]
                ps[2,i,t,3] <- SPiscataquis[i,t]* psiP[3,t]
                ps[2,i,t,4] <- 1-SPiscataquis[i,t]
                ps[3,i,t,1] <- SStillwater[i,t]*psiS[1,t]
                ps[3,i,t,2] <- SStillwater[i,t]*psiS[2,t]
                ps[3,i,t,3] <- SStillwater[i,t]*psiS[3,t]
                ps[3,i,t,4] <- 1-SStillwater[i,t]
                ps[4,i,t,1] <- 0
                ps[4,i,t,2] <- 0
                ps[4,i,t,3] <- 0
                ps[4,i,t,4] <- 1
              # Define probability of detection given h(t)
                po[1,i,t,1] <- p.Mainstem[i,t]
                po[1,i,t,2] <- 0
                po[1,i,t,3] <- 0
                po[1,i,t,4] <- 1-p.Mainstem[i,t]
                po[2,i,t,1] <- 0
                po[2,i,t,2] <- p.Piscataquis[i,t]
                po[2,i,t,3] <- 0
                po[2,i,t,4] <- 1-p.Piscataquis[i,t]
                po[3,i,t,1] <- 0
                po[3,i,t,2] <- 0
                po[3,i,t,3] <- p.Stillwater[i,t]
                po[3,i,t,4] <- 1-p.Stillwater[i,t]
                po[4,i,t,1] <- 0
                po[4,i,t,2] <- 0
                po[4,i,t,3] <- 0
                po[4,i,t,4] <- 1
            } # t
          } # i

      # Likelihood
        for(i in 1:nind){
          # Define latent state at first capture
            z[i,f[i]] <- y[i,f[i]]
            for (t in (f[i]+1):n.occasions){
              # State process: draw S(t) given S(t-1)
                z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,])
              # Observation process: draw O(t) given S(t)
                y[i,t] ~ dcat(po[z[i,t],i,t-1,])
            } #t
        } #i
    }
  ",fill = TRUE)
  sink()
# End model specification

# Function to create known latent states z
  known.state.ms <- function(ms, notseen){
   # notseen: label for not seen
   state <- ms
   state[state==notseen] <- NA
   for (i in 1:dim(ms)[1]){
      m <- min(which(!is.na(state[i,])))
      state[i,m] <- NA
      }
   return(state)
   }

# Function to create initial values for unknown z
  ms.init.z <- function(ch, f){
    for (i in 1:dim(ch)[1]){
      ch[i,1:f[i]] <- NA
    }
    states <- max(ch, na.rm = TRUE)
    known.states <- 1:(states-1)
    v <- which(ch==states)
    ch[-v] <- NA
    ch[v] <- sample(known.states, length(v), replace = TRUE)
    return(ch)
  }

# Bundle data
  bugs.data <- list(y = CH, f = f, n.occasions = dim(CH)[2],
    nind = dim(CH)[1], z = known.state.ms(CH, 4), CONF=9, STILL=12, CONFB=15,
  	dMain=dMain, dPisc=dPisc, dstill=dstill, Tag=Tag, Discharge=Discharge,
    group=as.numeric(as.factor(covs$Origin)), n.groups=n.groups)

# Initial values
  inits <- function(){
    list(
      Smain=structure(runif(n.groups * (n.occasions-1), 0, 1),
        Dim=c(n.groups, (n.occasions-1))),
      Spisc=structure(c(runif(9, 0, 1), rep(NA, 9), runif(9, 0, 1), rep(NA, 9)),
        Dim=c(n.groups, (n.occasions-1))),
      Sstill=structure(c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3), rep(NA, 12),
        runif(3, 0, 1), rep(NA, 3)), Dim=c(2, (n.occasions-1))),
      #pPisc=c(runif(8, 0, 1), rep(NA, 9)),
      pMain=c(runif((n.occasions-1), 0, 1)),
      pStill=c(rep(NA, 12), runif(3, 0, 1), rep(NA, 3)),
      z=ms.init.z(CH, f)
    )
  }

# Parameters monitored
  parameters <- c("S.Mainstem",  "S.Piscataquis", "S.Stillwater")#, "p.Mainstem",
     #"p.Piscataquis","pStill", "psiM")

# MCMC settings
  ni <- 33000
  nt <- 3
  nb <- 3000
  nc <- 3

# Call BUGS from R and run the model
  msGOD.O <- bugs(bugs.data, inits, parameters, "ms.god", n.chains = nc,
    n.thin = nt, n.iter = ni, n.burnin = nb, bugs.directory=bugs.dir,
    working.directory=getwd(), debug=T)

# Print the model results
  GodOrgRes <- data.frame(msGOD.O$summary)
  GodOrgRes

# Write the annual survival estimates to an rdata file
  parmsOrg <- data.frame(msGOD.O$sims.list)
  head(parmsOrg)
  save(parmsOrg, file='GodModelOriginEstimates3.rda')
################################################################################

################################################################################
# Make figures for the freshwater survival models other than the figure of
# reach-type survival in the multiannual model
#
# INPUT FILENAME(S): 'GodModelOriginEstimates.rda' (parmsOrg df)
#             	     'GodModelAnnualSurvivalEstimates.rda' (parmsYear df)
#                    'pseudoPriorsForRoundTwo.r' (parmsdis df)
#                    'GodCapturesReady.csv', 'GodlyCovariates.csv',
#                    'GodModelOriginEstimates.rda' (parmsOrg df)
# MY FUNCTIONS:
# NAMED OUPUT:
#
# OUTPUT FILENAME:   'Figure5.tiff', 'Figure6.tiff', 'Figure7.tiff'
################################################################################
# Package install and loading
	#install.packages('tcltk2')
	#install.packages('reshape2')
	#isntall.packages('plotrix')
	require(plotrix)
	require(reshape2)
	require(tcltk2)

# FIGURE 5- Survival throughout the Penobscot by rearing history
# Read in the posterior estimates for hatchery- and wild-reared smolts across
# years in the Penobscot River catchment
  load('C:/Origin/GodModelOriginEstimates.rda') # It's called parmsOrg
# Plot the results
  # Create a file to which the figure can be written
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/Figure5.tiff',
  	  width=10000, height=8000, pointsize=22, compression="lzw", res=600)

  # Set up plotting window
    par(mfrow=c(2,1), oma=c(5,4.5,0,0), mar=c(2,1,1,1))

  # Piscataquis survival
    # Hatchery fish
      plotCI(x=seq(1, 9, 1), y=apply(parmsOrg[ , c(seq(37, 53, 2))], 2, mean),
       uiw=apply(parmsOrg[ , c(seq(37, 53, 2))], 2, sd),
       liw=apply(parmsOrg[ , c(seq(37, 53, 2))], 2, sd),
       ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.9, 1), pch=22,
        col='black', pt.bg='black', cex=1.25, lwd=1, cex.axis=3, xlim=c(1,17),
      	bty='n', axes=F)

    # Wild fish
      plotCI(x=(seq(1, 9, 1)+.25),
        y=apply(parmsOrg[ , c(seq(38, 54, 2))], 2, mean),
        uiw=apply(parmsOrg[ , c(seq(38, 54, 2))], 2, sd),
        liw=apply(parmsOrg[ , c(seq(38, 54, 2))], 2, sd),
        ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.9, 1), pch=22,
        col='black', pt.bg='gray', cex=1.25, lwd=1, add=TRUE, slty=2, cex.axis=3,
        xlim=c(1,17), bty='n')

	  # Dam reaches
		  segments(x0=1.75, y0=1.004, x1=1.75, y1=.88,col='gray87')
			segments(x0=2.5, y0=1.004, x1=2.5, y1=.88,col='gray87')
	    segments(x0=3.75, y0=1.004, x1=3.75, y1=.88,col='gray87')
	    segments(x0=5.5, y0=1.004, x1=5.5, y1=.88,col='gray87')
		  segments(x0=8.75, y0=1.004, x1=8.75, y1=.88,col='gray87')

    # Axes and labels
      #axis(side=1, at=seq(1, 17, 1), as.character(seq(1, 17, 1)))
      axis(side=2, at=seq(0.9, 1, 0.02), sprintf('%.2f', seq(0.9, 1, 0.02)),
        las=2)
      mtext(side=2, expression(paste('Apparent survival per km ( ',
        widehat(italic("S"))[italic(D[t])], ')', sep='')),
        line=3.5, cex=1.5, adj=3)
      segments(x0=0, y0=1.004, x1=9.75, y1=1.004,col='black')
      segments(x0=9.75, y0=1.004, x1=9.75, y1=.88,col='black')
      segments(x0=0.36, y0=1.004, x1=0.36, y1=.896,col='black')
      segments(x0=0.36, y0=.896, x1=9.75, y1=.896,col='black')
      text(x=1, y=.905, 'Piscataquis River (B)', adj=0, cex=1.1)

  # Stillwater survival
    # Hatchery fish
      plotCI(x=seq(13, 15, 1), y=apply(parmsOrg[ , c(seq(55, 59, 2))], 2, mean),
       uiw=apply(parmsOrg[ , c(seq(55, 59, 2))], 2, sd),
       liw=apply(parmsOrg[ , c(seq(55, 59, 2))], 2, sd),
       ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.90, 1.00), pch=23,
        col='black', pt.bg='black', cex=1.25, lwd=1, cex.axis=3, xlim=c(1,17),
      	add=TRUE)

    # Wild fish
      plotCI(x=(seq(13, 15, 1)+.25),
        y=apply(parmsOrg[ , c(seq(56, 60, 2))], 2, mean),
        uiw=apply(parmsOrg[ , c(seq(56, 60, 2))], 2, sd),
        liw=apply(parmsOrg[ , c(seq(56, 60, 2))], 2, sd),
        ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.9, 1), pch=23,
        col='black', pt.bg='gray', cex=1.25, lwd=1, add=TRUE, slty=2, cex.axis=3,
        xlim=c(1,17))

	  # Dam reaches
		  segments(x0=12.5, y0=1.004, x1=12.5, y1=.88,col='gray87')
		  segments(x0=15.75, y0=1.004, x1=15.75, y1=.88,col='gray87')

    # Axes and labels
      segments(x0=12.25, y0=1.004, x1=17.75, y1=1.004,col='black')
      segments(x0=12.25, y0=1.004, x1=12.25, y1=.88,col='black')
      segments(x0=17.64, y0=1.004, x1=17.64, y1=.896,col='black')
      segments(x0=12.25, y0=.896, x1=17.75, y1=.896,col='black')
      text(x=17, y=.905, 'Stillwater Branch (C)', adj=1, cex=1.1)
      axis(side=1, at=c(seq(1,9,1), seq(13,15,1)), col=NA, col.ticks='black')

  # Mainstem survival
    # Hatchery fish
      plotCI(x=seq(1, 17, 1), y=apply(parmsOrg[ , c(seq(1, 33, 2))], 2, mean),
       uiw=apply(parmsOrg[ , c(seq(1, 33, 2))], 2, sd),
       liw=apply(parmsOrg[ , c(seq(1, 33, 2))], 2, sd),
       ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.9, 1), pch=21,
        col='black', pt.bg='black', cex=1.25, lwd=1, cex.axis=3)

    # Wild fish
      plotCI(x=(seq(1, 17, 1)+.25),
        y=apply(parmsOrg[ , c(seq(2, 34, 2))], 2, mean),
        uiw=apply(parmsOrg[ , c(seq(2, 34, 2))], 2, sd),
        liw=apply(parmsOrg[ , c(seq(2, 34, 2))], 2, sd),
        ylab='', xlab='', xaxt='n', yaxt='n', ylim=c(0.9, 1), pch=21,
        col='black', pt.bg='gray', cex=1.25, lwd=1, add=TRUE, slty=2,
      	cex.axis=3)
	  # Dam reaches
		  segments(x0=3.5, y0=1.004, x1=3.5, y1=.88,col='gray87')
		  segments(x0=4.75, y0=1.004, x1=4.75, y1=.88,col='gray87')
		  segments(x0=8.5, y0=1.004, x1=8.5, y1=.88,col='gray87')
		  segments(x0=9.75, y0=1.004, x1=9.75, y1=.88,col='gray87')
		  segments(x0=13.5, y0=1.004, x1=13.5, y1=.88,col='gray87')
		  segments(x0=15.75, y0=1.004, x1=15.75, y1=.88,col='gray87')
		  segments(x0=16.5, y0=1.004, x1=16.5, y1=.88,col='gray87')

    # Axes and labels
      axis(side=1, at=seq(1, 17, 1), as.character(seq(1, 17, 1)))
      axis(side=2, at=seq(0.9, 1, 0.02), sprintf('%.2f', seq(0.9, 1, 0.02)),
        las=2)
      text(x=17, y=.905, 'Penosbcot River (A)', adj=1, cex=1.1)
      mtext(side=1, 'Upstream', line=3, adj=0, cex=1.5)
      mtext(side=1, 'Downstream', line=3, adj=1, cex=1.5)
      mtext(side=1, 'Reach', line=5, cex=1.5, adj=.45)
	    box()
  # Turn off the graphics device and save the file
    dev.off()

# FIGURE 6- Environmental survival covariates
# Read in the capture history data and reconstruct the covariates used in the
# FW survival analysis
  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodCapturesReady.csv',
    	header=F)

  # Read in covariates for the god model
    covs <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda') # Its called ch9

# Do data manipulation to get the data that were used in the analysis
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

# Distance vectors for standardized survival estimation
  # Piscataquis River reach lengths
    NamPisc<-names(ch9)[c(9:17, 27)]
    rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
    rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
    dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

  # Penobscot River reach lengths
    NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
    rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
    rkmMain2 <- rkmMain[2:length(rkmMain)]
    dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

  # Stillwater Branch reach lengths
    NamStill<-names(ch9)[c(30:32, 36)]
    rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
    rkmStill2 <- rkmStill[2:length(rkmStill)]
    dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
    dstill <- c(rep(0, 12), dStill, rep(0,4))

# Go back and re-create the standardized individual covariates
  # Format and standardize covariates from covs df for bugs input
    # Discharge covariates
      Discharge <- scale(covs$Discharge)[,1]
      DischargeS <- c((scale(covs$Discharge))[,1]^2)

    # Accumulated thermal units
      ATU <- scale(covs$ATU)[,1]
      ATUS <- c((scale(covs$ATU))[,1]^2)

    # Accumulated thermal units
      Ph <- scale(covs$Ph)[,1]
      PhS <- c((scale(covs$Ph))[,1]^2)

    # Accumulated thermal units
      T <- scale(covs$T)[,1]
      TS <- c((scale(covs$T))[,1]^2)

    # Rearing history
      Origin <- as.numeric(factor(covs$Origin,
        levels=c("Wild", "Hatchery")))-1

# Load the file containing posterior estimates for environmental covariates
	load(file='C:/Tag/pseudoPriorsForRoundTwo.r')

# Create a df with just the betas
  betas <- parmdis[ , c(90:101)]
	head(betas)

# Detection probability
  # Calculate effect of tag type on detection probability
	  tagmod <- c(mean(betas[ , 1]), quantile(betas[ , 1], probs=c(0.025, 0.975)))
	  tagmod
	  exp(tagmod)/(1+exp(tagmod))
	# Calculate effect of discharge on detection probability
	  dismod <- c(mean(betas[ , 2]), quantile(betas[ , 2], probs=c(0.025, 0.975)))
	  newflow <- seq(min(Discharge), max(Discharge), 0.001)
	  rnFlow <- newflow*sd(covs$Discharge)+mean(covs$Discharge)
	  preds1 <- exp(dismod[1]*newflow)/(1+exp(dismod[1]*newflow))
		preds2 <- exp(dismod[2]*newflow)/(1+exp(dismod[2]*newflow))
		preds3 <- exp(dismod[3]*newflow)/(1+exp(dismod[3]*newflow))
	  plot(rnFlow, preds1, type='l', col='black', lty=1, ylim=c(0,1), lwd=3)
	  par(new=TRUE)
	  plot(rnFlow, preds2, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3)
	  par(new=TRUE)
		plot(rnFlow, preds3, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3)
	  upper <- c(max(preds1), max(preds2), max(preds3))
	  lower <- c(min(preds1), min(preds2), min(preds3))
	  upper
	  lower

	# Get the betas for survival
	  sbetas <- betas[ , 3:10]

	# Create a file to which the figure can be written
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/Figure6.tiff',
  	  width=9000, height=8000, pointsize=18, compression="lzw", res=500)
	# Set up the plotting frame
	  par(mfrow=c(2,2))
	# Discharge
	  dismod <- c(mean(sbetas[ , 1]), quantile(sbetas[ , 1],
	  	probs=c(0.025, 0.975)))
	  dismod2 <- c(mean(sbetas[ , 2]), quantile(sbetas[ , 2],
	  	probs=c(0.025, 0.975)))
	  newflow <- seq(min(Discharge), max(Discharge), 0.001)
	  rnFlow <- newflow*sd(covs$Discharge)+mean(covs$Discharge)
	  preds1 <- exp(dismod[1]*newflow+dismod2[1]*newflow^2)/(1+exp(dismod[1]*
	  	newflow+dismod2[1]*newflow^2))
		preds2 <- exp(dismod[2]*newflow+dismod2[2]*newflow^2)/(1+exp(dismod[2]*
			newflow+dismod2[2]*newflow^2))
		preds3 <- exp(dismod[3]*newflow+dismod2[3]*newflow^2)/(1+exp(dismod[3]*
			newflow+dismod2[3]*newflow^2))
	  par(mar=c(6,7,1,1))
	  plot(rnFlow, preds1, type='l', col='black', lty=1, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
	  plot(rnFlow, preds2, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
		plot(rnFlow, preds3, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
			xlab='', ylab='', xaxt='n', yaxt='n')
		axis(side=1, at=seq(200, 2200, 500), as.character(seq(200, 2200, 500)),
			cex.axis=1.25)
	  axis(side=2, at=seq(0, 1,.2), sprintf('%.1f',seq(0, 1, .2)), cex.axis=1.25,
	  	las=2)
	  mtext(side=2, line=4, cex=1.75, expression(paste("Apparent survival ( ",
	  	widehat(italic('S'))[italic(t)],")", sep='')), adj=-1.5)
	  mtext(side=1, line=4, expression(paste('Discharge (m'^'3', plain('\u00b7'),
		  's'^'-1', ')', sep='')), cex=1.5)
	  text(x=min(rnFlow), y=.975, '(a)', cex=2, adj=0.25)
	# ATU
	  ATUmod <- c(mean(sbetas[ , 3]), quantile(sbetas[ , 3], probs=c(0.025, 0.975)))
	  ATUmod2 <- c(mean(sbetas[ , 4]), quantile(sbetas[ , 4], probs=c(0.025, 0.975)))
	  newATU <- seq(min(ATU), max(ATU), 0.001)
	  rnATU <- newATU*sd(covs$ATU)+mean(covs$ATU)
	  apreds1 <- exp(ATUmod[1]*newATU+ATUmod2[1]*newATU^2)/(1+exp(ATUmod[1]*
	  	newATU+ATUmod2[1]*(newATU^2)))
		apreds2 <- exp(ATUmod[2]*newATU+ATUmod2[2]*newATU^2)/(1+exp(ATUmod[2]*
			newATU+ATUmod2[2]*(newATU^2)))
		apreds3 <- exp(ATUmod[3]*newATU+ATUmod2[3]*newATU^2)/(1+exp(ATUmod[3]*
			newATU+ATUmod2[3]*(newATU^2)))
	  plot(rnATU, apreds1, type='l', col='black', lty=1, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
	  plot(rnATU, apreds2, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
		plot(rnATU, apreds3, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
		axis(side=1, at=seq(200, 500, 50), as.character(seq(200, 500, 50)),
			cex.axis=1.25)
	  axis(side=2, at=seq(0, 1, .2), sprintf('%.1f',seq(0, 1, .2)),
	  	cex.axis=1.25, las=2)
	  mtext(side=1, line=4, 'Accumulated thermal units', cex=1.5)
	  text(x=min(rnATU), y=.975, '(b)', cex=2, adj=0.25)
	# Photoperiod
		par(mar=c(6,7,1,1))
	  Phmod <- c(mean(sbetas[ , 5]), quantile(sbetas[ , 5],
	  	probs=c(0.025, 0.975)))
	  Phmod2 <- c(mean(sbetas[ , 6]), quantile(sbetas[ , 6],
	  	probs=c(0.025, 0.975)))
	  newPh <- seq(min(Ph), max(Ph), 0.001)
	  rnPh <- newPh*sd(covs$Ph)+mean(covs$Ph)
	  ppreds1 <- exp(Phmod[1]*newPh+Phmod2[1]*newPh^2)/(1+exp(Phmod[1]*
	  	newPh+Phmod2[1]*newPh^2))
		ppreds2 <- exp(Phmod[2]*newPh+Phmod2[2]*newPh^2)/(1+exp(Phmod[2]*
			newPh+Phmod2[2]*newPh^2))
		ppreds3 <- exp(Phmod[3]*newPh+Phmod2[3]*newPh^2)/(1+exp(Phmod[3]*
			newPh+Phmod2[3]*newPh^2))
	  plot(rnPh, ppreds1, type='l', col='black', lty=1, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
	  plot(rnPh, ppreds2, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
		plot(rnPh, ppreds3, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
		axis(side=1, at=seq(13, 16, .5), sprintf('%.1f', seq(13, 16, .5)),
			cex.axis=1.25)
	  axis(side=2, at=seq(0, 1,.2), sprintf('%.1f', seq(0, 1, .2)),
	  	cex.axis=1.25, las=2)
	  mtext(side=1, line=4, 'Photoperiod (hours)', cex=1.5)
	  text(x=min(rnPh), y=.975, '(c)', cex=2, adj=0.25)
	# Temperature
	  Tmod <- c(mean(sbetas[ , 7]), quantile(sbetas[ , 7],
	  	probs=c(0.025, 0.975)))
	  Tmod2 <- c(mean(sbetas[ , 8]), quantile(sbetas[ , 8],
	  	probs=c(0.025, 0.975)))
	  newT <- seq(min(T), max(T), 0.001)
	  rnT <- newT*sd(covs$T)+mean(covs$T)
	  tpreds1 <- exp(Tmod[1]*newT+Tmod2[1]*newT^2)/(1+exp(Tmod[1]*
	  	newT+Tmod2[1]*newT^2))
		tpreds2 <- exp(Tmod[2]*newT+Tmod2[2]*newT^2)/(1+exp(Tmod[2]*
			newT+Tmod2[2]*newT^2))
		tpreds3 <- exp(Tmod[3]*newT+Tmod2[3]*newT^2)/(1+exp(Tmod[3]*
			newT+Tmod2[3]*newT^2))
	  plot(rnT, tpreds1, type='l', col='black', lty=1, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
	  plot(rnT, tpreds2, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
	  par(new=TRUE)
		plot(rnT, tpreds3, type='l', col='gray', lty=2, ylim=c(0,1), lwd=3,
	  	xlab='', ylab='', xaxt='n', yaxt='n')
		axis(side=1, at=seq(0, 20, 5), as.character(seq(0, 20, 5)), cex.axis=1.25)
	  axis(side=2, at=seq(0, 1, .2), sprintf('%.1f',seq(0, 1, .2)), cex.axis=1.25,
	  	las=2)
    mtext(side=1, line=4,
    	expression(paste('Temperature (', plain('\u00b0'), 'C)')), cex=1.5)
	  text(x=min(rnT), y=.975, '(d)', cex=2, adj=0.25)
  # Turn off the graphics device and save the file
	  dev.off()

# FIGURE 7- CHANGES IN SURVIVAL FOLLOWING MANAGEMENT ACTIONS (CALCULATIONS)
  # Load the data for the annual survival estimates
    require(plotrix)
    require(vioplot)
		load('C:/Year/GodModelAnnualSurvivalEstimates.rda')

	# Empty vectors
		gwr <- c()
		vzr <- c()
		gws <- c()
		hds <- c()
		vzs <- c()
		mdi <- c()
		ori <- c()
		swi <- c()

  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
  for(i in 1:30000){
    Sys.sleep(0.001)
    # Great Works removal
      gwr[i] <- c(
        mean(c(parmsYear[i ,119], parmsYear[i ,120]))-
        mean(c(parmsYear[i ,113],parmsYear[i ,114],parmsYear[i ,115],
		    parmsYear[i ,116], parmsYear[i ,117], parmsYear[i ,118])))
    # VZ removal
      vzr[i] <- c(
        mean(c(parmsYear[i ,136]))-
        mean(c(parmsYear[i ,129], parmsYear[i ,130], parmsYear[i ,131],
			  parmsYear[i ,132], parmsYear[i ,133], parmsYear[i ,134],
        parmsYear[i ,135])))
    # GW shutdown
      gws[i] <- c(
        mean(c(parmsYear[i ,116], parmsYear[i ,117],
			  parmsYear[i ,118], parmsYear[i ,119], parmsYear[i ,120]))-
        mean(c(parmsYear[i ,113], parmsYear[i ,114], parmsYear[i ,115])))
		# Howland shutdown
			hds[i] <- c(
				mean(c(parmsYear[i ,212],parmsYear[i ,213],
					parmsYear[i ,214], parmsYear[i ,215], parmsYear[i ,216]))-
				mean(c(parmsYear[i ,209], parmsYear[i ,210], parmsYear[i ,211])))
		# VZ shutdown
			vzs[i] <- c(
				mean(c(parmsYear[i ,132], parmsYear[i ,133], parmsYear[i ,134],
					parmsYear[i ,135]), 100000, replace=T)-
				mean(c(parmsYear[i ,129], parmsYear[i ,130],
					parmsYear[i ,131])))
		# Milford Increases
			mdi[i] <- c(
			 mean(c(parmsYear[i ,110], parmsYear[i ,111], parmsYear[i ,112]))-
			 mean(c(parmsYear[i ,105], parmsYear[i ,106], parmsYear[i ,107],
			 parmsYear[i ,108], parmsYear[ ,109])))
		# Orono increases
			ori[i] <- c(
				mean(c(parmsYear[i ,239], parmsYear[i ,240]))-
				mean(c(parmsYear[i ,233], parmsYear[i ,234], parmsYear[i ,235],
				parmsYear[i ,236], parmsYear[i ,237], parmsYear[i ,238])))
		# Stillwater increases
			swi[i] <- c(
				mean(c(parmsYear[i ,231], parmsYear[i ,232]))-
				mean(c(parmsYear[i ,225], parmsYear[i ,226], parmsYear[i ,227],
				parmsYear[i ,228], parmsYear[i ,229], parmsYear[i ,230])))
    setTxtProgressBar(pb, i)
  }
  close(pb)

# Changes through free-flowing (reference) reaches for periods during turbine
# shutdowns at Veazie, Great Works, and Howland Dams
  # Great Works shutdowns
  freeDSgwTurbines <- c()
  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
  for(i in 1:30000){
    Sys.sleep(0.001)
    # Great Works removal
      freeDSgwTurbines[i] <- c(
        mean(c(parmsYear[i ,124], parmsYear[i ,125], parmsYear[i ,126],
        parmsYear[i ,127], parmsYear[i ,128]))-
        mean(c(parmsYear[i ,121],parmsYear[i ,122],parmsYear[i ,123]
		   )))
    setTxtProgressBar(pb, i)
  }
  close(pb)
  freeUSvzTurbines <- freeDSgwTurbines # VZ is same as gw

  # Howland shutdowns
  freeDShwTurbines <- c()
  for(i in 1:30000){
    Sys.sleep(0.001)
    # Great Works removal
			freeDShwTurbines[i] <- c(
				mean(c(parmsYear[i ,76],parmsYear[i ,77],
					parmsYear[i ,78], parmsYear[i ,79], parmsYear[i ,80]))-
				mean(c(parmsYear[i ,73], parmsYear[i ,74], parmsYear[i ,75])))
    setTxtProgressBar(pb, i)
  }
  close(pb)

	# Changes through free-flowing (reference) reaches for periods before and after
	# dam removal at Veazie and Great Works Dams
	  # Great Works removal
	  freeDSgwRemoval <- c()
	  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
	  for(i in 1:30000){
	    Sys.sleep(0.001)
	    # Great Works removal
	      freeDSgwRemoval[i] <- c(
	        mean(c(parmsYear[i ,127], parmsYear[i ,128]))-
	        mean(c(parmsYear[i ,121],parmsYear[i ,122],parmsYear[i ,123],
			    parmsYear[i ,124], parmsYear[i ,125], parmsYear[i ,126])))
	    setTxtProgressBar(pb, i)
	  }
	  close(pb)

	  # Veazie Removal
	  freeUSvzRemoval <- c()
	  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
	  for(i in 1:30000){
	    Sys.sleep(0.001)
	    # Veazie removal
	      freeUSvzRemoval[i] <- c(
	        mean(c(parmsYear[i ,128]))-
	        mean(c(parmsYear[i ,121],parmsYear[i ,122],parmsYear[i ,123],
			    parmsYear[i ,124], parmsYear[i ,125], parmsYear[i ,126],
	       parmsYear[i, 127])))
	    setTxtProgressBar(pb, i)
	  }
	  close(pb)

	# Changes through free-flowing (reference) reaches for periods before and after
	# increases in hydropower generation
	  # Milford increase
	  freeMLusIncrease <- c()
	  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
	  for(i in 1:30000){
	    Sys.sleep(0.001)
	    # Milford increase
	      freeMLusIncrease[i] <- c(
	        mean(c(parmsYear[i ,94], parmsYear[i ,95], parmsYear[i ,96]))-
	        mean(c(parmsYear[i ,89],parmsYear[i ,90],parmsYear[i ,91],
			    parmsYear[i ,92], parmsYear[i ,93])))
	    setTxtProgressBar(pb, i)
	  }
	  close(pb)

	  # Stillwater increase
	  freeUSswIncrease <- c()
	  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
	  for(i in 1:30000){
	    Sys.sleep(0.001)
	    # Stillwater increase
	      freeUSswIncrease[i] <- c(
					mean(c(parmsYear[i ,95], parmsYear[i ,96]))-
					mean(c(parmsYear[i ,89], parmsYear[i ,90], parmsYear[i ,91],
					parmsYear[i ,92], parmsYear[i ,93], parmsYear[i ,94])))
	    setTxtProgressBar(pb, i)
	  }
	  close(pb)

	  # Orono increase
	  freeDSorIncrease <- c()
	  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
	  for(i in 1:30000){
	    Sys.sleep(0.001)
	    # Orono increase
	      freeDSorIncrease[i] <- c(
					mean(c(parmsYear[i ,127], parmsYear[i ,128]))-
					mean(c(parmsYear[i ,121], parmsYear[i ,122], parmsYear[i ,123],
					parmsYear[i ,124], parmsYear[i ,125], parmsYear[i ,126])))
	    setTxtProgressBar(pb, i)
	  }
	  close(pb)

# FIGURE 7- CHANGES IN SURVIVAL FOLLOWING MANAGEMENT ACTIONS
  # Load the data for the annual survival estimates
    require(plotrix)
    require(vioplot)
		load('C:/Year/GodModelAnnualSurvivalEstimates.rda')

	# Empty vectors
		gwr <- c()
		vzr <- c()
		gws <- c()
		hds <- c()
		vzs <- c()
		mdi <- c()
		ori <- c()
		swi <- c()

  pb <- txtProgressBar(min = 0, max = nrow(parmsYear), style = 3)
  for(i in 1:30000){
    Sys.sleep(0.001)
    # Great Works removal
      gwr[i] <- c(
        mean(c(parmsYear[i ,119], parmsYear[i ,120]))-
        mean(c(parmsYear[i ,113],parmsYear[i ,114],parmsYear[i ,115],
		    parmsYear[i ,116], parmsYear[i ,117], parmsYear[i ,118])))
    # VZ removal
      vzr[i] <- c(
        mean(c(parmsYear[i ,136]))-
        mean(c(parmsYear[i ,129], parmsYear[i ,130], parmsYear[i ,131],
			  parmsYear[i ,132], parmsYear[i ,133], parmsYear[i ,134],
        parmsYear[i ,135])))
    # GW shutdown
      gws[i] <- c(
        mean(c(parmsYear[i ,116], parmsYear[i ,117],
			  parmsYear[i ,118], parmsYear[i ,119], parmsYear[i ,120]))-
        mean(c(parmsYear[i ,113], parmsYear[i ,114], parmsYear[i ,115])))
		# Howland shutdown
			hds[i] <- c(
				mean(c(parmsYear[i ,212],parmsYear[i ,213],
					parmsYear[i ,214], parmsYear[i ,215], parmsYear[i ,216]))-
				mean(c(parmsYear[i ,209], parmsYear[i ,210], parmsYear[i ,211])))
		# VZ shutdown
			vzs[i] <- c(
				mean(c(parmsYear[i ,132], parmsYear[i ,133], parmsYear[i ,134],
					parmsYear[i ,135]), 100000, replace=T)-
				mean(c(parmsYear[i ,129], parmsYear[i ,130],
					parmsYear[i ,131])))
		# Milford Increases
			mdi[i] <- c(
			 mean(c(parmsYear[i ,110], parmsYear[i ,111], parmsYear[i ,112]))-
			 mean(c(parmsYear[i ,105], parmsYear[i ,106], parmsYear[i ,107],
			 parmsYear[i ,108], parmsYear[ ,109])))
		# Orono increases
			ori[i] <- c(
				mean(c(parmsYear[i ,239], parmsYear[i ,240]))-
				mean(c(parmsYear[i ,233], parmsYear[i ,234], parmsYear[i ,235],
				parmsYear[i ,236], parmsYear[i ,237], parmsYear[i ,238])))
		# Stillwater increases
			swi[i] <- c(
				mean(c(parmsYear[i ,231], parmsYear[i ,232]))-
				mean(c(parmsYear[i ,225], parmsYear[i ,226], parmsYear[i ,227],
				parmsYear[i ,228], parmsYear[i ,229], parmsYear[i ,230])))
    setTxtProgressBar(pb, i)
  }
  close(pb)

# FIGURE 7- CHANGES IN SURVIVAL FOLLOWING MANAGEMENT ACTIONS
# Create a file to which the figure can be written
	tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/Figure7.tiff',
	  width=9000, height=8000, pointsize=18, compression="lzw", res=500)
	par(mfrow=c(2,1), mar=c(1,1,1,1), oma=c(7,6.25,3.5,1))

  # Reference reaches
  plot(0:1,0:1,type='n', xlim=c(0.75,8.25), ylim=c(-.3,.2), axes=FALSE,
    ann=FALSE)
  abline(h=0, lty=2, col='black', lwd=2)
  vioplot(add=TRUE,
    freeDSgwRemoval , freeUSvzRemoval, freeDSgwTurbines, freeDShwTurbines,
  	freeUSvzTurbines, freeMLusIncrease, freeDSorIncrease, freeUSswIncrease,
    wex=1.1, col='gray87', boxwex=2)
	# Labels for management actions
	mtext(side=3, 'Action', line=3, cex=2)
	mtext(side=3, 'Removals', line=.5, adj=0.07, cex=1.3)
	mtext(side=3, 'Seasonal shutdowns', line=.5, adj=0.41, cex=1.3)
	mtext(side=3, 'Increased generation', line=.5, adj=0.9, cex=1.3)
	# Y-axis labels
	axis(side=2, at=seq(-.5, .5, .2), sprintf('%.2f', seq(-.5, .5, .2)), las=2,
		cex.axis=1.5)
  abline(v=2.5, lty=1, lwd=2)
	abline(v=5.5, lty=1, lwd=2)
	text(x=.69, y=.17, '(a)', cex=2)

	# Impacted reaches
  plot(0:1,0:1,type='n', xlim=c(0.75,8.25), ylim=c(-.3,.2), axes=FALSE,
    ann=FALSE)
  abline(h=0, lty=2, col='black', lwd=2)
  vioplot(add=TRUE,
    gwr, vzr, gws, hds, vzs, mdi, ori, swi,
    wex=1.1, col='gray87', boxwex=2)
	abline(v=2.5, lty=1, lwd=2)
	abline(v=5.5, lty=1, lwd=2)
	mtext(side=1, 'Reach', line=6.5, cex=2)
  text(x=.69, y=.17, '(b)', cex=2)
	mtext(side=2, expression(paste('Change in survival per km ( ',
		widehat(italic('S'))[italic(D[t])], ')', sep='')),
		line=4.5, cex=2, adj = -2)

	axis(side=1, at=seq(1,8,1), c('Great Works', 'Veazie', 'Great Works',
		'Howland', 'Veazie', 'Milford', 'Orono', 'Stillwater'),
		cex.axis=1.3, padj=.5)
	axis(side=1, at=seq(1,8,1),
    c(expression(paste('(', italic(widehat('S')[15]^A),')', sep='')),
      expression(paste('(', italic(widehat('S')[17]^A),')', sep='')),
      expression(paste('(', italic(widehat('S')[15]^A),')', sep='')),
      expression(paste('(', italic(widehat('S')[9]^B),')', sep='')),
      expression(paste('(', italic(widehat('S')[17]^A),')', sep='')),
      expression(paste('(', italic(widehat('S')[14]^A),')', sep='')),
      expression(paste('(', italic(widehat('S')[15]^C),')', sep='')),
      expression(paste('(', italic(widehat('S')[14]^C),')', sep=''))
    ),
		cex.axis=1.5, padj=0, line=3, col=NA)
	axis(side=2, at=seq(-.5, .5, .2), sprintf('%.2f', seq(-.5, .5, .2)), las=2,
		cex.axis=1.5)
# Turn of device and save the file
  dev.off()
################################################################################

################################################################################
# Create table of summary statistics for all fish tagged in all years by all
# USGS and UMaine researchers
#
# INPUT FILENAME(S):  'GodlyCovariates.csv', 'time' df from PART 5
# MY FUNCTIONS
# NAMED OUPUT:        taggingTable (df)
# OUTPUT FILENAME:    'TaggingTable.csv'
################################################################################
# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')
	  covs$ReleaseSite <- as.character(covs$ReleaseSite)

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    totes <- sum(ddply(covs, .(Year, ReleaseSite, Origin), nrow)[,4])
    nreleased <- ddply(covs, .(Year, ReleaseSite, Origin), nrow)[ ,4]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

  # Get rid of Piscataquis detections for fish released below Howland in 2005
    for(i in 1:nrow(GD)){
      if(covs$Year[i]==2005 & covs$ReleaseSite[i]=='Howland'){
        GD[i, 1:9] <- 0
      } else {
        next;
      }
    }

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

# Correct erroneous spelling
	covs$ReleaseSite[covs$ReleaseSite=='EastBranch'] <- 'East Branch'
	covs$ReleaseSite[covs$ReleaseSite=='Abbott'] <- 'Abbot'

# Get summary stats for the the release groups
	tagged <- ddply(covs, .(Year, ReleaseSite, Origin), summarise,
		ATU=round(mean(ATU)),
		Q=round(mean(Discharge)),
		PP=sprintf('%.1f', mean(Ph)),
		T=sprintf('%.1f', mean(T))
		)
  tagged
	dates <- ddply(covs, .(Year, ReleaseSite, Origin), summarise,
		SD = sd(yday(ReleaseDate)))

# Get total of fish tagged and relocated
  relocated <- ddply(covs, .(Year, ReleaseSite, Origin), nrow)[ ,4]
# Make a column for number released vs relocated
  nreleased
# Make it all into a df and write a file
  t.table <- data.frame(tagged[ , c(1,3,2)], paste(relocated, '(', nreleased,
  	')', sep=''), tagged [ , c(4:7)])
  names(t.table)[4] <- 'n'
  write.table(t.table, 'TaggingTable.csv', row.names=FALSE, sep=',',
  	quote=FALSE)
################################################################################

################################################################################
# Make data for supplemental material for JAE
#
# INPUT FILENAME(S): 'GodCapturesReady.csv', 'GodlyCovariates.csv',
#                    'GodModelCHWithColnames.rda'
# MY FUNCTIONS:
# NAMED OUPUT:       'GD', 'CH', 'ms.god', 'msGOD.Tag', 'msData'
# OUTPUT FILENAME:   'msData.rda'
################################################################################
# PACKAGE INSTALL AND LOAD
  #install.packages('reshape2')
  require(reshape2)

# SET DIRECTORIES AND READ IN THE DATA
  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')

  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('GodModelCHWithColnames.rda') # Its called ch9

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

  # Get the desired years- change or comment out based on objectives
    #GD <- GD[covs$Year==2013 | covs$Year==2014 ]

  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]


  # Make the capture history for Bugs model
    # Make an empty matrix to hold the captures
      CH <- matrix(ncol=ncol(GD), nrow=nrow(GD))

    # Fill in the matrix with encounter data
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          CH[i, t] <- GD[i, t]
        } # t
      } # i

    # Get first occasion of encounter for each individual
      get.first <- function(x) min(which(x!=0))
      f <- apply(CH, 1, get.first)

    # Replace all of the zeros with 4s b/c zeros aren't allowed and there are
    # three observable states in the model
      for(i in 1:nrow(GD)){
        for(t in 1:ncol(GD)){
          if(CH[i, t]<1) CH[i, t] <- 4
        } # t
      } # i

  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2
      dstill <- c(rep(0, 12), dStill, rep(0,4))

# INDIVIDUAL COVARIATES
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(covs)){
	    if((covs$Origin[i] == 'Wild') & covs$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(covs$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }

  msData <- list()
  msData$CH <- CH
  msData$Covariates <- data.frame(covs$Origin, covs$Year, covs$Discharge,
    covs$Ph, covs$ATU, covs$T, Tag)
  msData$dStill <- dstill
  msData$dMain <- dMain
  msData$dPisc <- dPisc
  setwd("C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables")
  save(msData, file='msData.rda')
################################################################################

################################################################################
# Make appendix 3 for JAE submission- there are three tables:
# 1) Multiannual survival estimates
# 2) Multiannual survival estimates for each rearing history
# 3) Annual survival estimates
#
# INPUT FILENAME(S): 'GodModelCHWithColnames.rda',
#                    'MultiAnnualSurvivalEstimates.rda',
#                    'GodModelOriginEstimates.rda',
#                    'GodModelAnnualSurvivalEstimates.rda'
# MY FUNCTIONS:
# NAMED OUPUT:       'Res', 'oRes', 'yRes'
# OUTPUT FILENAME:   'MultiAnnualSurvival.csv, 'OriginSurvival.csv',
#                    'AnnualSurvival.csv'
################################################################################
# Package install and load
  require(reshape2)

# APPENDIX S3.A- Mean annual survival estimates in the Penobscot
# Data load
  load('C:/Time/MultiAnnualSurvivalEstimates.rda')
  names(parmsMA)
  names(parmsMA)[28:30] <- c('S.Stillwater.13', 'S.Stillwater.14',
    'S.Stillwater.15')

# Make function to calculate and format CRIS
  CRIS <- function(x){
    return(paste(sprintf('%.3f', quantile(x, probs=0.025)),
      "\u2013",
      sprintf('%.3f', quantile(x, probs=0.975)),
      sep=''))
  }

# Assign names to the reaches within each state
  mReach <- c('Weldon Head Pond 01', 'Weldon Head Pond 02',
    'Weldon Head Pond 03', 'Weldon Dam', '', '', '', 'West Enfield Head Pond',
    'West Enfield Dam', '', '', '', 'Milford Head Pond', 'Milford Dam',
    'Great Works Dam', 'Veazie Head Pond', 'Veazie Dam', '')
  pReach <- c(' ', 'Guilford Dam', 'Dover Head Pond', 'Dover (Moosehead) Dam',
    'Brown Mills Dam', ' ', ' ', 'Howland Head Pond', 'Howland Dam')
  sReach <- c('Gilman Falls', 'Stillwater Dam', 'Orono Dam')

# Assign reach lengths
  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda') # Its called ch9
  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2

  # Combine reach lengths into a vector
    L <- c(dMain[1:18], dPisc, dStill)

# Make formatted columns with the estimate values
  Estimate <- sprintf('%.3f', apply(parmsMA[, c(1:30)], 2, mean))
  SD <- sprintf('%.3f', apply(parmsMA[, c(1:30)], 2, sd))
  Q<- apply(parmsMA[, c(1:30)], 2, CRIS)

# Make everthing into a data table
  Res <- data.frame(format(c(mReach, pReach, sReach),
   justify='left'), Estimate, SD, Q, L)
  Res <- data.frame(
    colsplit(rownames(Res), pattern='\\.', names=c('Par','State', 'Num'))[,2:3],
    Res)
  Res[,1] <- format(Res[,1], justify='left')
  names(Res) <- c('State', 'Num', 'Reach', 'Mean', 'S.D.', '95% CRI', 'L')
  Res <- Res[with(Res, order(Num, State)), ]
  row.names(Res) <- NULL
  Res
  write.table(Res, 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/MultiAnnualSurvival.csv',
    row.names=FALSE, quote=FALSE, sep=',')

# APPENDIX S3.B- mean annual survival estimates for each rearing history in the
# Penobscot
# Data load
  load('C:/Origin/GodModelOriginEstimates.rda')
  names(parmsOrg)
  parmsOrg <- parmsOrg[ ,1:60]

# Make new names for the dataframe that can be used to sort estimates later
  mName <- paste('S.Mainstem.', sort(rep(seq(1,18,1), 2)), sep='')
  pName <- paste('S.Piscataquis.', sort(rep(seq(1,9,1), 2)), sep='')
  sName <- paste('S.Stillwater.', sort(rep(seq(13,15,1), 2)), sep='')

  names(parmsOrg) <- c(mName, pName, sName)
  n <- c(mName, pName, sName) # For sorting later b/c cant have dup rownames

# Make function to calculate and format CRIS
  CRIS <- function(x){
    return(paste(sprintf('%.3f', quantile(x, probs=0.025)),
      "\u2013",
      sprintf('%.3f', quantile(x, probs=0.975)),
      sep=''))
  }

# Assign names to the reaches within each state
  mReach <- c('Weldon Head Pond 01', 'Weldon Head Pond 02',
    'Weldon Head Pond 03', 'Weldon Dam', '', '', '', 'West Enfield Head Pond',
    'West Enfield Dam', '', '', '', 'Milford Head Pond', 'Milford Dam',
    'Great Works Dam', 'Veazie Head Pond', 'Veazie Dam', '')
  pReach <- c(' ', 'Guilford Dam', 'Dover Head Pond', 'Dover (Moosehead) Dam',
    'Brown Mills Dam', ' ', ' ', 'Howland Head Pond', 'Howland Dam')
  sReach <- c('Gilman Falls', 'Stillwater Dam', 'Orono Dam')

# Now repeat the reach names for each rearing history
  main <- c()
  for(i in 1:length(mReach)){
   main <- append(main, rep(mReach[i],2))
  }
  pisc <- c()
  for(i in 1:length(pReach)){
   pisc <- append(pisc, rep(pReach[i],2))
  }
  still <- c()
  for(i in 1:length(sReach)){
   still <- append(still, rep(sReach[i],2))
  }

# Assign rearing history to each rep of reach
  Origin <- rep(c('Hatchery', 'Wild'), 30)

# Assign reach lengths
  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda') # Its called ch9
  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2

# Now repeat the reach lengths for each rearing history
  lmain <- c()
  for(i in 1:(length(dMain)-1)){
   lmain <- append(lmain, rep(dMain[i],2))
  }
  lpisc <- c()
  for(i in 1:length(dPisc)){
   lpisc <- append(lpisc, rep(dPisc[i],2))
  }
  lstill <- c()
  for(i in 1:length(dStill)){
   lstill <- append(lstill, rep(dStill[i],2))
  }

# Combine reach lengths into a vector
  L <- c(lmain, lpisc, lstill)

# Make formatted columns with the estimate values
  Estimate <- sprintf('%.3f', apply(parmsOrg[, c(1:ncol(parmsOrg))], 2, mean))
  SD <- sprintf('%.3f', apply(parmsOrg[, c(1:ncol(parmsOrg))], 2, sd))
  Q<- apply(parmsOrg[, c(1:ncol(parmsOrg))], 2, CRIS)

# Make everthing into a data table
  oRes <- data.frame(format(c(main, pisc, still),
   justify='left'), Origin, Estimate, SD, Q, sprintf('%.1f',L))
  row.names(oRes) <- NULL
  oRes <- data.frame(
    colsplit(n, pattern='\\.', names=c('Par','State', 'Num'))[,2:3],
    oRes)
  oRes[,1] <- format(oRes[,1], justify='left')
  oRes[,4] <- format(oRes[,4], justify='left')
  names(oRes) <- c('State', 'Num', 'Reach', 'Origin', 'Mean', 'S.D.',
    '95% CRI', 'L')
  oRes <- oRes[with(oRes, order(Num, State)), ]
  row.names(oRes) <- NULL
  oRes
  write.table(oRes, 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/OriginSurvival.csv',
    row.names=FALSE, quote=FALSE, sep=',')

# APPENDIX S3.C- annual survival estimates in the Penobscot
# Data load
  load('C:/Year/GodModelAnnualSurvivalEstimates.rda')
  names(parmsYear)
  parmsYear <- parmsYear[ ,1:240]

# Make new names for the dataframe that can be used to sort estimates later
  mName <- paste('S.Mainstem.', sort(rep(seq(1,18,1), 8)), sep='')
  pName <- paste('S.Piscataquis.', sort(rep(seq(1,9,1), 8)), sep='')
  sName <- paste('S.Stillwater.', sort(rep(seq(13,15,1), 8)), sep='')

  names(parmsYear) <- c(mName, pName, sName)
  n <- c(mName, pName, sName) # For sorting later b/c cant have dup rownames

# Make function to calculate and format CRIS
  CRIS <- function(x){
    return(paste(sprintf('%.3f', quantile(x, probs=0.025)),
      "\u2013",
      sprintf('%.3f', quantile(x, probs=0.975)),
      sep=''))
  }

# Assign names to the reaches within each state
  mReach <- c('Weldon Head Pond 01', 'Weldon Head Pond 02',
    'Weldon Head Pond 03', 'Weldon Dam', '', '', '', 'West Enfield Head Pond',
    'West Enfield Dam', '', '', '', 'Milford Head Pond', 'Milford Dam',
    'Great Works Dam', 'Veazie Head Pond', 'Veazie Dam', '')
  pReach <- c(' ', 'Guilford Dam', 'Dover Head Pond', 'Dover (Moosehead) Dam',
    'Brown Mills Dam', ' ', ' ', 'Howland Head Pond', 'Howland Dam')
  sReach <- c('Gilman Falls', 'Stillwater Dam', 'Orono Dam')

# Now repeat the reach names for each rearing history
  main <- c()
  for(i in 1:length(mReach)){
   main <- append(main, rep(mReach[i],8))
  }
  pisc <- c()
  for(i in 1:length(pReach)){
   pisc <- append(pisc, rep(pReach[i],8))
  }
  still <- c()
  for(i in 1:length(sReach)){
   still <- append(still, rep(sReach[i],8))
  }
  Year <- rep(c(2005,2006,seq(2009, 2014, 1)), 30)

# Assign reach lengths
  # Read in the ch that has column names to get the distance matrices for
  # reach-length standardization of survival
    load('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda') # Its called ch9
  # Distance vectors for standardized survival estimation
    # Piscataquis River reach lengths
      NamPisc<-names(ch9)[c(9:17, 27)]
      rkmPisc<-colsplit(NamPisc, pattern="_", names=c('rkm', 'site'))[,1]
      rkmPisc2 <- rkmPisc[2:length(rkmPisc)]
      dPisc <- rkmPisc[1:(length(rkmPisc)-1)] - rkmPisc2

    # Penobscot River reach lengths
      NamMain<-names(ch9)[c(18:29, 33:length(names(ch9)))]
      rkmMain<-colsplit(NamMain, pattern="_", names=c('rkm', 'site'))[,1]
      rkmMain2 <- rkmMain[2:length(rkmMain)]
      dMain <- rkmMain[1:(length(rkmMain)-1)] - rkmMain2

    # Stillwater Branch reach lengths
      NamStill<-names(ch9)[c(30:32, 36)]
      rkmStill<-colsplit(NamStill, pattern="_", names=c('rkm', 'site'))[,1]
      rkmStill2 <- rkmStill[2:length(rkmStill)]
      dStill <- rkmStill[1:(length(rkmStill)-1)] - rkmStill2

# Now repeat the reach lengths for each rearing history
  lmain <- c()
  for(i in 1:(length(dMain)-1)){
   lmain <- append(lmain, rep(dMain[i],8))
  }
  lpisc <- c()
  for(i in 1:length(dPisc)){
   lpisc <- append(lpisc, rep(dPisc[i],8))
  }
  lstill <- c()
  for(i in 1:length(dStill)){
   lstill <- append(lstill, rep(dStill[i],8))
  }

# Combine reach lengths into a vector
  L <- c(lmain, lpisc, lstill)

# Make formatted columns with the estimate values
  Estimate <- sprintf('%.3f', apply(parmsYear[, c(1:ncol(parmsYear))], 2, mean))
  SD <- sprintf('%.3f', apply(parmsYear[, c(1:ncol(parmsYear))], 2, sd))
  Q<- apply(parmsYear[, c(1:ncol(parmsYear))], 2, CRIS)

# Make everthing into a data table
  yRes <- data.frame(format(c(main, pisc, still),
   justify='left'), Year, Estimate, SD, Q, sprintf('%.1f',L))
  row.names(yRes) <- NULL
  yRes <- data.frame(
    colsplit(n, pattern='\\.', names=c('Par','State', 'Num'))[,2:3],
    yRes)
  yRes[,1] <- format(yRes[,1], justify='left')
  yRes[,4] <- format(yRes[,4], justify='left')
  names(yRes) <- c('State', 'Num', 'Reach', 'Year', 'Mean', 'S.D.',
    '95% CRI', 'L')
  yRes <- yRes[with(yRes, order(Num, State)), ]
  row.names(yRes) <- NULL
  yRes
  write.table(yRes, 'C:/Users/Dan/Desktop/Manuscripts/GodModel/Figures & Tables/AnnualSurvival.csv',
    row.names=FALSE, quote=FALSE, sep=',')
################################################################################
} # Code-folding marker
# END PART 9. FRESHWATER SURVIVAL ANALYSIS--------------------------------------


# PART 10. FRESHWATER BEHAVIORAL ANALYSIS---------------------------------------
{ # Code-folding marker
################################################################################
# Create table of summary statistics for all fish tagged in all years by all
# USGS and UMaine researchers- This one is for FW analyses.  It re-writes the
# surgery file to finalize tagging data compilation through 2014 as well
#
# INPUT FILENAME(S):  'GodCapturesReady.csv', 'GodlyCovariates.csv'
# MY FUNCTIONS
# NAMED OUPUT:        tagged (df)
# OUTPUT FILENAME:    'FWTaggingTable.csv'
################################################################################
# Package install and load
  #install.packages('plyr')
  #install.packages('lubridate')
  require(plyr)
  require(lubridate)

# SET DIRECTORIES AND READ IN THE DATA
	# Set system time to UTC for ease of handling time/number conversions later on
	Sys.setenv(TZ='UTC')

	# Make fxn for getting dates back from numeric format using UTC, default origin
	  DTAgain<- function(x){
		  as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
			strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
	  }

  # Set working directory
    setwd('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis')

  # Read in the capture history data created in the GodModelCaptureHistory code
    GD <- read.csv('GodCapturesReady.csv', header=F)

  # Read in covariates for the god model
    covs <- read.csv('GodlyCovariates.csv')
	  covs$ReleaseSite <- as.character(covs$ReleaseSite)

# DATA MANIPULATION AND CAPTURE HISTORY CODE
  # Remove fish released at Verona and Brewer (estuary releases)
    GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
    n <- ddply(covs, .(Year, ReleaseSite, Origin), nrow)[ , 4] # Count total
  # Remove fish that were never located
    covs <- covs[rowSums(GD)!=0, ]
    GD <- GD[rowSums(GD)!=0, ]

# Correct erroneous spelling
	covs$ReleaseSite[covs$ReleaseSite=='EastBranch'] <- 'East Branch'
	covs$ReleaseSite[covs$ReleaseSite=='Abbott'] <- 'Abbot'

# Use h and w for hatchery and wild
  covs$Origin<-as.character(covs$Origin)
  for(i in 1:nrow(covs)){
    if(covs$Origin[i]=='Hatchery'){
    covs$Origin[i] <- 'H'
    } else {
      covs$Origin[i] <- 'W'
    }
  }

# Get summary stats for the the release groups
	tagged <- ddply(covs, .(Year, ReleaseSite, Origin), summarise,
		LF=paste(round(mean(ForkLength.mm)), '(', '\U00b1 ' ,
			round(sd(ForkLength.mm)), ')', sep=''),
		Mass=paste(round(mean(Mass.g)), '(', '\U00b1 ' , round(sd(Mass.g)), ')',
			sep=''),
    NKA=paste(sprintf('%.1f', mean(ATPaseActivity)), '(', '\U00b1 ' ,
    	sprintf('%.1f', sd(ATPaseActivity)), ')', sep=''),
		Date=substr(DTAgain(mean(as.numeric(as.POSIXct(Date)))),
      start=6, stop=10)
		#T=sprintf('%.1f', mean(T))
		)
  tagged
  tagged <- data.frame(tagged[ , c(1:3)],n,tagged[ ,c(4:7)]

# Add release rkms to the table
  ReleaseSites <- c(sort(unique(as.character(covs$ReleaseSite))))
  ReleaseRKMs <- c(187, 162, 99, 144, 142,  92.3, 62, 149)
  cbind(ReleaseSites, ReleaseRKMs)

  for(i in 1:nrow(tagged)){
    for(t in 1:length(ReleaseSites)){
      if(tagged$ReleaseSite[i]==ReleaseSites[t]){
        tagged$ReleaseSite[i] <- paste(ReleaseSites[t], '(', ReleaseRKMs[t],
        	')', sep='')
      } else {
        next;
      }
    }
  }

# Write the tagging table to a file
  write.table(tagged, 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/FWTaggingTable.csv', sep=',', quote=FALSE,
    row.names=FALSE)
################################################################################

################################################################################
# Make whole-river capture histories for all of the fish ever
# released for use in getting times of fish relocations in FW.
# This code is virtually identical for code used in making the ch for the god
# model.
#
# INPUT FILENAME(S):  'Final 2005-present Database.rda' ('AllData'),
# MY FUNCTIONS:
# NAMED OUPUT:        'ch'...'ch8', 'FWrecaptures'
# OUTPUT FILENAME:    'FullFWCaptureHistories.rda', 'FWrecapsSimplified.rda'
################################################################################
# Install packages
  #install.packages("tcltk2")
  #install.packages("reshape")
  #install.packages("reshape2")
  #install.packages("lubridate")
  require(reshape)
  require(tcltk2)
  require(lubridate)
  require(reshape2)

# Read in the 2005-present acoustic telemetry detection data
  load(file.choose())

# Make the capture histories out of the comprehensive location histories for
# each fish.
  # Use the cast function to make the capture-history matrix for processing.
  # Takes about 5 minutes to complete...
    #unix.time( # Uncomment to time the cast fxn
    ch <- cast(AllData,
    	TagID + ForkLength.mm + Mass.g + ATPaseActivity + ReleaseSite +
    	ReleaseDate + Origin ~ RKM + LocationCode)
    #) # Uncomment to time the cast fxn

  # Remove duplicated tag IDs in ch
    ch <- ch[!duplicated(ch$TagID),]

  # Reverse the order of the Location columns to sort by decreasing rkms
    ch <- ch[ , c(1:7, rev(8:ncol(ch)))]

  # Use a nested for loop to make a 1/0 matrix of captures at each site
  # Takes about 20 seconds to run
    #unix.time( # uncomment to time
    for(i in 1:nrow(ch)){
      for (t in 8:ncol(ch)){
        if(ch[i, t] > 0){
          ch[i, t] <- 1
        } else {
          ch[i, t] <- 0
        }
      } # t
    } # i
    #) # uncomment to time

  # Spot-check the capture history to make sure it worked ok
    # First ten rows and columns
      ch[1:10, 1:10]
    # First ten rows and last ten columns
      ch[1:10, (ncol(ch) - 10):ncol(ch)]

  # Add 'Year' to the capture history for each fish as a grouping variable that
  # can be used to help model year/release-group random effects later on
    ch$Year <- substr(as.character(ch$ReleaseDate), 1, 4)

    # Rearrange the df so that 'Year' is in the front with other covariates
      ch <- ch[ , c(1:7, ncol(ch), 8:(ncol(ch)-1))]

  # Write this object to a file because it contains all of the FW releases and
  # relocations- may want it later on...
  # Its called FullFWCaptureHistories.rda
    setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
    save(ch, file='FullFwCaptureHistories.rda')


  # Now combine all detections after FortPoint to create a 'Final' detection
  # array at the end of the river
    for(i in 1:nrow(ch)){
	  if(sum(ch[i , (ncol(ch) - 5):ncol(ch)]) > 0){
        ch[i, (ncol(ch) - 5)] <- 1
      } else {
      	ch[i, (ncol(ch) - 5)] <- 0
      }
    } # i

    # Get rid of the columns for those intervals
      ch <- ch[ , 1:(ncol(ch) - 5)]

    # Rename the final interval to be 'Bay'
	  colnames(ch)[ncol(ch)] <- "-6_BAY"

  # Now combine all detections from Graham Station through FortPoint to create a
  # detection event for the estuary
  # array at the end of the river
    for(i in 1:nrow(ch)){
	  if(sum(ch[i , (ncol(ch) - 39):(ncol(ch)-1)]) > 0){
        ch[i, (ncol(ch) - 39)] <- 1
      } else {
      	ch[i, (ncol(ch) - 39)] <- 0
      }
    } # i

    # Get rid of the extra columns
      ch <- ch[ , c(1:(ncol(ch) - 39), ncol(ch))]

    # Rename the final interval to be 'Estuary'
	  colnames(ch)[ncol(ch) - 1] <- "44_Estuary"

  # Now move on to simplifying the capture history to get something that is
  # similar among years so it can be used to assess changes in mvmt rate

  # Get rid of the release site columns
    ch2 <- ch[ , c(1:8, 10:16, 19:22, 24:25, 27:36, 38, 40:44, 46:ncol(ch))]

  # Get rid of columns that suck for detection or are not shared across years
    ch3 <- ch2[ , c(1:14, 16:22, 26:ncol(ch2))]

  # Combine locations that are one
    # First, do Stillwater
      for(i in 1:nrow(ch3)){
        if(sum(ch3[i , c(37:39)]) > 0) {
          ch3[i , c(37)] <- 1
        } else {
          ch3[i, 37] <- 0
        }
      }
      ch4 <- ch3[ , c(1:37, 40:ncol(ch3))] # Take out the extra rows
      colnames(ch4)[37]<- '61.5_STHP'

    # Next, do Milford
      for(i in 1:nrow(ch4)){
        if(sum(ch4[i , c(35:36)]) > 0) {
          ch4[i , c(35)] <- 1
        } else {
          ch4[i, 35] <- 0
        }
      }
      ch5 <- ch4[ , c(1:35, 37:ncol(ch4))] # Take out the extra rows
      colnames(ch5)[35] <- '62.2_MLHPW'

    # Next, do Orson Island and Gilman Falls
      for(i in 1:nrow(ch5)){
        if(sum(ch5[i , c(32, 34)]) > 0) {
          ch5[i , c(32)] <- 1
        } else {
          ch5[i, 32] <- 0
        }
      }
      ch6 <- ch5[ , c(1:33, 35:ncol(ch5))] # Take out the extra rows
      colnames(ch6)[32] <- '63_GFHP'

    # Combine PISC02 and PISC03 for the 2005-2006 data...or don't
      for(i in 1:nrow(ch6)){
        if(ch6$Year[i]==2005|ch6$Year[i]==2005){
          ch6[i, 20] <- sum(ch6[i , 20], ch6[ , 22])
        } else {
          ch6[i, 20] <- ch6[i, 20]
        }
      }
      for(i in 1:nrow(ch6)){
        if(ch6[i, 20] > 0){
          ch6[i, 20] <- 1
        } else {
          ch6[i, 20] <- 0
        }
      }

    # Get rid of the crappy sites I forgot
      ch7 <- ch6[ , c(1:21, 23:29, 31:36, 38:ncol(ch6))]

    # Reorganize the capture history
      ch8 <- ch7[ , c(1:14, 20, 24, 26, 15:19,21:23, 25, 27:30, 33, 35, 31:32,
        34, 36:ncol(ch7))]

    # Fix detection at GDHP to 1.00 for 2014...or don't
       ch8[ch8$Year==2014 & ch8$ReleaseSite=='Abbott' , 10] <-
         ch8[ch8$Year==2014 & ch8$ReleaseSite=='Abbott' , 9]

    # Make the states for the final capture history
      # First, re-code the piscataquis
        for (i in 1:nrow(ch8)){
          for(t in 9:17){
            if(ch8[i, t] > 0){
              ch8[i, t] <- 2
            } else {
              ch8[i, t] <- 0
            }
          }
        }

      # Now, re-code the Stillwater Branch
        for (i in 1:nrow(ch8)){
          for(t in 30:32){
            if(ch8[i, t] > 0){
              ch8[i, t] <- 3
            } else {
              ch8[i, t] <- 0
            }
          }
        }

    # Concatenate the actual capture histories (just the ones and zeros,
    # based on release site and migratory route in the lower river
      # Create variable for Stillwater or Mainstem, unknown gets mean 0.5
      # Numbers in this loop don't need to change unless ch changes in the
      # future
        Stillwater <- c()
	      for(i in 1:nrow(ch8)){
	        if( sum(ch8[i, c(31:32)]) > 0){
	          Stillwater[i] <- 1
	        } else {
            Stillwater[i] <- 0
	        }
	      } # i

# Write the simplified capture history with data from all years to a file, save
# for later.
  FWrecaptures <- ch8
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  save(FWrecaptures, file='FWrecapsSimplified.rda')
################################################################################

################################################################################
# Get minimum of date and time for all fish used in the FW behavioral analyses
# at each receiver location using functions created above in previous sections
# then match the column names to the column names in the simplified captrure
# history.  Need to run it again b/c the 2014 data weren't in the first one!
#
# INPUT FILENAME(S): Final 2005-present database.rda (AllData),
#                    FullFWCaptureHistories.rda (ch),
# MY FUNCTIONS:      DTAgain created, timesC created, rkmC created
# NAMED OUTPUT:
# OUTPUT FILENAME:   MinDateTime05-14.rda, FWMovementData.rda,
# 	                 DateTimeMatrix05-14.rda, FWDateTimeTag05-14.rda
#	                   FWDateTimeTagCH05-14.rda, FWmvmtData05-14.rda
#             	     GodModelCHWithColnames.rda,
#                    FWMovementRatesForAnalysis.rda
################################################################################
# Install and load necessary packages
	#install.packages('Rcpp') # Uncomment to run, may require restart of R
	#install.packages('lubridate') # Uncomment to run
	#install.packages('reshape') # Uncomment to run
	#install.packages('reshape2') # Uncomment to run
	#install.packages('tcltk2') # Uncomment to run
	require(Rcpp)
  require(lubridate)
	require(reshape)
	require(reshape2)
	require(tcltk2)

# Read in the capture history data and do necessary manipulation
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  load(file='FullFwCaptureHistories.rda') # It's called ch!

  # Create variable to hold release rkm in ch
	  # Convert the vector data type for ReleaseSite in ch to chr from factor
	    ch$ReleaseSite <- as.character(ch$ReleaseSite)
    # Fix the ones that are spelled wrong
      ch$ReleaseSite[ch$ReleaseSite=='EastBranch'] <- 'East Branch'

	# Get RKM for each of the release sites so it can be added to 'Releases'
    # Get the unique values of release site
      ReleaseSites <- c(sort(unique(as.character(ch$ReleaseSite))))

    # Make a vector of corresponding rkms for release sites
      ReleaseRKMs <- c(187, 43.5, 162, 99, 144, 142, 92.3, 62, 9.5, 149)

  # Create a column in the capture history to hold release RKM and fill it in
	  ch$ReleaseRKM <- 0
	  for(i in 1:length(ReleaseSites)){
      for(t in 1:nrow(ch)){
	      if(ch$ReleaseSite[t]==ReleaseSites[i]){
	      	ch$ReleaseRKM[t] <- ReleaseRKMs[i]
	      } else {
	      	next;
	      } # else
	    } # t
    } # i

  # Check to make sure it worked
    print(c(head(ch$ReleaseRKM, 10), tail(ch$ReleaseRKM, 10)))

  # Change the order of the columns to get the ch at the end
    ch <- ch[ , c( 1:8, ncol(ch), 9:(ncol(ch)-1) )]

# Read in the 2005-present acoustic telemetry detection data and do manipulation
  load(file.choose()) # AllData

# Set system time to UTC for ease of handling time/number conversions later on
  Sys.setenv(TZ='UTC')

# Make fxn for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

# Prep timestamp data from the main database and put it in necessary format
  dt <- as.POSIXct(AllData$DateTime, tz="UTC")

# Variable definition for use in c++ fxns
   y <- as.character(AllData$TagID) # Make TagID character var
   w  <- as.numeric(dt, origin=1970-01-01) # Make date numeric for ease
   v <- as.character(AllData$LocationCode) # Make LocationCode character var
   data2 <- unique(data.frame(y, v, w)[ , 1:2]) # Make df of unique combinations
   y2 <- data2[ , 1] # Vectorize values of TagID in data2
   v2 <- data2[ , 2] # Vectorize values of LocationCode in data2

# Create function to get min DateTime for each fish at each location
# Includes a progress meter that reports % complete for external ('i') loop
  cppFunction('
    NumericVector timesC(CharacterVector v, CharacterVector y,
    NumericVector w, CharacterVector v2, CharacterVector y2){
      int n = v.size();
      NumericVector z(n);
      int m = v2.size();
      NumericVector Timestamp(m);
      float length = m;
      std::cout << std::endl;

      for(int i=0; i < m; ++i) {
        for(int t=0; t < n; ++t) {
          if(y[t]==y2[i] && v[t]==v2[i]) {
            z[t] = w[t];
          } else {
            z[t] = 1e16;
          }
        }
        std::cout<<"\\r"
        <<100*(float)(i+1)/(float)length
        <<"% "
        <<std::flush;
        Timestamp[i] = min(z);
      }
      return Timestamp;
    }
  ')

# Get min Date time for each fish at each location (run the function)
  timestamp <- timesC(v, y, w, v2, y2) # Call fxn & get the data
  head(DTAgain(timestamp)) # View results to check datatype
  Dates <- DTAgain(timestamp) # Convert results back to DateTime from numeric
  head(Dates) # Data QC- spotcheck

# Write minDateTime to a file
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  save(Dates, file='MinDateTime05-14.rda')

# Make df with TagID, LocationCode, min date for each tag at each location
  FWcaps <- data.frame(data2, Dates) # Create the object
  names(FWcaps)<-c("TagID", "LocationCode", "DateTime") # Rename the columns

# Create a vector of River kilometers for each location in the receiver diary
  rkms <- unique(AllData[ , c(4, 5)])

# Make sure variable type is not factor
	rkms$LocationCode <- as.character(rkms$LocationCode)

# Read in 'DateTimeGod.csv' if caps is not in workspace
  #caps <- read.csv(file.choose()) # Uncomment to run
  FWcaps$LocationCode <- as.character(FWcaps$LocationCode) # Format data

# Create variables for C++ Loop used to get RKM in each
  rkm <- rkms$RKM # Vectorize unique values of rkm
  receiversLocations <- rkms$LocationCode # Vectorize unique location code
  FWcapsLocations <- FWcaps$LocationCode # Vectorize location from caps df above

# Make c++ loop to get rkm for each location in caps df above
# Includes a progress meter that reports % complete for external ('i') loop
  cppFunction('
    NumericVector rkmC(CharacterVector FWcapsLocations,
    NumericVector rkm, CharacterVector receiversLocations){
      int n = FWcapsLocations.size();
      NumericVector z(n);
      int m = receiversLocations.size();
      NumericVector RKM(m);
      float length = m;
      std::cout << std::endl;

        for(int i=0; i < m; ++i) {
          for(int t=0; t < n; ++t) {
            if(FWcapsLocations[t]==receiversLocations[i]) {
              z[t] = rkm[i];
            } else {
              continue;
            }
          }
          std::cout<<"\\r"
          <<100*(float)(i+1)/(float)length
          <<"% "
          <<std::flush;
        }
        return z;
      }
    ')
    rk<-rkmC(FWcapsLocations, rkm, receiversLocations) # Run the function
    newFWCaps<-data.frame(FWcaps, rk) # Add rkm to the caps df
    names(newFWCaps)[4]<-"RKM" # Change the name of the RKM column
    head(newFWCaps) # Data QC- spotcheck

# Write the results to an r-data file to save it forever
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  fwMvmt <- newFWCaps
  save(fwMvmt, file='FWMovementData.rda')
  load(file='FWMovementData.rda')

# Fix the spelling for East Branch...again
  fwMvmt$LocationCode[fwMvmt$LocationCode=='EastBranch'] <- 'East Branch'

# Now make the Date-Time Matrix
  Date <- cast(fwMvmt, TagID ~ RKM + LocationCode, value="DateTime",
  	fun.aggregate=min)

# Save the matrix
  save(Date, file='DateTimeMatrix05-14.rda')
  load(file='DateTimeMatrix05-14.rda')

# Put the columns in order of decreasing rkm
	Date <- Date[,c(1,rev(2:ncol(Date)))]

# Compile the last few detections into a single column
  Salt <- apply(Date[ , 51:ncol(Date)], 1, min)

# Rearrange date matrix
	Date <- cbind(Date[ , c(1:50)], Salt)

# Get rid of 'infinity' values in the DateTime matrix
  for(i in 2:ncol(Date)){
  	Date[ , i][is.infinite(Date[ , i])] <- 0
  	#Date[ , i] <- DTAgain(Date[ , i])
  }
  for(i in 2:ncol(Date)){
  	#Date[ , i][is.infinite(Date[ , i])] <- 0
  	Date[ , i] <- DTAgain(Date[ , i])
  }

# Save the matrix again
  save(Date, file='FWDateTimeTag05-14.rda')
  load(file='FWDateTimeTag05-14.rda')

# Get the GodModel ch to get the reaches we want, its called ch8
  load(file='C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda')

# Combine the STHP locations in the 'Date' df so that all of the data are kept
	Date <- c()
  for(i in 1:nrow(Date)){
  	for(t in 41:43){
  		Date[, t] <- as.numeric(Date[,t])
      if(Date[i, t]==0){
      	Date[i, t] <- 3E+9
      } else {
        next;
      }
  	}
	  Date[i, 41] <- apply( Date[i, c(41:43)] ,1, min )
  }

	# Rename the column that is holding the compiled data
	  names(Date)[41] <- names(ch9)[31]

	# Get rid of the extra SW columns
	  Date <- Date[ ,c(1:41, 44:ncol(Date))]

	# Now get rid of 300000000 and replace with 1970-01-01 00:00:00
	  for(i in 1:nrow(Date)){
     if(Date[i, 41]==3E+9){
      	Date[i, 41] <- 0
      } else {
        next;
      }
  	}
	  iscmoveDate[,41] <- DTAgain(Date[,41])

# Get the necessary columns and keep the salty data from Dates df
  Dates2 <- cbind(Date[ , names(Date) %in% names(ch9)])
  Dates2$Salt <- Date$Salt
  names(Dates2)
  save(Dates2, file='FWDateTimeTagCH05-14.rda')
  load(file='FWDateTimeTagCH05-14.rda')

# Now get the covariates for the fish used in the survival analysis
  fwMvmtData<- cbind(ch9[ ,c(1:8)], Dates2[ , 2:ncol(Dates2)])
  fwmvmtdata2 <- fwMvmtData[ , ]
  save(fwMvmtData, file='FWmvmtData05-14.rda')

# Get the rest of the covariates for freshwater that were used in the god model!
	# Read in the movement data and those covariates
    load(file='FWmvmtData05-14.rda')

	# Read in the covariates for the god model to move them into the behavior df
	  covs <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv')

 	# Combine the movement data and covariates with the survival covariates
	# Note that at this point both the movement and survival dfs are sorted by
	# TagID so the rows all match...awesome
	  fwmove <- cbind(fwMvmtData[ , c(1:8)], covs[ , c(9:ncol(covs))],
	   fwMvmtData[ , c(9:ncol(fwMvmtData))])

	# Save the file out...can tell I'm getting paranoid and obsessive as the end-
	# time grows near
	  save(fwmove, file='FullFWMovements.rda')
################################################################################

################################################################################
# Organize the movements and calculate movement rate for each fish through the
# standardized fw reaches in the Penobscot River
#
# INPUT FILENAME(S): FullFWMovements.rda,
#                    GodCapturesReady.csv, GodlyCovariates.csv
# MY FUNCTIONS:      DTAgain created
# NAMED OUTPUT:
# OUTPUT FILENAME:   *moves.txt (path-specific movement rates),
#                    FWMovementRatesForAnalysis.rda
################################################################################
# Install and load necessary packages
	#install.packages('reshape2)
	#install.packages('lubridate')
	require(reshape2)
	require(lubridate)

# Set system time to UTC for ease of handling time/number conversions later on
  Sys.setenv(TZ='UTC')

# FXN for getting dates back from numeric format using UTC, default origin
  DTAgain<- function(x){
    as.POSIXct(as.numeric(x), tz=Sys.timezone(), origin=as.POSIXct(
      strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")))
  }

# Set the directory and read in the data file
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
	load(file='FullFWMovements.rda')
	# Add RKM to the SW column
	  names(fwmove)[ncol(fwmove)] <- "43.5_Salt"

# Read in the capture history data created in the GodModelCaptureHistory code
  GD <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodCapturesReady.csv', header=F)
  load(file='C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda')

# Read in covariates for the god model
  covs <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv')

# Remove fish released at Verona and Brewer (estuary releases)
  GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
  covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

# Remove fish that were never located
	covs <- covs[rowSums(GD)!=0, ]
  GD <- GD[rowSums(GD)!=0, ]

# Remove fish that were never located from the movement data, too
	mvmt <- fwmove[fwmove$TagID %in% covs$TagID,]
	ch9 <- ch9[ch9$TagID %in% covs$TagID,]

# Fix the VZHP02 reach rkms, b/c it should only be about .25 km downstream of
# VZHP03...not sure how they got all effed up
	names(mvmt)[46] <- "51.07_VZHP02"

# Set up unique matrices for each of the four possible movement paths
	piscmoves <- mvmt[ , c(1:17, 18:23,29,33, 35:38, 39,41,43, 45:ncol(mvmt))]
	piscStillmoves <- mvmt[ , c(1:17, 18:23,29,33, 35:38, 40,42,44, 45:ncol(mvmt))]
	mainmoves <- mvmt[ , c(1:17, 24:28,30:32,34,  36:38, 39,41,43, 45:ncol(mvmt))]
	mainStillmoves <- mvmt[ , c(1:17, 24:28,30:32,34,  36:38, 40,42,44, 45:ncol(mvmt))]

# Now, need to set up a seperate procedure with which to calculate reach-
# specific movement rates within the appropriate path for each fish.

# Calculate movement rates for fish stocked in PISCATAQUIS using MAINSTEM
  # Create a text-based progress bar and initialize it
	  file.remove('piscmoves.txt')
    pb <- txtProgressBar(min = 0, max = nrow(piscmoves), style = 3)
    for(t in 1:nrow(piscmoves)){
      Sys.sleep(0.0001) # Length of time-out for each progress bar write
      # Define a row for each fish
        FishRow <- c(piscmoves[t,18:ncol(piscmoves)])
      # Get the sites with detections
        ListSites <- names(FishRow)
      # Get the rkm values for those sites
        RKMs <- colsplit(ListSites, "_", names=c("RKM","Site"))[,1]
      # Define a vector over which movement rates will be calculated
        MoVector <- piscmoves[t, 18:ncol(piscmoves)]
    	  MoVector <- as.numeric(MoVector)
      # Define empty vectors to hold:
        Time <- c() # Amount of time elapsed between min det at each relocation
        Dists <- c() # Distance between relocation events
        Rates <- c() # Rate of movement between relocation events
      # Now calculate speeds, distances, and movement rates
        for(i in 2:length(MoVector)){ # For each fish calculate at each location
          Time[i-1] <- (MoVector[i]-MoVector[i-1]) # Time elapsed during move
          Dists[i-1] <- RKMs[i-1]-RKMs[i] # Distance traveled in RKM
        	if(Time[i-1]==0| Time[i-1]<0 | Time[i-1]>1E+9){
        		Rates[i-1] <- NA
        	} else {
        		if(Time[i-1]>0 & Time[i-1]<3E+6){
              Rates[i-1] <- Dists[i-1]/Time[i-1] # Rate of movement (km/second)
        		} # if brackets
        	} # else brackets
        } # Close i loop

      # Repeat the fish data so the rate measurements can be matched up with
      # individuals and individual covariates (this is just formatting).
        FishD <- piscmoves[c(rep(t,(length(Rates)))), 1:17]
      # Make a dataframe holding fish data and movement rates for all sites at
      # at which each fish was located
        d <- data.frame(FishD, ListSites[1:(length(ListSites)-1)], Rates,
          MoVector[2:length(MoVector)])
        # Write the data to a file, append all fish after first to the end
          write.table(d, "piscmoves.txt", row.names=FALSE, col.names=FALSE,
            append=TRUE, sep=",")
      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
    } # Close t loop

# Calculate movement rates for fish stocked in PISCATAQUIS using Stillwater
  # Create a text-based progress bar and initialize it
		file.remove('piscStillmoves.txt')
    pb <- txtProgressBar(min = 0, max = nrow(piscStillmoves), style = 3)
    for(t in 1:nrow(piscStillmoves)){
      Sys.sleep(0.0001) # Length of time-out for each progress bar write
      # Define a row for each fish
        FishRow <- c(piscStillmoves[t,18:ncol(piscStillmoves)])
      # Get the sites with detections
        ListSites <- names(FishRow)
      # Get the rkm values for those sites
        RKMs <- colsplit(ListSites, "_", names=c("RKM","Site"))[,1]
      # Define a vector over which movement rates will be calculated
        MoVector <- piscStillmoves[t, 18:ncol(piscStillmoves)]
    	  MoVector <- as.numeric(MoVector)
      # Define empty vectors to hold:
        Time <- c() # Amount of time elapsed between min det at each relocation
        Dists <- c() # Distance between relocation events
        Rates <- c() # Rate of movement between relocation events
      # Now calculate speeds, distances, and movement rates
        for(i in 2:length(MoVector)){ # For each fish calculate at each location
          Time[i-1] <- (MoVector[i]-MoVector[i-1]) # Time elapsed during move
          Dists[i-1] <- RKMs[i-1]-RKMs[i] # Distance traveled in RKM
        	if(Time[i-1]==0| Time[i-1]<0 | Time[i-1]>1E+9){
        		Rates[i-1] <- NA
        	} else {
        		if(Time[i-1]>0 & Time[i-1]<3E+6){
              Rates[i-1] <- Dists[i-1]/Time[i-1] # Rate of movement (km/second)
        		} # if brackets
        	} # else brackets
        } # Close i loop

      # Repeat the fish data so the rate measurements can be matched up with
      # individuals and individual covariates (this is just formatting).
        FishD <- piscStillmoves[c(rep(t,(length(Rates)))), 1:17]
      # Make a dataframe holding fish data and movement rates for all sites at
      # at which each fish was located
        d <- data.frame(FishD, ListSites[1:(length(ListSites)-1)], Rates,
          MoVector[2:length(MoVector)])
        # Write the data to a file, append all fish after first to the end
          write.table(d, "piscStillmoves.txt", row.names=FALSE, col.names=FALSE,
            append=TRUE, sep=",")
      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
    } # Close t loop

# Calculate movement rates for fish stocked in MAINSTEM using MAINSTEM
  # Create a text-based progress bar and initialize it
		file.remove('mainmoves.txt')
    pb <- txtProgressBar(min = 0, max = nrow(mainmoves), style = 3)
    for(t in 1:nrow(mainmoves)){
      Sys.sleep(0.0001) # Length of time-out for each progress bar write
      # Define a row for each fish
        FishRow <- c(mainmoves[t,18:ncol(mainmoves)])
      # Get the sites with detections
        ListSites <- names(FishRow)
      # Get the rkm values for those sites
        RKMs <- colsplit(ListSites, "_", names=c("RKM","Site"))[,1]
      # Define a vector over which movement rates will be calculated
        MoVector <- mainmoves[t, 18:ncol(mainmoves)]
    	  MoVector <- as.numeric(MoVector)
      # Define empty vectors to hold:
        Time <- c() # Amount of time elapsed between min det at each relocation
        Dists <- c() # Distance between relocation events
        Rates <- c() # Rate of movement between relocation events
      # Now calculate speeds, distances, and movement rates
        for(i in 2:length(MoVector)){ # For each fish calculate at each location
          Time[i-1] <- (MoVector[i]-MoVector[i-1]) # Time elapsed during move
          Dists[i-1] <- RKMs[i-1]-RKMs[i] # Distance traveled in RKM
        	if(Time[i-1]==0| Time[i-1]<0 | Time[i-1]>1E+9){
        		Rates[i-1] <- NA
        	} else {
        		if(Time[i-1]>0 & Time[i-1]<3E+6){
              Rates[i-1] <- Dists[i-1]/Time[i-1] # Rate of movement (km/second)
        		} # if brackets
        	} # else brackets
        } # Close i loop

      # Repeat the fish data so the rate measurements can be matched up with
      # individuals and individual covariates (this is just formatting).
        FishD <- mainmoves[c(rep(t,(length(Rates)))), 1:17]
      # Make a dataframe holding fish data and movement rates for all sites at
      # at which each fish was located
        d <- data.frame(FishD, ListSites[1:(length(ListSites)-1)], Rates,
          MoVector[2:length(MoVector)])
        # Write the data to a file, append all fish after first to the end
          write.table(d, "mainmoves.txt", row.names=FALSE, col.names=FALSE,
            append=TRUE, sep=",")
      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
    } # Close t loop

# Calculate movement rates for fish stocked in MAINSTEM using MAINSTEM
  # Create a text-based progress bar and initialize it
		file.remove('mainStillmoves.txt')
    pb <- txtProgressBar(min = 0, max = nrow(mainStillmoves), style = 3)
    for(t in 1:nrow(mainStillmoves)){
      Sys.sleep(0.0001) # Length of time-out for each progress bar write
      # Define a row for each fish
        FishRow <- c(mainStillmoves[t,18:ncol(mainStillmoves)])
      # Get the sites with detections
        ListSites <- names(FishRow)
      # Get the rkm values for those sites
        RKMs <- colsplit(ListSites, "_", names=c("RKM","Site"))[,1]
      # Define a vector over which movement rates will be calculated
        MoVector <- mainStillmoves[t, 18:ncol(mainStillmoves)]
    	  MoVector <- as.numeric(MoVector)
      # Define empty vectors to hold:
        Time <- c()  # Amount of time elapsed between min det at each relocation
        Dists <- c() # Distance between relocation events
        Rates <- c() # Rate of movement between relocation events
      # Now calculate speeds, distances, and movement rates
        for(i in 2:length(MoVector)){ # For each fish calculate at each location
          Time[i-1] <- (MoVector[i]-MoVector[i-1]) # Time elapsed during move
          Dists[i-1] <- RKMs[i-1]-RKMs[i] # Distance traveled in RKM
        	if(Time[i-1]==0| Time[i-1]<0 | Time[i-1]>1E+9){
        		Rates[i-1] <- NA
        	} else {
        		if(Time[i-1]>0 & Time[i-1]<3E+6){
              Rates[i-1] <- Dists[i-1]/Time[i-1] # Rate of movement (km/second)
        		} # if brackets
        	} # else brackets
        } # Close i loop

      # Repeat the fish data so the rate measurements can be matched up with
      # individuals and individual covariates (this is just formatting).
        FishD <- mainStillmoves[c(rep(t,(length(Rates)))), 1:17]
      # Make a dataframe holding fish data and movement rates for all sites at
      # at which each fish was located
        d <- data.frame(FishD, ListSites[1:(length(ListSites)-1)], Rates,
          MoVector[2:length(MoVector)])
        # Write the data to a file, append all fish after first to the end
          write.table(d, "mainStillmoves.txt", row.names=FALSE, col.names=FALSE,
            append=TRUE, sep=",")
      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
    } # Close t loop

# Read in the new fw movement data (created in previous loops) for each path
	setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  piscRate <- read.csv('piscmoves.txt', header=F)
  piscStillRate <- read.csv('piscStillmoves.txt', header=F)
  mainRate <- read.csv('mainmoves.txt', header=F)
  mainStillRate <- read.csv('mainStillmoves.txt', header=F)

# Get reach descriptions for each interval in each migratory path
  # Get reaches for piscmain movements
	  reachespR <- paste (
		  colsplit(piscRate$V18[1:18], split="_", names=c("rkm","Loc2"))[,2],
		  "to",
		  colsplit(names(piscmoves)[19:length(names(piscmoves))],
		  	split="_", names=c("rkm","Loc2"))[,2])

  # Get reaches for piscStill movements
  	reachespSR <- paste (
	  	colsplit(piscStillRate$V18[1:18], split="_", names=c("rkm","Loc2"))[,2],
		  "to",
		  colsplit(names(piscStillmoves)[19:length(names(piscStillmoves))],
			  split="_", names=c("rkm","Loc2"))[,2])

  # Get reaches for mainMain movements
	  reachesmR <- paste (
		  colsplit(mainRate$V18[1:18], split="_", names=c("rkm","Loc2"))[,2],
		  "to",
		  colsplit(names(mainmoves)[19:length(names(mainmoves))],
			  split="_", names=c("rkm","Loc2"))[,2])

  # Get reaches for mainStill movements
	  reachesmSR <- paste (
		  colsplit(mainStillRate$V18[1:18], split="_", names=c("rkm","Loc2"))[,2],
		  "to",
		  colsplit(names(mainStillmoves)[19:length(names(mainStillmoves))],
			  split="_", names=c("rkm","Loc2"))[,2])

	# Look at the vectors to make sure they all make sense
	  reachespR
  	reachespSR
	  reachesmR
	  reachesmSR

	# Create a reach type for all of the unique reaches
		# Get the unique reaches from all four movement paths
	    allReaches <- unique(c(reachespR, reachespSR, reachesmR, reachesmSR))
	    # Visual check
	      allReaches
	  # Manually assign a reach type to each of the unique reaches
	    reachtype <- c('H','D','H','D','D',
	    	             'F','F','H','D','F',
	    	             'F','F','H','D','D',
	    	             'H','H','D','H','D',
	    	             'D','D','H','H','H',
	    	             'D','F','F','F','H',
	    	             'D')
	 # Combine unique reaches with reach type
	   reachAndType <- cbind(allReaches, reachtype)
     # Visual check
	     reachAndType

	  # Assign a reach type to each unique reach from four movement paths
	    reach <-list(reachespR,	reachespSR, reachesmR, reachesmSR)
	    reachT <- reach # Place-holder list

	  # Now cross reference reach description with reach types
	  # Made very fast by recursive indexing in lists
	    for(i in 1:length(reach)){
	  	  for(t in 1:length(reach[[i]])){
	  		  for(j in 1:nrow(reachAndType)){
	  		    if(reach[[i]][t]==reachAndType[j, 1]){
	  		  	  reachT[[i]][t]<-reachAndType[j, 2]
	  		    } else {
	  		  	  next;
	  		    }
	  		  }
	  	  }
	    }
	    reach
	    reachT

  # Add reach names to the movement data for each migratory path
    piscRate$Reach <- rep(unlist(reach[1]), 1870)
    piscStillRate$Reach	 <- rep(unlist(reach[2]), 1870)
    mainRate$Reach <- rep(unlist(reach[3]), 1870)
    mainStillRate$Reach	 <- rep(unlist(reach[4]), 1870)
  # Add reach types to the movement data for each migratory path
    piscRate$ReachType <- rep(unlist(reachT[1]), 1870)
    piscStillRate$ReachType	 <- rep(unlist(reachT[2]), 1870)
    mainRate$ReachType <- rep(unlist(reachT[3]), 1870)
    mainStillRate$ReachType	 <- rep(unlist(reachT[4]), 1870)

# Combine the movement rates from all migratory paths into a single df
	fwmvmt <- rbind(piscRate, piscStillRate, mainRate, mainStillRate)
	nrow(fwmvmt)

	# Give the data some names
		names(fwmvmt) <- c(names(mvmt[1:17]), "RKM_Loc", "Rate", "MovDate","Reach",
			"ReachType")

	# Get rid of bogus movement rates that occur as a result of calculations for
	# the different migratory paths
	  fwmvmt <- fwmvmt[fwmvmt$Rate > 0 & !is.na(fwmvmt$Rate),]
	  nrow(fwmvmt)

	# Convert from km/s to km/hr
	  fwmvmt$Rate <- fwmvmt$Rate*3600

	# Now scan for bad data
	  fwmvmt <- fwmvmt[fwmvmt$Rate < 10 | fwmvmt$Rate==10, ]

	# Create a column for rkm
	  fwmvmt$RKM <- colsplit(fwmvmt$RKM_Loc, "_", names=c("RKM","Site"))[,1]

	# Just keep the columns we need
	  fwmvmt <- fwmvmt[ , c(1:9, 11, 13, 15, 19:ncol(fwmvmt))]

	# Create easily manageable names for the data
	  names(fwmvmt) <- c('TagID', 'FL', 'Mass', 'ATP', 'RelSite', 'RelDate',
	  	'Org', 'Year', 'Dis', 'Ph', 'ATU', 'T', 'Rate','MovDate', 'Reach',
	  	'ReachType', 'RKM')

	# Change the date to ordinal
	  fwmvmt$MovDate <- yday(DTAgain(fwmvmt$MovDate))

	# Write the final movement data to a file for future use
    save(fwmvmt, file= 'FWMovementRatesForAnalysis.rda')
################################################################################

################################################################################
# Run the analysis for freshwater movement rates in Penobscot River
#
# INPUT FILENAME(S): FWMovementRatesForAnalysis.rda

# MY FUNCTIONS:      aic.tableR created
# NAMED OUTPUT:      MoveResults, best.mov, Mres
# OUTPUT FILENAME:   MovementModelSelection.csv
#                    Figure2.tif, Figure3.tif
################################################################################
# Install and load packages
  #install.packages('lme4')
	#install.packages('lmerTest')
	#install.packages('AICcmodavg')
  #install.packages('vioplot')
  require(lme4)
	require(lmerTest)
	require(AICcmodavg)
	require(vioplot)

# Read in the data for analysis
	setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  load(file= 'FWMovementRatesForAnalysis.rda') # Its called fwmvmt

# Change the reach type for GW and Veazie in 2014!!!
	for(i in 1:nrow(fwmvmt)){
		if((substr(fwmvmt$Reach[i],start=1,stop=4)=='GWHP' |
		  substr(fwmvmt$Reach[i],start=1,stop=4)=='VZHP') &
	    fwmvmt$Year[i]==2014){
			fwmvmt$ReachType[i] <- 'F'
		} else {
			next;
		}
	}

# Create standardized variables
	originaldata <- fwmvmt
  fwmvmt[ , c(2:4, 9:12, 14, 17)] <- (apply(fwmvmt[ , c(2:4, 9:12, 14, 17)] , 2, scale))

# Create quadratix
	fwmvmt$Dis2 <- fwmvmt$Dis^2
	fwmvmt$Ph2 <- fwmvmt$Ph^2
	fwmvmt$ATU2 <- fwmvmt$ATU^2
	fwmvmt$T2 <- fwmvmt$T^2
	fwmvmt$RKM2 <- fwmvmt$RKM^2

# FUNCTION DEFINITION
  # 'aic.tableR'
  # Function to create AIC Model selection table for mixed models
    # Write a fxn to do it
      aic.tableR <- function(x, y){
        # Create a column to hold number of parameters
          k <- c()
        # Get number of parameters
          for (i in 1:length(x)){
            #k[i] <- (length(x[[i]]$coefficients))
            k[i]<-nrow(data.frame((summary(x[[i]])[10]))) + 1
          }
        # Make blank columns to hold statistics
          AICc <- c(rep(0, length(k)))
          DeltaAIC <- c(rep(0, length(k)))
          AICWeight <- c(rep(0, length(k)))
          Deviance <- c(rep(0, length(k)))
          Relative.likelihood<-c(rep(0, length(k)))
        # Make dataframe to hold results
          ranks<-data.frame(y, k, AICc, DeltaAIC, AICWeight)
          names(ranks)[1] <- "Model"
        # Get AIC for each model
          for(i in 1:nrow(ranks)){
            ranks$AICc[i] <- AIC(x[[i]])+(((2*k[i])*(k[i]+1))/
              (nrow(fwmvmt)-k[i]-1))
          }
        # Sort the table by AICc
          ranks <- ranks[with(ranks, order(AICc)), ]
        # Calculate delta AIC
          for(i in 1:nrow(ranks)){
            ranks$DeltaAIC[i] <- ranks$AICc[i]-ranks$AICc[1]
          }
        # Calculate relative likelihoods
          for(i in 1:nrow(ranks)){
            Relative.likelihood[i] <- exp(-.5*ranks$DeltaAIC[i])
          }
        # Calculate AIC weights
          for(i in 1:length(Relative.likelihood)){
            ranks$AICWeight[i] <- Relative.likelihood[i]/
             sum(Relative.likelihood[1:length(Relative.likelihood)])
          }
        # Round off the calculated columns
          ranks[ , 3:5] <- round(ranks[ , 3:5], 2)
        # Print the model selection results
          return(ranks)
    }

# Create a candidate model set
  Mov.mods = list()
    Mov.mods[[1]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + RKM, data = fwmvmt)
    Mov.mods[[2]] = lmer(log(Rate) ~ (1|TagID)  + RKM + T + T2, data = fwmvmt)
    Mov.mods[[3]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + RKM, data = fwmvmt)
    Mov.mods[[4]] = lmer(log(Rate) ~ (1|TagID)  + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[5]] = lmer(log(Rate) ~ (1|TagID)  + Org + RKM, data = fwmvmt)
    Mov.mods[[6]] = lmer(log(Rate) ~ (1|TagID)  + ATP + RKM, data = fwmvmt)
    Mov.mods[[7]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Org + RKM, data = fwmvmt)
    Mov.mods[[8]] = lmer(log(Rate) ~ (1|TagID)  + Org + RKM + T + T2, data = fwmvmt)
    Mov.mods[[9]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + RKM, data = fwmvmt)
    Mov.mods[[10]] = lmer(log(Rate) ~ (1|TagID)  + Org + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[11]] = lmer(log(Rate)~ (1|TagID)  + ATP + ATU + ATU2 + RKM, data = fwmvmt)
    Mov.mods[[12]] = lmer(log(Rate) ~ (1|TagID)  + ATP + RKM + T + T2, data = fwmvmt)
    Mov.mods[[13]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + RKM, data = fwmvmt)
    Mov.mods[[14]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[15]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[16]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[17]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Org + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[18]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[19]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[20]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[21]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + RKM, data = fwmvmt)
    Mov.mods[[22]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Dis + Dis2 + RKM, data = fwmvmt)
    Mov.mods[[23]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Org + RKM, data = fwmvmt)
    Mov.mods[[24]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[25]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[26]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + RKM + T + T2, data = fwmvmt)
    Mov.mods[[27]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[28]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[29]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Org + Ph + Ph2 + RKM, data = fwmvmt)
    Mov.mods[[30]] = lmer(log(Rate) ~ (1|TagID)  + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[31]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[32]] = lmer(log(Rate) ~ (1|TagID)  + Org + Ph + Ph2 + RKM + T + T2, data = fwmvmt)
    Mov.mods[[33]] = lmer(log(Rate) ~ (1|TagID)  + ReachType + RKM, data = fwmvmt)
    Mov.mods[[34]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[35]] = lmer(log(Rate) ~ (1|TagID)  + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[36]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[37]] = lmer(log(Rate) ~ (1|TagID)  + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[38]] = lmer(log(Rate) ~ (1|TagID)  + Org + ReachType + RKM, data = fwmvmt)
    Mov.mods[[39]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ReachType + RKM, data = fwmvmt)
    Mov.mods[[40]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Org + ReachType + RKM, data = fwmvmt)
    Mov.mods[[41]] = lmer(log(Rate) ~ (1|TagID)  + Org + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[42]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + ReachType + RKM, data = fwmvmt)
    Mov.mods[[43]] = lmer(log(Rate) ~ (1|TagID)  + Org + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[44]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[45]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[46]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[47]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[48]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[49]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[50]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[51]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[52]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[53]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[54]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[55]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Dis + Dis2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[56]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Dis + Dis2 + Org + ReachType + RKM, data = fwmvmt)
    Mov.mods[[57]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[58]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Dis + Dis2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[59]] = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[60]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[61]] = lmer(log(Rate) ~ (1|TagID)  + ATP + ATU + ATU2 + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[62]] = lmer(log(Rate) ~ (1|TagID)  + ATU + ATU2 + Org + Ph + Ph2 + ReachType + RKM, data = fwmvmt)
    Mov.mods[[63]] = lmer(log(Rate) ~ (1|TagID)  + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[64]] = lmer(log(Rate) ~ (1|TagID)  + ATP + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
    Mov.mods[[65]] = lmer(log(Rate) ~ (1|TagID)  + Org + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)

# Make a vector of model names
  MovNames <- c()
    MovNames[1]  <- '  ATU + ATU2 + RKM'
    MovNames[2]  <- '  RKM + T + T2'
    MovNames[3]  <- '  Dis + Dis2 + RKM'
    MovNames[4]  <- '  Ph + Ph2 + RKM'
    MovNames[5]  <- '  Org + RKM'
    MovNames[6]  <- '  ATP + RKM'
    MovNames[7]  <- '  ATU + ATU2 + Org + RKM'
    MovNames[8]  <- '  Org + RKM + T + T2'
    MovNames[9]  <- '  Dis + Dis2 + Org + RKM'
    MovNames[10]  <- '  Org + Ph + Ph2 + RKM'
    MovNames[11]  <- '  ATP + ATU + ATU2 + RKM'
    MovNames[12]  <- '  ATP + RKM + T + T2'
    MovNames[13]  <- '  ATP + Dis + Dis2 + RKM'
    MovNames[14]  <- '  ATP + Ph + Ph2 + RKM'
    MovNames[15]  <- '  ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RKM'
    MovNames[16]  <- '  ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RKM'
    MovNames[17]  <- '  ATU + ATU2 + Dis + Dis2 + Org + Ph + Ph2 + RKM'
    MovNames[18]  <- '  Dis + Dis2 + Ph + Ph2 + RKM + T + T2'
    MovNames[19]  <- '  ATP + Dis + Dis2 + Ph + Ph2 + RKM + T + T2'
    MovNames[20]  <- '  Dis + Dis2 + Org + Ph + Ph2 + RKM + T + T2'
    MovNames[21]  <- '  ATU + ATU2 + Dis + Dis2 + RKM'
    MovNames[22]  <- '  ATP + ATU + ATU2 + Dis + Dis2 + RKM'
    MovNames[23]  <- '  ATU + ATU2 + Dis + Dis2 + Org + RKM'
    MovNames[24]  <- '  Dis + Dis2 + RKM + T + T2'
    MovNames[25]  <- '  ATP + Dis + Dis2 + RKM + T + T2'
    MovNames[26]  <- '  Dis + Dis2 + Org + RKM + T + T2'
    MovNames[27]  <- '  ATU + ATU2 + Ph + Ph2 + RKM'
    MovNames[28]  <- '  ATP + ATU + ATU2 + Ph + Ph2 + RKM'
    MovNames[29]  <- '  ATU + ATU2 + Org + Ph + Ph2 + RKM'
    MovNames[30]  <- '  Ph + Ph2 + RKM + T + T2'
    MovNames[31]  <- '  ATP + Ph + Ph2 + RKM + T + T2'
    MovNames[32]  <- '  Org + Ph + Ph2 + RKM + T + T2'
    MovNames[33]  <- ' ReachType + RKM'
    MovNames[34]  <- ' ATU + ATU2 + ReachType + RKM'
    MovNames[35]  <- ' ReachType + RKM + T + T2'
    MovNames[36]  <- ' Dis + Dis2 + ReachType + RKM'
    MovNames[37]  <- ' Ph + Ph2 + ReachType + RKM'
    MovNames[38]  <- ' Org + ReachType + RKM'
    MovNames[39]  <- ' ATP + ReachType + RKM'
    MovNames[40]  <- '  ATU + ATU2 + Org + ReachType + RKM'
    MovNames[41]  <- '  Org + ReachType + RKM + T + T2'
    MovNames[42]  <- '  Dis + Dis2 + Org + ReachType + RKM'
    MovNames[43]  <- '  Org + Ph + Ph2 + ReachType + RKM'
    MovNames[44]  <- '  ATP + ATU + ATU2 + ReachType + RKM'
    MovNames[45]  <- '  ATP + ReachType + RKM + T + T2'
    MovNames[46]  <- '  ATP + Dis + Dis2 + ReachType + RKM'
    MovNames[47]  <- '  ATP + Ph + Ph2 + ReachType + RKM'
    MovNames[48]  <- '  ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + ReachType + RKM'
    MovNames[49]  <- '  ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + ReachType + RKM'
    MovNames[50]  <- '  ATU + ATU2 + Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM'
    MovNames[51]  <- '  Dis + Dis2 + Ph + Ph2 + ReachType + RKM + T + T2'
    MovNames[52]  <- '  ATP + Dis + Dis2 + Ph + Ph2 + ReachType + RKM + T + T2'
    MovNames[53]  <- '  Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM + T + T2'
    MovNames[54]  <- '  ATU + ATU2 + Dis + Dis2 + ReachType + RKM'
    MovNames[55]  <- '  ATP + ATU + ATU2 + Dis + Dis2 + ReachType + RKM'
    MovNames[56]  <- '  ATU + ATU2 + Dis + Dis2 + Org + ReachType + RKM'
    MovNames[57]  <- '  Dis + Dis2 + ReachType + RKM + T + T2'
    MovNames[58]  <- '  ATP + Dis + Dis2 + ReachType + RKM + T + T2'
    MovNames[59]  <- '  Dis + Dis2 + Org + ReachType + RKM + T + T2'
    MovNames[60]  <- '  ATU + ATU2 + Ph + Ph2 + ReachType + RKM'
    MovNames[61]  <- '  ATP + ATU + ATU2 + Ph + Ph2 + ReachType + RKM'
    MovNames[62]  <- '  ATU + ATU2 + Org + Ph + Ph2 + ReachType + RKM'
    MovNames[63]  <- '  Ph + Ph2 + ReachType + RKM + T + T2'
    MovNames[64]  <- '  ATP + Ph + Ph2 + ReachType + RKM + T + T2'
    MovNames[65]  <- '  Org + Ph + Ph2 + ReachType + RKM + T + T2'

# Create a model-selection table
  MovResults <- head(aic.tableR(Mov.mods, MovNames), 10)

# Look at model-selection results
	MovResults

# Save the model-selection table to a file
   setwd("C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables")
   write.table(MovResults, "MovementModelSelection.csv", row.names=FALSE,
     sep=",")

# Look at a qq plot of the residuals for reviewer comments
  mod = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt)
  qqnorm(resid(mod), ylim=c(-4,4))


# Check autocorrelation of residuals
  mod = lme(log(Rate) ~ Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM + T + T2, data = fwmvmt, random = ~1|TagID)

# Get R2 for fit in glmm for reviewer comments
  source('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/rsquaredGLMM.R')
  modelR2s = rsquared.glmm(Mov.mods)
  write.table(cbind(MovNames, modelR2s), "MovementR2s.csv", row.names=FALSE,
     sep=",")

# Look at results of best model
  best.mov <- summary(Mov.mods[[as.numeric(row.names(MovResults)[1])]])
	best.mov

# Look at a qq plot of the residuals for reviewer comments
  # Do a check for reviewers to show that movement doesnt differ by tag type
  # Create a factor variable for tagtype
    Tag <- c()
	  for(i in 1:nrow(fwmvmt)){
	    if((fwmvmt$Org[i] == 'Wild') & fwmvmt$Year[i] == 2011){
	      Tag[i]  <- 0
	    } else {
        if(fwmvmt$Year[i] == 2005){
	        Tag[i] <- 0
	      } else {
          Tag[i] <- 1
	      }
      }
    }
  fwmvmt2 = cbind(fwmvmt, Tag)
  for(i in 1:nrow(fwmvmt2)){
  	if(fwmvmt2$Tag[i]==0){
  		fwmvmt2$TagMass[i]= 1.4
  	} else {
  		fwmvmt2$TagMass[i]=2.9
  	}
  }
  fwmvmt2$Ratio = scale(fwmvmt2$TagMass/fwmvmt2$Mass)
  mod2 = lmer(log(Rate) ~ (1|TagID)  + Dis + Dis2 + Org + Ph + Ph2 + ReachType + RKM + T + T2 + Ratio, data = fwmvmt2)
  qqnorm(resid(mod), ylim=c(-4,4))

# Make a dataframe out of the movement results
	Mres <- data.frame(best.mov$coefficients)
  Mres

# Plot effects of covariates on fish movement in the Penobscot
	dat <- originaldata
	# Effect of discharge
	  # New values for predictions
  	  midis <- min(fwmvmt$Dis)
	    madis <- max(fwmvmt$Dis)
	    ndis <- seq(midis, madis, 0.01)
	    rndis <- ndis*sd(dat$Dis) + mean(dat$Dis)
	  # Predicted values
	    X <- c(-1,0,1)
	    meandis <- list()
	    dis <- list()
	    for(i in 1:length(X)){
	      Mres <- Mres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(Mres)){
	    		  for(t in 1:2){
	            meandis[[i]] <-
	    	        (Mres[1,1] + X[i] * (1.96*Mres[1,2])) +
		            (Mres[2,1] + X[i] * (1.96*Mres[2,2])) * ndis +
		            (Mres[3,1] + X[i] * (1.96*Mres[3,2])) * (ndis^2) +
		            (Mres[4,1] + X[i] * (1.96*Mres[4,2])) +
		            (Mres[5,1] + X[i] * (1.96*Mres[5,2])) * median(fwmvmt$Ph) +
		            (Mres[6,1] + X[i] * (1.96*Mres[6,2])) * median(fwmvmt$Ph2) +
		            (Mres[7,1] + X[i] * (1.96*Mres[7,2])) +
		            (Mres[8,1] + X[i] * (1.96*Mres[8,2])) +
		            (Mres[9,1] + X[i] * (1.96*Mres[9,2])) * median(fwmvmt$RKM) +
		            (Mres[10,1] + X[i] * (1.96*Mres[10,2])) * median(fwmvmt$T) +
		            (Mres[11,1] + X[i] * (1.96*Mres[11,2])) * median(fwmvmt$T2)
	            dis[[i]] <- exp(meandis[[i]])
	          }
	       }
	    }

	# Effect of photoperiod
	  # New values for predictions
  	  miPh <- min(fwmvmt$Ph)
	    maPh <- max(fwmvmt$Ph)
	    nPh <- seq(miPh, maPh, 0.01)
	    nPh2 <- nPh^2
	    rnPh <- nPh*sd(dat$Ph) + mean(dat$Ph)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanPh <- list()
	    Ph <- list()
	    for(i in 1:length(X)){
	      Mres <- Mres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(Mres)){
	    		  for(t in 1:2){
	            meanPh[[i]] <-
	    	        (Mres[1,1] + X[i] * (1.96*Mres[1,2])) +
		            (Mres[2,1] + X[i] * (1.96*Mres[2,2])) * median(fwmvmt$Dis) +
		            (Mres[3,1] + X[i] * (1.96*Mres[3,2])) * median(fwmvmt$Dis2) +
		            (Mres[4,1] + X[i] * (1.96*Mres[4,2])) +
		            (Mres[5,1] + X[i] * (1.96*Mres[5,2])) * nPh +
		            (Mres[6,1] + X[i] * (1.96*Mres[6,2])) * nPh2 +
		            (Mres[7,1] + X[i] * (1.96*Mres[7,2])) +
		            (Mres[8,1] + X[i] * (1.96*Mres[8,2])) +
		            (Mres[9,1] + X[i] * (1.96*Mres[9,2])) * median(fwmvmt$RKM) +
		            (Mres[10,1] + X[i] * (1.96*Mres[10,2])) * median(fwmvmt$T) +
		            (Mres[11,1] + X[i] * (1.96*Mres[11,2])) * median(fwmvmt$T2)
	            Ph[[i]] <- exp(meanPh[[i]])
	          }
	       }
	    }

	# Effect of RKM
	  # New values for predictions
  	  miRKM <- min(fwmvmt$RKM)
	    maRKM <- max(fwmvmt$RKM)
	    nRKM <- seq(miRKM, maRKM, 0.01)
	    rnRKM <- nRKM*sd(dat$RKM) + mean(dat$RKM)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanRKM <- list()
	    RKM <- list()
	    for(i in 1:length(X)){
	      Mres <- Mres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(Mres)){
	    		  for(t in 1:2){
	            meanRKM[[i]] <-
	    	        (Mres[1,1] + X[i] * (1.96*Mres[1,2])) +
		            (Mres[2,1] + X[i] * (1.96*Mres[2,2])) * median(fwmvmt$Dis) +
		            (Mres[3,1] + X[i] * (1.96*Mres[3,2])) * median(fwmvmt$Dis2) +
		            (Mres[4,1] + X[i] * (1.96*Mres[4,2])) +
		            (Mres[5,1] + X[i] * (1.96*Mres[5,2])) * median(fwmvmt$Ph) +
		            (Mres[6,1] + X[i] * (1.96*Mres[6,2])) * median(fwmvmt$Ph2) +
		            (Mres[7,1] + X[i] * (1.96*Mres[7,2])) +
		            (Mres[8,1] + X[i] * (1.96*Mres[8,2])) +
		            (Mres[9,1] + X[i] * (1.96*Mres[9,2])) * nRKM +
		            (Mres[10,1] + X[i] * (1.96*Mres[10,2])) * median(fwmvmt$T) +
		            (Mres[11,1] + X[i] * (1.96*Mres[11,2])) * median(fwmvmt$T2)
	            RKM[[i]] <- exp(meanRKM[[i]])
	          }
	       }
	    }

	# Effect of temperature
	  # New values for predictions
  	  miT <- min(fwmvmt$T)
	    maT <- max(fwmvmt$T)
	    nT <- seq(miT, maT, 0.01)
	    nT2 <- nT^2
	    rnT <- nT*sd(dat$T) + mean(dat$T)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanT <- list()
	    T <- list()
	    for(i in 1:length(X)){
	      Mres <- Mres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(Mres)){
	    		  for(t in 1:2){
	            meanT[[i]] <-
	    	        (Mres[1,1] + X[i] * (1.96*Mres[1,2])) +
		            (Mres[2,1] + X[i] * (1.96*Mres[2,2])) * median(fwmvmt$Dis) +
		            (Mres[3,1] + X[i] * (1.96*Mres[3,2])) * median(fwmvmt$Dis2) +
		            (Mres[4,1] + X[i] * (1.96*Mres[4,2])) +
		            (Mres[5,1] + X[i] * (1.96*Mres[5,2])) * median(fwmvmt$Ph) +
		            (Mres[6,1] + X[i] * (1.96*Mres[6,2])) * median(fwmvmt$Ph2) +
		            (Mres[7,1] + X[i] * (1.96*Mres[7,2])) +
		            (Mres[8,1] + X[i] * (1.96*Mres[8,2])) +
		            (Mres[9,1] + X[i] * (1.96*Mres[9,2])) * median(fwmvmt$RKM) +
		            (Mres[10,1] + X[i] * (1.96*Mres[10,2])) * nT +
		            (Mres[11,1] + X[i] * (1.96*Mres[11,2])) * nT2
	            T[[i]] <- exp(meanT[[i]])
	          }
	       }
	    }

  # Select and/or create a .tiff file to which the plot will be written
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/Figure5.tiff',
  	  width=9000, height=8000, pointsize=18, compression="lzw", res=500)
	  par(mfrow=c(2,2), mar=c(6,7,1,0))
  # Discharge
	  plot(rndis, dis[[1]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rndis, dis[[2]], ylim=c(0,10), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rndis, dis[[3]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
		axis(side=1, at=seq(250, 1750, 250), as.character(seq(250, 1750, 250)), cex.axis=1.25)
	  axis(side=2, at=seq(0,10,2), as.character(seq(0,10, 2)), cex.axis=1.25, las=2)
	  mtext(side=2, line=4, cex=1.75, expression(paste("Movement rate (km" ,
		  plain('\u00b7'), 'h'^'-1',')', sep='')), adj=-2)
	  mtext(side=1, line=4, expression(paste('Discharge (m'^'3', plain('\u00b7'),
		  's'^'-1', ')', sep='')), cex=1.5)
	  text(x=250, y=9.5, '(a)', cex=2)
	# Photoperiod
		par(mar=c(6,5,1,3))
	  plot(rnPh, Ph[[1]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  par(new=TRUE)
	  plot(rnPh, Ph[[2]], ylim=c(0,10), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  par(new=TRUE)
	  plot(rnPh, Ph[[3]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  axis(side=2, at=seq(0,10,2), as.character(seq(0,10, 2)), cex.axis=1.25, las=2)
	  mtext(side=1, line=4, 'Photoperiod (hours)', cex=1.5)
		text(x=15.25, y=9.5, '(b)', cex=2)
	# Distance from ocean
		par(mar=c(6,7,1,0))
	  plot(rnRKM, RKM[[1]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xlim=c(187,43.5), xaxt='n')
	  par(new=TRUE)
	  plot(rnRKM, RKM[[2]], ylim=c(0,10), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xlim=c(187,43.5), xaxt='n')
	  par(new=TRUE)
	  plot(rnRKM, RKM[[3]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xlim=c(187,43.5), xaxt='n')
	  axis(side=1, at=seq(175, 50, -25), as.character(seq(175, 50, -25)), cex.axis=1.25)
	  axis(side=2, at=seq(0,10,2), as.character(seq(0,10, 2)), cex.axis=1.25, las=2)
	  mtext(side=1, line=4, 'Distance from ocean (km)', cex=1.5)
	  text(x=183, y=9.5, '(c)', cex=2)
	# Temperature
		par(mar=c(6,5,1,3))
	  plot(rnT, T[[1]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  par(new=TRUE)
	  plot(rnT, T[[2]], ylim=c(0,10), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  par(new=TRUE)
	  plot(rnT, T[[3]], ylim=c(0,10), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1.25)
	  axis(side=2, at=seq(0,10,2), as.character(seq(0,10, 2)), cex.axis=1.25, las=2)
	  mtext(side=1, line=4, expression(paste('Temperature (',plain('\u00b0'),'C)')), cex=1.5)
	  text(x=21.5, y=9.5, '(d)', cex=2)
	# Turn off graphics device to save the file
	  dev.off()

# Violin Plots
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/Figure2.tiff',
  	  width=8000, height=6500, pointsize=18, compression="lzw", res=500)
	  par(mar=c(6,6,6,1))
  	plot(0:1,0:1,type='n',xlim=c(0.5,6.5),ylim=c(0,7), axes=FALSE, ann=FALSE)
    vioplot(add=TRUE,
  	  preds[fwmvmt$ReachType=='D' & fwmvmt$Org=='Hatchery'],
  	  preds[fwmvmt$ReachType=='F' & fwmvmt$Org=='Hatchery'],
  	  preds[fwmvmt$ReachType=='H' & fwmvmt$Org=='Hatchery'],
  	  preds[fwmvmt$ReachType=='D' & fwmvmt$Org=='Wild'],
  	  preds[fwmvmt$ReachType=='F' & fwmvmt$Org=='Wild'],
  	  preds[fwmvmt$ReachType=='H' & fwmvmt$Org=='Wild'],
  	  col='gray87', colMed='yellow', rectCol='brown', border='white', wex=1.1,
    	  names=c(rep('', 6)), h=.35)
	    abline(v=3.5)
    box()
	  boxplot(preds~as.factor(fwmvmt$ReachType)*as.factor(fwmvmt$Org),
	    col=c(rep('gray50',3), rep('gray50',3)), ylim=c(0,8), ylab='',
	    yaxt='n', xaxt='n', xlab='', notch=TRUE, add=TRUE,
	    pars = list(boxwex = 0.12, staplewex = 0.5, outwex = 0.5), outline=FALSE
		)
	  axis(side=1, at=seq(1,6,1), rep(c('Dam', 'Free flowing', 'Head pond'),2),
	  	cex.axis=1, cex.axis=1.25)
	  axis(side=2, at=seq(0,8,2), as.character(seq(0,8,2)), las=2, cex.axis=1.25)
	  mtext(side=1, 'Reach type', cex=2, line=3.5)
	  mtext(side=2, line=3.5, cex=2, expression(paste("Movement rate (km " ,
  	  plain('\u00b7'), 'h'^'-1',')', sep='')))
	  mtext(side=3, at=c(2,5), c('Hatchery', 'Wild'), cex=1.25, line=.5)
	  mtext(side=3, at=c(3.5), c('Rearing history'), cex=2, line=3)
# Turn off graphics device to save the file
	dev.off()

# Run tests and plot data for the difference in movement rate before and after
  # Read in the data again
  	load(file= 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis/FWMovementRatesForAnalysis.rda') # Its called fwmvmt
  	dat = fwmvmt
# Great Works and Veazie dam removals
  # Get the data split out
    gdubs <- dat[substr(dat$Reach,start=1,stop=4)=='GWHP' |
  		substr(dat$Reach,start=1,stop=4)=='VZHP',]
    postGW <- gdubs$Rate[gdubs$Year==2014]
    preGW <- gdubs$Rate[gdubs$Year!=2014]
  # Run the test
    wilcox.test(postGW, preGW)
  # Plot the results
    # Select and/or create a .tiff file to which the plot will be written
  # Select and/or create a .tiff file to which the plot will be written
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/Figure4.tiff',
  	  width=7000, height=7000, pointsize=18, compression="lzw", res=500)
    par(mfrow=c(2,1), oma=c(1,4,1.5,0))
	  par(mar=c(2,1,1,1))
  	plot(0:1,0:1,type='n',xlim=c(0.5,8.5),ylim=c(0,10), axes=FALSE, ann=FALSE)
    vioplot(add=TRUE,
  	  gdubs$Rate[gdubs$Year==2005],
  	  gdubs$Rate[gdubs$Year==2006],
  	  gdubs$Rate[gdubs$Year==2009],
  	  gdubs$Rate[gdubs$Year==2010],
  	  gdubs$Rate[gdubs$Year==2011],
  	  gdubs$Rate[gdubs$Year==2012],
    	gdubs$Rate[gdubs$Year==2013],
    	#gdubs$Rate[gdubs$Year==2014],
  	  col=c(rep('gray75',6),'gray87'), colMed='black', rectCol='gray50',
    	border='white', wex=1, drawRect=FALSE, names=c(rep('', 6)))
  	vioplot(add=TRUE, at=8, gdubs$Rate[gdubs$Year==2014],
  	  col=c('gray87'), border='white', wex=1.1, names=c(rep('', 1)),
  	  drawRect=FALSE)
    boxplot(gdubs$Rate~gdubs$Year, yaxt='n', col=c(rep('gray50',7), 'gray50'),
	    cex.axis=1.5, ylim=c(0,10), outline=FALSE, add=TRUE, wex=.3, notch=TRUE,
    	pars=list(boxwex=.15), xaxt='n', lty=1)
    axis(side=2, at=seq(0,10,2), as.character(seq(0,10,2)), las=2,
      	cex.axis=1.5)
    mtext(side=2, expression(paste("Movement rate (km " , plain('\u00b7'),
	     'h'^'-1',')', sep='')), line=2.5, cex=2, adj=-10)
    #mtext(side=1, 'Year', line=3.5, cex=2)
    abline(v=7.5, lty=2, lwd=4)
	  text(x=.5, y=9.5, '(a)', cex=2)
  	b <- boxplot(gdubs$Rate~gdubs$Year, plot=0)
  	axis(side=3, at=c(seq(1,8)), as.character(paste('(',b$n,')', sep='')),
  	  cex.axis=1, tck=FALSE, padj=1.25)
# Run tests and plot data for the difference in movement rate before and after
# Orono and Stillwater Dams received new powerhouses
  # Get the data split out
    sdubs <- dat[substr(dat$Reach,start=1,stop=4)=='STHP' |
  		substr(dat$Reach,start=1,stop=4)=='ORHP',]
    postSW <- sdubs$Rate[sdubs$Year==2014]
    preSW <- sdubs$Rate[sdubs$Year!=2014]
  # Run the test
    wilcox.test(postSW, preSW)
  # Plot the results
  	plot(0:1,0:1,type='n',xlim=c(0.5,8.5),ylim=c(0,10), axes=FALSE, ann=FALSE)
    vioplot(add=TRUE,
  	  sdubs$Rate[sdubs$Year==2005],
  	  sdubs$Rate[sdubs$Year==2006],
  	  sdubs$Rate[sdubs$Year==2009],
  	  sdubs$Rate[sdubs$Year==2010],
  	  sdubs$Rate[sdubs$Year==2011],
  	  sdubs$Rate[sdubs$Year==2012],
    	sdubs$Rate[sdubs$Year==2013],
    	  #sdubs$Rate[sdubs$Year==2014],
  	  col=c(rep('gray75',6),'gray87'), colMed='black', rectCol='gray50',
    	border='white', wex=c(1), drawRect=FALSE, names=c(rep('', 6)))
  	vioplot(add=TRUE, at=8, sdubs$Rate[sdubs$Year==2014],
  	  col=c('gray87'), border='white', wex=1.1, names=c(rep('', 1)),
  	  drawRect=FALSE)
    boxplot(sdubs$Rate~sdubs$Year, yaxt='n', col=c(rep('gray50',7), 'gray50'),
	    cex.axis=1.5, ylim=c(0,10), outline=FALSE, add=TRUE, wex=.2, notch=TRUE,
    	pars=list(boxwex=.15), lty=1)
    axis(side=2, at=seq(0,10,2), as.character(seq(0,10,2)), las=2,
      cex.axis=1.5)
    mtext(side=1, 'Year', line=4, cex=2)
    abline(v=7.5, lty=2, lwd=4)
	  text(x=.5, y=9.5, '(b)', cex=2)
  	b <- boxplot(sdubs$Rate~sdubs$Year, plot=0)
  	axis(side=3, at=c(seq(1,8)), as.character(paste('(',b$n,')', sep='')),
  	  cex.axis=1, tck=FALSE, padj=1.25)
  # Turn off graphics device to save the file
	  dev.off()
################################################################################

################################################################################
# Data for timimg of hatchery and wild movement in the Penobscot River
#
# INPUT FILENAME(S): FWMovementRatesForAnalysis.rda,
#                    FullFWMovements.rda, GodCapturesReady.csv
#                    GodModelCHWithColnames.rda, GodlyCovariates.csv
#
# MY FUNCTIONS:
# NAMED OUTPUT:      wildtime (df), hatchtime (df)
# OUTPUT FILENAME:   fwTiming.txt, WildFishTimingData.rda,
#                    HatcheryFishTimingData.rda
################################################################################
# Install and load packages
  #install.packages('reshape2')
  require(reshape2)

# Read in the data for analysis
	setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  load(file= 'FWMovementRatesForAnalysis.rda') # Its called fwmvmt

# Set the directory and read in the data file
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
	load(file='FullFWMovements.rda')
	# Add RKM to the SW column
	  names(fwmove)[ncol(fwmove)] <- "43.5_Salt"

# Read in the capture history data created in the GodModelCaptureHistory code
  GD <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodCapturesReady.csv', header=F)
  load(file='C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodModelCHWithColnames.rda')

# Read in covariates for the god model
  covs <- read.csv('C:/Users/Dan/Desktop/Manuscripts/GodModel/Analysis/GodlyCovariates.csv')

# Remove fish released at Verona and Brewer (estuary releases)
  GD <- GD[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]
  covs <- covs[covs$ReleaseSite!='Verona' & covs$ReleaseSite!='Brewer',]

# Remove fish that were never located
	covs <- covs[rowSums(GD)!=0, ]
  GD <- GD[rowSums(GD)!=0, ]

# Remove fish that were never located from the movement data, too
	mvmt <- fwmove[fwmove$TagID %in% covs$TagID,]
	ch9 <- ch9[ch9$TagID %in% covs$TagID,]

# Create a stacked data frame out of the date/time matrix and covariates
	file.remove("fwTiming.txt")
  pb <- txtProgressBar(min = 0, max = length(unique(mvmt$TagID)), style = 3)
  for(i in 1:length(unique(mvmt$TagID))){
	  Sys.sleep(0.0001) # Sys time out for pb printing
	  FishRow <- unlist(c(mvmt[i , 18:ncol(mvmt)]), use.names=FALSE)
    FishD <- mainStillmoves[c(rep(i,(length(FishRow)))), 1:17]
	  ListSites <- colsplit(names(mvmt[i , 18:ncol(mvmt)]), split="_", names=c('RKM', 'Site'))[,2]
	  RKM <- colsplit(names(mvmt[i , 18:ncol(mvmt)]), split="_", names=c('RKM', 'Site'))[,1]
	  d <- data.frame(FishD, FishRow, ListSites,RKM)
	  # Write the data to a file, append all fish after first to the end
      write.table(d, "fwTiming.txt", row.names=FALSE, col.names=FALSE,
        append=TRUE, sep=",")
	  setTxtProgressBar(pb, i) # Update progress meter, move on to the next fish
  }

# Read in the fw timing data
  fwtime <- read.csv('fwTiming.txt', header=FALSE)
	# Take out crap quadratix, will be re-calculated after scaling linear terms
	  fwtime <- fwtime[ , c(1:9,11,13,15,18:ncol(fwtime))]

	# Give the data column names
	# Create easily manageable names for the data
	  names(fwtime) <- c('TagID', 'FL', 'Mass', 'ATP', 'RelSite', 'RelDate',
	  	'Org', 'Year', 'Dis', 'Ph', 'ATU', 'T', 'MovDate', 'Site', 'RKM')

# Create variable to hold release rkm
	# Get RKM for each of the release sites so it can be added to 'Releases'
		# Convert the vector data type for ReleaseSite in ch to chr from factor
	    fwtime$RelSite <- as.character(fwtime$RelSite)

	    fwtime$RelSite[fwtime$RelSite=='EastBranch'] <- 'East Branch'

    # Get the unique values of release site
      ReleaseSites <- c(sort(unique(as.character(fwtime$RelSite))))

    # Make a vector of corresponding rkms for release sites
      ReleaseRKMs <- c(187, 162, 99, 144, 142,  92.3, 63, 149)

  # Create a column in the capture history to hold release RKM and fill it in
	  fwtime$RelKM <- 0 # Empty column to hold release rkms
	  pb <- txtProgressBar(min = 0, max = length(ReleaseSites), style = 3)
	  for(i in 1:length(ReleaseSites)){
	    Sys.sleep(0.0001) # Sys time out for pb printing
      for(t in 1:nrow(fwtime)){
	      if(fwtime$RelSite[t]==ReleaseSites[i]){
	      	fwtime$RelKM[t] <- ReleaseRKMs[i]
	      } else {
	      	next;
	      } # else
	    } # t
	  	setTxtProgressBar(pb, i) # Update progress meter, move on to next site
    } # i

# GET TIMING DATA FROM HATCHERY SMOLTS
# THIS WILL BE TIME BETWEEN RELEASE AND FIRST RELOCATION 5-25 KM DONSTREAM
	 # Remove the zero times at locations from the dataframe because these
	 # represent locations at which fish were never observed.
	   fwtime <- fwtime[fwtime$MovDate!=0,]
	   nrow(fwtime)

	# Create a column to hold difference between release and detections for each
	# fish.  Add the column to the df for later use
	  diff <- fwtime$RelKM-fwtime$RKM
	  fwtime$diff <- diff

	# Create a vector of distances from release to first occasion > 5 km away
	# for each fish that was ever relocated
    Init <- 0
    pb <- txtProgressBar(min = 0, max = length(unique(fwtime$TagID)), style = 3)
    for(i in 1:length(unique(fwtime$TagID))){
	    Sys.sleep(0.0001) # Sys time out for pb printing
      Init[i] <- min(diff[diff>5 & diff<10 &
      	fwtime$TagID==unique(fwtime$TagID[i])])
      setTxtProgressBar(pb, i) # Update progress meter, move on to the next fish
    }	# i
	  close(pb)
    Init # Visual data check

  # Get the length of the initial reach that is between 5 & 10 km for each fish
	# and add it to the fwtime df to get just the data we want (soon).
    pb <- txtProgressBar(min = 0, max = nrow(fwtime), style = 3)
	  for(t in 1:nrow(fwtime)){
	    for(i in 1:length(unique(fwtime$TagID))){
	  	  Sys.sleep(0.0001) # Sys time out for pb printing
  	    if(fwtime$TagID[t]==unique(fwtime$TagID[i])){
	  		  fwtime$Init[t] <- Init[i]
  	    } else {
  	  	  next;
  	    }
	  	} # i
      setTxtProgressBar(pb, t) # Update progress meter, move on to the next fish
	  } # t
    close(pb)

  # Get the length of the initial reach that is between 5 & 25 km for each fish
	# and add it to the fwtime df to get just the data we want.
	  test <- data.frame(unique(fwtime$TagID), Init) # Make a df of init and Tag
	  names(test) <- c('TagID', 'Init') # Give it names
	  fwtime <- merge(fwtime, test, by='TagID') # Merge the init object with df
	  fwtime$diff <- fwtime$RelKM-fwtime$RKM # Calculate diff...again

	# Keep only hatchery fish that were located 5-25 km after release
	  hatchtime <- fwtime[fwtime$Org=='Hatchery' & fwtime$diff>5 &
	  	fwtime$diff<25, ]

	# Remove anything other than the initial relocation
	  hatchtime <- hatchtime[!duplicated(hatchtime$TagID), ]

	# Put re-location D/T back into POSIXct format
    hatchtime$MovDate <- DTAgain(hatchtime$MovDate)

	# Calculate time from release to relocation
	  hatchtime$Time <- as.numeric(
	  	difftime(hatchtime$MovDate,hatchtime$RelDate, units='hours'))

	# Save the hatchery timing data as an r-data file
    save(hatchtime, file= 'HatcheryFishTimingData.rda')


# NOW DO CAPTURE DATE FOR WILD FISH
# Organize the timing data for wild fish
	wildtime <- fwmove[fwmove$Origin=='Wild' , c(1:9, 11,13,15)]
	wildtime <- wildtime[!duplicated(wildtime$TagID),]
	wildtime <- wildtime[wildtime$ReleaseSite!='Brewer',]

# Create easily manageable names for the data
	names(wildtime) <- c('TagID', 'FL', 'Mass', 'ATP', 'RelSite', 'RelDate',
	  'Org', 'Year', 'Dis', 'Ph', 'ATU','T')

  # Create a column in the capture history to hold release RKM and fill it in
	  wildtime$RelKM <- 0 # Empty column to hold release rkms
	  pb <- txtProgressBar(min = 0, max = length(ReleaseSites), style = 3)
	  for(i in 1:length(ReleaseSites)){
	    Sys.sleep(0.0001) # Sys time out for pb printing
      for(t in 1:nrow(wildtime)){
	      if(wildtime$RelSite[t]==ReleaseSites[i]){
	      	wildtime$RelKM[t] <- ReleaseRKMs[i]
	      } else {
	      	next;
	      } # else
	    } # t
	  	setTxtProgressBar(pb, i) # Update progress meter, move on to next site
    } # i

# Write these data out to a file
  save(wildtime, file= 'WildFishTimingData.rda')
################################################################################

################################################################################
# Run the analysis for timing of wild smolt capture
#
# INPUT FILENAME(S): WildFishTimingData.rda (wildtime)
#
# MY FUNCTIONS:
# NAMED OUTPUT:      tResults, best.t, tres
# OUTPUT FILENAME:   wildInitModelSelection.csv
#                    Figure2.tif,
################################################################################
# Install and load packages
	#install.packages('AICcmodavg')
	require(AICcmodavg)

# Read in the data for analysis
	setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')

# Write these data out to a file
  load('WildFishTimingData.rda')

# Create standardized variables
	originaldata <- wildtime
  wildtime[ , c(2:4, 9:13)] <- (apply(wildtime[ , c(2:4, 9:13)], 2, scale))

# Create quadratix
	wildtime$Dis2 <- wildtime$Dis^2
	wildtime$Ph2 <- wildtime$Ph^2
	wildtime$ATU2 <- wildtime$ATU^2
	wildtime$T2 <- wildtime$T^2

# Create an ordinal date response variable
	wildtime$Init <- yday(wildtime$RelDate)

# Create a list of candidate models for wild smolt timing
  fwTmods = list()
    fwTmods[[1]] = glm(Init ~ ATP, data = wildtime, family='poisson')
    fwTmods[[2]] = glm(Init ~ ATU, data = wildtime, family='poisson')
    fwTmods[[3]] = glm(Init ~ T, data = wildtime, family='poisson')
    fwTmods[[4]] = glm(Init ~ Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[5]] = glm(Init ~ RelKM, data = wildtime, family='poisson')
    fwTmods[[6]] = glm(Init ~ ATP + ATU, data = wildtime, family='poisson')
    fwTmods[[7]] = glm(Init ~ ATP + T, data = wildtime, family='poisson')
    fwTmods[[8]] = glm(Init ~ ATP + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[9]] = glm(Init ~ ATP + RelKM, data = wildtime, family='poisson')
    fwTmods[[10]] = glm(Init ~ ATP + ATU + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[11]] = glm(Init ~ ATP + ATU + RelKM, data = wildtime, family='poisson')
    fwTmods[[12]] = glm(Init ~ ATP + T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[13]] = glm(Init ~ ATP + T + RelKM, data = wildtime, family='poisson')
    fwTmods[[14]] = glm(Init ~ ATP + ATU + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[15]] = glm(Init ~ ATP + T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[16]] = glm(Init ~ ATU + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[17]] = glm(Init ~ ATU + RelKM, data = wildtime, family='poisson')
    fwTmods[[18]] = glm(Init ~ ATU + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[19]] = glm(Init ~ T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[20]] = glm(Init ~ T + RelKM, data = wildtime, family='poisson')
    fwTmods[[21]] = glm(Init ~ T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[22]] = glm(Init ~ Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[23]] = glm(Init ~ ATU + T, data = wildtime, family='poisson')
    fwTmods[[24]] = glm(Init ~ ATU + T, data = wildtime, family='poisson')
    fwTmods[[25]] = glm(Init ~ ATP + ATU + T, data = wildtime, family='poisson')
    fwTmods[[26]] = glm(Init ~ ATP + ATU + T, data = wildtime, family='poisson')
    fwTmods[[27]] = glm(Init ~ ATP + ATU + T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[28]] = glm(Init ~ ATP + ATU + T + RelKM, data = wildtime, family='poisson')
    fwTmods[[29]] = glm(Init ~ ATP + ATU + T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[30]] = glm(Init ~ ATP + ATU + T + RelKM, data = wildtime, family='poisson')
    fwTmods[[31]] = glm(Init ~ ATP + ATU + T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[32]] = glm(Init ~ ATP + ATU + T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[33]] = glm(Init ~ ATU + T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[34]] = glm(Init ~ ATU + T + RelKM, data = wildtime, family='poisson')
    fwTmods[[35]] = glm(Init ~ ATU + T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')
    fwTmods[[36]] = glm(Init ~ ATU + T + Dis + Dis2, data = wildtime, family='poisson')
    fwTmods[[37]] = glm(Init ~ ATU + T + RelKM, data = wildtime, family='poisson')
    fwTmods[[38]] = glm(Init ~ ATU + T + Dis + Dis2 + RelKM, data = wildtime, family='poisson')

#MODELNAMES
  TNames <- c()
    TNames[1]  <- 'ATP'
    TNames[2]  <- 'ATU'
    TNames[3]  <- 'T'
    TNames[4]  <- 'Dis + Dis2'
    TNames[5]  <- 'RelKM'
    TNames[6]  <- 'ATP + ATU'
    TNames[7]  <- 'ATP + T'
    TNames[8]  <- 'ATP + Dis + Dis2'
    TNames[9]  <- 'ATP + RelKM'
    TNames[10]  <- 'ATP + ATU + Dis + Dis2'
    TNames[11]  <- 'ATP + ATU + RelKM'
    TNames[12]  <- 'ATP + T + Dis + Dis2'
    TNames[13]  <- 'ATP + T + RelKM'
    TNames[14]  <- 'ATP + ATU + Dis + Dis2 + RelKM'
    TNames[15]  <- 'ATP + T + Dis + Dis2 + RelKM'
    TNames[16]  <- 'ATU + Dis + Dis2'
    TNames[17]  <- 'ATU + RelKM'
    TNames[18]  <- 'ATU + Dis + Dis2 + RelKM'
    TNames[19]  <- 'T + Dis + Dis2'
    TNames[20]  <- 'T + RelKM'
    TNames[21]  <- 'T + Dis + Dis2 + RelKM'
    TNames[22]  <- 'Dis + Dis2 + RelKM'
    TNames[23]  <- 'ATU + T'
    TNames[24]  <- 'ATU + T'
    TNames[25]  <- 'ATP + ATU + T'
    TNames[26]  <- 'ATP + ATU + T'
    TNames[27]  <- 'ATP + ATU + T + Dis + Dis2'
    TNames[28]  <- 'ATP + ATU + T + RelKM'
    TNames[29]  <- 'ATP + ATU + T + Dis + Dis2'
    TNames[30]  <- 'ATP + ATU + T + RelKM'
    TNames[31]  <- 'ATP + ATU + T + Dis + Dis2 + RelKM'
    TNames[32]  <- 'ATP + ATU + T + Dis + Dis2 + RelKM'
    TNames[33]  <- 'ATU + T + Dis + Dis2'
    TNames[34]  <- 'ATU + T + RelKM'
    TNames[35]  <- 'ATU + T + Dis + Dis2 + RelKM'
    TNames[36]  <- 'ATU + T + Dis + Dis2'
    TNames[37]  <- 'ATU + T + RelKM'
    TNames[38]  <- 'ATU + T + Dis + Dis2 + RelKM'

# Create a model-selection table for the wild timing models
  aictab(fwTmods, TNames)

# Create a model-selection table
  tResults <- aictab(fwTmods, TNames)

# Look at model-selection results
	tResults

# Save the model-selection table to a file
   setwd("C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables")
   write.table(tResults, "wildInitModelSelection.csv", row.names=FALSE,
     sep=",")

# Look at results of best model
  best.t <- summary(fwTmods[[as.numeric(row.names(tResults)[1])]])
	best.t

# Make a dataframe out of the movement results
	tres <- data.frame(best.t$coefficients)
  tres

# Plot effects of covariates on fish movement in the Penobscot
	dat <- originaldata
		# Effect of ATU on timing of wild smolt capture
	  # New values for predictions
  	  miATU <- min(wildtime$ATU)
	    maATU <- max(wildtime$ATU)
	    nATU  <- seq(miATU, maATU, 0.01)
	    rnATU <- nATU*sd(dat$ATU) + mean(dat$ATU)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanATU <- list()
	    ATU <- list()
	    for(i in 1:length(X)){
	      tres <- tres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(tres)){
	    		  for(t in 1:2){
	            meanATU[[i]] <-
	    	        (tres[1,1] + X[i] * (1.96*tres[1,2])) +
		            (tres[2,1] + X[i] * (1.96*tres[2,2])) * nATU +
		            (tres[3,1] + X[i] * (1.96*tres[3,2])) * median(wildtime$Dis) +
		            (tres[4,1] + X[i] * (1.96*tres[4,2])) * median(wildtime$Dis2)
	            ATU[[i]] <- exp(meanATU[[i]])
	          }
	       }
	    }

	# Effect of discharge
	  # New values for predictions
  	  midis <- min(wildtime$Dis)
	    madis <- max(wildtime$Dis)
	    ndis <- seq(midis, madis, 0.01)
	    rndis <- ndis*sd(dat$Dis) + mean(dat$Dis)
	  # Predicted values
	    X <- c(-1,0,1)
	    meandis <- list()
	    dis <- list()
	    for(i in 1:length(X)){
	      tres <- tres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(tres)){
	    		  for(t in 1:2){
	            meandis[[i]] <-
	    	        (tres[1,1] + X[i] * (1.96*tres[1,2])) +
		            (tres[2,1] + X[i] * (1.96*tres[2,2])) * median(wildtime$ATU) +
		            (tres[3,1] + X[i] * (1.96*tres[3,2])) * ndis +
		            (tres[4,1] + X[i] * (1.96*tres[4,2])) * (ndis^2)
	            dis[[i]] <- exp(meandis[[i]])
	          }
	       }
	    }

	# Select and/or create a .tiff file to which the plot will be written
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/Figure2.tiff',
  	  width=10000, height=5000, pointsize=18, compression="lzw", res=500)
	# Set up plotting window
	  par(mfrow=c(1,2), mar=c(6,7,1,0))
  # ATU
	  plot(rnATU, ATU[[1]], ylim=c(100,155), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rnATU, ATU[[2]], ylim=c(100,155), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rnATU, ATU[[3]], ylim=c(100,155), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
		axis(side=1, at=seq(250, 550, 50), as.character(seq(250, 550, 50)), cex.axis=1.25)
	  axis(side=2, at=seq(100,150, 10), as.character(seq(100,150, 10)), cex.axis=1.25, las=2)
	  mtext(side=2, line=4, cex=1.75, 'Date of capture')
	  mtext(side=1, line=4, 'Accumulated thermal units (ATU)', cex=1.5)
	  text(x=250, y=9.5, '(a)', cex=2)
  # Discharge
	  par(mar=c(6,5,1,2))
	  plot(rndis, dis[[1]], ylim=c(100,155), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rndis, dis[[2]], ylim=c(100,155), type='l', col='black', lty=1, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
	  par(new=TRUE)
	  plot(rndis, dis[[3]], ylim=c(100,155), type='l', col='gray', lty=2, lwd=3,
		  ylab='', xlab='', yaxt='n', cex.axis=1, xaxt='n')
		axis(side=1, at=seq(250, 1750, 250), as.character(seq(250, 1750, 250)), cex.axis=1.25)
	  axis(side=2, at=seq(100,150, 10), as.character(seq(100,150, 10)), cex.axis=1.25, las=2)
	  mtext(side=1, line=4, expression(paste('Discharge (m'^'3', plain('\u00b7'),
		  's'^'-1', ')', sep='')), cex=1.5)
	  text(x=250, y=9.5, '(a)', cex=2)
	# Turn off graphics device
	  dev.off()
################################################################################

################################################################################
# Run the analysis for time to initiation of migratory behavior by hatchery-
# reared smolts in the Penobscot River
#
# INPUT FILENAME(S): HatcheryFishTimingData.rda (wildtime)
#
# MY FUNCTIONS:
# NAMED OUTPUT:      thResults, best.ht, htres
# OUTPUT FILENAME:   hatchInitModelSelection.csv
#                    Figure3.tif,
################################################################################
# Install and load packages
	#install.packages('AICcmodavg')
	require(AICcmodavg)
	require(geosphere)

# Set the directory and read in the data file
  setwd('C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Analysis')
  load('HatcheryFishTimingData.rda')	# Dataframe is called hatchtime

# Create standardized variables
	originaldata <- hatchtime

  hatchtime[ , c(2:4, 9:12, 15:16, 18)] <-
		(apply(hatchtime[ , c(2:4, 9:12, 15:16, 18)] , 2, scale))

# Create quadratix
	hatchtime$Dis2 <- hatchtime$Dis^2
	hatchtime$Ph2 <- hatchtime$Ph^2
	hatchtime$ATU2 <- hatchtime$ATU^2
	hatchtime$T2 <- hatchtime$T^2

# Create a model list
  hatchmods = list()
   hatchmods[[1]] = glm(log(Time) ~ diff  + ATP, data = hatchtime)
   hatchmods[[2]] = glm(log(Time) ~ diff  + ATU, data = hatchtime)
   hatchmods[[3]] = glm(log(Time) ~ diff  + ATU + ATU2, data = hatchtime)
   hatchmods[[4]] = glm(log(Time) ~ diff  + Dis, data = hatchtime)
   hatchmods[[5]] = glm(log(Time) ~ diff  + Dis + Dis2, data = hatchtime)
   hatchmods[[6]] = glm(log(Time) ~ diff  + FL, data = hatchtime)
   hatchmods[[7]] = glm(log(Time) ~ diff  + Ph, data = hatchtime)
   hatchmods[[8]] = glm(log(Time) ~ diff  + Ph + Ph2, data = hatchtime)
   hatchmods[[9]] = glm(log(Time) ~ diff  + RelKM, data = hatchtime)
   hatchmods[[10]] = glm(log(Time) ~ diff  + T, data = hatchtime)
   hatchmods[[11]] = glm(log(Time) ~ diff  + T + T2, data = hatchtime)
   hatchmods[[12]] = glm(log(Time) ~ diff  + ATP + ATU, data = hatchtime)
   hatchmods[[13]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2, data = hatchtime)
   hatchmods[[14]] = glm(log(Time) ~ diff  + ATP + Dis, data = hatchtime)
   hatchmods[[15]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2, data = hatchtime)
   hatchmods[[16]] = glm(log(Time) ~ diff  + ATP + Ph, data = hatchtime)
   hatchmods[[17]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2, data = hatchtime)
   hatchmods[[18]] = glm(log(Time) ~ diff  + ATP + RelKM, data = hatchtime)
   hatchmods[[19]] = glm(log(Time) ~ diff  + ATP + T, data = hatchtime)
   hatchmods[[20]] = glm(log(Time) ~ diff  + ATP + T + T2, data = hatchtime)
   hatchmods[[21]] = glm(log(Time) ~ diff  + ATU + FL, data = hatchtime)
   hatchmods[[22]] = glm(log(Time) ~ diff  + ATU + ATU2 + FL, data = hatchtime)
   hatchmods[[23]] = glm(log(Time) ~ diff  + Dis + FL, data = hatchtime)
   hatchmods[[24]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL, data = hatchtime)
   hatchmods[[25]] = glm(log(Time) ~ diff  + FL + Ph, data = hatchtime)
   hatchmods[[26]] = glm(log(Time) ~ diff  + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[27]] = glm(log(Time) ~ diff  + FL + RelKM, data = hatchtime)
   hatchmods[[28]] = glm(log(Time) ~ diff  + FL + T, data = hatchtime)
   hatchmods[[29]] = glm(log(Time) ~ diff  + FL + T + T2, data = hatchtime)
   hatchmods[[30]] = glm(log(Time) ~ diff  + ATU + Dis, data = hatchtime)
   hatchmods[[31]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2, data = hatchtime)
   hatchmods[[32]] = glm(log(Time) ~ diff  + ATU + Ph, data = hatchtime)
   hatchmods[[33]] = glm(log(Time) ~ diff  + ATU + Ph + Ph2, data = hatchtime)
   hatchmods[[34]] = glm(log(Time) ~ diff  + ATU + RelKM, data = hatchtime)
   hatchmods[[35]] = glm(log(Time) ~ diff  + ATU + T, data = hatchtime)
   hatchmods[[36]] = glm(log(Time) ~ diff  + ATU + T + T2, data = hatchtime)
   hatchmods[[37]] = glm(log(Time) ~ diff  + ATU, data = hatchtime)
   hatchmods[[38]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis, data = hatchtime)
   hatchmods[[39]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2, data = hatchtime)
   hatchmods[[40]] = glm(log(Time) ~ diff  + ATU + ATU2 + Ph, data = hatchtime)
   hatchmods[[41]] = glm(log(Time) ~ diff  + ATU + ATU2 + Ph + Ph2, data = hatchtime)
   hatchmods[[42]] = glm(log(Time) ~ diff  + ATU + ATU2 + RelKM, data = hatchtime)
   hatchmods[[43]] = glm(log(Time) ~ diff  + ATU + ATU2 + T, data = hatchtime)
   hatchmods[[44]] = glm(log(Time) ~ diff  + ATU + ATU2 + T + T2, data = hatchtime)
   hatchmods[[45]] = glm(log(Time) ~ diff  + ATU + ATU2, data = hatchtime)
   hatchmods[[46]] = glm(log(Time) ~ diff  + Dis + Ph, data = hatchtime)
   hatchmods[[47]] = glm(log(Time) ~ diff  + Dis + Ph + Ph2, data = hatchtime)
   hatchmods[[48]] = glm(log(Time) ~ diff  + Dis + RelKM, data = hatchtime)
   hatchmods[[49]] = glm(log(Time) ~ diff  + Dis + T + T2, data = hatchtime)
   hatchmods[[50]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[51]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[52]] = glm(log(Time) ~ diff  + Dis + Dis2 + RelKM, data = hatchtime)
   hatchmods[[53]] = glm(log(Time) ~ diff  + Dis + Dis2 + T + T2, data = hatchtime)
   hatchmods[[54]] = glm(log(Time) ~ diff  + Ph + RelKM, data = hatchtime)
   hatchmods[[55]] = glm(log(Time) ~ diff  + Ph + T + T2, data = hatchtime)
   hatchmods[[56]] = glm(log(Time) ~ diff  + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[57]] = glm(log(Time) ~ diff  + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[58]] = glm(log(Time) ~ diff  + ATP + ATU + Dis, data = hatchtime)
   hatchmods[[59]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2, data = hatchtime)
   hatchmods[[60]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2, data = hatchtime)
   hatchmods[[61]] = glm(log(Time) ~ diff  + ATP + ATU + Ph, data = hatchtime)
   hatchmods[[62]] = glm(log(Time) ~ diff  + ATP + ATU + Ph + Ph2, data = hatchtime)
   hatchmods[[63]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Ph, data = hatchtime)
   hatchmods[[64]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Ph + Ph2, data = hatchtime)
   hatchmods[[65]] = glm(log(Time) ~ diff  + ATP + ATU + RelKM, data = hatchtime)
   hatchmods[[66]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + RelKM, data = hatchtime)
   hatchmods[[67]] = glm(log(Time) ~ diff  + ATP + ATU + T, data = hatchtime)
   hatchmods[[68]] = glm(log(Time) ~ diff  + ATP + ATU + T + T2, data = hatchtime)
   hatchmods[[69]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + T + T2, data = hatchtime)
   hatchmods[[70]] = glm(log(Time) ~ diff  + ATP + Dis + Ph, data = hatchtime)
   hatchmods[[71]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2, data = hatchtime)
   hatchmods[[72]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[73]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[74]] = glm(log(Time) ~ diff  + ATP + Dis + RelKM, data = hatchtime)
   hatchmods[[75]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + RelKM, data = hatchtime)
   hatchmods[[76]] = glm(log(Time) ~ diff  + ATP + Dis + T, data = hatchtime)
   hatchmods[[77]] = glm(log(Time) ~ diff  + ATP + Dis + T + T2, data = hatchtime)
   hatchmods[[78]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + T, data = hatchtime)
   hatchmods[[79]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + T + T2, data = hatchtime)
   hatchmods[[80]] = glm(log(Time) ~ diff  + ATP + Ph + RelKM, data = hatchtime)
   hatchmods[[81]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[82]] = glm(log(Time) ~ diff  + ATP + Ph + T, data = hatchtime)
   hatchmods[[83]] = glm(log(Time) ~ diff  + ATP + Ph + T + T2, data = hatchtime)
   hatchmods[[84]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[85]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[86]] = glm(log(Time) ~ diff  + ATP + RelKM + T, data = hatchtime)
   hatchmods[[87]] = glm(log(Time) ~ diff  + ATP + RelKM + T + T2, data = hatchtime)
   hatchmods[[88]] = glm(log(Time) ~ diff  + ATU + Dis + FL, data = hatchtime)
   hatchmods[[89]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL, data = hatchtime)
   hatchmods[[90]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL, data = hatchtime)
   hatchmods[[91]] = glm(log(Time) ~ diff  + ATU + FL + Ph, data = hatchtime)
   hatchmods[[92]] = glm(log(Time) ~ diff  + ATU + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[93]] = glm(log(Time) ~ diff  + ATU + ATU2 + FL + Ph, data = hatchtime)
   hatchmods[[94]] = glm(log(Time) ~ diff  + ATU + ATU2 + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[95]] = glm(log(Time) ~ diff  + ATU + FL + RelKM, data = hatchtime)
   hatchmods[[96]] = glm(log(Time) ~ diff  + ATU + ATU2 + FL + RelKM, data = hatchtime)
   hatchmods[[97]] = glm(log(Time) ~ diff  + ATU + FL + T, data = hatchtime)
   hatchmods[[98]] = glm(log(Time) ~ diff  + ATU + FL + T + T2, data = hatchtime)
   hatchmods[[99]] = glm(log(Time) ~ diff  + ATU + ATU2 + FL + T + T2, data = hatchtime)
   hatchmods[[100]] = glm(log(Time) ~ diff  + Dis + FL + Ph, data = hatchtime)
   hatchmods[[101]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[102]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph, data = hatchtime)
   hatchmods[[103]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[104]] = glm(log(Time) ~ diff  + Dis + FL + RelKM, data = hatchtime)
   hatchmods[[105]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + RelKM, data = hatchtime)
   hatchmods[[106]] = glm(log(Time) ~ diff  + Dis + FL + T, data = hatchtime)
   hatchmods[[107]] = glm(log(Time) ~ diff  + Dis + FL + T + T2, data = hatchtime)
   hatchmods[[108]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + T, data = hatchtime)
   hatchmods[[109]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + T + T2, data = hatchtime)
   hatchmods[[110]] = glm(log(Time) ~ diff  + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[111]] = glm(log(Time) ~ diff  + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[112]] = glm(log(Time) ~ diff  + FL + Ph + T, data = hatchtime)
   hatchmods[[113]] = glm(log(Time) ~ diff  + FL + Ph + T + T2, data = hatchtime)
   hatchmods[[114]] = glm(log(Time) ~ diff  + FL + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[115]] = glm(log(Time) ~ diff  + FL + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[116]] = glm(log(Time) ~ diff  + FL + RelKM + T, data = hatchtime)
   hatchmods[[117]] = glm(log(Time) ~ diff  + FL + RelKM + T + T2, data = hatchtime)
   hatchmods[[118]] = glm(log(Time) ~ diff  + ATU + Dis + Ph, data = hatchtime)
   hatchmods[[119]] = glm(log(Time) ~ diff  + ATU + Dis + Ph + Ph2, data = hatchtime)
   hatchmods[[120]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[121]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[122]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[123]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[124]] = glm(log(Time) ~ diff  + ATU + Ph + RelKM, data = hatchtime)
   hatchmods[[125]] = glm(log(Time) ~ diff  + ATU + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[126]] = glm(log(Time) ~ diff  + ATU + ATU2 + Ph + RelKM, data = hatchtime)
   hatchmods[[127]] = glm(log(Time) ~ diff  + ATU + ATU2 + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[128]] = glm(log(Time) ~ diff  + ATU + RelKM + T, data = hatchtime)
   hatchmods[[129]] = glm(log(Time) ~ diff  + ATU + RelKM + T + T2, data = hatchtime)
   hatchmods[[130]] = glm(log(Time) ~ diff  + ATU + ATU2 + RelKM + T, data = hatchtime)
   hatchmods[[131]] = glm(log(Time) ~ diff  + ATU + ATU2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[132]] = glm(log(Time) ~ diff  + Dis + Ph + RelKM, data = hatchtime)
   hatchmods[[133]] = glm(log(Time) ~ diff  + Dis + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[134]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + RelKM, data = hatchtime)
   hatchmods[[135]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[136]] = glm(log(Time) ~ diff  + Dis + Ph + T, data = hatchtime)
   hatchmods[[137]] = glm(log(Time) ~ diff  + Dis + Ph + T + T2, data = hatchtime)
   hatchmods[[138]] = glm(log(Time) ~ diff  + Dis + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[139]] = glm(log(Time) ~ diff  + Dis + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[140]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + T, data = hatchtime)
   hatchmods[[141]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + T + T2, data = hatchtime)
   hatchmods[[142]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[143]] = glm(log(Time) ~ diff  + Dis + Dis2 + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[144]] = glm(log(Time) ~ diff  + Ph + RelKM + T, data = hatchtime)
   hatchmods[[145]] = glm(log(Time) ~ diff  + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[146]] = glm(log(Time) ~ diff  + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[147]] = glm(log(Time) ~ diff  + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[148]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph, data = hatchtime)
   hatchmods[[149]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2, data = hatchtime)
   hatchmods[[150]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[151]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[152]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph, data = hatchtime)
   hatchmods[[153]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + Ph2, data = hatchtime)
   hatchmods[[154]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph, data = hatchtime)
   hatchmods[[155]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2, data = hatchtime)
   hatchmods[[156]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + RelKM, data = hatchtime)
   hatchmods[[157]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + RelKM, data = hatchtime)
   hatchmods[[158]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + RelKM, data = hatchtime)
   hatchmods[[159]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + RelKM, data = hatchtime)
   hatchmods[[160]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + T, data = hatchtime)
   hatchmods[[161]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + T + T2, data = hatchtime)
   hatchmods[[162]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + T, data = hatchtime)
   hatchmods[[163]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + T + T2, data = hatchtime)
   hatchmods[[164]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + T, data = hatchtime)
   hatchmods[[165]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + T + T2, data = hatchtime)
   hatchmods[[166]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + T, data = hatchtime)
   hatchmods[[167]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + T + T2, data = hatchtime)
   hatchmods[[168]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + RelKM, data = hatchtime)
   hatchmods[[169]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[170]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + RelKM, data = hatchtime)
   hatchmods[[171]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[172]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + T, data = hatchtime)
   hatchmods[[173]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + T + T2, data = hatchtime)
   hatchmods[[174]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[175]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[176]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + T, data = hatchtime)
   hatchmods[[177]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + T + T2, data = hatchtime)
   hatchmods[[178]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[179]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[180]] = glm(log(Time) ~ diff  + ATP + Ph + RelKM + T, data = hatchtime)
   hatchmods[[181]] = glm(log(Time) ~ diff  + ATP + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[182]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[183]] = glm(log(Time) ~ diff  + ATP + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[184]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph, data = hatchtime)
   hatchmods[[185]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[186]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph, data = hatchtime)
   hatchmods[[187]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[188]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph, data = hatchtime)
   hatchmods[[189]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[190]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph, data = hatchtime)
   hatchmods[[191]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2, data = hatchtime)
   hatchmods[[192]] = glm(log(Time) ~ diff  + ATU + Dis + FL + RelKM, data = hatchtime)
   hatchmods[[193]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + RelKM, data = hatchtime)
   hatchmods[[194]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + RelKM, data = hatchtime)
   hatchmods[[195]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + RelKM, data = hatchtime)
   hatchmods[[196]] = glm(log(Time) ~ diff  + ATU + Dis + FL + T, data = hatchtime)
   hatchmods[[197]] = glm(log(Time) ~ diff  + ATU + Dis + FL + T + T2, data = hatchtime)
   hatchmods[[198]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + T, data = hatchtime)
   hatchmods[[199]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + T + T2, data = hatchtime)
   hatchmods[[200]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + T, data = hatchtime)
   hatchmods[[201]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + T + T2, data = hatchtime)
   hatchmods[[202]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + T, data = hatchtime)
   hatchmods[[203]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + T + T2, data = hatchtime)
   hatchmods[[204]] = glm(log(Time) ~ diff  + Dis + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[205]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[206]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[207]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[208]] = glm(log(Time) ~ diff  + Dis + FL + Ph + T, data = hatchtime)
   hatchmods[[209]] = glm(log(Time) ~ diff  + Dis + FL + Ph + T + T2, data = hatchtime)
   hatchmods[[210]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[211]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[212]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + T, data = hatchtime)
   hatchmods[[213]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + T + T2, data = hatchtime)
   hatchmods[[214]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2 + T, data = hatchtime)
   hatchmods[[215]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2 + T + T2, data = hatchtime)
   hatchmods[[216]] = glm(log(Time) ~ diff  + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[217]] = glm(log(Time) ~ diff  + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[218]] = glm(log(Time) ~ diff  + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[219]] = glm(log(Time) ~ diff  + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[220]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + RelKM, data = hatchtime)
   hatchmods[[221]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[222]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + RelKM, data = hatchtime)
   hatchmods[[223]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[224]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + RelKM, data = hatchtime)
   hatchmods[[225]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[226]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM, data = hatchtime)
   hatchmods[[227]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[228]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + RelKM + T, data = hatchtime)
   hatchmods[[229]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[230]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[231]] = glm(log(Time) ~ diff  + ATP + Dis + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[232]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + RelKM + T, data = hatchtime)
   hatchmods[[233]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[234]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[235]] = glm(log(Time) ~ diff  + ATP + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[236]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + RelKM + T, data = hatchtime)
   hatchmods[[237]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[238]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[239]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[240]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + RelKM + T, data = hatchtime)
   hatchmods[[241]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[242]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[243]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[244]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[245]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[246]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[247]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[248]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[249]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[250]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM, data = hatchtime)
   hatchmods[[251]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM, data = hatchtime)
   hatchmods[[252]] = glm(log(Time) ~ diff  + Dis + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[253]] = glm(log(Time) ~ diff  + Dis + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[254]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[255]] = glm(log(Time) ~ diff  + Dis + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[256]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[257]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[258]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[259]] = glm(log(Time) ~ diff  + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[260]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[261]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[262]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[263]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[264]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[265]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[266]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[267]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[268]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + RelKM + T, data = hatchtime)
   hatchmods[[269]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[270]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[271]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[272]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + RelKM + T, data = hatchtime)
   hatchmods[[273]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[274]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[275]] = glm(log(Time) ~ diff  + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[276]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + RelKM + T, data = hatchtime)
   hatchmods[[277]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[278]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[279]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[280]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM + T, data = hatchtime)
   hatchmods[[281]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[282]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[283]] = glm(log(Time) ~ diff  + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[284]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[285]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[286]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[287]] = glm(log(Time) ~ diff  + ATU + Dis + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[288]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[289]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[290]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[291]] = glm(log(Time) ~ diff  + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[292]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[293]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[294]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[295]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[296]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM + T, data = hatchtime)
   hatchmods[[297]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM + T + T2, data = hatchtime)
   hatchmods[[298]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T, data = hatchtime)
   hatchmods[[299]] = glm(log(Time) ~ diff  + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2, data = hatchtime)
   hatchmods[[300]] = glm(log(Time) ~ diff , data = hatchtime)

# Give the models names
  hatchNames <- c()
    hatchNames[1]  <- ' diff + ATP'
    hatchNames[2]  <- ' diff + ATU'
    hatchNames[3]  <- ' diff + ATU + ATU2'
    hatchNames[4]  <- ' diff + Dis'
    hatchNames[5]  <- ' diff + Dis + Dis2'
    hatchNames[6]  <- ' diff + FL'
    hatchNames[7]  <- ' diff + Ph'
    hatchNames[8]  <- ' diff + Ph + Ph2'
    hatchNames[9]  <- ' diff + RelKM'
    hatchNames[10]  <- ' diff + T'
    hatchNames[11]  <- ' diff + T + T2'
    hatchNames[12]  <- ' diff + ATP + ATU'
    hatchNames[13]  <- ' diff + ATP + ATU + ATU2'
    hatchNames[14]  <- ' diff + ATP + Dis'
    hatchNames[15]  <- ' diff + ATP + Dis + Dis2'
    hatchNames[16]  <- ' diff + ATP + Ph'
    hatchNames[17]  <- ' diff + ATP + Ph + Ph2'
    hatchNames[18]  <- ' diff + ATP + RelKM'
    hatchNames[19]  <- ' diff + ATP + T'
    hatchNames[20]  <- ' diff + ATP + T + T2'
    hatchNames[21]  <- ' diff + ATU + FL'
    hatchNames[22]  <- ' diff + ATU + ATU2 + FL'
    hatchNames[23]  <- ' diff + Dis + FL'
    hatchNames[24]  <- ' diff + Dis + Dis2 + FL'
    hatchNames[25]  <- ' diff + FL + Ph'
    hatchNames[26]  <- ' diff + FL + Ph + Ph2'
    hatchNames[27]  <- ' diff + FL + RelKM'
    hatchNames[28]  <- ' diff + FL + T'
    hatchNames[29]  <- ' diff + FL + T + T2'
    hatchNames[30]  <- ' diff + ATU + Dis'
    hatchNames[31]  <- ' diff + ATU + Dis + Dis2'
    hatchNames[32]  <- ' diff + ATU + Ph'
    hatchNames[33]  <- ' diff + ATU + Ph + Ph2'
    hatchNames[34]  <- ' diff + ATU + RelKM'
    hatchNames[35]  <- ' diff + ATU + T'
    hatchNames[36]  <- ' diff + ATU + T + T2'
    hatchNames[37]  <- ' diff + ATU'
    hatchNames[38]  <- ' diff + ATU + ATU2 + Dis'
    hatchNames[39]  <- ' diff + ATU + ATU2 + Dis + Dis2'
    hatchNames[40]  <- ' diff + ATU + ATU2 + Ph'
    hatchNames[41]  <- ' diff + ATU + ATU2 + Ph + Ph2'
    hatchNames[42]  <- ' diff + ATU + ATU2 + RelKM'
    hatchNames[43]  <- ' diff + ATU + ATU2 + T'
    hatchNames[44]  <- ' diff + ATU + ATU2 + T + T2'
    hatchNames[45]  <- ' diff + ATU + ATU2'
    hatchNames[46]  <- ' diff + Dis + Ph'
    hatchNames[47]  <- ' diff + Dis + Ph + Ph2'
    hatchNames[48]  <- ' diff + Dis + RelKM'
    hatchNames[49]  <- ' diff + Dis + T + T2'
    hatchNames[50]  <- ' diff + Dis + Dis2 + Ph'
    hatchNames[51]  <- ' diff + Dis + Dis2 + Ph + Ph2'
    hatchNames[52]  <- ' diff + Dis + Dis2 + RelKM'
    hatchNames[53]  <- ' diff + Dis + Dis2 + T + T2'
    hatchNames[54]  <- ' diff + Ph + RelKM'
    hatchNames[55]  <- ' diff + Ph + T + T2'
    hatchNames[56]  <- ' diff + Ph + Ph2 + RelKM'
    hatchNames[57]  <- ' diff + Ph + Ph2 + T + T2'
    hatchNames[58]  <- ' diff + ATP + ATU + Dis'
    hatchNames[59]  <- ' diff + ATP + ATU + Dis + Dis2'
    hatchNames[60]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2'
    hatchNames[61]  <- ' diff + ATP + ATU + Ph'
    hatchNames[62]  <- ' diff + ATP + ATU + Ph + Ph2'
    hatchNames[63]  <- ' diff + ATP + ATU + ATU2 + Ph'
    hatchNames[64]  <- ' diff + ATP + ATU + ATU2 + Ph + Ph2'
    hatchNames[65]  <- ' diff + ATP + ATU + RelKM'
    hatchNames[66]  <- ' diff + ATP + ATU + ATU2 + RelKM'
    hatchNames[67]  <- ' diff + ATP + ATU + T'
    hatchNames[68]  <- ' diff + ATP + ATU + T + T2'
    hatchNames[69]  <- ' diff + ATP + ATU + ATU2 + T + T2'
    hatchNames[70]  <- ' diff + ATP + Dis + Ph'
    hatchNames[71]  <- ' diff + ATP + Dis + Ph + Ph2'
    hatchNames[72]  <- ' diff + ATP + Dis + Dis2 + Ph'
    hatchNames[73]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2'
    hatchNames[74]  <- ' diff + ATP + Dis + RelKM'
    hatchNames[75]  <- ' diff + ATP + Dis + Dis2 + RelKM'
    hatchNames[76]  <- ' diff + ATP + Dis + T'
    hatchNames[77]  <- ' diff + ATP + Dis + T + T2'
    hatchNames[78]  <- ' diff + ATP + Dis + Dis2 + T'
    hatchNames[79]  <- ' diff + ATP + Dis + Dis2 + T + T2'
    hatchNames[80]  <- ' diff + ATP + Ph + RelKM'
    hatchNames[81]  <- ' diff + ATP + Ph + Ph2 + RelKM'
    hatchNames[82]  <- ' diff + ATP + Ph + T'
    hatchNames[83]  <- ' diff + ATP + Ph + T + T2'
    hatchNames[84]  <- ' diff + ATP + Ph + Ph2 + T'
    hatchNames[85]  <- ' diff + ATP + Ph + Ph2 + T + T2'
    hatchNames[86]  <- ' diff + ATP + RelKM + T'
    hatchNames[87]  <- ' diff + ATP + RelKM + T + T2'
    hatchNames[88]  <- ' diff + ATU + Dis + FL'
    hatchNames[89]  <- ' diff + ATU + Dis + Dis2 + FL'
    hatchNames[90]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL'
    hatchNames[91]  <- ' diff + ATU + FL + Ph'
    hatchNames[92]  <- ' diff + ATU + FL + Ph + Ph2'
    hatchNames[93]  <- ' diff + ATU + ATU2 + FL + Ph'
    hatchNames[94]  <- ' diff + ATU + ATU2 + FL + Ph + Ph2'
    hatchNames[95]  <- ' diff + ATU + FL + RelKM'
    hatchNames[96]  <- ' diff + ATU + ATU2 + FL + RelKM'
    hatchNames[97]  <- ' diff + ATU + FL + T'
    hatchNames[98]  <- ' diff + ATU + FL + T + T2'
    hatchNames[99]  <- ' diff + ATU + ATU2 + FL + T + T2'
    hatchNames[100]  <- ' diff + Dis + FL + Ph'
    hatchNames[101]  <- ' diff + Dis + FL + Ph + Ph2'
    hatchNames[102]  <- ' diff + Dis + Dis2 + FL + Ph'
    hatchNames[103]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2'
    hatchNames[104]  <- ' diff + Dis + FL + RelKM'
    hatchNames[105]  <- ' diff + Dis + Dis2 + FL + RelKM'
    hatchNames[106]  <- ' diff + Dis + FL + T'
    hatchNames[107]  <- ' diff + Dis + FL + T + T2'
    hatchNames[108]  <- ' diff + Dis + Dis2 + FL + T'
    hatchNames[109]  <- ' diff + Dis + Dis2 + FL + T + T2'
    hatchNames[110]  <- ' diff + FL + Ph + RelKM'
    hatchNames[111]  <- ' diff + FL + Ph + Ph2 + RelKM'
    hatchNames[112]  <- ' diff + FL + Ph + T'
    hatchNames[113]  <- ' diff + FL + Ph + T + T2'
    hatchNames[114]  <- ' diff + FL + Ph + Ph2 + T'
    hatchNames[115]  <- ' diff + FL + Ph + Ph2 + T + T2'
    hatchNames[116]  <- ' diff + FL + RelKM + T'
    hatchNames[117]  <- ' diff + FL + RelKM + T + T2'
    hatchNames[118]  <- ' diff + ATU + Dis + Ph'
    hatchNames[119]  <- ' diff + ATU + Dis + Ph + Ph2'
    hatchNames[120]  <- ' diff + ATU + Dis + Dis2 + Ph'
    hatchNames[121]  <- ' diff + ATU + Dis + Dis2 + Ph + Ph2'
    hatchNames[122]  <- ' diff + ATU + ATU2 + Dis + Dis2 + Ph'
    hatchNames[123]  <- ' diff + ATU + ATU2 + Dis + Dis2 + Ph + Ph2'
    hatchNames[124]  <- ' diff + ATU + Ph + RelKM'
    hatchNames[125]  <- ' diff + ATU + Ph + Ph2 + RelKM'
    hatchNames[126]  <- ' diff + ATU + ATU2 + Ph + RelKM'
    hatchNames[127]  <- ' diff + ATU + ATU2 + Ph + Ph2 + RelKM'
    hatchNames[128]  <- ' diff + ATU + RelKM + T'
    hatchNames[129]  <- ' diff + ATU + RelKM + T + T2'
    hatchNames[130]  <- ' diff + ATU + ATU2 + RelKM + T'
    hatchNames[131]  <- ' diff + ATU + ATU2 + RelKM + T + T2'
    hatchNames[132]  <- ' diff + Dis + Ph + RelKM'
    hatchNames[133]  <- ' diff + Dis + Ph + Ph2 + RelKM'
    hatchNames[134]  <- ' diff + Dis + Dis2 + Ph + RelKM'
    hatchNames[135]  <- ' diff + Dis + Dis2 + Ph + Ph2 + RelKM'
    hatchNames[136]  <- ' diff + Dis + Ph + T'
    hatchNames[137]  <- ' diff + Dis + Ph + T + T2'
    hatchNames[138]  <- ' diff + Dis + Ph + Ph2 + T'
    hatchNames[139]  <- ' diff + Dis + Ph + Ph2 + T + T2'
    hatchNames[140]  <- ' diff + Dis + Dis2 + Ph + T'
    hatchNames[141]  <- ' diff + Dis + Dis2 + Ph + T + T2'
    hatchNames[142]  <- ' diff + Dis + Dis2 + Ph + Ph2 + T'
    hatchNames[143]  <- ' diff + Dis + Dis2 + Ph + Ph2 + T + T2'
    hatchNames[144]  <- ' diff + Ph + RelKM + T'
    hatchNames[145]  <- ' diff + Ph + RelKM + T + T2'
    hatchNames[146]  <- ' diff + Ph + Ph2 + RelKM + T'
    hatchNames[147]  <- ' diff + Ph + Ph2 + RelKM + T + T2'
    hatchNames[148]  <- ' diff + ATP + ATU + Dis + Ph'
    hatchNames[149]  <- ' diff + ATP + ATU + Dis + Ph + Ph2'
    hatchNames[150]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph'
    hatchNames[151]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2'
    hatchNames[152]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph'
    hatchNames[153]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + Ph2'
    hatchNames[154]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph'
    hatchNames[155]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2'
    hatchNames[156]  <- ' diff + ATP + ATU + Dis + RelKM'
    hatchNames[157]  <- ' diff + ATP + ATU + Dis + Dis2 + RelKM'
    hatchNames[158]  <- ' diff + ATP + ATU + ATU2 + Dis + RelKM'
    hatchNames[159]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + RelKM'
    hatchNames[160]  <- ' diff + ATP + ATU + Dis + T'
    hatchNames[161]  <- ' diff + ATP + ATU + Dis + T + T2'
    hatchNames[162]  <- ' diff + ATP + ATU + Dis + Dis2 + T'
    hatchNames[163]  <- ' diff + ATP + ATU + Dis + Dis2 + T + T2'
    hatchNames[164]  <- ' diff + ATP + ATU + ATU2 + Dis + T'
    hatchNames[165]  <- ' diff + ATP + ATU + ATU2 + Dis + T + T2'
    hatchNames[166]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + T'
    hatchNames[167]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + T + T2'
    hatchNames[168]  <- ' diff + ATP + Dis + Ph + RelKM'
    hatchNames[169]  <- ' diff + ATP + Dis + Ph + Ph2 + RelKM'
    hatchNames[170]  <- ' diff + ATP + Dis + Dis2 + Ph + RelKM'
    hatchNames[171]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2 + RelKM'
    hatchNames[172]  <- ' diff + ATP + Dis + Ph + T'
    hatchNames[173]  <- ' diff + ATP + Dis + Ph + T + T2'
    hatchNames[174]  <- ' diff + ATP + Dis + Ph + Ph2 + T'
    hatchNames[175]  <- ' diff + ATP + Dis + Ph + Ph2 + T + T2'
    hatchNames[176]  <- ' diff + ATP + Dis + Dis2 + Ph + T'
    hatchNames[177]  <- ' diff + ATP + Dis + Dis2 + Ph + T + T2'
    hatchNames[178]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2 + T'
    hatchNames[179]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2 + T + T2'
    hatchNames[180]  <- ' diff + ATP + Ph + RelKM + T'
    hatchNames[181]  <- ' diff + ATP + Ph + RelKM + T + T2'
    hatchNames[182]  <- ' diff + ATP + Ph + Ph2 + RelKM + T'
    hatchNames[183]  <- ' diff + ATP + Ph + Ph2 + RelKM + T + T2'
    hatchNames[184]  <- ' diff + ATU + Dis + FL + Ph'
    hatchNames[185]  <- ' diff + ATU + Dis + FL + Ph + Ph2'
    hatchNames[186]  <- ' diff + ATU + Dis + Dis2 + FL + Ph'
    hatchNames[187]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2'
    hatchNames[188]  <- ' diff + ATU + ATU2 + Dis + FL + Ph'
    hatchNames[189]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + Ph2'
    hatchNames[190]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph'
    hatchNames[191]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2'
    hatchNames[192]  <- ' diff + ATU + Dis + FL + RelKM'
    hatchNames[193]  <- ' diff + ATU + Dis + Dis2 + FL + RelKM'
    hatchNames[194]  <- ' diff + ATU + ATU2 + Dis + FL + RelKM'
    hatchNames[195]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + RelKM'
    hatchNames[196]  <- ' diff + ATU + Dis + FL + T'
    hatchNames[197]  <- ' diff + ATU + Dis + FL + T + T2'
    hatchNames[198]  <- ' diff + ATU + Dis + Dis2 + FL + T'
    hatchNames[199]  <- ' diff + ATU + Dis + Dis2 + FL + T + T2'
    hatchNames[200]  <- ' diff + ATU + ATU2 + Dis + FL + T'
    hatchNames[201]  <- ' diff + ATU + ATU2 + Dis + FL + T + T2'
    hatchNames[202]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + T'
    hatchNames[203]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + T + T2'
    hatchNames[204]  <- ' diff + Dis + FL + Ph + RelKM'
    hatchNames[205]  <- ' diff + Dis + FL + Ph + Ph2 + RelKM'
    hatchNames[206]  <- ' diff + Dis + Dis2 + FL + Ph + RelKM'
    hatchNames[207]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2 + RelKM'
    hatchNames[208]  <- ' diff + Dis + FL + Ph + T'
    hatchNames[209]  <- ' diff + Dis + FL + Ph + T + T2'
    hatchNames[210]  <- ' diff + Dis + FL + Ph + Ph2 + T'
    hatchNames[211]  <- ' diff + Dis + FL + Ph + Ph2 + T + T2'
    hatchNames[212]  <- ' diff + Dis + Dis2 + FL + Ph + T'
    hatchNames[213]  <- ' diff + Dis + Dis2 + FL + Ph + T + T2'
    hatchNames[214]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2 + T'
    hatchNames[215]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2 + T + T2'
    hatchNames[216]  <- ' diff + FL + Ph + RelKM + T'
    hatchNames[217]  <- ' diff + FL + Ph + RelKM + T + T2'
    hatchNames[218]  <- ' diff + FL + Ph + Ph2 + RelKM + T'
    hatchNames[219]  <- ' diff + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[220]  <- ' diff + ATP + ATU + Dis + Ph + RelKM'
    hatchNames[221]  <- ' diff + ATP + ATU + Dis + Ph + Ph2 + RelKM'
    hatchNames[222]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + RelKM'
    hatchNames[223]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM'
    hatchNames[224]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + RelKM'
    hatchNames[225]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM'
    hatchNames[226]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM'
    hatchNames[227]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM'
    hatchNames[228]  <- ' diff + ATP + Dis + Ph + RelKM + T'
    hatchNames[229]  <- ' diff + ATP + Dis + Ph + RelKM + T + T2'
    hatchNames[230]  <- ' diff + ATP + Dis + Ph + Ph2 + RelKM + T'
    hatchNames[231]  <- ' diff + ATP + Dis + Ph + Ph2 + RelKM + T + T2'
    hatchNames[232]  <- ' diff + ATP + Dis + Dis2 + Ph + RelKM + T'
    hatchNames[233]  <- ' diff + ATP + Dis + Dis2 + Ph + RelKM + T + T2'
    hatchNames[234]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2 + RelKM + T'
    hatchNames[235]  <- ' diff + ATP + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2'
    hatchNames[236]  <- ' diff + ATP + ATU + Dis + Ph + RelKM + T'
    hatchNames[237]  <- ' diff + ATP + ATU + Dis + Ph + RelKM + T + T2'
    hatchNames[238]  <- ' diff + ATP + ATU + Dis + Ph + Ph2 + RelKM + T'
    hatchNames[239]  <- ' diff + ATP + ATU + Dis + Ph + Ph2 + RelKM + T + T2'
    hatchNames[240]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + RelKM + T'
    hatchNames[241]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + RelKM + T + T2'
    hatchNames[242]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T'
    hatchNames[243]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2'
    hatchNames[244]  <- ' diff + ATU + Dis + FL + Ph + RelKM'
    hatchNames[245]  <- ' diff + ATU + Dis + FL + Ph + Ph2 + RelKM'
    hatchNames[246]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + RelKM'
    hatchNames[247]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM'
    hatchNames[248]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + RelKM'
    hatchNames[249]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM'
    hatchNames[250]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM'
    hatchNames[251]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM'
    hatchNames[252]  <- ' diff + Dis + FL + Ph + RelKM + T'
    hatchNames[253]  <- ' diff + Dis + FL + Ph + RelKM + T + T2'
    hatchNames[254]  <- ' diff + Dis + FL + Ph + Ph2 + RelKM + T'
    hatchNames[255]  <- ' diff + Dis + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[256]  <- ' diff + Dis + Dis2 + FL + Ph + RelKM + T'
    hatchNames[257]  <- ' diff + Dis + Dis2 + FL + Ph + RelKM + T + T2'
    hatchNames[258]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T'
    hatchNames[259]  <- ' diff + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[260]  <- ' diff + ATU + Dis + FL + Ph + RelKM + T'
    hatchNames[261]  <- ' diff + ATU + Dis + FL + Ph + RelKM + T + T2'
    hatchNames[262]  <- ' diff + ATU + Dis + FL + Ph + Ph2 + RelKM + T'
    hatchNames[263]  <- ' diff + ATU + Dis + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[264]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + RelKM + T'
    hatchNames[265]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + RelKM + T + T2'
    hatchNames[266]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T'
    hatchNames[267]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[268]  <- ' diff + ATP + ATU + Dis + Ph + RelKM + T'
    hatchNames[269]  <- ' diff + ATP + ATU + Dis + Ph + RelKM + T + T2'
    hatchNames[270]  <- ' diff + ATP + ATU + Dis + Ph + Ph2 + RelKM + T'
    hatchNames[271]  <- ' diff + ATP + ATU + Dis + Ph + Ph2 + RelKM + T + T2'
    hatchNames[272]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + RelKM + T'
    hatchNames[273]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + RelKM + T + T2'
    hatchNames[274]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T'
    hatchNames[275]  <- ' diff + ATP + ATU + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2'
    hatchNames[276]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + RelKM + T'
    hatchNames[277]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + RelKM + T + T2'
    hatchNames[278]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM + T'
    hatchNames[279]  <- ' diff + ATP + ATU + ATU2 + Dis + Ph + Ph2 + RelKM + T + T2'
    hatchNames[280]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM + T'
    hatchNames[281]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + RelKM + T + T2'
    hatchNames[282]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM + T'
    hatchNames[283]  <- ' diff + ATP + ATU + ATU2 + Dis + Dis2 + Ph + Ph2 + RelKM + T + T2'
    hatchNames[284]  <- ' diff + ATU + Dis + FL + Ph + RelKM + T'
    hatchNames[285]  <- ' diff + ATU + Dis + FL + Ph + RelKM + T + T2'
    hatchNames[286]  <- ' diff + ATU + Dis + FL + Ph + Ph2 + RelKM + T'
    hatchNames[287]  <- ' diff + ATU + Dis + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[288]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + RelKM + T'
    hatchNames[289]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + RelKM + T + T2'
    hatchNames[290]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T'
    hatchNames[291]  <- ' diff + ATU + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[292]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + RelKM + T'
    hatchNames[293]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + RelKM + T + T2'
    hatchNames[294]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM + T'
    hatchNames[295]  <- ' diff + ATU + ATU2 + Dis + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[296]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM + T'
    hatchNames[297]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + RelKM + T + T2'
    hatchNames[298]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T'
    hatchNames[299]  <- ' diff + ATU + ATU2 + Dis + Dis2 + FL + Ph + Ph2 + RelKM + T + T2'
    hatchNames[300]  <- ' diff'

# Create a model-selection table
  hatchResults <- aictab(hatchmods, hatchNames)

# Look at model-selection results
	hatchResults <- head(hatchResults, 10)

# Save the model-selection table to a file
   setwd("C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables")
   write.table(hatchResults, "hatchInitModelSelection.csv", row.names=FALSE,
     sep=",")

# Look at results of best model
  best.hatch <- summary(hatchmods[[as.numeric(row.names(hatchResults)[1])]])
	best.hatch

# Make an object of the top ten models for model averaging
  hatches = list()
  for(i in 1:11) {
  	hatches[[i]] <- hatchmods[[c(as.numeric(row.names(hatchResults)[i]))]]
  }
  hatches[[10]] <- NULL

# Get model-averaged parameter estimates
  g = model.avg(hatches, revised.var=TRUE, se.fit=TRUE, delta < 2.1)
  g$coefTable
  # Awesome

# Get model-averaged parameter estimates
  atuParm = modavg(hatches, parm = 'ATU', exclude = 'ATU2')[c(3,6,7)]
  atu2Parm	= modavg(hatches, parm = 'ATU2', second.order=TRUE)[c(3,6,7)]
  nkaParm	= modavg(hatches, parm = 'ATP')[c(3,6,7)]
  ppParm = modavg(hatches, parm = 'Ph', exclude='Ph2')[c(3,6,7)]
  pp2Parm = modavg(hatches, parm = 'Ph2', second.order=TRUE)[c(3,6,7)]
  disParm = modavg(hatches, parm = 'Dis', exclude='Dis2')[c(3,6,7)]
  disParm = modavg(hatches, parm = 'Dis2', exclude='Dis')[c(3,6,7)]


# Make a dataframe out of the movement results
	hres <- data.frame(best.hatch$coefficients)
  hres

# Plot effects of covariates on initiation of hatchery fish migration
# in the Penobscot River
	dat <- originaldata
	# Effect of gill NKA activity
	  # New values for predictions
  	  miatp <- min(hatchtime$ATP)
	    maatp <- max(hatchtime$ATP)
	    natp <- seq(miatp, maatp, 0.01)
	    rnatp <- natp*sd(dat$ATP) + mean(dat$ATP)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanatp <- list()
	    atp <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanatp[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* natp +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* median(hatchtime$ATU) +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* median(hatchtime$ATU2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* median(hatchtime$Dis) +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* median(hatchtime$Dis2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* median(hatchtime$Ph) +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* median(hatchtime$RelKM) +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* median(hatchtime$T)
	            atp[[i]] <- exp(meanatp[[i]])
	          }
	       }
	    }

	# Effect of ATU
	  # New values for predictions
  	  miATU <- min(hatchtime$ATU)
	    maATU <- max(hatchtime$ATU)
	    nATU <- seq(miATU, maATU, 0.01)
	    rnATU <- nATU*sd(dat$ATU) + mean(dat$ATU)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanATU <- list()
	    ATU <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanATU[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* median(hatchtime$ATP) +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* nATU +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* (nATU^2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* median(hatchtime$Dis) +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* median(hatchtime$Dis2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* median(hatchtime$Ph) +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* median(hatchtime$RelKM) +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* median(hatchtime$T)
	            ATU[[i]] <- exp(meanATU[[i]])
	          }
	       }
	    }

	# Effect of Discharge
	  # New values for predictions
  	  miDis <- min(hatchtime$Dis)
	    maDis <- max(hatchtime$Dis)
	    nDis <- seq(miDis, maDis, 0.01)
	    rnDis <- nDis*sd(dat$Dis) + mean(dat$Dis)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanDis <- list()
	    Dis <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanDis[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* median(hatchtime$ATP) +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* median(hatchtime$ATU) +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* median(hatchtime$ATU2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* nDis +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* (nDis^2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* median(hatchtime$Ph) +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* median(hatchtime$RelKM) +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* median(hatchtime$T)
	            Dis[[i]] <- exp(meanDis[[i]])
	          }
	       }
	    }

	# Effect of photoperiod
	  # New values for predictions
  	  miPh <- min(hatchtime$Ph)
	    maPh <- max(hatchtime$Ph)
	    nPh <- seq(miRelKM, maPh, 0.01)
	    rnPh <- nPh*sd(dat$Ph) + mean(dat$Ph)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanPh <- list()
	    Ph <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanPh[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* median(hatchtime$ATP) +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* median(hatchtime$ATU) +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* median(hatchtime$ATU2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* median(hatchtime$Dis) +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* median(hatchtime$Dis2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* nPh +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* median(hatchtime$RelKM) +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* median(hatchtime$T)
	            Ph[[i]] <- exp(meanPh[[i]])
	          }
	       }
	    }

	# Effect of Release rkm
	  # New values for predictions
  	  miRelKM <- min(hatchtime$RelKM)
	    maRelKM <- max(hatchtime$RelKM)
	    nRelKM <- seq(miRelKM, maRelKM, 0.01)
	    rnRelKM <- nRelKM*sd(dat$RelKM) + mean(dat$RelKM)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanRelKM <- list()
	    RelKM <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanRelKM[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* median(hatchtime$ATP) +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* median(hatchtime$ATU) +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* median(hatchtime$ATU2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* median(hatchtime$Dis) +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* median(hatchtime$Dis2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* median(hatchtime$Ph) +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* nRelKM +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* median(hatchtime$T)
	            RelKM[[i]] <- exp(meanRelKM[[i]])
	          }
	       }
	    }

	# Effect of temperature at release
	  # New values for predictions
  	  miT <- min(hatchtime$T)
	    maT <- max(hatchtime$T)
	    nT <- seq(miT, maT, 0.01)
	    rnT <- nT*sd(dat$T) + mean(dat$T)
	  # Predicted values
	    X <- c(-1,0,1)
	    meanT <- list()
	    T <- list()
	    for(i in 1:length(X)){
	      hres <- hres
	    	# Now loop through the matrix of coefficient estimates to calculate
	    	# effect of the desired covariate.  This set of nested loops just
	    	# protects the reference to the matrix indices inside of the i loop.
	    	  for(j in 1:nrow(hres)){
	    		  for(t in 1:2){
	            meanT[[i]] <-
	    	        (hres[1,1] + X[i] * (1.96*hres[1,2])) +
		            (hres[2,1] + X[i] * (1.96*hres[2,2]))* median(hatchtime$diff) +
		            (hres[3,1] + X[i] * (1.96*hres[3,2]))* median(hatchtime$ATP) +
		            (hres[4,1] + X[i] * (1.96*hres[4,2]))* median(hatchtime$ATU) +
		            (hres[5,1] + X[i] * (1.96*hres[5,2]))* median(hatchtime$ATU2) +
		            (hres[6,1] + X[i] * (1.96*hres[6,2]))* median(hatchtime$Dis) +
		            (hres[7,1] + X[i] * (1.96*hres[7,2]))* median(hatchtime$Dis2) +
		            (hres[8,1] + X[i] * (1.96*hres[8,2]))* median(hatchtime$Ph) +
		            (hres[9,1] + X[i] * (1.96*hres[9,2]))* median(hatchtime$RelKM) +
		            (hres[10,1] + X[i] * (1.96*hres[10,2]))* nT
	            T[[i]] <- exp(meanT[[i]])
	          }
	       }
	    }

# Plot the results
	# Create a file to which the plot will be saved
    tiff(file = 'C:/Users/Dan/Desktop/Manuscripts/Initiation and movement/Figures & Tables/Figure5.tiff',
  	  width=5000, height=7000, pointsize=18, compression="lzw", res=500)
  # Plot effect of gill NKA activity
	  par(mfrow=c(3,2), mar=c(6,7,1,1))
	  plot(rnatp, atp[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnatp, atp[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnatp, atp[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  axis(side=1, at=c(seq(0, 15, 2.5)), sprintf("%.1f", seq(0, 15, 2.5)),
	  	cex.axis=1.1)
	  mtext(side=1, 'Gill NKA activity', line=4, cex=1.25)
	  text(x=1.5, y=170, '(a)', cex=2)
  # Plot effect of ATU
	  par(mar=c(6,7,1,1))
	  plot(rnATU, ATU[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(230, 450))
	  par(new=TRUE)
	  plot(rnATU, ATU[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(230, 450))
	  par(new=TRUE)
	  plot(rnATU, ATU[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(230, 450))
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  axis(side=1, at=c(seq(200, 550, 50)), as.character(seq(200, 550, 50)))
	  mtext(side=1, 'Accumulated thermal units', line=4, cex=1.1)
		text(x=238, y=170, '(b)', cex=2)
  # Plot effect of Discharge
	  par(mar=c(6,7,1,1))
	  plot(rnDis, Dis[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnDis, Dis[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnDis, Dis[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  mtext(side=2, 'Time to initiate migration (hours)', cex=1.5, line=4)
	  axis(side=1, at=c(seq(250, 1750, 250)), as.character(seq(250, 1750, 250)),
	  	cex.axis=1.1)
	  mtext(side=1, expression(paste('Discharge (m'^'3', plain('\u00b7'),
		  's'^'-1', ')', sep='')), line=4, cex=1.1)
		text(x=300, y=170, '(c)', cex=2)
  # Plot effect of photoperiod
	  par(mar=c(6,7,1,1))
	  plot(rnPh, Ph[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnPh, Ph[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnPh, Ph[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  axis(side=1, at=c(seq(13.5, 14.5, .25)), sprintf("%.2f",
	  	seq(13.5, 14.5, .25)), cex.axis=1.1)
	  mtext(side=1, 'Photoperiod (hours)', line=4, cex=1.1)
		text(x=13.36, y=170, '(d)', cex=2)
  # Plot effect of release rkm
	  par(mar=c(6,7,1,1))
	  plot(rnRelKM, RelKM[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(190, 60))
	  par(new=TRUE)
	  plot(rnRelKM, RelKM[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(190, 60))
	  par(new=TRUE)
	  plot(rnRelKM, RelKM[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(190, 60))
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  axis(side=1, at=c(seq(175, 75, -25)), as.character(seq(175, 75, -25)),
	  	cex.axis=1.1)
	  mtext(side=1, 'Distance from ocean (km)', line=4, cex=1.1)
		text(x=186, y=170, '(e)', cex=2)
  # Plot effect of temperature
	  par(mar=c(6,7,1,1))
	  plot(rnT, T[[1]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnT, T[[2]], type='l', col='black', lty=1, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  par(new=TRUE)
	  plot(rnT, T[[3]], type='l', col='gray', lty=2, lwd=3, ylim=c(0,175),
		  xlab='', ylab='', yaxt='n', xaxt='n')
	  axis(side=2, at=c(seq(0, 175, 25)), as.character(seq(0, 175, 25)),
		  cex.axis=1.1, las=2)
	  axis(side=1, at=c(seq(4, 16, 2)), as.character(seq(4, 16, 2)),
	  	cex.axis=1.1)
	  mtext(side=1, expression(paste('Temperature (',plain('\u00b0'),'C)')),
	  	line=4, cex=1.1)
		text(x=4.1, y=170, '(f)', cex=2)
	# Turn off the graphics device and save the file
	  dev.off()
################################################################################
} # Code-folding marker
# END PART 10. FRESHWATER BEHAVIORAL ANALYSIS-----------------------------------