

#Calculate total alkalinity using potentiometric titrations
#Calculate total alkalinity using potentiometric titrations
#Uses a for loop to read in data exported as a titration file and calculate Total alkalinity
#At the end it exports your data as a .csv file. Comment the last line out if your don't want that.

### Files needed ######
# 1. pHCalibration.csv in your "Data" folder
#Inside the Data folder You must have a subfolder for each data set. In each subfolder there is:
#2. the mass file for your run  
#3. a subfolder named "TodaysDate" (where all of your titration files are) directly exported from LabX.


# Created by Nyssa Silbiger 03/28/2014
# modified 20210126 Sam Gurr - set diff directories and such for PPP Lab titrator (URI)
# new acid bottle 20190626 by Sam Gurr - changed lines 
#line 68 changed SampleID to Sample.ID - Sam Gurr 20180713
#modified 20210221 Danielle Becker - set different mass file and Titratorfile to match code
#modified 20210318 Danielle Becker - opened a new acid titrant, same batch #A16
#new acid bottle 20220127 by Danielle Becker - new batch number A22, updated script calculation
# modified 20220720 by Lauren Zane - added date column for each sample; create cumulative TA .csv for CBLS aquarium titrations (URI)
## modified 20241008 by Florence Fields - new batch number A24, updated script calculation 
## modified 20250714 by Pooja Pednekar- opened a new acid titrant batch #A14, updated script calculation

#------------------------------------------------------------
rm(list=ls()) # sweep environment

#set working directory---------------------------------------------------------------------------------------------

setwd("C:/PUTNAM LAB/Titrator/Data")
#load libraries----------------------------------------------
## NOTE: newer versions of the seacarb package have a hard time recognizing the "at" function
## you need to check that the version installed is version 3.2, not version 3.3.1 which is the newest version of the "seacarb"
## package as of 20220822
## LZ replaced v 3.3.1 with version 3.2 from the CRAN archive (https://cran.r-project.org/src/contrib/Archive/seacarb/) 20220822
packageurl <- "https://cran.r-project.org/src/contrib/Archive/seacarb/seacarb_3.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

library(seacarb) #used to calculate TA
library(tidyverse)

#CHANGE THESE VALUES EVERY DAY----------------------------------------------

## <<<<<<< HEAD
massfile<-"Mass_20250730_CRM.csv" # name of your file with masses
titrationfile<-'20250730_CBLS_CRM.csv'# name of the last titration file run
date<-'20250730' #date that data was run
path<-"C:/PUTNAM LAB/Titrator/Data/BlueTank_Titrations/20250730" #the location of all your titration files, your folder of the day!



## 


#DO NOT CHANGE ANYTHING BELOW THIS LINE UNLESS A NEW BOTTLE OF ACID IS USED

#load Data---------------------------------------------------

#load Mass Data

Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 



#### pH Calibration #####


pHCal<-read.csv("C:/PUTNAM LAB/Titrator/Data/pHCalibration.csv") # read in the pH Calibration file



#select the calibration for the correct date

pHData<-pHCal[pHCal$Date==date,]



# calculate pH 3 and 3.5 based on the slope and intercept from pH 4, 7, and 10 calibration

mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model



# print a plot of the relationship between pH and mV

png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)


plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')

lines(c(4,7,10), predict(mod.pH))

R2<-summary(mod.pH)$r.squared

legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')

dev.off()



# Select the mV for pH=3 and pH=3.5 based on your probe calibration

pH35<-mod.pH$coefficients[1]+mod.pH$coefficients[2]*3.5

pH3<-mod.pH$coefficients[1]+mod.pH$coefficients[2]*3


##### titration###########

#create an empty matrix to put the TA values in

nrows<-nrow(Mass) #need file length/number of rows


TA <- data.frame(matrix(nrow = nrows, ncol = 5)) # changed from 4 columns to 5 LZ 20220720

rownames(TA)<-Mass$Sample.ID1[1:nrows]

colnames(TA)<-c("Date","SampleID",'TA','Mass','Salinity') # added date column LZ 20220720
# changed Sample.ID1 to SampleID in the TA data frame only


#run a for loop to bring in the titration files one at a time and calculate TA
# read in the mega concatenated titration results file

filename<-file.path(path,titrationfile)

AllData<-read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

#Identifies rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

## parse through all the data in the one file ###
sample_names<-Mass$Sample.ID1

# create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}


for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
 
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  #To obtain this information use the Certificate of Analysis from the Andrew Dickson's Labs for respective acid titrant batches.
  
  #For 1(d). For the density of the titrant, you will need three values highlighted in blue (in the script below). In the density section of the certificate, the formula with calculated values have all the values required. 
    #Formula from the certificate: Density= xa- xb-xc, look below for comparing & inputing the values in the script. 
  
  #line 205: d <- (xc*mean(Data$Temperature[mV], na.rm=T)^2-xb*mean(Data$Temperature[mV], na.rm=T)+xa)
  
  
  #For 2(c).Line 215: The concentration of the acid is obtained from HCl conc.= xxxxxxx ±0.000 006 mol kg^-1."xxxxx" is your value "c" (in the script below)for HCl concentration. If confused, look at the protocol for detailed explanation.
  
  #1. density of your titrant: change every time acid is changed 
  #d <- (-0.0000037*mean(Data$Temperature[mV], na.rm=T)^2-0.0001233*mean(Data$Temperature[mV], na.rm=T)+1.02900) #20250714 Batch A14 PP
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  #d <- (-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  #Florence Fields updated script on 20241008 with the current batch opened 20240613 #A24
  #d <- (-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001069*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20240613 Batch A24 FF
  
  #Pooja Pednekar updated script on 20250714 with the new batch opened on 20250714 by HP #A14
  
  d <- (-0.0000037*mean(Data$Temperature[mV], na.rm=T)^2-0.0001233*mean(Data$Temperature[mV], na.rm=T)+1.02900) #20250714 Batch A14 PP
  
  #2. concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  #c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  #c<-0.099922 ##Batch A24 currently in use since 20240613 -FF
  
  c<-0.100183 ##Batch A14 currently in use since 20250714 PP. First used by PP on 20250714
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$Sample.ID1==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$Sample.ID1==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in seacarb package by Steve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  #NOTE: newer versions of the seacarb package have a hard time recognizing the "at" function
  ## you need to check that the version installed is version 3.2.14, not version 3.3.1 which is the newest version of the "seacarb". 
  #You can use the below code after uninstalling the current version of seacarb and if need a version 3.2 for the at function
  
  #install.packages("remotes")  # if not already installed
  #remotes::install_version("seacarb", version = "3.2.14", repos = "http://cran.us.r-project.org")
  
  TA[i,1]<-date #exports the date into output file, column 1; added by LZ 20220720
  TA[i,2]<-name #exports the sample ID into output file, column 2
  TA[i,3]<-1000000*at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,4]<-mass #exports the mass into the TA output file, column 4
  TA[i,5]<-s #exports the salinity column into the output file, column 5
}

TA[,3:4]<-sapply(TA[,3:4], as.numeric) # make sure the appropriate columns are numeric

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("C:/PUTNAM LAB/Titrator/Data/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

getwd()
#export data as csv file
write.table(update.data,"C:/PUTNAM LAB/Titrator/Data/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)

