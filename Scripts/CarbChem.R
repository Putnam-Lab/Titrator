#Title: CarbChem.R
#Contact: Hollie Putnam hollieputnam@gmail.com
#Changes made 20200126 by Sam Gurr for PPP Lab titrator computer
#See Readme file for details on data files and metadata

# path changes made on 20180712 sor SJG personal comp at Pt Whitney - original paths kept with
# added loop to print anova results  - SJG on 20170714

rm(list=ls()) # removes all prior objects

#R Version: R version 3.3.1
#RStudio Version: 1.0.44
######Read in required libraries#####
library(car) #version 2.1-4 Date: 2016-11-30 Depends: R (>= 3.2.0) Imports:MASS, mgcv, nnet, pbkrtest (>= 0.3-2), quantreg, grDevices, utils, stats, graphics, Rcpp
library(ggplot2) #version 2.2.1 Date/Publication: 2016-12-30 Depends: R (>= 3.1) Imports: digest, grid, gtable (>= 0.1.1), MASS, plyr (>= 1.7.1),reshape2, scales (>= 0.4.1), stats, tibble, lazyeval
library(gridExtra) #version: 2.2.1 Date/Publication: 2016-02-29 Depends: R(>= 2.5.0) Imports: gtable, grid, grDevices, graphics, utils
library(lsmeans)  #version: 2.26-3 Date: 2017-05-09 Depends: estimability, methods, R (>= 3.0) Imports: graphics, stats, utils, nlme, coda (>= 0.17), multcomp, plyr,mvtnorm, xtable (>= 1.8-2)
library(multcomp) #version: 1.4-6 Date: 2016-07-14 Depends: stats, graphics, mvtnorm (>= 1.0-3), survival (>= 2.39-4), TH.data (>= 1.0-2)
library(nlme) #version: 3.1-131 Date: 2017-02-06 Depends: R (>= 3.0.2) Imports: graphics, stats, utils, lattice
library(plotrix) #version: 3.6-5 Date: 2017-05-09 Depends: NA Imports: grDevices, graphics, stats, utils
library(plyr) #version: 1.8.4 Date/Publication: 2016-06-08 Depends: R (>= 3.1.0) Imports: Rcpp (>= 0.11.0)
library(reshape) #version: 3.3.1 Date/Publication: 2016-06-24  Depends: R (>= 3.3.1)
library(seacarb) #version: 3.2 Date/Publication: 2017-06-19 Depends: R (>= 2.10), oce, gsw Imports: NA
library(grid) #version: 3.3.1 Date/Publication: 2016-06-24  Depends: R (>= 3.3.1)
library(xtable) #version 1.8-2 Date/Publication: 2016-01-08 Depends: R (>= 2.10.0)
library(lme4) #version: 1.1-13 Date/Publication: 2017-04-19 Depends: R (>= 3.0.2), Matrix (>= 1.1.1), methods, stats Imports: graphics, grid, splines, utils, parallel, MASS, lattice, nlme(>= 3.1-123), minqa (>= 1.1.15), nloptr (>= 1.0.4)
library(blmeco) #version: 1.1 Date/Publication: 2015-08-22 Depends: R (>= 3.0.0), stats, MASS Imports: MuMIn, arm, lme4
library(MuMIn) #version: 1.15.6 Date/Publication: 2016-01-07 Depends: R (>= 3.0.0) Imports: graphics, methods, Matrix, stats, stats4
library(dplyr)
#####Required Data files#####

#CRM_TA_Data.csv
#Daily_Temp_pH_Sal.csv
#~/MyProjects/BioMin_HIS/RAnalysis/Data/pH_Calibration_Files


#############################################################
setwd("C:/Users/PPP Lab/Documents/Water_Chemistry/Data") #set working directory
# mainDir<-'C:/Users/samjg/Documents/My_Projects/Inragenerational_thresholds_OA/RAnalysis/' #set main directory
#############################################################


##### DISCRETE pH CALCULATIONS #####
path <-("C:/Users/PPP Lab/Documents/Water_Chemistry/Data/pH_Calibration_Files")
file.names<-list.files(path = path, pattern = "csv$") #list all the file names in the folder to get only get the csv files
pH.cals <- data.frame(matrix(NA, nrow=length(file.names), ncol=3, dimnames=list(file.names,c("Date", "Intercept", "Slope")))) #generate a 3 column dataframe with specific column names

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Calib.Data <-read.table(file.path(path,file.names[i]), header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
  model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
  coe <- coef(model) #extracts the coeffecients
  summary(model)$r.squared
  plot(Calib.Data$mVTris, Calib.Data$TTris)
  pH.cals[i,2:3] <- coe #inserts them in the dataframe
  pH.cals[i,1] <- substr(file.names[i],1,8) #stores the file name in the Date column
}
colnames(pH.cals) <- c("Calib.Date",  "Intercept",  "Slope") #rename columns
pH.cals #view data

#constants for use in pH calculation 
R <- 8.31447215 #gas constant in J mol-1 K-1 
F <-96485.339924 #Faraday constant in coulombs mol-1

#read in probe measurements of pH, temperature, and salinity from tanks
daily <- read.csv("Daily_Temp_pH_Sal.csv", header=TRUE, sep=",", na.strings="NA") #load data with a header, separated by commas, with NA as NA

#merge with Seawater chemistry file
SW.chem <- merge(pH.cals, daily, by="Calib.Date")

mvTris <- SW.chem$Temperature*SW.chem$Slope+SW.chem$Intercept #calculate the mV of the tris standard using the temperature mv relationships in the measured standard curves 
STris<-34.5 #salinity of the Tris
phTris<- (11911.08-18.2499*STris-0.039336*STris^2)*(1/(SW.chem$Temperature+273.15))-366.27059+ 0.53993607*STris+0.00016329*STris^2+(64.52243-0.084041*STris)*log(SW.chem$Temperature+273.15)-0.11149858*(SW.chem$Temperature+273.15) #calculate the pH of the tris (Dickson A. G., Sabine C. L. and Christian J. R., SOP 6a)
SW.chem$pH.Total<-phTris+(mvTris/1000-SW.chem$pH.MV/1000)/(R*(SW.chem$Temperature+273.15)*log(10)/F) #calculate the pH on the total scale (Dickson A. G., Sabine C. L. and Christian J. R., SOP 6a)


pdf("C:/Users/PPP Lab/Documents/Water_Chemistry/Output/Daily_Treatment_Measures.pdf")
par(mfrow=c(2,2))
plot(SW.chem$Treatment, SW.chem$Temperature, xlab="Treatment", ylab="Temperature°C", ylim=c(12,20), las=2)
plot(SW.chem$Treatment, SW.chem$pH.Total, xlab="Treatment", ylab="pH Total Scale", ylim=c(7.0,8.2), las=2)
plot(SW.chem$Treatment, SW.chem$Salinity, xlab="Treatment", ylab="Salinity psu", ylim=c(26,30), las=2)
dev.off()

pdf("C:/Users/PPP Lab/Documents/Water_Chemistry/Output/Daily_Tank_Measures.pdf")
par(mfrow=c(3,1))
plot(SW.chem$Sample.ID, SW.chem$Temperature, xlab="Tank", ylab="Temperature°C", ylim=c(12,20),las=2)
plot(SW.chem$Sample.ID, SW.chem$pH.Total, xlab="Tank", ylab="pH Total Scale", ylim=c(6.5,8.2),las=2)
plot(SW.chem$Sample.ID, SW.chem$Salinity, xlab="Tank", ylab="Salinity psu", ylim=c(26,30),las=2)
dev.off()

##### DISCRETE TA CALCULATIONS #####
TA <- read.csv("Cumulative_TA_Output.csv", header=TRUE, sep=",", na.strings="NA")  #read in  TA results


##### SEAWATER CHEMISTRY ANALYSIS FOR DISCRETE MEASUREMENTS#####
#Seawater chemistry table from simultaneous TA, pH, temperature and salinity measurements
#merge calculated pH and daily measures with TA data and run seacarb
SW.chem$Sample.ID <- paste(SW.chem$Date, SW.chem$Sample.ID, sep='_') #generate new row with concatenated sample id
SW.chem <- merge(SW.chem,TA, by="Sample.ID", all = TRUE, sort = T) #merge seawater chemistry with total alkalinity
SW.chem <- na.omit(SW.chem) #remove NA

#Calculate CO2 parameters using seacarb
carb.output <- carb(flag=8, var1=SW.chem$pH.Total, var2=SW.chem$TA/1000000, S= SW.chem$Salinity, T=SW.chem$Temperature, P=0, Pt=0, Sit=0, pHscale="T", kf="pf", k1k2="l", ks="d") #calculate seawater chemistry parameters using seacarb
carb.output$ALK <- carb.output$ALK*1000000 #convert to µmol kg-1
carb.output$CO2 <- carb.output$CO2*1000000 #convert to µmol kg-1
carb.output$HCO3 <- carb.output$HCO3*1000000 #convert to µmol kg-1
carb.output$CO3 <- carb.output$CO3*1000000 #convert to µmol kg-1
carb.output$DIC <- carb.output$DIC*1000000 #convert to µmol kg-1
carb.output <- carb.output[,-c(1,4,5,8,10:13,19)] #subset variables of interest
carb.output <- cbind(SW.chem$Date,  SW.chem$Sample.ID,  SW.chem$Treatment, carb.output) #combine the sample information with the seacarb output
colnames(carb.output) <- c("Date",  "Tank",  "Treatment",	"Salinity",	"Temperature", "pH",	"CO2",	"pCO2","HCO3",	"CO3",	"DIC", "TA",	"Aragonite.Sat") #Rename columns to describe contents

write.table(carb.output, "C:/Users/PPP Lab/Documents/Water_Chemistry/Output/Seawater_chemistry_table_Output_All.csv", sep=",", row.names = FALSE) #save data

# new lines to condence data for the experimental tanks and call the tank ID without the date
target <- c("Elevated", "Ambient") # target values
carb.output <- filter(carb.output, Treatment %in% target) # call the target tanks during the experiment
#carb.output$Tank <- substr(carb.output$Tank, 10,11) # call only the tank number (new line for summer 19 experiments)

carbo.melted <- melt(carb.output) #reshape the dataframe to more easily summarize all output parameters
mean.carb.output <-ddply(carbo.melted, .(Treatment, variable), summarize, #For each subset of a data frame, apply function then combine results into a data frame.
                         N = length(na.omit(value)), #number of records
                         mean = (mean(value)),       #take the average of the parameters (variables) summarized by treatments
                         sem = (sd(value)/sqrt(N))) #calculate the SEM as the sd/sqrt of the count or data length
mean.carb.output # display mean and sem 

#need to separate out tank name from date

# pdf("~/MyProjects/Geoduck_Conditioning/RAnalysis/Output/Water_Chem_withTA.pdf")
# CHECK YOUR Y AXIS!
pdf("C:/Users/samjg/Documents/My_Projects/Inragenerational_thresholds_OA/RAnalysis/Output/Treatment_Water_Chem_withTA.pdf")
par(mfrow=c(3,2))
plot(carb.output$Treatment, carb.output$Temperature, xlab="Treatment", ylab="Temperature°C", ylim=c(16,20), las=2)
plot(carb.output$Treatment, carb.output$pH, xlab="Treatment", ylab="pH Total Scale", ylim=c(6.8,8), las=2)
plot(carb.output$Treatment, carb.output$pCO2, xlab="Treatment", ylab="pCO2 µmol kg-1", ylim=c(350,7000), las=2)
plot(carb.output$Treatment, carb.output$Salinity, xlab="Treatment", ylab="Salinity psu", ylim=c(28,30), las=2)
plot(carb.output$Treatment, carb.output$TA, xlab="Treatment", ylab="Total Alkalinity µmol kg-1", ylim=c(1800,2400), las=2)
plot(carb.output$Treatment, carb.output$Aragonite.Sat, xlab="Treatment", ylab="Aragonite Saturation State", ylim=c(0,1.5), las=2)
dev.off()

pdf("C:/Users/samjg/Documents/My_Projects/Inragenerational_thresholds_OA/RAnalysis/Output/Tank_Water_Chem_withTA.pdf")
par(mfrow=c(3,2))
plot(carb.output$Tank, carb.output$Temperature, xlab="Tank.ID", ylab="Temperature°C", ylim=c(16,20), las=2)
plot(carb.output$Tank, carb.output$pH, xlab="Tank.ID", ylab="pH Total Scale", ylim=c(6.8,8), las=2)
plot(carb.output$Tank, carb.output$pCO2, xlab="Tank.ID", ylab="pCO2 µmol kg-1", ylim=c(350,7000), las=2)
plot(carb.output$Tank, carb.output$Salinity, xlab="Tank.ID", ylab="Salinity psu", ylim=c(28,30), las=2)
plot(carb.output$Tank, carb.output$TA, xlab="Tank.ID", ylab="Total Alkalinity µmol kg-1", ylim=c(1800,2400), las=2)
plot(carb.output$Tank, carb.output$Aragonite.Sat, xlab="Treatment", ylab="Aragonite Saturation State", ylim=c(0,1.5), las=2)
dev.off()

