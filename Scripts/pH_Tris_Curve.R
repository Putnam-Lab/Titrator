# modified for personal Lenovo PC at PT Whitney summer 2019
# modified for PPP Labs (URI) 
# last modification on 20200126 by SJG

# About: Tris calibration plots 
# Change just TWO lines every time you run
# (1) filename <- <yourdate>.csv [Line 13]
# (2) plots/<yourdate>.png       [Line 22]

# set wd
setwd("C:/Users/PPP Lab/Documents/Water_Chemistry/Data/pH_Calibration_Files/") #set working directory
#call file
filename <- "20200126.csv"

# read .csv for mVTris and TTris calibration measurements
Calib.Data <-read.table(filename, header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
coe <- coef(model) #extracts the coeffecients
R2<-summary(model)$r.squared

# plot and save to 'plots' folder
png("plots/20200126.png", 1000, 1000, pointsize=20)
plot(mVTris ~ TTris, data=Calib.Data)
abline(lm(mVTris ~ TTris, data=Calib.Data))
legend('topleft', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()
