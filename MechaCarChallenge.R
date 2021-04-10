# using library function
library(dplyr)

# Importing i used  the import dataset button
MechaCar_mpg <- read.csv("~/MechaCarChallenge.Rscript/MechaCar_mpg.csv")

View(MechaCar_mpg)

# linear regression using lm
mecha_lm <- lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)

# summary to determine p value and r squared value
summary(mecha_lm)

# import the other data set
Suspension_Coil <- read.csv("~/MechaCarChallenge.Rscript/Suspension_Coil.csv")

# summarize mean, median, variance and standard D.
total_summary <- Suspension_Coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

# use group_by for each lot
lot_sumamry <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI), Variance=var(PSI),SD=sd(PSI),groups='keep')

# t tests!
t.test(Suspension_Coil$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)


