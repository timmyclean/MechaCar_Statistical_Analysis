library(dplyr)

MechaCar_mpg <- read.csv("~/MechaCarChallenge.Rscript/MechaCar_mpg.csv")

View(MechaCar_mpg)

Summary(lm(vehicle_length~vehicle_weight,spoiler_angle,ground_clearance,AWD,mpg))

mecha_lm <- lm(mpg~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)

summary(mecha_lm)

Suspension_Coil <- read.csv("~/MechaCarChallenge.Rscript/Suspension_Coil.csv")

total_summary <- Suspension_Coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

lot_sumamry <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI), Variance=var(PSI),SD=sd(PSI),groups='keep')

t.test(Suspension_Coil$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)


