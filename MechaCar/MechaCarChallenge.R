install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
#Read Mechacar Data
mechacar_data <- read.csv(file='MechaCar_mpg.csv')
head(mechacar_data)


#Correlation table
mpg_matrix <- as.matrix(mechacar_data[,c('vehicle.length', 'vehicle.weight', 'spoiler.angle', 'ground.clearance', 'AWD', 'mpg')])
cor(mpg_matrix)

#Create Multiple linear regressoin summary
summary(lm(mpg ~ vehicle.length + ground.clearance,data=mechacar_data)) 

#Read Coil cuspension data
coil_data <- read.csv(file='Suspension_Coil.csv')
head(coil_data)

#Coil statistical summary
summarize_coil <- coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI), Stand_Dev_PSI=sd(PSI), Var_PSI=var(PSI))

#Coil T-Test
t.test(coil_data$PSI, mu=1500)
