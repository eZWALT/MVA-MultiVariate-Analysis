############## MVA LAB 2 ###########################################
##### Data Imputation, Visualization and Descriptive Analysis ######
#### Nihan Acar-Denizli, UPC   #####################################
####################################################################

## Open data set "airquality"###
library(datasets)
head(airquality)
summary(airquality)

## Proportion of missing observations in airquality dataset ###
for (i in 1:6){
  print(sum(is.na(airquality[,i]))/length(airquality[,i])*100)
  }

### Descriptive Statistics ####
### RowMeans and ColMeans give the means of values
mu <- colMeans(airquality)
mu
mu <- colMeans(airquality, na.rm = TRUE)
mu

### Omitting NA values #######
air_omit <- na.omit(airquality)
head(air_omit)
summary(air_omit)

plot(air_omit[,1:4])
S <- cov(air_omit[,1:4])
S
R<-cor(air_omit[,1:4])
# After (+/-) 0.5 correlation is more than high
# 0.2 - 0.3 correlation indicates a small linear relationship
R

##########################################
#### Visualize Missing Data Proportions ##
##########################################
install.packages("mice")
library(mice)

## Visualizing missing data patterns by md.pattern function
?md.pattern
md.pattern(airquality)

## Using package VIM ###
install.packages("VIM")
library(VIM)
aggr(airquality)

###Impute NA values in Ozone by the mean
install.packages("Hmisc")
library(Hmisc)
airquality_imp<-airquality

###Impute Ozone ##
airquality_imp$Ozone<-with(airquality_imp,impute(airquality$Ozone,mean))
summary(airquality$Ozone)
aggr(airquality_imp)

cor(airquality_imp$Ozone,airquality$Temp)
plot(airquality_imp$Ozone,airquality$Temp)
plot(airquality$Ozone, airquality$Temp)

########################
#### Data Imputation ###
########################
### Source: https://www.rpubs.com/justjooz/miss_data
################################################################
### Data imputation by using mice package ###
## Multivariate data imputation
install.packages("mi")
library(mi)   
?mice
# mean imputation with one iteration
imp_mice<- mice(airquality, method = "mean", m = 1, maxit = 1) 
imp_mice$chainMean
#imputed values 
imp_mice$imp$Ozone

## Use complete function to complete missing data
?complete
imputed_data<-mice::complete(imp_mice)

plot(density(airquality$Ozone, na.rm=TRUE), ylim=c(0,0.025))
lines(density(airquality_imp$Ozone),ylim=c(0,0.025),col=2)

###Data imputation with regression ###
reg_imp<-mice(airquality, method = "norm.predict", m=1)
air_reg_imp<-mice::complete(reg_imp)

hist(airquality$Ozone)
hist(reg_imp$imp$Ozone[,1],add=TRUE, col=2)

##Stochastic Regression Imputation
data <- airquality[, c("Ozone", "Solar.R")]
imp <- mice(data, method = "norm.nob", m = 1, maxit = 1,
            seed = 1, print = FALSE)
names(imp)
str(imp$imp$Ozone)

## Histograms of original data and imputed data
hist(airquality$Ozone)
hist(imp$imp$Ozone[,1], add=TRUE, col=2)

#### Multiple Imputation ###
?mice
imp <- mice(airquality, seed = 1, m = 20, print = FALSE)
fit <- with(data=imp,lm(Ozone ~ Wind + Temp + Solar.R))
names(fit)
fit$analyses

# Imputation results of first 5 iterations
imp$imp$Ozone
boxplot(imp$imp$Ozone[,1:5])

pool(fit) #gives the pooled result

########################################################

#############################################
######### PovertyStudy Data #################
### Source: http://www-eio.upc.edu/~jan/ ####
#############################################

## Different missing data codifications
## dim, ncol, nrow
library(readr)
pov_data<-read_table("PovertyStudy.dat")
pov_data
summary(pov_data)

sum(pov_data$GNP==-99)
pov_data$GNP[pov_data$GNP==-99] <- NA
sum(is.na(pov_data$GNP))/nrow(pov_data)

## Data imputation with median
pov_med<-median(pov_data$GNP,na.rm=TRUE)
pov_data$GNP[is.na(pov_data$GNP)]<-pov_med
pov_data

##############################
### Box-Cox Transformation ###
##############################

hist(pov_data$GNP)

library(MASS)
boxcox(pov_data$GNP~1,lambda=seq(-1,1,by=0.1))

boxplot(pov_data$GNP, main="Distribution of GNP")
lGNP<-log(pov_data$GNP)
boxplot(lGNP, main="Distribution of Log GNP")

############################
#### Data Visualization ####
############################

## Visualization of Numerical Variables
## Chernoff Faces
install.packages("aplpack")
library(aplpack)
faces()

?faces
data(longley)
faces(longley[1:9,],face.type=0)
faces(longley[1:9,],face.type=1)

## Poverty Data Visualization
summary(pov_data)
faces(pov_data[1:6,1:5],face.type = 0)
head(pov_data)

options(digits = 2)
cov(pov_data[,1:6])
cor(pov_data[,1:6])

plot(pov_data[,1:5])

### Star Plots
?stars
stars(pov_data[,1:5])

### Corplot
install.packages("corrplot")
library(corrplot)

cr<-cor(pov_data[,1:6])
corrplot(cr)
corrplot(cr, method="number")

### Visualization of a Numerical and a Categorical Variable
### Boxplots ###
boxplot(count ~ spray, data = InsectSprays, xlab = "Insect Spray Brand", ylab ="Insect count", main=" Number of insects killed by sprays")

###############################################################################

##################################
##### Factor Data and Tables #####
##################################

?mtcars
summary(mtcars)
View(mtcars)

for (i in 8:9){
  mtcars[,i]<-as.factor(mtcars[,i])
}

summary(mtcars)
levels(mtcars[,8])<-c("V-shaped","straight")
levels(mtcars[,9])<-c("automatic","manual")

## Cross Table ##
tab<-table(mtcars$vs,mtcars$am)
tab

?prop.table

#Joint Probability
prop.table(tab)

#Marginal Probabilities
colSums(prop.table(tab))
rowSums(prop.table(tab))

##########################
#Conditional Probabilities
##########################

# Row Profile
prop.table(tab,1)

#Column Profile
prop.table(tab,2)

## Visualization of categorical variables
## Barplots
?barplot
barplot(prop.table(tab))
barplot(prop.table(tab),col=c("red","blue"),legend.position="top",legend.text = c("vshaped","straight"),main="Distribution of Satisfaction Level",ylim=c(0,0.7))
barplot(prop.table(tab,2),col=c("red","blue"),legend.position="top",beside=TRUE, legend.text = c("vshaped","straight"),main="Distribution of Satisfaction Level",ylim=c(0,1))

## Mozaic Plots
?mosaicplot
mosaicplot(tab,main="Transmission Type vs. Engine Shape", color=c("red","blue"))

