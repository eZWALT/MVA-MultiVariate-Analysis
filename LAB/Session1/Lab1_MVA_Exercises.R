####################################################
##### MVA Lab 1: Introduction to Data Analysis ####
#####  Nihan Acar-Denizli, UPC #####################
####################################################

########################
#### Data Importing ####
########################

### Q1: Importing data set from .csv file to R ###
?read.csv2
cars<-read.csv2("cars.csv",stringsAsFactors = TRUE)
View(cars)
cars<-cars[,-1]

#Q2:Summarize the data. 
### How many quantitative and qualitative variables are there in the data set?
str(cars)
summary(cars)

## Converting year and number of cylinders (ITS IS CATEGORICAL DATA!!) to factor and summarize data
cars$year<-as.factor(cars$year)
cars$cylinders<-as.factor(cars$cylinders)
summary(cars)

## Get the brands of cars that have either 3 or 5 cylinders
cars$brand[cars$cylinders==3|cars$cylinders==5]

#Q3: Visualize the distribution of the variables year and brand.
barplot(summary(cars$year),ylab="Frequency")
barplot(summary(cars$brand))

#Q4: Classify year into three levels as 1971-1975, 1976-1979, 1980-1983. Construct a contingency table cross classifying year and brand variables.
## USE THIS TIME VARIABLES INTO YEAR
cars$year<-as.numeric(as.character(cars$year))
cars$year.cat<-cut(cars$year, breaks=c(1970,1975,1979,1983))

t<-table(cars$year.cat,cars$brand)
t
prop.table(t)
prop.table(t,1)
prop.table(t,2)

#Q5:a) Visualize the distribution of mpg, hp and weight of the cars by using a proper chart.
## b) Interpret the graphs. 
hist(cars$mpg, main = "Histogram of SUS", xlab = "mpg (SUSSY)", ylab = "Frequency (SUSSIER)")
hist(cars$hp)
hist(cars$weightlbs)


#Q6: Construct a descriptive statistics table with mean, median and standard deviation for the variables mpg, hp and weight. 
for(i in 2:6){
  cars[,i]<-as.numeric(cars[,i])
}
str(cars)
apply(cars[,c(1,4,5)],2,mean,na.rm=TRUE)

options(digits = 3)
avr<-apply(cars[,2:6],2,mean,na.rm=TRUE)
med<-apply(cars[,2:6],2,median,na.rm=TRUE)
std<-apply(cars[,2:6],2,sd,na.rm=TRUE)

tab<-rbind.data.frame(avr,med,std)
rownames(tab)<-c("mean","median","std. dev.")
colnames(tab)<-names(avr)
tab

#Q6: Compare the distribution of weight across car brands. Visualize it by using boxplot.
?tapply

cars$brand
tapply(cars)

tapply(cars$weightlbs,cars$brand,summary)

?boxplot
boxplot(cars$weightlbs~cars$brand)
