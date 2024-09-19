
setwd("~/Escritorio/College/Q9/MVA/LAB/Session1")
airline <- read.csv2("airline.csv", stringsAsFactors=TRUE)
View(airline)

colnames(airline)

ctm_typ_sat <- airline[, c("satisfaction", "Customer.Type")]
# Create a contingency table from the DF
ctm_table <- table(ctm_typ_sat)
# Joint probability (By rows)
ctm_joint <- prop.table(ctm_table, margin = 1)
# Conditional probabilities (By columns)
ctm_cond <- prop.table(ctm_table, margin = 2)

class_sat <- airline[, c("satisfaction", "Class")]
class_table <- table(class_sat)
# Joint probabilities
class_joint <- prop.table(class_table, margin = 1)
# Conditional probabilities
class_cond <- prop.table(class_table, margin = 2)

# 4. 3-way table (satisfaction, class and customer type)
tbl3 <- xtabs(formula=~satisfaction + Customer.Type + Class, data=airline)
tbl3

flat_tbl3 <- ftable(tbl3)


# 5. Visualize class satisfaction through 2 plots:
ctm_joint
ctm_cond

#Conditional probabilities make much more sense
barplot(class_cond, legend=TRUE, xlab="Class", ylab="Count", main="Satisfaction Vs Airline Class", beside=TRUE)
#barplot(class_joint, legend=TRUE, xlab="Class", ylab="Count", main="Satisfaction Vs Airline Class", beside=TRUE)

mosaicplot(class_joint, xlab="Class", ylab="Count", legend=TRUE, main="Satisfaction Vs Airline Class", col=c("red","yellow", "red"))

# 6. 
library(dplyr)

subset <- airline[, c("Seat.comfort", "Food.and.drink", "Online.support", "Cleanliness")]
df <- data.frame(
  Seat.comfort = mean(airline$Seat.comfort),
  Food.and.drink = mean(airline$Food.and.drink),
  Online.support = mean(airline$Online.support),
  Cleanliness = mean(airline$Cleanliness)
)
summary(subset)
