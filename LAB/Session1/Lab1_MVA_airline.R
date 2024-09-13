
setwd("~/Escritorio/College/Q9/Multivariate-Analysis/LAB/Session1")
airline <- read.csv2("airline.csv", stringsAsFactors=TRUE)
View(airline)

colnames(airline)

ctm_typ_sat <- airline[, c("satisfaction", "Customer.Type")]
# Create a contingency table from the DF
ctm_table <- table(ctm_typ_sat)
# Joint probability (By rows)
prop.table(ctm_table, margin = 1)
# Conditional probabilities (By columns)
prop.table(ctm_table, margin = 2)

class_sat <- airline[, c("satisfaction", "Class")]
class_table <- table(class_sat)
# Joint probabilities
prop.table(class_table, margin = 1)
# Conditional probabilities
prop.table(class_table, margin = 2)

# 3-way table (satisfaction, class and customer type)
tbl3 <- xtabs(formula=~satisfaction + Customer.Type + Class, data=airline)
tbl3

flat_tbl3 <- ftable(tbl3)


