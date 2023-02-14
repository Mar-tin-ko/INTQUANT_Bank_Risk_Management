# Case Study Group: Martin, Linlin, Bartosz 

# Install and load packages
install.packages('ROCR')
library(ROCR)

##### CLEAR VARIABLES & LOAD THE CUSTOMER DATA #####  
rm(list= ls())
data <- read.table("E:/Case_study/O1_IP2_Iasi_Data.csv", header=TRUE, sep = ";", na.strings = c("","."))

##### CLEARING THE DATABASE #####  
## Neglecting attributes with missing value fraction > 5% threshold
low.dq <- apply(data, 2, function(x) sum(is.na(x))/nrow(data)*100) > 5
names(data[low.dq]) # Attributes containing missing values above threshold
names(data[!low.dq]) # Attributes containing missing values below threshold

delete.cols1 <- c(names(data[low.dq])) # Deleting attributes above threshold
data <- data[, ! names(data) %in% delete.cols1, drop = F]

## Identifying variables with negligible explanatory power and their elimination
sum(data$PENSION_FUND)/nrow(data) # Is 0%, meaning no clients used pension fund products -> no explanatory power
sum(data$DEPOSIT)/nrow(data) # Only 0.35% of customers have active deposits out of the full data sample -> no explanatory power
sum(data$FOREIGN_ACCOUNT)/nrow(data) # Only 0.01% of customers have active foreign accounts out of the full data sample -> no explanatory power
sum(data$SAVING_ACCOUNT)

data$PENSION_FUNDS <- NULL # Deleting
data$DEPOSIT <- NULL
data$FOREIGN_ACCOUNT <- NULL
data$SAVING_ACCOUNT <- NULL

## Identifying + managing outliers
boxplot(data$INCOME)
upper.value <- quantile(data$INCOME, 0.999)
upper.value
for (i in 1:nrow(data)) {
  if (data$INCOME[i] > upper.value) {data$INCOME[i] <- upper.value
  }
}
boxplot(data$INCOME)


# CONVERTONG AND RENAMING VARIABLE "BIRTH_DATE" TO "AGE" (AT THE TIME OF PRODUCT REQUEST)
birth.date <- as.Date(data$BIRTH_DATE, origin="1899-12-30")
age <- round((data$REQUEST_DATE - data$BIRTH_DATE)/365, 0)
colnames(data)[4] <- "AGE"
data[,4] <- as.numeric(age)
data$REQUEST_DATE <- NULL 

##### ANALIZING THE CLEANED FULL DATA SAMPLE #####  
length(data)
length(data[,1])
apply(data, 2, function(x) length(unique(x)))
apply(data, 2, function(x) sum(is.na(x)))

table (data$AREA) ## Shows number of observations for each attribute in a variable
table (data$PRODUCT)
table (data$RESIDENTIAL_PLACE)

table (data$FINALIZED_LOAN)
table (data$DEPOSIT)

table (data$AREA) * nrow(data)^-1 ## Gives the shares of each attribute in a variable

round(table(data$DEFAULT_FLAG)/nrow(data), 4)
 
##### TRAINING AND TESTING SAMPLE #####  
train.list <- sort(sample(1:nrow(data), 0.75*nrow(data), replace=FALSE, prob=NULL))
train.counter <- 1
test.counter <- 1
training <- data[1:(0.75*nrow(data)),] # Initialization
testing <- data[1:(nrow(data)-0.75*nrow(data)),] # Initialization
for(i in 1:nrow(data)) {
  if (i == train.list[train.counter]) {
    training[train.counter,] <- data[i,]
    train.counter <- train.counter + 1
  }
  else {
    testing[test.counter,] <- data[i,]
    test.counter <- test.counter + 1
  }
}


##### DEFINING RANGES - INCOME #####

GroupingIncome <- quantile(training$INCOME, probs = seq(0,1,0.125),names = FALSE) 

m <- as.matrix(GroupingIncome)

d <- vector(mode="numeric",length=0) # Define empty vector
t <- vector(mode="numeric",length=0) # Define empty vector
nm <- vector(mode="numeric",length=0) # Define empty sumvector

ndata <- subset(training,select=c("INCOME","DEFAULT_FLAG")) ## data frame

for (i in 1:(nrow(m)-1)){
  dn <- sum(ndata[ndata$INCOME<=m[i+1,1],2])- sum(ndata[ndata$INCOME<m[i,1],2])
  tn <- length(ndata[ndata$INCOME<=m[i+1,1],2])- length(ndata[ndata$INCOME<m[i,1],2])    
  
  d <- append(d,dn)
  t <- append(t,tn)
  
}

nwm <- cbind(t,d) # new matrix
nwm

observationsIncome <- t(nwm)
observationsIncome

##### WEIGHT OF EVIDENCE - INCOME (simple) #####

WE <- function(observationsIncome, labels=NULL, details=TRUE,
               iv.output=TRUE) {
  observationsIncome <- matrix(c(nrow(training), sum(data$DEFAULT_FLAG)), nrow=2, ncol=1)
  
  if (!is.matrix(observationsIncome)) stop ('observations must be a matrix') 
  if (nrow(observationsIncome) != 2) stop ('observations must have two columns') 
  if (!is.null(labels)) { 
    if (!is.matrix(labels)) { 
      if (ncol(observationsIncome) != length(labels)) 
        stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations') }
  } 
  else stop ('labels must be a string vector, not a matrix') 
}
# Calculation of the WEs for Income
we <- matrix(0, ncol=ncol(observationsIncome), nrow=1) # initialisation 
for (i in 1:ncol(observationsIncome)) { 
  we[1, i] <- log((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,])-sum(observationsIncome[2,])) ^ -1 * (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1) ^ -1) 
}
we
barplot(we)


##### Information Value for Income #####
# Calculation of the IV
iv.output=TRUE
if (iv.output) {
  iv <- 0  # initialisation
  for (i in 1:ncol(observationsIncome)) {
    iv <- iv + ((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,]) - sum(observationsIncome[2,])) ^ -1 -
                  (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1)) *
      we[1,i]
  }
}

##### WEIGHTS OF EVIDENCE - INCOME (details) #####
WE <- function(observationsIncome, labels=NULL, details=TRUE, iv.output=TRUE) {
  # Author: Christian Cech
  # Date: 2016
  # From: lecture material
  # Functionality: Returns the Weights of Evidence (WE) or the Information Value (IV) for categorical data
  #
  # Args:
  #   observations: two-rowed data matrix that contains the number of observations (row 1) and the number of defaults (row 2) for every attribute. The values in the different columns contain the values for the different attributes.
  #   labels: a string array with the labels of the attributes.
  #   details: if details is FALSE only a vector of the WE-values respectively an IV-value are returned. If details is TRUE a more detailed table is returned. The table contains: (i) the number of observations per attrtibute and the total number of observations in the righmost column, (ii) the number of non-derfaults, (iii) the number of defaults, (iv) the fraction of non-defaults, (v) the fraction of defaults, (vi) the WEs. If iv.ouput is TRUE the table additionally contains the "contribution" per attribute for the IV and the IV (i.e. the sum of the "contributions") in the rightmost column.
  #   if iv.output is FALSE then the WE-values are returned. If IV is true then the IV-value is returned.
  #
  # Returns:
  #   the Weights of Evidence (WE) or the Information Value (IV) for categorical data.
  
  # Compatibility checks:
  if (!is.matrix(observationsIncome)) stop ('observations must be a matrix')
  if (nrow(observationsIncome) != 2) stop ('observations must have two columns')
  if (!is.null(labels)) {
    if (!is.matrix(labels)) {
      if (ncol(observationsIncome) != length(labels)) stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations')
    }
    else stop ('labels must be a string vector, not a matrix')
  }
  
  # Calculation of the WEs
  we <- matrix(0, ncol=ncol(observationsIncome), nrow=1)  # Initialization
  for (i in 1:ncol(observationsIncome)) {
    we[1, i] <- log((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,])-sum(observationsIncome[2,])) ^ -1 *
                      (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1) ^ -1)
  }
  
  # Calculation of the IV
  if (iv.output) {
    iv <- 0  # Initialization
    for (i in 1:ncol(observationsIncome)) {
      iv <- iv + ((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,]) - sum(observationsIncome[2,])) ^ -1 -
                    (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1)) *
        we[1,i]
    }
  }
  
  # Return results
  if (!details) {
    if (!iv.output) {
      if (!is.null(labels)) {
        colnames(we) <- labels  # Labels not NULL, details=FALSE, iv.output=FALSE
      }
      return(we)  # Details=FALSE, iv.output=FALSE
    }
    else {
      return(iv) # Details=FALSE, iv.output=TRUE
    }
  }
  else {
    result.table <- matrix(0, ncol=(ncol(observationsIncome)+1), nrow=6)  # Initialization
    if (is.null(labels)) {
      colnames(result.table) <- c(1:ncol(observationsIncome), "total")
    }
    else {
      colnames(result.table) <- c(labels, "total")
    }
    rownames(result.table) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE")
    result.table[1,] <- c(observationsIncome[1,], sum(observationsIncome[1,]))  # Number of observations
    result.table[2,] <- c(observationsIncome[1,] - observationsIncome[2,], sum(observationsIncome[1,]) - sum(observationsIncome[2,]))  # number of non-defaults
    result.table[3,] <- c(observationsIncome[2,], sum(observationsIncome[2,]))  # Number of defaults
    result.table[4,] <- result.table[2,] * result.table[2,ncol(result.table)] ^ -1  # Fraction of non-defaults
    result.table[5,] <- result.table[3,] * result.table[3,ncol(result.table)] ^ -1  # Fraction of defaults
    result.table[6,] <- c(we, 0)  # WEs
    if (!iv.output) {  # WE-table
      return(result.table)
    }
    else {  # IV-table
      result.table2 <- matrix(0, ncol=(ncol(observationsIncome)+1), nrow=7)  # initialisation
      colnames(result.table2) <- colnames(result.table)
      rownames(result.table2) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE", "IV")
      result.table2[1:6,] <- result.table
      result.table2[7,] <- c((result.table2[4, 1:(ncol(result.table2) - 1)] - result.table2[5, 1:(ncol(result.table2) - 1)]) * we, iv)  # "IV-contributions" and in the right-most columne IV. (Remark: piecewise multiplication)
      return(result.table2)
    }
  }
}
WE(observationsIncome, label=NULL, details=TRUE, iv.output=TRUE)


### WEIGHTS OF EVIDENCE - MARITAL STATUS
table(training$MARITAL_STATUS,training$DEFAULT_FLAG)
matrix(c(table(training$MARITAL_STATUS),table(training$MARITAL_STATUS,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_marital_status <- WE(matrix(c(table(training$MARITAL_STATUS), table(training$MARITAL_STATUS, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$MARITAL_STATUS, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_marital_status

### WEIGHTS OF EVIDENCE - EDUCATION
WOE_education <- WE(matrix(c(table(training$EDUCATION), table(training$EDUCATION, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$EDUCATION, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_education

### SALARY_ACCOUNT
table(training$SALARY_ACCOUNT,training$DEFAULT_FLAG)
matrix(c(table(training$SALARY_ACCOUNT),table(training$SALARY_ACCOUNT,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_salary_account <-WE(matrix(c(table(training$SALARY_ACCOUNT), table(training$SALARY_ACCOUNT, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$SALARY_ACCOUNT, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_salary_account
  
### DEBIT_CARD
table(training$DEBIT_CARD,training$DEFAULT_FLAG)
matrix(c(table(training$DEBIT_CARD),table(training$DEBIT_CARD,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_debit_card <- WE(matrix(c(table(training$DEBIT_CARD), table(training$DEBIT_CARD, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$DEBIT_CARD, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_debit_card

### CURRENT_ACCOUNT
table(training$CURRENT_ACCOUNT,training$DEFAULT_FLAG)
matrix(c(table(training$CURRENT_ACCOUNT),table(training$CURRENT_ACCOUNT,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_current_account <- WE(matrix(c(table(training$CURRENT_ACCOUNT), table(training$CURRENT_ACCOUNT, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$CURRENT_ACCOUNT, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_current_account

### FINALIZED_LOAN
table(training$FINALIZED_LOAN,training$DEFAULT_FLAG)
matrix(c(table(training$FINALIZED_LOAN),table(training$FINALIZED_LOAN,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_finalized_loan <- WE(matrix(c(table(training$FINALIZED_LOAN), table(training$FINALIZED_LOAN, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$FINALIZED_LOAN, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_finalized_loan

### AREA
table(training$AREA,training$DEFAULT_FLAG)
matrix(c(table(training$AREA),table(training$AREA,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_area <- WE(matrix(c(table(training$AREA), table(training$AREA, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$AREA, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_area

### PRODUCT
table(training$PRODUCT,training$DEFAULT_FLAG)
matrix(c(table(training$PRODUCT),table(training$PRODUCT,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_product <- WE(matrix(c(table(training$PRODUCT), table(training$PRODUCT, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$PRODUCT, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_product

### RESIDENTIAL_PLACE
table(training$RESIDENTIAL_PLACE,training$DEFAULT_FLAG)
matrix(c(table(training$RESIDENTIAL_PLACE),table(training$RESIDENTIAL_PLACE,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_residential_place <- WE(matrix(c(table(training$RESIDENTIAL_PLACE), table(training$RESIDENTIAL_PLACE, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$RESIDENTIAL_PLACE, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_residential_place

### HOUSEHOLD_MEMBERS
table(training$HOUSEHOLD_MEMBERS,training$DEFAULT_FLAG)
matrix(c(table(training$HOUSEHOLD_MEMBERS),table(training$HOUSEHOLD_MEMBERS,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_household_members <- WE(matrix(c(table(training$HOUSEHOLD_MEMBERS), table(training$HOUSEHOLD_MEMBERS, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$HOUSEHOLD_MEMBERS, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_household_members

### DISTRICT
table(training$DISTRICT,training$DEFAULT_FLAG)
matrix(c(table(training$DISTRICT),table(training$DISTRICT,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_district <- WE(matrix(c(table(training$DISTRICT), table(training$DISTRICT, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$DISTRICT, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_district

### DISTRICT
table(training$PROFESSION,training$DEFAULT_FLAG)
matrix(c(table(training$PROFESSION),table(training$PROFESSION,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_profession <- WE(matrix(c(table(training$PROFESSION), table(training$PROFESSION, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$PROFESSION, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_profession

### BIRTH_PLACE
table(training$BIRTH_PLACE,training$DEFAULT_FLAG)
matrix(c(table(training$BIRTH_PLACE),table(training$BIRTH_PLACE,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_birth_place <- WE(matrix(c(table(training$BIRTH_PLACE), table(training$BIRTH_PLACE, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$BIRTH_PLACE, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_birth_place


### NO_OF_DEPENDENTS
table(training$NO_OF_DEPENDENTS,training$DEFAULT_FLAG)
matrix(c(table(training$NO_OF_DEPENDENTS),table(training$NO_OF_DEPENDENTS,training$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_no_of_dependents <- WE(matrix(c(table(training$NO_OF_DEPENDENTS), table(training$NO_OF_DEPENDENTS, training$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(training$NO_OF_DEPENDENTS, training$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_no_of_dependents


##### DEFINING RANGES - AGE ##### 
GroupingAge <- quantile(training$AGE, probs = seq(0,1,0.2),names = FALSE) # don't want the name use false

m1 <- as.matrix(GroupingAge)

d1 <- vector(mode="numeric",length=0) # Define empty vector
t1 <- vector(mode="numeric",length=0) # Define empty vector
nm1 <- vector(mode="numeric",length=0) # Define empty sumvector

ndata1 <- subset(training, select=c("AGE","DEFAULT_FLAG")) # Data frame

for (i in 1:(nrow(m1)-1)){
  dn1 <- sum(ndata[ndata$AGE<=m1[i+1,1],2])- sum(ndata1[ndata1$AGE<m1[i,1],2])
  tn1 <- length(ndata[ndata$AGE<=m1[i+1,1],2])- length(ndata1[ndata1$AGE<m1[i,1],2])    
  
  d1 <- append(d1,dn1)
  t1 <- append(t1,tn1)
  
}

nwm1 <- cbind(t1,d1) # new matrix
nwm1

observationsAge <- t(nwm1)
observationsAge

##### WEIGHT OF EVIDENCE - AGE (simple) ##### 

WE <- function(observationsAge, labels=NULL, details=FALSE,
               iv.output=FALSE) {
  observationsAge <- matrix(c(nrow(training), sum(data$DEFAULT_FLAG)), nrow=2, ncol=1)
  
  if (!is.matrix(observationsAge)) stop ('observations must be a matrix') 
  if (nrow(observationsAge) != 2) stop ('observations must have two columns') 
  if (!is.null(labels)) { 
    if (!is.matrix(labels)) { 
      if (ncol(observationsAge) != length(labels)) 
        stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations') }
  } 
  else stop ('labels must be a string vector, not a matrix') 
}

# Calculation of the WEs for Age
we <- matrix(0, ncol=ncol(observationsAge), nrow=1) # Initialization 
for (i in 1:ncol(observationsAge)) { 
  we[1, i] <- log((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,])-sum(observationsAge[2,])) ^ -1 * (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1) ^ -1) 
}
we



##### WEIGHT OF EVIDENCE - AGE (details) ##### 

WE <- function(observationsAge, labels=NULL, details=TRUE, iv.output=TRUE) {
  
  
  # Compatibility checks:
  if (!is.matrix(observationsAge)) stop ('observations must be a matrix')
  if (nrow(observationsAge) != 2) stop ('observations must have two columns')
  if (!is.null(labels)) {
    if (!is.matrix(labels)) {
      if (ncol(observationsAge) != length(labels)) stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations')
    }
    else stop ('labels must be a string vector, not a matrix')
  }
  
  # Calculation of the WEs
  we <- matrix(0, ncol=ncol(observationsAge), nrow=1)  # Initialization
  for (i in 1:ncol(observationsAge)) {
    we[1, i] <- log((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,])-sum(observationsAge[2,])) ^ -1 *
                      (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1) ^ -1)
  }
  
  # Calculation of the IV
  if (iv.output) {
    iv <- 0  # Initialization
    for (i in 1:ncol(observationsAge)) {
      iv <- iv + ((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,]) - sum(observationsAge[2,])) ^ -1 -
                    (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1)) *
        we[1,i]
    }
  }
   
  # Return results
  if (!details) {
    if (!iv.output) {
      if (!is.null(labels)) {
        colnames(we) <- labels  # labels not NULL, details=FALSE, iv.output=FALSE
      }
      return(we)  # Details=FALSE, iv.output=FALSE
    }
    else {
      return(iv) # Details=FALSE, iv.output=TRUE
    }
  }
  else {
    result.table <- matrix(0, ncol=(ncol(observationsAge)+1), nrow=6)  # Initialization
    if (is.null(labels)) {
      colnames(result.table) <- c(1:ncol(observationsAge), "total")
    }
    else {
      colnames(result.table) <- c(labels, "total")
    }
    rownames(result.table) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE")
    result.table[1,] <- c(observationsAge[1,], sum(observationsAge[1,]))  # Number of observations
    result.table[2,] <- c(observationsAge[1,] - observationsAge[2,], sum(observationsAge[1,]) - sum(observationsAge[2,]))  # Number of non-defaults
    result.table[3,] <- c(observationsAge[2,], sum(observationsAge[2,]))  # Number of defaults
    result.table[4,] <- result.table[2,] * result.table[2,ncol(result.table)] ^ -1  # Fraction of non-defaults
    result.table[5,] <- result.table[3,] * result.table[3,ncol(result.table)] ^ -1  # Fraction of defaults
    result.table[6,] <- c(we, 0)  # WEs
    if (!iv.output) {  # WE-table
      return(result.table)
    }
    else {  # IV-table
      result.table2 <- matrix(0, ncol=(ncol(observationsAge)+1), nrow=7)  # Initialization
      colnames(result.table2) <- colnames(result.table)
      rownames(result.table2) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE", "IV")
      result.table2[1:6,] <- result.table
      result.table2[7,] <- c((result.table2[4, 1:(ncol(result.table2) - 1)] - result.table2[5, 1:(ncol(result.table2) - 1)]) * we, iv)  # "IV-contributions" and in the right-most columne IV. (Remark: piecewise multiplication)
      return(result.table2)
    }
  }
}
WE(observationsAge, label=NULL, details=TRUE, iv.output=TRUE)



##### CHECKING NO OF DEFAULTS BETWEEN ARBITRARY AGE RANGE ##### 
flag.sum <- 0
for (i in 1:nrow(data)) {
  if (data$AGE[i] >= 18 & data$AGE[i]  < 74)  {
    flag.sum <- flag.sum + data$DEFAULT_FLAG[i]
  }
}
flag.sum
flag.sum/sum(data$DEFAULT_FLAG)
sum(data$DEFAULT_FLAG)


##### BASIC DESCRIPTIVE STATISTICS OF TRAINING SAMPLE ##### 
product.pie <- pie(table(training$PRODUCT)*nrow(training)^-1)
table(training$PRODUCT)/nrow(training)*100 # Percentage of clients per product type

marital.status_pie <- pie(table(training$MARITAL_STATUS))
table(training$MARITAL_STATUS)/nrow(data)*100 # Percentage of clients per marital status

education_pie <- pie(table(training$EDUCATION))
table(training$EDUCATION)/nrow(training)*100 # Percentage of clients per education level

household.members_pie <- pie(table(training$HOUSEHOLD_MEMBERS))
table(training$HOUSEHOLD_MEMBERS)/nrow(training)*100  # Percentage of clients per household members

area_pie <- pie(table(training$AREA))
table(training$AREA)/nrow(training)*100 # Percentage of clients per area

# Variable AGE  
hist(training$AGE)
mean(training$AGE)
max(training$AGE)
min(training$AGE)
boxplot(training$AGE)
plot(training$AGE, training$DEFAULT_FLAG)
quantile(training$AGE, c(0.25, 0.5, 0.75, 1))
plot(density(training$AGE, bw=0.004))

# Variable INCOME
mean(training$INCOME)
max(training$INCOME)
min(training$INCOME)
hist(training$INCOME)
plot(density(training$INCOME, bw=0.004))


# CALCULATE THE CORRELATION
apply(training, 2, function(x) length(unique(x))) # Check unique values per variable

training["AREA_CODED"] <- NA # Creating new column
training$AREA_CODED <- as.numeric(factor(training$AREA, labels=c(1:3))) # Transforming categorical variable to numerical one
class(training$AREA_CODED) # Check the variable type
training$AREA_CODED[is.na(training$AREA_CODED)] <- 0

training["DISTRICT_CODED"] <- NA # Creating new column
training$DISTRICT_CODED <- as.numeric(factor(training$DISTRICT, labels=c(1:42))) # Transforming categorical variable to numerical one
class(training$DISTRICT_CODED) # Check the variable type
training$DISTRICT_CODED[is.na(training$DISTRICT_CODED)] <- 0

training["EDUCATION_CODED"] <- NA # Creating new column
training$EDUCATION_CODED <- as.numeric(factor(training$EDUCATION, labels=c(1, 2,3,4,5,6,7,8,9))) # Transforming categorical variable to numerical one
class(training$EDUCATION_CODED) # Check the variable type
training$EDUCATION_CODED[is.na(training$EDUCATION_CODED)] <- 0

training["RESIDENTIAL_PLACE_CODED"] <- NA # Creating new column
training$RESIDENTIAL_PLACE_CODED <- as.numeric(factor(training$RESIDENTIAL_PLACE, labels=c(1, 2,3,4,5,6,7))) # Transforming categorical variable to numerical one
class(training$RESIDENTIAL_PLACE_CODED) # Check the variable type
training$RESIDENTIAL_PLACE_CODED[is.na(training$RESIDENTIAL_PLACE_CODED)] <- 0

cor(data.matrix(training[,2:22]))


##### LOGIT REGRESSION ##### 

### DIVIDING INCOME INTO RANGES FOR REGRESSION
quantile(training$INCOME, probs = seq(0,1,0.125),names = FALSE)

training["INCOME_RANGES"] <- NA # Creating new column
for (i in 1:nrow(training)) { 
  if (training$INCOME[i] <= quantile(training$INCOME, 0.125)) {
    training$INCOME_RANGES[i] <- "<= 815"
  }  
  else if (training$INCOME[i] > quantile(training$INCOME, 0.125) & training$INCOME[i] <= quantile(training$INCOME, 0.25)) {
    training$INCOME_RANGES[i] <- "> 815 & <= 1007"
  }  
  else if (training$INCOME[i] > quantile(training$INCOME, 0.25) & training$INCOME[i] <= quantile(training$INCOME, 0.375)) {
    training$INCOME_RANGES[i] <- "> 1007 & <= 1195.906"
  } 
  else if (training$INCOME[i] > quantile(training$INCOME, 0.375) & training$INCOME[i] <= quantile(training$INCOME, 0.5)) {
    training$INCOME_RANGES[i] <- "> 1195.906 & <= 1431.85"
  }  
  else if (training$INCOME[i] > quantile(training$INCOME, 0.5) & training$INCOME[i] <= quantile(training$INCOME, 0.625)) {
    training$INCOME_RANGES[i] <- "> 1431.85 & <= 1780"
  } 
  else if (training$INCOME[i] > quantile(training$INCOME, 0.625) & training$INCOME[i] <= quantile(training$INCOME, 0.75)) {
    training$INCOME_RANGES[i] <- "> 1780 & <= 2280.039"
  } 
  else if (training$INCOME[i] > quantile(training$INCOME, 0.75) & training$INCOME[i] <= quantile(training$INCOME, 0.875)) {
    training$INCOME_RANGES[i] <- "> 2280 & <= 3184.943"
  } 
  else if (training$INCOME[i] > quantile(training$INCOME, 0.875) & training$INCOME[i] <= quantile(training$INCOME, 1)) {
    training$INCOME_RANGES[i] <- "> 3184.943"
  }
}
class(training$INCOME_RANGES)

### DIVIDING AGE INTO RANGES FOR REGRESSION
quantile(training$AGE, probs = seq(0,1,0.2),names = FALSE)

training["AGE_RANGES"] <- NA # Creating new column
for (i in 1:nrow(training)) { 
  if (training$AGE[i] <= quantile(training$AGE, 0.2)) {
    training$AGE_RANGES[i] <- "<= 29"
  }  
  else if (training$AGE[i] > quantile(training$AGE, 0.2) & training$AGE[i] <= quantile(training$AGE, 0.4)) {
    training$AGE_RANGES[i] <- "> 29 & <= 37"
  }  
  else if (training$AGE[i] > quantile(training$AGE, 0.4) & training$AGE[i] <= quantile(training$AGE, 0.6)) {
    training$AGE_RANGES[i] <- "> 37 & <= 45"
  } 
  else if (training$AGE[i] > quantile(training$AGE, 0.6) & training$AGE[i] <= quantile(training$AGE, 0.8)) {
    training$AGE_RANGES[i] <- "> 45 & <= 58"
  }  
  else if (training$AGE[i] > quantile(training$AGE, 0.8) & training$AGE[i] <= quantile(training$AGE, 1)) {
    training$AGE_RANGES[i] <- "> 58 & <= 74"
  }
}
class(training$AGE_RANGES)


### RUNNING THE REGRESION
modelfinal1 <- glm(DEFAULT_FLAG ~ INCOME_RANGES + AGE_RANGES + EDUCATION + RESIDENTIAL_PLACE + FINALIZED_LOAN  ,family=binomial(link="logit"), data=training)
summary(modelfinal1)

modelfinal2 <- glm(DEFAULT_FLAG ~ INCOME + AGE + EDUCATION + RESIDENTIAL_PLACE + FINALIZED_LOAN + HOUSEHOLD_MEMBERS,family=binomial(link="logit"), data=training)
summary(modelfinal2)

predict(modelfinal, data.frame(AGE=44, INCOME=20000), type="response") # prediction


##### DEFINING RANGES - INCOME ##### 
GroupingIncome <- quantile(testing$INCOME, probs = seq(0,1,0.125),names = FALSE) 

m <- as.matrix(GroupingIncome)

d <- vector(mode="numeric",length=0) #define empty vector
t <- vector(mode="numeric",length=0) #define empty vector
nm <- vector(mode="numeric",length=0) #define empty sumvector

ndata <- subset(testing,select=c("INCOME","DEFAULT_FLAG")) ## data frame

for (i in 1:(nrow(m)-1)){
  dn <- sum(ndata[ndata$INCOME<=m[i+1,1],2])- sum(ndata[ndata$INCOME<m[i,1],2])
  tn <- length(ndata[ndata$INCOME<=m[i+1,1],2])- length(ndata[ndata$INCOME<m[i,1],2])    
  
  d <- append(d,dn)
  t <- append(t,tn)
}

nwm <- cbind(t,d) # new matrix
nwm

observationsIncome <- t(nwm)
observationsIncome

##### WEIGHT OF EVIDENCE - INCOME (simple) ##### 

WE <- function(observationsIncome, labels=NULL, details=TRUE,
               iv.output=TRUE) {
  observationsIncome <- matrix(c(nrow(testing), sum(data$DEFAULT_FLAG)), nrow=2, ncol=1)
  
  if (!is.matrix(observationsIncome)) stop ('observations must be a matrix') 
  if (nrow(observationsIncome) != 2) stop ('observations must have two columns') 
  if (!is.null(labels)) { 
    if (!is.matrix(labels)) { 
      if (ncol(observationsIncome) != length(labels)) 
        stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations') }
  } 
  else stop ('labels must be a string vector, not a matrix') 
}
# Calculation of the WEs for Income
we <- matrix(0, ncol=ncol(observationsIncome), nrow=1) # Initialization 
for (i in 1:ncol(observationsIncome)) { 
  we[1, i] <- log((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,])-sum(observationsIncome[2,])) ^ -1 * (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1) ^ -1) 
}
we
barplot(we)


##### Information Value for Income ##### 
# Calculation of the IV
iv.output=TRUE
if (iv.output) {
  iv <- 0  # Initialization
  for (i in 1:ncol(observationsIncome)) {
    iv <- iv + ((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,]) - sum(observationsIncome[2,])) ^ -1 -
                  (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1)) *
      we[1,i]
  }
}

##### WEIGHTS OF EVIDENCE - INCOME (details) ##### 
WE <- function(observationsIncome, labels=NULL, details=TRUE, iv.output=TRUE) {
  # Author: Christian Cech
  # Date: 2016
  # From: lecture material
  # Functionality: Returns the Weights of Evidence (WE) or the Information Value (IV) for categorical data
  #
  # Args:
  #   observations: two-rowed data matrix that contains the number of observations (row 1) and the number of defaults (row 2) for every attribute. The values in the different columns contain the values for the different attributes.
  #   labels: a string array with the labels of the attributes.
  #   details: if details is FALSE only a vector of the WE-values respectively an IV-value are returned. If details is TRUE a more detailed table is returned. The table contains: (i) the number of observations per attrtibute and the total number of observations in the righmost column, (ii) the number of non-derfaults, (iii) the number of defaults, (iv) the fraction of non-defaults, (v) the fraction of defaults, (vi) the WEs. If iv.ouput is TRUE the table additionally contains the "contribution" per attribute for the IV and the IV (i.e. the sum of the "contributions") in the rightmost column.
  #   if iv.output is FALSE then the WE-values are returned. If IV is true then the IV-value is returned.
  #
  # Returns:
  #   the Weights of Evidence (WE) or the Information Value (IV) for categorical data.
  
  # Compatibility checks:
  if (!is.matrix(observationsIncome)) stop ('observations must be a matrix')
  if (nrow(observationsIncome) != 2) stop ('observations must have two columns')
  if (!is.null(labels)) {
    if (!is.matrix(labels)) {
      if (ncol(observationsIncome) != length(labels)) stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations')
    }
    else stop ('labels must be a string vector, not a matrix')
  }
  
  # Calculation of the WEs
  we <- matrix(0, ncol=ncol(observationsIncome), nrow=1)  # initialisation
  for (i in 1:ncol(observationsIncome)) {
    we[1, i] <- log((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,])-sum(observationsIncome[2,])) ^ -1 *
                      (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1) ^ -1)
  }
  
  # Calculation of the IV
  if (iv.output) {
    iv <- 0  # Initialization
    for (i in 1:ncol(observationsIncome)) {
      iv <- iv + ((observationsIncome[1,i] - observationsIncome[2,i]) * (sum(observationsIncome[1,]) - sum(observationsIncome[2,])) ^ -1 -
                    (observationsIncome[2,i] * sum(observationsIncome[2,]) ^ -1)) *
        we[1,i]
    }
  }
  
  # Return results
  if (!details) {
    if (!iv.output) {
      if (!is.null(labels)) {
        colnames(we) <- labels  # Labels not NULL, details=FALSE, iv.output=FALSE
      }
      return(we)  # Details=FALSE, iv.output=FALSE
    }
    else {
      return(iv) # Details=FALSE, iv.output=TRUE
    }
  }
  else {
    result.table <- matrix(0, ncol=(ncol(observationsIncome)+1), nrow=6)  # Initialization
    if (is.null(labels)) {
      colnames(result.table) <- c(1:ncol(observationsIncome), "total")
    }
    else {
      colnames(result.table) <- c(labels, "total")
    }
    rownames(result.table) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE")
    result.table[1,] <- c(observationsIncome[1,], sum(observationsIncome[1,]))  # Number of observations
    result.table[2,] <- c(observationsIncome[1,] - observationsIncome[2,], sum(observationsIncome[1,]) - sum(observationsIncome[2,]))  # Number of non-defaults
    result.table[3,] <- c(observationsIncome[2,], sum(observationsIncome[2,]))  # Number of defaults
    result.table[4,] <- result.table[2,] * result.table[2,ncol(result.table)] ^ -1  # Fraction of non-defaults
    result.table[5,] <- result.table[3,] * result.table[3,ncol(result.table)] ^ -1  # Fraction of defaults
    result.table[6,] <- c(we, 0)  # WEs
    if (!iv.output) {  # WE-table
      return(result.table)
    }
    else {  # IV-table
      result.table2 <- matrix(0, ncol=(ncol(observationsIncome)+1), nrow=7)  # Initialization
      colnames(result.table2) <- colnames(result.table)
      rownames(result.table2) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE", "IV")
      result.table2[1:6,] <- result.table
      result.table2[7,] <- c((result.table2[4, 1:(ncol(result.table2) - 1)] - result.table2[5, 1:(ncol(result.table2) - 1)]) * we, iv)  # "IV-contributions" and in the right-most columne IV. (Remark: piecewise multiplication)
      return(result.table2)
    }
  }
}
WE(observationsIncome, label=NULL, details=TRUE, iv.output=TRUE)


##### WEIGHTS OF EVIDENCE - MARITAL STATUS ##### 
table(testing$MARITAL_STATUS,testing$DEFAULT_FLAG)
matrix(c(table(testing$MARITAL_STATUS),table(testing$MARITAL_STATUS,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_marital_status <- WE(matrix(c(table(testing$MARITAL_STATUS), table(testing$MARITAL_STATUS, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$MARITAL_STATUS, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_marital_status

### WEIGHTS OF EVIDENCE - EDUCATION
WOE_education <- WE(matrix(c(table(testing$EDUCATION), table(testing$EDUCATION, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$EDUCATION, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_education
plot(WOE_education)

### SALARY_ACCOUNT
table(testing$SALARY_ACCOUNT,testing$DEFAULT_FLAG)
matrix(c(table(testing$SALARY_ACCOUNT),table(testing$SALARY_ACCOUNT,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_salary_account <-WE(matrix(c(table(testing$SALARY_ACCOUNT), table(testing$SALARY_ACCOUNT, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$SALARY_ACCOUNT, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_salary_account

### DEBIT_CARD
table(testing$DEBIT_CARD,testing$DEFAULT_FLAG)
matrix(c(table(testing$DEBIT_CARD),table(testing$DEBIT_CARD,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_debit_card <- WE(matrix(c(table(testing$DEBIT_CARD), table(testing$DEBIT_CARD, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$DEBIT_CARD, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_debit_card

### CURRENT_ACCOUNT
table(testing$CURRENT_ACCOUNT,testing$DEFAULT_FLAG)
matrix(c(table(testing$CURRENT_ACCOUNT),table(testing$CURRENT_ACCOUNT,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
WOE_current_account <- WE(matrix(c(table(testing$CURRENT_ACCOUNT), table(testing$CURRENT_ACCOUNT, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$CURRENT_ACCOUNT, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
WOE_current_account

### FINALIZED_LOAN
table(testing$FINALIZED_LOAN,testing$DEFAULT_FLAG)
matrix(c(table(testing$FINALIZED_LOAN),table(testing$FINALIZED_LOAN,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_finalized_loan <- WE(matrix(c(table(testing$FINALIZED_LOAN), table(testing$FINALIZED_LOAN, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$FINALIZED_LOAN, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_finalized_loan

### AREA
table(testing$AREA,testing$DEFAULT_FLAG)
matrix(c(table(testing$AREA),table(testing$AREA,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_area <- WE(matrix(c(table(testing$AREA), table(testing$AREA, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$AREA, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_area

### PRODUCT
table(testing$PRODUCT,testing$DEFAULT_FLAG)
matrix(c(table(testing$PRODUCT),table(testing$PRODUCT,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_product <- WE(matrix(c(table(testing$PRODUCT), table(testing$PRODUCT, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$PRODUCT, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_product

### RESIDENTIAL_PLACE
table(testing$RESIDENTIAL_PLACE,testing$DEFAULT_FLAG)
matrix(c(table(testing$RESIDENTIAL_PLACE),table(testing$RESIDENTIAL_PLACE,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_residential_place <- WE(matrix(c(table(testing$RESIDENTIAL_PLACE), table(testing$RESIDENTIAL_PLACE, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$RESIDENTIAL_PLACE, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_residential_place

### HOUSEHOLD_MEMBERS
table(testing$HOUSEHOLD_MEMBERS,testing$DEFAULT_FLAG)
matrix(c(table(testing$HOUSEHOLD_MEMBERS),table(testing$HOUSEHOLD_MEMBERS,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_household_members <- WE(matrix(c(table(testing$HOUSEHOLD_MEMBERS), table(testing$HOUSEHOLD_MEMBERS, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$HOUSEHOLD_MEMBERS, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_household_members

### DISTRICT
table(testing$DISTRICT,testing$DEFAULT_FLAG)
matrix(c(table(testing$DISTRICT),table(testing$DISTRICT,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_district <- WE(matrix(c(table(testing$DISTRICT), table(testing$DISTRICT, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$DISTRICT, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_district

### DISTRICT
table(testing$PROFESSION,testing$DEFAULT_FLAG)
matrix(c(table(testing$PROFESSION),table(testing$PROFESSION,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_profession <- WE(matrix(c(table(testing$PROFESSION), table(testing$PROFESSION, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$PROFESSION, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_profession

### BIRTH_PLACE
table(testing$BIRTH_PLACE,testing$DEFAULT_FLAG)
matrix(c(table(testing$BIRTH_PLACE),table(testing$BIRTH_PLACE,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_birth_place <- WE(matrix(c(table(testing$BIRTH_PLACE), table(testing$BIRTH_PLACE, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$BIRTH_PLACE, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_birth_place


### NO_OF_DEPENDENTS
table(testing$NO_OF_DEPENDENTS,testing$DEFAULT_FLAG)
matrix(c(table(testing$NO_OF_DEPENDENTS),table(testing$NO_OF_DEPENDENTS,testing$DEFAULT_FLAG)[,2]),nrow=2,byrow=TRUE)
woe_no_of_dependents <- WE(matrix(c(table(testing$NO_OF_DEPENDENTS), table(testing$NO_OF_DEPENDENTS, testing$DEFAULT_FLAG)[,2]), nrow=2, byrow=TRUE), rownames(table(testing$NO_OF_DEPENDENTS, testing$DEFAULT_FLAG)), details=TRUE, iv.output=TRUE)
woe_no_of_dependents

##### DEFINING RANGES - AGE ##### 
GroupingAge <- quantile(testing$AGE, probs = seq(0,1,0.2),names = FALSE) # don't want the name use false

m1 <- as.matrix(GroupingAge)

d1 <- vector(mode="numeric",length=0) #define empty vector
t1 <- vector(mode="numeric",length=0) #define empty vector
nm1 <- vector(mode="numeric",length=0) #define empty sumvector

ndata1 <- subset(testing, select=c("AGE","DEFAULT_FLAG")) ## data frame

for (i in 1:(nrow(m1)-1)){
  dn1 <- sum(ndata[ndata$AGE<=m1[i+1,1],2])- sum(ndata1[ndata1$AGE<m1[i,1],2])
  tn1 <- length(ndata[ndata$AGE<=m1[i+1,1],2])- length(ndata1[ndata1$AGE<m1[i,1],2])    
  
  d1 <- append(d1,dn1)
  t1 <- append(t1,tn1)
}

nwm1 <- cbind(t1,d1) # New matrix
nwm1

observationsAge <- t(nwm1)
observationsAge

##### WEIGHT OF EVIDENCE - AGE (simple) ##### 

WE <- function(observationsAge, labels=NULL, details=FALSE,
               iv.output=FALSE) {
  observationsAge <- matrix(c(nrow(testing), sum(data$DEFAULT_FLAG)), nrow=2, ncol=1)
  
  if (!is.matrix(observationsAge)) stop ('observations must be a matrix') 
  if (nrow(observationsAge) != 2) stop ('observations must have two columns') 
  if (!is.null(labels)) { 
    if (!is.matrix(labels)) { 
      if (ncol(observationsAge) != length(labels)) 
        stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations') }
  } 
  else stop ('labels must be a string vector, not a matrix') 
}

# Calculation of the WEs for Age
we <- matrix(0, ncol=ncol(observationsAge), nrow=1) # initialisation 
for (i in 1:ncol(observationsAge)) { 
  we[1, i] <- log((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,])-sum(observationsAge[2,])) ^ -1 * (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1) ^ -1) 
}
we


##### WEIGHT OF EVIDENCE - AGE (details) ##### 
WE <- function(observationsAge, labels=NULL, details=TRUE, iv.output=TRUE) {
  
  # Compatibility checks:
  if (!is.matrix(observationsAge)) stop ('observations must be a matrix')
  if (nrow(observationsAge) != 2) stop ('observations must have two columns')
  if (!is.null(labels)) {
    if (!is.matrix(labels)) {
      if (ncol(observationsAge) != length(labels)) stop ('labels must be a row vector (data-type matrix) with the same number of columns as observations')
    }
    else stop ('labels must be a string vector, not a matrix')
  }
  
  # Calculation of the WEs
  we <- matrix(0, ncol=ncol(observationsAge), nrow=1)  # initialisation
  for (i in 1:ncol(observationsAge)) {
    we[1, i] <- log((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,])-sum(observationsAge[2,])) ^ -1 *
                      (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1) ^ -1)
  }
  
  # Calculation of the IV
  if (iv.output) {
    iv <- 0  # initialisation
    for (i in 1:ncol(observationsAge)) {
      iv <- iv + ((observationsAge[1,i] - observationsAge[2,i]) * (sum(observationsAge[1,]) - sum(observationsAge[2,])) ^ -1 -
                    (observationsAge[2,i] * sum(observationsAge[2,]) ^ -1)) *
        we[1,i]
    }
  }
  
  # Return results
  if (!details) {
    if (!iv.output) {
      if (!is.null(labels)) {
        colnames(we) <- labels  # Labels not NULL, details=FALSE, iv.output=FALSE
      }
      return(we)  # Details=FALSE, iv.output=FALSE
    }
    else {
      return(iv) # Details=FALSE, iv.output=TRUE
    }
  }
  else {
    result.table <- matrix(0, ncol=(ncol(observationsAge)+1), nrow=6)  # Initialization
    if (is.null(labels)) {
      colnames(result.table) <- c(1:ncol(observationsAge), "total")
    }
    else {
      colnames(result.table) <- c(labels, "total")
    }
    rownames(result.table) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE")
    result.table[1,] <- c(observationsAge[1,], sum(observationsAge[1,]))  # number of observations
    result.table[2,] <- c(observationsAge[1,] - observationsAge[2,], sum(observationsAge[1,]) - sum(observationsAge[2,]))  # number of non-defaults
    result.table[3,] <- c(observationsAge[2,], sum(observationsAge[2,]))  # number of defaults
    result.table[4,] <- result.table[2,] * result.table[2,ncol(result.table)] ^ -1  # fraction of non-defaults
    result.table[5,] <- result.table[3,] * result.table[3,ncol(result.table)] ^ -1  # fraction of defaults
    result.table[6,] <- c(we, 0)  # WEs
    if (!iv.output) {  # WE-table
      return(result.table)
    }
    else {  # IV-table
      result.table2 <- matrix(0, ncol=(ncol(observationsAge)+1), nrow=7)  # Initialization
      colnames(result.table2) <- colnames(result.table)
      rownames(result.table2) <- c("number of observations", "number of non-defaults", "number of defaults","fraction non-defaults", "fraction defaults", "WE", "IV")
      result.table2[1:6,] <- result.table
      result.table2[7,] <- c((result.table2[4, 1:(ncol(result.table2) - 1)] - result.table2[5, 1:(ncol(result.table2) - 1)]) * we, iv)  # "IV-contributions" and in the right-most columne IV. (Remark: piecewise multiplication)
      return(result.table2)
    }
  }
}
WE(observationsAge, label=NULL, details=TRUE, iv.output=TRUE)


# CHECKING NO OF DEFAULTS BETWEEN ARBITRARY AGE RANGE
flag.sum <- 0
for (i in 1:nrow(data)) {
  if (data$AGE[i] >= 18 & data$AGE[i]  < 74)  {
    flag.sum <- flag.sum + data$DEFAULT_FLAG[i]
  }
}
flag.sum
flag.sum/sum(data$DEFAULT_FLAG)
sum(data$DEFAULT_FLAG)


##### BASIC DESCRIPTIVE STATISTICS OF testing SAMPLE ##### 
product.pie <- pie(table(testing$PRODUCT)*nrow(testing)^-1)
table(testing$PRODUCT)/nrow(testing)*100 #percentage of clients per product type

marital.status_pie <- pie(table(testing$MARITAL_STATUS))
table(testing$MARITAL_STATUS)/nrow(data)*100 #percentage of clients per marital status

education_pie <- pie(table(testing$EDUCATION))
table(testing$EDUCATION)/nrow(testing)*100 #percentage of clients per education level

household.members_pie <- pie(table(testing$HOUSEHOLD_MEMBERS))
table(testing$HOUSEHOLD_MEMBERS)/nrow(testing)*100 #percentage of clients per household members

area_pie <- pie(table(testing$AREA))
table(testing$AREA)/nrow(testing)*100 #percentage of clients per area

# Variable AGE
hist(testing$AGE)
mean(testing$AGE)
max(testing$AGE)
min(testing$AGE)
boxplot(testing$AGE)
plot(testing$AGE, testing$DEFAULT_FLAG)
quantile(testing$AGE, c(0.25, 0.5, 0.75, 1))
plot(density(testing$AGE, bw=0.004))

# Variable INCOME
mean(testing$INCOME)
max(testing$INCOME)
min(testing$INCOME)
hist(testing$INCOME)
plot(density(testing$INCOME, bw=0.004))


# CALCULATE THE CORRELATION
apply(testing, 2, function(x) length(unique(x))) # Check unique values per variable

testing["AREA_CODED"] <- NA # Creating new column
testing$AREA_CODED <- as.numeric(factor(testing$AREA, labels=c(1:3))) # Transforming categorical variable to numerical one
class(testing$AREA_CODED) # Check the variable type
testing$AREA_CODED[is.na(testing$AREA_CODED)] <- 0

testing["DISTRICT_CODED"] <- NA # Creating new column
testing$DISTRICT_CODED <- as.numeric(factor(testing$DISTRICT, labels=c(1:42))) # Transforming categorical variable to numerical one
class(testing$DISTRICT_CODED) # Check the variable type
testing$DISTRICT_CODED[is.na(testing$DISTRICT_CODED)] <- 0

testing["EDUCATION_CODED"] <- NA # Creating new column
testing$EDUCATION_CODED <- as.numeric(factor(testing$EDUCATION, labels=c(1, 2,3,4,5,6,7,8,9))) # Transforming categorical variable to numerical one
class(testing$EDUCATION_CODED) # Check the variable type
testing$EDUCATION_CODED[is.na(testing$EDUCATION_CODED)] <- 0

testing["RESIDENTIAL_PLACE_CODED"] <- NA # Creating new column
testing$RESIDENTIAL_PLACE_CODED <- as.numeric(factor(testing$RESIDENTIAL_PLACE, labels=c(1, 2,3,4,5,6,7))) # Transforming categorical variable to numerical one
class(testing$RESIDENTIAL_PLACE_CODED) # Check the variable type
testing$RESIDENTIAL_PLACE_CODED[is.na(testing$RESIDENTIAL_PLACE_CODED)] <- 0

cor(data.matrix(testing[,2:22]))


##### LOGIT REGRESSION ##### 

# DIVIDING INCOME INTO RANGES FOR REGRESSION
quantile(testing$INCOME, probs = seq(0,1,0.125),names = FALSE)

testing["INCOME_RANGES"] <- NA # Creating new column
for (i in 1:nrow(testing)) { 
  if (testing$INCOME[i] <= quantile(testing$INCOME, 0.125)) {
    testing$INCOME_RANGES[i] <- "<= 815"
  }  
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.125) & testing$INCOME[i] <= quantile(testing$INCOME, 0.25)) {
    testing$INCOME_RANGES[i] <- "> 815 & <= 1007"
  }  
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.25) & testing$INCOME[i] <= quantile(testing$INCOME, 0.375)) {
    testing$INCOME_RANGES[i] <- "> 1007 & <= 1195.906"
  } 
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.375) & testing$INCOME[i] <= quantile(testing$INCOME, 0.5)) {
    testing$INCOME_RANGES[i] <- "> 1195.906 & <= 1431.85"
  }  
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.5) & testing$INCOME[i] <= quantile(testing$INCOME, 0.625)) {
    testing$INCOME_RANGES[i] <- "> 1431.85 & <= 1780"
  } 
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.625) & testing$INCOME[i] <= quantile(testing$INCOME, 0.75)) {
    testing$INCOME_RANGES[i] <- "> 1780 & <= 2280.039"
  } 
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.75) & testing$INCOME[i] <= quantile(testing$INCOME, 0.875)) {
    testing$INCOME_RANGES[i] <- "> 2280 & <= 3184.943"
  } 
  else if (testing$INCOME[i] > quantile(testing$INCOME, 0.875) & testing$INCOME[i] <= quantile(testing$INCOME, 1)) {
    testing$INCOME_RANGES[i] <- "> 3184.943"
  }
}
class(testing$INCOME_RANGES)

# DIVIDING AGE INTO RANGES FOR REGRESSION
quantile(testing$AGE, probs = seq(0,1,0.2),names = FALSE)

testing["AGE_RANGES"] <- NA # Creating new column
for (i in 1:nrow(testing)) { 
  if (testing$AGE[i] <= quantile(testing$AGE, 0.2)) {
    testing$AGE_RANGES[i] <- "<= 29"
  }  
  else if (testing$AGE[i] > quantile(testing$AGE, 0.2) & testing$AGE[i] <= quantile(testing$AGE, 0.4)) {
    testing$AGE_RANGES[i] <- "> 29 & <= 37"
  }  
  else if (testing$AGE[i] > quantile(testing$AGE, 0.4) & testing$AGE[i] <= quantile(testing$AGE, 0.6)) {
    testing$AGE_RANGES[i] <- "> 37 & <= 45"
  } 
  else if (testing$AGE[i] > quantile(testing$AGE, 0.6) & testing$AGE[i] <= quantile(testing$AGE, 0.8)) {
    testing$AGE_RANGES[i] <- "> 45 & <= 58"
  }  
  else if (testing$AGE[i] > quantile(testing$AGE, 0.8) & testing$AGE[i] <= quantile(testing$AGE, 1)) {
    testing$AGE_RANGES[i] <- "> 58 & <= 74"
  }
}
class(testing$AGE_RANGES)


### LOGISTIC REGRESSION 
training["HOUSEHOLD_MEMBERS_NUMERIC"] <- NA # Creating new column
training$HOUSEHOLD_MEMBERS_NUMERIC <- factor(training$HOUSEHOLD_MEMBERS, labels=c("1", "2", "3", "4", "5", "6"))
class(training$HOUSEHOLD_MEMBERS_NUMERIC) # Check the variable type

testing["HOUSEHOLD_MEMBERS_NUMERIC"] <- NA # Creating new column
testing$HOUSEHOLD_MEMBERS_NUMERIC <- factor(testing$HOUSEHOLD_MEMBERS, labels=c("1", "2", "3", "4", "5", "6"))
class(testing$HOUSEHOLD_MEMBERS_NUMERIC) # Check the variable type

modelfinal1 <- glm(DEFAULT_FLAG ~ INCOME_RANGES + AGE_RANGES + EDUCATION + AREA + FINALIZED_LOAN + HOUSEHOLD_MEMBERS_NUMERIC,family=binomial(link="logit"), data=trainint)
summary(modelfinal1)


##### VALIDATION using package "ROCR" ##### 
predict.pd1 <- prediction(predict(modelfinal1,  # Choose model
                                  newdata=subset(testing,select=c(6,8,10,17,23,24)),  # Testing, columns 2 to 6
                                  type="response"),
                          testing$DEFAULT_FLAG)  # Variabe to be predicted
predict.pd.roc1 <- performance(predict.pd1, measure="tpr", x.measure="fpr")  # ROC-curve-data
plot(predict.pd.roc1, main="linear model")  # Plot ROC-curve

auc1 <- performance(predict.pd1, measure="auc")  # AUC
auc1@y.values[[1]]  # Returns the relevant information from the AUC-object

#### Second model
modelfinal2 <- glm(DEFAULT_FLAG ~ INCOME_RANGES + AGE_RANGES + EDUCATION + NO_OF_DEPENDENTS + FINALIZED_LOAN  ,family=binomial(link="logit"), data=training)
summary(modelfinal2)

predict.pd2 <- prediction(predict(modelfinal2,  # Choose model
                                  newdata=subset(testing,select=c(8,11,17,23,24)),  # Testing, columns 2 to 6
                                  type="response"),
                          testing$DEFAULT_FLAG)  # Variabe to be predictedt
predict.pd.roc2 <- performance(predict.pd2, measure="tpr", x.measure="fpr")  # ROC-curve-data
plot(predict.pd.roc2, main="logit model")  # Plot ROC-curve

auc2 <- performance(predict.pd2, measure="auc")  # AUC
auc2@y.values[[1]]  # Returns the relevant information from the AUC-object

### Comparison of the ROC-curves:
plot(predict.pd.roc1, lty=3)
plot(predict.pd.roc2, add=TRUE)

### CALIBRATION
modelfinal1 <- glm(DEFAULT_FLAG ~ INCOME_RANGES + AGE_RANGES + EDUCATION + NO_OF_DEPENDENTS + FINALIZED_LOAN  ,family=binomial(link="logit"), data=training)
summary(modelfinal1)

modelfinal2 <- glm(DEFAULT_FLAG ~ INCOME_RANGES + AGE_RANGES + EDUCATION + AREA + FINALIZED_LOAN + HOUSEHOLD_MEMBERS,family=binomial(link="logit"), data=training)
summary(modelfinal2)

PD =predict(modelfinal2, data.frame(INCOME_RANGES="> 3184.943", AGE_RANGES="> 29 & <= 37", EDUCATION="Universitate" , AREA="Rural area",
HOUSEHOLD_MEMBERS=3, FINALIZED_LOAN=1), type="response") # prediction
PD

S=218-72*log(PD/(1-PD))
S