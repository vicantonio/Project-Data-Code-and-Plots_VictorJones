library(dplyr)   
install.packages("gplots")
library(gplots)

#Data Loading, Cleaning, and Merging
my_dataAAeduc <-data.frame(read.csv("AA educ by MSA.csv", stringsAsFactors =T, header = T))
my_dataAAinc <- data.frame(read.csv("AA inc  by MSA.csv", stringsAsFactors =  T, header = T))
my_dataWNHeduc <- read.csv("WNH educ by MSA.csv", stringsAsFactors = T, header = T)
my_dataWNHinc <- read.csv("WNH inc by MSA.csv", stringsAsFactors = F, header = T)

#Convert MSA to Character from Factor. 
my_dataAAeduc$MSA <-  as.character(my_dataAAeduc$MSA)
my_dataAAinc$MSA <- as.character(my_dataAAinc$MSA)
my_dataWNHeduc$MSA <- as.character(my_dataWNHeduc$MSA)
my_dataWNHinc$MSA <- as.character(my_dataWNHinc$MSA)
#Renaming Columns
my_dataAAeduc <- rename(my_dataAAeduc, ninth_twelfthgrade = X9.12th.grade)
my_dataAAeduc <- rename(my_dataAAeduc, ninthgrade = X.9th.grade)
my_dataAAeduc <- rename(my_dataAAeduc, TotalPop = Total.Population)
my_dataAAinc <- rename(my_dataAAinc, TotalPop = Total.Population)
names(my_dataAAinc)[3:17] <- column_inc_names
column_inc_names <- c( "10K", "15K", "20K", "25K", "30K", "35K", "40K", "45K", "50K", "60K", "75K", "100K", "125K", "150K", "200K", "200K+")
my_dataAAinc$X200K <- my_dataAAinc$"200K+"
my_dataAAinc <- rename(my_dataAAinc, "200K+" =  X200K.)
my_dataAAinc

#Manipulate Data
# Show the percentage of people in each income group by city. Divide the number of people per group by the TotalPop for the city. 
#Repeat for educational attainment per city. 
my_dataAAinc[3] <- (my_dataAAinc[3]/my_dataAAinc[2]) * 100 
my_dataAAinc[4] <- (my_dataAAinc[4]/my_dataAAinc[2]) * 100
my_dataAAinc[5] <- (my_dataAAinc[5]/my_dataAAinc[2]) * 100
my_dataAAinc[6] <- (my_dataAAinc[6]/my_dataAAinc[2]) * 100
my_dataAAinc[7] <- (my_dataAAinc[7]/my_dataAAinc[2]) * 100
my_dataAAinc[8] <- (my_dataAAinc[8]/my_dataAAinc[2]) * 100
my_dataAAinc[9] <- (my_dataAAinc[9]/my_dataAAinc[2]) * 100
my_dataAAinc[10] <- (my_dataAAinc[10]/my_dataAAinc[2]) * 100
my_dataAAinc[11] <- (my_dataAAinc[11]/my_dataAAinc[2]) * 100
my_dataAAinc[12] <- (my_dataAAinc[12]/my_dataAAinc[2]) * 100
my_dataAAinc[13] <- (my_dataAAinc[13]/my_dataAAinc[2]) * 100
my_dataAAinc[14] <- (my_dataAAinc[14]/my_dataAAinc[2]) * 100
my_dataAAinc[15] <- (my_dataAAinc[15]/my_dataAAinc[2]) * 100
my_dataAAinc[16] <- (my_dataAAinc[16]/my_dataAAinc[2]) * 100
my_dataAAinc[17] <- (my_dataAAinc[17]/my_dataAAinc[2]) * 100
my_dataAAinc[18] <- (my_dataAAinc[18]/my_dataAAinc[2]) * 100

my_dataAAinc

names(my_dataWNHinc)[2:18] <- c( "TotalPop", "10K", "15K", "20K", "25K", "30K", "35K", "40K", "45K", "50K", "60K", "75K", "100K", "125K", "150K", "200K", "200K+")
my_dataWNHinc

my_dataAAeduc
names(my_dataWNHeduc)[2:4] <- c("TotalPop", "ninthgrade", "ninth_twelfthgrade")
my_dataWNHeduc

#MERGE DATA SETS FOR EDUCATION AND INCOME 

# Merge data sets for African American educational attainment and income. 
AAeducandinc <- mutate(my_dataAAinc, ninthgrade = my_dataAAeduc$ninthgrade, ninth_twelfthgrade = my_dataAAeduc$ninth_twelfthgrade, HSDiploma = my_dataAAeduc$HSDiploma, GED = my_dataAAeduc$GED, SomeCollege = my_dataAAeduc$SomeCollege, Assoc = my_dataAAeduc$Assoc, BA = my_dataAAeduc$BA, Grad = my_dataAAeduc$Grad)
AAeducandinc$TotalPopEduc <- my_dataAAeduc$TotalPop
AAeducandinc
#Merge data sets for Non-Hispanic Whites educational attaintment and income. 
WNHeducandinc <- mutate(my_dataWNHinc, ninthgrade = my_dataWNHeduc$ninthgrade, ninth_twelfthgrade = my_dataWNHeduc$ninth_twelfthgrade, HSDiploma = my_dataWNHeduc$HSDiploma, GED = my_dataWNHeduc$GED, SomeCollege = my_dataWNHeduc$SomeCollege, Assoc = my_dataWNHeduc$Assoc, BA = my_dataWNHeduc$BA, Grad = my_dataWNHeduc$Grad)
WNHeducandinc$TotalPopEduc <- my_dataWNHeduc$TotalPop

str(AAeducandinc)
str(WNHeducandinc)
AAeducandinc
                                                        # Plots and Summary Statistics

data <- AAeducandinc[, c(1,13:18)]
data
rowsum <- 
rowsum
data
data <- mutate(data, 'Percentage of AA Households with $75K+ Income' = rowSums((data[2:6])))

#Subset 75K+ households by MSA and show share of such households by MSA. Create new column with rowsum/total number of AA households with 75K + Income by MSA 
data <- mutate(data, Share_by_MSA = (('Percentage of Affluent Households'/100)*AAeducandinc$TotalPop)/sum(AAeducandinc$TotalPop)) 
x <- data[8]
labels <- data[1]
Share_by_MSA <- (data[8]/100 * AAeducandinc$TotalPop)/ sum(data[8]/100 * AAeducandinc$TotalPop) * 100

data$Share_by_MSA <- (data[8]/100 * AAeducandinc$TotalPop)/ sum(data[8]/100 * AAeducandinc$TotalPop) * 100

sum(Share_by_MSA) == 100
#Plot MSA and Share of Pop with incomes of at least 75k

data$MSA
data[8]
share_values <- c(33.80571, 33.10691, 48.43889, 28.57706, 27.90215,28.75524, 32.33470, 32.12436, 22.99172, 33.30378)
cities <- data$MSA
chart <- table(MSA, share_values)

#Plot 1
table1 <- plot(data.frame(MSA, share_values),xaxt = "n", col = "red",
               main = "Cities' Share of AA with 75K+ Income", 
              xlab = "Cities", 
              ylab = "Percent of AA Households $75K+", 
              ylim = c(0, 60), pch = 19)
axis(1, at = 1:10, labels = sort(MSA), tck = .6, tcl = -.5, cex.axis = 1, xaxp = c(1,10,1))

str(MSA)
MSA <- as.vector(MSA)



#Plot 2

Cities <- c("NYC", "Atlanta", "DC", "Chicago", "Miami", "Philadelphia", " Houston", "Dallas", "Detroit", "Los Angeles")
Share_by_MSA <-Share_by_MSA[,1]
Share_by_MSA

#Plot 2
plot(sort(MSA), Share_by_MSA, xlab = "Blah", ylab = "Bloop", type = "n", main = "Heading"), xlim = (0:100))
plot(1:10, Share_by_MSA, ylim = c(1,20), xaxt = "n", xlab = "Cities", ylab = "Percent of National AA Households with Income >= $75K", main = "Where AA with at least 75K Household Income Live" )
axis(1, at = 1:10, labels = Cities, col = "red")
text(1:10, Share_by_MSA, Cities, pos = 3)

#Plot 3
# Plot MSA by Share of AA pop with at least Some College

data$Percent_Educ_AA_MSA <-c(
sum(AAeducandinc[1,23:26])/AAeducandinc[1,27] * 100,
sum(AAeducandinc[2,23:26])/AAeducandinc[2,27] * 100,
sum(AAeducandinc[3,23:26])/AAeducandinc[3,27] * 100,
sum(AAeducandinc[4,23:26])/AAeducandinc[4,27] * 100,
sum(AAeducandinc[5,23:26])/AAeducandinc[5,27] * 100,
sum(AAeducandinc[6,23:26])/AAeducandinc[6,27] * 100,
sum(AAeducandinc[7,23:26])/AAeducandinc[7,27] * 100,
sum(AAeducandinc[8,23:26])/AAeducandinc[8,27] * 100,
sum(AAeducandinc[9,23:26])/AAeducandinc[9,27] * 100,
sum(AAeducandinc[10,23:26])/AAeducandinc[10,27] * 100)

#Plot 3
plot(1:10,data[[10]], ylim = c(40,70), xaxt = "n", ylab = "Share of Pop Surveyed With At Least Some College", main = "MSA by Share of Pop Surveyed With At Least Some College", xlab = "Cities")
axis(1, at = 1:10, labels = Cities, col = "red")


                                    #Analysis
# Show correlation between above values and Share of Pop with Incomes at least 75K for each MSA


