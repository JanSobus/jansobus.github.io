##############################################
# The Data Incubator - Problem 1 - Jan Sobus #
##############################################


library("readr")
raw_data <- read_csv("Parking_Citations.csv")
head(raw_data, 10)
colnames(raw_data)

#Picking the subset of the data that we'll need
data <- raw_data[,c("Make","ViolFine","ViolDate","OpenPenalty","PoliceDistrict")]

#Extracting years from date and sorting by year
data$ViolDate <- as.numeric(substring(data$ViolDate,7,10))
data <- data[data$ViolDate != 2019 | is.na(data$ViolDate),]
data <- data[order(data$ViolDate),]

#Question 1
ans1 <- mean(data$ViolFine)

#Question 2
# Remove district with no information and take only PDistrict and VFine data
district_data <- data[!is.na(data$PoliceDistrict),c("ViolFine","PoliceDistrict")]

#Transform lowercases, check for any remaining errors
district_data$PoliceDistrict <- toupper(district_data$PoliceDistrict)
unique(district_data$PoliceDistrict)

#Change typo
district_data$PoliceDistrict[district_data$PoliceDistrict == "NOTHEASTERN"] <- ("NORTHEASTERN")
#Calculate mean aggregated by districts
ans2 <- max(aggregate(district_data$ViolFine, list(district_data$PoliceDistrict), mean)$x)

#Question 3
#Create vector of years I'm interested in
x <- 2004:2014
#Calculate yearly totals
totals <- aggregate(data$ViolDate, list(data$ViolDate), length)
#Create appropriate data frame
totals_frame <- data.frame(cbind(Year = x,Total = totals$x[totals$Group.1 > 2003 & totals$Group.1 < 2015]))
m1 <- lm(Total ~ Year, data = totals_frame)
summary(m1)
m1$coefficients

#Question 4
#Getting proper data subset
data_pen <- data[ !is.na(data$OpenPenalty) & data$OpenPenalty > 0,]
ans4 <- quantile(data_pen$OpenPenalty, probs = 0.81)

#Question 5
#Choosing Year 2017
data_2k17 <- data[ !is.na(data$ViolDate) & data$ViolDate == 2017,]
#Ordering by make
data_2k17 <- data_2k17[order(data_2k17$Make),]
#Making DF with Makes ordered by their count number
freq <- ave(rep(1, times=nrow(data_2k17)), data_2k17$Make, FUN=sum) 
MakeSums <- data.frame(rowSums(table(data_2k17$Make,freq)))
MakeSums$Make <- rownames(MakeSums)
names(MakeSums)[1] <- "Sum"
MakeSums <- MakeSums[order(MakeSums$Sum, decreasing = T),]

#Picking top ten Makes

#Dummy DF for results
TopMakes <- data.frame(matrix(ncol = 2, nrow = 0))


#Placeholder DF for removing data
TempSums <- MakeSums
#Find the top Make and its other names, sum them up and remove from DF, repeat 10 times
for(i in 1:25){
  
  topstring <- TempSums$Make[1]
  grep(substring(topstring,1,3),TempSums$Make)
  TopMakes <- rbind(TopMakes,c(TempSums$Make[1],sum(TempSums$Sum[grep(substring(topstring,1,3),TempSums$Make)])), stringsAsFactors = F)
  TempSums <- TempSums[-grep(substring(topstring,1,3),TempSums$Make),]
}
names(TopMakes) <- c("Make","Total")
TopMakes$Total <- as.numeric(TopMakes$Total)
TopMakes <- TopMakes[order(TopMakes$Total, decreasing = T),]
# Looking at at the Table, there are 4 JPN manufacturers in the top 10 - Toyota, Nissan, Honda and Acura
#Summing them up
tot_jap <- sum(TopMakes$Total[TopMakes$Make == "HONDA" |TopMakes$Make == "TOYOT"|TopMakes$Make == "NISSA"|TopMakes$Make == "ACURA"])
ans5 <- tot_jap/nrow(data_2k17)

#Question 6
raw_data2 <- read_csv("BPD_Part_1_Victim_Based_Crime_Data.csv")
