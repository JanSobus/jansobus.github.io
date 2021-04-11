
library("readr")
library("dplyr")
library("plotly")
library("ggplot2")
library("htmlwidgets")

cen1 <- t(read_csv("2016Census_G10A_AUS.csv",col_names=F))[-1,]
cen2 <- t(read_csv("2016Census_G10B_AUS.csv",col_names=F))[-1,]
cen3 <- t(read_csv("2016Census_G10C_AUS.csv",col_names=F))[-1,]
raw_cen <- rbind(cen1,cen2,cen3)
cen_names <- unlist(strsplit(unname(raw_cen[16*c(1:(dim(raw_cen)[1]/16)),1]),"_Tot"))
census <- as.data.frame(matrix(as.numeric(raw_cen[,2]), ncol=16, byrow=TRUE))
census$Country <-cen_names[-33]
names(census)[1:16] <- as.character(c("Mig_Bef_1946","Mig_1946_1955","Mig_1956_1965","Mig_1966_1975","Mig_1976_1985","Mig_1986_1995","Mig_1996_2005","Mig_2006_2010","Mig_2011","Mig_2012","Mig_2013","Mig_2014","Mig_2015","Mig_2016","Mig_NS","Mig_Total"))
census <- rbind(arrange(.data= census[1:34,],desc(Mig_Total)),census[c(35,36),])
census$Country<-gsub("_"," ",census$Country)
census$Country[census$Country=="China exc SARs Taiwan"] <- "China"


        