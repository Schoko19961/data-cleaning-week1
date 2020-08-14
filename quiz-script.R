# Question 1
library(data.table)
library(dplyr)
library(tidyr)
q1 <- fread("q1.csv") %>% as_tibble()
resultQ1 <- q1 %>% select(VAL) %>% filter(VAL == 24) %>% nrow()
names(resultQ1) <- "Question 1"
print(resultQ1)

#Question 3
library(xlsx)
q2 <- read.xlsx("q3.xlsx", 1,colIndex = 7:15, rowIndex = 18:23)
resultQ2 <- sum(q2$Zip*q2$Ext,na.rm=T)
names(resultQ2) <- "Question 3"
print(resultQ2)
# Question 4
library(xml2)
q4 <- read_xml("q4.xml")
zipcodes <- xml_find_all(q4, ".//zipcode") %>% xml_text() %>% as.numeric()
resultq4<-zipcodes[zipcodes == 21231] %>% length()
names(resultq4) <- "Question 4"
print(resultq4)

# Question 5
stopTime <- function(exec){
  start <- Sys.time()
  exec()
  Sys.time() - start
}

DT <- fread("q5.csv")

o1exec <- function(){
  mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
}

o2exec <- function(){
  tapply(DT$pwgtp15,DT$SEX,mean)
}
o3exec <- function(){
  mean(DT$pwgtp15,by=DT$SEX)
}
o4exec <- function(){
  rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
}

o5exec <- function(){
  sapply(split(DT$pwgtp15,DT$SEX),mean)
}

o6exec <- function(){
  DT[,mean(pwgtp15),by=SEX]
}

rep <- 100

print("Question 5")
print("Option 1")
mean(DT[DT$SEX==1,]$pwgtp15) %>% print
mean(DT[DT$SEX==2,]$pwgtp15) %>% print
r1 <- replicate(rep, stopTime(o1exec)) %>% sum
names(r1) <- "Time"
print(r1)

print("Option 2")
tapply(DT$pwgtp15,DT$SEX,mean) %>% print
r2 <- replicate(rep, stopTime(o2exec)) %>% sum
names(r2) <- "Time"
print(r2)

print("Option 3")
mean(DT$pwgtp15,by=DT$SEX) %>% print
r3 <- replicate(rep, stopTime(o1exec)) %>% sum
names(r3) <- "Time"
print(r3)

#print("Option 4")
#rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2] %>% print
#replicate(rep, stopTime(o4exec)) %>% sum %>% print

print("Option 5")
sapply(split(DT$pwgtp15,DT$SEX),mean) %>% print
r5<-replicate(rep, stopTime(o5exec)) %>% sum
names(r5) <- "Time"
print(r5)

print("Option 6")
DT[,mean(pwgtp15),by=SEX] %>% print
r6<-replicate(rep, stopTime(o6exec)) %>% sum
names(r6) <- "Time"
print(r6)
