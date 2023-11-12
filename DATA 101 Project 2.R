library(tidyverse)
library(ggplot2)

# Question 1 
#"How many countries did not report any amount of freshwater?" 
#"Did any countries report only some of the time?"
rowSums(is.na(Drinking_Water_Clean))

#Questions 2 and 3
#"Which country has the smallest average amount of renewable freshwater?"
#"Which country has the largest average amount of renewable freshwater?"
C <- rowMeans(Drinking_Water_Clean[5:16], na.rm=TRUE)
C
V <- mutate(Drinking_Water_Clean, `Mean Water by Country` = `C`)
V
L <- arrange(V, (`Mean Water by Country`))
L

# Question 4
#"Find the mean amount of renewable freshwater for each of the 5 year periods."
X <- colMeans(Drinking_Water_Clean[5:16], na.rm=TRUE)
X

#Question 5
AA <- mutate(Drinking_Water_Clean, `1967-1962` = `1967` - `1962`, na.rm=TRUE)
AB <- mutate(Drinking_Water_Clean, `1972-1967` = `1972` - `1967`, na.rm=TRUE)
AC <- mutate(Drinking_Water_Clean, `1977-1972` = `1977` - `1972`, na.rm=TRUE)
AD <- mutate(Drinking_Water_Clean, `1982-1977` = `1982` - `1977`, na.rm=TRUE)
AE <- mutate(Drinking_Water_Clean, `1987-1982` = `1987` - `1982`, na.rm=TRUE)
AF <- mutate(Drinking_Water_Clean, `1992-1987` = `1992` - `1987`, na.rm=TRUE)
AG <- mutate(Drinking_Water_Clean, `1997-1992` = `1997` - `1992`, na.rm=TRUE)
AH <- mutate(Drinking_Water_Clean, `2002-1997` = `2002` - `1997`, na.rm=TRUE)
AI <- mutate(Drinking_Water_Clean, `2007-2002` = `2007` - `2002`, na.rm=TRUE)
AJ <- mutate(Drinking_Water_Clean, `2012-2007` = `2012` - `2007`, na.rm=TRUE)
AK <- mutate(Drinking_Water_Clean, `2017-2012` = `2017` - `2012`, na.rm=TRUE)

#Question 6
#"Which country had the largest decrease in amount of renewable freshwater from 1962 to 2017?"
Z <- mutate(Drinking_Water_Clean, `Difference` = `2017` - `1962`, na.rm=TRUE)
Z
select(Z, `Difference`, `Country Name`)
summarise(Z, min(`Difference`, na.rm=TRUE))
B <- arrange(Z, desc(`Difference`, na.rm=TRUE))
B

#Question 7
#"Which country had the largest increase in amount of renewable freshwater for each 5-year period beginning in 1962?"
Y1 <- arrange(AA, desc(`1967-1962`, na.rm=TRUE))
Y2 <- arrange(AB, desc(`1972-1967`, na.rm=TRUE))
Y3 <- arrange(AC, desc(`1977-1972`, na.rm=TRUE))
Y4 <- arrange(AD, desc(`1982-1977`, na.rm=TRUE))
Y5 <- arrange(AE, desc(`1987-1982`, na.rm=TRUE))
Y6 <- arrange(AF, desc(`1992-1987`, na.rm=TRUE))
Y7 <- arrange(AG, desc(`1997-1992`, na.rm=TRUE))
Y8 <- arrange(AH, desc(`2002-1997`, na.rm=TRUE))
Y9 <- arrange(AI, desc(`2007-2002`, na.rm=TRUE))
Y10 <- arrange(AJ, desc(`2012-2007`, na.rm=TRUE))
Y11 <- arrange(AK, desc(`2017-2012`, na.rm=TRUE))

#Question 8
#"Create a graph that illustrates the change in the amount of renewable freshwater versus
#the year for the entire world."

#get means
X1 <- summarise(AA, mean(`1967-1962`, na.rm=TRUE))
X2 <- summarise(AB, mean(`1972-1967`, na.rm=TRUE))
X3 <- summarise(AC, mean(`1977-1972`, na.rm=TRUE))
X4 <- summarise(AD, mean(`1982-1977`, na.rm=TRUE))
X5 <- summarise(AE, mean(`1987-1982`, na.rm=TRUE))
X6 <- summarise(AF, mean(`1992-1987`, na.rm=TRUE))
X7 <- summarise(AG, mean(`1997-1992`, na.rm=TRUE))
X8 <- summarise(AH, mean(`2002-1997`, na.rm=TRUE))
X9 <- summarise(AI, mean(`2007-2002`, na.rm=TRUE))
X10 <- summarise(AJ, mean(`2012-2007`, na.rm=TRUE))
X11 <- summarise(AK, mean(`2017-2012`, na.rm=TRUE))

#put means into array, then into a variable
J <- c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11)
J[[2]]

#convert array variable into data frame
df <- as.data.frame(J)
df

#rename columns in data frame
colnames(df)[1]<-"'67-'62"
colnames(df)[2]<-"'72-'67"
colnames(df)[3]<-"'77-'72"
colnames(df)[4]<-"'82-'77"
colnames(df)[5]<-"'87-'82"
colnames(df)[6]<-"'92-'87"
colnames(df)[7]<-"'97-'92"
colnames(df)[8]<-"'02-'97"
colnames(df)[9]<-"'07-'02"
colnames(df)[10]<-"'12-'07"
colnames(df)[11]<-"'17-'12"


#put values into columns instead of row
D <- df %>%
  pivot_longer(c(`'67-'62`, `'72-'67`, `'77-'72`, `'82-'77`, `'87-'82`, `'92-'87`, `'97-'92`, `'02-'97`, `'07-'02`, `'12-'07`, `'17-'12`), names_to = "5-Year Period", values_to = "Mean Difference")
D

#create graph comparing mean difference of each 5-year period (columns) to the entire world (rows)
ggplot(data = D)+
  geom_point(mapping = aes(x = `5-Year Period`, y = `Mean Difference`))
