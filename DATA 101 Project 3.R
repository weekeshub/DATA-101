library(tidyverse)

#Question 1

#join datasets together
X1 <- full_join(Data, Metadata, by = "Country Code")

#remove NA values
Y1 <- na.omit(X1)

#median value for 1962 based on region and income group
Y1 %>%
  group_by(`Region`, `IncomeGroup`) %>%
  summarise(median = median(`Y1962`))

#median value for 2017 based on region and income group
Y1 %>%
  group_by(`Region`, `IncomeGroup`) %>%
  summarise(median = median(`Y2017`))

#Question 2

#by region
ggplot(data = Y1, aes(x = `Region`, y = `Y2017`)) +
  geom_boxplot()

#by income group
ggplot(data = Y1, aes(x = `IncomeGroup`, y = `Y2017`)) +
  geom_boxplot()

#Question 3

#get difference for each 5-year period
AA <- mutate(Y1, `67-62` = `Y1967` - `Y1962`)
AB <- mutate(AA, `72-67` = `Y1972` - `Y1967`)
AC <- mutate(AB, `77-72` = `Y1977` - `Y1972`)
AD <- mutate(AC, `82-77` = `Y1982` - `Y1977`)
AE <- mutate(AD, `87-82` = `Y1987` - `Y1982`)
AF <- mutate(AE, `92-87` = `Y1992` - `Y1987`)
AG <- mutate(AF, `97-92` = `Y1997` - `Y1992`)
AH <- mutate(AG, `02-97` = `Y2002` - `Y1997`)
AI <- mutate(AH, `07-02` = `Y2007` - `Y2002`)
AJ <- mutate(AI, `12-07` = `Y2012` - `Y2007`)
AK <- mutate(AJ, `17-12` = `Y2017` - `Y2012`)

#put values into columns instead of row
D <- AK %>%
  pivot_longer(c(`67-62`, `72-67`, `77-72`, `82-77`, `87-82`, `92-87`, `97-92`, `02-97`, `07-02`, `12-07`, `17-12`), names_to = "5-Year Period", values_to = "Difference")
D

#graph based on region
ggplot(data = D, aes(x = `Region`, y = `Difference`))+
  geom_point()

#graph based on income group
ggplot(data = D, aes(x = `IncomeGroup`, y = `Difference`))+
  geom_point()

#Question 4

#get mean amount of freshwater for each 5 year period
By_Region_Income <- group_by(AK, `Region`, `IncomeGroup`)
summary62 <- summarise(By_Region_Income, mean(`Y1962`))
summary67 <- summarise(By_Region_Income, mean(`Y1967`))
summary72 <- summarise(By_Region_Income, mean(`Y1972`))
summary77 <- summarise(By_Region_Income, mean(`Y1977`))
summary82 <- summarise(By_Region_Income, mean(`Y1982`))
summary87 <- summarise(By_Region_Income, mean(`Y1987`))
summary92 <- summarise(By_Region_Income, mean(`Y1992`))
summary97 <- summarise(By_Region_Income, mean(`Y1997`))
summary02 <- summarise(By_Region_Income, mean(`Y2002`))
summary07 <- summarise(By_Region_Income, mean(`Y2007`))
summary12 <- summarise(By_Region_Income, mean(`Y2012`))
summary17 <- summarise(By_Region_Income, mean(`Y2017`))

#combine means into one data frame
K1 <- merge(summary62, summary67)
K2 <- merge(K1, summary72)
K3 <- merge(K2, summary77)
K4 <- merge(K3, summary82)
K5 <- merge(K4, summary87)
K6 <- merge(K5, summary92)
K7 <- merge(K6, summary97)
K8 <- merge(K7, summary02)
K9 <- merge(K8, summary07)
K10 <- merge(K9, summary12)
K11 <- merge(K10, summary17)

#rename columns
names(K11)[names(K11) == "mean(Y1962)"] <- "1962"
names(K11)[names(K11) == "mean(Y1967)"] <- "1967"
names(K11)[names(K11) == "mean(Y1972)"] <- "1972"
names(K11)[names(K11) == "mean(Y1977)"] <- "1977"
names(K11)[names(K11) == "mean(Y1982)"] <- "1982"
names(K11)[names(K11) == "mean(Y1987)"] <- "1987"
names(K11)[names(K11) == "mean(Y1992)"] <- "1992"
names(K11)[names(K11) == "mean(Y1997)"] <- "1997"
names(K11)[names(K11) == "mean(Y2002)"] <- "2002"
names(K11)[names(K11) == "mean(Y2007)"] <- "2007"
names(K11)[names(K11) == "mean(Y2012)"] <- "2012"
names(K11)[names(K11) == "mean(Y2017)"] <- "2017"

#put means for each year into one column
L <- K11 %>%
  pivot_longer(c(`1962`, `1967`, `1972`, `1977`, `1982`, `1987`, `1992`, `1997`, `2002`, `2007`, `2012`, `2017`), names_to = "5-Year Period", values_to = "Mean Amount of Water")
L

#create graph of average amount of drinking water for every time period for entire world based on region and income group
ggplot(data = L) +
  geom_point(mapping = aes(x = `5-Year Period`, y = `Mean Amount of Water`, color = `Region`, shape = `IncomeGroup`))

