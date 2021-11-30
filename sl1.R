
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis,httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr, caret, lars, tidyverse, psych, dygraphs,
               vioplot, gapminder, nycflights13, gapminder, Lahman, ISLR2 )

summary(Smarket)
lm(Today~Lag1+Lag2,data=Smarket)
head(Smarket)
plot(Smarket)
head(Wage)
head()

library(readr)
Advertising <- read_csv("Data/Advertising.csv")
View()
Advertising
plot(Advertising)

tv <- select(Advertising,"TV","sales")
head(tv)
view(tv)
plot(tv)

library(readr)

  
  
  
Credit <- read_csv("Data/Credit.csv")
View(Credit)
?Credit # Information vedr. Credit er fejlbehæftet
summary(Credit) # Her får du mere information om Credit
lm(Balance ~ Student + Limit, data=Credit) # Lineær regression med forklarende variabler
plot(lm(Balance ~ Student + Limit, data=Credit))
plot(Credit)
Credit

library(readr)
Advertising <- read_csv("data/Credit.csv")
View(Credit)

