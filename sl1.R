
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


y <- 3
x <- 2
