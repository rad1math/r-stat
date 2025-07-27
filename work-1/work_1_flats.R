#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)

library(dplyr)
library(ggplot2)

setwd("/Users/radyslav/coding/data_science/laba_DS")
flats <- read.csv("flats.csv", stringsAsFactors=FALSE, dec= ",")
str(flats)

dim(flats)
names(flats)

summary(flats)

count(flats, Місто)

flats %>%
  filter(Місто == "Львів") %>%
  filter(Кімнат == 1) %>%
  summarise(mean=median(Загальна_площа))
