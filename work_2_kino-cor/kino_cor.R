#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)

library(dplyr)
library(ggplot2)

setwd("/Users/radyslav/coding/data_science/laba_R_3")
movie_body_counts <- read.csv('filmdeathcounts.csv', encoding = 'UTF-8')

head(movie_body_counts)
str(movie_body_counts)

movie_body_counts$body_per_min <- movie_body_counts$Body_Count/movie_body_counts$Length_Minutes

ggplot(movie_body_counts, aes(x=Body_Count)) + geom_histogram(bins=20, color='gray', fill='lightblue')

movie_body_counts %>% 
  top_n(n=10, Body_Count) %>%
  arrange(desc(Body_Count))

movie_body_counts %>% 
  top_n(n=10, body_per_min) %>%
  arrange(desc(body_per_min))

ggplot(movie_body_counts, aes(x=IMDB_Rating)) + geom_histogram(bins = 10, color='grey', fill='lightblue')

imdb_mean <- mean(movie_body_counts$IMDB_Rating)
print(imdb_mean)
imdb_sd <- sd(movie_body_counts$IMDB_Rating)
print(imdb_sd)

set.seed(900)
imdb_simulation <- rnorm(n=nrow(movie_body_counts), mean = imdb_mean, sd = imdb_sd)
movie_body_counts$imdb_simulation <- imdb_simulation

print(movie_body_counts)

ggplot(movie_body_counts, aes(x=imdb_simulation)) + geom_histogram(bins = 10, color='grey', fill='lightblue')

ggplot(movie_body_counts, aes(sample = imdb_simulation)) + stat_qq()

ggplot(movie_body_counts, aes(sample = IMDB_Rating)) + stat_qq()

pnorm(4, imdb_mean, imdb_sd)

pnorm(8, imdb_mean, imdb_sd) - pnorm(4, imdb_mean, imdb_sd)

cor(movie_body_counts$Body_Count, movie_body_counts$IMDB_Rating)
