install.packages('lubridate')

library(lubridate)
library(dplyr)
library(ggplot2)

crime <- read.csv("crimes.csv", header = TRUE)
str(crime)

crime$POSIX <- ymd_hms(as.character(crime$Dates))
crime$Dates <- as.Date(ymd_hms(as.character(crime$Dates)))

moon <- read.csv("moon.csv", header = TRUE)
moon$date <- as.Date(moon$date, "%m/%d/%Y")

full_data <- inner_join(crime, moon, by=c("Dates"="date"))

date_phase <- full_data %>%
  group_by(Dates, phase) %>%
  count() %>%
  arrange(desc(n))
glimpse(date_phase)

ggplot(date_phase, aes(Dates, n)) + geom_line(alpha = 0.5) + labs(title = "Злочини в Сан-Франциско (2003-2015)", x = "Дата", y = "Кількість злочинів") + geom_point(data = date_phase[date_phase$phase == "Full Moon", ], color = "red") + geom_smooth()

x <- mean(date_phase$n[date_phase$phase == "Full Moon"])
x
## [1] 395.7273
mu <- mean(date_phase$n[date_phase$phase != "Full Moon"])
mu
## [1] 391.7522 - mu for H0, alfa = 0.5

n <- length(date_phase$n[date_phase$phase == "Full Moon"])
n
## [1] 77
s <- sd(date_phase$n[date_phase$phase == "Full Moon"])
s
## [1] 41.63615
## test stat = 0.839, needed p-value

p_value <- 2*pt(0.839, df=76, lower.tail = FALSE)
p_value
## [1] 0.4041006 - p_value > alfa - H0 accept

x_vector <- date_phase$n[date_phase$phase == "Full Moon"]
t.test(x_vector, mu = 391.75, alternative = "two.sided", conf.level = 0.95)
## interval [386.2770; 405.1775]
## decision - фаза Місяця не має впливу на середнє значення кількості злочинів

day_of_week_crimes <- full_data %>%
  group_by(DayOfWeek) %>%
  count()
glimpse(day_of_week_crimes)

day_of_week_crimes$DayOfWeek <- factor(day_of_week_crimes$DayOfWeek , levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data=day_of_week_crimes, aes(x=DayOfWeek, y=n)) + geom_bar(stat="identity", fill="lightblue")

crimes_by_day <- full_data %>%
  group_by(Dates, DayOfWeek) %>%
  count()

sample_vector <- crimes_by_day$n[crimes_by_day$DayOfWeek == "Friday"]
n_friday <- length(sample_vector)
n_friday

# 1. Кількість спостережень
n <- length(sample_vector)
# 2. Вибіркове середнє
xbar <- mean(sample_vector)
# 3. Вибіркове СКВ (за замовчуванням: n–1 у sd())
s <- sd(sample_vector)
# 4. Обчислення t‑статистики
t_stat <- (xbar - 391.75) / (s / sqrt(n))
# 5. Вивід
t_stat
  
df <- n - 1
p_value <- 2 * pt(-abs(t_stat), df)
p_value_rounded <- round(p_value, 4)
p_value_rounded

tt <- t.test(sample_vector, mu = 391.75)
tt$p.value

ttе <- t.test(sample_vector, mu = 391.75, conf.level = 0.99)
ttе$conf.int