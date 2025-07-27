install.packages('ggplot2')
install.packages('dplyr')
library('ggplot2')
library('dplyr')
data('diamonds')
View(diamonds)

ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point(col="lightblue")

cor(diamonds$carat, diamonds$price)

lin.diamond.ideal <- lm(price ~ carat,
                        data = filter(diamonds, cut == "Ideal"))
lin.diamond.fair  <- lm(price ~ carat,
                        data = filter(diamonds, cut == "Fair"))

# 2. Подивитися коротко результати
summary(lin.diamond.ideal)
summary(lin.diamond.fair)

# 3. Прогноз для 1 карата
new   <- data.frame(carat = 1)
pred_ideal <- predict(lin.diamond.ideal, newdata = new)
pred_fair  <- predict(lin.diamond.fair,  newdata = new)

# 4. Вивід
pred_ideal
pred_fair