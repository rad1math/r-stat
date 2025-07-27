anscombe

str(anscombe)

summary(anscombe)

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)

plot(anscombe$x1, anscombe$y1, main = 'Розсіювання', xlab = 'ікси', ylab = 'ігрики', pch = 19, col = 'blue')

model <- lm(y1 ~ x1, data = anscombe)
summary(model)

coefs <- coef(model)
b0 <- coefs["(Intercept)"]
b1 <- coefs["x1"]

eq <- paste0("ŷ = ", round(b0, 3),
  ifelse(b1 >= 0, " + ", " - "),
  abs(round(b1, 3)), "·x1")
print(eq)

plot(anscombe$x1, anscombe$y1, pch = 19,
  xlab = "x1", ylab = "y1",
  main = "Регресія y1 ~ x1")
abline(model, col = "red", lwd = 2)