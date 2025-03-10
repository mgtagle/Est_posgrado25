

# Regresión lineal

geyser <- read.csv("erupciones.csv", header = T)

plot(geyser$waiting , geyser$eruptions, pch = 19,
     xlab = "Tiempo de espera (min)",
     ylab = "Duración (min)")

cor(geyser$waiting, geyser$eruptions)
cor.test(geyser$waiting, geyser$eruptions)

g.lm <- lm(geyser$eruptions ~ geyser$waiting)
g.lm
summary(g.lm)


# Graficar la línea de tendencia central
plot(geyser$waiting , geyser$eruptions, pch = 19,
     xlab = "Tiempo de espera (min)",
     ylab = "Duración (min)")
abline(g.lm, col="red")

-1.8 + 0.07*60

g.lm$coefficients[1]+g.lm$coefficients[2]*60

geyser$yprima <- g.lm$fitted.values

geyser$residuales <- g.lm$residuals
sum(geyser$residuales)

geyser$res2 <- geyser$residuales^2

sum(geyser$res2)/270 # Varianza del modelo

mod.lm <- anova(g.lm)
mod.lm

sum(geyser$res2)
