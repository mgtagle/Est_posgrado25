# Importar datos a R proveniente de diferentes fuentes
# 10/02/2025
# 

# Importar datos ----------------------------------------------------------

ocampo <- read.csv("Datos_ocampo.csv", header = T)
ocampo
View(ocampo)

names(ocampo)

mean(ocampo$TEMP)
mean(ocampo$DIRS)

boxplot(ocampo$TEMP, col = "lightgreen",
        main = "Temperatura Ocampo")
hist(ocampo$TEMP, col = "indianred",
     main= "Sitio Ocampo",
     xlab = "Tempretura (C)",
     ylab = "Frecuencia")

stem(ocampo$TEMP)

boxplot(ocampo$VELR)

vivero <- read.csv("vivero.csv", header = T)
vivero$Tratamiento <- as.factor(vivero$Tratamiento)
boxplot(vivero$IE ~ vivero$Tratamiento)

# Revisar la normalidad de los datos IE

shapiro.test(vivero$IE)
ctrl <- subset(vivero$IE, vivero$Tratamiento == "Ctrl")
fert <- subset(vivero$IE, vivero$Tratamiento != "Ctrl")
var(ctrl)
var(fert)
# Revisar homogeneidad de varianzas
bartlett.test(vivero$IE ~ vivero$Tratamiento)


t.test(vivero$IE~ vivero$Tratamiento, var.equal = F)

t.test(vivero$IE~ vivero$Tratamiento, paired =T)

# Pruebas dependientes

t.test(vivero$IE ~ vivero$Tratamiento, paired = T)


# t.test una sola muestra

t.test(vivero$IE, mu = 0.90)
