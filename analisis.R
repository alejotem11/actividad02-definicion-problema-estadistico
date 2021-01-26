# Quitar el comentario de la siguiente línea si no está instalado ggplot2
# install.packages("ggplot2")

library(ggplot2)

# TODO: Ajustar la ruta según corresponda
ruta.dataset <- "<DIRECTORIO_DEL_DATASET>/dataset_final.csv"
df <- read.csv(file = ruta.dataset, header = T)

# Ignorar edades
ignorar.edades <- c("De 16 a 19 años", "De 65 a 69 años", "70 Y Más Años")
registrosAEliminar <- which(df$Edad %in% ignorar.edades)
df <- df[-aEliminar,]

# Transformar la edad a <<factor>> para ordenar las gráficas
df$Edad <- factor(df$Edad, levels = c(
  "De 20 a 24 años",
  "De 25 a 29 años",
  "De 30 a 34 años",
  "De 35 a 39 años",
  "De 40 a 44 años",
  "De 45 a 49 años",
  "De 50 a 54 años",
  "De 55 a 59 años",
  "De 60 a 64 años"
))

# Ordenar ascendentemente los registros por trimestre
df <- df[order(df$Trimestre),]
df$Trimestre <- factor(df$Trimestre)

# Transformar el sexo a <<factor>> para usarlo como leyenda en los gráficos
df$Sexo <- factor(df$Sexo)

# Se separan los registros por segmento (Activos y Ocupados) para sacar la
# tasa de ocupación
campos <- c("Edad", "Sexo", "Trimestre", "Cantidad")
df.activos <- df[df$Segmento == "Activos",][campos]
df.ocupados <- df[df$Segmento == "Ocupados",][campos]

# Renombrar la columna <<Cantidad>>
colnames(df.activos)[4] <- "Activos"
colnames(df.ocupados)[4] <- "Ocupados"

# Tasa de ocupación
df.ratio <- merge(df.activos, df.ocupados, by = c("Trimestre", "Edad", "Sexo"))
df.ratio$Ratio <- df.ratio$Ocupados / df.ratio$Activos

color.hombres <- "#56B4E9"
color.mujeres <- "#F8917E"
colors.vector <- c(color.hombres, color.mujeres)

#******************************************************************************#
#*              Comparación de tasa de ocupación por edad y sexo              *#
#******************************************************************************#
# ******** Diagrama de cajas y bigotes
p_box_sex <- ggplot(data = df.ratio, aes(x = Edad, y = Ratio)) +
  geom_boxplot(aes(fill = Sexo)) +
  facet_wrap(~Edad, scales="free") +
  xlab(NULL) + ylab(NULL) + ggtitle("Tasa de ocupación") +
  coord_cartesian(ylim = c(0.4, 1)) +
  scale_fill_manual(values = colors.vector)
p_box_sex
# ******** Evolución en el tiempo
# Convertir los trimestres a número para ajustar los valores del eje x:
trimestres.2010.2019 <- 40
trimestres.2020 <- 3
total.trimestres <- trimestres.2010.2019 + trimestres.2020
df.ratio$TrimestreAsNum <- df.ratio$Trimestre
levels(df.ratio$TrimestreAsNum) <- 1:total.trimestres
trimestres.intervalo.size <- 8
labels.trimestre <- c("1" = 2010,
                      "9" = 2012,
                      "17" = 2014,
                      "25" = 2016,
                      "33" = 2018,
                      "41" = 2020)
p_line <- ggplot(data = df.ratio, aes(x = TrimestreAsNum, y = Ratio, group = Sexo)) +
  geom_line(aes(color = Sexo))+
  facet_wrap(~Edad, scales="free") +
  xlab(NULL) + ylab(NULL) + ggtitle("Tasa de ocupación") +
  coord_cartesian(ylim = c(0.4, 1)) +
  scale_color_manual(values = colors.vector) +
  scale_x_discrete(breaks = seq(1, total.trimestres, trimestres.intervalo.size),
                   labels = labels.trimestre)
p_line

#******************************************************************************#
#*         Comparación de tasa de ocupación de las mujeres por edad           *#
#******************************************************************************#
df.mujeres <- df.ratio[df.ratio$Sexo == "Mujeres",]
# Diagrama de cajas y bigotes
p_box_m <- ggplot(data = df.mujeres, aes(x=Edad, y = Ratio)) + 
  geom_boxplot(fill = color.mujeres) +
  xlab(NULL) + ylab(NULL) + ggtitle("Tasa de ocupación de mujeres")
p_box_m



# Gráfica de la tasa de ocupación de hombres vs tasa de ocupación de mujeres
# df.hombres <- df.ratio[df.ratio$Sexo == "Hombres",]
# df.mujeres <- df.ratio[df.ratio$Sexo == "Mujeres",]
# plot(df.hombres$Ratio, df.mujeres$Ratio)