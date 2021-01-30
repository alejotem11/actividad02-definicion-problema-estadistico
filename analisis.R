# Quitar el comentario de la siguiente línea si no está instalado ggplot2
# install.packages("ggplot2")

library(tidyverse) #incluye ggplot2 y otros 
library(readxl) #read_xlsx

# TODO: Ajustar la ruta según corresponda
ruta.dataset <- "<DIRECTORIO_DEL_DATASET>/dataset_final.csv"
df <- read.csv(file = ruta.dataset, header = T)
#Si se trabaja con un proyecto no es necesario poner la ruta, al final cada persona 
#tiene su propia ruta y no es reproducible. Esto debería funcionar:
df <- read.csv("dataset_final.csv")

# Ignorar edades
ignorar.edades <- c("De 16 a 19 años", "De 65 a 69 años", "70 Y Más Años")
aEliminar <- which(df$Edad %in% ignorar.edades) 
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

color.hombres <- "limegreen"
color.mujeres <- "purple1"
colors.vector <- c(color.hombres, color.mujeres)

#******************************************************************************#
#*              Comparación de tasa de ocupación por edad y sexo              *#
#******************************************************************************#
# ******** Diagrama de cajas y bigotes
ggplot(data = df.ratio, aes(x = Edad, y = Ratio)) +
  geom_boxplot(aes(fill = Sexo)) +
  facet_wrap(~Edad, scales="free", ncol = 3) +
  xlab(NULL) + 
  ylab(NULL) + 
  ggtitle("Tasa de ocupación") +
  coord_cartesian(ylim = c(0.4, 1)) +
  scale_fill_manual(values = colors.vector) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 16),
        plot.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

#******************************************************************************#
#*              Media, mediana, desviación, ceof. de variación                *#
#******************************************************************************#

valores <- df.ratio %>% 
              group_by(Edad, Sexo) %>% 
              summarise(media = mean(Ratio),
                        mediana = median(Ratio),
                        desv = sd(Ratio),
                        coef.var = desv/media)


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

ggplot(data = df.ratio, aes(x = TrimestreAsNum, y = Ratio, group = Sexo)) +
  geom_line(aes(color = Sexo), size = 1)+
  facet_wrap(~Edad, scales="free") +
  xlab(NULL) + 
  ylab(NULL) + 
  coord_cartesian(ylim = c(0.4, 1)) +
  scale_color_manual(values = colors.vector) +
  scale_x_discrete(breaks = seq(1, total.trimestres, trimestres.intervalo.size),
                   labels = labels.trimestre) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.title = element_blank(), #elimina el título
        legend.title = element_blank(),
        legend.text = element_text(size = 16))


#******************************************************************************#
#*         Comparación de tasa de ocupación de las mujeres por edad           *#
#******************************************************************************#
df.mujeres <- df.ratio[df.ratio$Sexo == "Mujeres",]

df.mujeres$year <- substr(df.mujeres$Trimestre, 1, 4)
df.mujeres$year <- as.numeric(df.mujeres$year)

# Diagrama de cajas y bigotes
ggplot(data = df.mujeres, aes(x=Edad, y = Ratio)) + 
  geom_boxplot(fill = color.mujeres) +
  xlab(NULL) + ylab(NULL) + ggtitle("Tasa de ocupación de mujeres")

df.mujeres.simpl <- df.mujeres %>%
                      filter(year %in% c(2010, 2020)) %>%
                      group_by(Edad, year) %>%
                      summarise(promedio_ratio = mean(Ratio))
df.mujeres.2010 <- filter(df.mujeres.simpl , year %in% c(2010))
colnames(df.mujeres.2010 )[3] <-"promedio_ratio_2010"
df.mujeres.2020 <- filter(df.mujeres.simpl , year %in% c(2020))
colnames(df.mujeres.2020)[3] <-"promedio_ratio_2020"

###################Lollipop charts
mujeres.lolipop <- df.mujeres.2010[, -2]
mujeres.lolipop <- cbind(mujeres.lolipop, df.mujeres.2020[,3])
mujeres.lolipop$diferencia <- NA
mujeres.lolipop$diferencia <- mujeres.lolipop$promedio_ratio_2020 - mujeres.lolipop$promedio_ratio_2010
mujeres.lolipop$Edad_corta <- NA
mujeres.lolipop$Edad_corta <- substr(mujeres.lolipop$Edad, 4, 15)

ggplot(mujeres.lolipop) +
  geom_segment(aes(x=Edad_corta, xend=Edad_corta, y=promedio_ratio_2020, yend=promedio_ratio_2010), 
               color="black", size = 1) +
  geom_point(aes(x=Edad_corta, y=promedio_ratio_2020, color = "darkblue"), size=5) +
  geom_point(aes(x=Edad_corta, y=promedio_ratio_2010, color = "lightblue"), size=5) +
  scale_color_manual(values = c("darkblue", "lightblue"), 
                     labels = c("2020", "2010")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16, angle = 50, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  xlab("") +
  ylab("Promedio de la tasa de ocupación-mujer") +
  ggtitle("b)")


#Histograma
ggplot(df.mujeres, aes(Edad, Ratio)) + 
  geom_bar(stat = "identity", fill = color.mujeres) +
  xlab(NULL) + 
  ylab(NULL) + 
  ggtitle("Tasa de ocupación de mujeres") +
  theme_bw() 

##################Promedio############################
promedios <- read_xlsx("promedios_anuales.xlsx", sheet = 2)

scaleFUN <- function(x) sprintf("%.0f", x)

ggplot(promedios, aes(year, promedio_mujeres, color = color.mujeres)) +
  geom_line(size = 1) +
  geom_line(aes(year, promedio_hombres, color = color.hombres), size = 1) +
  scale_color_manual(values = c(color.hombres, color.mujeres), labels = c("Hombre", "Mujer")) +
  ylab("Promedio") +
  xlab(" ") +
  theme_bw() + 
  scale_x_continuous(labels = scaleFUN) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16))


##################Brecha############################
promedios <- promedios %>%
              mutate(brecha = (promedio_hombres - promedio_mujeres)*100)

ggplot(promedios, aes(year, brecha)) +
  geom_bar(stat = "identity") +
  ylab("Brecha (%)") +
  xlab(" ") +
  theme_bw() + 
  scale_x_continuous(labels = scaleFUN) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

###############Lollipop hombres/mujeres
ggplot(promedios) +
  geom_segment(aes(x=year, xend=year, y=promedio_hombres, yend=promedio_mujeres), 
               color="black", size = 1) +
  geom_point(aes(x=year, y=promedio_hombres, color = color.hombres), size=5) +
  geom_point(aes(x=year, y=promedio_mujeres, color = color.mujeres), size=5) +
  scale_color_manual(values = c(color.hombres, color.mujeres), 
                     labels = c("Hombre", "Mujer")) +
  scale_x_continuous(labels = scaleFUN) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  xlab("") +
  ylab("Promedio de la tasa de ocupación") +
  ggtitle("a)") 

# Gráfica de la tasa de ocupación de hombres vs tasa de ocupación de mujeres
# df.hombres <- df.ratio[df.ratio$Sexo == "Hombres",]
# df.mujeres <- df.ratio[df.ratio$Sexo == "Mujeres",]
# plot(df.hombres$Ratio, df.mujeres$Ratio)

edad1 <- c("72 %", "77 %")
edad2 <- c("81 %", "84 %")

d <- data.frame(edad1, edad2)
colnames(d) <- c("20-29 años", "40-64 años")
rownames(d) <- c("Mujeres", "Hombres")
