library(tidyverse)
library(tidylog)
library(ggplot2)
library(lubridate)
library(scales)
library(patchwork)


#Muertes Chile
cv.d <- read_csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario.csv"))
cv.d <- cv.d %>% pivot_longer(starts_with("2020"), "fecha", values_to="muertes")
cv.d$fecha <- as_date(cv.d$fecha)
str(cv.d)
cv.d <- cv.d %>% rename(edad="Grupo de edad")
cv.d$edad<-factor(cv.d$edad, levels = c("<=39","40-49","50-59","60-69", "70-79", "80-89", ">=90"))


#Gráfico Barra
g1 <- cv.d %>%
  drop_na(muertes) %>%
  ggplot(aes(fecha, muertes)) +
  geom_col(aes(fill=edad), width = 1)+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype="dotted", color = "red", size=0.6) +
  #geom_line(aes(y=muertes, colour=edad)) +
  theme_minimal() +
  #scale_y_continuous(trans = "log2") +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d-%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(subtitle="Composición por edad",
       y="Muertes",
       x="Día",
       fill = "Edad",
       title="Muertes COVID 19 por Edad - Chile")
#caption="Fuente: Ministerio de Salud, Chile.")
g1

#Gr?fico L?nea
g2 <- cv.d %>%
  drop_na(muertes) %>%
  ggplot(aes(fecha)) +
  geom_line(aes(y=muertes, colour=edad)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-07")), linetype="dotted", color = "red", size=0.6) +
  theme_minimal() +
  scale_y_continuous(trans = "log2") +
  labs(x = "Día", y = "Muertes") +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d-%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(subtitle="Evolución por edad",
       y="Muertes (logs)",
       x="Día",
       colour = "Edad",
       #title="Muertes COVID 19 por Edad - Chile",
       caption="Fuente: Ministerio de Salud, Chile // @erauld"
  )
g2

g <- g1 / g2
g


cv.d2 <- pivot_wider(cv.d,names_from = "edad", values_from = "muertes" )