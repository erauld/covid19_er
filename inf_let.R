library(tidyverse)
library(tidylog)
library(readxl)
library(ggplot2)
library(roll)

#Datos diarios totales
datosdia.cl <- read_csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv"))
datosdia.cl <- datosdia.cl %>%
  rename(fecha = Fecha, casos_nuevos_sintomas = "Casos nuevos con sintomas", casos_tot = "Casos totales", casos_rec = `Casos recuperados`, casos_fall= Fallecidos, casos_act = `Casos activos`, casos_nuevos_nosintomas = `Casos nuevos sin sintomas`, casos_nuevos_dia = `Casos nuevos totales` )

# Creaci?n de variables
datosdia.cl <- datosdia.cl %>%
  mutate(let_dia_por = (casos_fall/casos_tot)*100,
         fall_nuevos = casos_fall - lag(casos_fall, default = first(casos_fall), order_by = fecha),
         fall_nuevos_prom5 = roll_mean(fall_nuevos, 5))

#Nuevos fall d?a
l1 <- datosdia.cl %>%
  filter(casos_fall>=50) %>%
  ggplot(aes(fecha, fall_nuevos_prom5)) +
  geom_line(aes(y=fall_nuevos_prom5)) +
  theme_minimal() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 4), se = T) +
  #scale_y_continuous(trans = "log2") +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d-%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Fallecidos nuevos COVID-19 Chile",
       subtitle="Desde 50 fallecidos",
       y="Fallecidos nuevos (prom. mov. 5 d?as)",
       x="D?a",
       #title="Muertes COVID 19 por Edad - Chile",
       caption="Fuente: Ministerio de Salud, Chile // @erauld"
  )
l1
#Letalidad d?a
let.dia <- datosdia.cl %>% transmute(fecha, casos_nuevos_dia, fall_nuevos, casos_tot, casos_fall, let_dia_por = round(let_dia_por,2))
l2 <- let.dia %>%
  filter(casos_fall>=50) %>%
  ggplot(aes(fecha)) +
  geom_line(aes(y=let_dia_por)) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d-%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Letalidad COVID-19 Chile",
       subtitle="Desde 50 fallecidos",
       y="Letalidad en porcentaje",
       x="D?a",
       colour = "Edad",
       #title="Muertes COVID 19 por Edad - Chile",
       caption="Fuente: Ministerio de Salud, Chile // @erauld"
  )
l2

j <- lm(fall_nuevos ~ casos_nuevos_dia + lag(casos_nuevos_dia,1)+ lag(casos_nuevos_dia,2) + lag(casos_nuevos_dia,3) + lag(casos_nuevos_dia,4) + lag(casos_nuevos_dia,5), datosdia.cl)
summary(j)