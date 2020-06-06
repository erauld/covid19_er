library(tidyverse)
library(tidylog)
library(readxl)
library(ggplot2)
library(roll)
library(lubridate)


#Cargar BBDD
pop.cl <- read_excel("C:/Users/eraul/Documents/ER/Estudio/corona_virus/datos/pop_cl.xlsx")
deaths.cl <- read_csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_std.csv"))
casos.cl <- read_csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario_std.csv"))


#Edades

tramosed <- c("<=39","40-49","50-59","60-69", "70-79", ">=80")

#Arreglar bases

## Casos
casos.cl <- casos.cl %>% 
  rename(edad = "Grupo de edad" , sexo = Sexo, fecha = Fecha, casos_conf = "Casos confirmados") %>% 
  group_by(edad, fecha) %>% 
  summarise(casos_conf = sum(casos_conf))



casos.cl$edad[casos.cl$edad %in% c(  "00 - 04 años",
                                     "05 - 09 años",
                                     "10 - 14 años",
                                     "15 - 19 años",
                                     "20 - 24 años",
                                     "25 - 29 años",
                                     "30 - 34 años",
                                     "35 - 39 años")] <- "<=39"

casos.cl$edad[casos.cl$edad %in% c("40 - 44 años",
                                   "45 - 49 años")] <- "40-49"

casos.cl$edad[casos.cl$edad %in% c("50 - 54 años",
                                   "55 - 59 años")] <- "50-59"

casos.cl$edad[casos.cl$edad %in% c("60 - 64 años",
                                   "65 - 69 años")] <- "60-69"

casos.cl$edad[casos.cl$edad %in% c("70 - 74 años",
                                   "75 - 79 años")] <- "70-79"

casos.cl$edad[casos.cl$edad %in% c("80 y más años")] <- ">=80"


casos.cl$edad <- factor(casos.cl$edad, levels = tramosed)

casos.cl <- casos.cl %>% 
  select(fecha, edad, everything()) %>% 
  group_by(fecha, edad) %>% 
  summarise(casos_conf = sum(casos_conf)) %>%  
  arrange(fecha, edad)


## Base fallecidos

deaths.cl <- deaths.cl %>% 
  rename(edad = "Grupo de edad" , casos_fall = "Casos confirmados")


deaths.cl$edad[deaths.cl$edad %in% c("80-89",
                                   ">=90")] <- ">=80"

deaths.cl <- deaths.cl %>%
  group_by(fecha, edad) %>% 
  summarise(casos_fall=sum(casos_fall))

deaths.cl$edad <- factor(deaths.cl$edad, levels = tramosed)

deaths.cl <- deaths.cl %>%
  select(fecha, edad, everything()) %>% 
  arrange(fecha, edad)


## joint

epi.cl <- left_join(casos.cl, deaths.cl)
pop.cl$edad <- factor(pop.cl$edad, levels=tramosed)
epi.cl <- left_join(epi.cl, pop.cl)


#Creación de variables.

epi2.cl <- epi.cl %>% 
  mutate(let_edad_porc = round((casos_fall/casos_conf)*100, 2),
         let_edad_cm = round((casos_fall/casos_conf)*100000,2),
         inc_casos_cm = round((casos_conf/pop)*100000,2),
         inc_fall_cm = round((casos_fall/pop)*100000,2))


#Gráfico

t1 <- epi2.cl %>%
  drop_na(let_edad_porc) %>% 
  ggplot(aes(fecha)) +
  geom_line(aes(y=let_edad_porc, colour=edad)) +
  theme_minimal() +
  scale_y_continuous(trans = "log2") +
  labs(x = "Día", y = "Muertes") +
  scale_x_date(date_breaks = "2 day", date_labels =  "%d-%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(subtitle="Evolución por edad",
       y="Letalidad en porcentaje",
       x="Día",
       colour = "Edad",
       title="Tasa Letalidad  COVID 19 por Edad - Chile",
       caption="Fuente: Ministerio de Salud, Chile // @erauld"
  )


t1
