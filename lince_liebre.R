#################################################
##
##      Trabajo dinámica población Lince :)
##
################################################

library(tidyverse)
library(ggplot2)
library(mapSpain)

# Leemos los datos ####
censo_lince <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
censo_liebre <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
censo_conejo <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")

#Limpieza

lince_avistamientos <- censo_lince |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_lince = n)


conejo_avistamientos <- censo_conejo |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_conejo = n)

liebre_avistamientos <- censo_liebre |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_liebre = n)


# lince_liebre <- inner_join(coordenadas_liebre, coordenadas_lince)|>
#   mutate(
#     n= pmin(n_lince,n_liebre)
#   )
# ?mutate

#lince_liebre[is.na(lince_liebre)] <- 0

# 
# ggplot(lince_liebre, aes(x = decimalLongitude, y = decimalLatitude)) +
#   # Usamos puntos cuyo tamaño y color dependan del número de coincidencias
#   geom_point(aes(size = n, color = n), alpha = 0.7) +
#   # Dividimos por año
#   facet_wrap(~year) +
#   # Estética
#   scale_color_viridis_c(option = "magma") +
#   theme_minimal() +
#   labs(
#     title = "Densidad de Coincidencias Geográficas por Año",
#     size = "Nº Coincidencias",
#     color = "Intensidad",
#     x = "Longitud",
#     y = "Latitud"
#   )
# 





library(sf)

#coordenadas en sf
coordlince = st_as_sf(lince_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)

coordliebre = st_as_sf(liebre_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)


coordconejo = st_as_sf(conejo_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                       crs=4258)


liebre_avistamientos
lince_avistamientos
#dev.off()

plot(coordlince$geometry)
plot(coordliebre$geometry, col="red", add=T)
plot(coordconejo$geometry, col="blue", add=T)

distancias = st_distance(coordlince,coordliebre)
dim(distancias)

# distancias[distancias>10000]=NA

#Liebres a menos de 10km de cada lince
distanciascount = apply(distancias, 1, function(x){sum(x<10000)}) 

  
  
lince_pob <- censo_lince |>
  select(stateProvince, year, individualCount) |>
  replace_na(list(individualCount = 1)) |>
  mutate(stateProvince = na_if(stateProvince, "")) |>
  drop_na(stateProvince) |>
  mutate(stateProvince = if_else(stateProvince == "Huelva", "Andalucía", stateProvince)) |>
  rename(comunidad = stateProvince, año = year)

lince_comunidad <- lince_pob |>
  group_by(año, comunidad) |>
  summarise(individuos_lince = sum(individualCount))

liebre_pob <- censo_liebre |>
  select(stateProvince, year, individualCount) |>
  replace_na(list(individualCount = 1)) |>
  mutate(stateProvince = na_if(stateProvince, "")) |>
  drop_na(stateProvince) |>
  mutate(stateProvince = if_else(stateProvince %in% c(
    "Córdoba", "Almería", "Almeria", "Cádiz", "Huelva", "Jaén", 
    "Sevilla", "Granada", "Málaga"), "Andalucía", stateProvince),
    stateProvince = if_else(stateProvince %in% c(
      "Albacete", "Ciudad Real", "Guadalajara", "Toledo", "Cuenca"),
      "Castilla-La Mancha", stateProvince)) |>
  rename(comunidad = stateProvince, año = year)

liebre_comunidad <- liebre_pob |>
  group_by(año, comunidad) |>
  summarise(individuos_liebre = sum(individualCount))

lince_liebre <- left_join (lince_comunidad, liebre_comunidad)

#gráficas
gg_lince <- ggplot(lince_comunidad, aes(x = año, y = individuos_lince, color = comunidad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "right")
gg_lince

gg_liebre <- ggplot(lince_liebre, aes(x = año, y = individuos_liebre, color = comunidad)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "right")
gg_liebre

gg_lince_liebre <- ggplot(lince_liebre, aes(x = individuos_liebre, y = individuos_lince, color = año)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "right")
gg_lince_liebre
modelo <- lm(individuos_liebre ~ individuos_lince, data = lince_liebre)
summary(modelo)$r.squared





