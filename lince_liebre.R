#################################################
##
##      Trabajo dinámica población Lince :)
##
################################################

library(tidyverse)
library(ggplot2)
library(mapSpain)
library(sf)

# Leemos los datos ####
censo_lince <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
censo_liebre <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
censo_conejo <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")

#Limpieza

lince_avistamientos <- censo_lince |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_lince = n)

lista_lince <- split(lince_avistamientos, lince_avistamientos$year)

conejo_avistamientos <- censo_conejo |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_conejo = n)

lista_conejo <- split(conejo_avistamientos, conejo_avistamientos$year)

liebre_avistamientos <- censo_liebre |>
  select(year, decimalLongitude, decimalLatitude)|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_liebre = n)

lista_liebre <- split(liebre_avistamientos, liebre_avistamientos$year)

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






#coordenadas en sf sin año
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


#Con años
lista_sf_lince <- lapply(lista_lince, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                                           crs=4258)})

lista_sf_liebre <- lapply(lista_liebre, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                              crs=4258)})
lista_sf_conejo <- lapply(lista_conejo, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                              crs=4258)})

distancias_año_linces <- lapply(lista_sf_lince ,function(x)
  {st_distance(x)})

# matriz_distancias_año = lapply(lista_sf_lince ,function(matriz)
#   x <- as.matrix(matriz),
#   {apply(x, 1, function(y){sum(y<10000)-1})}) 
# Probar con loop

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





