#################################################
##
##      Trabajo dinámica población Lince :)
##
#################################################

library(tidyverse)
library(ggplot2)
library(patchwork)
library(mapSpain)
library(rnaturalearth)
library(sf)
library(units)
library(rnaturalearth)



# Leemos los datos ####
censo_lince <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
censo_liebre <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
censo_conejo <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")


# Limpiamos los datos #####
## Lince ####
lince_avistamientos <- censo_lince |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_lince = n)

lista_lince <- split(lince_avistamientos, lince_avistamientos$year)

## Conejo ####
conejo_avistamientos <- censo_conejo |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_conejo = n)

lista_conejo <- split(conejo_avistamientos, conejo_avistamientos$year)

## Liebre ####
liebre_avistamientos <- censo_liebre |>
                        select(year, decimalLongitude, decimalLatitude)|>
                        count(year, decimalLatitude, decimalLongitude)|>
                        rename(n_liebre = n)

lista_liebre <- split(liebre_avistamientos, liebre_avistamientos$year)


# Coordenadas en sf sin año ####
## Lince ####
coordlince = st_as_sf(lince_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)

## Conejo ####
coordconejo = st_as_sf(conejo_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                       crs=4258)
## Liebre ####
coordliebre = st_as_sf(liebre_avistamientos[,2:3], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)

#dev.off()

# Coordenadas con años ####
## Lince ####
lista_sf_lince <- lapply(lista_lince, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                                           crs=4258)})
## Conejo ####
lista_sf_conejo <- lapply(lista_conejo, 
                          function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                               crs=4258)})

## Liebre ####
lista_sf_liebre <- lapply(lista_liebre, 
                         function(x){st_as_sf(x, coords=c("decimalLongitude","decimalLatitude"),
                                              crs=4258)})

# Distancias entre linces ####
distancias_año_linces <- lapply(lista_sf_lince ,function(x)
                          {st_distance(x)})

# Linces a menos de 10 Km
matriz_distancias_año = lapply(distancias_año_linces, function(x)
                        {rowSums(x< set_units(10000, "m"))-1
                        })


# Distancias entre linces y liebres ####
años_comunes_liebre <- intersect(names(lista_sf_lince), names(lista_sf_liebre))

distancias_año_liebres <- lapply(años_comunes_liebre, function(año){
                          linces <- lista_sf_lince[[año]]
                          liebres <- lista_sf_liebre[[año]]
                          st_distance(linces, liebres)
                          })

# Liebres a menos de 10 Km
matriz_distancias_liebre = lapply(distancias_año_liebres, function(x)
                            {rowSums(x< set_units(10000, "m"))
                            })
names(matriz_distancias_liebre) <- años_comunes_liebre

# Distancias entre linces y conejos ####
años_comunes_conejo <- intersect(names(lista_sf_lince), names(lista_sf_conejo))

distancias_año_conejo <- lapply(años_comunes_conejo, function(año){
                          linces <- lista_sf_lince[[año]]
                          conejos <- lista_sf_conejo[[año]]
                          st_distance(linces, conejos)
                          })

# Conejos a menos de 10 Km
matriz_distancias_conejo = lapply(distancias_año_conejo, function(x)
                            {rowSums(x< set_units(10000, "m"))
                            })
names(matriz_distancias_conejo) <- años_comunes_conejo

# Unión de linces, liebres y conejos a menos de 10 Km ####
años_con_presas <- union(names(lista_sf_conejo), names(lista_sf_liebre))
años_presas_lince <- intersect(names(lista_sf_lince), años_con_presas)

distancias_finales <- lapply(años_presas_lince, function(año){
                      n_año <- length(matriz_distancias_año[[año]])
                      cercanos <- data.frame(
                      linces = matriz_distancias_año[[año]],
                      
                      liebres = if(año %in% names(matriz_distancias_liebre)) {
                      matriz_distancias_liebre[[año]] 
                      } else { rep(NA, n_año) },
  
                      conejos = if(año %in% names(matriz_distancias_conejo)) {
                      matriz_distancias_conejo[[año]] 
                      } else { rep(NA, n_año) }
                      )
                      return(cercanos)
                      })
names(distancias_finales) <- names(lista_sf_lince)

## Lista con coordenadas e individuos a menos de 10 Km
lista_final <- setNames(lapply(names(lista_sf_lince), function(año){
  lista_sf_lince[[año]]$linces_cerca <- distancias_finales[[año]]$linces
  lista_sf_lince[[año]]$liebres_cerca <- distancias_finales[[año]]$liebres
  lista_sf_lince[[año]]$conejos_cerca <- distancias_finales[[año]]$conejos
  return(lista_sf_lince[[año]])
}), names(lista_sf_lince))


# Mapas ####
## Mapa de España
españa <- esp_get_ccaa()

## Mapa de Portugal
portugal <- ne_countries(scale = "medium",
                         country = "Portugal",
                         returnclass = "sf") |>
  st_crop(xmin = -10, xmax = -6, ymin = 36, ymax =43)

## Linces cercanos ####
mapas_linces <- lapply(names(lista_final), function(año){
  ggplot() +
    geom_sf(data = españa, fill = "white") + 
    geom_sf(data = portugal, fill = "white") +
    geom_sf(data = lista_final[[año]], aes(color=linces_cerca), size = 2) +
    scale_color_viridis_c(name = "Linces a menos de 10 Km") + 
    theme_minimal() + 
    labs(title = paste("Distribución de linces en", as.character(año)))
    })

## Recortado
año_mapa <- "2010"
ggplot() +
  geom_sf(data = españa, fill = "#FFFAFA") +
  geom_sf(data = lista_final[[año_mapa]], aes(color= linces_cerca), size = 3) +
  scale_color_viridis_c(begin = 0, end = 0.85) +
  theme_minimal() +
  labs(title = paste("Distribución de linces en", año_mapa),
       color = "Linces a menos\nde 10 Km") +
  theme(legend.position = "left", legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  coord_sf(xlim = c(-7, -1), ylim = c(36, 40))

## Liebres cercanas ####
mapas_liebres <- setNames(lapply(names(lista_final), function(año){
  ggplot() +
    geom_sf(data = españa, fill = "white") + 
    geom_sf(data = portugal, fill = "white") +
    geom_sf(data = lista_final[[año]], aes(color=liebres_cerca), size = 2) +
    scale_color_viridis_c(name = "Liebres a menos de 10 Km") + 
    theme_minimal() + 
    labs(title = paste("Distribución de linces en", as.character(año)))
  }), names(lista_final))

## Recortado
ggplot() +
  geom_sf(data = españa, fill = "#FFFAFA") +
  geom_sf(data = lista_final[[año_mapa]], aes(color= liebres_cerca), size = 1) +
  scale_color_viridis_c(limits = c(0, 3)) +
  theme_minimal() +
  labs(title = paste("Distribución de linces en", año_mapa),
       color = "Liebres a menos\nde 10 Km") +
  theme(legend.position = "left", legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  coord_sf(xlim = c(-7, -1), ylim = c(36, 40))

## Conejos cercanos ####
mapas_conejos <- lapply(names(lista_final), function(año){
  ggplot() +
    geom_sf(data = españa, fill = "white") + 
    geom_sf(data = portugal, fill = "white") +
    geom_sf(data = lista_final[[año]], aes(color=conejos_cerca), size = 2) +
    scale_color_viridis_c(name = "Conejos a menos de 10 Km") + 
    theme_minimal() + 
    labs(title = paste("Distribución de linces en", as.character(año)))
})

## Recortado
ggplot() +
  geom_sf(data = españa, fill = "#FFFAFA") +
  geom_sf(data = lista_final[[año_mapa]], aes(color= conejos_cerca), size = 1) +
  scale_color_viridis_c(limits = c(0, 3)) +
  theme_minimal() +
  labs(title = paste("Distribución de linces en", año_mapa),
       color = "Conejos a menos\nde 10 Km") +
  theme(legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  coord_sf(xlim = c(-7, -1), ylim = c(36, 40))




## Linces en Parques Nacionales ####
getwd()
setwd("1_data")
enps <- st_read("Enp/Enp2025.shp")
enps$geometry

espana <- ne_countries(scale = "medium", country = "Spain", returnclass = "sf")

dev.off()

#Gráfico con los puntos normales
plot(coordlince$geometry)
plot(subset(enps, FIGURA_LP == "Parque Nacional"), col="blue", add= TRUE)
plot(st_geometry(espana), add = TRUE)

#Gráfico con los puntos como si fuesen linces, lo malo es que se hacen muy grandes
plot(coordlince$geometry, pch= 10, )
coords <- st_coordinates(coordlince)
text(coords[,1], coords[,2], labels = "🐱")
plot(subset(enps, FIGURA_LP == "Parque Nacional"), col="blue", add= TRUE)
plot(st_geometry(espana),  add = TRUE)

#Vemos el porcentaje de linces dentro de los parques nacionales
ppnn <- subset(enps, FIGURA_LP == "Parque Nacional")
dentro <- st_within(coordlince, ppnn, sparse = FALSE)
en_parque <- rowSums(dentro) > 0

porcentaje <- mean(en_parque) * 100
porcentaje


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

#plot(coordlince$geometry)
#plot(coordliebre$geometry, col="red", add=T)
#plot(coordconejo$geometry, col="blue", add=T)
# 
# distancias_liebre_lince = st_distance(coordlince,coordliebre)
# distancias_conejo_lince =  st_distance(coordlince,coordconejo)
# 
# dim(distancias_liebre_lince)
# dim(distancias_conejo_lince)
# 
# # distancias[distancias>10000]=NA
# 
# #Liebres a menos de 10km de cada lince
# distanciascount = apply(distancias_liebre_lince, 1, function(x){sum(x<10000)}) 
# 

#gg_lince <- ggplot(lince_comunidad, aes(x = año, y = individuos_lince, color = comunidad)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  theme_minimal() +
#  theme(legend.position = "right")
#gg_lince

#gg_liebre <- ggplot(lince_liebre, aes(x = año, y = individuos_liebre, color = comunidad)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  theme_minimal() +
#  theme(legend.position = "right")
#gg_liebre

#gg_lince_liebre <- ggplot(lince_liebre, aes(x = individuos_liebre, y = individuos_lince, color = año)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  theme_minimal() +
#  theme(legend.position = "right")
#gg_lince_liebre
#modelo <- lm(individuos_liebre ~ individuos_lince, data = lince_liebre)
#summary(modelo)$r.squared