censo_lince <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
censo_liebre <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
censo_conejo <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")

unique(censo_conejo$individualCount)
library(tidyverse)

lince_avistamientos <- censo_lince |>
  summarise(
    year= year,
    decimalLongitude= decimalLongitude,
    decimalLatitude=  decimalLatitude
    
  )|>
  drop_na()


liebre_avistamientos <- censo_liebre |>
  summarise(
    year= year,
    decimalLongitude= decimalLongitude,
    decimalLatitude=  decimalLatitude
  )|>
  drop_na()


coordenadas_liebre <- liebre_avistamientos|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_liebre = n)



coordenadas_lince <- lince_avistamientos|>
  count(year, decimalLatitude, decimalLongitude)|>
  rename(n_lince = n)


lince_liebre <- inner_join(coordenadas_liebre, coordenadas_lince)|>
  mutate(
    n= pmin(n_lince,n_liebre)
  )
?mutate

#lince_liebre[is.na(lince_liebre)] <- 0

ggplot(lince_liebre, aes(x=n_liebre, y=n_lince, colour = year))+
  geom_point()+
  theme_minimal()



ggplot(lince_liebre, aes(x = decimalLongitude, y = decimalLatitude)) +
  # Usamos puntos cuyo tamaño y color dependan del número de coincidencias
  geom_point(aes(size = n, color = n), alpha = 0.7) +
  # Dividimos por año
  facet_wrap(~year) +
  # Estética
  scale_color_viridis_c(option = "magma") +
  theme_minimal() +
  labs(
    title = "Densidad de Coincidencias Geográficas por Año",
    size = "Nº Coincidencias",
    color = "Intensidad",
    x = "Longitud",
    y = "Latitud"
  )






library(sf)
coordlince = st_as_sf(lince_avistamientos[,3:2], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)

coordliebre = st_as_sf(liebre_avistamientos[,3:2], coords=c("decimalLongitude","decimalLatitude"),
                      crs=4258)

liebre_avistamientos
lince_avistamientos
dist()

plot(coordlince$geometry)
plot(coordliebre$geometry, col="red", add=T)

distancias = st_distance(coordlince,coordliebre)
dim(distancias)

distancias[distancias>10000]=NA
distanciascount = apply(distancias, 1, function(x){sum(x<10000)}) 

  
  
  




