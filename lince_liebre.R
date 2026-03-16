censo_lince <- read.csv("1_data/linx_pardinus.csv")
censo_liebre <- read.csv("1_data/lepus_granatensis.csv")


lince_avistamientos <- censo_lince |>
  summarise(
    year= year,
    decimalLatitude=  signif(decimalLatitude, 3),
    decimalLongitude=  signif(decimalLongitude, 2)
  )

liebre_avistamientos <- censo_liebre |>
  summarise(
    year= year,
    decimalLatitude=  signif(decimalLatitude, 3),
    decimalLongitude=  signif(decimalLongitude, 2)
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

