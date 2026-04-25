#Necesarios los datos de lista_final_intervalos y librerías del script principal

#Liebre
#Intervalos
lista_gráficos_liebre_intervalos <-  lapply(names(lista_final_intervalos), function(año){
  ggplot(lista_final_intervalos[[año]], aes(liebres_cerca))+
    geom_point(aes(y=linces_cerca), col= "red", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    theme_minimal()+
    labs(title =as.character(año),
         x= "Liebres",
         y= "Linces")
})

wrap_plots(lista_gráficos_liebre_intervalos, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )


#Años 20
años_20 <- as.character(c(2020:2024))

lista_gráficos_liebre_años <-  lapply(años_20, function(año){
    ggplot(lista_final_años[[año]], aes(liebres_cerca))+
    geom_point(aes(y=linces_cerca), col= "red", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    theme_minimal()+
    labs(title =as.character(año),
         x= "Liebres",
         y= "Linces")
})

wrap_plots(lista_gráficos_liebre_años, nrow = 2)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )


#Conejo
lista_gráficos_conejo_intervalos <-  lapply(names(lista_final_intervalos), function(año){
  ggplot(lista_final_intervalos[[año]], aes(conejos_cerca))+
    geom_point(aes(y=linces_cerca), col= "blue", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    theme_minimal()+
    labs(title =as.character(año),
         x= "Conejos",
         y= "Linces")
})

wrap_plots(lista_gráficos_conejo_intervalos, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

#Años 20

lista_gráficos_conejo_años <-  lapply(años_20, function(año){
  ggplot(lista_final_años[[año]], aes(conejos_cerca))+
    geom_point(aes(y=linces_cerca), col= "blue", alpha= 0.3)+
    scale_y_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(
      breaks = function(x) {
        max_val <- max(x, na.rm = TRUE)
        if(max_val <= 5) {
          return(0:ceiling(max_val))
        }
        cortes <- pretty(c(0, max_val), n = 5)
        return(unique(floor(cortes)))
      },
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    )+
    theme_minimal()+
    labs(title =as.character(año),
         x= "Conejos",
         y= "Linces")
})

wrap_plots(lista_gráficos_conejo_años, nrow = 2)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )
