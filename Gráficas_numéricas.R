#Necesarios los datos de lista_final_años y librerías del script principal
#Liebre

lista_gráficos_liebre <-  lapply(names(lista_final_años), function(año){
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

wrap_plots(lista_gráficos_liebre, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

#Conejo
lista_gráficos_conejo <-  lapply(names(lista_final_años), function(año){
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

wrap_plots(lista_gráficos_conejo, nrow = 3)+
  plot_annotation(
    title = "Ocurrencias de linces a menos de 10km",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )
