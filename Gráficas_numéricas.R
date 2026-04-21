#Necesarios los datos de lista_final y librerías del script principal
#Liebre

lista_gráficos_liebre <-  lapply(names(lista_final), function(año){
  ggplot(lista_final[[año]], aes(linces_cerca))+
    geom_point(aes(y=liebres_cerca), col= "red", alpha= 0.3)+
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
         x= "linces cerca",
         y= "presas cerca")
})

wrap_plots(lista_gráficos_liebre, nrow = 3)+
  plot_annotation(
    title = "Liebres cercanas a cada lince",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

#Conejo
lista_gráficos_conejo <-  lapply(names(lista_final), function(año){
  ggplot(lista_final[[año]], aes(linces_cerca))+
    geom_point(aes(y=conejos_cerca), col= "blue", alpha= 0.3)+
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
         x= "linces cerca",
         y= "presas cerca")
})

wrap_plots(lista_gráficos_conejo, nrow = 3)+
  plot_annotation(
    title = "Conejos cercanos a cada lince",
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )
