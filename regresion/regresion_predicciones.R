library(data.table)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(ggpattern)
library(ggprism)
library(patchwork)
library(magrittr)
library(rstatix)
library(dataPreparation)
library(tidyr)
library(datos)
library(plotrix)
library(svglite)
library(extrafont)
library(ragg)
library(readxl)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Configuración para mejora de gráficos
knitr::opts_knit$set(dev.args = list(type = "cairo"))
windows.options(antialias = "cleartype")

# Levanto la base
data <- fread("C:/Franco/Doctorado/Laboratorio/38_marcio_hc_mci/regresion/recuerdo_agradable.csv")

#data <- data %>%
#  select(id, grupo, contains("promedio"),
#         sum_pal_cont,
#         span_inverso_z_score, 
#         hayling_b_z_score,
#         tmt_b_z_score)

#data <- data %>%
#  group_by(id) %>%
#  summarize(across(where(is.numeric), mean))

# Features a graficar
features <- list("Prediction" = "y_pred")

# Variables del eje y.
psycho_tests <- list('Real value'= "y_true")

# features <- list(list("fluency_p_entero_todo_log_frq_promedio","fluency_animals_entero_todo_log_frq_promedio"),
#                  list("fluency_p_entero_todo_sa_num_phon_promedio","fluency_animals_entero_todo_sa_num_phon_promedio"),
#                  list("fluency_p_entero_todo_sa_NP_promedio","fluency_animals_entero_todo_sa_NP_promedio"),
#                  list("fluency_p_entero_todo_familiarity_promedio","fluency_animals_entero_todo_familiarity_promedio"),
#                  list("fluency_p_entero_todo_imageability_promedio","fluency_animals_entero_todo_imageability_promedio"),
#                  list("fluency_p_entero_todo_osv","fluency_animals_entero_todo_osv"),
#                  list("fluency_p_entero_todo_granularidad_filtrada_promedio","fluency_animals_entero_todo_granularidad_filtrada_promedio"),
#                  list("fluency_p_entero_cantidad_palabras_correctas","fluency_animals_entero_cantidad_palabras_correctas"))

# Función para que los ticks de los ejes x e y se vean lindos en cantidad y decimales
get_range <- function(x){
  valor_minimo <- min(x)
  valor_maximo <- max(x)
  
  if ((valor_minimo%%1==0) && (valor_maximo%%1==0)){
    divisible <- FALSE
    aumento_max=0
    while (!divisible){
      if((((valor_maximo+aumento_max)-valor_minimo)%%4)==0){
        rango <- seq(valor_minimo, valor_maximo+aumento_max+1, by = (((valor_maximo+aumento_max)-valor_minimo)/4))
        divisible <- TRUE
      }else if ((((valor_maximo+aumento_max)-valor_minimo)%%5)==0){
        rango <- seq(valor_minimo, valor_maximo+aumento_max+1, by = (((valor_maximo+aumento_max)-valor_minimo)/5))
        divisible <- TRUE
      }
      aumento_max <- aumento_max+1
    }
  }else{
    if ((valor_maximo-valor_minimo)>2){
      rango <-round(seq(valor_minimo, valor_maximo, by = ((valor_maximo-valor_minimo)/5)),1)
    }else if (((valor_maximo-valor_minimo)<2) && ((valor_maximo-valor_minimo)>1)){
      rango <-round(seq(valor_minimo, valor_maximo, by = ((valor_maximo-valor_minimo)/5)),2)
    }else if (((valor_maximo-valor_minimo)<1)){
      rango <-round(seq(valor_minimo, valor_maximo, by = ((valor_maximo-valor_minimo)/5)),3)
    }
  }
  
  return (rango)
}

# Doble for para variables en eje x y en eje y
for (i_psycho in seq_along(psycho_tests)){
  for (i_feature in seq_along(features)){
    #myplots <- vector('list', length(databases))
    
    #base_sin_outlier<-data[[i_uni_base]] %>% drop_na(features[[i_feature]])
    #base_sin_outlier<-base_sin_outlier %>% drop_na(psycho_tests[[i_psycho]])
    
    # base_sin_outlier <- remove_sd_outlier(base_sin_outlier[, c(features[[i_feature]],psycho_tests[[i_psycho]])], cols = "auto", n_sigmas = 3, verbose = TRUE)
    #base_sin_outlier <- data[, c(features[[i_feature]],psycho_tests[[i_psycho]])]
    base_sin_outlier <- subset(data, select = c(features[[i_feature]], psycho_tests[[i_psycho]]))
    
    valor_minimo_x <- min(base_sin_outlier[[features[[i_feature]]]])
    valor_maximo_x <- max(base_sin_outlier[[features[[i_feature]]]])
    
    valor_minimo_y <- min(base_sin_outlier[[psycho_tests[[i_psycho]]]])
    valor_maximo_y <- max(base_sin_outlier[[psycho_tests[[i_psycho]]]])
    a<-ggscatter(base_sin_outlier, x=features[[i_feature]], y=psycho_tests[[i_psycho]],add = "reg.line", conf.int = TRUE,
                 add.params = list(color = "black", fill = "lightgray")) +
      geom_point(size = 4, color= "#ff66c4") +
      theme(legend.position="none",
            panel.background = element_rect(fill = "white", colour = "grey50"),
            axis.title.x = element_text(size = 32,face="bold"),
            axis.title.y = element_text(size = 32,face="bold"),
            axis.text=element_text(size=18, colour="black", face='bold'),
            plot.title = element_text(size=27,hjust = 0.48,face='bold'))+
      labs(x=names(features)[i_feature], y=names(psycho_tests)[i_psycho])
      #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, size = 6)
    
    #scale_x_continuous(breaks = get_range(base_sin_outlier[[features[[i_feature]]]])) +
    #scale_y_continuous(breaks = get_range(base_sin_outlier[[psycho_tests[[i_psycho]]]]))
    # scale_x_continuous(breaks = round(seq(valor_minimo_x, valor_maximo_x, by = ((valor_maximo_x-valor_minimo_x)/4)),1)) +
    # scale_y_continuous(breaks = round(seq(valor_minimo_y, valor_maximo_y, by = ((valor_maximo_y-valor_minimo_y)/4)),1))
    ggsave(file=paste(getwd(),"/imagenes/recuerdo_agradable.svg", sep = ""), plot=a, width=10, height=6)
    #print(a)
    
  }
}