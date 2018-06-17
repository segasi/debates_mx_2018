### Paquetes ----
library(pacman)
p_load(animation, cowplot, extrafont, forcats, gganimate, 
       ggforce, ggmap, ggraph, ggrepel, ggridges, hrbrthemes, 
       igraph, janitor, lubridate, mapdata, maps, maptools, 
       purrr, readxl, rgdal, rgeos, scales, sp, splitstackshape, 
       stringi, stringr, stringdist, syuzhet, textreadr, tidyr, 
       tidygraph, tidytext, tidyverse, tm, treemapify, tweenr, 
       udpipe, zoo)

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))



### PRIMER DEBATE ----
## Cargar texto ----
primer_debate <- "http://segasi.com.mx/clases/cide/vis_man/datos/primer_debate_completo.docx" %>%
  download() %>%
  read_docx(skip = 3, remove.empty = TRUE, trim = TRUE)


## Transformaciones ----

# Generar un renglón para cada intervención ----
bd_pd <- primer_debate %>% 
  str_replace_all("DENISE MAERKER, CONDUCTORA:", "~DENISE MAERKER~") %>%
  str_replace_all("DENISE MAERKER:", "~DENISE MAERKER~") %>%
  str_replace_all("AZUCENA URESTI, MODERADORA:", "~AZUCENA URESTI~") %>%
  str_replace_all("AZUCENA URESTI:", "~AZUCENA URESTI~") %>%
  str_replace_all("SERGIO SARMIENTO, CONDUCTOR:", "~SERGIO SARMIENTO~") %>%
  str_replace_all("SERGIO SARMIENTO:", "~SERGIO SARMIENTO~") %>%
  str_replace_all("VOZ EN OFF:", "~VOZ EN OFF~") %>%
  str_replace_all("MARGARITA ZAVALA, CANDIDATA INDEPENDIENTE A LA PRESIDENCIA:", "~MARGARITA ZAVALA~") %>%
  str_replace_all("MARGARITA ZAVALA:", "~MARGARITA ZAVALA~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL TODOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE COMO CANDIDATO PRESIDENCIAL DEL PRI:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA:", "~RICARDO ANAYA~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO DE LA COALICIÓN `JUNTOS HAREMOS HISTORIA` A LA PRESIDENCIA DE LA REPÚBLICA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL PUES OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>%  
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE A LA PRESIDENCIA:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE A LA PRESIDENCIA DE LA REPÚBLICA", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERON:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ `EL BRONCO`:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_split("~")

# Nombrar la lista como "diálogo" ----
names(bd_pd) <- "dialogo"

# Guardar la lista como un data frame ----
bd_pd <- as_data_frame(bd_pd)

# Generar una columna para el nombre de quién habla, reordenar columnas y eliminar espacios en blanco
bd_pd <- bd_pd %>% 
  mutate(nombre = ifelse(str_detect(dialogo, "DENISE MAERKER|AZUCENA URESTI|SERGIO SARMIENTO|VOZ EN OFF|MARGARITA ZAVALA|JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN"), dialogo, NA),
         nombre = lag(nombre)) %>% 
  filter(!is.na(nombre)) %>% 
  mutate(rol = ifelse(str_detect(nombre, "DENISE MAERKER|AZUCENA URESTI|SERGIO SARMIENTO"), "Moderador", ifelse(str_detect(nombre, "MARGARITA ZAVALA|JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN"), "Candidato", "Voz en Off"))) %>% 
  mutate(nombre_corto = case_when(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR" ~ "López Obrador",
                                  nombre == "JAIME RODRÍGUEZ CALDERÓN" ~ "El Bronco",
                                  nombre == "JOSÉ ANTONIO MEADE" ~ "Meade",
                                  nombre == "MARGARITA ZAVALA" ~ "Zavala",
                                  nombre == "RICARDO ANAYA" ~ "Anaya")) %>% 
  mutate(dialogo = str_trim(dialogo, "both")) %>% 
  select(nombre, nombre_corto, rol, dialogo)  



# Eliminar algunos términos que incluyeron los capturistas pero que no son palabras mencionadas por los candidatos o moderadores ----
bd_pd <- bd_pd %>% 
  mutate(dialogo = str_replace(dialogo, "\\(INAUDIBLE\\)", ""),
         dialogo = str_replace(dialogo, "\\(Inaudible\\)", ""),
         dialogo = str_replace(dialogo, "\\(PANELISTAS\\)", ""),
         dialogo = str_replace(dialogo, "\\(SIC\\)", ""),
         dialogo = str_replace(dialogo, "\\(Sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(FALLA DE ORIGEN\\)", ""))

# Generar variable que indique a qué debate corresponde el texto ----
bd_pd <- bd_pd %>% 
  mutate(num_debate = 1)




### SEGUNDO DEBATE ----
## Cargar texto ----
segundo_debate <- "http://segasi.com.mx/clases/cide/vis_man/datos/segundo_debate_completo_bis.docx" %>%
  download() %>%
  read_docx(remove.empty = TRUE, trim = TRUE)

## Transformaciones ----

# Generar un renglón para cada intervención ----
bd_sd <- segundo_debate %>% 
  str_replace_all("LEÓN KRAUZE, CONDUCTOR:", "~LEÓN KRAUZE~") %>%
  str_replace_all("LEÓN KRAUZE, MODERADOR:", "~LEÓN KRAUZE~") %>%
  str_replace_all("LEÓN KRAUZE:", "~LEÓN KRAUZE~") %>%
  str_replace_all("LEON KRAUZE:", "~LEÓN KRAUZE~") %>%
  str_replace_all("YURIRIA SIERRA, CONDUCTORA:", "~YURIRIA SIERRA~") %>%
  str_replace_all("YURIRIA SIERRA:", "~YURIRIA SIERRA~") %>%
  str_replace_all("YURIRIA SIERRA:", "~YURIRIA SIERRA~") %>%
  str_replace_all("YURIRIA SIERRA, COLABORADORA", "~YURIRIA SIERRA~") %>%
  str_replace_all("YURIRIA SIERRA, MODERADORA:", "~YURIRIA SIERRA~") %>%
  str_replace_all("VOZ EN OFF:", "~VOZ EN OFF~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL TODOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE KURIBREÑA, CANDIDATO DE LA COALICIÓN `TODOS POR MÉXICO` A LA PRESIDENCIA DE LA REPÚBLICA:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE KURIBREÑA", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE KURIBREÑO", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA CORTÉS, CANDIDATO DE LA COALICIÓN `POR MÉXICO AL FRENTE` A LA PRESIDENCIA DE LA REPÚBLICA:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA CORTÉS:", "~RICARDO ANAYA~") %>% 
  str_replace_all("RICARDO ANAYA:", "~RICARDO ANAYA~") %>% 
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL:", "~RICARDO ANAYA~") %>%
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO DE LA COALICIÓN `JUNTOS HAREMOS HISTORIA` A LA PRESIDENCIA DE LA REPÚBLICA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO DE LA COALICIÓN `JUNTOS HAREMOS HISTORIA` A LA PRESIDENCIA DE LA REPÚBLICA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL ÓPEZ OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL PUES OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO PRESIDENCIAL INDEPENDIENTE:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE A LA PRESIDENCIA DE LA REPÚBLICA:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO PRESIDENCIAL:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERON:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ `EL BRONCO`:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("LUIS ÁNGEL AMADOR PÉREZ, ASISTENTE:", "~ASISTENTE~") %>%
  str_replace_all("DIEGO DOMÍNGUEZ SÁNCHEZ, ASISTENTE:", "~ASISTENTE~") %>%
  str_replace_all("TERESA REYNAGA, ASISTENTE:", "~ASISTENTE~") %>%
  str_replace_all("GERARDO OSUNA, ASISTENTE DEBATE:", "~ASISTENTE~") %>%
  str_replace_all("VENECIA ZENDEJAS, PARTICIPANTE:", "~ASISTENTE~") %>%
  str_replace_all("TERESA MERCADO, PARTICIPANTE:", "~ASISTENTE~") %>% 
  str_split("~")  

# Nombrar la lista como "diálogo" ----
names(bd_sd) <- "dialogo"

# Guardar la lista como un data frame ----
bd_sd <- as_data_frame(bd_sd)


# Generar una columna para el nombre de quién habla, reordenar columnas y eliminar espacios en blanco
bd_sd <- bd_sd %>% 
  mutate(nombre = ifelse(str_detect(dialogo, "LEÓN KRAUZE|YURIRIA SIERRA|VOZ EN OFF|JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN|ASISTENTE"), dialogo, NA),
         nombre = lag(nombre)) %>% 
  filter(!is.na(nombre)) %>% 
  mutate(rol = ifelse(str_detect(nombre, "LEÓN KRAUZE|YURIRIA SIERRA"), "Moderador", ifelse(str_detect(nombre, "JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN"), "Candidato", ifelse(str_detect(nombre, "ASISTENTE"), "Público", "Voz en Off")))) %>% 
  mutate(nombre_corto = case_when(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR" ~ "López Obrador",
                                  nombre == "JAIME RODRÍGUEZ CALDERÓN" ~ "El Bronco",
                                  nombre == "JOSÉ ANTONIO MEADE" ~ "Meade",
                                  nombre == "RICARDO ANAYA" ~ "Anaya")) %>% 
  mutate(dialogo = str_trim(dialogo, "both")) %>% 
  select(nombre, nombre_corto, rol, dialogo) %>%
  mutate(rol = ifelse(!is.na(nombre_corto), "Candidato", rol))



# Eliminar algunos términos que incluyeron los capturistas pero que no son palabras mencionadas por los candidatos o moderadores ----
bd_pd <- bd_pd %>% 
  mutate(dialogo = str_replace(dialogo, "\\(INAUDIBLE\\)", ""),
         dialogo = str_replace(dialogo, "\\(Inaudible\\)", ""),
         dialogo = str_replace(dialogo, "\\(PANELISTAS\\)", ""),
         dialogo = str_replace(dialogo, "\\(SIC\\)", ""),
         dialogo = str_replace(dialogo, "\\(Sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(FALLA DE ORIGEN\\)", ""))

# Generar variable que indique a qué debate corresponde el texto ----
bd_sd <- bd_sd %>% 
  mutate(num_debate = 2)



### TERCER DEBATE ----
## Cargar texto ----
tercer_debate <- "http://segasi.com.mx/clases/cide/vis_man/datos/tercer_debate_presidencial_completo.docx" %>%
  download() %>%
  read_docx(remove.empty = TRUE, trim = TRUE)

## Transformaciones ----

# Generar un renglón para cada intervención ----
bd_td <- tercer_debate %>% 
  str_replace_all("GABRIELA WARKENTIN, CONDUCTORA:", "~GABRIELA WARKENTIN~") %>%
  str_replace_all("GABRIELA WARKENTIN, MODERADORA:", "~GABRIELA WARKENTIN~") %>%
  str_replace_all("GABRIELA WARKENTIN:", "~GABRIELA WARKENTIN~") %>%
  str_replace_all("GABRIEL WARKENTIN, MODERADORA:", "~GABRIELA WARKENTIN~") %>%str_replace_all("CARLOS PUIG, CONDUCTOR:", "~CARLOS PUIG~") %>%
  str_replace_all("CARLOS PUIG, MODERADOR:", "~CARLOS PUIG~") %>%
  str_replace_all("CARLOS PUIG:", "~CARLOS PUIG~") %>%
  str_replace_all("LEONARDO CURZIO CONDUCTOR:", "~LEONARDO CURZIO~") %>%
  str_replace_all("LEONARDO CURZIO, CONDUCTOR:", "~LEONARDO CURZIO~") %>%
  str_replace_all("LEONARDO CURZIO, MODERADOR:", "~LEONARDO CURZIO~") %>%
  str_replace_all("LEONARDO CURZIO:", "~LEONARDO CURZIO~") %>%
  str_replace_all("VOZ EN OFF:", "~VOZ EN OFF~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE CANDIDATO DE LA COALICIÓN TODOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL PRI:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL DE LA COALICIÓN TODOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO DE LA COALICIÓN TODOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO A LA PRESIDENCIA DE LA COALICIÓN UNIDOS POR MÉXICO:", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("JOSÉ ANTONIO MEADE, CANDIDATO PRESIDENCIAL DE LA COALICIÓN TODOS POR MÉXICO: ", "~JOSÉ ANTONIO MEADE~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO DE LA COALICIÓN POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA CANDIDATO DE POR FRENTE AL MÉXICO:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA:", "~RICARDO ANAYA~") %>% 
  str_replace_all("RICARDO ANAYA, CANDIDATO A LA PRESIDENCIA DE LA COALICIÓN POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>% 
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL PAN:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL DE LA COALICIÓN POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL DE LA COALICIÓN POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL POR MÉXICO AL FRENTE:", "~RICARDO ANAYA~") %>%
  str_replace_all("RICARDO ANAYA, CANDIDATO PRESIDENCIAL:", "~RICARDO ANAYA~") %>%
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO DE LA COALICIÓN JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR CANDIDATO DE JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL DE LA COALICIÓN JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL MORENA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL POR LA COALICIÓN JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO PRESIDENCIAL DE LA COALICION JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("ANDRÉS MANUEL LÓPEZ OBRADOR, CANDIDATO A LA PRESIDENCIA DE LA COALICIÓN JUNTOS HAREMOS HISTORIA:", "~ANDRÉS MANUEL LÓPEZ OBRADOR~") %>% 
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE A LA PRESIDENCIA DE LA REPÚBLICA:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO INDEPENDIENTE A LA PRESIDENCIA DE LA REPÚBLICA:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO PRESIDENCIAL INDEPENDIENTE:", "~JAIME RODRÍGUEZ CALDERÓN~") %>% 
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO PRESIDENCIAL:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_replace_all("JAIME RODRÍGUEZ CALDERÓN, CANDIDATO PRESIDENCIAL INDEPENDIENTE:", "~JAIME RODRÍGUEZ CALDERÓN~") %>%
  str_split("~")  

# Nombrar la lista como "diálogo" ----
names(bd_td) <- "dialogo"

# Guardar la lista como un data frame ----
bd_td <- as_data_frame(bd_td)

# Generar una columna para el nombre de quién habla, reordenar columnas y eliminar espacios en blanco
bd_td <- bd_td %>% 
  mutate(nombre = ifelse(str_detect(dialogo, "GABRIELA WARKENTIN|CARLOS PUIG|LEONARDO CURZIO|VOZ EN OFF|JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN|ASISTENTE"), dialogo, NA),
         nombre = lag(nombre)) %>% 
  filter(!is.na(nombre)) %>% 
  mutate(rol = ifelse(str_detect(nombre, "GABRIELA WARKENTIN|CARLOS PUIG|LEONARDO CURZIO"), "Moderador", ifelse(str_detect(nombre, "JOSÉ ANTONIO MEADE|RICARDO ANAYA|ANDRÉS MANUEL LÓPEZ OBRADOR|JAIME RODRÍGUEZ CALDERÓN"), "Candidato", "Voz en Off"))) %>% 
  mutate(nombre_corto = case_when(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR" ~ "López Obrador",
                                  nombre == "JAIME RODRÍGUEZ CALDERÓN" ~ "El Bronco",
                                  nombre == "JOSÉ ANTONIO MEADE" ~ "Meade",
                                  nombre == "RICARDO ANAYA" ~ "Anaya")) %>% 
  mutate(dialogo = str_trim(dialogo, "both")) %>% 
  select(nombre, nombre_corto, rol, dialogo) %>%
  mutate(rol = ifelse(!is.na(nombre_corto), "Candidato", rol))


# Eliminar algunos términos que incluyeron los capturistas pero que no son palabras mencionadas por los candidatos o moderadores ----
bd_td <- bd_td %>% 
  mutate(dialogo = str_replace(dialogo, "\\(INAUDIBLE\\)", ""),
         dialogo = str_replace(dialogo, "\\(PANELISTAS\\)", ""),
         dialogo = str_replace(dialogo, "\\(SIC\\)", ""),
         dialogo = str_replace(dialogo, "\\(FALLA DE ORIGEN\\)", ""),
         dialogo = str_replace(dialogo, "\\(FRASE OTOMÍ\\)", ""))


# Generar variable que indique a qué debate corresponde el texto ----
bd_td <- bd_td %>% 
  mutate(num_debate = 3)



### Unir base de datos de debates ----
bd <- rbind(bd_pd, bd_sd)
bd <- rbind(bd, bd_td)

### Guardar la base de datos de los tres debates como archivo .csv ----
write_csv(bd, path = "04_datos_output/bd_tres_debates.csv")
