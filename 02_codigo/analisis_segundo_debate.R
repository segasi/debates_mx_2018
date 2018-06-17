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


### Cargar texto del segundo debate ----
segundo_debate <- "http://segasi.com.mx/clases/cide/vis_man/datos/segundo_debate_completo_bis.docx" %>%
  download() %>%
  read_docx(remove.empty = TRUE, trim = TRUE)

### Transformaciones ----

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



### Eliminar algunos términos que incluyeron los capturistas pero que no son palabras mencionadas por los candidatos o moderadores ----
bd_pd <- bd_pd %>% 
  mutate(dialogo = str_replace(dialogo, "\\(INAUDIBLE\\)", ""),
         dialogo = str_replace(dialogo, "\\(Inaudible\\)", ""),
         dialogo = str_replace(dialogo, "\\(PANELISTAS\\)", ""),
         dialogo = str_replace(dialogo, "\\(SIC\\)", ""),
         dialogo = str_replace(dialogo, "\\(Sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(sic\\)", ""),
         dialogo = str_replace(dialogo, "\\(FALLA DE ORIGEN\\)", ""))

### Guardar base en formato .csv ----
write_csv(bd_sd, path = "04_datos_output/bd_segundo_debate.csv")


### Generar variable que indique a qué debate corresponde el texto ----
bd_sd <- bd_sd %>% 
  mutate(num_debate = 2)


### Análisis usando tidytext ----

# Contar palabras totales por candidato
bd_sd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>%   
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() 


# Contar palabras totales por moderador
bd_sd %>% 
  filter(rol == "Moderador") %>% 
  unnest_tokens(word, dialogo) %>%   
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup()  

### Gráfica de palabras totales de cada candidato ----
bd_sd %>% 
  filter(rol %in% c("Moderador", "Candidato")) %>% 
  unnest_tokens(word, dialogo) %>%   
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  mutate(pal_por_min = paste(round(total/20.5, 0), "palabras por min.", sep = " "),
         pal_por_min = ifelse(nombre %in% c("LEÓN KRAUZE", "YURIRIA SIERRA"), NA, pal_por_min)) %>%  
  ggplot(aes(reorder(nombre, total), total, fill = nombre)) +
  geom_col() +
  geom_text(aes(label = paste("Total: ", comma(total), sep = "")), hjust = 1.15, vjust = -0.6, size = 7, col = "white", fontface = "bold") +
  geom_text(aes(label = pal_por_min), hjust = 1.12, vjust = 1.35,  size = 6, col = "white") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3800)) +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a", "#00cdcd", "grey50", "grey50")) +
  labs(title = "NÚMERO DE PALABRAS PRONUNCIADAS POR CADA CANDIDATO\nY MODERADOR EN EL SEGUNDO DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption= "\nSebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx") +
  coord_flip() +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.caption = element_text(size = 24), 
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(size = 17, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"),
        legend.position = "none")

ggsave(filename = "palabras_por_actor.jpg", path = "03_graficas/palabras/segundo/", width = 15, height = 10, dpi = 100)



### Gráfica de palabras totales de cada candidato o moderador ----
bd_sd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>%   
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(reorder(nombre, total), total, fill = nombre)) +
  geom_col() +
  geom_text(aes(label = paste("Total: ", comma(total), sep = "")), hjust = 1.15, vjust = -0.6, size = 7, col = "white", fontface = "bold") +
  geom_text(aes(label = paste(round(total/25, 0), " palabras x min.", sep = "")), hjust = 1.12, vjust = 1.35,  size = 6, col = "white") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3800)) +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  labs(title = "NÚMERO DE PALABRAS PRONUNCIADAS POR CADA CANDIDATO\nEN EL SEGUNDO DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption= "\nSebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx") +
  coord_flip() +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.caption = element_text(size = 24), 
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(size = 17, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"),
        legend.position = "none")

ggsave(filename = "palabras_por_candidato.jpg", path = "03_graficas/palabras/segundo/", width = 15, height = 10, dpi = 100)

