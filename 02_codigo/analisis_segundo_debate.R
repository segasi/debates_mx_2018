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

# Generar lista de stopwords en español ----
# Código adaptado de aquí: http://jvera.rbind.io/post/2017/10/16/spanish-stopwords-for-tidytext-package/

# Definir stopwords ----
palabras <- c("cada", "candidato", "e", "sé", "si", "usted", "ustedes", "va", "voy")
word <-  c(tm::stopwords("spanish"), palabras)
custom_stop_words <- data_frame(word,
                                lexicon = "custom")

custom_stop_words %>% arrange(word) %>%  print(n = nrow(.))



# Top-10 palabras pronunciadas por cada candidato, facet ----
# OJO: Esta gráfica no fue incluida en el texto publicado

lista_candidatos_sd_sd <- bd_sd %>% filter(rol == "Candidato") %>% distinct(nombre)
candidatos_sd <- lista_candidatos_sd_sd$nombre


bd_sd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>% 
  anti_join(custom_stop_words) %>% # Remover stopwords
  group_by(nombre) %>% 
  count(word, sort = TRUE, 
        rol = last(rol)) %>%   
  mutate(ranking = rank(-n, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(ranking < 10) %>% 
  arrange(nombre, ranking) %>% 
  # print(n = 200)
  ggplot(aes(fct_reorder(word, ranking), n, fill = nombre)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 32), expand = c(0, 0)) +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) +
  labs(title = paste("LAS 10 PALABRAS MÁS PRONUNCIADAS POR", candidatos_sd[i], sep = " "),
       x = "",
       y = "\nFrecuencia") +
  coord_flip() +
  facet_wrap(~ nombre, ncol = 2, scales = "free") +
  tema +
  theme(panel.grid.major.y = element_blank())

# Top-10 palabras pronunciadas por cada candidato ----
# OJO: Estas gráficas no fue incluida en el texto publicado
for (i in seq_along(candidatos_sd)) {
  bd_sd %>% 
    unnest_tokens(word, dialogo) %>% 
    anti_join(custom_stop_words) %>% # Remover stopwords
    group_by(nombre) %>% 
    count(word, sort = TRUE, 
          rol = last(rol)) %>% 
    mutate(ranking = min_rank(-n)) %>% 
    ungroup() %>% 
    filter(ranking < 10, 
           nombre == candidatos_sd[i]) %>% 
    arrange(nombre, ranking) %>% 
    ggplot(aes(reorder(word, n), n)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(breaks = seq(0, 30, 2), limits = c(0, 32), expand = c(0, 0)) +
    labs(title = paste("LAS 10 PALABRAS MÁS PRONUNCIADAS POR", candidatos_sd[i], sep = " "),
         x = "",
         y = "\nFrecuencia") +
    coord_flip() +
    tema +
    theme(panel.grid.major.y = element_blank())
  
  ggsave(filename = paste("10_palabras_mas_pronunciadas", candidatos_sd[i],".jpg", sep = "_"), path = "03_graficas/palabras/segundo/top_10/", width = 15, height = 12, dpi = 100)
}


# Top-10 palabras con mayor tf-idf por candidato ----
# OJO: Esta gráfica no fue incluida en el texto publicado

# Desanidar palabras por candidato y contar su frecuencia
palabras_candidatos_sd <- bd_sd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>% 
  count(nombre, word, sort = TRUE) %>%
  ungroup()


# Contar palabras totales de cada candidato
palabras_totales <- palabras_candidatos_sd %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup()


# Unir datos de frecuencia de palabras por candidato con la cifra total de palabras que cada uno mencionó
palabras_candidatos_sd <- left_join(palabras_candidatos_sd, palabras_totales)

# Calcular el tf-idf
palabras_candidatos_sd <- palabras_candidatos_sd %>%
  bind_tf_idf(word, nombre, n) 


# Graficar el resultado
palabras_candidatos_sd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(word, tf_idf, fill = nombre)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  labs(title = "LAS 10 PALABRAS MÁS DISTINTIVAS DE CADA CANDIDATO (Tf-idf) EN EL\nSEGUNDO DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption= "\nSebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx") +
  facet_wrap(~ nombre, ncol = 2, scales = "free") +
  coord_flip() +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.caption = element_text(size = 24), 
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(size = 17, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"),
        legend.position = "none")

ggsave(filename = "tf_idf_por_candidato.jpg", path = "03_graficas/palabras/segundo/tf_idf/", width = 15, height = 12, dpi = 100)

# Análisis Anaya
palabras_candidatos_sd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "RICARDO ANAYA")

bd_sd %>% 
  filter(nombre == "RICARDO ANAYA", 
         str_detect(dialogo, "página"))


# Análisis Meade
palabras_candidatos_sd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "JOSÉ ANTONIO MEADE")

bd_sd %>% 
  filter(nombre == "JOSÉ ANTONIO MEADE", 
         str_detect(dialogo, "implica"))


# Análisis AMLO
palabras_candidatos_sd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR")

bd_sd %>% 
  filter(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR", 
         str_detect(dialogo, "implica"))



# Análisis y gráficas de bigramas ----
bd_sd_bigrams <- bd_sd %>%
  filter(rol == "Candidato") %>% 
  unnest_tokens(bigram, dialogo, token = "ngrams", n = 2) %>% 
  separate(bigram, c("palabra_1", "palabra_2"), sep = " ", remove = F) %>% 
  filter(!palabra_1 %in% custom_stop_words$word) %>%
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  count(nombre, bigram, palabra_1, palabra_2, sort = TRUE) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-n, ties.method = "first")) %>% 
  ungroup

bd_sd_bigrams %>% 
  filter(palabra_1 == "andrés" | palabra_1 == "lópez") %>% 
  arrange(bigram) %>% 
  print(n = 50)


# Una gráfica para todos los candidatos_sd
bd_sd_bigrams %>% 
  group_by() %>% 
  filter(ranking < 11) %>% 
  ungroup() %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(reorder(bigram, n), n, fill = nombre)) +
  geom_col() +
  geom_text(aes(label = n), hjust = 1.5, col ="white", fontface = "bold") +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  labs(title = "LOS 10 PARES DE PALABRAS MÁS FRECUENTES DE CADA CANDIDATO\nEN EL SEGUNDO DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption= "\nSebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx") +
  facet_wrap(~ nombre, ncol = 2, scales = "free") +
  coord_flip() +
  tema +
  theme(plot.title = element_text(size = 28),
        plot.caption = element_text(size = 24), 
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(size = 17, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"),
        legend.position = "none")

ggsave(filename = "bigrama_por_candidato_faceta.jpg", path = "03_graficas/palabras/segundo/top_10_pares/", width = 15, height = 11, dpi = 100)


# Una gráfica por candidato
for (i in seq_along(candidatos_sd)) {
  bd_sd_bigrams %>% 
    filter(ranking < 11, 
           nombre == candidatos_sd[i]) %>% 
    mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
    arrange(nombre, ranking) %>% 
    ggplot(aes(bigram, n)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(breaks = seq(0, 18, 2), limits = c(0, 19), expand = c(0, 0)) +
    labs(title = paste("LOS 10 PARES DE PALABRAS MÁS FRECUENTES EN LAS INTERVENCIONES DE\n", candidatos_sd[i], sep = " "),
         x = NULL, 
         y = "Frecuencia") +
    coord_flip() +
    tema +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 14, face = "bold", color = "white"),
          strip.background =element_rect(fill = "#66666680", color = "#66666600"))
  
  ggsave(filename = paste("top_10_pares_palabras_mas_pronunciadas", candidatos_sd[i],".jpg", sep = " "), path = "03_graficas/palabras/segundo/top_10_pares", width = 15, height = 12, dpi = 100)
}

# Análisis de trigramas ----
bd_sd %>%
  filter(rol == "Candidato") %>% 
  unnest_tokens(trigram, dialogo, token = "ngrams", n = 3) %>% 
  separate(trigram, c("palabra_1", "palabra_2", "palabra_3"), sep = " ", remove = FALSE) %>%
  
  filter(!palabra_1 %in% custom_stop_words$word) %>%
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  filter(!palabra_3 %in% custom_stop_words$word) %>%
  count(nombre, trigram, palabra_1, palabra_2, palabra_3, sort = TRUE) %>% 
  filter(n >= 2) %>% 
  print(n = 150)

# Conteo de menciones de otros candidatos_sd por cada candidato ----
bd_sd_menciones <- bd_sd %>% 
  filter(rol == "Candidato") %>% 
  group_by(nombre) %>% 
  mutate(menciones_amlo = str_count(dialogo, "Andrés Manuel López Obrador|Andrés Manuel|López Obrador|Andrés"), 
         menciones_anaya = str_count(dialogo, "Ricardo Anaya|Anaya|Ricardo|Ricky"),
         menciones_meade = str_count(dialogo, "José Antonio Meade|José Antonio|Meade|Pepe Toño"),
         menciones_bronco = str_count(dialogo, "Jaime Rodríguez \`El Bronco\`|El Bronco|Jaime Rodríguez|Jaime"),
         menciones_total = menciones_amlo + menciones_anaya + menciones_bronco + menciones_meade) %>% 
  ungroup() 


# Gráficas de barras de menciones----
bd_sd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_bronco, menciones_total) %>% 
  group_by(nombre, nombre_corto) %>% 
  summarise_all(funs(tot = sum)) %>% 
  ungroup() %>% 
  gather(key = "cand_mencionado",
         value = "num",
         -nombre, -nombre_corto) %>%  
  mutate(cand_mencionado = case_when(cand_mencionado == "menciones_amlo_tot" ~ "Veces que AMLO fue mencionado por",
                                     cand_mencionado == "menciones_anaya_tot" ~ "Veces que Anaya fue mencionado por",
                                     cand_mencionado == "menciones_bronco_tot" ~ "Veces que El Bronco fue mencionado por",
                                     cand_mencionado == "menciones_meade_tot" ~ "Veces que Meade fue mencionado por",
                                     cand_mencionado == "menciones_zavala_tot" ~ "Veces que Zavala fue mencionada por")) %>% 
  filter(!is.na(cand_mencionado)) %>% 
  ggplot(aes(nombre_corto, num)) +
  geom_col(fill = "steelblue") +
  # geom_text(aes(label = num), hjust = -1) +
  coord_flip() +
  facet_wrap(~ cand_mencionado) +
  labs(title = "NÚMERO DE VECES QUE _____ MENCIONÓ A _____ EN EL SEGUNDO DEBATE",
       x = "",
       y = "\nNúmero de menciones",
       caption = "Sebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx") +
  tema +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 26),
        axis.title.x = element_text(hjust = 0),
        strip.text = element_text(size = 16, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"))

ggsave(filename = "num_menciones_candidatos_sd.jpg", path = "03_graficas/menciones/segundo/", width = 15, height = 10, dpi = 100)  


# Gráficas de redes de menciones ----
nodos_sd <- bd_sd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_bronco) %>% 
  group_by(nombre, nombre_corto) %>% 
  summarise_all(funs(tot = sum)) %>% 
  ungroup() %>% 
  gather(key = "cand_mencionado",
         value = "num",
         -nombre, -nombre_corto) %>% 
  group_by(cand_mencionado) %>% 
  summarise(menciones_tot = sum(num)) %>% 
  ungroup() %>% 
  mutate(cand_mencionado = case_when(cand_mencionado == "menciones_amlo_tot" ~ "AMLO",
                                     cand_mencionado == "menciones_anaya_tot" ~ "Anaya",
                                     cand_mencionado == "menciones_bronco_tot" ~ "El Bronco",
                                     cand_mencionado == "menciones_meade_tot" ~ "Meade")) %>%
  rename(candidato = cand_mencionado) %>% 
  rowid_to_column("id") %>% 
  mutate(nom_etiqueta = paste(candidato, "\n(", menciones_tot, ")",sep = ""))

menciones_sd <- bd_sd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_bronco, menciones_total) %>% 
  group_by(nombre, nombre_corto) %>% 
  summarise_all(funs(tot = sum)) %>% 
  ungroup() %>% 
  gather(key = "cand_mencionado",
         value = "num",
         -nombre, -nombre_corto) %>%  
  mutate(cand_mencionado = case_when(cand_mencionado == "menciones_amlo_tot" ~ "AMLO",
                                     cand_mencionado == "menciones_anaya_tot" ~ "Anaya",
                                     cand_mencionado == "menciones_bronco_tot" ~ "El Bronco",
                                     cand_mencionado == "menciones_meade_tot" ~ "Meade")) %>% 
  filter(!is.na(cand_mencionado)) %>% 
  mutate(nombre_corto = ifelse(nombre_corto == "López Obrador", "AMLO", nombre_corto)) %>% 
  rename(weight = num) %>% 
  select(-nombre) %>% 
  filter(weight > 0)


enlaces_sd <- menciones_sd %>% 
  left_join(nodos_sd, by = c("nombre_corto" = "candidato")) %>% 
  rename(from = id) %>% 
  select(-menciones_tot)

enlaces_sd <- enlaces_sd %>% 
  left_join(nodos_sd, by = c("cand_mencionado" = "candidato")) %>% 
  rename(to = id) %>% 
  select(from, to, weight, everything())

### Guardar versiones en formato .csv 
write_csv(enlaces_sd, "04_datos_output/enlaces_sd.csv")  
write_csv(nodos_sd, "04_datos_output/nodos_sd.csv")

# Generar objeto redes con igraph
datos_red_sd <- graph_from_data_frame(d = enlaces_sd, vertices = nodos_sd, directed = TRUE)

# Cambiar tipo de letra
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))
par(family = 'avenir')

# Cambiar márgenes

png("03_graficas/menciones/segundo/num_menciones_candidatos_sd_red.png", width = 9, height = 7, units = "in", res = 200)
par(mar = c(0, 0, 1, 0))
plot.igraph(datos_red_sd, 
            edge.arrow.size = enlaces_sd$weight/1.2, 
            edge.width = enlaces_sd$weight/3, 
            edge.curved=.3,
            edge.label = enlaces_sd$weight,
            edge.label.family = "Trebuchet MS Bold",
            edge.label.font = 2,
            edge.label.color=c("grey20"),
            layout = layout_in_circle, 
            vertex.size = nodos_sd$menciones_tot,
            vertex.color = c("#a62a2a", "steelblue", "black", "#f14b4b", "#00cdcd"), 
            vertex.label = nodos_sd$nom_etiqueta,
            vertex.frame.color="#66666600", 
            vertex.label.family = "Trebuchet MS Bold",
            vertex.label.font = 2,
            vertex.label.color=c("grey20"),
            remove.loops = T,
            label.font = 2,
            cex = 2, 
            cex.main = 4,
            axes = F,
            xlim = c(-1, 1.5),
            ylim = c(-1, 1))

title(main = "NÚMERO DE VECES QUE ____ SE REFIRIÓ A ____ EN EL SEGUNDO DEBATE PRESIDENCIAL", adj = 0.2, line = -0.3, cex = 4)
text(-1.42, 1.15, labels = "La cifra entre paréntesis indica el total de veces que se mencionó el nombre de ese candidato ", adj = 0, cex = 1, col = "grey40")
text(-1.36, -1.2, labels = "Sebastián Garrido / @segasi / Juan Ricardo Pérez / @juanrpereze / oraculus.mx", adj = 0, cex = 1, col = "grey40")
dev.off()
par(mar=c(5.1,4.1,4.1,2.1))

