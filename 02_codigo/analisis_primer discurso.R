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



### Cargar texto del primer debate ----
primer_debate <- "http://segasi.com.mx/clases/cide/vis_man/datos/primer_debate_completo.docx" %>%
  download() %>%
  read_docx(skip = 3, remove.empty = TRUE, trim = TRUE)


### Transformaciones ----

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
write_csv(bd_pd, path = "04_datos_output/bd_primer_debate.csv")

### Generar variable que indique a qué debate corresponde el texto ----
bd_pd <- bd_pd %>% 
  mutate(num_debate = 1)


### Análisis usando tidytext ----

# Contar palabras totales por candidato
bd_pd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>% 
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  print()

# Contar palabras totales por moderador
bd_pd %>% 
  filter(rol == "Moderador") %>% 
  unnest_tokens(word, dialogo) %>%   
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  print() 

# Gráfica palabras totales de cada candidato ----

# En el análisis original consideramos que cada candidato tenía hasta 19 minutos para intervenir en el debate, cuando en realidad tenían 16 minutos. El código a continuación ya incluye esta corrección.

bd_pd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>% 
  count(nombre, word, sort = TRUE) %>%
  ungroup() %>%
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "MARGARITA ZAVALA", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(reorder(nombre, total), total, fill = nombre)) +
  geom_col() +
  geom_text(aes(label = paste("Total: ", comma(total), sep = "")), hjust = 1.15, vjust = -0.6, size = 7, col = "white", fontface = "bold") +
  geom_text(aes(label = paste(round(total/16, 0), " palabras x min. disponible", sep = "")), hjust = 1.12, vjust = 1.35,  size = 6, col = "white") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  labs(title = "NÚMERO DE PALABRAS MENCIONADAS POR CADA CANDIDATO\nEN EL PRIMER DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption = "\nSebastián Garrido de Sierra / @segasi / oraculus.mx") +
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

ggsave(filename = "palabras_por_candidato.jpg", path = "03_graficas/palabras/primero/", width = 15, height = 10, dpi = 100)

# Generar lista de stopwords en español ----
# Código adaptado de aquí: http://jvera.rbind.io/post/2017/10/16/spanish-stopwords-for-tidytext-package/

# Definir stopwords ----
palabras_pd <- c("cada", "candidato", "e", "sé", "si", "usted", "ustedes", "va", "voy")
word <-  c(tm::stopwords("spanish"), palabras_pd)
custom_stop_words <- data_frame(word,
                                lexicon = "custom")

custom_stop_words %>% arrange(word) %>%  print(n = nrow(.))


# Top-10 palabras mencionadas por cada candidato ----
lista_candidatos <- bd_pd %>% filter(rol == "Candidato") %>% distinct(nombre)
candidatos <- lista_candidatos$nombre

# Generar una gráfica por candidato
for (i in seq_along(candidatos)) {
  bd_pd %>% 
    unnest_tokens(word, dialogo) %>% 
    anti_join(custom_stop_words) %>% # Remover stopwords
    group_by(nombre) %>% 
    count(word, sort = TRUE, 
          rol = last(rol)) %>% 
    mutate(ranking = min_rank(-n)) %>% 
    ungroup() %>% 
    filter(ranking < 10, 
           nombre == candidatos[i]) %>% 
    arrange(nombre, ranking) %>% 
    ggplot(aes(reorder(word, n), n)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 21), expand = c(0, 0)) +
    labs(title = paste("LAS 10 PALABRAS MÁS MENCIONADAS POR", candidatos[i], sep = " "),
         x = "",
         y = "\nFrecuencia") +
    coord_flip() +
    tema +
    theme(panel.grid.major.y = element_blank())
  
  ggsave(filename = paste("10_palabras_mas_mencionadas", candidatos[i],".jpg", sep = "_"), path = "03_graficas/palabras/primero/top_10", width = 15, height = 12, dpi = 100)
}


# Top-10 palabras con mayor tf-idf por candidato ----

# Desanidar palabras por actor y contar su frecuencia
palabras_por_actor_pd <- bd_pd %>% 
  unnest_tokens(word, dialogo) %>% 
  count(nombre, word, sort = TRUE) %>%
  ungroup()

# Contar palabras totales de cada actor
palabras_totales_por_actor_pd <- palabras_por_actor_pd %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup()


# Desanidar palabras por candidato y contar su frecuencia
palabras_candidatos_pd <- bd_pd %>% 
  filter(rol == "Candidato") %>% 
  unnest_tokens(word, dialogo) %>% 
  count(nombre, word, sort = TRUE) %>%
  ungroup()


# Contar palabras totales de cada candidato y moderador
palabras_totales_pd <- palabras_candidatos_pd %>% 
  group_by(nombre) %>%
  summarize(total = sum(n)) %>% 
  ungroup()


# Unir datos de frecuencia de palabras por candidato con la cifra total de palabras que cada uno mencionó
palabras_candidatos_pd <- left_join(palabras_candidatos_pd, palabras_totales_pd)

# Calcular el tf-idf
palabras_candidatos_pd <- palabras_candidatos_pd %>%
  bind_tf_idf(word, nombre, n) 


# Graficar el resultado
palabras_candidatos_pd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "MARGARITA ZAVALA", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(word, tf_idf, fill = nombre)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  labs(title = "LAS 10 PALABRAS MÁS DISTINTIVAS DE CADA CANDIDATO (Tf-idf) EN EL\nPRIMER DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption = "\nSebastián Garrido de Sierra / @segasi / oraculus.mx") +
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

ggsave(filename = "tf_idf_por_candidato.jpg", path = "03_graficas/palabras/primero/tf_idf/", width = 15, height = 18, dpi = 100)

# Análisis Anaya
palabras_candidatos_pd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "RICARDO ANAYA")

bd_pd %>% 
  filter(nombre == "RICARDO ANAYA", 
         str_detect(dialogo, "página"))


# Análisis Meade
palabras_candidatos_pd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "JOSÉ ANTONIO MEADE")

bd_pd %>% 
  filter(nombre == "JOSÉ ANTONIO MEADE", 
         str_detect(dialogo, "implica"))


# Análisis AMLO
palabras_candidatos_pd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  arrange(nombre, -tf_idf) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-tf_idf, ties.method = "first")) %>% 
  ungroup %>% 
  filter(ranking < 11) %>% 
  filter(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR")

bd_pd %>% 
  filter(nombre == "ANDRÉS MANUEL LÓPEZ OBRADOR", 
         str_detect(dialogo, "implica"))


# Análisis y gráficas de bigramas ----
bd_pd_bigrams <- bd_pd %>%
  filter(rol == "Candidato") %>% 
  unnest_tokens(bigram, dialogo, token = "ngrams", n = 2) %>% 
  separate(bigram, c("palabra_1", "palabra_2"), sep = " ", remove = F) %>% 
  filter(!palabra_1 %in% custom_stop_words$word) %>%
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  count(nombre, bigram, palabra_1, palabra_2, sort = TRUE) %>% 
  group_by(nombre) %>% 
  mutate(ranking = rank(-n, ties.method = "first")) %>% 
  ungroup

bd_pd_bigrams %>% 
  filter(palabra_1 == "andrés" | palabra_1 == "lópez") %>% 
  arrange(bigram) %>% 
  print(n = 50)


# Una gráfica para todos los candidatos
bd_pd_bigrams %>% 
  group_by() %>% 
  filter(ranking < 11) %>% 
  ungroup() %>% 
  mutate(nombre = fct_relevel(nombre, "RICARDO ANAYA", "JOSÉ ANTONIO MEADE", "ANDRÉS MANUEL LÓPEZ OBRADOR", "MARGARITA ZAVALA", "JAIME RODRÍGUEZ CALDERÓN")) %>% 
  ggplot(aes(reorder(bigram, n), n, fill = nombre)) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "#f14b4b", "#a62a2a",  "#00cdcd", "grey50")) + 
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "LOS 10 PARES DE PALABRAS MÁS FRECUENTES DE CADA CANDIDATO\nEN EL PRIMER DEBATE PRESIDENCIAL",
       x = NULL, 
       y = NULL, 
       caption = "\nSebastián Garrido de Sierra / @segasi / oraculus.mx") +
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

ggsave(filename = "bigrama_por_candidato_faceta.jpg", path = "03_graficas/palabras/primero/top_10_pares/", width = 15, height = 18, dpi = 100)


# Una gráfica por candidato
for (i in seq_along(candidatos)) {
  bd_pd_bigrams %>% 
    filter(ranking < 11, 
           nombre == candidatos[i]) %>% 
    mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
    arrange(nombre, ranking) %>% 
    ggplot(aes(bigram, n)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(breaks = seq(0, 18, 2), limits = c(0, 19), expand = c(0, 0)) +
    labs(title = paste("LOS 10 PARES DE PALABRAS MÁS FRECUENTES EN LAS INTERVENCIONES DE\n", candidatos[i], sep = " "),
         x = NULL, 
         y = "Frecuencia") +
    coord_flip() +
    tema +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(size = 14, face = "bold", color = "white"),
          strip.background =element_rect(fill = "#66666680", color = "#66666600"))
  
  ggsave(filename = paste("top_10_pares_palabras_mas_mencionadas", candidatos[i],".jpg", sep = "_"), path = "03_graficas/palabras/primero/top_10_pares", width = 15, height = 12, dpi = 100)
}

# Análisis de trigramas ----
bd_pd %>%
  filter(rol == "Candidato") %>% 
  unnest_tokens(trigram, dialogo, token = "ngrams", n = 3) %>% 
  separate(trigram, c("palabra_1", "palabra_2", "palabra_3"), sep = " ", remove = FALSE) %>%
  
  filter(!palabra_1 %in% custom_stop_words$word) %>%
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  filter(!palabra_3 %in% custom_stop_words$word) %>%
  count(nombre, trigram, palabra_1, palabra_2, palabra_3, sort = TRUE) %>% 
  filter(n >= 2) %>% 
  print(n = 150)

# Conteo de menciones de otros candidatos por cada candidato ----
bd_pd_menciones <- bd_pd %>% 
  filter(rol == "Candidato") %>% 
  group_by(nombre) %>% 
  mutate(menciones_amlo = str_count(dialogo, "Andrés Manuel López Obrador|Andrés Manuel|López Obrador|Andrés"), 
         menciones_anaya = str_count(dialogo, "Ricardo Anaya|Anaya|Ricardo"),
         menciones_meade = str_count(dialogo, "José Antonio Meade|José Antonio|Meade|Pepe Toño"),
         menciones_zavala = str_count(dialogo, "Margarita Zavala|Margarita|Zavala"),
         menciones_bronco = str_count(dialogo, "Jaime Rodríguez \`El Bronco\`|El Bronco|Jaime Rodríguez|Jaime"),
         menciones_total = menciones_amlo + menciones_anaya + menciones_bronco + menciones_zavala + menciones_meade) %>% 
  ungroup() 


# Gráficas de barras de menciones ----
bd_pd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_zavala, menciones_bronco, menciones_total) %>% 
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
  coord_flip() +
  facet_wrap(~ cand_mencionado) +
  labs(title = "NÚMERO DE VECES QUE _____ MENCIONÓ A _____ EN EL PRIMER DEBATE",
       x = "",
       y = "\nNúmero de menciones") +
  tema +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 26),
        axis.title.x = element_text(hjust = 0),
        strip.text = element_text(size = 16, face = "bold", color = "white"),
        strip.background =element_rect(fill = "#66666680", color = "#66666600"))

ggsave(filename = "num_menciones_candidatos.jpg", path = "03_graficas/menciones/primero/", width = 15, height = 10, dpi = 100)  


# Gráficas de redes de menciones ----
nodos_pd <- bd_pd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_zavala, menciones_bronco) %>% 
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
                                     cand_mencionado == "menciones_meade_tot" ~ "Meade",
                                     cand_mencionado == "menciones_zavala_tot" ~ "Zavala")) %>%
  rename(candidato = cand_mencionado) %>% 
  rowid_to_column("id") %>% 
  mutate(nom_etiqueta = paste(candidato, "\n(", menciones_tot, ")",sep = ""))

menciones_pd <- bd_pd_menciones %>% 
  select(nombre, nombre_corto, menciones_amlo, menciones_anaya, menciones_meade, menciones_zavala, menciones_bronco, menciones_total) %>% 
  group_by(nombre, nombre_corto) %>% 
  summarise_all(funs(tot = sum)) %>% 
  ungroup() %>% 
  gather(key = "cand_mencionado",
         value = "num",
         -nombre, -nombre_corto) %>%  
  mutate(cand_mencionado = case_when(cand_mencionado == "menciones_amlo_tot" ~ "AMLO",
                                     cand_mencionado == "menciones_anaya_tot" ~ "Anaya",
                                     cand_mencionado == "menciones_bronco_tot" ~ "El Bronco",
                                     cand_mencionado == "menciones_meade_tot" ~ "Meade",
                                     cand_mencionado == "menciones_zavala_tot" ~ "Zavala")) %>% 
  filter(!is.na(cand_mencionado)) %>% 
  mutate(nombre_corto = ifelse(nombre_corto == "López Obrador", "AMLO", nombre_corto)) %>% 
  rename(weight = num) %>% 
  select(-nombre) %>% 
  filter(weight > 0)


enlaces_pd <- menciones_pd %>% 
  left_join(nodos_pd, by = c("nombre_corto" = "candidato")) %>% 
  rename(from = id) %>% 
  select(-menciones_tot)

enlaces_pd <- enlaces_pd %>% 
  left_join(nodos_pd, by = c("cand_mencionado" = "candidato")) %>% 
  rename(to = id) %>% 
  select(from, to, weight, everything())

### Guardar versiones en formato .csv 
write_csv(enlaces_pd, "04_datos_output/enlaces_pd.csv")  
write_csv(nodos_pd, "04_datos_output/nodos_pd.csv")

# Generar objeto redes con igraph
datos_red_pd <- graph_from_data_frame(d = enlaces_pd, vertices = nodos_pd, directed = TRUE)

# Cambiar tipo de letra
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))
par(family = 'avenir')

# Cambiar márgenes

png("03_graficas/menciones/primero/num_menciones_candidatos_red_pd.png", width = 9, height = 7, units = "in", res = 200)
par(mar = c(0, 0, 0, 0))
plot.igraph(datos_red_pd, 
            edge.arrow.size = enlaces_pd$weight/1.2, 
            edge.width = enlaces_pd$weight/3, 
            edge.curved=.3,
            edge.label = enlaces_pd$weight,
            edge.label.family = "Trebuchet MS Bold",
            edge.label.font = 2,
            edge.label.color=c("grey20"),
            layout = layout_in_circle, 
            vertex.size = nodos_pd$menciones_tot,
            vertex.color = c("#a62a2a", "steelblue", "black", "#f14b4b", "#00cdcd"), 
            vertex.label = nodos_pd$nom_etiqueta,
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

title(main = "NÚMERO DE VECES QUE ____ SE REFIRIÓ A ____ EN EL PRIMER DEBATE PRESIDENCIAL", adj = 0.2, line = -1.3, cex = 4.5)
text(-1.58, 1.25, labels = "La cifra entre paréntesis indica el total de veces que se mencionó el nombre de ese candidato ", adj = 0, cex = 1, col = "grey40")
text(-1.58, -1.3, labels = "Sebastián Garrido de Sierra / @segasi / oraculus.mx", adj = 0, cex = 1, col = "grey40")
dev.off()
par(mar=c(5.1,4.1,4.1,2.1))

