## instalando bibliotecas necessárias ##
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(purrr)

## baixando dados do sinasc ##
SINASC_2014 = read_delim("dados_sinasc/SINASC_2014.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2015 = read_delim("dados_sinasc/SINASC_2015.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2016 = read_delim("dados_sinasc/SINASC_2016.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2017 = read_delim("dados_sinasc/SINASC_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2018 = read_delim("dados_sinasc/SINASC_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2019 = read_delim("dados_sinasc/SINASC_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2020 = read_delim("dados_sinasc/SINASC_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2021 = read_delim("dados_sinasc/SINASC_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2022 = read_delim("dados_sinasc/SINASC_2022.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2023 = read_delim("dados_sinasc/SINASC_2023.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

SINASC_2024 = read_delim("dados_sinasc/SINASC_2024.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

## separando dados do RS: ##
SINASC_2014 = SINASC_2014 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2015 = SINASC_2015 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2016 = SINASC_2016 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2017 = SINASC_2017 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2018 = SINASC_2018 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2019 = SINASC_2019 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2020 = SINASC_2020 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2021 = SINASC_2021 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2022 = SINASC_2022 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2023 = SINASC_2023 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

SINASC_2024 = SINASC_2024 %>%
  filter(substr(CODMUNRES, 1, 2) == "43")

## Salvando total de nascimentos por ano para calcular as prevalências das anomalias ##
#2014:
SINASC_2014$contador = 1

nascimentos14 = SINASC_2014 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos14$ANONASC = 2014

#2015:
SINASC_2015$contador = 1

nascimentos15 = SINASC_2015 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos15$ANONASC = 2015

#2016:
SINASC_2016$contador = 1

nascimentos16 = SINASC_2016 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos16$ANONASC = 2016

#2017:
SINASC_2017$contador = 1

nascimentos17 = SINASC_2017 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos17$ANONASC = 2017

#2018:
SINASC_2018$contador = 1

nascimentos18 = SINASC_2018 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos18$ANONASC = 2018

#2019:
SINASC_2019$contador = 1

nascimentos19 = SINASC_2019 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos19$ANONASC = 2019

#2020:
SINASC_2020$contador = 1

nascimentos20 = SINASC_2020 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos20$ANONASC = 2020

#2021:
SINASC_2021$contador = 1

nascimentos21 = SINASC_2021 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos21$ANONASC = 2021


#2022:
SINASC_2022$contador = 1

nascimentos22 = SINASC_2022 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos22$ANONASC = 2022

#2023:
SINASC_2023$contador = 1

nascimentos23 = SINASC_2023 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos23$ANONASC = 2023

#2024:
SINASC_2024$contador = 1

nascimentos24 = SINASC_2024 %>%
  group_by(CODMUNRES) %>%
  summarise(n_nasc_vivos = sum(contador))

nascimentos24$ANONASC = 2024


#juntando todos bancos:
banco_nascimentos = bind_rows(nascimentos14, nascimentos15, nascimentos16, nascimentos17,
                              nascimentos18, nascimentos19, nascimentos20, nascimentos21,
                              nascimentos22, nascimentos23, nascimentos24)


## filtrando anomalias selecionadas ##
# 1) selecionando todas as anomalias:
# 2014:
anomalias14 = SINASC_2014[grepl("Q", SINASC_2014$CODANOMAL), ]

anomalias14$ANONASC = 2014
anomalias14 = anomalias14 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2015:
anomalias15 = SINASC_2015[grepl("Q", SINASC_2015$CODANOMAL), ]

anomalias15$ANONASC = 2015
anomalias15 = anomalias15 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2016:
anomalias16 = SINASC_2016[grepl("Q", SINASC_2016$CODANOMAL), ]

anomalias16$ANONASC = 2016
anomalias16 = anomalias16 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2017:
anomalias17 = SINASC_2017[grepl("Q", SINASC_2017$CODANOMAL), ]

anomalias17$ANONASC = 2017
anomalias17 = anomalias17 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2018:
anomalias18 = SINASC_2018[grepl("Q", SINASC_2018$CODANOMAL), ]

anomalias18$ANONASC = 2018
anomalias18 = anomalias18 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2019:
anomalias19 = SINASC_2019[grepl("Q", SINASC_2019$CODANOMAL), ]

anomalias19$ANONASC = 2019
anomalias19 = anomalias19 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2020:
anomalias20 = SINASC_2020[grepl("Q", SINASC_2020$CODANOMAL), ]

anomalias20$ANONASC = 2020
anomalias20 = anomalias20 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2021:
anomalias21 = SINASC_2021[grepl("Q", SINASC_2021$CODANOMAL), ]

anomalias21$ANONASC = 2021
anomalias21 = anomalias21%>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2022:
anomalias22 = SINASC_2022[grepl("Q", SINASC_2022$CODANOMAL), ]

anomalias22$ANONASC = 2022
anomalias22 = anomalias22 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2023:
anomalias23 = SINASC_2023[grepl("Q", SINASC_2023$CODANOMAL), ]

anomalias23$ANONASC = 2023
anomalias23 = anomalias23 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2024:
anomalias24 = SINASC_2024[grepl("Q", SINASC_2024$CODANOMAL), ]

anomalias24$ANONASC = 2024
anomalias24 = anomalias24 %>%
  select(CODMUNRES, ANONASC, CODANOMAL)

# 2) Juntando todos os bancos:
banco_anomalias = bind_rows(anomalias14, anomalias15, anomalias16, anomalias17,
                            anomalias18, anomalias19, anomalias20, anomalias21,
                            anomalias22, anomalias23, anomalias24)

# 3) Separando as anomalias separadas por categorias
categorias <- tribble(
  ~regex_match,     ~id_cid,
  "^Q2[0-8][0-9]$",       1,     
  "^Q66[0-9]|Q69[0-9]|Q71[0-9]|Q72[0-9]|Q73[0-9]|Q743$", 2,
  "^Q000|Q001|Q002|Q01[0-9]|Q05[0-9]$", 3, 
  "^Q35[0-9]|Q36[0-9]|Q37[0-9]$", 4,
  "^Q02$",           5,
  ".*",                 6,
)


# 4) função para categorizar cada linha com as categorias acima:
classificar_codigos = function(codigos) {
  
  split_codigos = str_extract_all(codigos, "Q\\d+")
  
  map(split_codigos, function(cods) {
    
    ids = map_int(cods, function(cod) {
      match = categorias %>% 
        filter(str_detect(cod, regex_match)) %>%
        slice(1) %>%
        pull(id_cid)
      return(match)
    })
    unique(ids)
  })
}

# 5) aplicando função no banco:
df_resultado = banco_anomalias %>%
  mutate(id_codigos = classificar_codigos(CODANOMAL)) %>%
  mutate(id_codigos = map_chr(id_codigos, ~ paste(sort(.x), collapse = ",")))

# 6) tirando as anomalias que não serão utilizadas:
df_resultado = df_resultado %>%
  filter(id_codigos != 6)

# 7) criando colunas para quantidade de cada anomalia:
df_resultado = df_resultado %>%
  mutate(
    id_codigos_lista = str_split(id_codigos, ",") %>% map(as.integer),
    Cardiopatias = map_int(id_codigos_lista, ~ as.integer(1 %in% .x)),
    Membros = map_int(id_codigos_lista, ~ as.integer(2 %in% .x)),
    Neural = map_int(id_codigos_lista, ~ as.integer(3 %in% .x)),
    Fendas = map_int(id_codigos_lista, ~ as.integer(4 %in% .x)),
    Microcefalia = map_int(id_codigos_lista, ~ as.integer(5 %in% .x))
  ) %>%
  select(-id_codigos_lista)  

# 8) agrupando dados por município e por ano:
dados_finais_anomalias = df_resultado %>%
  group_by(CODMUNRES, ANONASC) %>%
  summarise(
    Cardiopatias = sum(Cardiopatias),
    Membros = sum(Membros),
    Neural = sum(Neural),
    Fendas = sum(Fendas),
    Microcefalia = sum(Microcefalia), .groups = "drop")

## juntando banco de nascimentos com de anomalias ##
banco_final = left_join(banco_nascimentos,
                        dados_finais_anomalias,
                        by = c("CODMUNRES", "ANONASC"))

banco_final[is.na(banco_final)] = 0

banco_final$CODMUNRES = as.character(banco_final$CODMUNRES)

## baixando dados municípios ##
municipios = read_csv("municipios.csv")

## selecionando apenas municípios do RS ##
municipios = municipios %>%
  filter(substr(codigo_ibge, 1, 2) == "43")

municipios = municipios %>%
  mutate(CODMUNRES = substr(codigo_ibge, 1, 6))

municipios = municipios %>%
  select(CODMUNRES, nome, latitude, longitude)

## juntando banco dos municípios com banco final ##
banco_final = left_join(banco_final,
                        municipios,
                        by = "CODMUNRES")

banco_final = banco_final %>%
  filter(!is.na(nome))

## salvando banco final para passar para o Power BI ##
write.csv(banco_final, "banco_final.csv", row.names = FALSE)
