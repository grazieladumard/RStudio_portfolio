# Manipulando dados com dplyr
# 1. Importar os dados
library(tidyverse) # conjunto de pacotes dentro do tidyverse
library(readxl)
munic_tbl <- read_excel("dados/Atlas 2013_municipal, estadual e Brasil.xlsx", 
                        sheet = "MUN 91-00-10")
View(munic_tbl)

install.packages("summarytools") #quando for para instalar tem que colocar entre aspas.E aqui ele precisa procurar em uma lista, é uma palavra.
library(summarytools) #depois de instalado, ele passa ser um objeto na biblioteca e nao precisa colocar aspas.
# o library pega da biblioteca e traz da bibleoteca e coloca na memoria. É como pegarum livro e trazer pra ler.

view(dfSummary(munic_tbl))

# Criar uma variável com o município por 
# classe de tamanho de população

# classe de tamanho da população Total IBGE
# 
# Até 5.000 
# 5.001 a 20.000 
# 20.001 a 100.000 
# 100.001 a 500.000 
# Mais de 500.000

# Classificação ONU para IDH e IDHM
# Muito baixo: 0 a 04,99. 
# Baixo: 0,500 a 0,599. 
# Médio: 0,600 a 0,699. 
# Alto: 0,700 a 0,799. 
# Muito alto: 0,800 a 1.

# Taxa de Urbanização

#Script de tratamento dos dados

library(tidyverse)

munic_tbl |>
  select(ANO, UF, Codmun7, Município, IDHM, IDHM_E,IDHM_L, IDHM_R, POP, pesotot,
         pesourb, pesoRUR) %>%
  mutate(classe_pop = case_when(
    (POP < 5001) ~ "1) Até 5.000",
    (POP > 5000 & POP < 20001) ~ "2) 5.001 a 20.000",
    (POP > 20000 & POP < 100000) ~ "3) 20.001 a 100.000",
    (POP > 100000 & POP < 500000) ~ "4) 100.001 a 500.000",
    (POP > 500000) ~ "5) Mais de 500.000",
  ))|>
  
  mutate(classe_idhm = case_when(
    (IDHM < 0.5) ~ "Muito baixo",
    (IDHM >= 0.5 & IDHM < 0.6) ~ "Baixo",
    (IDHM >= 0.6 & IDHM < 0.7) ~ "Médio",
    (IDHM >= 0.7 & IDHM < 0.8) ~ "Alto",
    (IDHM >= 0.8) ~ "Muito Alto",
  ))|>

mutate(taxa_urbanizacao = (pesourb / pesotot) *100) |>
  mutate(urbano_rural = case_when(
    (taxa_urbanizacao >= 50) ~ "Urbano",
    (taxa_urbanizacao < 50) ~ "Rural")) -> m2_munic


#Agrupados por ano se tem diferença entre municipio rural e urbano a média do IDHM
m2_munic |>
  group_by(ANO,urbano_rural) |>
  summarise(media = mean(IDHM),total_de_municipios = n())

m2_munic |>
  group_by(ANO,classe_pop) |>
  summarise(media = mean(IDHM),total_de_municipios = n())

#IDHM Educação

m2_munic |>
  group_by(ANO,urbano_rural) |>
  summarise(media = mean(IDHM_E), total_de_municipios =n())

m2_munic |>
  group_by(ANO, classe_pop) |>
  summarise(media = mean (IDHM_E), total_de_municipios = n())


#IDHM Renda media
m2_munic |>
  group_by(ANO,urbano_rural) |>
  summarise(media = mean(IDHM_R), total_de_municipios =n())

m2_munic |>
  group_by(ANO,classe_pop) |>
  summarise(media = mean(IDHM_R), total_de_municipios =n())

#IDHM Longevidade
m2_munic |>
  group_by(ANO,urbano_rural) |>
  summarise(media = mean(IDHM_L), total_de_municipios =n())

m2_munic |>
  group_by(ANO,classe_pop) |>
  summarise(media = mean(IDHM_L), total_de_municipios =n())
