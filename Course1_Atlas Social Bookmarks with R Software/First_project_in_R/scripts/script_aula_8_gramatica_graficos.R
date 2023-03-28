#1. Carregar pacotes e os dados 

library(tidyverse)
library(readxl)
library(esquisse)

#2. Importar dados

registro_mun <- read_excel("C:/Users/Graziela/Downloads/Curso R para indicadores sociais _UEL/Exercícios/DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx", 
                              sheet = "MUNICÍPIO")
censo_mun <- read_excel("dados/Atlas 2013_municipal, estadual e Brasil.xlsx",
                        sheet = "MUN 91-00-10")

# 3. Selecionar e modificar variáveis em censo_mun
# Criar uma variável com o município por 
# classe de tamanho de população
# classe de tamanho da população Total IBGE
# 
# Até 5.000 
# 5.001 a 20.000 
# 20.001 a 100.000 
# 100.001 a 500.000 
# Mais de 500.000
#
# Classificação ONU para IDH e IDHM
# Muito baixo: 0 a 04,99. 
# Baixo: 0,500 a 0,599. 
# Médio: 0,600 a 0,699. 
# Alto: 0,700 a 0,799. 
# Muito alto: 0,800 a 1.
#
# Criar a variávei Taxa de Urbanização

censo_mun |>
  select(ANO, UF, Codmun7, Município, IDHM, IDHM_E, IDHM_L, IDHM_R, POP, pesotot, pesourb, pesoRUR) %>%
  mutate(classe_pop = case_when(
    (POP < 5001) ~ "1) Até 5.000",
    (POP > 5000 & POP < 20001) ~ "2) 5.001 a 20.000",
    (POP > 20000 & POP < 100000) ~ "3) 20.001 a 100.000",
    (POP > 100000 & POP < 500000) ~ "4) 100.001 a 500.000",
    (POP > 500000) ~ "5) Mais de 500.000",
  )) |>
  mutate(classe_idhm = case_when(
    (IDHM < 0.5) ~ "Muito baixo",
    (IDHM >= 0.5 & IDHM < 0.6) ~ "Baixo",
    (IDHM >= 0.6 & IDHM < 0.7) ~ "Médio",
    (IDHM >= 0.7 & IDHM < 0.8) ~ "Alto",
    (IDHM >= 0.8) ~ "Muito Alto",
  )) |>
  mutate(taxa_urbanizacao = (pesourb / pesotot) * 100) |>
  mutate(urbano_rural = case_when(
    (taxa_urbanizacao >= 50) ~ "Urbano",
    (taxa_urbanizacao < 50) ~ "Rural")) -> m1_censo_mun

# Selecionar as variáveis em registro_mun
m1_regist_mun <- registro_mun |>
  select(ANO, NOME, IBGE7, IDEB_AI, IDEB_AF)

# filtrar anos em m1_censo_mun e m1_regist_mun
m1_censo_mun |>
  filter(ANO == 2010) -> c2_2010
m1_regist_mun |>
  filter(ANO == 2013) -> r2_2013

# juntar tabelas pela chave do código do município de sete dígitos do IBGE
c2_2010 |>
  full_join(r2_2013, by = c("Codmun7"="IBGE7")) |>
  # o na.omit - elimina os casos omissos 
  na.omit() -> j1_censo_reg    # o j1_censo é a variavel final

#Executa o esquisse no navegador 
esquisse::esquisser(viewer = "browser")

