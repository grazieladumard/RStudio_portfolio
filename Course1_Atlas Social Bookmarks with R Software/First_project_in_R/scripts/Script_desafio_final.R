# Desafio

#1. Carregar pacotes e os dados 

library(tidyverse)
library(readxl)
library(esquisse)

#2. Importar dados

registro_munic <- read_excel("C:/Users/Graziela/Downloads/Curso R para indicadores sociais _UEL/Exercícios/DOWNLOAD REGISTRO ADMINISTRATIVO TOTAL 2012 A 2017.xlsx", 
                          sheet = "MUNICÍPIO")
censo_munic <- read_excel("C:/Users/Graziela/Downloads/Curso R para indicadores sociais _UEL/Exercícios/Atlas 2013_municipal, estadual e Brasil.xlsx", 
                          sheet = "MUN 91-00-10")

# 3. Selecionar e modificar variáveis em censo_UF
# Criar uma variável com o Estado por 
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

censo_munic |>
  select(ANO, UF, Codmun7, IDHM, IDHM_E, IDHM_L, IDHM_R, POP, pesotot, pesourb, pesoRUR, E_ANOSESTUDO) %>%
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
    (taxa_urbanizacao < 50) ~ "Rural")) -> m1_censo_munic

# Selecionar as variáveis em registro_munic
m1_regist_munic <- registro_munic |>
  select(ANO, NOME, IBGE7, IDEB_AI, IDEB_AF)

# filtrar anos em m1_censo_munic e m1_regist_munic
m1_censo_munic |>
  filter(ANO == 2010) -> censo_munic2_2010
m1_regist_munic |>
  filter(ANO == 2013) -> registro_munic2_2013

# juntar tabelas pela chave do código dos Municipios de sete dígitos do IBGE
censo_munic2_2010 |>
  full_join(registro_munic2_2013, by = c("Codmun7"="IBGE7")) |>
  na.omit() -> juncao1_censo_reg    

#Executa o esquisse no navegador 
esquisse::esquisser(viewer = "browser")

#Grafico 1

ggplot(juncao1_censo_reg) +
  aes(x = IDHM, y = IDHM_R, colour = E_ANOSESTUDO) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_distiller(palette = "Paired", direction = 1) +
  labs(title = "Anos de Estudo e relação IDHM e IDHM_R") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(urbano_rural))
  
#Grafico 2

ggplot(juncao1_censo_reg) +
  aes(x = IDHM_R, y = E_ANOSESTUDO, colour = urbano_rural) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_manual(
    values = c(Rural = "#A6CEE3",
               Urbano = "#B15928")
  ) +
  labs(
    title = "Relação IDHM_R e Anos de Estudo",
    caption = "Fonte: Atlas Brasil e IBGE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )
