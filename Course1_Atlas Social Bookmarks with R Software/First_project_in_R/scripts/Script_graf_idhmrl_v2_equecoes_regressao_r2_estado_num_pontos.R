# 4 fases necessárias do fluxo de trabalho.
# 1. Carregar pacotes necessários
library(tidyverse)
library(esquisse)

# 2. Importar dados
# Para importar os dados ir em: 
# Environment > import Dataset > formato escolhido (exemplo excel)
# Abrirá uma janela: Browse > escolhe a pasta > e o sheet (planilha)

library(readxl)
Atlas_2013_municipal_estadual_e_Brasil <- read_excel("dados/Atlas 2013_municipal, estadual e Brasil.xlsx", 
                                                     sheet = "UF 91-00-10")
View(Atlas_2013_municipal_estadual_e_Brasil)

# 3. Gerar Gráficos
# utilizar o Esquisse ir em > Addins > ggplot2 > montar o gráfico na aba do ggplot2
# na montagem do gráfico, seguir a ordem de colocação das variáveis, no encaixe. 
# quando houver ano, ele vem por ultimo no encaixe.


library(ggplot2)

ggplot(Atlas_2013_municipal_estadual_e_Brasil) +
 aes(x = IDHM_R, y = IDHM_L, colour = IDHM, size = pesotot) + #o + significa que há mais linhas
 geom_point(shape = "circle") +
 scale_color_gradient(low = "#132B43", high = "#2AFF00") +
 labs(title = " IDHM_Renda_Long_1999_2000_2010", 
 caption = "Fonte: Atlas Brasil, 2013.") +
 theme_minimal() +
 theme(plot.title = element_text(face = "bold", 
 hjust = 0.5), plot.caption = element_text(size = 10L, hjust = 0.5)) +
 facet_wrap(vars(ANO))

# 4. Disseminar ou publicar o material analisado, exemplo usar github. 


### Início para adicionar regressão R2_estados e numeros 
# vamos instalar os pacotes necessários

install.packages("ggrepel")
install.packages("ggpmisc")
install.pcakages("scales")
# o ggplot2 já tinha sido instalado

# carrega os pacotes da biblioteca
# ggplot2 faz os gráficos
library(ggplot2)

# O pacote ggpmisc acrescenta ao pacote ggplot2 uma miscelânia de novas funções.
# Vamos usar as funções stat_poly_line(), stat_poly_eq() para incluir a reta de regressão, equação e R²
library(ggpmisc)

# O pacote ggrepel vamos usar a função geom_text_repel() para incluir a variável UFN,
# que contém os nomes dos estados, como rótulo para os pontos no gráfico.
# Essa função além de incluir os nomes, procura ajustar a distância para
# evitar sopreposições, repelindo os textos em torno de cada círculo no gráfico.
library(ggrepel)

# O pacote scales faz as alterações no formato das legendas, escalas e eixos
# com a função format_format()
library(scales)

#Executar o código ggplot2 com as mudanças
ggplot(Atlas_2013_municipal_estadual_e_Brasil) +
  aes(x = IDHM_R, y = IDHM_L, colour = IDHM, size = pesotot) +
  # inclui uma reta de regressão com intervalos de confiança    
  stat_poly_line() +  
  # inlcui a equação e R², posicionados na altura do eixo x = 0.5 e y = 0.002, para não ficar em cima dos pontos do gráfico
  stat_poly_eq((use_label(c("eq", "R2"))), label.x = 0.5, label.y = 0.02) +
  geom_point(shape = "circle") +
  # inclui a formatação de casas decimais nas legendas de Cor/colour e Tamanho/size
  scale_color_gradient(low = "#FF00CA", high = "#0AD53E", labels = format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_size(labels = format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(color = "IDHM",
       size = "População") +
  theme_minimal() +
  facet_wrap(vars(ANO)) +
  # acrescenta as unidades da federação como rótulo de ponto de dados.
  geom_text_repel(aes(label = UFN), size = 3, max.overlaps = Inf)

### fim