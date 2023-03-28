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
