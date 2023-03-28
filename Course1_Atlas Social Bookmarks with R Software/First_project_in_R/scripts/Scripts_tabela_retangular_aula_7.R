curso <- c("Sociologia", "Administração", "Administração", "Ciências Sociais", "Educação",
           "Administração","Comunicação")
uf_origem <-c("Paraná", "São Paulo","Paraná","Mato Grosso",
              "Rio de Janeiro", "São Paulo", "Paraná")
uf_destino <-c("Paraná", "Paraná", "São Paulo", "Paraná",rep("São Paulo",3))

idade <-c(24,21,22,22,20,31,30)

tabela<-data.frame(curso, uf_origem, uf_destino, idade)

tabela[tabela$curso == "Administração", c("curso","idade")]

       