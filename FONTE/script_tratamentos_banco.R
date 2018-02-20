# DEFINIÇÕES DE BANCO E AMOSTRA:

##Carregando Banco de dados da população
  #dadosAaron = read.table("~/Documentos/Code/R/PROJETO ESTATISTICA-20180206T113737Z-001/PROJETO ESTATISTICA/DADOS BRUTOS/Banco_de_dados_Trabalho_tratado.txt", header = T, sep ="\t",  encoding="latin1")
  
##Gerando amostra com 60 elementos
  #amostra = dadosAaron[sample(nrow(dadosAaron), 60),]

##Escrevendo amostra em disco:
   #write.table(amostra, file="~/Documentos/Code/R/PROJETO ESTATISTICA-20180206T113737Z-001/PROJETO ESTATISTICA/FONTE/amostraAaron.txt", sep = "\t")

##Lendo amostra de disco:
  amostra = read.table("~/Documentos/Code/R/PROJETO ESTATISTICA-20180206T113737Z-001/PROJETO ESTATISTICA/FONTE/amostraAaron.txt", header = T, sep = "\t")

##Criando amostra tratada:
  amostraTratada = na.exclude(amostra)
  

