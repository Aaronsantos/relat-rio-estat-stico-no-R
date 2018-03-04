# ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO #

### INSIRA AQUI O CAMINHO DO DIRETORIO EM QUE SE ENCONTRA O PROJETO 
#         (O CAMINHO ATÉ O DIRETORIO QUE CONTÉM AS PASTAS "FONTE", "DOCUMENTOS", "DADOS BRUTOS", "ANÁLISES")

caminho_projeto = "C:\\Users\\Aaron\\Desktop\\rel_est"

# ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO # ATENÇÃO #




############Implementação de funções de uso genérico.
#install.packages('nortest')
library(nortest)
#install.packages('ggplot2')
#library(ggplot2)

#Função de moda:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Função Tira NA
tiraNa = function(lista){
  lista2 = c()
  for( n in lista){
    if (!is.na(n)){
      lista2 = c(lista2,n)
    }
    
  }
  return(lista2)
}

#Função CV:
cv<-function(x){coef<-sd(x)/mean(x)*100 
return(coef)}

##################################### DEFINIÇÕES DO BANCO E AMOSTRA############################################

##Carregando Banco de dados da população
dadosAaron = read.table(paste( caminho_projeto, "\\DADOS BRUTOS\\Banco_de_dados_Trabalho_tratado.txt", sep=""), header = T, sep ="\t",  encoding="latin1")

##Gerando amostra com 60 elementos
#amostra = dadosAaron[sample(nrow(dadosAaron), 60),]

##Escrevendo amostra em disco:
#write.table(amostra, file="~/Documentos/Code/R/PROJETO ESTATISTICA-20180206T113737Z-001/PROJETO ESTATISTICA/FONTE/amostraAaron.txt", sep = "\t")

##Lendo amostra de disco:
amostra = read.table(paste(caminho_projeto,"\\FONTE\\amostraAaron.txt", sep = ""), header = T, sep = "\t")


#######################################FUNÇÕES DA ETAPA 1#############################################################


#Funções de análise descritiva de variáveis qualitativas

geraAnaliseQuali = function(lista,nome,caminho)
{
  tabela = table(lista)
  
  caminhoCsv = paste(caminho,"\\",nome, ".csv",sep="")
  caminhoPng1 = paste(caminho,"\\",nome,"_pie.png",sep="")
  caminhoPng2 = paste(caminho,"\\",nome,"_bar.png",sep="")
  print("#")
  print(paste("Variável: ",nome))
  
  tryCatch({
    png(file=caminhoPng1, width = 400, height = 400 )
    pie(tabela)
    dev.off()
  }, error=function(cond){
    print("###ERRO AO EXPORTAR GRÁFICO PIZZA###")
    print(cond)
    print("#")
  })
  
  tryCatch({
    png(file=caminhoPng2,width = 400, height = 400)
    barplot(tabela)
    dev.off()
  }, error=function(cond){
    print("###ERRO AO EXPORTAR GRÁFICO DE BARRAS###")
    print(cond)
    print("#")
  })
  
  tabela["Total"] = sum(tabela)[1]
  tabela = t(tabela)
  
  tryCatch({
    write.csv(tabela,caminhoCsv, row.names = FALSE)
  }, error=function(cond){
    print("###ERRO AO EXPORTAR CSV###")
    print(cond)
    print("#")
  })
  
  print(tabela)
}


#Funções de análises descritivas Quantitativa

geraAnaliseQuant = function(lista,nome,caminho) {
  lista = tiraNa(lista)
  tabela = summary(lista)
  tabela["Mode"] = Mode(lista)
  tabela["S.d."] = sd(lista)
  tabela["var."] = var(lista)
  tabela["C.V"] = cv(lista)
  
  caminhoCsv = paste(caminho,"\\",nome,".csv",sep="")
  caminhoPng = paste(caminho,"\\",nome,"_hist.png",sep="")
  print("#")
  print(paste("Variável: ", nome))
  tryCatch(
    {
      
      write.csv(t(tabela),caminhoCsv, row.names = F)
      
    }, error=function(cond){
      print("###ERRO AO EXPORTAR CSV###")
      print(cond)
      print("#")
    }
  )
  tryCatch(
    {
      png(file=caminhoPng, width = 400, height = 400 )
      hist(tabela)
      dev.off()
    }, error=function(cond){
      print("###ERRO AO EXPORTAR GRÁFICO HISTOGRAMA###")
      print(cond)
      print("#")
    })
  
  
  print(tabela)
}


# Função que gera resultado da etapa 1

resultado_1 = function(caminho) {
  geraAnaliseQuali(amostra$NCidade, "V2_Ncidade", caminho)
  geraAnaliseQuali(amostra$Local, "V3_Local", caminho)
  geraAnaliseQuant(amostra$Idade, "V4_Idade", caminho)
  geraAnaliseQuali(amostra$Cidade, "V5_Cidade", caminho)
  geraAnaliseQuali(amostra$Tempo, "V6_Tempo", caminho)
  geraAnaliseQuali(amostra$Bairro, "V7_Bairro", caminho)
  geraAnaliseQuali(amostra$Cor, "V8_Cor", caminho)
  geraAnaliseQuali(amostra$ReligiÃ.o, "V9_Religião", caminho)
  geraAnaliseQuali(amostra$ReligiÃ.o_qual, "V10_Religião_qual", caminho)
  geraAnaliseQuali(amostra$Estudando, "V11_Estudando", caminho)
  geraAnaliseQuali(amostra$Estudo, "v12_Estudo", caminho)
  geraAnaliseQuant(amostra$Casa, "V13_Casa", caminho)
  geraAnaliseQuali(amostra$Qualeotipodehabitacao, "V14_QualTipoDeHabitação", caminho)
  geraAnaliseQuali(amostra$Habit_outro, "V15_Habit_outro", caminho)
  geraAnaliseQuali(amostra$Habit_alug, "V16_habit_alug", caminho)
  geraAnaliseQuali(amostra$Comodos, "V17_Comodos", caminho)
  geraAnaliseQuant(amostra$Trabalho, "V18_trabalho", caminho)
  geraAnaliseQuant(amostra$Recursos, "V19_Recursos", caminho)
  geraAnaliseQuant(amostra$Renda_familiar, "V20_Renda_familiar", caminho)
  geraAnaliseQuant(amostra$Renda_div_pessoa, "V21_Renda_div_Pessoa", caminho)
  geraAnaliseQuant(amostra$Renda_da_familia, "V22_Renda_da_familia", caminho)
  geraAnaliseQuali(amostra$Classe, "V23_Classe", caminho)
  geraAnaliseQuali(amostra$Civil, "V24_Civil", caminho)
  
}

  print("ANÁLISE DESCRITÍVAS E MEDIDAS DE POSIÇÃO DAS VARIÁVEIS:")
  resultado_1(paste(caminho_projeto, "\\Analises", sep = ""))

##########################################FUNÇÕES ETAPA 2 ####################################################

  analisaP = function(teste) {
    
    if(teste["p.value"] < 0.05){
      return( paste("Rejeitado no ",teste["method"]," com p-value de: ", teste["p.value"]) ) 
    }else {
      return( paste("Aceito no ",teste["method"]," com p-value de: ", teste["p.value"]) ) 
    }
  }  

  testesModelo = function(lista) {
  
    result = c( analisaP(shapiro.test(lista)), analisaP(ad.test(lista)), analisaP(lillie.test(lista)), analisaP(sf.test(lista)) )
    return(result)
  }
  
  print("TESTE DE NORMALIDADE DAS VARIÁVEIS:")
  print("Variável Idade:")
  testesModelo(amostra$Idade)
  print("Variável Recursos:")
  testesModelo(amostra$Recursos)
  print("Variável Casa:")
  testesModelo(amostra$Casa)
  print("Variável Renda_familiar:")
  testesModelo(amostra$Renda_familiar)
  print("Variável Renda_div_pessoa")
  testesModelo(amostra$Renda_div_pessoa)
  print("Variável Renda_da_familia")
  testesModelo(amostra$Renda_da_familia)
  

##########################################FUNÇÕES ETAPA 3 #######################################################
  
  teste_hip = function(h0,h1){
    teste = t.test(h0,h1)
    if(teste$p.value < 0.05){
      return(paste("P-value para h0: ",teste$p.value,". Hipotese rejeitada."))
    }else {
      return(paste("P-value para h0: ",teste$p.value,". Hipotese não rejeitada"))
    }
  }
  
  print("TESTES DE HIPOTESE DAS VARIÁVEIS E INTERVALOS DE CONFIANÇA:")
  
  print("Variável Idade:")
  print("Intervalo de confiança:")
  t.test(amostra$Idade)
  teste_hip(amostra$Idade, dadosAaron$Idade)
  
  print("Variável Recursos:")
  print("Intervalo de confiança:")
  t.test(amostra$Recursos)
  teste_hip(amostra$Recursos, dadosAaron$Recursos)
  teste_hip(amostra$Casa, dadosAaron$Casa)
  
  
  print("Variável Renda_familiar:")
  print("Intervalo de confiança:")
  t.test(amostra$Renda_familiar)
  teste_hip(amostra$Renda_familiar, dadosAaron$Renda_familiar)
  
  
  print("Variável Renda_da_familia:")
  print("Intervalo de confiança:")
  t.test(amostra$Renda_da_familia)
  teste_hip(amostra$Renda_da_familia, dadosAaron$Renda_da_familia)
  
  
  print("Variável Renda_div_pessoa:")
  print("Intervalo de confiança:")
  t.test(amostra$Renda_div_pessoa)
  teste_hip(amostra$Renda_div_pessoa, dadosAaron$Renda_div_pessoa)
  
  
##########################################FUNÇÕES ETAPA 4 ######################################################

  print("TESTE DE CORRELAÇÃO ENTRE NÚMERO DE PESSOAS E A RENDA FAMILIAR:")
  cor.test(amostra$Renda_div_pessoa, amostra$Renda_familiar)
  print("REGRESSÃO LINEAR ENTRE NÚMERO DE PESSOAS E A RENDA FAMILIAR:")
  regressaoLinear = lm(amostra$Renda_familiar ~ amostra$Renda_div_pessoa )
  summary(regressaoLinear)
  
  plot(amostra$Renda_div_pessoa,amostra$Renda_familiar)
  grid(regr)
  abline(regrecaoLinear)
  regr
  