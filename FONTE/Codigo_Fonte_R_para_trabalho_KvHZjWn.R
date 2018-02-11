# Atividade de hoje (01/02/2018) - Realizar a leitura de um
# banco de dados externo

#Objetivo da aula:
# 1. Retirar uma amostra de tamanho 60 para compor o banco de
# dados de cada um individualmente

dadosAaron <- read.table("D:\\Probabilidade\\fwdprobscripts\\Banco_de_dados_Trabalho.txt", header = T, sep ="\t")


#Banco de dados Ok!!!!

#Agora estamos prontos para tentar retirar nossa amostra
# de tamanho 60.

amostra <- dadosAaron[sample(nrow(dadosAaron), 60),]

#--Escrevendo amostra em disco:
write.table(amostra, file="D:\\Probabilidade\\fwdprobscripts\\amostraAaron.txt", sep = "\t")

#--CARREGANDO AMOSTRA EM DISCO:
amostra = read.table("~/Documentos/Code/R/PROJETO ESTATISTICA-20180206T113737Z-001/PROJETO ESTATISTICA/FONTE/amostraAaron.txt", header = T, sep = "\t", encoding="latin1")



# Pronto! Agora temos nosso banco de dados! Mãos a obra com as análises!


###################    PASSO 1 #####################

#Para o primeiro momento de an?lises, o interessante ? classificar cada um dos tipos de vari?veis que o banco tem

##Contando ocorrências de valores nas variáveis
i = 0



## FREQUENCIA DE OCORRENCIA DE CADA CIDADE, ONDE totalNcidade[i] == total de participantes que marcaram o i valor no questionário
freqNcidade = table(amostra[,2])

##FREQUENCIA DE OCORRENCIA DE CADA LOCAL DE APLICAÇÃO DO QUESTIONÁRIO
freqLocal = table(amostra[,3])

##Frequencia de local de nascença
freqCidade = table(amostra[,5])

##FREQUENCIA DE TEMPO EM QUE VIVE NA CIDADE
freqTempo = table(amostra[,6])

##FREQUENCIA DO BAIRRO
freqBairro = table(amostra[,7])

#Frequencia cor de pele
freqCor = table(amostra[,8])

#Frequencia Religião
freqReligião = table(amostra[,9])

#Frequencia Religião QUal
freqReligião_qual = table(amostra[,10])

#Frequencia Estudando
freqEstudando = table(amostra[,11])

#Frequencia Estudo
freqEstudo = table(amostra[,12])

#Frequencia Tipo de habitação
freqTipoHab = table(amostra[,14])

#Frequencia outra habitação
freqHabOutro = table(amostra[,15])

#frequencia habit aluga
freqHabitAlug = table(amostra[,16])

#frequencia trabalho
freqTrabalho = table(amostra[,17])

#

#Depois Construir Estat?sticas Descritivas para cada uma das vari?veis QUALITATIVAS em formato de tabela e gr?ficos

##GGPLOT --> PESQUISAR

#Para as vari?veis QUANTITATIVAS, estou interessado em informa??es das medidas-resumo (medidas de posi??o e medi
# das de dispers?o

####################   PASSO 2 ######################

#Para as vari?veis quantitativas (cont?nuas e discretas), verificar qual o modelo probabil?stico que melhor se
# ajusta para modelar as probabilidades envolvidas

#Para tal, consulte o material no SUAP para entender um pouco mais sobre a natureza desses testes e depois veja
# se d? aplica??o desses testes no R


# Conclua esse passo dizendo qual ? o melhor modelo probabil?stico para esse caso

#####################  PASSO 3 ####################

#Nesse passo, construa intervalos de confian?a e fa?a testes de hip?teses confrontando a m?dia, a vari?ncia
#e a propor??o das vari?veis cont?nuas e discretas estudadas no nosso banco.

####################  PASSO 4 #####################

#Nesse ponto, termine seu trabalho realizando uma an?lise de correla??o e regress?o entre o n?mero de pessoas e
# a renda familiar

################### PASSO EXTRA ##################

#Voc? tem chances de tirar uma pontua??o extra se conseguir concatenar tudo pedido no formato
#functions do R
