#Funções de análises descritivas Quantitativa

geraAnaliseQuant = function(lista,nome,caminho) {
  tabela = summary(lista)
  tabela["Mode"] = Mode(lista)
  
  caminhoCsv = paste(caminho,"/",nome,".csv",sep="")
  caminhoPng = paste(caminho,"/",nome,"_hist.png",sep="")
  
  write.csv(t(tabela),caminhoCsv, row.names = F)
  png(file=caminhoPng, width = 800, height = 800 )
  hist(tabela)
  dev.off()
  

  print(paste("Variável: ", nome))
  print(tabela)
}