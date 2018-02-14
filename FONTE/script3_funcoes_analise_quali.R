#Funções de análises descritivas

geraAnaliseQuali = function(lista,nome,caminho)
{
  tabela = table(lista)
  
  caminhoCsv = paste(caminho,"/",nome,".csv",sep="")
  caminhoPng = paste(caminho,"/",nome,".png",sep="")
  
  write.csv(tabela,caminhoCsv, row.names = FALSE)
  
  png(file=caminhoPng, width = 800, height = 800 )
  pie(tabela)
  dev.off()

}

