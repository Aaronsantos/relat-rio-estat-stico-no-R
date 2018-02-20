#Funções de análises descritivas

geraAnaliseQuali = function(lista,nome,caminho)
{
  tabela = table(lista)
   
  caminhoCsv = paste(caminho,"/",nome, ".csv",sep="")
  caminhoPng1 = paste(caminho,"/",nome,"_pie.png",sep="")
  caminhoPng2 = paste(caminho,"/",nome,"_bar.png",sep="")
  
  
  png(file=caminhoPng1, width = 800, height = 800 )
  pie(tabela)
  dev.off()
  
  png(file=caminhoPng2,width = 800, height = 800)
  barplot(tabela)
  dev.off()
  
  tabela["Total"] = sum(tabela)[1]
  tabela = t(tabela)
  
  write.csv(tabela,caminhoCsv, row.names = FALSE)
  
  print(tabela)
}

