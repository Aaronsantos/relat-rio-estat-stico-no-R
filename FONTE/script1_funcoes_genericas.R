##IMPLEMENTAÇÃO DE FUNÇÕES DE USO GENÉRICO:


#Função de moda:
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }