


colunas_corretas <- c("Mês", "Ano", "IBGE1", "IBGE2", "SEF", "Municípios", "População bruto", 
                      "População dos 50 + Populosos", "Área Geográfica", "Educação", 
                      "Patrimônio Cultural", "Receita Própria", "Mineradores", 
                      "Saúde per capita", "VAF", "Esportes", "Turismo", "Penitenciárias", 
                      "Recursos Hídricos", "Produção de Alimentos", "Unidades de Conservação (IC i)", 
                      "Saneamento", "Mata Seca", "Número Equipes de Saúde")



nomes_do_Idx <- colnames(idx)

teste <- all(colunas_corretas %in% nomes_do_Idx)

if(!teste){
  print(as.character(colunas_corretas[!colunas_corretas %in% nomes_do_Idx], "\n"))
  stop("A sua planilha base do Idx, não possui as colunas definidas acima. Confirme se a ortografia dos nomes de cada coluna estão corretas. Caso queira ver a lista completa com os nomes corretos das colunas, que o seu Idx deveria possuir, execute o seguinte comando:\n\nprint(colunas_corretas)")
}





##### CONFERINDO OS CÓDIGOS DO IBGE2 -------------------------------------

codigos_IBGE2 <- suppressMessages(read_csv2("Idx/codigos.csv")[["IBGE2"]])

teste <- all(idx$IBGE2 %in% codigos_IBGE2)

if(!teste){

  stop("A sua planilha base do Idx, na coluna de IBGE2, possui pelo menos um desses códigos IBGE2 que não estão inclusos nos códigos verdadeiros. O mais provável que tenha ocorrido, é que você tenha trocado sem querer o nome da coluna de IBGE2, pelo nome da coluna do IBGE1. Lembre-se que os códigos IBGE2 correspondem aos códigos de até 4 dígitos do IBGE (10-7220), já os códigos IBGE1, correspondem aos códigos IBGE de 6 dígitos (310010-317720). Ou seja, o mais provável é que tenha nomeado a coluna com os códigos de até 4 dígitos de IBGE1, e a coluna com os códigos de 6 dígitos de IBGE2. Agora, caso não seja este o problema, isso significa que um dos códigos dos municípios em seu Idx, está corrompido de alguma forma. Você possui a lista de códigos corretos no arquivo codigos.csv da pasta.")
}









###### CONFERINDO O NÚMERO DE LINHAS ---------------------------------------

numero_linhas <- nrow(idx)

if(!numero_linhas == 853){
  
  print(numero_linhas)
  stop("Sua planilha base do Idx, possue o número de linhas definido acima. Ou seja, a sua planilha base do Idx, não possue 853 linhas (número de municípios de Minas Gerais). É necessário que essa planilha, defina os índices de todos os municípios de Minas Gerais.")
}
