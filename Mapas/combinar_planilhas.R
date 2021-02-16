## Script para combinar e fortificar todas as planilhas mensais de transferências
## em um único arquivo.
library(fs)
library(readxl)
library(tidyverse)

pasta <- "C:/Users/Pedro/Desktop/Lei Robin Hood/Lei Robin Hood/Transferências - 18.030/Ano 2019/"

pasta <- iconv(pasta, from = "ISO-8859-1", to = "UTF-8")

caminhos <- dir_ls(
    path = pasta, 
    recurse = TRUE
  ) %>% 
  grep(
    "Cálculo (.*) ([0-9]{4}).xlsx",
    x = .,
    value = TRUE
  ) %>% 
  unname()


caminhos <- iconv(caminhos, "UTF-8", "ISO-8859-1")


dados <- map(
  caminhos,
  function(x){
    sheets <- excel_sheets(x)
    last_sheet <- length(sheets)
    dados <- read_excel(x, sheet = last_sheet, skip = 3)
    return(dados)
  } 
)


for(i in seq_along(dados)){
  primeira_linha_vazia <- all( is.na( dados[[i]] )[1, ] )
  
  if(primeira_linha_vazia){
    dados[[i]] <- dados[[i]][-1, ]
  }
}


nomes_colunas <- map(dados, colnames)

for(i in seq_along(nomes_colunas)){
   print(nomes_colunas[[i]] %in% nomes_colunas[[i + 1]])
}


colunas_corretas <- c("IBGE...1", "IBGE...2", "SEF", "Municípios", "População", "População dos 50 + Populosos", 
                      "Área Geográfica", "Educação", "Patrimônio Cultural", "Receita Própria", 
                      "Cota Mínima", "Mineradores", "Saúde Per Capita", "VAF", "Esportes", 
                      "Turismo", "Penitenciárias", "Recursos Hídricos", "Produção de Alimentos 2º semestre", 
                      "Unidades de Conservação (IC i)", "Saneamento", "Mata Seca", 
                      "Meio Ambiente", "PSF", "ICMS Solidário", "Índice Mínimo per capita", 
                      "Subtotal", "Compensação financeira", "Total com compensação")


for(i in seq_along(dados)){
  colunas_presentes <- colnames(dados[[i]])
  colunas_estao_presentes <- colunas_corretas %in% colunas_presentes
  
  if(!all(colunas_estao_presentes)){
    stop(
      paste(
        "O erro está na planilha", i,
        ", mais especificamente, na coluna ",
        colunas_presentes[!colunas_estao_presentes]
      )
    )
  }
  
  dados[[i]] <- dados[[i]][ , colunas_corretas]
}



meses <- c(
  "Abril",
  "Agosto",
  "Dezembro",
  "Fevereiro",
  "Janeiro",
  "Julho",
  "Junho",
  "Maio",
  "Março",
  "Novembro",
  "Outubro",
  "Setembro"
)


add_periodo <- function(x){
  df <- x %>% 
    mutate(
      "Ano" = 2019,
      "Mês" = mes_select
    )
  
  return(df)
}


for(i in seq_along(dados)){
  mes_select <- meses[i]
  dados[[i]] <- add_periodo(dados[[i]])
}


dados <- bind_rows(dados)
colnames(dados)[1:2] <- c("IBGE6", "IBGE2")




#### Exportando o resultado final -----------------------
###
write_csv2(
  dados,
  "Transferências_completas_2019.csv"
)
