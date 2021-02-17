#### PACOTES NECESSÁRIOS ---------------------------
###
library(extrafont)
loadfonts()
library(ragg)
library(tidyverse)
library(geobr)


#### IMPORTANDO AS TRANSFERÊNCIAS POR CRITÉRIO --------------------------
###
arquivo_com_dados_completos <- "Transferencias_completas_2019.csv"


transf <- read_csv2(
  arquivo_com_dados_completos
)




#### BAIXANDO OS DADOS ESPACIAIS DO ESTADO DE MG -------------------------
###
mg_geom <- read_municipality(code_muni = "MG") %>%
  mutate(
    IBGE6 = as.integer(str_sub(code_muni, 1, 6))
  )





#### DEFININDO PARA QUAIS CRITÉRIOS UM MAPA SERÁ GERADO --------------------------
###
criterios <- c(
  "População", "População dos 50 + Populosos",
  "Área Geográfica", "Educação", "Patrimônio Cultural", "Receita Própria",
  "Cota Mínima", "Municípios Mineradores", "Saúde Per Capita", "VAF", "Esportes",
  "Turismo", "Municípios sede de Penitenciárias", "Recursos Hídricos", "Produção de Alimentos",
  "Unidades de Conservação (IC i)", "Saneamento", "Mata Seca",
  "Meio Ambiente", "PSF", "ICMS Solidário", "Índice Mínimo per capita",
  "Subtotal", "Total com compensação"
)




#### FUNÇÕES -----------------------------------------
###

## Essa função recebe um vetor numérico, e me
## retorna uma tabela contendo o próprio vetor numérico
## que a função recebeu, além do intervalo numérico que
## representa o quintil a que cada um dos valores
## presentes no vetor numérico, pertencem.

calc_quintis <- function(x) {
  corte <- cut_number(x, n = 5)
  tabela <- tibble(
    valor = x,
    quintil = corte
  )
  
  return(tabela)
}


## Para a legenda do mapa, é interessante montarmos rótulos 
## bem formatados, que descrevem o intervalo numérico
## que representa cada quintil. A função abaixo busca
## montar esses rótulos.

montar_legendas <- function(x) {
  legendas <- x %>%
    group_by(quintil) %>%
    summarise(
      min = round(min(valor) / 1000, 1),
      max = round(max(valor) / 1000, 1)
    ) %>%
    mutate(
      min = format(min, decimal.mark = ",", big.mark = ".", trim = TRUE),
      max = format(max, decimal.mark = ",", big.mark = ".", trim = TRUE)
    )
  
  legendas$igual_a_zero <- is.na(legendas$quintil)
  
  legendas <- legendas %>% 
    mutate(
      rotulo = if_else(
        igual_a_zero,
        "Não se encaixa no critério",
        str_c("De ", min, " a ", max, sep = "")
      )
    ) %>% 
    arrange(quintil)
  
  return(legendas[["rotulo"]])
}





## Com o objetivo de deixar o script mais claro,
## a função abaixo busca unir os dados de transferência
## selecionados à tabela que contém os dados espaciais
## a serem plotados no mapa.

join_dados <- function(mapa, transf, criterio) {
  join <- inner_join(
    mapa,
    transf, 
    by = "IBGE6"
  )
  
  return(join)
}




## A intenção desses mapas é mostrar os valores
## totais transferidos em um ano específico. Ou seja,
## os mapas buscam mostrar o valor acumulado, recebido pelos
## municípios ao longo de todo o ano. Com isso,
## a função abaixo, busca agregar os valores que no momento
## se encontram no formato mensal, para o formato anual.

agregado_anual <- function(x, criterio) {
  agreg <- x %>%
    group_by(Ano, IBGE6) %>%
    summarise(across(all_of(criterio), sum, na.rm = TRUE))
  
  agreg[agreg == 0] <- NA_real_
  
  return(agreg)
}





## Todas as funções acima tem um papel específico
## e coletam uma parte específica dos dados. Já a
## função abaixo será a responsável por aplicar
## cada uma das funções acima. Portanto, em conjunto,
## as funções acima geram todos os dados necessários
## para construírmos o mapa desejado.

dados_necessarios <- function(criterio) {
  
  df_transf <- agregado_anual(transf, criterio)
  
  quintis <- calc_quintis(df_transf[[criterio]])
  
  df_transf$quintis <- quintis[["quintil"]]
  
  rotulos <- montar_legendas(quintis)
  
  dados_necessarios <- join_dados(
    mg_geom,
    df_transf
  )
  
  return(
    list(
      dados = dados_necessarios,
      rotulos = rotulos
    )
  )
}






## As cores a serem aplicadas no mapa
## estam guardadas nesse vetor

cores <- c(
  "#ffcccc",
  "#ff6666",
  "#fc5a23",
  "#ff0000",
  "#800000"
)




## O objeto abaixo guarda certas configurações estéticas
## do mapa. Como as configurações que eliminam os eixos do mapa,
## e que defini as fontes a serem utilizadas no mapa, etc.

sem_eixos <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank(),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  text = element_text(family = "Roboto"),
  legend.text = element_text(size = 9),
  legend.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold"),
  plot.caption = element_text(hjust = 0)
)



## O texto a ser inserido no mapa, descrevendo a fonte dos dados:

fonte_dados <- "Fonte: Diretoria de Estatística e Informações - DIREI/FJP."



## A função abaixo, é a responsável por gerar o mapa em si.
## Ela primeiro, aplica a função dados_necessarios(), para
## coletar os dados a serem utilizados no mapa. Em seguida,
## a função busca definir qual o título a ser utilizado na
## legenda do mapa. Por último, a função vai se preocupar
## em construir o mapa, com as informações que ela coletou.

make_plot <- function(criterio) {
  dados <- dados_necessarios(
    criterio = criterio
  )
  
  criterio_ou_total <- if(criterio == "Subtotal") {
    
    criterio_ou_total <- "Valores totais transferidos segundo a Lei Robin Hood, no ano de 2019 (Em R$1.000)"
  
  } else if(criterio == "Total com compensação") {
    
    criterio_ou_total <- "Valores totais transferidos segundo a Lei Robin Hood, incluindo as compensações financeiras realizadas, no ano de 2019 (Em R$1.000)"
  
  } else {
    
    criterio_ou_total <- str_c(
      "Valores transferidos segundo o critério de ",
      criterio,
      ", no ano de 2019 (Em R$1.000)"
    )
  
  }
  
  
  ## Adicionando quebra de linhas no título da legenda:
  titulo_legenda <- str_wrap(
    criterio_ou_total,
    width = 25
  )
  
  
  plot <- dados$dados %>%
    ggplot() +
    geom_sf(
      aes(fill = quintis),
      color = "black",
      size = 0.13
    ) +
    scale_fill_manual(
      values = cores,
      labels = dados$rotulos
    ) +
    sem_eixos +
    labs(
      caption = fonte_dados,
      fill = titulo_legenda
    )
  
  print(plot)
}



for(i in criterios){
  nome_arquivo <- str_c("Resultados/", i, ".png")
  nome_arquivo <- iconv(nome_arquivo, "UTF-8", "ISO-8859-1")
  
  agg_png(nome_arquivo, width = 4700, height = 4200, res = 520)
  make_plot(i)
  dev.off()
}





