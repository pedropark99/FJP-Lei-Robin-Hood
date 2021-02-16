library(extrafont)
loadfonts()
library(ragg)
library(tidyverse)
library(geobr)



arquivo_com_dados <- "Transferências_completas_2019.csv"


transf <- read_csv2(
  arquivo_com_dados
)



mg_geom <- read_municipality(code_muni = "MG") %>%
  mutate(
    IBGE6 = as.integer(str_sub(code_muni, 1, 6))
  )



criterios <- c(
  "População", "População dos 50 + Populosos",
  "Área Geográfica", "Educação", "Patrimônio Cultural", "Receita Própria",
  "Cota Mínima", "Municípios Mineradores", "Saúde Per Capita", "VAF", "Esportes",
  "Turismo", "Municípios sede de Penitenciárias", "Recursos Hídricos", "Produção de Alimentos",
  "Unidades de Conservação (IC i)", "Saneamento", "Mata Seca",
  "Meio Ambiente", "PSF", "ICMS Solidário", "Índice Mínimo per capita",
  "Subtotal", "Total com compensação"
)


calc_quintis <- function(x) {
  corte <- cut_number(x, n = 5)
  return(corte)
}


montar_legendas <- function(x, quintis) {
  cortes <- tibble(
    value = x,
    quintil = quintis
  )
  
  cortes$igual_a_zero <- is.na(cortes$value)
  
  legendas <- cortes %>%
    group_by(quintil) %>%
    mutate(
      max = round(max(value) / 1000, 1),
      min = round(min(value) / 1000, 1)
    ) %>%
    mutate(
      max = format(max, decimal.mark = ",", big.mark = "."),
      min = format(min, decimal.mark = ",", big.mark = "."),
      rotulo = if_else(
        igual_a_zero,
        "Não se encaixa no critério",
        str_c("De ", min, " a ", max, sep = "")
      )
    )
  
  return(legendas[["rotulo"]])
}



filter_mes <- function(x, dados) {
  filtro <- dados %>%
    filter(`Mês` == x)
  
  return(filtro)
}


join_dados <- function(mapa, transf, criterio) {
  join <- inner_join(
    mapa,
    transf, 
    by = "IBGE6"
  )
  
  return(join)
}



dados_necessarios <- function(criterio, agregado = TRUE) {
  if (isFALSE(agregado)) {
    df_transf <- filter_mes(mes, transf)
  } else {
    df_transf <- transf %>%
      group_by(Ano, IBGE6) %>%
      summarise(across(all_of(criterio), sum, na.rm = TRUE))
    
    df_transf[df_transf == 0] <- NA_real_
  }
  
  df_transf$quintis <- calc_quintis(df_transf[[criterio]])
  
  df_transf$legendas <- montar_legendas(
    df_transf[[criterio]],
    df_transf$quintis
  )
  
  rotulos <- df_transf %>%
    distinct(quintis, legendas) %>%
    arrange(quintis)
  
  dados_necessarios <- join_dados(
    mg_geom,
    df_transf
  )
  
  return(
    list(
      dados = dados_necessarios,
      rotulos = rotulos$legendas
    )
  )
}




cores <- c(
  "#ffcccc",
  "#ff6666",
  "#fc5a23",
  "#ff0000",
  "#800000"
)

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

fonte_dados <- "Fonte: Diretoria de Estatística e Informações - DIREI/FJP."


make_plot <- function(criterio) {
  dados <- dados_necessarios(
    criterio = criterio
  )
  
  criterio_ou_total <- if(criterio == "Subtotal") {
    criterio_ou_total <- "Valores totais transferidos segundo a Lei Robin Hood, no ano de 2019 (Em R$1.000)"
  } else
  if(criterio == "Total com compensação") {
    criterio_ou_total <- "Valores totais transferidos segundo a Lei Robin Hood, incluindo as compensações financeiras realizadas, no ano de 2019 (Em R$1.000)"
  } else {
    criterio_ou_total <- str_c(
      "Valores transferidos segundo o critério de ",
      criterio,
      ", no ano de 2019 (Em R$1.000)"
    )
  }
    
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





