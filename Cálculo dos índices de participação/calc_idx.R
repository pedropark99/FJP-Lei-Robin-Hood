##### PACOTES NECESSÁRIOS ---------------------------------------------------

library(tidyverse)
library(readxl)


##### IMPORTANDO OS DADOS E CONFERINDO A PLANILHA dados_Idx --------------------------

idx <- read_excel("dados_Idx.xlsx")

pesos_idx <- read_csv2("pesos.csv")

source("conferir_dados_idx.R", encoding = "UTF-8")









####### ADICIONANDO POSSÍVEIS COLUNAS CASO NECESSÁRIO ------------------------------

colunas <- colnames(idx)

colunas_geralmente_faltantes <- c("Meio Ambiente", "População", "Cota Mínima")

if(!all(colunas_geralmente_faltantes %in% colunas)){
  
  idx <- idx %>% 
    mutate(
      "Meio Ambiente" = 0.4545 * Saneamento + 0.4545 * `Unidades de Conservação (IC i)` + 0.091 * `Mata Seca`,
      "População" = `População bruto` * 100 / sum(`População bruto`, na.rm = T),
      "Cota Mínima" = (1/853) * 100
    )
  
}










####### CONFERINDO SE AS COLUNAS SOMAM 100 ------------------------------------

colunas_criterios <- c("População dos 50 + Populosos", "Área Geográfica", "Educação", 
                       "Patrimônio Cultural", "Receita Própria", "Cota Mínima", "Mineradores", 
                       "Saúde per capita", "VAF", "Esportes", "Turismo", "Penitenciárias", 
                       "Recursos Hídricos", "Produção de Alimentos", "Unidades de Conservação (IC i)", 
                       "Saneamento", "Mata Seca")


somatorios <- map_dbl(
  idx %>% select(all_of(colunas_criterios)),
  sum
)

if(!all(near(somatorios, 100))){
  
  print(somatorios[!near(somatorios, 100)])
  stop("Os índices dos critérios acima não somam 100%, por favor, reajuste essas colunas para que elas somem 100%.")
}










#### CALCULANDO OS CONSOLIDADOS --------------------------------------------

calc_consolidado <- function(x){
  
  criterios <- x
  
  pesos <- pesos_idx$Peso[pesos_idx$Critério %in% x]
  
  formula <- paste(
    "idx[[",
    "'",
    criterios,
    "']]",
    " * ",
    pesos,
    sep = "",
    collapse = " + "
  )
  
  expression <- parse(text = formula)
  
  resultado <- eval(expression) / 100
  
  return(resultado)
  
}



criterios_anuais <- pesos_idx$Critério[pesos_idx$Periodicidade == "Anual"]

criterios_semestrais <- pesos_idx$Critério[pesos_idx$Periodicidade == "Semestral"]

criterios_trimestrais <- pesos_idx$Critério[pesos_idx$Periodicidade == "Trimestral"]


consolidado_anual <- calc_consolidado(criterios_anuais)

consolidado_semestral <- calc_consolidado(criterios_semestrais)

consolidado_trimestral <- calc_consolidado(criterios_trimestrais)






###### CALCULANDO OS ÍNDICES DE PSF -------------------------------------------------

total_equipes_PSF <- sum(idx[["Número Equipes de Saúde"]], na.rm = T)

calc_PSF <- function(x){
  
  indice_por_equipe <- if_else(x == 0, 0, 100 / total_equipes_PSF)
  
  indice_atendimento <- x * indice_por_equipe
  
  sobra_total <- 100 - sum(indice_atendimento)
  
  sobras <- sobra_total * x / total_equipes_PSF
  
  primeiro_ajuste <- indice_atendimento + sobras
  
  sobra_total <- 100 - sum(primeiro_ajuste)
  
  sobras <- sobra_total * x / total_equipes_PSF
  
  segundo_ajuste <- primeiro_ajuste + sobras
  
  
  return(segundo_ajuste)
  
}

idx$PSF <- calc_PSF(idx[["Número Equipes de Saúde"]])









######### CALCULANDO OS CONSOLIDADOS DO ART. 1° ----------------------------------------

consolidado1_art1 <- consolidado_anual + consolidado_semestral + consolidado_trimestral + (idx$PSF / 100)

ICMS_per_capita <- consolidado1_art1 / idx[["População bruto"]]

media <- sum(consolidado1_art1) / sum(idx[["População bruto"]])

media_40_por_cento <- 1.4 * media

pop_selecionada <- tibble(
    "População bruto" = idx[["População bruto"]],
    ICMS_per_capita
  ) %>% 
  mutate(
    Pop_selecionada = case_when(
      ICMS_per_capita < media_40_por_cento ~ `População bruto`,
      ICMS_per_capita > media_40_por_cento & ICMS_per_capita < 6 * media & `População bruto` < 10188 ~ `População bruto`,
      ICMS_per_capita > media_40_por_cento & ICMS_per_capita < 2 * media & `População bruto` > 100000 ~ `População bruto`,
      TRUE ~ 0
    )
  )


pop_selecionada <- pop_selecionada[["Pop_selecionada"]]

pop_selecionada_total <- sum(pop_selecionada)

ICMS_Solidario <- pop_selecionada * 100 / pop_selecionada_total

consolidado2_art1 <- ((ICMS_Solidario * 4.14) / 100) + consolidado1_art1










###### CALCULANDO O MÍNIMO PER CAPITA --------------------------------------------

minimo_per_capita <- consolidado2_art1 / idx[["População bruto"]]

media <- (sum(consolidado2_art1) / sum(idx[["População bruto"]])) * 1/3

pop_selecionada <- tibble(
    "População bruto" = idx[["População bruto"]],
    minimo_per_capita
  ) %>% 
  mutate(
    Pop_selecionada = if_else(minimo_per_capita < media, `População bruto`, 0)
  )

pop_selecionada <- pop_selecionada[["Pop_selecionada"]]

ind_minimo_per_capita <- pop_selecionada / sum(pop_selecionada) * 100






######## ADICIONANDO ÚLTIMOS ÍNDICES E CALCULANDO O ÍNDICE DE PARTICIPAÇÃO FINAL ---------------------

ind_final <- round(((ind_minimo_per_capita * 0.1 / 100) + consolidado2_art1), 8)

ind_final <- ind_final + ((100 - sum(ind_final)) / 853)


idx <- idx %>% 
  mutate(
    "ICMS Solidário" = ICMS_Solidario,
    "Índice Mínimo per capita" = ind_minimo_per_capita,
    "Índice de participação" = ind_final
  )




##### EXPORTANDO OS RESULTADOS, INCLUINDO A PLANILHA PARA O SITE ---------------------

source("idx_site.R", encoding = "UTF-8")

write.csv2(idx, "Idx_resultado.csv", row.names = F)








