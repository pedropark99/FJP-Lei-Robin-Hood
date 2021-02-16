#### PACOTES NECESSÁRIOS PARA O SERVIÇO -------------------------
## install.packages("readxl")
library(readxl)
library(tidyverse)



#### Defina nesta linha, qual o nome do arquivo que
#### contém os valores transferidos de ICMS.
arquivo_transferencias <- "Resultado_final.xlsx"



### IMPORTANDO OS DADOS --------------

Cod_indices_site <- read_csv2(
  "Cod_indices_site.csv", 
  locale = locale(encoding = "Latin1")
)

codigos_IBGE <- read_csv2("codigos.csv")

dados <- read_excel(arquivo_transferencias)


#### CONFERINDO OS NOMES DAS COLUNAS
## NA TABELA DE TRANSFERÊNCIAS.

source("conferir_transferencias.R", encoding = "UTF-8")


#### Selecionando as colunas dos critérios

colunas_de_identificacao <- c(
  "Ano",
  "Mês",
  "IBGE1",
  "IBGE2",
  "SEF",
  "Município"
)

colunas <- colunas_corretas[!colunas_corretas %in% colunas_de_identificacao]

tabela <- dados %>% 
  pivot_longer(
    cols = all_of(colunas),
    names_to = "Variavel",
    values_to = "Valor"
  ) %>% 
  left_join(
    codigos_IBGE,
    by = "IBGE2"
  )



tabela <- tabela %>% 
  left_join(
    Cod_indices_site,
    by = "Variavel"
  ) %>% 
  select(Ano, `Mês`, SEF, `Cód índ`, Valor, Variavel)



meses <-  1:12
names(meses) <- c("Janeiro", "Fevereiro", "Março", "Abril", 
                  "Maio", "Junho", "Julho", "Agosto", "Setembro",
                  "Outubro", "Novembro", "Dezembro")


tabela$Mês <- unname(meses[tabela$Mês])


criterios <- read_csv(
  "População,	Pop
População dos 50 + Populosos,	Pop 50+
Área Geográfica,	Área
Educação,	Educ
Patrimônio Cultural,	Patr
Receita Própria,	Rec Prop
Meio Ambiente,	Meio Ambiente
Produção de Alimentos,	Prod Alimentos
VAF,	VAF
Cota Mínima,	Cota Min
Mineradores,	Mun Miner
PSF,	Saúde PSF
Saúde per capita,	Saúde Hab
Índice de participação,	Subtotal
Compensações,	Compensação Financeira
Valor Líquido + Compensações,	Total
Esportes,	Esportes
ICMS Solidário,	ICMS Solidário
Índice Mínimo per capita,	Mínimo per capita
Penitenciárias,	Penitenciárias
Recursos Hídricos,	Recursos hídricos
Turismo,	Turismo
Unidades de Conservação (IC i),	UC
Saneamento,	Saneam
Mata Seca,	Mata Seca",
  col_names = c("script", "Índice")
)



tabela <- tabela %>% 
  left_join(
    criterios,
    by = c("Variavel" = "script")
  ) %>% 
  select(-`Variavel`)



#### EXPORTANDO OS DADOS ------------------

write.csv2(tabela, file = "Transf.csv", row.names = F)




