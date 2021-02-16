
colunas <- c("População dos 50 + Populosos", "Área Geográfica", "Educação", 
             "Patrimônio Cultural", "Receita Própria", "Cota Mínima", "Mineradores", 
             "Saúde per capita", "VAF", "Esportes", "Turismo", "Penitenciárias", 
             "Recursos Hídricos", "Produção de Alimentos", "Unidades de Conservação (IC i)", 
             "Saneamento", "Mata Seca", "Meio Ambiente", "População", "PSF", 
             "ICMS Solidário", "Índice Mínimo per capita", "Índice de participação")



idx_formatado <- idx %>% 
  select(Ano, `Mês`, IBGE2, all_of(colunas)) %>% 
  pivot_longer(
    cols = all_of(colunas),
    names_to = "Critério",
    values_to = "Valor"
  )


codigos_site <- read_csv2(
  "Idx/Cod_indices_site.csv",
  locale = locale(encoding = "Latin1")
  )


idx_formatado <- idx_formatado %>% 
  left_join(
    codigos_site,
    by = c("Critério" = "Variavel")
  ) %>% 
  select(Ano, `Mês`, IBGE2, `Cód índ`, Valor)


<<<<<<< HEAD:Cálculo dos índices de participação/idx_site.R
meses <-  1:12
names(meses) <- c("Janeiro", "Fevereiro", "Março", "Abril", 
                  "Maio", "Junho", "Julho", "Agosto", "Setembro",
                  "Outubro", "Novembro", "Dezembro")


idx_formatado$Mês <- unname(meses[idx_formatado$Mês])


colnames(idx_formatado) <- c("ano",	"mês", "Cod", "Cód índ", "Valor índ") 


write.csv2(idx_formatado, "Idx_para_site.csv", row.names = F)
=======


datas <- tribble(
  ~Mês, ~Número,
  "Janeiro", 1,
  "Fevereiro", 2,
  "Março", 3,
  "Abril", 4,
  "Maio", 5,
  "Junho", 6,
  "Julho", 7,
  "Agosto", 8,
  "Setembro", 9,
  "Outubro", 10,
  "Novembro", 11,
  "Dezembro", 12
)

mes_atual <- unique(idx$`Mês`)

numero_mes_atual <- datas$Número[datas$Mês == mes_atual]

idx_formatado$Mês <- numero_mes_atual




write.csv2(idx_formatado, "Idx/Idx_para_site.csv", row.names = F)



>>>>>>> 53f52cfeb9a1d9266fff6a3dd0f7d755974eb438:Idx/idx_site.R
