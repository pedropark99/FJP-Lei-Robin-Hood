
colunas <- c("População dos 50 + Populosos", "Área Geográfica", "Educação", 
             "Patrimônio Cultural", "Receita Própria", "Cota Mínima", "Mineradores", 
             "Saúde per capita", "VAF", "Esportes", "Turismo", "Penitenciárias", 
             "Recursos Hídricos", "Produção de Alimentos", "Unidades de Conservação (IC i)", 
             "Saneamento", "Mata Seca", "Meio Ambiente", "População", "PSF", 
             "ICMS Solidário", "Índice Mínimo per capita", "Índice de participação")



idx_formatado <- idx %>% 
  pivot_longer(
    cols = all_of(colunas),
    names_to = "Critério",
    values_to = "Valor"
  ) %>% 
  select(Ano, `Mês`, IBGE2, Critério, Valor)


codigos_site <- read_csv2(
  "Cod_indices_site.csv",
  locale = locale(encoding = "Latin1")
  )


idx_formatado <- idx_formatado %>% 
  left_join(
    codigos_site,
    by = c("Critério" = "Variavel")
  ) %>% 
  select(Ano, `Mês`, IBGE2, `Cód índ`, Valor)


meses <-  1:12
names(meses) <- c("Janeiro", "Fevereiro", "Março", "Abril", 
                  "Maio", "Junho", "Julho", "Agosto", "Setembro",
                  "Outubro", "Novembro", "Dezembro")


idx_formatado$Mês <- unname(meses[idx_formatado$Mês])


colnames(idx_formatado) <- c("ano",	"mês", "Cod", "Cód índ", "Valor índ") 


write.csv2(idx_formatado, "Idx_para_site.csv", row.names = F)
