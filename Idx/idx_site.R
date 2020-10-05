
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
  select(Ano, `Mês`, IBGE2, `Cód índ`, Valor, `Critério`)




write.csv2(idx_formatado, "Idx/Idx_para_site.csv", row.names = F)



