# pacotes -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(DBI)

# conexao com banco -------------------------------------------------------

conexao <- dbConnect(
  RSQLite::SQLite(),
  "data-raw/base_assinaturas.sqlite"
)
# essa conexão poderia ser com qualquer banco!
# desde que eu tivesse um usuario, senha, acesso a um endpoint do banco etc

tabela <- dbGetQuery(conexao, "SELECT * FROM dim_assinatura")
# eu poderia pegar dados pra analisar direto de queries como essas ^

# ou eu poderia escrever codigo em R que é traduzido para SQL on the fly:
# ORM

dim_assinatura <- tbl(conexao, "dim_assinatura") |>
  mutate(
    #data_entrada = as.Date(data_entrada),
    tipo_assinatura = ifelse(year == 1, "anual", "mensal")
  ) |>
  select(id_cliente, id_assinatura, tipo_assinatura, data_entrada) |>
  collect() |>
  mutate(
    data_entrada = as.Date(data_entrada, "1970-01-01")
  )
# isso aqui é uma "blueprint", um rascunho de execução


dim_assinatura |> collect()
# a execução que vai acontecer no dia em que eu der "collect"
# com esse show_query ^
# isso aqui ^ é equivalente a:

# pegar o conteudo de dim_assinatura |> show_query()
# SELECT
# `dim_assinatura`.*,
# CASE WHEN (`year` = 1.0) THEN 'anual' WHEN NOT (`year` = 1.0) THEN 'mensal' END AS `tipo_assinatura`
# FROM `dim_assinatura`
# rodar:
# dbGetQuery(conexao, "SELECT
#   `dim_assinatura`.*,
#   CASE WHEN (`year` = 1.0) THEN 'anual' WHEN NOT (`year` = 1.0) THEN 'mensal' END AS `tipo_assinatura`
# FROM `dim_assinatura`")

# primeiras visualizacoes basicas -----------------------------------------

fato_pag_aprovado <- tbl(conexao, "fato_pag_aprovado") |>
  collect() |>
  mutate(
    data_referencia = as.Date(data_referencia, "1970-01-01"),
    valor_pago = valor_pago/1000
  )
# digamos que a gente tenha ido verificar se a origem é essa mesmo
# com a TI e com a área responsável pelo dado e é mesmo...

# dentro do problema 1: a receita está caindo...
# QUANTO está caindo?

# grafico de receita ao longo do tempo

fato_pag_aprovado |>
  group_by(data_referencia) |>
  summarise(
    valor_total = sum(valor_pago)
  ) |>
  ggplot(aes(x = data_referencia, y = valor_total)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    labels = scales::dollar_format(
      scale = 10^(-6),
      accuracy = 0.1,
      prefix = "R$ ",
      suffix = "MM",
      big.mark = ".",
      decimal.mark = ",")
  )

fato_pag_aprovado |>
  group_by(data_referencia) |>
  summarise(
    valor_total = sum(valor_pago)
  ) |>
  mutate(
    diferenca_valor = c(0, diff(valor_total))
  ) |>
  ggplot(aes(x = data_referencia, y = diferenca_valor)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    labels = scales::dollar_format(
      scale = 10^(-6),
      accuracy = 0.1,
      prefix = "R$ ",
      suffix = "MM",
      big.mark = ".",
      decimal.mark = ",")
  )

fato_pag_aprovado |>
  group_by(data_referencia) |>
  summarise(
    total_assinantes = n(),
    valor_total = sum(valor_pago)
  ) |>
  ggplot(aes(x = data_referencia, y = total_assinantes)) +
  geom_point() +
  geom_line()

fato_pag_aprovado |>
  group_by(data_referencia) |>
  summarise(
    total_assinantes = n(),
    valor_total = sum(valor_pago)
  ) |>
  mutate(
    diferenca_assinantes = c(NA, diff(total_assinantes))
  ) |>
  ggplot(aes(x = data_referencia, y = diferenca_assinantes)) +
  geom_point() +
  geom_line()

fato_pag_aprovado |>
  group_by(data_referencia) |>
  summarise(
    total_assinantes = n(),
    valor_total = sum(valor_pago),
    valor_medio = valor_total/total_assinantes
  ) |>
  mutate(
    diferenca_assinantes = c(NA, diff(total_assinantes))
  ) |>
  ggplot(aes(x = data_referencia, y = valor_medio)) +
  geom_point() +
  geom_line()

fato_pag_aprovado |>
  left_join(dim_assinatura) |>
  group_by(data_referencia, tipo_assinatura) |>
  summarise(
    valor_total = sum(valor_pago)
  ) |>
  ggplot(aes(x = data_referencia, y = valor_total, color = tipo_assinatura)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(
    labels = scales::dollar_format(
      scale = 10^(-6),
      accuracy = 0.1,
      prefix = "R$ ",
      suffix = "MM",
      big.mark = ".",
      decimal.mark = ",")
  )

fato_pag_aprovado |>
  left_join(dim_assinatura) |>
  group_by(
    data_referencia,
    tipo_assinatura) |>
  summarise(
    total_assinaturas = n_distinct(id_assinatura),
    valor_total = sum(valor_pago),
    valor_medio = valor_total/total_assinaturas
  ) |>
  ggplot(
    aes(
      x = data_referencia,
      y = valor_medio,
      color = tipo_assinatura)) +
  geom_point() +
  geom_line() +
  facet_wrap(~tipo_assinatura, scales = 'free')

# entradas e saidas -------------------------------------------------------

# missão: construir um gráfico de entradas x saídas na base

entradas <- dim_assinatura |>
  #filter(tipo_assinatura == "mensal") |>
  group_by(data_referencia = data_entrada,
           #tipo_assinatura
           ) |>
  summarise(
    assinaturas_novas = n()
  )

entradas |>
  ggplot(
    aes(
      x = data_referencia,
      y = assinaturas_novas,
      #col = tipo_assinatura
      )) +
  geom_point() +
  geom_line()

ativos <- fato_pag_aprovado |>
  left_join(dim_assinatura) |>
  #filter(tipo_assinatura == "mensal") |>
  group_by(
    data_referencia,
    #tipo_assinatura
    ) |>
  summarise(
    total_pagamentos = n_distinct(id_assinatura),
    valor_total = sum(valor_pago),
    valor_medio = valor_total/total_pagamentos
  )

estimativa_inicial_churn <- ativos |>
  left_join(entradas) |>
  select(data_referencia, total_pagamentos, assinaturas_novas) |>
  mutate(
    nao_pagaram_novamente = lag(total_pagamentos) - (total_pagamentos-assinaturas_novas)
  )

estimativa_inicial_churn |>
  ggplot(aes(x = data_referencia, y = nao_pagaram_novamente)) +
  geom_point() +
  geom_line()

# construindo a variavel de churn -----------------------------------------

# temos entendimento o suficiente pra entender que a base possivelmente
# está maturando a ponto de ter mais contratos saindo do que tinha antes
# é importante saber o porque pra conseguir dar uma resposta pra
# área de negócio e aumentar a receita
# por que tem um mesmo cliente com duas assinaturas?
# PERGUNTAR

# queremos criar uma variável na dim_assinatura que seja uma dentre as opcoes:
# 1. data em que achamos que o cliente saiu da base (parou de pagar)
# 2. NA caso possamos achar que o cliente continua na base
# vamos chamar isso de data_churn

# com isso vamor criar uma variável "churn"
# se data_churn for NA, é 0
# se não for é 1

# uma estratégia é a seguinte:
# vou pegar o dim_assinatura, verificar todas as datas
# em que um pagamento é esperado.

# a partir dessas datas, vou ver quantos pagamentos efetivamente aconteceram
# e em função de "tolerância" a não pagamentos,
# vou definir quem teve churn ou não

# PRIMEIRO: para cada assinatura vamos construir as "datas de pagamento"

# exemplo
# pessoa assinou em 2023-08-01

as.Date('2023-08-01') %m+% months(0:7)

dim_assinatura$data_de_pagamento <- vector("list", 11237L)

for(ii in 1:nrow(dim_assinatura)){
  print(ii)
  if(dim_assinatura$tipo_assinatura[ii] == "mensal"){

    meses_de_pagamento <- dim_assinatura$data_entrada[ii] %m+% months(0:30)
    meses_de_pagamento <- meses_de_pagamento[meses_de_pagamento <= as.Date("2024-03-01")]

    dim_assinatura$data_de_pagamento[[ii]] <-  meses_de_pagamento
  } else {
    meses_de_pagamento <- dim_assinatura$data_entrada[ii] %m+% years(0:3)
    meses_de_pagamento <- meses_de_pagamento[meses_de_pagamento <= as.Date("2024-03-01")]

    dim_assinatura$data_de_pagamento[[ii]] <-  meses_de_pagamento
  }
}

# SEGUNDO vamos efetivamente ver quando um pagamento aconteceu corretamente

registro_pagamentos_esperados <- dim_assinatura |>
  unnest(data_de_pagamento) |>
  left_join(
    fato_pag_aprovado, by = c("data_de_pagamento" = "data_referencia", "id_assinatura")
  ) |>
  group_by(id_assinatura) |>
  mutate(
    periodos_inadimplentes = sum(is.na(pagamentoAprovado)),
    periodos_adimplentes = sum(!is.na(pagamentoAprovado))
  )

# TERCEIRO vamos construir nossas variaveis

tabela_churn <- registro_pagamentos_esperados |>
  group_by(id_assinatura) |>
  summarise(
    data_primeira_inad = case_when(
      any(is.na(pagamentoAprovado)) ~ min(data_de_pagamento[is.na(pagamentoAprovado)]),
      TRUE ~ NA
    ),
    data_ultima_inad = case_when(
      any(is.na(pagamentoAprovado)) ~ max(data_de_pagamento[is.na(pagamentoAprovado)]),
      TRUE ~ NA
    )
  ) |>
  ungroup() |>
  left_join(dim_assinatura) |>
  mutate(
    data_ultima_inad = case_when(
      tipo_assinatura == "anual" & !is.na(data_ultima_inad) ~ as.Date("2024-03-01"),
      TRUE ~ data_ultima_inad),
    # nos casos anuais a data de ultima inadimplencia deve ser 2024-03 porque
    # nao encontramos cura na analise feita em 29/04
    churn = case_when(
      (data_ultima_inad-data_primeira_inad) >= 30 ~ 1,
      TRUE ~ 0
    )
  )

tabela_churn |>
  count(churn) |>
  mutate(
    p = n/sum(n)
  )

tabela_churn |>
  count(data_primeira_inad) |>
  drop_na() |>
  ggplot(aes(x = data_primeira_inad, y = n)) +
  geom_point() +
  geom_line()

## modelo para prever o churn

tbl(conexao, "fato_behavior") |>
  collect() |>
  mutate(
    mes_ref = as.Date(mes_ref, "1970-01-01")
  ) |>
  View()

data_ref <- "2024-03-01"

dados_behavior_varias_datas <- tbl(conexao, "fato_behavior") |>
  collect() |>
  mutate(
    mes_ref = as.Date(mes_ref, "1970-01-01")
  ) |>
  group_by(id_cliente) |>
  mutate(
    var_lag_mensag = lag(numero_mensagens_portal),
    var_lag_evento = lag(participacao_evento),
    max_mensag = lag(cummax(numero_mensagens_portal)),
    max_evento = lag(cummax(participacao_evento)),
    min_mensag = lag(cummin(numero_mensagens_portal)),
    min_evento = lag(cummin(participacao_evento))
  ) |>
  select(
    -numero_mensagens_portal, -participacao_evento
  )

dim_cliente <- tbl(conexao, "dim_cliente") |>
  collect()

valor_pago <- fato_pag_aprovado |>
  group_by(id_assinatura) |>
  summarise(
    valor_pago_medio = mean(valor_pago)
  ) |>
  ungroup()

base_modelagem <- tabela_churn |>
  mutate(
    data_ref_behavior = case_when(
      !is.na(data_primeira_inad) ~ data_primeira_inad %m-% months(1),
      TRUE ~ as.Date(data_ref)
    )
  ) |>
  left_join(dim_assinatura) |>
  left_join(dim_cliente) |>
  left_join(valor_pago) |>
  left_join(dados_behavior_varias_datas, by = c("id_cliente", "data_ref_behavior" = "mes_ref"))

saveRDS(base_modelagem, "base_modelagem.rds")
