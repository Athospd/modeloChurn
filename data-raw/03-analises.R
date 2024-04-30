# pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

# dados -------------------------------------------------------------------

dados <- readRDS("base_modelagem.rds") |>
  #sample_n(1000) |>
  mutate(
    idade_assinatura = as.numeric(round((data_ref_behavior-data_entrada)/30))
  ) |>
  select(
    -id_assinatura,
    -data_primeira_inad,
    -data_ultima_inad,
    -id_cliente,
    -data_de_pagamento,
    -data_ref_behavior
  ) |>
  mutate(
    mes = as.character(month(data_entrada)),
    ano = as.character(year(data_entrada)),
    churn = as.factor(churn)
  ) |>
  select(-data_entrada) |>
  mutate(
    observacao_recente = as.character(is.na(var_lag_mensag))
  ) |>
  replace_na(
    list(
      var_lag_mensag = 0,
      var_lag_evento = 0,
      max_mensag = 0,
      min_mensag = 0,
      max_evento = 0,
      min_evento = 0
    )
  ) |>
  select(-ano, -mes)

# prob churn --------------------------------------------------------------

base_assinaturas_ativas <- dados |>
  filter(churn == 0)

prob_churn = predict(modelo_vigente, new_data = base_assinaturas_ativas, type = "prob")$.pred_1

base_assinaturas_ativas_churn <- base_assinaturas_ativas |>
  mutate(
    expectativa_churn = prob_churn > .7
  )

base_assinaturas_ativas_churn |>
  group_by(valor_pago_medio) |>
  summarise(
    freq = n(),
    expectativa_churn = sum(expectativa_churn)
  )

base_assinaturas_ativas_churn |>
  group_by(sexo) |>
  summarise(
    freq = n(),
    expectativa_churn = sum(expectativa_churn)
  )

base_assinaturas_ativas_churn |>
  group_by(idade_assinatura) |>
  summarise(
    freq = n(),
    expectativa_churn = sum(expectativa_churn),
    p = expectativa_churn/freq
  ) |>
  ggplot(aes(x = idade_assinatura, y = p)) +
  geom_point()


