# pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
set.seed(20240410)

# lendo dados -------------------------------------------------------------

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

treino_e_teste <- initial_split(dados)

treino <- training(treino_e_teste)
teste <- testing(treino_e_teste)

# receita -----------------------------------------------------------------

receita <- recipe(churn ~ ., data = treino) |>
  step_normalize(all_numeric()) |>
  step_dummy(all_nominal_predictors())

# modelo ------------------------------------------------------------------

especificacao_modelo <- logistic_reg(mixture = 1, penalty = tune()) |>
  # especificação LASSO
  set_mode("classification") |>
  set_engine("glmnet")

meu_workflow <- workflow(
  preprocessor = receita,
  spec = especificacao_modelo
)

# escolha de hiperparametro -----------------------------------------------

reamostras <- vfold_cv(treino, v = 5)

resultado_cv <- tune_grid(
  meu_workflow,
  reamostras, grid = 40,
  control = control_grid(verbose = TRUE)
)


# plot resultados ---------------------------------------------------------

autoplot(resultado_cv)

show_best(resultado_cv, metric = "accuracy")

# ajuste do modelo --------------------------------------------------------

especificacao_final <- especificacao_modelo |>
  finalize_model(
    select_best(resultado_cv, "roc_auc")
  )

workflow_final <- workflow(receita, especificacao_final)

last_fit(workflow_final, treino_e_teste) |>
  collect_metrics()


last_fit(workflow_final, treino_e_teste)$.predictions[[1]] |>
  mutate(
    predicao_alternativa = .pred_1 > 0.7
  ) |>
  count(predicao_alternativa, churn) |>
  group_by(
    predicao_alternativa
  ) |>
  mutate(
    p = n/sum(n)
  )
# aqui quandou falo que vai ter churn tem 85% de chance
# de estar certo

modelo_vigente <- workflow(
  preprocessor = receita,
  spec = especificacao_final
) |>
  fit(dados)


# importancia das variaveis -----------------------------------------------

library(vip)

modelo_vigente |>
  extract_fit_engine() |>
  vi(num_features = 14) |>
  mutate(
    Variable = fct_reorder(Variable, Importance)
  ) |>
  ggplot(aes(x = Variable, y = Importance, fill = Sign)) +
  geom_col() +
  coord_flip()

predict(modelo_vigente,
        dados |> filter(churn == 0),
        type = "prob") |>
  count(
    sum(.pred_1 > .7)
  )

usethis::use_data(modelo_vigente, overwrite = TRUE)


