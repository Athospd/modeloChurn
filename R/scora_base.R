scora_base <- function(dados_carteira){

  prob_churn <- predict(modelo_vigente,
                        new_data = dados_carteira,
                        type = "prob")$.pred_1

  dados_carteira |>
    dplyr::mutate(
      probabilidade_churn = round(1000*prob_churn)/10,
      expectativa_churn = prob_churn > .7,
      valor_pago_medio_mensal = ifelse(tipo_assinatura == "mensal",
                                       valor_pago_medio,
                                       valor_pago_medio/12)
    ) |>
    dplyr::select(
      id_assinatura, probabilidade_churn, expectativa_churn
    )
}
