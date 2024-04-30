preve_receita <- function(dados_carteira){

  prob_churn <- predict(modelo_vigente,
                        new_data = dados_carteira,
                        type = "prob")$.pred_1

  dados_carteira |>
    dplyr::mutate(
      expectativa_churn = prob_churn > .7,
      valor_pago_medio_mensal = ifelse(tipo_assinatura == "mensal",
                                       valor_pago_medio,
                                       valor_pago_medio/12)
    ) |>
    dplyr::summarise(
      numero_esperado_churn = round(sum(expectativa_churn)),
      valor_financeiro_perdido = -sum(expectativa_churn*valor_pago_medio)
    )
}
