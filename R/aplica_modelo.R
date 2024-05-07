aplica_modelo <- function(
  tipo_assinatura = "mensal",
  sexo = "M",
  idade = 36,
  channel = "Google",
  valor_pago_medio = 10,
  var_lag_mensag = 1,
  var_lag_evento = 1,
  max_mensag = 20,
  max_evento = 21,
  min_mensag = 1,
  min_evento = 1,
  idade_assinatura = 5,
  observacao_recente = "TRUE"
){
  prob_churn <- predict(modelo_vigente,
          new_data = data.frame(
            tipo_assinatura,
            sexo,
            idade,
            channel,
            valor_pago_medio,
            var_lag_mensag ,
            var_lag_evento ,
            max_mensag,
            max_evento,
            min_mensag,
            min_evento,
            idade_assinatura,
            observacao_recente
          ), type = "prob")$.pred_1

  if(prob_churn > 0.7){
    return(list(predicao = "Churn", prob = prob_churn))
  } else {
    return(list(predicao = "N\u00e3o Churn", prob = prob_churn))
  }
}
