devtools::load_all()
library(plumber)

#* @apiTitle API do modelo de Churn
#* @apiDescription Esta API recebe dados sobre a assinatura do cliente

#* Devolve diagnóstico sobre a possibilidade de Churn
#* @param tipo_assinatura:chr 'mensal' ou 'anual'
#* @param sexo:chr 'M' ou 'F'
#* @param idade:dbl idade do cliente assinante em anos
#* @param channel:chr canal de conversão do cliente
#* @param valor_pago_medio:dbl Valor pago médio por mês em R$
#* @param var_lag_mensag:dbl Número de mensagens enviadas no último mês vigente
#* @param var_lag_evento:dbl Número de eventos no último mês vigente
#* @param max_mensag:dbl Número máximo de mensagens enviadas em um mês do histórico do cliente
#* @param max_evento:dbl Número máximo de eventos em que o cliente participou em um único mês
#* @param min_mensag:dbl Número mínimo de mensagens enviadas em um mês do histórico
#* @param min_evento:dbl Número mínimo de eventos em que o cliente participou em um único mês
#* @param idade_assinatura:dbl Número de meses desde o primeiro pagamento da assinatura
#* @param observacao_recente:chr 'TRUE' se só houve um pagamento até agora e 'FALSE' em caso contrário
#* @post /gera_previsao
function(tipo_assinatura,
         sexo,
         idade,
         channel,
         valor_pago_medio,
         var_lag_mensag,
         var_lag_evento,
         max_mensag,
         max_evento,
         min_mensag,
         min_evento,
         idade_assinatura,
         observacao_recente) {

  lista_resposta <- data.frame(
    tipo_assinatura = tipo_assinatura,
    sexo = sexo,
    idade = as.numeric(idade),
    channel = channel,
    valor_pago_medio = as.numeric(valor_pago_medio),
    var_lag_mensag = as.numeric(var_lag_mensag),
    var_lag_evento = as.numeric(var_lag_evento),
    max_mensag = as.numeric(max_mensag),
    max_evento = as.numeric(max_evento),
    min_mensag = as.numeric(min_mensag),
    min_evento = as.numeric(min_evento),
    idade_assinatura = as.numeric(idade_assinatura),
    observacao_recente = observacao_recente
  )

  resposta <- aplica_modelo(
    tipo_assinatura = tipo_assinatura,
    sexo = sexo,
    idade = as.numeric(idade),
    channel = channel,
    valor_pago_medio = as.numeric(valor_pago_medio),
    var_lag_mensag = as.numeric(var_lag_mensag),
    var_lag_evento = as.numeric(var_lag_evento),
    max_mensag = as.numeric(max_mensag),
    max_evento = as.numeric(max_evento),
    min_mensag = as.numeric(min_mensag),
    min_evento = as.numeric(min_evento),
    idade_assinatura = as.numeric(idade_assinatura),
    observacao_recente = observacao_recente
  )

  lista_resposta$previsao_churn_1m = resposta$predicao

  lista_resposta$prob = resposta$prob

  return(lista_resposta)
}
