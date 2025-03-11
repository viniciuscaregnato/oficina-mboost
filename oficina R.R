#### IMPLEMENTANDO O ALGORITIMO BOOSTING ####
library(mboost)

f_boost_simple <- function(
    x, # matriz dos regressores
    y, # vetor com a variável dependente
    eta = 0.1, # parâmetro de amortecimento
    num_ite = 100 # número de iterações
) {
  require(mboost)
  
  x <- as.matrix(x)
 
  ## definindo a regressao ## 
  reg_opt <- glmboost(
    y = as.vector(y),
    x = x,
    offset = 0,
    center = TRUE,
    control = boost_control(
      mstop = num_ite,
      nu = eta
    )
  )
  
  ## avaliando a regressao ##
  aic <- AIC(reg_opt, method = "corrected")
  aic_seq <- attributes(aic)$AIC
  m_opt <- min(
    c(
      which(diff(aic_seq) > 0)[1],
      which.min(aic_seq)
    ),
    na.rm = TRUE
  )
  reg_opt[m_opt]
  
  df_opt <- attributes(aic)$df[m_opt]
  
  coef_opt_aux <- mboost::extract(reg_opt, what = "coefficients")
  coef_opt <- rep(0, ncol(x))
  names(coef_opt) <- colnames(x)
  coef_opt[names(coef_opt_aux)] <- coef_opt_aux
  
  return(
    list(
      reg_opt = reg_opt,
      coef_opt = coef_opt,
      df_opt = df_opt,
      m_opt = m_opt,
      aic_opt = min(aic_seq)
    )
  )
}

#### MAO NA MASSA: adicionando as variaveis componentes ####

library(tidyverse)


load("data.rda")
View(data)

# dependent variable: `CPIAUCSL`

row.names(data) <- data$date #colocou as datas da coluna date como nome das linhas

data_1 <- select(data, -"date") #exclue coluna data
View(data_1)

data_2 <- cbind( # aqui ele combina o data_1 com os 4 principais componentes, definidos pelo PCA sobre os dados normalizados
  data_1,
  factors = princomp(scale(data_1))$scores[, 1:4]
)
View(data_2)
colnames(data_2) # as 4 colunas factors.Comp._ sao os componentes adicionados


dados_cpi <- data_2$CPIAUCSL

tempo <- seq(                # apenas define os parametros do grafico
  from = as.Date(row.names(data)[1]),
  to = as.Date(tail(row.names(data), 1)),
  by = "months"
)
plot(tempo, dados_cpi, type = "l") #observamos a variação de CPIAUCSL no tempo

dados_y <- data.frame(y = dados_cpi) #cria um data frame com os valores de dados_cpi na coluna "y"
dados_x <- select(data_2, -"CPIAUCSL") #data frame de data_2 sem o CPIAUCSL
y_vec <- dados_y$y 

dim(dados_x) #dimensao dos dados dos regressores

#### SERAO CRIADAS FUNÇÕES PARA INCLUIR AS DEFASAGENS DAS VAR. IND E DEPENDENTES ####

f_add_lags_aux <- function(  # a função apenas criar os lags desejados
    x, # vetor com os valores numéricos
    lag_max, # lag máx. desejado
    name # nome a ser colocado no vetor resultante
) {
  result <- embed(x, lag_max) %>% data.frame()
  colnames(result) <- c(
    paste(name, "_t_", 1:lag_max, sep = "")
  )
  
  return(result)
}

f_add_lags <- function(
    x, # data.frame com colunas nomeadas
    lag_max # lag máx. desejado
) {
  
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame"
    )
  }
  
  dados <- data.frame(
    temp = rep(1, nrow(x) - lag_max + 1)
  )
  name_aux <- colnames(x)
  
  for (i in seq_len(ncol(x))) {
    dados <- data.frame(
      dados,
      f_add_lags_aux(
        x = x[, i],
        lag_max = lag_max,
        name = name_aux[i]
      )
    )
  }
  dados_aux <- data.frame(dados[, -1])
  colnames(dados_aux) <- colnames(dados)[-1]
  return(dados_aux)
}


