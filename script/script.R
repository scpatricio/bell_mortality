setwd("/Users/scp93/Dropbox/CEDEPLAR/script/")
rm(list=ls(all=TRUE))

# carrega script com funcoes
source("functions.R")

# carrega as bases de dados
load("HMD_Dx.Rdata")
load("HMD_Ex.Rdata")
load("HMD_LT_t.Rdata")

############# mortalidade em 1900 #############
ano = 1900
# dados
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
dados_Lt = HMD_LT_t$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total,
                  ex = dados_Lt$ex)

plot(data$idade, data$Dx/data$Ex, xlab = "idade", ylab = 'mortality rate', pch = 16)
plot(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', pch = 16)

abline(v = c(30, 90))

data = data[which(data$idade>=30 & data$idade<=90),]

########### Gompertz ########### 

# estimacao bell
bell_estim = abs(optim(par = c(0.001, 0.1),
                       fn = logLike_bell,
                       Ex = data$Ex,
                       Dx = data$Dx,
                       mu = gompertz,
                       t = data$idade-30)$par)

# estimacao BN
bn_estim = abs(optim(par = c(1, 0.001, 0.1),
                     fn = logLike_BN,
                     Ex = data$Ex,
                     Dx = data$Dx,
                     mu = gompertz,
                     t = data$idade-30)$par)

# plot do ajusta
lines(data$idade, log(gompertz(data$idade-30, bell_estim)), col = 2, lwd = 2)
lines(data$idade, log(gompertz(data$idade-30, bn_estim[-1])), col = 4, lwd = 2)


# Qual a melhor distribuicao para esse caso? - Usaremos o MAPE 
# log-mortalidade observada
obs = log(data$Dx/data$Ex)

# log-mortalidade estimada - Bell
est_bell = log(gompertz(data$idade-30, bell_estim))

# log-mortalidade estimada - BN
est_bn = log(gompertz(data$idade-30, bn_estim[-1]))

# MAPE - Bell
mean(abs(est_bell/obs-1))*100

# MAPE - BN
mean(abs(est_bn/obs-1))*100

# A distribuicao Bell forneceu melhores estimativas para o modelo gompertz
# Interpretacao dos parametros

# a --> força inicial de mortalidade
# b --> aumento da força de mortalidade ao longo da idade t

# Vida media residual - 30a
ex_gomp(0, bell_estim[1], bell_estim[2])

# lifetable
data[1,]

############  Makeham ########### 
# dados
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
dados_Lt = HMD_LT_t$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total,
                  ex = dados_Lt$ex)

# plot(data$idade, data$Dx/data$Ex, xlab = "idade", ylab = 'mortality rate', pch = 16)
plot(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', pch = 16)

abline(v = c(30, 90))

data = data[which(data$idade>=30 & data$idade<=90),]
# estimacao bell
bell_estim = abs(optim(par = c(0.001, 0.1, 0.001),
                       fn = logLike_bell,
                       Ex = data$Ex,
                       Dx = data$Dx,
                       mu = makeham,
                       t = data$idade-30)$par)

# estimacao BN
bn_estim = abs(optim(par = c(1, 0.001, 0.1, 0.001),
                     fn = logLike_BN,
                     Ex = data$Ex,
                     Dx = data$Dx,
                     mu = makeham,
                     t = data$idade-30)$par)

# plot do ajusta
lines(data$idade, log(makeham(data$idade-30, bell_estim)), col = 2, lwd = 2)
lines(data$idade, log(makeham(data$idade-30, bn_estim[-1])), col = 4, lwd = 2)


# Qual a melhor distribuicao para esse caso? - Usaremos o MAPE 
# log-mortalidade observada
obs = log(data$Dx/data$Ex)

# log-mortalidade estimada - Bell
est_bell = log(makeham(data$idade-30, bell_estim))

# log-mortalidade estimada - BN
est_bn = log(makeham(data$idade-30, bn_estim[-1]))

# MAPE - Bell
mean(abs(est_bell/obs-1))*100

# MAPE - BN
mean(abs(est_bn/obs-1))*100

# A distribuicao Bell forneceu melhores estimativas para o modelo gompertz
# Interpretacao dos parametros

# a --> força inicial de mortalidade - degradacao do corpo
# b --> aumento da força de mortalidade ao longo da idade t
# c --> forca de mortalidade (acidentes, doenças...)

# Vida media residual - 30a
ex_mak(0, bell_estim[1], bell_estim[2], bell_estim[3])

# lifetable
data[1,]

# Qual modelo utilizar? Para estes dados, a distribuicao Bell com a Lei Makeham forneceu melhores resultado


############# mortalidade em 2000 #############
ano = 2000
# dados
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
dados_Lt = HMD_LT_t$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total,
                  ex = dados_Lt$ex)

plot(data$idade, data$Dx/data$Ex, xlab = "idade", ylab = 'mortality rate', pch = 16)
plot(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', pch = 16)

abline(v = c(30, 90))

data = data[which(data$idade>=30 & data$idade<=90),]

########### Gompertz ########### 

# estimacao bell
bell_estim = abs(optim(par = c(0.001, 0.1),
                       fn = logLike_bell,
                       Ex = data$Ex,
                       Dx = data$Dx,
                       mu = gompertz,
                       t = data$idade-30)$par)

# estimacao BN
bn_estim = abs(optim(par = c(1, 0.001, 0.1),
                     fn = logLike_BN,
                     Ex = data$Ex,
                     Dx = data$Dx,
                     mu = gompertz,
                     t = data$idade-30)$par)

# plot do ajusta
lines(data$idade, log(gompertz(data$idade-30, bell_estim)), col = 2, lwd = 2)
lines(data$idade, log(gompertz(data$idade-30, bn_estim[-1])), col = 4, lwd = 2)


# Qual a melhor distribuicao para esse caso? - Usaremos o MAPE 
# log-mortalidade observada
obs = log(data$Dx/data$Ex)

# log-mortalidade estimada - Bell
est_bell = log(gompertz(data$idade-30, bell_estim))

# log-mortalidade estimada - BN
est_bn = log(gompertz(data$idade-30, bn_estim[-1]))

# MAPE - Bell
mean(abs(est_bell/obs-1))*100

# MAPE - BN
mean(abs(est_bn/obs-1))*100

# A distribuicao Bell forneceu melhores estimativas para o modelo gompertz
# Interpretacao dos parametros

# a -->
# b -->

# Vida media residual - 30a
ex_gomp(0, bell_estim[1], bell_estim[2])

# lifetable
data[1,]

############  Makeham ########### 
# dados
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
dados_Lt = HMD_LT_t$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total,
                  ex = dados_Lt$ex)

# plot(data$idade, data$Dx/data$Ex, xlab = "idade", ylab = 'mortality rate', pch = 16)
plot(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', pch = 16)

abline(v = c(30, 90))

data = data[which(data$idade>=30 & data$idade<=90),]
# estimacao bell
bell_estim = abs(optim(par = c(0.001, 0.1, 0.001),
                       fn = logLike_bell,
                       Ex = data$Ex,
                       Dx = data$Dx,
                       mu = makeham,
                       t = data$idade-30)$par)

# estimacao BN
bn_estim = abs(optim(par = c(1, 0.001, 0.1, 0.001),
                     fn = logLike_BN,
                     Ex = data$Ex,
                     Dx = data$Dx,
                     mu = makeham,
                     t = data$idade-30)$par)

# plot do ajusta
lines(data$idade, log(makeham(data$idade-30, bell_estim)), col = 2, lwd = 2)
lines(data$idade, log(makeham(data$idade-30, bn_estim[-1])), col = 4, lwd = 2)

# Qual a melhor distribuicao para esse caso? - Usaremos o MAPE 
# log-mortalidade observada
obs = log(data$Dx/data$Ex)

# log-mortalidade estimada - Bell
est_bell = log(makeham(data$idade-30, bell_estim))

# log-mortalidade estimada - BN
est_bn = log(makeham(data$idade-30, bn_estim[-1]))

# MAPE - Bell
mean(abs(est_bell/obs-1))*100

# MAPE - BN
mean(abs(est_bn/obs-1))*100

# A distribuicao Bell forneceu melhores estimativas para o modelo gompertz
# Interpretacao dos parametros

# a -->
# b -->
# c -->

# Vida media residual - 30a
ex_mak(0, bell_estim[1], bell_estim[2], bell_estim[3])

# lifetable
data[1,]

# Qual modelo utilizar? Para estes dados, a distribuicao Bell com a Lei Makeham forneceu melhores resultado


############# mortalidade em 2000 vs 1900 #############
# taxa de mortalidade em 2000
ano = 2000
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total)

plot(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', type = "l", lwd = 2)

# taxa de mortalidade em 1900
ano = 1900
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total)

lines(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', col = 2, lwd = 2)

# taxa de mortalidade em 1950
ano = 1950
dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]

data = data.frame(idade = dados_Dx$Age,
                  Dx = dados_Dx$Total,
                  Ex = dados_Ex$Total)

lines(data$idade, log(data$Dx/data$Ex), xlab = "idade", ylab = 'log mortality rate', col = 4, lwd = 2)


############# taxa mortalidade ao longo dos anos ############# 
# plot dos dados em 3d
year <- HMD_Dx$years
age <- HMD_Dx$ages

data.aux = HMD_Dx$data
aux = as.matrix(log(data.aux[,4:6]/HMD_Ex$data[,4:6]))

aux[which(is.na(aux)| abs(aux)==Inf)] = NA

data.aux[,4:6] = aux

mu_observ = matrix(NA, ncol = length(year), nrow = length(age))
for(i in 1:length(year)){
  mu_observ[,i] = data.aux$Total[which(data.aux$Year==year[i])]
}

if(!require(plotly)) {
  install.packages("plotly"); require(plotly)}

plot_ly(x = year, y = age, z = mu_observ) %>%
  layout(title = "Log-mortality rate") %>%
  add_surface(  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ) ) 


plot_ly(x = year, y = 30:90, z = mu_observ[30:90,]) %>%
  layout(title = "Log-mortality rate - [30,90]") %>%
  add_surface(  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ) ) 

############# e_x ao longo dos anos ############# 
year <- HMD_Dx$years
age <- HMD_Dx$ages

aux = HMD_LT_t$data

mu_observ = matrix(NA, ncol = length(year), nrow = length(age))
for(i in 1:length(year)){
  mu_observ[,i] = aux$ex[which(aux$Year==year[i])]
}

if(!require(plotly)) {
  install.packages("plotly"); require(plotly)}

plot_ly(x = year, y = age, z = mu_observ) %>%
  layout(title = "Life expectancy") %>%
  add_surface(  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ) ) 


plot_ly(x = year, y = 30:90, z = mu_observ[30:90,]) %>%
  layout(title = "Life expectancy - [30,90]") %>%
  add_surface(  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ) ) 

########### Gompertz ########### 
estim = apply(as.matrix(year), 1, function(ano){
  dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
  dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
  
  data = data.frame(idade = dados_Dx$Age,
                    Dx = dados_Dx$Total,
                    Ex = dados_Ex$Total)
  
  data = data[which(data$idade>=30 & data$idade<=90),]
  
  # estimacao bell
  bell_estim = abs(optim(par = c(0.001, 0.1),
                         fn = logLike_bell,
                         Ex = data$Ex,
                         Dx = data$Dx,
                         mu = gompertz,
                         t = data$idade-30)$par)
  
  fvmr = ex_gomp(0, bell_estim[1], bell_estim[2])
  
  out = c(bell_estim, fvmr)
  
  return(out)
})

estim_gomp = data.frame(ano = year, a = estim[1,], b = estim[2,],
                        fvmr = estim[3,], ex = HMD_LT_t$data$ex[HMD_LT_t$data$Age == 30])

# Evolucao do parametro a
plot(estim_gomp$ano, estim_gomp$a, pch = 16, xlab = "ano", ylab = "a", type = 'l')

# Evolucao do parametro b
plot(estim_gomp$ano, estim_gomp$b, pch = 16, xlab = "ano", ylab = "b", type = 'l')

# forca de mortalidade aos 30a
plot(estim_gomp$ano, estim_gomp$a, pch = 16, xlab = "ano", ylab = "a", type = 'l', col = 2, lwd = 2)
lines(estim_gomp$ano, 
      HMD_Dx$data$Total[HMD_Dx$data$Age == 30]/HMD_Ex$data$Total[HMD_Ex$data$Age == 30],
      pch = 16, xlab = "ano", ylab = "a", lwd = 2)

# Evolucao da life expectancy
plot(estim_gomp$ano, estim_gomp$fvmr, pch = 16, xlab = "ano", ylab = "life expectancy", type = 'l', lwd = 2)
lines(estim_gomp$ano, estim_gomp$ex, col = 2, type = 'l', lwd = 2)

########### Makeham ########### 
estim = apply(as.matrix(year), 1, function(ano){
  dados_Dx = HMD_Dx$data[which(HMD_Dx$data$Year == ano),]
  dados_Ex = HMD_Ex$data[which(HMD_Dx$data$Year == ano),]
  
  data = data.frame(idade = dados_Dx$Age,
                    Dx = dados_Dx$Total,
                    Ex = dados_Ex$Total)
  
  data = data[which(data$idade>=30 & data$idade<=90),]
  
  # estimacao bell
  bell_estim = abs(optim(par = c(0.001, 0.1, 0.001),
                         fn = logLike_bell,
                         Ex = data$Ex,
                         Dx = data$Dx,
                         mu = makeham,
                         t = data$idade-30)$par)
  
  fvmr = ex_mak(0, bell_estim[1], bell_estim[2], bell_estim[3])
  
  out = c(bell_estim, fvmr)
  
  return(out)
})

estim_mak = data.frame(ano = year, a = estim[1,], b = estim[2,], c = estim[3,],
                       fvmr = estim[4,], ex = HMD_LT_t$data$ex[HMD_LT_t$data$Age == 30])

# Evolucao do parametro a
plot(estim_mak$ano, estim_mak$a, pch = 16, xlab = "ano", ylab = "a", type = "l")

# Evolucao do parametro b
plot(estim_mak$ano, estim_mak$b, pch = 16, xlab = "ano", ylab = "b", type = "l")

# Evolucao do parametro c
plot(estim_mak$ano, estim_mak$c, pch = 16, xlab = "ano", ylab = "c", type = "l")


plot(estim_mak$ano, estim_mak$c+estim_mak$a, pch = 16, xlab = "ano", ylab = "a+c", type = "l")
lines(estim_mak$ano, estim_gomp$a, col = 2)

# forca de mortalidade aos 30a
plot(estim_mak$ano, estim_mak$c+estim_mak$a, pch = 16, xlab = "ano", ylab = "c", type = 'l', col = 2, lwd = 2)
lines(estim_gomp$ano, 
      HMD_Dx$data$Total[HMD_Dx$data$Age == 30]/HMD_Ex$data$Total[HMD_Ex$data$Age == 30],
      pch = 16, xlab = "ano", ylab = "a", lwd = 2)

# Evolucao da life expectancy
plot(estim_mak$ano, estim_mak$fvmr, pch = 16, xlab = "ano", ylab = "life expectancy", type = 'l', lwd = 2)
lines(estim_mak$ano, estim_mak$ex, col = 2, type = 'l', lwd = 2)