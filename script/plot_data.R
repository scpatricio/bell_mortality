load("HMD_Dx.Rdata")
load("HMD_Ex.Rdata")

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
