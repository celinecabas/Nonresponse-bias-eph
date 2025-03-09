# Otras pruebas

## Modelo para logIPCF_d ####
library(plm)
panel_ind <- pdata.frame(individual_NEA, index=c("index", "periodo"))
panel_ind <- panel_ind %>% 
  mutate(anio=as.factor(ANO4),
         nro_rep=as.numeric(nro_rep)) %>% 
  filter(is.infinite(logIPCF_d)==F)

hist(individual_NEA$IPCF_d)
hist(log(individual_NEA$IPCF_d))

### Modelo para el NEA ####
formula <- "logIPCF_d ~ horas_trab + NIVEL_ED + CAT_OCUP + CALIFICACION + 
             + otros_ing_nolab + mujer + anio + nro_rep"

modelo_nea <- plm(formula,
                  model="random", 
                  data = subset(panel_ind,!(ANO4 %in% c(2016,2017,2023))))

summary(modelo_nea)

### Modelo para Gran Rcia ####
modelo_rcia <- plm(formula,
                   model="random", 
                   data = subset(panel_ind, !(ANO4 %in% c(2016,2017,2023)) & AGLO_DESC=="Gran Resistencia"))
summary(modelo_rcia)

### Modelo para Corrientes ####
modelo_ctes <- plm(formula,
                   model="random", 
                   data = subset(panel_ind, !(ANO4 %in% c(2016,2017,2023)) & AGLO_DESC=="Corrientes"))
summary(modelo_ctes)

### Modelo para Formosa ####
modelo_formosa <- plm(formula,
                      model="random", 
                      data = subset(panel_ind, !(ANO4 %in% c(2016,2017,2023)) & AGLO_DESC=="Formosa"))
summary(modelo_formosa)

### Modelo para Posadas ####
modelo_posadas <- plm(formula,
                      model="random", 
                      data = subset(panel_ind, !(ANO4 %in% c(2016,2017,2023)) & AGLO_DESC=="Posadas"))
summary(modelo_posadas)

# Tabla resumen de modelos para logingreso
library(modelsummary)

modelos_panel <- list("Gran Rcia" = modelo_rcia, 
                      "Corrientes" = modelo_ctes, 
                      "Formosa" = modelo_formosa, 
                      "Posadas" = modelo_posadas)


modelsummary(modelos_panel, stars = T, 
             estimate="{estimate}{stars}",
             statistic = "conf.int")

# Diagnóstico
plot(c(fitted.values(modelo)), c(modelo$residuals))
hist(c(modelo$residuals), freq=F)
lines(density(c(modelo$residuals)))

# En todos los casos se rechaza homocedasticidad
lmtest::bptest(modelo_rcia) 
lmtest::bptest(modelo_ctes)
lmtest::bptest(modelo_formosa)
lmtest::bptest(modelo_posadas)




# Notas de modelo generalizado de momentos
# AJUSTE DEL PONDERADOR (PONDIH)
# Por método generalizado de momentos
library(gmm)

moment_conditions <- function(theta, m1, P, m) {
  # Aquí se definen las condiciones de momento ψ_j(θ)
  # Suponiendo que theta es el parámetro que estamos estimando.
  
  # Por simplicidad, en este ejemplo, θ no se usa. Debes ajustarlo según tu modelo.
  
  psi_j = sum(m1 / P) - m
  
  
  return(psi_j)
}

criterion <- function(theta, m1, P, m, W) {
  psi <- moment_conditions(theta, m1, P, m)
  criterion_value <- t(psi) %*% solve(W) %*% psi
  return(as.numeric(criterion_value))
}

W <- diag(nrow(individual_GR))  # Matriz de ponderación inicial

initial_theta <- 0  # Valor inicial para theta
result <- optim(initial_theta,
                criterion, 
                m1 = individual_GR$PONDIH, 
                P = individual_GR$prob4,
                m = individual_GR$mj, 
                W = W,
                method = "BFGS")

# Resultados
result$par  # Valor estimado de theta

# Definir la función de momento
moment_function <- function(t0, x) {
  
  mij = x$PONDIH
  P = x$prob4
  
  psi_j <- (mij/P) - t0
  return(psi_j)
  
}

# Crear una función de GMM
gmm_model <- gmm(g = moment_function,
                 t0 = c(t0=3),
                 x = individual_GR)

# Estimar el parámetro
summary(gmm_model)


# Función de momento para gmm
moment_conditions <- function(t0, x, weights) {
  alpha <- t0[1]
  beta <- t0[2]
  
  # Momentos teóricos
  theoretical_mean <- alpha * beta
  theoretical_variance <- alpha * beta^2
  
  # Condiciones de momento
  moment1 <- theoretical_mean - x
  moment2 <- theoretical_variance - (x-theoretical_mean)^2
  
  return(c(moment1, moment2))
  
}



gmm_ing <- gmm(g = moment_conditions,
               t0 = c(alpha=0.1,beta=0.1),
               x = rep(individual_GR$IPCF_d, individual_GR$PONDIH))

hist(rep(individual_GR$IPCF_d,individual_GR$PONDIH), freq = F)
points(individual_GR$IPCF_d,
       dgamma(x = individual_GR$IPCF_d, 
              shape = gmm_ing$coefficients[1], 
              scale = gmm_ing$coefficients[2]))

