# Método generalizado de momentos para las distribuciones
library(gmm)

# Función de momento para gmm
moment_conditions <- function(t0, x, weights) {
  alpha <- t0[1]
  beta <- t0[2]
  
  # Momentos teóricos
  theoretical_mean <- alpha * beta
  theoretical_variance <- alpha * beta^2
  
  # Condiciones de momento
  moment1 <- (theoretical_mean - x )*weights
  moment2 <- (theoretical_variance - (x-theoretical_mean)^2)*weigths
  
  return(c(moment1, moment2))
}

gmm_ing <- gmm(g = moment_conditions,
               t0 = c(alpha=0.1,beta=0.1),
               x = individual_GR$IPCF_d,
               weights = individual_GR$PONDIH)

hist(individual_GR$IPCF_d, freq = F)
points(individual_GR$IPCF_d,
       dgamma(x = individual_GR$IPCF_d, 
              shape = gmm_ing$coefficients[1], 
              scale = gmm_ing$coefficients[2]))
