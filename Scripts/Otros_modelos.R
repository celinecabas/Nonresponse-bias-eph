# Otras pruebas

## Modelo para logIPCF_d ####
library(plm)
panel_ind <- pdata.frame(individual_NEA, index=c("index", "periodo"))
panel_ind <- panel_ind %>% 
  mutate(anio=as.factor(ANO4)) %>% 
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