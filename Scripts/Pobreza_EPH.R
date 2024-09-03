# Pobreza EPH

library(eph)
library(data.table)
library(openxlsx)
library(readxl)
library(tidyverse)
library(nlme)
library(zoo)


# Carga de las bases ####
# Hogar
hogar_NEA <- fread("Bases/hogar_NEA.txt")
hogar_NEA[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]
# Individual
individual_NEA <- fread("Bases/individual_NEA.txt")
individual_NEA[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]
individual_NEA <- individual_NEA[CH03==1,] # Filtramos por jefe de hogar

setnames(hogar_NEA, "CBA", "CBA_hogar")
setnames(hogar_NEA, "CBT", "CBT_hogar")
individual_NEA <- merge.data.table(individual_NEA, hogar_NEA[,c(2:6,11,25,66,67,91:92,98:99)],
                                   by = c("AGLOMERADO", "ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR"))

setorder(individual_NEA, CODUSU, NRO_HOGAR, ANO4, TRIMESTRE)

#individual_NEA[, patron:= ifelse(CAT_OCUP==1, 1, 0)]
# Index
individual_NEA[, index:= paste0(CODUSU, NRO_HOGAR)]
# Informal
individual_NEA[, informal:= ifelse(PP07H==0, 1, 0)]
# Aglomerado
individual_NEA[, AGLO_DESC:= as.factor(AGLO_DESC)]
# Nivel educativo
individual_NEA[, NIVEL_ED:= as.factor(case_when(NIVEL_ED%in%c(7,1,2) ~ "Primario completo",
                                                NIVEL_ED==3 ~ "Secundario incompleto",
                                                NIVEL_ED==4 ~ "Secundario completo",
                                                NIVEL_ED==5 ~ "Universitario incompleto",
                                                NIVEL_ED==6 ~ "Universitario completo"))]
# Horas trabajadas
individual_NEA[, horas_trab:= as.numeric(PP3E_TOT) + PP3F_TOT]

# Ingreso per cápita familiar
individual_NEA[, IPCF:= as.numeric(str_replace(IPCF, ",","."))]

# Categoría de ocupación
individual_NEA[, CAT_OCUP:= as.factor(case_when(CAT_OCUP==1 ~ "Patrón",
                                                CAT_OCUP==2 ~ "Cuenta propia",
                                                CAT_OCUP==3 ~ "Obrero o empleado",
                                                CAT_OCUP==4 ~ "Trabajo fliar sin rem",
                                                CAT_OCUP==0 & ESTADO==3 ~ "Inactivo"))]

# Deflactamos IPCF
ipc <- read_excel("IPC_NEA_2004.xlsx", sheet = "Trimestral") %>% na.omit() %>% as.data.table()
ipc <- ipc %>% mutate(periodo = as.yearqtr(paste0(anio,"-",trimestre))) 
ipc <- ipc[ ,c("periodo", "ipc_b24")]
individual_NEA <- merge.data.table(individual_NEA, ipc, by = "periodo")

# Ingreso per cápita familiar deflactado
individual_NEA[, IPCF_d:= IPCF/ipc_b24*100]

# Calificación del puesto
individual_NEA <- organize_cno(individual_NEA)
individual_NEA[, CALIFICACION:= ifelse(is.na(CALIFICACION)==T | 
                                         CALIFICACION %in% c("falta informacion","Ns.Nc","otro"), "N/s", CALIFICACION)]

# Sector de actividad
individual_NEA <- organize_caes(individual_NEA)

# Logaritmo de IPCF
individual_NEA[, logIPCF_d:=log(IPCF_d)]

# Dummie de mujer
individual_NEA[, mujer:= ifelse(CH04==2, 1, 0)]

# Otros ingresos no laborales
individual_NEA[, otros_ing_nolab:= T_VI - V5_M - V8_M]

# Filtramos por jefes de hogar
individual_NEA <- individual_NEA[CH03==1]

individual_NEA[, index:= as.factor(index)]
individual_NEA[, anio:= as.factor(ANO4)]

# Variables socioeconómicas
individual_NEA[, nro_rep:= as.factor(nro_rep)]
individual_NEA[, casadounido:= ifelse(CH07 %in% c(1,2), 1, 0)]
individual_NEA[, casadpto:= ifelse(IV1 %in% c(1,2), 1, 0)]
individual_NEA[, leer:= ifelse(CH09==1, 1, 0)]
individual_NEA[, basural:= ifelse(IV12_1==1, 1, 0)]

# Nro de entrevista
setorder(individual_NEA, "CODUSU", "NRO_HOGAR","periodo")
individual_NEA[, n:=1]
individual_NEA[, n_entrevista:= cumsum(n), by = .(CODUSU, NRO_HOGAR)]
individual_NEA$n=NULL

# Calculamos el identificador por 'área'
individual_NEA[, area:= substr(CODUSU, 1, 8)]

# Filtramos la base de datos por los períodos a analizar
individual_NEA <- subset(individual_NEA, !(ANO4 %in% c(2016,2017,2023)) & IPCF_d>0)


# Modelos #####




# Tablas descriptivas
tabla1 <- prop.table(table(individual_NEA$AGLO_DESC, individual_NEA$nro_rep), margin=2)
tabla1

# Test Chi-cuadrado para asociación de variables
test1 <- chisq.test(table(individual_NEA$nro_rep, individual_NEA$hogar_pobre))
test2 <- chisq.test(table(individual_NEA$nro_rep, individual_NEA$NIVEL_ED))
test3 <- chisq.test(table(individual_NEA$nro_rep, individual_NEA$casadounido))
test4 <- chisq.test(table(individual_NEA$nro_rep, individual_NEA$ESTADO))
test5 <- chisq.test(table(individual_NEA$nro_rep, individual_NEA$casadpto))
# Se rechaza la hipótesis nula de independencia entre variables en todos los casos.

# Guardamos los resultados en una tabla
tabla2 <- data.frame(Variables = c("NRO_REP ~ hogar_pobre",
                                   "NRO_REP ~ NIVEL_ED",
                                   "NRO_REP ~ casadounido",
                                   "NRO_REP ~ ESTADO",
                                   "NRO_REP ~ casadpto"),
                     Estadístico = c(test1$statistic, test2$statistic, test3$statistic,test4$statistic, test5$statistic),
                     df = c(test1$parameter,test2$parameter,test3$parameter,test4$parameter,test5$parameter),
                     p.value = c(test1$p.value, test2$p.value, test3$p.value, test4$p.value, test5$p.value))




library(ggthemr)
ggthemr("sky")

# Serie de nro de repeticiones por aglomerado
grafico1 <- individual_NEA %>% 
  mutate(n=1) %>% 
  group_by(periodo, AGLO_DESC, nro_rep) %>% 
  summarise(n=sum(n)) %>% 
  ggplot(aes(periodo, n, fill=as.factor(nro_rep))) +
  geom_bar(stat = "identity", position = "fill", color="white") +
  facet_wrap(.~AGLO_DESC, ncol = 2, scales = "free_x") +
  scale_x_yearqtr(format="%Y-%qT", expand=c(0,0)) + 
  xlab("") + ylab("") + 
  theme_grey() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Valores altos de PONDIH y PONDERA
# Suponemos reponderación por baja tasa de respuesta
individual_NEA %>% 
  ggplot(aes(PONDIH,AGLO_DESC)) +
  geom_boxplot() +
  theme_grey()

# Resultados pobreza con base hogar ####
resultados_pobreza <- individual_NEA[CH03==1, 
                                     .(hogares_pobres=sum(hogar_pobre*PONDIH)/sum(PONDIH),
                                       hogares_indigentes=sum(hogar_indigente*PONDIH)/sum(PONDIH)),
                                by = .(periodo, AGLO_DESC)]

# Resultados pobreza
ggplot(resultados_pobreza, aes(periodo,hogares_pobres,fill=AGLO_DESC)) +
  geom_bar(stat="identity", position = "dodge") + 
  theme_grey() + 
  ggtitle("Resultados de pobreza") +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab("") + ylab("")

# Resultados indigencia
ggplot(resultados_pobreza, aes(periodo,hogares_indigentes,fill=AGLO_DESC)) +
  geom_line(stat="identity") +
  theme_minimal() +
  ggtitle("Resultados de indigencia") +
  xlab("") + ylab("") + 
  

## MMultinomial para nro rep #### 
library(mclogit)
library(VGAM)

# Modelos para probabilidad de respuesta del hogar
# Modelo para Gran Resistencia
glm.multi.rcia <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Gran Resistencia"))


# # Test para evaluar bondad de ajuste
# library(ResourceSelection)
# ResourceSelection::hoslem.test(predict(glm.multi.rcia, type="link"), 
#                                ordered(individual_NEA$nro_rep[individual_NEA$AGLO_DESC=="Gran Resistencia"]))


# Modelo para Corrientes
glm.multi.ctes <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + IX_TOT + casadpto + casadounido, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Corrientes"))

# Modelo para Posadas
glm.multi.psdas <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto, 
                        family = cumulative(parallel = TRUE),
                        data = subset(individual_NEA, AGLO_DESC=="Posadas"))

# Modelo para Formosa
glm.multi.fmsa <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Formosa"))

# Tabla resumen del modelo
modelos.glm.multi <- list("Gran Rcia" = glm.multi.rcia, 
                          "Corrientes" = glm.multi.ctes, 
                          "Formosa" = glm.multi.fmsa, 
                          "Posadas" = glm.multi.psdas)

modelsummary(modelos.glm.multi, gof_map = "all", 
             stars = T,
             exponentiate = T,
             shape = term ~ model + statistic,
             estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
             statistic = "p.value")

anova()


# Modelos alternativos para probabilidades predichas -----------------------------

library(caret)       # Para matriz de confusión
library(rpart)       # Árbol de decisión
library(rpart.plot)  # Plot árbol de decisións
library(stats)       # Medidas de bondad de clasificación
library(ROCR)        # Para curvas ROC y AUC
library(performance) # Para curva ROC

# Indicadora de esquema completo (responde 4 veces)
individual_NEA[, completo:= ifelse(nro_rep==4, 1, 0)]

# Variables a utilizar (agregar más)
formula <- "as.factor(completo) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto"

# Muestra en train (70%) y test (30%) por aglomerado y período
individual_NEA_train <- data.table()
individual_NEA_test <- data.table()

for (i in unique(individual_NEA$AGLO_DESC)){
  for (j in unique(individual_NEA[AGLO_DESC==i]$periodo)){
    # Filtramos por aglomerado y período
    base <- individual_NEA %>% filter(AGLO_DESC==i & periodo==j)
    # Creamos la partición
    particion <- createDataPartition(base$completo, p = 0.7, list=FALSE)
    # Dividimos la muestra
    train_data <- base[particion, ]
    test_data <- base[-particion, ]
    # Anexamos a las tablas vacías
    individual_NEA_train <- rbind(individual_NEA_train, train_data)
    individual_NEA_test <- rbind(individual_NEA_test, test_data)
    # Limpiamos memoria
    rm(base, particion, train_data, test_data)
  }
}


# 1) Modelo logístico # --------------------
modelo_logit <- glm(formula, data=individual_NEA_train, family = binomial(link="logit"))
summary(modelo_logit)

# Clases predichas
individual_NEA_test$pclass_logit <- ifelse(predict.glm(modelo_logit, 
                                                       newdata = individual_NEA_test, 
                                                       type = "response")>0.5, 1, 0)

# Matriz de confusión
cm_logit <- confusionMatrix(table(individual_NEA_test$completo,
                                  individual_NEA_test$pclass_logit), 
                            positive="1")
cm_logit

# Curva ROC
pred.logit <- prediction(individual_NEA_test$pclass_logit, individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.logit <- performance (pred.logit, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.logit, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Logístico",
                            accuracy = round(cm_logit$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_logit$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_logit$byClass[["Specificity"]], 5),
                            auc = round(performance(pred.logit, measure = "auc")@y.values[[1]], 5)))

# 2) Árbol de decisión # ----------------

# Ajuste
modelo_rpart <- rpart(formula, data = individual_NEA_train, control = rpart.control(cp=0))

# Poda del árbol según relación costo-complejidad 
printcp(modelo_rpart)
modelo_rpart_podado <- prune(modelo_rpart, cp=0.002)
prp(modelo_rpart_podado, extra=101, type=2,  xsep="/") 

# Clases predichas
individual_NEA_test$pclass_cart <- as.factor(predict(modelo_rpart_podado, newdata=individual_NEA_test, type="class"))

# Probabilidades predichas
# individual_NEA_test$pprob_cart <- predict(modelo_rpart_podado, newdata=individual_NEA_test, type="prob")

# Matriz de confusión
cm_rpart <- confusionMatrix(table(individual_NEA_test$completo, individual_NEA_test$pclass_cart), positive="1")
cm_rpart

# Curva ROC
pred.cart <- prediction(individual_NEA_test$pclass_cart, individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.cart <- performance (pred.cart, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.logit, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Árbol CART",
                            accuracy = round(cm_rpart$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_rpart$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_rpart$byClass[["Specificity"]], 5),
                            auc = round(performance(pred.cart, measure = "auc")@y.values[[1]], 5)))


# 3) Random Forest # --------------
library(randomForest)
modelo_rf <- randomForest(formula, data = individual_GR)

# Matriz de confusión
individual_GR$predict <- predict(modelo, individual_GR)


confusionMatrix(table(individual_GR$completo, individual_GR$predict), positive = "1")
modelo$importance
predict(modelo, individual_GR, type="prob")


# Guardamos las probabilidades predichas
individual_GR$prob_completo <- predict(modelo, individual_GR, type="prob")[,2]
hist(individual_GR$prob_completo)
hist(individual_GR$prob4)
names(individual_GR) <- c(names(individual_GR)[1:224], "prob1", "prob2", "prob3", "prob4")


# Corrección del ponderador


# Vivienda relevadas por área y período
individual_GR %>% 
  group_by(periodo, area) %>% 
  summarise(viviendas = n_distinct(CODUSU)) %>% 
  ggplot(aes(as.factor(periodo), viviendas)) + 
  geom_boxplot()

# PONDIH según área y período
individual_GR %>% 
  filter(!(periodo %in% c("2020 Q1", "2020 Q2", "2020 Q3","2020 Q4"))) %>% 
  ggplot(aes(area, PONDIH)) + 
  geom_point() +
  facet_wrap(.~periodo)

# Viviendas relevadas por área
individual_GR %>% 
  filter(!(periodo %in% c("2020 Q1", "2020 Q2", "2020 Q3","2020 Q4"))) %>% 
  group_by(periodo, area) %>% 
  summarise(viviendas = n_distinct(CODUSU)) %>% 
  ggplot(aes(area, viviendas)) +
  geom_boxplot()

# Viviendas por área
individual_GR[, viviendas:= n_distinct(CODUSU), by = .(periodo, area)]
hist(individual_GR$viviendas)
summary(individual_GR$viviendas)

# Agregamos el total de PONDIH por 'área'
individual_GR[, mj:= sum(PONDIH,na.rm=T), by = .(periodo, area)]
hist(individual_GR$mj)
summary(individual_GR$mj)

# Ponderador corregido
individual_GR <- as.data.table(individual_GR)
individual_GR$w <- fitted(glm.multi.4)[,4]
individual_GR[, PONDIH_c:= round(ifelse(prob_completo>0, PONDIH/prob_completo, PONDIH))]
individual_GR[, mj_c:=sum(PONDIH_c), by = .(periodo, area)]
individual_GR[, PONDIH_c:= round(PONDIH_c*(mj/mj_c))]
hist(individual_GR$PONDIH_c)
hist(individual_GR$PONDIH)

# PONDIH vs PONDIH corregido
ggplot(individual_GR, aes(PONDIH, PONDIH_c)) +
  geom_point()

# PONDIH corregido
individual_GR %>% 
  filter(!(periodo %in% c("2020 Q1", "2020 Q2", "2020 Q3","2020 Q4"))) %>% 
  ggplot(aes(area, PONDIH_c)) + 
  geom_point() +
  geom_point(aes(area, PONDIH), color=2) +
  facet_wrap(.~periodo)

# Tablas de frecuencia de nro de repeticiones
library(expss)
fre(individual_GR$nro_rep, weight = individual_GR$PONDIH)
fre(individual_GR$nro_rep, weight = individual_GR$PONDIH_c)



# Percentil del ingreso per cápita familiar del aglomerado (orPONDIH# Percentil del ingreso per cápita familiar del aglomerado (original)
library(Hmisc)
tabla1 <- data.frame()
for (i in unique(individual_GR$periodo)){
  ecdf_IPCF <- wtd.Ecdf(individual_GR$IPCF_d[individual_GR$periodo==i], 
                        weights = individual_GR$PONDIH[individual_GR$periodo==i])
  
  anexo <- data.frame(periodo=i, IPCF=ecdf_IPCF$x, freq=ecdf_IPCF$ecdf)
  tabla1 <- rbind(tabla1, anexo)
  rm(anexo)
  
}

# Percentil del ingreso per cápita familiar del aglomerado (corregido)
tabla2 <- data.frame()
for (i in unique(individual_GR$periodo)){
  ecdf_IPCF <- wtd.Ecdf(individual_GR$IPCF_d[individual_GR$periodo==i],
                        weights = individual_GR$PONDIH_c[individual_GR$periodo==i])
  anexo <- data.frame(periodo=i, IPCF_c=ecdf_IPCF$x, freq_c=ecdf_IPCF$ecdf)
  tabla2 <- rbind(tabla2, anexo)
  rm(anexo)
}

tabla <- cbind(tabla1, tabla2[,-1])
rm(tabla1, tabla2)
tabla$periodo <- as.yearqtr(tabla$periodo)

# Distribuciones acumuladas de IPCF
tabla %>% 
  filter(year(periodo)==2018) %>%
  ggplot() + 
  geom_line(aes(IPCF, freq), linetype = "dashed") + 
  geom_line(aes(IPCF_c, freq_c)) + 
  facet_wrap(.~periodo, scales = "free")




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



# Calculamos pobreza con el cambio
resultados <- 
  individual_GR[CH03==1,
                 .(hogares_pobres=sum(hogar_pobre*PONDIH_c,na.rm=T),
                   hogares_indigentes=sum(hogar_indigente*PONDIH_c,na.rm=T),
                   total_hogares=sum(PONDIH_c,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados[, pobreza:= hogares_pobres/total_hogares]
resultados[, indigencia:= hogares_indigentes/total_hogares]

resultados_GR <- resultados_pobreza %>% 
  filter(AGLO_DESC=="Gran Resistencia") %>% 
  dplyr::select(periodo, pobreza, indigencia) %>% 
  arrange(periodo) %>% 
  merge.data.frame(resultados[,c(2,6,7)], by = "periodo") 




plot(fitted.values(modelo_multi), rstandard(modelo_multi))

# Probabilidades estimadas para cada nivel de nro_rep
probs <- predict(object=glm.multi.4, type="response")

# Para identificar el nivel de Response con mayor probabilidad
aux_fun <- function(x) names(x)[which.max(x)]
nrorep_hat <- apply(X=probs, MARGIN=1, FUN=aux_fun)

# Matriz de confusion
nrorep_hat <- factor(nrorep_hat, levels=levels(individual_GR$nro_rep))
tabla <- table(individual_GR$nro_rep, nrorep_hat)
tabla

# Precisión
sum(diag(tabla)) / sum(tabla)


# Guardamos valores ajustados
ajustados <- fitted.values(modelo_multi)
colnames(ajustados) <- paste0("fit_",colnames(ajustados))
tabla <- cbind(tabla, ajustados)

# Vemos probabilidades predichas
tabla %>% 
  select(hogar_pobre, AGLO_DESC, fit_rep_1, fit_rep_2, fit_rep_3, fit_rep_4) %>% 
  pivot_longer(cols=3:6, names_to = "repeticiones", values_to = "prob_predicha") %>% 
  mutate(hogar_pobre= as.factor(ifelse(hogar_pobre==1, "Pobre", "No pobre"))) %>% 
  ggplot(aes(x=AGLO_DESC, y = prob_predicha, color=hogar_pobre)) + 
  geom_point() + 
  facet_wrap(.~ repeticiones) +
  xlab("") + 
  theme_light()


# La chance de ser encuestado 4 veces en lugar de 3 veces para hogares pobres es 1.2 veces la de hogares no pobres 



# Calculamos ingreso per cápita por adulto equivalente
individual_NEA <- individual_NEA %>% 
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) %>% 
  mutate(adequi_hogar=sum(adequi,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(IPCAE=ITF/adequi_hogar) %>% 
  as.data.table()

# Modelo de regresión
# Canastas
canastas <- read_excel("Canastas_regionales.xlsx", sheet = "Mensual") %>% 
  mutate(trimestre=quarter(ymd(paste0(anio,"-",mes,"-01")))) %>%
  group_by(anio,trimestre,region,codigo) %>% 
  summarise(CBA=mean(CBA),CBT=mean(CBT)) %>% 
  dplyr::select(region,anio,trimestre,CBA,CBT,codigo) %>% 
  as.data.table()
names(canastas) <- c("REGION_DESC","ANO4","TRIMESTRE","CBA","CBT","REGION_COD")

canastas1 <- subset(canastas, REGION_DESC=="Noreste")[,c(2,3,5)]
setnames(canastas1, "CBT", "CBT_adequi")
individual_NEA <- merge.data.table(individual_NEA, canastas1, by = c("ANO4","TRIMESTRE"), all.x=T)


# Proporciones promedio NEA #####

tabla <- prop.table(table(individual_NEA$nro_rep, individual_NEA$AGLO_DESC), margin = 2)
rowMeans(tabla[,c(1,2,4)])

individual_GR <- individual_NEA[AGLO_DESC=="Gran Resistencia",]
         


# Calculamos pobreza con el cambio
resultados_pobreza <- 
  individual_NEA[CH03==1,
                 .(hogares_pobres=sum(hogar_pobre*PONDIH,na.rm=T),
                   hogares_indigentes=sum(hogar_indigente*PONDIH,na.rm=T),
                   total_hogares=sum(PONDIH,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados_pobreza[, pobreza:= hogares_pobres/total_hogares]
resultados_pobreza[, indigencia:= hogares_indigentes/total_hogares]

t2 <- resultados_pobreza1 %>% filter(AGLO_DESC=="Gran Resistencia")

cbind(t1, t2[,c(6,7)]) %>% 
  ggplot(aes(periodo, hogares_pobres)) + 
  geom_bar(stat="identity")

ggplot(resultados_pobreza, aes(periodo,pobreza)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~AGLO_DESC) + 
  theme_minimal() + 
  ggtitle("Resultados de pobreza") +
  xlab("") + ylab("") + 
  stat_smooth()

ggplot(resultados_pobreza, aes(periodo,hogares_indigentes)) +
  geom_bar(stat="identity") + 
  facet_wrap(.~AGLO_DESC) + 
  theme_minimal() +
  ggtitle("Resultados de indigencia") +
  xlab("") + ylab("") + 
  stat_smooth()
              

# Resistencia tiene alta probabilidad de que respondan 1 y 2 veces, al
# mismo tiempo que tiene baja probabilidad de que respondan 4 veces respecto
# a los demás aglomerados.
# Quienes responden menos veces es más probable que sean no pobres que pobres
# Ver por trimestre: ¿Qué explica el resultado del trimestre?
#   Todos los trimestres se renueva el 25% de la muestra



# Guardamos resultados
save(tabla1, tabla2, grafico1, modelos_panel, modelos.glm.multi,
     file = "Resultados.RData")
