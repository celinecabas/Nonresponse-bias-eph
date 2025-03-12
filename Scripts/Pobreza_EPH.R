# Pobreza EPH

library(eph)
library(data.table)
library(openxlsx)
library(readxl)
library(tidyverse)
library(nlme)
library(zoo)

# Paleta de colores de los gráficos
# https://colormagic.app/es/palette/6715f176e7aeb007989f9a3d


# Carga de las bases ####
# Hogar
hogar_NEA <- fread("Bases/hogar_NEA.txt")
hogar_NEA[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]

# Características de la vivienda
hogar_NEA[, (names(.SD)) := lapply(.SD, as.factor), 
          .SDcols = c("IV1", "IV3", "IV4", "IV5", "IV6", "IV7", "IV8", "IV9",
                      "IV10", "IV11", "IV12_1", "IV12_2", "IV12_3")]

# Características habitacionales del hogar
hogar_NEA[, (names(.SD)) := lapply(.SD, as.factor), 
          .SDcols = c("II3", "II4_1", "II4_2", "II4_3",
                      "II5", "II6", "II7", "II8", "II9")]

# Individual
individual_NEA <- fread("Bases/individual_NEA.txt")
individual_NEA[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]

setnames(hogar_NEA, "CBA", "CBA_hogar")
setnames(hogar_NEA, "CBT", "CBT_hogar")
individual_NEA <- merge.data.table(individual_NEA, hogar_NEA[,c(2:6,11,25,66,67,91:92,98)],
                                   by = c("AGLOMERADO", "ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR"))

setorder(individual_NEA, CODUSU, NRO_HOGAR, ANO4, TRIMESTRE)

#individual_NEA[, patron:= ifelse(CAT_OCUP==1, 1, 0)]

# Index
individual_NEA[, index:= paste0(CODUSU, NRO_HOGAR)]
# Informal
individual_NEA[, informal:= ifelse(PP07H==0, 1, 0)]
individual_NEA[, informal:= ifelse(is.na(informal)==T, 0, informal)]
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

# Sector de trabajo (ocupados)
individual_NEA[, PP04A:= ifelse(is.na(PP04A)==T, 0, PP04A)]
individual_NEA[, PP04A:= factor(PP04A, 
                                levels=c(0,1,2,3,9),
                                labels = c("No ocupado","Estatal","Privada","De otro tipo","Ns/Nr"))]
# Empleo doméstico (ocupados)
individual_NEA[, PP04B1:= ifelse(is.na(PP04B1)==T, 0, PP04B1)]
individual_NEA[, PP04B1:= factor(PP04B1,
                                 levels=c(0,1,2),
                                 labels=c("No ocupado","Presta servicio","No presta"))]


# Deflactamos IPCF
ipc <- read_excel("Bases/IPC_NEA_2004.xlsx", sheet = "Trimestral") %>% na.omit() %>% as.data.table()
ipc <- ipc %>% mutate(periodo = as.yearqtr(paste0(anio,"-",trimestre))) 
ipc <- ipc[ ,c("periodo", "ipc_b24")]
individual_NEA <- merge.data.table(individual_NEA, ipc, by = "periodo")

# Ingreso per cápita familiar deflactado
individual_NEA[, IPCF_d:= IPCF/ipc_b24*100]

# Logaritmo de IPCF
individual_NEA[, logIPCF_d:=log(IPCF_d)]

# Cantidad de varones
individual_NEA[, cantidad_varones:= n_distinct(COMPONENTE[CH04==1]),  by = .(periodo,CODUSU,NRO_HOGAR)]

# Cantidad de mujeres
individual_NEA[, cantidad_mujeres:= n_distinct(COMPONENTE[CH04==2]), by = .(periodo,CODUSU,NRO_HOGAR)]

# Cantidad de ocupados
individual_NEA[, cantidad_ocupados:= n_distinct(COMPONENTE[ESTADO==1]), by = .(periodo,CODUSU,NRO_HOGAR)]

# Cantidad de desocupados
individual_NEA[, cantidad_desocupados:= n_distinct(COMPONENTE[ESTADO==2]), by = .(periodo,CODUSU,NRO_HOGAR)]

# Cantidad de informales
individual_NEA[, cantidad_informales:= n_distinct(COMPONENTE[informal==1]), by = .(periodo,CODUSU,NRO_HOGAR)]

# Edad promedio del hogar
individual_NEA[, edad_promedio_hogar:= mean(CH06,na.rm=T), by = .(periodo,CODUSU,NRO_HOGAR)]

# Calificación del puesto
individual_NEA <- organize_cno(individual_NEA)
individual_NEA[, CALIFICACION:= ifelse(CALIFICACION %in% c("falta informacion","Ns.Nc","otro"), "N/s", CALIFICACION)]
individual_NEA[, CALIFICACION:= ifelse(is.na(CALIFICACION)==T, "No ocupado",CALIFICACION)]
individual_NEA[, CALIFICACION:= as.factor(CALIFICACION)]

# Jerarquía del puesto de trabajo
individual_NEA[, JERARQUIA:= ifelse(is.na(JERARQUIA)==T, "No ocupado", JERARQUIA)]
individual_NEA[, JERARQUIA:= as.factor(JERARQUIA)]

# Sector de actividad
individual_NEA <- organize_caes(individual_NEA)
individual_NEA[, caes_seccion_cod:= ifelse(ESTADO==1 & is.na(caes_seccion_cod)==T,"Ns/Nr",caes_seccion_cod)]
individual_NEA[, caes_seccion_cod:= ifelse(is.na(caes_seccion_cod)==T, "No ocupado",caes_seccion_cod)]

# Dummie de mujer
individual_NEA[, mujer:= ifelse(CH04==2, 1, 0)]

# Otros ingresos no laborales
individual_NEA[, otros_ing_nolab:= T_VI - V5_M - V8_M]

# Edad al cuadrado
individual_NEA[, CH06_2:= CH06^2]

# Filtramos por jefes de hogar
individual_NEA <- individual_NEA[CH03==1,]

# Calculamos el identificador por 'área'
individual_NEA[, area:= substr(CODUSU, 1, 8)]

# # Medidas resumen por área
# # Proporción de ocupados
# individual_NEA[, id_persona:= paste0(CODUSU,NRO_HOGAR,COMPONENTE)]
# individual_NEA[, prop_ocupados_area:= n_distinct(id_persona[ESTADO==1])/n_distinct(id_persona), by = .(area)]
# # Proporción de desocupados
# individual_NEA[, prop_desocup_area:= n_distinct(id_persona[ESTADO==2])/n_distinct(id_persona), by = .(area)]
# # Proporción de informales
# individual_NEA[, prop_informal_area:= n_distinct(id_persona[informal==1])/n_distinct(id_persona), by = .(area)]

# Formateamos "CODUSU-NROHOGAR" y año como factor
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


# Analizamos los períodos a filtrar
prop_por_periodo <- individual_NEA %>% 
  group_by(periodo, nro_rep) %>% 
  summarise(n=n_distinct(index)) %>% 
  group_by(periodo) %>% 
  mutate(total=sum(n)) %>% 
  ungroup() %>% 
  mutate(prop=n/total) %>% 
  select(periodo, nro_rep, prop) %>% 
  pivot_wider(names_from = "nro_rep", values_from = "prop")

# Quitamos un año al comienzo y al final + período de pandemia
# 2016 Q2, 2016 Q3, 2016Q4, 2017 Q1, 2020 Q2, 2023 Q3, 2023 Q4, 2024 Q1, 2024 Q2


# Filtramos la base de datos por los períodos a analizar
individual_NEA <- subset(individual_NEA, 
                         !(periodo %in% c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2020 Q2", "2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2")) & 
                           IPCF_d>0)


# Análisis de estructuras de respuesta # -------------------------------------

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




#library(ggthemr)
#ggthemr("sky")

# Serie de nro de repeticiones por aglomerado
grafico1 <- individual_NEA %>% 
  mutate(n=1) %>% 
  group_by(periodo, AGLO_DESC, nro_rep) %>% 
  summarise(n=sum(n)) %>% 
  ggplot(aes(periodo, n, fill=as.factor(nro_rep))) +
  geom_bar(stat = "identity", position = "fill", color="white") +
  facet_wrap(.~AGLO_DESC, ncol = 2, scales = "free_x") +
  scale_fill_manual(values=c("#ced2d3","#737373","#22373a","#ff914d")) +
  scale_x_yearqtr(format="%Y-%qT", expand=c(0,0)) + 
  xlab("") + ylab("") + 
  labs(fill="Entrevistas realizadas") +
  theme_light() + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill="#fbfbfb"),
        text = element_text(family="serif"))

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
grafico2 <- 
  ggplot(resultados_pobreza, aes(periodo,hogares_pobres,color=AGLO_DESC)) +
  geom_line(stat="identity") +
  geom_smooth(se=FALSE) +
  theme_light() + 
  ggtitle("Resultados de pobreza") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("#5c6b8a","#a2b8d2","#f07838","#ba4c40")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family="serif")) + 
  xlab("") + ylab("")

# Resultados indigencia
grafico3 <- 
  ggplot(resultados_pobreza, aes(periodo,hogares_indigentes,color=AGLO_DESC)) +
  geom_line(stat="identity") +
  geom_smooth(se=FALSE) + 
  theme_light() +
  ggtitle("Resultados de indigencia") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("#5c6b8a","#a2b8d2","#f07838","#ba4c40")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text=element_text(family="serif")) +
  xlab("") + ylab("") 

library(patchwork)

# ! Exportar gráficos 2 y 3 con resultados de pobreza e indigencia


## MMultinomial para nro rep #### 
library(mclogit)
library(VGAM)

# Modelos para probabilidad de respuesta del hogar
# Modelo para Gran Resistencia
glm.multi.rcia <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Gran Resistencia"))
summary(glm.multi.rcia)

# # Test para evaluar bondad de ajuste
# library(ResourceSelection)
# ResourceSelection::hoslem.test(predict(glm.multi.rcia, type="link"), 
#                                ordered(individual_NEA$nro_rep[individual_NEA$AGLO_DESC=="Gran Resistencia"]))


# Modelo para Corrientes
glm.multi.ctes <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Corrientes"))

# Modelo para Posadas
glm.multi.psdas <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido, 
                        family = cumulative(parallel = TRUE),
                        data = subset(individual_NEA, AGLO_DESC=="Posadas"))

# Modelo para Formosa
glm.multi.fmsa <- vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido, 
                       family = cumulative(parallel = TRUE),
                       data = subset(individual_NEA, AGLO_DESC=="Formosa"))

# Tabla resumen del modelo
library(modelsummary)
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



# Bootstrap para estimación de coeficientes # ----------------------------------
library(boot)

# Modelo Gran Resistencia
# Función para calcular el coeficiente de logIPCF_d
funcion_coef_multinomial <- function(data, i){
  data = subset(individual_NEA, AGLO_DESC=="Gran Resistencia")
  data = data[i]
  modelo = vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido,
                family = cumulative(parallel = TRUE),
                data = data)
  exp(coefficients(modelo))
}
# Corremos bootstrap
res.boot.GR <- boot(individual_NEA, funcion_coef_multinomial, R=1000)
res.boot.GR

# Tabla vacía para guardar resultados
glm.rcia.boot <- data.frame(coeficiente=names(coef(glm.multi.rcia)),
                            original=rep(NA,9),
                            bias=rep(NA,9),
                            std.error=rep(NA,9),
                            lim.inf=rep(NA,9),
                            lim.sup=rep(NA,9))

# Anexamos los resultados a la tabla
for (i in 1:9){
  # Intervalo de confianza con percentiles
  coef.ci.GR <- boot.ci(res.boot.GR, type="perc",t0=res.boot.GR$t0[i], t=res.boot.GR$t[,i])
  glm.rcia.boot$original[i] = coef.ci.GR$t0
  glm.rcia.boot$bias[i] = sd(res.boot.GR$t[,i])
  glm.rcia.boot$std.error[i] = mean(res.boot.GR$t[,i]) - coef.ci.GR$t0
  glm.rcia.boot$lim.inf[i] <- coef.ci.GR$percent[4]
  glm.rcia.boot$lim.sup[i] <- coef.ci.GR$percent[5]
}

# Agregamos el nombre del aglomerado
glm.rcia.boot$Aglomerado="Gran Resistencia"

# Modelo Corrientes
# Función para calcular el coeficiente de logIPCF_d
funcion_coef_multinomial <- function(data, i){
  data = subset(individual_NEA, AGLO_DESC=="Corrientes")
  data = data[i]
  modelo = vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido,
                family = cumulative(parallel = TRUE),
                data = data)
  coefficients(modelo)
}
# Corremos bootstrap
res.boot.Ctes <- boot(individual_NEA, funcion_coef_multinomial, R=1000)
res.boot.Ctes

# Tabla vacía para guardar resultados
glm.ctes.boot <- data.frame(coeficiente=names(coef(glm.multi.ctes)),
                            original=rep(NA,9),
                            bias=rep(NA,9),
                            std.error=rep(NA,9),
                            lim.inf=rep(NA,9),
                            lim.sup=rep(NA,9))

# Anexamos los resultados a la tabla
for (i in 1:9){
  # Intervalo de confianza con percentiles
  coef.ci.Ctes <- boot.ci(res.boot.Ctes, type="perc",t0=res.boot.Ctes$t0[i], t=res.boot.Ctes$t[,i])
  glm.ctes.boot$original[i] = coef.ci.Ctes$t0
  glm.ctes.boot$bias[i] = sd(res.boot.Ctes$t[,i])
  glm.ctes.boot$std.error[i] = mean(res.boot.Ctes$t[,i]) - coef.ci.Ctes$t0
  glm.ctes.boot$lim.inf[i] <- coef.ci.Ctes$percent[4]
  glm.ctes.boot$lim.sup[i] <- coef.ci.Ctes$percent[5]
}

# Agregamos el nombre del aglomerado
glm.ctes.boot$Aglomerado="Corrientes"


# Modelo Posadas
# Función para calcular el coeficiente de logIPCF_d
funcion_coef_multinomial <- function(data, i){
  data = subset(individual_NEA, AGLO_DESC=="Posadas")
  data = data[i]
  modelo = vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadounido,
                family = cumulative(parallel = TRUE),
                data = data)
  coefficients(modelo)
}
# Corremos bootstrap
res.boot.Psdas <- boot(individual_NEA, funcion_coef_multinomial, R=1000)
res.boot.Psdas

# Tabla vacía para guardar resultados
glm.psdas.boot <- data.frame(coeficiente=names(coef(glm.multi.psdas)[-8]),
                             original=rep(NA,8),
                             bias=rep(NA,8),
                             std.error=rep(NA,8),
                             lim.inf=rep(NA,8),
                             lim.sup=rep(NA,8))

# Anexamos los resultados a la tabla
for (i in 1:8){
  # Intervalo de confianza con percentiles
  coef.ci.Psdas <- boot.ci(res.boot.Psdas, type="perc",t0=res.boot.Psdas$t0[i], t=res.boot.Psdas$t[,i])
  glm.psdas.boot$original[i] = coef.ci.Psdas$t0
  glm.psdas.boot$bias[i] = sd(res.boot.Psdas$t[,i])
  glm.psdas.boot$std.error[i] = mean(res.boot.Psdas$t[,i]) - coef.ci.Psdas$t0
  glm.psdas.boot$lim.inf[i] <- coef.ci.Psdas$percent[4]
  glm.psdas.boot$lim.sup[i] <- coef.ci.Psdas$percent[5]
}

# Agregamos el nombre del aglomerado
glm.psdas.boot$Aglomerado="Posadas"


# Modelo Formosa
# Función para calcular el coeficiente de logIPCF_d
funcion_coef_multinomial <- function(data, i){
  data = subset(individual_NEA, AGLO_DESC=="Formosa")
  data = data[i]
  modelo = vglm(formula = ordered(nro_rep) ~ logIPCF_d + CH06 + I(CH06^2) + IX_TOT + casadpto + casadounido,
                family = cumulative(parallel = TRUE),
                data = data)
  coefficients(modelo)
}
# Corremos bootstrap
res.boot.Fmsa <- boot(individual_NEA, funcion_coef_multinomial, R=1000)
res.boot.Fmsa

# Tabla vacía para guardar resultados
glm.fmsa.boot <- data.frame(coeficiente=names(coef(glm.multi.fmsa)),
                            original=rep(NA,9),
                            bias=rep(NA,9),
                            std.error=rep(NA,9),
                            lim.inf=rep(NA,9),
                            lim.sup=rep(NA,9))

# Anexamos los resultados a la tabla
for (i in 1:9){
  # Intervalo de confianza con percentiles
  coef.ci.Fmsa <- boot.ci(res.boot.Fmsa, type="perc",t0=res.boot.Fmsa$t0[i], t=res.boot.Fmsa$t[,i])
  glm.fmsa.boot$original[i] = coef.ci.Fmsa$t0
  glm.fmsa.boot$bias[i] = sd(res.boot.Fmsa$t[,i])
  glm.fmsa.boot$std.error[i] = mean(res.boot.Fmsa$t[,i]) - coef.ci.Fmsa$t0
  glm.fmsa.boot$lim.inf[i] <- coef.ci.Fmsa$percent[4]
  glm.fmsa.boot$lim.sup[i] <- coef.ci.Fmsa$percent[5]
}

# Agregamos el nombre del aglomerado
glm.fmsa.boot$Aglomerado="Formosa"

# Anexamos todas las tablas
tabla_boot <- rbindlist(list(glm.rcia.boot[-c(1:3),], glm.ctes.boot[-c(1:3),], 
                             glm.fmsa.boot[-c(1:3),], glm.psdas.boot[-c(1:3),]),
                        use.names = TRUE, fill = TRUE)

tabla_boot$bootstrap = exp(tabla_boot$original+tabla_boot$bias)
tabla_boot$original = exp(tabla_boot$original)
tabla_boot$lim.inf = exp(tabla_boot$lim.inf)
tabla_boot$lim.sup = exp(tabla_boot$lim.sup)

# Formateamos la tabla
tabla_boot <- tabla_boot %>% 
  select(coeficiente, original, bootstrap, lim.inf, lim.sup, Aglomerado) %>% 
  mutate(original = format(original, digits=3), 
         bootstrap = format(bootstrap, digits=3),
         lim.inf = format(lim.inf, digits=3), 
         lim.sup = format(lim.sup, digits=3))

# Exportamos los resultados bootstrap
save(res.boot.GR, res.boot.Ctes, res.boot.Fmsa, res.boot.Psdas, tabla_boot, 
     file="Informe y resultados/Resultados_bootstrap.RData")



# Modelos alternativos para probabilidades predichas -----------------------------

library(caret)       # Para matriz de confusión y validación cruzada
library(rpart)       # Árbol de decisión
library(rpart.plot)  # Plot árbol de decisións
library(stats)       # Medidas de bondad de clasificación
library(ROCR)        # Para curvas ROC y AUC
library(performance) # Para curva ROC

# Indicadora de esquema completo (responde 4 veces)
individual_NEA[, completo:= ifelse(nro_rep==4, 1, 0)]

# Anexamos variables de la base hogar (características de la vivienda)
individual_NEA <- merge.data.table(individual_NEA, hogar_NEA[,c(3:6,13,14,16:19,21:24,26,27:39,41,43)],
                                   by = c("ANO4","TRIMESTRE","CODUSU","NRO_HOGAR"), all.x=T)

# Base para jupyternotebook
fwrite(individual_NEA, "Bases/individual_NEA_prediccion.txt", sep=";",encoding = "UTF-8")

# Seleccionamos las variables que vamos a utilizar en los modelos

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

# Muestra de entrenamiento
individual_NEA_train <- individual_NEA_train %>% 
  select(completo, # Variable de respuesta
         # Variables de identificación del hogar
         AGLO_DESC,
         # Ingreso del hogar
         IPCF_d, otros_ing_nolab, RDECOCUR, ADECOCUR,
         # Datos del jefe de hogar
         CH06, CH06_2, IX_TOT, informal, NIVEL_ED, mujer, 
         casadounido, ESTADO, CAT_INAC, CAT_OCUP,
         # Características de la vivienda
         IV1, IV2, IV3, IV4, IV5, IV6, IV7, IV8, IV9, IV10, IV11, IV12_1, IV12_2, IV12_3, 
         # Características habitacionales del hogar
         II1, II2, II3, II3_1, II4_1, II4_2, II4_3, II5, II6, II6_1, II7, II8, II9, 
         # Ocupación del jefe de hogar
         CALIFICACION, JERARQUIA, caes_seccion_cod, PP04B1,
         # Características de composición del hogar
         cantidad_varones, cantidad_mujeres, cantidad_ocupados, 
         cantidad_desocupados, cantidad_informales, edad_promedio_hogar,
         # Medidas por área
         prop_ocupados_area, prop_desocup_area, prop_informal_area)


# 1) Modelo logístico # --------------------

# Stepwise
modelofull <- glm(completo~., data=individual_NEA_train)
modelonull <- glm(completo~1, data=individual_NEA_train)

# Cantidad posible de modelos (841)
steplogit <- step(modelonull,
                  scope = list(lower=modelonull, upper=modelofull),
                  direction = "forward")
steplogit$formula

modelo_logit <- glm(formula=steplogit$formula, 
                    data=individual_NEA_train, 
                    family = binomial(link="logit"))
summary(modelo_logit)

# Clases predichas
individual_NEA_test$pclass_logit <- ifelse(predict.glm(modelo_logit, 
                                                       newdata = individual_NEA_test, 
                                                       type = "response")>0.6, 1, 0)

# Matriz de confusión
cm_logit <- confusionMatrix(table(individual_NEA_test$completo,
                                  individual_NEA_test$pclass_logit), 
                            positive="1")
cm_logit

# Curva ROC
pred.logit <- ROCR::prediction(individual_NEA_test$pclass_logit, individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.logit <- ROCR::performance (pred.logit, measure="tpr", x.measure="fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.logit, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Guardamos medidas de la clasificación
medidas <- data.frame()
medidas <- rbind(medidas,
                 data.frame(Modelo="Logístico",
                            accuracy = round(cm_logit$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_logit$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_logit$byClass[["Specificity"]], 5),
                            VPP = round(cm_logit$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_logit$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.logit, measure = "auc")@y.values[[1]], 5)))

# Prediction error
# Cross validation
cv_error <- cv.glm(data=individual_NEA_train, glmfit = modelo_logit, K = 10)
cv_error$delta

# Leave-One-Out (manual con los hatvalues, en caso de regresión logística)
muhat <- fitted(modelo_logit)
modelo.diag <- boot::glm.diag(modelo_logit)
cv.error <- mean((modelo_logit$y - muhat)^2/(1-modelo.diag$h)^2)
cv.error
medidas$cv.error <- cv.error

# Cross validation (Leave-One-Out)
# cv_error <- cv.glm(data=individual_NEA_train, glmfit = modelo_logit)
# cv_error$delta


# 2) Árbol de decisión # ----------------

# Ajuste
modelo_rpart <- rpart(completo ~ ., data = individual_NEA_train, control = rpart.control(cp=0))

# Poda del árbol según relación costo-complejidad 
printcp(modelo_rpart)
plotcp(modelo_rpart)
modelo_rpart_podado <- prune(modelo_rpart, cp=6.2333e-04)
prp(modelo_rpart_podado, extra=101, type=2,  xsep="/") 

# Clases predichas
individual_NEA_test$pclass_cart <- predict(modelo_rpart_podado, newdata=individual_NEA_test)
individual_NEA_test$pclass_cart <- ifelse(individual_NEA_test$pclass_cart>0.5, 1, 0)

# Probabilidades predichas
# individual_NEA_test$pprob_cart <- predict(modelo_rpart_podado, newdata=individual_NEA_test, type="prob")

# Matriz de confusión
cm_rpart <- confusionMatrix(table(individual_NEA_test$completo, individual_NEA_test$pclass_cart), positive="1")
cm_rpart

# Curva ROC
pred.cart <- ROCR::prediction(as.numeric(individual_NEA_test$pclass_cart), individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.cart <- ROCR::performance (pred.cart, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.cart, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Variables de importancia
modelo_rpart_podado$variable.importance

# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Árbol CART",
                            accuracy = round(cm_rpart$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_rpart$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_rpart$byClass[["Specificity"]], 5),
                            VPP = round(cm_rpart$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_rpart$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.cart, measure = "auc")@y.values[[1]], 5)))


# 3) Random Forest # --------------
library(randomForest)

individual_NEA_train$completo_f <- as.factor(individual_NEA_train$completo)
individual_NEA_test$completo_f <- as.factor(individual_NEA_test$completo)
individual_NEA$completo_f <- as.factor(individual_NEA$completo)

# Ajustamos el modelo
set.seed(123)
modelo_rf <- randomForest(completo_f ~ . - completo, 
                          data = individual_NEA_train, 
                          importance = T, 
                          na.action = na.omit)

# Matriz de confusión
individual_NEA_test$pclass_rf <- predict(modelo_rf, newdata=individual_NEA_test)
cm_rf <- confusionMatrix(table(individual_NEA_test$completo_f, individual_NEA_test$pclass_rf), positive = "1")
cm_rf

# Variables de importancia
data.frame(variables = rownames(modelo_rf$importance), 
           importancia = modelo_rf$importance[,4]) %>% 
  ggplot(aes(x=importancia,y=reorder(variables,importancia))) + 
  geom_bar(stat = "identity")

# Curva ROC
pred.rf <- ROCR::prediction(as.numeric(individual_NEA_test$pclass_rf), individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf <- ROCR::performance (pred.rf, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.rf, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Óptimo punto de corte
# Buscar

save(modelo_rf, file="Informe y resultados/Modelos_clasificacion.RData")


# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            accuracy = round(cm_rf$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_rf$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_rf$byClass[["Specificity"]], 5),
                            VPP = round(cm_rf$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_rf$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.rf, measure = "auc")@y.values[[1]], 5),
                            cv.error=NA))

# Validación cruzada
Y <- individual_NEA_train$completo
X <- individual_NEA_train[,-"completo"]
cv_error_rf <- rfcv(X,Y,cv.fold=10)
cv_error_rf


# XGBoost
library(xgboost)

individual_NEA_train1 <- individual_NEA_train %>% 
  dplyr::select(logIPCF_d, CH06, IX_TOT, casadpto, basural, leer, NIVEL_ED, mujer, casadounido) 

# Muestra de entrenamiento
# Transformar los factores en variables numéricas
individual_NEA_train1 <- map_df(individual_NEA_train, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

# Tranformar datos en formato DMatrix
individual_NEA_train1 <- individual_NEA_train1%>% 
  select(-completo) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = individual_NEA_train$completo)
individual_NEA_train1

# Muestra de testeo
# Transformar los factores en variables numéricas
individual_NEA_test1 <- map_df(individual_NEA_test, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
# Tranformar datos en formato DMatrix
variables_train <- names(individual_NEA_train)
individual_NEA_test1 <- individual_NEA_test1%>% 
  select(all_of(variables_train)) %>% 
  select(-completo) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = individual_NEA_test$completo)
individual_NEA_test1

# Ajustamos el modelo
modelo_xgboost <- xgboost(data = individual_NEA_train1, 
                          objective = "binary:logistic",
                          nrounds = 1000, max.depth = 2, eta = 0.3, nthread = 2)

# Matriz de confusión
individual_NEA_test$pclass_xgb <- predict(modelo_xgboost, individual_NEA_test1)
individual_NEA_test$pclass_xgb <- ifelse(individual_NEA_test$pclass_xgb>0.5, 1, 0)

cm_xgb <- confusionMatrix(table(individual_NEA_test$completo, individual_NEA_test$pclass_xgb), positive = "1")
cm_xgb

# Curva ROC
pred.xgb <- ROCR::prediction(as.numeric(individual_NEA_test$pclass_xgb), individual_NEA_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.xgb <- ROCR::performance (pred.xgb, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.xgb, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia


# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="XGBoost",
                            accuracy = round(cm_xgb$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_xgb$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_xgb$byClass[["Specificity"]], 5),
                            VPP = round(cm_xgb$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_xgb$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.xgb, measure = "auc")@y.values[[1]], 5)))



# Ridge regression and Lasso (para reducir varianza de las estimaciones)
library(glmnet)

# Definimos variables con conjunto de entrenamiento
x <- individual_NEA_train[,-"completo_f"]
x = model.matrix(completo ~ ., data = x)[,-1]
y = individual_NEA_train$completo

# Filtramos la base de testeo y generamos los objetos para predicción
regresores = colnames(individual_NEA_train)
x_test = individual_NEA_test %>% select(all_of(regresores),-completo_f)
x_test = model.matrix(completo ~ ., data = x_test)[,-1]
y_test = individual_NEA_test$completo

# Ridge regression
grid <- 10^seq(3,-1,length=100)
ridge.mod <- glmnet(x, y, family = "binomial", alpha = 0)
# Nos da una matriz 44x100 con 44 filas por cada variable y 100 columnas por cada lambda
dim(coef(ridge.mod))
plot(ridge.mod, xvar="lambda", label=TRUE)


# Calculamos el ECM
resultados <- data.frame()
for(i in grid){
  ridge.pred <- predict(ridge.mod, s=i, newx=x_test, type="response")
  tabla <- data.frame(y.test = y_test, y.pred = ifelse(as.numeric(ridge.pred)>0.5,1,0))
  bias2 <- (mean(tabla$y.pred) - mean(tabla$y.test))^2
  var <- var(tabla$y.pred)
  mse <- mean((tabla$y.test-tabla$y.pred)^2)
  resultados <- rbind(resultados, data.frame(loglambda=log(i), bias2=bias2, var=var, mse=mse))
  rm(ridge.pred, tabla, bias2, var, mse)
}

ggplot(resultados) + 
  geom_line(aes(loglambda, var, color="Var")) + 
  geom_line(aes(loglambda, mse, color="MSE")) + 
  geom_line(aes(loglambda, bias2, color="Bias")) +
  geom_point(data=resultados[resultados$mse==min(resultados$mse),][1,], 
             aes(loglambda, mse, color="MSE"), shape=4) +
  theme_light()
  
  

ridge.mod$lambda
ridge.mod$dev.ratio

# Validación cruzada
# Con Error cuadrático medio
cvfit.mse <- cv.glmnet(x = x, y = y, type.measure = "mse", alpha=0, family="binomial")
plot(cvfit.mse)
log(cvfit.mse$lambda.min)

cvfit.mse

# Con deviance
cvfit.dev <- cv.glmnet(x = x, y = y, type.measure = "deviance", alpha=0, family="binomial")
plot(cvfit.dev)
log(cvfit.dev$lambda.min)

# AUC
cvfit.auc <- cv.glmnet(x = x, y = y, type.measure = "auc", alpha=0, family="binomial")
plot(cvfit.auc)
log(cvfit.auc$lambda.min)

# Misclassification error
cvfit.error <- cv.glmnet(x = x, y = y, type.measure = "class", alpha=0, family="binomial")
plot(cvfit.error)
log(cvfit.error$lambda.min)


# Varianza

individual_NEA_test[,-c("completo","completo_f")]

predict(fit, newx=individual_NEA_test[,-c("completo","completo_f")])

# Lasso
grid <- 10^seq(3,-2,length=100)
lasso.mod <- glmnet(x, y, family = "binomial", alpha = 1, lambda = grid)
# Nos da una matriz 44x100 con 44 filas por cada variable y 100 columnas por cada lambda
dim(coef(ridge.mod))
plot(lasso.mod, xvar="lambda", label=TRUE)


# Calculamos el ECM
resultados <- data.frame()
for(i in grid){
  lasso.pred <- predict(lasso.mod, s=i, newx=x_test, type="response")
  tabla <- data.frame(y.test = y_test, y.pred = ifelse(as.numeric(lasso.pred)>0.5,1,0))
  bias2 <- (mean(tabla$y.pred) - mean(tabla$y.test))^2
  var <- var(tabla$y.pred)
  mse <- mean((tabla$y.test-tabla$y.pred)^2)
  resultados <- rbind(resultados, data.frame(loglambda=log(i), bias2=bias2, var=var, mse=mse))
  rm(lasso.pred, tabla, bias2, var, mse)
}

resultados %>% 
  filter(loglambda<0) %>% 
  ggplot() + 
  geom_line(aes(loglambda, var, color="Var")) + 
  geom_line(aes(loglambda, mse, color="MSE")) + 
  geom_line(aes(loglambda, bias2, color="Bias")) +
  geom_point(data=resultados[resultados$mse==min(resultados$mse),][1,], 
             aes(loglambda, mse, color="MSE"), shape=4) +
  theme_light()


# Validación cruzada
# Con Error cuadrático medio
cvfit.mse <- cv.glmnet(x = x, y = y, type.measure = "mse", alpha=1, family="binomial")
plot(cvfit.mse)
log(cvfit.mse$lambda.min)

cvfit.mse

# Con deviance
cvfit.dev <- cv.glmnet(x = x, y = y, type.measure = "deviance", alpha=1, family="binomial")
plot(cvfit.dev)
log(cvfit.dev$lambda.min)

# AUC
cvfit.auc <- cv.glmnet(x = x, y = y, type.measure = "auc", alpha=1, family="binomial")
plot(cvfit.auc)
log(cvfit.auc$lambda.min)


# Al momento de elegir el mejor modelo (al final)
# Guardamos las probabilidades predichas
load("Informe y resultados/Modelos_clasificacion.RData") # Provisoriamente
individual_NEA$prob_completo <- predict(modelo_rf, newdata=individual_NEA, type="prob")[,2]
hist(individual_NEA$prob_completo)

ggplot(individual_NEA, aes(nro_rep, prob_completo)) + 
  geom_boxplot() + 
  facet_wrap(.~AGLO_DESC)

# Corrección del ponderador

# Algunos gráficos descriptivos previos para Gran Resistencia
# Vivienda relevadas por área y período
individual_NEA %>% 
  filter(AGLO_DESC=="Gran Resistencia") %>% 
  group_by(periodo, area) %>% 
  summarise(viviendas = n_distinct(CODUSU)) %>% 
  ggplot(aes(as.factor(periodo), viviendas)) + 
  geom_boxplot()

# PONDIH según área y período
individual_NEA %>% 
  filter(AGLO_DESC=="Gran Resistencia") %>% 
  filter(!(periodo %in% c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2020 Q2", "2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2"))) %>% 
  ggplot(aes(area, PONDIH)) + 
  geom_point() +
  facet_wrap(.~periodo)

# Viviendas relevadas por área
individual_NEA %>% 
  filter(AGLO_DESC=="Gran Resistencia") %>% 
  filter(!(periodo %in% c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2020 Q2", "2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2"))) %>% 
  group_by(periodo, area) %>% 
  summarise(viviendas = n_distinct(CODUSU)) %>% 
  ggplot(aes(area, viviendas)) +
  geom_boxplot()

# Viviendas por área
individual_NEA[, viviendas:= n_distinct(CODUSU), by = .(periodo, area, AGLOMERADO)]
hist(individual_NEA$viviendas)
summary(individual_NEA$viviendas)

# Agregamos el total de PONDIH por 'área'
individual_NEA[, mj:= sum(PONDIH,na.rm=T), by = .(periodo, area, AGLOMERADO)]
hist(individual_NEA$mj)
summary(individual_NEA$mj)

# Ponderador corregido
individual_NEA[, media_prob:= mean(prob_completo,na.rm=T), by = .(periodo, area, AGLOMERADO)]
individual_NEA[, PONDIH_c:= round(ifelse(prob_completo>0, PONDIH/(prob_completo/media_prob), PONDIH))]
individual_NEA[, mj_c:=sum(PONDIH_c), by = .(periodo, area, AGLOMERADO)]
individual_NEA[, PONDIH_c:= round(PONDIH_c*(mj/mj_c))]
hist(individual_NEA$PONDIH)
hist(individual_NEA$PONDIH_c, add=T, col=2)

# PONDIH vs PONDIH corregido
ggplot(individual_NEA, aes(PONDIH, PONDIH_c)) +
  geom_point()

# PONDIH corregido
individual_NEA %>% 
  filter(!(periodo %in% c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2020 Q2", "2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2"))) %>% 
  ggplot(aes(area, PONDIH_c)) + 
  geom_point() +
  geom_point(aes(area, PONDIH), color=2) +
  facet_wrap(.~periodo)


summary(individual_NEA$PONDIH)
summary(individual_NEA$PONDIH_c)
sd(individual_NEA$PONDIH)
sd(individual_NEA$PONDIH_c)

# Controlamos los totales poblacionales
control_poblacion <- cbind(
  individual_GR[, .(total_poblacion= sum(PONDIH,na.rm=T)), by = .(periodo)],
  individual_GR[, .(total_poblacion= sum(PONDIH_c,na.rm=T)), by = .(periodo)]
); control_poblacion


# Tablas de frecuencia de nro de repeticiones
library(expss)
fre(individual_GR$nro_rep, weight = individual_GR$PONDIH)
fre(individual_GR$nro_rep, weight = individual_GR$PONDIH_c)



## Distribuciones de ingreso ####------------------------------------------------

# 1) Percentil del ingreso per cápita familiar por aglomerado (original, con PONDIH)

# Combinaciones de aglomerados y períodos
aglo_periodos <- individual_NEA %>% select(periodo, AGLO_DESC) %>% distinct()

# Tabla para guardar resultados
tabla1 <- data.table()

# Iteramos por aglomerado y período 
for (i in 1:nrow(aglo_periodos)){
  periodo_selec = aglo_periodos$periodo[i]
  aglo_selec = aglo_periodos$AGLO_DESC[i]
  # Filtramos la base
  base <- individual_NEA %>% 
    filter(AGLO_DESC==aglo_selec & 
             periodo==periodo_selec)
  # Calculamos la densidad
  kde_IPCF <- density(x = base$IPCF, weights = base$PONDIH, 
                      bw = 500, kernel = "gaussian")
  # Guardamos los datos
  anexo <- data.table(AGLO_DESC=rep(aglo_selec, length(kde_IPCF$x)), 
                      periodo=rep(periodo_selec, length(kde_IPCF$x)), 
                      IPCF=kde_IPCF$x, freq=kde_IPCF$y)
  # Calculamos la densidad acumulada
  anexo$cdf <- cumsum(anexo$freq) / sum(anexo$freq)
  # Formateamos el periodo
  anexo$periodo <- as.yearqtr(anexo$periodo)
  # Anexamos a la tabla 1
  tabla1 <- rbind(tabla1, anexo)
  # Limpiamos memoria
  rm(anexo, base)
}


# 2) Percentil del ingreso per cápita familiar por aglomerado (modificado, PONDIH_c)

# Tabla para guardar resultados
tabla2 <- data.table()

# Iteramos por aglomerado y período 
for (i in 1:nrow(aglo_periodos)){
  periodo_selec = aglo_periodos$periodo[i]
  aglo_selec = aglo_periodos$AGLO_DESC[i]
  # Filtramos la base
  base <- individual_NEA %>% 
    filter(AGLO_DESC==aglo_selec & 
             periodo==periodo_selec)
  # Calculamos la densidad
  kde_IPCF <- density(x = base$IPCF, weights = base$PONDIH_c, 
                      bw = 700, kernel = "gaussian")
  # Guardamos los datos
  anexo <- data.table(AGLO_DESC=rep(aglo_selec, length(kde_IPCF$x)), 
                      periodo=rep(periodo_selec, length(kde_IPCF$x)), 
                      IPCF_c=kde_IPCF$x, freq_c=kde_IPCF$y)
  # Calculamos la densidad acumulada
  anexo$cdf_c <- cumsum(anexo$freq) / sum(anexo$freq)
  # Formateamos el periodo
  anexo$periodo <- as.yearqtr(anexo$periodo)
  # Anexamos a la tabla 1
  tabla2 <- rbind(tabla2, anexo)
  # Limpiamos memoria
  rm(anexo, base)
}


# Cortamos las frecuencias a 2 dígitos
tabla1$freq_acum <- round(tabla1$cdf, 2)
tabla2$freq_acum <- round(tabla2$cdf_c, 2)

# Filtramos por el máximo salario por frecuencia
tabla1[, IPCF_prev:= max(IPCF), by = .(AGLO_DESC, periodo, freq_acum)]
tabla_prev <- distinct(tabla1[,c(1,2,6,7)])
tabla2[, IPCF_post:= max(IPCF_c), by = .(AGLO_DESC, periodo, freq_acum)]
tabla_post <- distinct(tabla2[,c(1,2,6,7)])

# Unimos las dos tablas
tabla_diferencia <- merge.data.table(tabla_prev, tabla_post, by = c("AGLO_DESC","periodo","freq_acum"))
tabla_diferencia[, diferencia:= (IPCF_post-IPCF_prev)/IPCF_prev]

# Gráfico de diferencias (Gran Resistencia)
tabla_diferencia %>%
  filter(AGLO_DESC=="Gran Resistencia") %>% 
  filter(freq_acum!=0 & freq_acum!=1) %>% 
  ggplot() +
  geom_line(aes(freq_acum, diferencia), stat = "identity") + 
  # geom_smooth(aes(freq_acum, diferencia)) +
  facet_wrap(.~periodo) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_light() + 
  ylab("") + xlab("")

# Gráfico de diferencias (Corrientes)
tabla_diferencia %>%
  filter(AGLO_DESC=="Corrientes") %>% 
  filter(periodo != "2019 Q4" & freq_acum>=0.05 & freq_acum<=0.95) %>% 
  ggplot() +
  geom_line(aes(freq_acum, diferencia), stat = "identity") + 
  # geom_smooth(aes(freq_acum, diferencia)) +
  facet_wrap(.~periodo) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_light() + 
  ylab("") + xlab("")

# Gráfico de diferencias (Posadas)
tabla_diferencia %>%
  filter(freq_acum>=0.05 & freq_acum<=0.95) %>% 
  ggplot() +
  geom_line(aes(freq_acum, diferencia, color=AGLO_DESC), stat = "identity") + 
  geom_smooth(aes(freq_acum, diferencia, color=AGLO_DESC)) +
  # geom_smooth(aes(freq_acum, diferencia)) +
  facet_wrap(.~periodo) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  theme_light() + 
  ylab("") + xlab("")



# Distribuciones acumuladas de IPCF
tabla_diferencia %>% 
  filter(year(periodo)==2018) %>%
  ggplot() + 
  geom_line(aes(IPCF_prev,freq_acum), linetype = "dashed") + 
  geom_line(aes(IPCF_post,freq_acum)) + 
  facet_wrap(.~periodo, scales = "free")

# Pobreza hogares -----------------------------------------------------
# Pobreza con el cambio de ponderador
resultados_post <- 
  individual_NEA[CH03==1,
                 .(hogares_pobres=sum(hogar_pobre*PONDIH_c,na.rm=T),
                   hogares_indigentes=sum(hogar_indigente*PONDIH_c,na.rm=T),
                   total_hogares=sum(PONDIH_c,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados_post[, pobreza:= hogares_pobres/total_hogares]
resultados_post[, indigencia:= hogares_indigentes/total_hogares]

# Pobreza (dato oficial)
resultados_prev <- 
  individual_NEA[CH03==1,
                 .(hogares_pobres=sum(hogar_pobre*PONDIH,na.rm=T),
                   hogares_indigentes=sum(hogar_indigente*PONDIH,na.rm=T),
                   total_hogares=sum(PONDIH,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados_prev[, pobreza:= hogares_pobres/total_hogares]
resultados_prev[, indigencia:= hogares_indigentes/total_hogares]

# Unimos ambas tablas para comparar resultados
resultados_NEA <- resultados_prev %>% 
  dplyr::select(AGLO_DESC, periodo, pobreza, indigencia) %>% 
  arrange(periodo) %>% 
  merge.data.frame(resultados_post[,c(1,2,6,7)], by = c("AGLO_DESC","periodo"))

# Gráfico de resultados de pobreza
resultados_NEA %>% 
  pivot_longer(cols = c(pobreza.x, pobreza.y), names_to = "tipo_pobreza", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_pobreza)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_pobreza)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(.~AGLO_DESC) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  ylab("") + xlab("")

# Gráfico de resultados de indigencia
resultados_NEA %>% 
  pivot_longer(cols = c(indigencia.x, indigencia.y), names_to = "tipo_indigencia", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_indigencia)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_indigencia)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(.~AGLO_DESC) +
  theme_light() + 
  theme(legend.position = "bottom") + 
  ylab("") + xlab("")


# Gráfico con diferencias en resultados de pobreza
resultados_NEA %>% 
  mutate(diferencia = pobreza.y - pobreza.x) %>% 
  ggplot(aes(periodo, diferencia)) + 
  geom_bar(stat = "identity") + 
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~AGLO_DESC)

# Gráfico con diferencias en resultados de indigencia
resultados_NEA %>% 
  mutate(diferencia = indigencia.y - indigencia.x) %>% 
  ggplot(aes(periodo, diferencia)) + 
  geom_bar(stat = "identity") + 
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~AGLO_DESC)

# Test de diferencia de medias
resultados_NEA <- resultados_NEA %>% 
  mutate(diferencia_pobreza = pobreza.y - pobreza.x,
         diferencia_indigencia = indigencia.y - indigencia.x)

# Para pobreza
anova_model <- aov(diferencia_pobreza ~ AGLO_DESC, data = resultados_NEA)
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# Para indigencia
anova_model <- aov(diferencia_indigencia ~ AGLO_DESC, data = resultados_NEA)
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# Pobreza semestral
resultados_NEA %>% 
  mutate(anio=year(periodo),
         semestre=ifelse(quarter(periodo) %in% c(1,2), 1, 2)) %>% 
  mutate(periodo=as.numeric(paste0(anio,semestre))) %>% 
  group_by(AGLO_DESC, periodo) %>% 
  summarise(pobreza.x=mean(pobreza.x), pobreza.y=mean(pobreza.y),
            indigencia.x=mean(indigencia.x), indigencia.y=mean(indigencia.y)) %>% 
  pivot_longer(cols = c(pobreza.x, pobreza.y), names_to = "tipo_pobreza", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_pobreza)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_pobreza)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(.~AGLO_DESC) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  ylab("") + xlab("")

# Indigencia semestral
resultados_NEA %>% 
  mutate(anio=year(periodo),
         semestre=ifelse(quarter(periodo) %in% c(1,2), 1, 2)) %>% 
  mutate(periodo=as.numeric(paste0(anio,semestre))) %>% 
  group_by(AGLO_DESC, periodo) %>% 
  summarise(pobreza.x=mean(pobreza.x), pobreza.y=mean(pobreza.y),
            indigencia.x=mean(indigencia.x), indigencia.y=mean(indigencia.y)) %>% 
  pivot_longer(cols = c(indigencia.x, indigencia.y), names_to = "tipo_indigencia", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_indigencia)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_indigencia)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(.~AGLO_DESC) +
  theme_light() + 
  theme(legend.position = "bottom") + 
  ylab("") + xlab("")



# Pobreza individuos # --------------------------------------------
individual_NEA_total <- fread("Bases/individual_NEA.txt")
individual_NEA_total[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]

# Cruzamos para asignar las probabilidades de respuesta
individual_NEA_total <- merge.data.table(individual_NEA_total, 
                                         individual_NEA[,c(3:6,222,252)],
                                         by = c("CODUSU","NRO_HOGAR","AGLOMERADO","periodo"))


# Agregamos el total de PONDIH por 'área'
individual_NEA_total[, mj:= sum(PONDIH,na.rm=T), by = .(periodo, area, AGLOMERADO)]
hist(individual_NEA_total$mj)
summary(individual_NEA_total$mj)

# Ponderador corregido
individual_NEA_total[, media_prob:= mean(prob_completo,na.rm=T), by = .(periodo, area, AGLOMERADO)]
individual_NEA_total[, PONDIH_c:= round(ifelse(prob_completo>0, PONDIH/(prob_completo/media_prob), PONDIH))]
individual_NEA_total[, mj_c:=sum(PONDIH_c), by = .(periodo, area, AGLOMERADO)]
individual_NEA_total[, PONDIH_c:= round(PONDIH_c*(mj/mj_c))]
hist(individual_NEA_total$PONDIH)
hist(individual_NEA_total$PONDIH_c, add=T, col=2)


# Pobreza con el cambio de ponderador
resultados_post <- 
  individual_NEA[,
                 .(pobres=sum(hogar_pobre*PONDIH_c,na.rm=T),
                   indigentes=sum(hogar_indigente*PONDIH_c,na.rm=T),
                   total_personas=sum(PONDIH_c,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados_post[, pobreza:= pobres/total_personas]
resultados_post[, indigencia:= indigentes/total_personas]

# Pobreza (dato oficial)
resultados_prev <- 
  individual_NEA[,
                 .(pobres=sum(hogar_pobre*PONDIH,na.rm=T),
                   indigentes=sum(hogar_indigente*PONDIH,na.rm=T),
                   total_personas=sum(PONDIH,na.rm=T)),
                 by = .(AGLO_DESC, periodo)]
resultados_prev[, pobreza:= pobres/total_personas]
resultados_prev[, indigencia:= indigentes/total_personas]

# Unimos ambas tablas para comparar resultados
resultados_NEA <- resultados_prev %>% 
  dplyr::select(AGLO_DESC, periodo, pobreza, indigencia) %>% 
  arrange(periodo) %>% 
  merge.data.frame(resultados_post[,c(1,2,6,7)], by = c("AGLO_DESC","periodo"))

# Gráfico de resultados de pobreza
resultados_NEA %>% 
  pivot_longer(cols = c(pobreza.x, pobreza.y), names_to = "tipo_pobreza", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_pobreza)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_pobreza)) +
  facet_wrap(.~AGLO_DESC)


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
save(tabla1, tabla2, grafico1, grafico2, grafico3, modelos_panel, modelos.glm.multi,
     file = "Informe y resultados/Resultados.RData")



