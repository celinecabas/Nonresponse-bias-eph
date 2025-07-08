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

# Medida por área de representación de los hogares
hogar_NEA[, area:= substr(CODUSU, 1, 8)]
hogar_NEA[, PONDERA_repr:= sum(PONDERA)/n_distinct(paste0(CODUSU,NRO_HOGAR)),
          by = .(periodo, AGLOMERADO, AGLO_DESC, area)]

hist(hogar_NEA$PONDERA_repr)

# Individual
individual_NEA <- fread("Bases/individual_NEA.txt")
individual_NEA[, periodo:= as.yearqtr(paste0(ANO4,"-",TRIMESTRE))]

setnames(hogar_NEA, "CBA", "CBA_hogar")
setnames(hogar_NEA, "CBT", "CBT_hogar")
individual_NEA <- merge.data.table(individual_NEA, hogar_NEA[,c(2:6,11,25,66,67,91:92,98,101)],
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

# Representación de hogares dentro del área
individual_NEA[, represent:= sum(PONDERA)/n_distinct(CODUSU), by = .(area)]

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
  pivot_wider(names_from = "nro_rep", values_from = "prop"); prop_por_periodo

# Quitamos un año al comienzo y al final + período de pandemia
# 2016 Q2, 2016 Q3, 2016 Q4, 2017 Q1, 2020 Q2, 2023 Q3, 2023 Q4, 2024 Q1, 2024 Q2


# Filtramos la base de datos por los períodos a analizar
individual_NEA <- subset(individual_NEA, 
                         !(periodo %in% c("2016 Q2", "2016 Q3", "2016 Q4", "2017 Q1", "2020 Q2", "2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3")) & 
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
grafico1

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
grafico2

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
grafico3

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
library(LongituRF)   # Para Random Forest con datos longitudinales.

# Indicadora de esquema completo (responde 4 veces)
individual_NEA[, completo:= ifelse(nro_rep==4, 1, 0)]

# Anexamos variables de la base hogar (características de la vivienda)
individual_NEA <- merge.data.table(individual_NEA, hogar_NEA[,c(3:6,13,14,16:19,21:24,26,27:39,41,43)],
                                   by = c("ANO4","TRIMESTRE","CODUSU","NRO_HOGAR"), all.x=T)

# Formateamos las variables
individual_NEA <- individual_NEA %>% 
  mutate(across(c(informal,mujer,casadounido,ESTADO,CAT_INAC,CAT_OCUP,caes_seccion_cod), as.factor))

# Agregamos variables por área y período
individual_NEA[, viviendasxarea:= n_distinct(CODUSU), 
  by = .(periodo, AGLO_DESC, AGLOMERADO, area)]
hist(individual_NEA$viviendasxarea)
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
         AGLO_DESC, periodo, PONDERA_repr, viviendasxarea,
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
         cantidad_desocupados, cantidad_informales, edad_promedio_hogar)

#individual_NEA_train <- individual_NEA_train |> filter(n_entrevista==1)
#individual_NEA_train$n_entrevista=NULL

# Probamos ajustando un modelo por aglomerado
# Gran Resistencia
individual_GR_test <- individual_NEA_test[AGLO_DESC=="Gran Resistencia"]
individual_GR_train <- individual_NEA_train[AGLO_DESC=="Gran Resistencia"]

# Corrientes
individual_CT_test <- individual_NEA_test[AGLO_DESC=="Corrientes",]
individual_CT_train <- individual_NEA_train[AGLO_DESC=="Corrientes"]

# Formosa
individual_FM_test <- individual_NEA_test[AGLO_DESC=="Formosa",]
individual_FM_train <- individual_NEA_train[AGLO_DESC=="Formosa"]

# Posadas
individual_PS_test <- individual_NEA_test[AGLO_DESC=="Posadas",]
individual_PS_train <- individual_NEA_train[AGLO_DESC=="Posadas"]





# 1) Modelo logístico --------------------

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

# Probabilidad predicha
individual_NEA_test$pprob_logit <- predict.glm(modelo_logit, newdata = individual_NEA_test)

# Óptimo punto de corte
# Calcular curva ROC
roc_obj <- pROC::roc(individual_NEA_test$completo, individual_NEA_test$pprob_logit)

# Encontrar el umbral óptimo con el índice de Youden
youden_index <- which.max(roc_obj$sensitivities + roc_obj$specificities - 1)
optimal_threshold <- roc_obj$thresholds[youden_index]


# Clases predichas
individual_NEA_test$pclass_logit <- ifelse(predict.glm(modelo_logit, 
                                                       newdata = individual_NEA_test, 
                                                       type = "response")>0.47, 1, 0)

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




# 2. Árbol de decisión  ----------------

# Variables tipo factor
# NEA
individual_NEA_train$completo_f <- as.factor(individual_NEA_train$completo)
individual_NEA_test$completo_f <- as.factor(individual_NEA_test$completo)
individual_NEA$completo_f <- as.factor(individual_NEA$completo)

# Gran Resistencia
individual_GR_train$completo_f <- as.factor(individual_GR_train$completo)
individual_GR_test$completo_f <- as.factor(individual_GR_test$completo)

# Corrientes
individual_CT_train$completo_f <- as.factor(individual_CT_train$completo)
individual_CT_test$completo_f <- as.factor(individual_CT_test$completo)

# Formosa
individual_FM_train$completo_f <- as.factor(individual_FM_train$completo)
individual_FM_test$completo_f <- as.factor(individual_FM_test$completo)

# Posadas
individual_PS_train$completo_f <- as.factor(individual_PS_train$completo)
individual_PS_test$completo_f <- as.factor(individual_PS_test$completo)

# Modelo agregado para el NEA
# Ajuste
modelo_rpart <- rpart(formula = completo_f ~ ., 
                      data = individual_NEA_train[,-"completo"], 
                      control = rpart.control(cp=0), 
                      method = "class")

# Poda del árbol según relación costo-complejidad 
printcp(modelo_rpart)

##  2.1. Gráfico de relación costo-complejidad --------------------------------
# Convertir cptable a data.frame
cp_df <- modelo_rpart$cptable %>% as.data.frame()
cp0 <- modelo_rpart$cptable[, 1L]
cp_df$CP <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
cp_df$ns <- seq_along(cp_df$nsplit)

# Obtener valores con solo 5 breaks
breaks_x <- pretty(cp_df$ns, n = 5)
breaks_x[1]=1; breaks_x[8]=69

# Asegurar que los labels correspondan a los breaks
labels_x <- cp_df$CP[match(breaks_x, cp_df$ns)]
labels_x <- signif(labels_x, 3)  # Redondear para mejorar legibilidad

# Obtener valores para el eje secundario
breaks_sec <- breaks_x
labels_sec <- cp_df$nsplit[match(breaks_sec, cp_df$ns)]

# Gráfico del parámetro de Costo-Complejidad
grafico1_dt <- 
  ggplot(cp_df, aes(x = ns, y = xerror)) +
  geom_line(color = "#5c6b8a") +
  geom_point(size = 1, color = "#5c6b8a") +
  geom_hline(yintercept = min(cp_df$xerror), linetype = "dashed") +
  scale_x_continuous(
    name = "CP",
    breaks = breaks_x, 
    labels = labels_x,
    sec.axis = sec_axis(
      ~ ., 
      name = "Tamaño del árbol (nodos terminales)",
      breaks = breaks_sec, 
      labels = labels_sec
    )
  ) +
  labs(y = "Xerror") +
  theme_light() + 
  theme(text = element_text(family="serif"))
grafico1_dt

## 2.2. Gráfico de tasa de entrenamiento, testeo y cross validation -------------
cp_table <- modelo_rpart$cptable

# Número de nodos terminales en cada poda
tamaño_arbol <- cp_table[, "nsplit"] + 1  # Agregamos 1 porque un árbol con 0 splits tiene 1 nodo
cp <- cp_table[, "CP"]

# Errores:
error_train <- c()    # Train error
error_cv <- list()      # CV error
error_test <- c()     # Test error

# 1) Train error para cada poda del árbol
for (i in seq_along(tamaño_arbol)) {
  modelo_podado <- prune(modelo_rpart, cp = cp_table[i, "CP"])  # Poda
  pred_test <- predict(modelo_podado, individual_NEA_train, type="class")
  error_train[i] <- mean(pred_test != individual_NEA_train$completo_f)  # Tasa de error en test
}

# 2) Test error para cada poda del árbol
for (i in seq_along(tamaño_arbol)) {
  modelo_podado <- prune(modelo_rpart, cp = cp_table[i, "CP"])  # Poda
  pred_test <- predict(modelo_podado, individual_NEA_test, type="class")
  error_test[i] <- mean(pred_test != individual_NEA_test$completo_f)  # Tasa de error en test
}

# 3) Cross-validation error
y <- individual_NEA_train$completo_f
X <- individual_NEA_train[,-c("completo","completo_f")]
folds <- createFolds(y, k=10, list=TRUE)

# Lista para almacenar errores por fold y tamaño de árbol
errores_testeo <- list()

for(i in 1:10){
  test_idx <- folds[[i]]
  train_idx <- setdiff(1:nrow(individual_NEA_train), test_idx)
  
  base_train <- individual_NEA_train[train_idx, ]
  base_test <- individual_NEA_train[test_idx, ]
  
  # Entrenar modelo completo sin poda (cp=0)
  modelo_fold <- rpart(formula = completo_f ~ ., 
                  data = base_train[,-c("completo")], 
                  control = rpart.control(cp=0), 
                  method = "class")
 
  # Extraer valores de CP para podar
  cp_table <- modelo_fold$cptable
  cp_vals <- cp_table[, "CP"]  # Lista de CPs
  
  # Guardar errores en una tabla para este fold
  errores_fold <- numeric(length(cp_vals))
  
  # Iterar sobre cada tamaño del árbol
  for (j in seq_along(cp_vals)){
    modelo_podado <- prune(modelo_fold, cp = cp_vals[j])  # Poda 
    pred_test <- predict(modelo_podado, base_test, type="class")
    
    # Calcular tasa de error en test
    error_base_test <- mean(pred_test != base_test$completo_f)
    errores_fold[j] <- error_base_test
  }
  
  # Guardar errores del fold en la lista
  errores_testeo[[i]] <- errores_fold
  
}

# Convertir la lista a data.frame
errores_df <- do.call(rbind, errores_testeo)
colnames(errores_df) <- paste0("CP_", 1:68)
rownames(errores_df) <- paste0("Fold_", 1:10)

# Mostrar tabla final de errores
print(errores_df)
error_cv <- colMeans(errores_df)

# Crear un dataframe con los errores
df_errores <- data.table(
  Tamaño = tamaño_arbol,
  cp = cp,
  error_train = error_train,
  error_test = error_test,
  error_cv = error_cv[1:68]
)

# Marcamos mínimos en entrenamiento y testeo
df_errores[, minimo_testeo := ifelse(error_test==min(error_test), error_test, NA)]
df_errores[, minimo_cv := ifelse(error_cv==min(error_cv), error_cv, NA)]
df_errores <- df_errores %>% filter(Tamaño > 1 & Tamaño<1000)

# Graficamos las tasas
grafico2_dt <- 
  ggplot(df_errores, aes(x = Tamaño)) +
  geom_line(aes(y = error_train, color = "Entrenamiento"), linewidth = 1) +
  geom_line(aes(y = error_test, color = "Testeo"),linewidth = 1) +
  geom_line(aes(y = error_cv, color = "Validación cruzada"), linewidth = 1) +
  geom_hline(aes(yintercept = min(error_cv)), linetype = 2) + 
  labs(x = "Tamaño del árbol (nodos terminales)",
       y = "Tasa de error") +
  scale_color_manual(values = c("Entrenamiento" = "#ba4c40", 
                                "Testeo" = "#f07838", 
                                "Validación cruzada" = "#5c6b8a")) +
  theme_light() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(family="serif")); grafico2_dt


## 2.3. Poda del árbol según relación costo complejidad ---------------
cp_df$CP[cp_df$xerror==min(cp_df$xerror)]
modelo_rpart_podado <- prune(modelo_rpart, cp = 0.0002668677)


# Clases predichas
individual_NEA_test$pclass_cart <- predict(modelo_rpart_podado, newdata=individual_NEA_test, type="class")

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

# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Árbol CART",
                            accuracy = round(cm_rpart$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_rpart$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_rpart$byClass[["Specificity"]], 5),
                            VPP = round(cm_rpart$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_rpart$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.cart, measure = "auc")@y.values[[1]], 5)))


## 2.4. Variables de importancia ---------------------------------------------
var_importance <- as.data.frame(modelo_rpart$variable.importance) %>%
  tibble::rownames_to_column(var = "Variable") %>%
  rename(Importancia = `modelo_rpart$variable.importance`) %>% 
  arrange(-Importancia) %>% 
  head(15)

ggplot(var_importance, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_light() +
  theme(text = element_text(family="serif")) + xlab("")



# 3. Random Forest  --------------
library(randomForest)

# Ajustamos el modelo
set.seed(123)
modelo_rf <- randomForest(completo_f ~ . - completo, 
                          data = individual_NEA_train, 
                          importance = T, 
                          na.action = na.omit)

# Matriz de confusión
individual_NEA_test$pclass_rf <- predict(modelo_rf, newdata=individual_NEA_test, type="class")
individual_NEA_test$pprob_rf <- predict(modelo_rf, newdata=individual_NEA_test, type="prob")[,2]
cm_rf <- confusionMatrix(table(individual_NEA_test$completo_f, individual_NEA_test$pclass_rf), positive = "1")
cm_rf

# Variables de importancia
data.frame(variables = rownames(modelo_rf$importance)[1:20], 
           importancia = modelo_rf$importance[1:20,4]) %>% 
  ggplot(aes(x=importancia,y=reorder(variables,importancia))) + 
  geom_bar(stat = "identity")

# Curva ROC
library(ROCR)
na.position <- is.na(individual_NEA_test$pprob_rf)
pred.rf <- ROCR::prediction(individual_NEA_test$pprob_rf[na.position==F], as.factor(individual_NEA_test$completo[na.position==F])) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf <- ROCR::performance (pred.rf, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)
plot(perf.rf, main = "Curva ROC", ylab = "Sensibilidad", xlab = "1-especificidad") # grafica la curva ROC
abline(a=0, b=1) # agregamos la recta de referencia

# Óptimo punto de corte
# Buscar

save(modelo_rf, file="Informe y resultados/Modelos_clasificacion.RData")


# Guardamos medidas de la clasificación
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            Aglomerado="NEA",
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

# Probabilidades predichas con XGBoost
individual_NEA$pprob_rf <- predict(modelo_rf, newdata=individual_NEA, type="prob")[,2]

## 3.1. Random Forest. Por aglomerados -------------------

# Gran Resistencia
modelo_rf_GR <- randomForest(completo_f ~ . - completo - AGLO_DESC, 
                             data = individual_GR_train, 
                             importance = T, 
                             na.action = na.omit)

# Corrientes
modelo_rf_CT <- randomForest(completo_f ~ . - completo - AGLO_DESC, 
                             data = individual_CT_train, 
                             importance = T, 
                             na.action = na.omit)

# Formosa
modelo_rf_FM <- randomForest(completo_f ~ . - completo - AGLO_DESC, 
                             data = individual_FM_train, 
                             importance = T, 
                             na.action = na.omit)
# Posadas
modelo_rf_PS <- randomForest(completo_f ~ . - completo - AGLO_DESC, 
                             data = individual_PS_train, 
                             importance = T, 
                             na.action = na.omit)

individual_PS_test <- individual_PS_test |> select(names(individual_PS_train)) |> na.omit()

# Matriz de confusión
# Gran Resistencia
individual_GR_test$pclass_rf <- predict(modelo_rf_GR, newdata=individual_GR_test)
cm_GR_rf <- confusionMatrix(table(individual_GR_test$completo_f, individual_GR_test$pclass_rf), positive = "1")
cm_GR_rf
# Corrientes
individual_CT_test$pclass_rf <- predict(modelo_rf_CT, newdata=individual_CT_test)
cm_CT_rf <- confusionMatrix(table(individual_CT_test$completo_f, individual_CT_test$pclass_rf), positive = "1")
cm_CT_rf
# Formosa
individual_FM_test$pclass_rf <- predict(modelo_rf_FM, newdata=individual_FM_test)
cm_FM_rf <- confusionMatrix(table(individual_FM_test$completo_f, individual_FM_test$pclass_rf), positive = "1")
cm_FM_rf
# Posadas
individual_PS_test$pclass_rf <- predict(modelo_rf_PS, newdata=individual_PS_test)
cm_PS_rf <- confusionMatrix(table(individual_PS_test$completo_f, individual_PS_test$pclass_rf), positive = "1")
cm_PS_rf

# Curva ROC
# Gran Resistencia
pred.rf.GR <- ROCR::prediction(as.numeric(individual_GR_test$pclass_rf), individual_GR_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf.GR <- ROCR::performance (pred.rf.GR, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)

# Corrientes
pred.rf.CT <- ROCR::prediction(as.numeric(individual_CT_test$pclass_rf), individual_CT_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf.CT <- ROCR::performance (pred.rf.CT, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)

# Formosa
pred.rf.FM <- ROCR::prediction(as.numeric(individual_FM_test$pclass_rf), individual_FM_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf.FM <- ROCR::performance (pred.rf.FM, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)

# Posadas
pred.rf.PS <- ROCR::prediction(as.numeric(individual_PS_test$pclass_rf), individual_PS_test$completo) #solo cambia el formato del objeto para que sea soportable por la función performance
perf.rf.PS <- ROCR::performance (pred.rf.PS, "tpr", "fpr") #guarda los valores de TPR (sensibilidad) y FPR (1-especificidad)

# Guardamos medidas de la clasificación
# Gran Resistencia
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            Aglomerado="Gran Resistencia",
                            accuracy = round(cm_GR_rf$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_GR_rf$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_GR_rf$byClass[["Specificity"]], 5),
                            VPP = round(cm_GR_rf$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_GR_rf$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.rf.GR, measure = "auc")@y.values[[1]], 5),
                            cv.error=NA))
# Corrientes
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            Aglomerado="Corrientes",
                            accuracy = round(cm_CT_rf$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_CT_rf$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_CT_rf$byClass[["Specificity"]], 5),
                            VPP = round(cm_CT_rf$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_CT_rf$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.rf.CT, measure = "auc")@y.values[[1]], 5),
                            cv.error=NA))

# Formosa
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            Aglomerado="Formosa",
                            accuracy = round(cm_FM_rf$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_FM_rf$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_FM_rf$byClass[["Specificity"]], 5),
                            VPP = round(cm_FM_rf$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_FM_rf$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.rf.FM, measure = "auc")@y.values[[1]], 5),
                            cv.error=NA))

# Posadas
medidas <- rbind(medidas,
                 data.frame(Modelo="Random Forest",
                            Aglomerado="Posadas",
                            accuracy = round(cm_PS_rf$overall[["Accuracy"]],5),
                            sensibilidad = round(cm_PS_rf$byClass[["Sensitivity"]], 5),
                            especificidad = round(cm_PS_rf$byClass[["Specificity"]], 5),
                            VPP = round(cm_PS_rf$byClass[["Pos Pred Value"]], 5),
                            VPN = round(cm_PS_rf$byClass[["Neg Pred Value"]], 5),
                            auc = round(ROCR::performance(pred.rf.PS, measure = "auc")@y.values[[1]], 5),
                            cv.error=NA))


# Probabilidad promedio de que complete el esquema de la encuesta
bias_NEA <- (mean(as.numeric(individual_NEA_test$pclass_rf)) - mean(as.numeric(individual_NEA_test$completo_f)))^2; bias_NEA
bias_GR <- (mean(as.numeric(individual_GR_test$pclass_rf)) - mean(as.numeric(individual_GR_test$completo_f)))^2; bias_GR
bias_CT <- (mean(as.numeric(individual_CT_test$pclass_rf)) - mean(as.numeric(individual_CT_test$completo_f)))^2; bias_CT
bias_FM <- (mean(as.numeric(individual_FM_test$pclass_rf)) - mean(as.numeric(individual_FM_test$completo_f)))^2; bias_FM
bias_PS <- (mean(as.numeric(individual_PS_test$pclass_rf)) - mean(as.numeric(individual_PS_test$completo_f)))^2; bias_PS


## 3.2. Random Forest con datos longitudinales



# 4) XGBoost  --------------------------------------
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



# RIDGE Y LASSO REGRESSION ---------------------------------------------------
# Para reducir varianza en las estimaciones

library(glmnet)

# Definimos variables con conjunto de entrenamiento
X <- individual_NEA_train[,-"completo_f"]
X = model.matrix(completo ~ ., data = X)[,-1]
y = individual_NEA_train$completo

# Filtramos la base de testeo y generamos los objetos para predicción
regresores = colnames(individual_NEA_train)
X_test = individual_NEA_test %>% select(all_of(regresores),-completo_f)
X_test = model.matrix(completo ~ ., data = X_test)[,-1]
y_test = individual_NEA_test$completo


## Ridge Regression ------------------------------------------------------------

# 2. Definir validación cruzada (10 folds)
folds <- createFolds(y, k = 10, list = TRUE)

# 3. Almacenar métricas
bias_values <- c()
variance_values <- c()
ecm_values <- c()

# 4. Loop sobre cada fold
for (i in 1:10) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(1:nrow(individual_NEA_train), test_idx)
  
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[test_idx, ]
  y_test <- y[test_idx]
  
  # Ajustar modelo Ridge con regresión logística
  lambda_seq <- 10^seq(-3, 2, length = 100)  # Valores de lambda
  ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_seq, family = "binomial")
  
  # Predecir probabilidades en el conjunto de prueba
  preds <- predict(ridge_model, newx = X_test, s = lambda_seq, type = "response")
  
  # Calcular sesgo y varianza para cada valor de lambda
  bias_fold <- colMeans(preds) - mean(y_test)  # Sesgo^2 = (E[f(x)] - y)^2
  variance_fold <- apply(preds, 2, var)        # Varianza = Var(f(x))
  
  # Guardar valores
  bias_values <- rbind(bias_values, bias_fold)
  variance_values <- rbind(variance_values, variance_fold)
}

# 5. Promediar sesgo, varianza y ecm en los folds
bias_mean <- colMeans(bias_values^2)  # Sesgo² promedio
variance_mean <- colMeans(variance_values)  # Varianza promedio
ecm_mean <- bias_mean + variance_mean

# 6. Graficar el trade-off sesgo-varianza
grafico_ridge_tradeoff <- 
  ggplot(data = data.frame(lambda_seq=lambda_seq,
                           bias_mean = bias_mean,
                           variance_mean = variance_mean,
                           ecm_mean)) + 
  geom_line(aes(log(lambda_seq), sqrt(bias_mean), colour = "Sesgo"), linewidth = 0.7) + 
  geom_line(aes(log(lambda_seq), sqrt(variance_mean), colour = "Desvío estándar"), linewidth = 0.7) + 
  geom_line(aes(log(lambda_seq), sqrt(ecm_mean), colour="RMSE"), linewidth = 0.7) + 
  geom_vline(xintercept = 2.5, linetype = "dashed") + 
  theme_light() + 
  theme(legend.position = "top", 
        legend.title = element_blank(),
        text=element_text(family="serif")) +
  ylab("RMSE / Sesgo / Desvío estándar") + xlab(latex2exp::TeX("$log(\\lambda)$"))
grafico_ridge_tradeoff



# Validación cruzada
# Con Error cuadrático medio
cvfit.mse <- cv.glmnet(x = x, y = y, type.measure = "mse", alpha=0, family="binomial")
plot(cvfit.mse)
log(cvfit.mse$lambda.min)

cvfit.mse

# Con deviance
cvfit.dev <- cv.glmnet(x = X, y = y, type.measure = "deviance", alpha=0, family="binomial")
plot(cvfit.dev)
log(cvfit.dev$lambda.min)

# AUC
cvfit.auc <- cv.glmnet(x = X, y = y, type.measure = "auc", alpha=0, family="binomial")
plot(cvfit.auc)
log(cvfit.auc$lambda.min)

# Misclassification error
cvfit.error <- cv.glmnet(x = x, y = y, type.measure = "class", alpha=0, family="binomial")
plot(cvfit.error)
log(cvfit.error$lambda.min)


# Modelo seleccionado para suavizado de ponderadores
# Ajustamos la regresión ridge para el valor que minimiza la varianza

ridge_model <- glmnet(X, y, alpha = 0, lambda = exp(0), family = "binomial")

X_NEA <- individual_NEA %>% select(all_of(regresores),-completo_f)
X_NEA = model.matrix(completo ~ ., data = X_NEA)[,-1]
y_NEA = individual_NEA$completo

individual_NEA$pprob_ridge <- predict(ridge_model, newx=X_NEA)


# Probabilidades estimadas comparadas
ggplot(individual_NEA) + 
  geom_boxplot(aes(nro_rep, pprob_ridge)) + 
  facet_wrap(AGLO_DESC~.)

ggplot(individual_NEA) + 
  geom_boxplot(aes(nro_rep, pprob_rf)) + 
  facet_wrap(AGLO_DESC~.)


## Lasso Regression ---------------------------------------------------------------

# 2. Definir validación cruzada (10 folds)
folds <- createFolds(y, k = 10, list = TRUE)

# 3. Almacenar métricas
bias_values <- c()
variance_values <- c()
ecm_values <- c()

# 4. Loop sobre cada fold
for (i in 1:10) {
  test_idx <- folds[[i]]
  train_idx <- setdiff(1:nrow(individual_NEA_train), test_idx)
  
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[test_idx, ]
  y_test <- y[test_idx]
  
  # Ajustar modelo Lasso con regresión logística
  lambda_seq <- 10^seq(-3, 1, length = 100)  # Valores de lambda
  lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_seq, family = "binomial")
  
  # Predecir probabilidades en el conjunto de prueba
  preds <- predict(lasso_model, newx = X_test, s = lambda_seq, type = "response")
  
  # Calcular sesgo y varianza para cada valor de lambda
  bias_fold <- colMeans(preds) - mean(y_test)  # Sesgo^2 = (E[f(x)] - y)^2
  variance_fold <- apply(preds, 2, var)     # Varianza = Var(f(x))
  
  # Guardar valores
  bias_values <- rbind(bias_values, bias_fold)
  variance_values <- rbind(variance_values, variance_fold)
}

# 5. Promediar sesgo, varianza y ecm en los folds
bias_mean <- colMeans(bias_values^2)  # Sesgo² promedio
variance_mean <- colMeans(variance_values)  # Varianza promedio
ecm_mean <- bias_mean + variance_mean

# 6. Graficar el trade-off sesgo-varianza
grafico_lasso_tradeoff <- 
  ggplot(data = data.frame(lambda_seq=lambda_seq,
                           bias_mean=bias_mean,
                           variance_mean=variance_mean,
                           ecm_mean=ecm_mean)) + 
  geom_line(aes(log(lambda_seq), sqrt(bias_mean), colour = "Sesgo"), linewidth = 0.7) + 
  geom_line(aes(log(lambda_seq), sqrt(variance_mean), colour = "Desvío estándar"), linewidth = 0.7) + 
  geom_line(aes(log(lambda_seq), sqrt(ecm_mean), colour="RMSE"), linewidth = 0.7) + 
  geom_vline(xintercept = -2, linetype = "dashed") + 
  theme_light() + 
  theme(legend.position = "top", 
        legend.title = element_blank(),
        text=element_text(family="serif")) +
  ylab("RMSE / Sesgo / Desvío estándar") + xlab(TeX("$log(\\lambda)$"))
grafico_lasso_tradeoff



save(grafico1_dt, grafico2_dt,
     grafico_lasso_tradeoff, grafico_ridge_tradeoff, 
     file="Informe y resultados/Modelos_clasificacion.RData")


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

# Misclassification error
cvfit.error <- cv.glmnet(x = x, y = y, type.measure = "class", alpha=1, family="binomial")
plot(cvfit.error)
log(cvfit.error$lambda.min)



grafico_ridge_tradeoff + grafico_lasso_tradeoff

# Al momento de elegir el mejor modelo (al final)
# Guardamos las probabilidades predichas (modelo NEA)
load("Informe y resultados/Modelos_clasificacion.RData") # Provisoriamente
individual_NEA$prob_completo1 <- predict(modelo_rf, newdata=individual_NEA, type="prob")[,2]
hist(individual_NEA$prob_completo1)

ggplot(individual_NEA, aes(nro_rep, prob_completo1)) + 
  geom_boxplot() + 
  facet_wrap(.~AGLO_DESC)

# Guardamos probabilidades predichas (modelos por aglomerado)
# Gran Resistencia
individual_NEA$prob_completo[individual_NEA$AGLOMERADO==8] <- 
  predict(modelo_rf_GR, newdata=individual_NEA[individual_NEA$AGLOMERADO==8], type="prob")[,2]
# Corrientes
individual_NEA$prob_completo[individual_NEA$AGLOMERADO==12] <- 
  predict(modelo_rf_CT, newdata=individual_NEA[individual_NEA$AGLOMERADO==12], type="prob")[,2]
# Formosa
individual_NEA$prob_completo[individual_NEA$AGLOMERADO==15] <- 
  predict(modelo_rf_FM, newdata=individual_NEA[individual_NEA$AGLOMERADO==15], type="prob")[,2]
# Posadas
individual_NEA$prob_completo[individual_NEA$AGLOMERADO==7] <- 
  predict(modelo_rf_PS, newdata=individual_NEA[individual_NEA$AGLOMERADO==7], type="prob")[,2]

ggplot(individual_NEA, aes(nro_rep, prob_completo)) + 
  geom_boxplot() + 
  facet_wrap(.~AGLO_DESC)

# Corrección del ponderador ----------------

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

# 1) Ajuste con la función calibrate()
# Diseño muestral original
library(survey)
individual_NEA[, PONDIH_c:= round(ifelse(prob_completo>0 & is.na(prob_completo)==F, PONDIH/(prob_completo/media_prob), PONDIH))]
disenio_base <- svydesign(ids=~1, data=individual_NEA, strata=~area, weights = ~PONDIH_c)

# We define the calibration formula and the population totals.
formula <- ~ periodo + AGLO_DESC + area + CH04 + CH06
T <- apply(model.matrix(formula, data = individual_NEA, weights=individual_NEA$PONDIH), 2, sum)

# Calibration with raking
calib.logit <- calibrate(disenio_base, formula, population = T, calfun = "logit")
individual_NEA$pw_logit <- weights(calib.logit)

base_calib <- individual_NEA |> 
  group_by(periodo, AGLO_DESC, area, CH04, CH06) |> 
  summarise(freq=sum(PONDIH,na.rm=T)); base_calib
disenio_calibrado <- calibrate(disenio_base, 
  formula= ~ periodo + AGLO_DESC + area + CH04 + CH06, 
  population=base_calib)

# 2) Ajuste a mano
# Probabilidad media de respuesta por área
individual_NEA[, media_prob:= mean(prob_completo,na.rm=T), by = .(periodo, area, AGLOMERADO)]

# Agregamos el total de PONDIH por 'área'
individual_NEA[, mj:= sum(PONDIH,na.rm=T), by = .(periodo, area, AGLOMERADO)]
individual_NEA[, mj1:= round(ifelse(prob_completo>0, sum(PONDIH/(prob_completo/media_prob)), sum(PONDIH))), by = .(periodo, area, AGLOMERADO)]

# Reponderación de las áreas
individual_NEA[, marca_area:= duplicated(area), by = .(periodo, AGLOMERADO)]
individual_NEA[, freq_mj:= mj/sum(mj[marca_area==F]), by = .(periodo, AGLOMERADO)]
individual_NEA[, freq_mj1:= mj1/sum(mj1[marca_area==F]), by = .(periodo, AGLOMERADO)]
hist(individual_NEA$freq_mj)
hist(individual_NEA$freq_mj1)

# Re-escalamos el PONDIH para mantener el total real por aglomerado
# (Redistribuimos la representación de cada área)
individual_NEA[, PONDIH_aglo:= sum(PONDIH), by = .(periodo, AGLOMERADO)]
individual_NEA[, mj_c:= PONDIH_aglo*freq_mj1, by = .(periodo, AGLOMERADO)]
hist(individual_NEA$mj)
hist(individual_NEA$mj_c)

# Reponderamos las viviendas dentro de cada área
individual_NEA[, PONDIH_c:= round(ifelse(prob_completo>0, PONDIH/(prob_completo/media_prob), PONDIH))]
individual_NEA[, total_area:=sum(PONDIH_c), by = .(periodo, area, AGLOMERADO)]
individual_NEA[, freq_vivxarea:= PONDIH_c/total_area, by = .(periodo, area, AGLOMERADO)]
individual_NEA[, PONDIH_c:= round(mj_c*freq_vivxarea), by = .(periodo, area, AGLOMERADO)]

# Ponderador corregido
hist(individual_NEA$PONDIH)
hist(individual_NEA$PONDIH_c)

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

individual_NEA %>% 
  mutate(ajuste=PONDIH_c-PONDIH) %>% 
  ggplot() + 
  geom_boxplot(aes(nro_rep, ajuste)) + 
  facet_wrap(.~AGLO_DESC)

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

# Pobreza semestral (comparación)
resultados_NEA %>% 
  mutate(anio=year(periodo),
         semestre=ifelse(quarter(periodo) %in% c(1,2), 1, 2)) %>% 
  mutate(periodo=as.numeric(paste0(anio,semestre))) %>% 
  group_by(AGLO_DESC, periodo) %>% 
  summarise(pobreza.x=mean(pobreza.x), pobreza.y=mean(pobreza.y),
            indigencia.x=mean(indigencia.x), indigencia.y=mean(indigencia.y)) %>% 
  pivot_longer(cols = c(pobreza.x, pobreza.y), names_to = "tipo_pobreza", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = AGLO_DESC)) + 
  geom_smooth(aes(x = periodo, y = valor, color = AGLO_DESC)) +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(.~tipo_pobreza, nrow = 2) + 
  theme_light() + 
  theme(legend.position = "bottom") + 
  ylab("") + xlab("")

# Pobreza individuos # --------------------------------------------

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

resultados_NEA %>% 
  pivot_longer(cols = c(indigencia.x, indigencia.y), names_to = "tipo_pobreza", values_to = "valor") %>% 
  ggplot() + 
  geom_line(aes(x = periodo, y = valor, color = tipo_pobreza)) + 
  geom_smooth(aes(x = periodo, y = valor, color = tipo_pobreza)) +
  facet_wrap(.~AGLO_DESC)








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
              

# Resistencia tiene alta probabilidad de que respondan 1 y 2 veces, al
# mismo tiempo que tiene baja probabilidad de que respondan 4 veces respecto
# a los demás aglomerados.
# Quienes responden menos veces es más probable que sean no pobres que pobres
# Ver por trimestre: ¿Qué explica el resultado del trimestre?
#   Todos los trimestres se renueva el 25% de la muestra



# Guardamos resultados
save(tabla1, tabla2, grafico1, grafico2, grafico3, modelos_panel, modelos.glm.multi,
     file = "Informe y resultados/Resultados.RData")



