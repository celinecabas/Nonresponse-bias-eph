# Armado del panel de datos EPH Usuario ####

library(data.table)
library(tidyverse)
library(readxl)

# Cargamos los nombres de los archivos
files <- list.files("Bases/EPH_Base_Usuario")
archivos <- c()
for (i in 1:length(files)){
  archivo <- list.files(paste0("Bases/EPH_Base_Usuario/",files[i]),full.names = T)
  archivos <- c(archivo,archivos)
}

# Separamos las rutas de los archivos individuales y de hogar
archivos_individual <- archivos[grepl("individual|personas",archivos, ignore.case = T)]
archivos_hogar <- archivos[grepl("hogar",archivos, ignore.case = T)]

# Quitamos la base de 2024T4 del listado de archivos
posicion_2024T4_ind <- which(archivos_individual=="Bases/EPH_Base_Usuario/EPH_usu_4_Trim_2024_txt/usu_individual_T424.txt")
posicion_2024T4_hog <- which(archivos_hogar=="Bases/EPH_Base_Usuario/EPH_usu_4_Trim_2024_txt/usu_hogar_T424.txt")
archivos_individual <- archivos_individual[-posicion_2024T4_ind]
archivos_hogar <- archivos_hogar[-posicion_2024T4_hog]

# Cargamos las bases para comparar variables y quitar las nuevas
# ind_2024T3 <- fread("Bases/EPH_Base_Usuario/EPH_usu_3_Trim_2024_txt/usu_individual_T324.txt")
# ind_2024T4 <- fread("Bases/EPH_Base_Usuario/EPH_usu_4_Trim_2024_txt/usu_individual_T424.txt")
# hog_2024T3 <- fread("Bases/EPH_Base_Usuario/EPH_usu_3_Trim_2023_txt/usu_hogar_T323.txt")
# hog_2024T4 <- fread("Bases/EPH_Base_Usuario/EPH_usu_4_Trim_2024_txt/usu_hogar_T424.txt")


# Variables nuevas a partir de 2024 T4
# variables_nuevas <- setdiff(names(hog_2024T4),names(hog_2024T3))

# Las quitamos de la base
# hog_2024T4 <- hog_2024T4 |> select(-variables_nuevas)

# Bases individuales
individual <- data.table()
for (i in 1:length(archivos_individual)){
  base <- fread(archivos_individual[i])
  individual <- rbind(individual, base, fill=T)
  rm(base)
}

# Anexamos la base 2024 T4
# individual <- rbind(individual, base2024T4)

# Bases hogares
hogar <- data.table()
for (i in 1:length(archivos_hogar)){
  base <- fread(archivos_hogar[i])
  hogar <- rbind(hogar, base,fill=T)
  rm(base)
}

# Limpiamos memoria
rm(archivo, archivos, archivos_hogar, archivos_individual, files, i)

# Cargamos las canastas
# Canastas
canastas <- read_excel("Bases/Canastas_regionales.xlsx", sheet = "Mensual") %>% 
  mutate(trimestre=quarter(ymd(paste0(anio,"-",mes,"-01")))) %>%
  group_by(anio,trimestre,region,codigo) %>% 
  summarise(CBA=mean(CBA),CBT=mean(CBT)) %>% 
  select(REGION_DESC=region,ANO4=anio,TRIMESTRE=trimestre,CBA,CBT,REGION_COD=codigo) %>% 
  as.data.table()

# Anexamos coeficiente de adulto equivalente
individual <- merge.data.table(individual, eph::adulto_equivalente, by = c("CH04","CH06"))

# Cruzamos para agregar a las bases de hogar e individual
tabla_regiones <- read_excel("Bases/Tabla_regiones.xlsx", sheet = "Diccionario") %>% 
  distinct(AGLO_COD,AGLO_DESC,REGION_COD,REGION_DESC) %>% 
  as.data.table()

individual <- merge.data.table(individual,tabla_regiones,
                               by.x="AGLOMERADO", by.y="AGLO_COD")

# Cruzamos con las canastas
individual<- merge.data.table(individual, canastas, 
                              by = c("REGION_DESC","REGION_COD","ANO4","TRIMESTRE"))

# Filtramos por aglomerados del Chaco
# individual <- individual %>% filter(AGLOMERADO%in%c(8,44) & ANO4!=2020)

# Verificamos que hayan cruzado todos
sum(is.na(individual$CBA))
sum(is.na(individual$CBT))

# Calculamos las canastas y el ice
individual <- individual %>% 
  mutate(ice=CBT/CBA) %>% 
  mutate(CBA=adequi*CBA) %>% 
  mutate(CBT=CBA*ice)

# Agregamos las canastas por hogar
canastas_hogar <- individual %>% 
  group_by(ANO4,TRIMESTRE,AGLOMERADO, CODUSU, NRO_HOGAR) %>% 
  summarise(CBA=sum(CBA,na.rm=T),CBT=sum(CBT,na.rm=T))

# Cruzamos con la base de hogares
hogar <- merge.data.frame(hogar, canastas_hogar, 
                          by = c("ANO4","TRIMESTRE","CODUSU","NRO_HOGAR","AGLOMERADO"))
hogar <- merge.data.frame(hogar,tabla_regiones,
                          by.x="AGLOMERADO", by.y="AGLO_COD")

# Calculamos pobreza e indigencia a nivel hogar
hogar <- as.data.table(hogar)
hogar[, hogar_pobre:= ifelse(CBT>ITF,1,0)]
hogar[, hogar_indigente:= ifelse(CBA>ITF,1,0)]

# Asignamos la clasificación de pobre e indigente en la base de hogar
individual <- merge.data.table(individual, hogar[,c(1:5,95,96)], 
                               by = c("ANO4","TRIMESTRE","AGLOMERADO","CODUSU","NRO_HOGAR"))


# Analizamos el NEA
hogar_NEA <- hogar %>% filter(REGION==41)
hogar_NEA %>% distinct(AGLOMERADO,AGLO_DESC)

table(hogar_NEA$CODUSU, hogar_NEA$ANO4)

# Marca de los repetidos al menos dos períodos
hogar_NEA[, index:= paste0(CODUSU, NRO_HOGAR)]
codusus <- as.data.frame(table(hogar_NEA$index))
table(codusus$Freq) # Hasta 4 repeticiones en todos los casos
setnames(codusus,c("Var1","Freq"),c("index","nro_rep"))

# Cruzamos con la base de hogares
hogar_NEA <- merge.data.frame(hogar_NEA, codusus, by = "index")

# Exportamos la base de datos hogar
fwrite(hogar_NEA, "Bases/hogar_NEA.txt")

# Exportamos la base de datos individual
individual <- as.data.table(individual)
individual_NEA <- individual[CODUSU %in% hogar_NEA$CODUSU,]
fwrite(individual_NEA, "Bases/individual_NEA.txt")
