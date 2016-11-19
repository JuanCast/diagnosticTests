##################################################################
### Diagnostic tests studies in R
##################################################################

# Paso 1: Source + library  ####
library("e1071")
library("dplyr")
library("xlsx")
library("pastecs")
library("googlesheets")
library("OptimalCutpoints")
suppressPackageStartupMessages(library("dplyr"))
(my_sheets <- gs_ls())  # get sheets from google sheets ( go to https://goo.gl/6EKsFg)
tirasReactivas <- gs_title("fileTitle")  # register file
# gs_gs(tirasReactivas)  # uncoment to register updated file
gs_ws_ls(tirasReactivas)
datos <- tirasReactivas %>%  # read desired rows in ws 1 
gs_read(ws = "workSheetName1", range = cell_rows(1:37))

# Paso 2: Clean data ####

# https://goo.gl/mtKsZ8
# View the first 6 rows of data
# head(datos)
# # View the last 6 rows of data
# tail(datos)
# # View a condensed summary of the data
# # str(datos)
# glimpse(datos)
# #View its class
# class(datos) #data.frame
# #view dimensions
# dim(datos) # Rows Columns
# #Look at column names
# names(datos)
# # view a summary
# summary(datos)
#plots
      # dput(names(datos))
# continuas <- c("edad", "peso", "talla", "MELD", "GASA", "albuminaLA", 
#"proteinasLA", 
# "leucocitosLA", "PMNLA", "hematiesLA", "temperatura", "FC", "FR", "TAS", 
# "TAD", "leucocitos", "bandas", "plaquetas", "sodio", "AST", "ALT", 
# "PT", "INR", "albumina", "BT", "creatinina", "BUN")
# par(mfrow=c(3,2))
# sapply(datos[continuas], hist)
# 
# plot(datos$BUN, datos$creatinina)
# plot(datos$peso, datos$talla)
# plot(datos$PMNLA, datos$MELD)
# numeric Numeric data (approximations of the real numbers, ℝ)
# integer Integer data (whole numbers, ℤ)
# factor Categorical data (simple classifications, like gender)
# ordered Ordinal data (ordered classifications, like educational level)
# character Character data (strings)
# raw Binary data

  # Factors
datos$status <- cut(datos$PMNLA, c(0, 250, Inf), right=FALSE, labels=c(0:1))
datos$status <- factor(datos$status)

characters <- sapply(datos, is.character)
datos[characters] <- lapply(datos[characters], as.factor)
  # Ordered
  # CHILD
datos$CHILD <- ordered(datos$CHILD)
  # Encefalopatia
datos$encefalopatiaHepatica <- ordered(datos$encefalopatiaHepatica)
  # ascitis
datos$ascitis <- ordered(datos$ascitis)
  # LRA
nivelesLRA <- c("NO", "AKI1", "AKI2", "AKI3")
datos$LRA <- ordered(datos$LRA,
                     levels = nivelesLRA,
                     labels = nivelesLRA)
  # Interpretacion
nivelesIntrerpretacion <- c("Negativo", "Trazas", "1+", "2+")
datos$interpretacion1 <- ordered(datos$interpretacion1,
       levels = nivelesIntrerpretacion,
       labels = nivelesIntrerpretacion)
datos$interpretacion2 <- ordered(datos$interpretacion2,
       levels = nivelesIntrerpretacion,
       labels = nivelesIntrerpretacion)

# num
num <- sapply(datos, is.integer)
datos[num] <- lapply(datos[num], as.numeric)
datos$MELD <- as.integer(datos$MELD)


# Outliers
  # normal distribution
normalOutliers <- lapply(datos[num], boxplot.stats)
  # not normal
# interpretacion Vs pmn
# plot(datos$interpretacion1, datos$PMNLA)
# plot(datos$interpretacion2, datos$PMNLA)
# datos$PMNLA[datos$PMNLA >= 250]
# datos$hematiesLA[datos$hematiesLA >= 250]
# datos$interpretacion1[datos$interpretacion1 >= "2+"]
# table(datos$interpretacion1, datos$PMNLA)
# table(datos$interpretacion1, datos$status)
#  table(datos$interpretacion2, datos$status)
########################################################### 
### Reuslts 
########################################################### 

# Paso 1: descriptivo ####
glimpse(datos)
uniVariadoContinuio <- stat.desc(datos[num])
write.xlsx(uniVariadoContinuio, 
           "/Users/Juan/Google Drive/Projects/tirasReactivas/uniVariadoContinuio.xlsx")
univariadoCategotico <- summary(datos[characters])
write.xlsx(univariadoCategotico, 
           "/Users/Juan/Google Drive/Projects/tirasReactivas/univariadoCategotico.xlsx")
# Paso 2: Concordancia de la interpretación ####


tablaInterpretacion <- table(datos$interpretacion1, datos$interpretacion2)
classAgreement(tablaInterpretacion) # kappa 0.8966132
write.xlsx(tablaInterpretacion, 
           "/Users/Juan/Google Drive/Projects/tirasReactivas/tablaInterpretacion.xlsx")

# Paso 3: desempeno de la prueba ####
  # file:///Users/Juan/Google%20Drive/Reference/Statistics/pruebasDiagnosticas/v61i08%20(2).pdf
cutpoint1 <- optimal.cutpoints(X = "interpretacion1", status = "status",
                               tag.healthy = 0, 
                               methods = c("Youden", "SpEqualSe"), data = datos,
                               categorical.cov = NULL, pop.prev = NULL,
                               control = control.cutpoints(), ci.fit = TRUE)

write.xlsx(summary(cutpoint1)$p.table, 
           "/Users/Juan/Google Drive/Projects/tirasReactivas/cutpoint1.xlsx")
cutpoint2 <- optimal.cutpoints(X = "interpretacion2", status = "status",
                               tag.healthy = 0, 
                               methods = c("Youden", "SpEqualSe"), data = datos,
                               categorical.cov = NULL, pop.prev = NULL,
                               control = control.cutpoints(), ci.fit = TRUE)
write.xlsx(summary(cutpoint2)$p.table, 
           "/Users/Juan/Google Drive/Projects/tirasReactivas/cutpoint2.xlsx")
