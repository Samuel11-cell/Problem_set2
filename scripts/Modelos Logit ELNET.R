# install and load required packages
Packages <- c("tidyverse", 
              "ggplot2", 
              "pacman", 
              "dplyr",
              "haven",
              "boot",
              "broom",
              "lmtest", 
              "fixest", 
              "gridExtra", 
              "writexl", 
              "readxl",
              "glmnet",
              "VIM",
              "caret", 
              "MLmetrics",
              "Metrics",
              "pROC")

invisible(lapply(Packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)){ 
    install.packages(pkg)}
  library(pkg, character.only = TRUE)}))

# Recolección de los datos:
train_hogares<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\train_hogares.csv")
train_personas<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\train_personas.csv")
test_hogares<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\test_hogares.csv")
test_personas<-read.csv("C:\\Users\\samue\\OneDrive\\Escritorio\\Economia\\Big Data y Machine Learning\\Taller 2\\DATOS\\test_personas.csv")

##-----------------------------------------------------------------------------##

# organización y Pre procesamiento:

# Calculo de pobreza:
table(train_hogares$Pobre)

#Podemos generar variables a nivel de hogar a partir de la base de datos de personas:

# Realizamos el preprocesamiento para las bases de personas:
# Creamos variables usando unformacion por persona:
preproces_personas <- function(data,...){
  data<-data %>% mutate(
    mujer = ifelse(P6020==2,1,0),
    menor_de_edad = ifelse(P6040 <= 16, 1, 0),
    adulto_mayor = ifelse(P6040 >= 60, 1, 0),
    regimen_salud = ifelse(P6100==4,3,P6100),
    regimen_salud = ifelse(is.na(regimen_salud), 3, regimen_salud),
    EducLevel = ifelse(P6210==9,0,P6210),
    Jefe_H = ifelse(P6050==1,1,0),
    ocupado = ifelse(is.na(Oc),0,1),
    desocupado = ifelse(is.na(Des),0,1),
    Inactivo = ifelse(is.na(Ina),0,1),
    Tipo_primer_empleo = ifelse(P6430==9,0,P6430),
    Tipo_primer_empleo = ifelse(is.na(Tipo_primer_empleo),1,Tipo_primer_empleo),
    segundo_empleo = ifelse(P7040==1,1,0),
    segundo_empleo = ifelse(is.na(segundo_empleo), 0, segundo_empleo),
    Recibio_horasextra = ifelse(P6510==1,1,0),
    Recibio_horasextra = ifelse(is.na(Recibio_horasextra), 0, Recibio_horasextra)) %>%
    rename(edad = P6040) %>%
    select(id, mujer, edad, menor_de_edad, adulto_mayor, regimen_salud, EducLevel, Jefe_H, 
           ocupado,desocupado, Inactivo,Tipo_primer_empleo,segundo_empleo,
           Recibio_horasextra) 
  return(data)
}

train_personas <- preproces_personas(train_personas)
test_personas <- preproces_personas(test_personas)

# generamos variables agregadas con base personas:

preproces_personas_agregacion <- function(data,...){
  agregacion <- data %>%
    group_by(id)%>%
    summarise(
      num_mujeres = sum(mujer,na.rm = TRUE),
      num_menores = sum(menor_de_edad, na.rm = TRUE),
      num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
      maxEducLevel = max(EducLevel, na.rm = TRUE),
      num_ocupados = sum(ocupado, na.rm=TRUE),
      num_desocupados = sum(desocupado, na.rm = TRUE),
      num_inactivos = sum(Inactivo, na.rm = TRUE),
      num_con_segundo_empleo = sum(segundo_empleo, na.rm = TRUE),
      num_recibieron_hrextra = sum(Recibio_horasextra, na.rm = TRUE)
    )%>%
    ungroup()
    
  por_hogares_jefeh <- data %>%
      filter(Jefe_H==1) %>%
      select(id,mujer,regimen_salud,EducLevel,Jefe_H, ocupado,Inactivo,
             desocupado,Tipo_primer_empleo,segundo_empleo,edad,
             Recibio_horasextra)%>%
      rename(Jefe_H_mujer=mujer,
             Jefe_regimen_salud=regimen_salud,
             Jefe_EducLevel=EducLevel,
             Jefe_edad=edad,
             Jefe_ocupado=ocupado,
             Jefe_Inactivo=Inactivo,
             Jefe_desocupado=desocupado,
             Jefe_Tipo_primer_empleo=Tipo_primer_empleo,
             Jefe_segundo_empleo=segundo_empleo,
             Jefe_Recibio_horasextra=Recibio_horasextra
      )
    resultado_final <- agregacion %>%
      left_join(por_hogares_jefeh, by = "id")
    
    return(resultado_final)
  }


train_personas <- preproces_personas_agregacion(train_personas)
test_personas<-preproces_personas_agregacion(test_personas)


# generamos variables a nivel hogar con base hogar:

preproces_hogares <- function(data,...){
  data<-data %>% 
  mutate(
    arrienda=ifelse(P5090==3,1,0)
  ) %>%
    select(id, Clase,Dominio,arrienda,Nper,
           any_of("Pobre"))
  return(data)
}


train_hogares<-preproces_hogares(train_hogares)
test_hogares<-preproces_hogares(test_hogares)

# Unimos las bases de datos correspondientes a nivel hogar
TRAIN <-merge(train_hogares,train_personas)
TEST <- merge(test_hogares,test_personas)



#convertimos las variables a formatos adecuados
TRAIN<- TRAIN %>% 
  mutate(Pobre=factor(Pobre,
                      levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                             levels=c(1,2,3),
                             labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                           'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                             levels=c(0,1,2,3,4,5,6,7,8),
                             labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                      "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                      "Trabajador familiar sin remuneración",
                                      "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                      "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                                        levels=c(0,1),
                                        labels=c("No","Yes"))
  )

TEST<- TEST %>% 
  mutate(Dominio=factor(Dominio),
         Clase=factor(Clase),
         arrienda=factor(arrienda,
                         levels=c(0,1),
                         labels = c("No","Yes")),
         Jefe_H_mujer=factor(Jefe_H_mujer,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_regimen_salud=factor(Jefe_regimen_salud,
                             levels=c(1,2,3),
                             labels=c("Contributivo (eps)","Especial","Subsidiado")),
         Jefe_EducLevel=factor(Jefe_EducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                        'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,
                             levels=c(0:6), 
                             labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 
                                      'Secundaria','Media', 'Universitaria')),
         Jefe_ocupado=factor(Jefe_ocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Inactivo=factor(Jefe_Inactivo,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_desocupado=factor(Jefe_desocupado,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Tipo_primer_empleo=factor(Jefe_Tipo_primer_empleo,
                             levels=c(0,1,2,3,4,5,6,7,8),
                             labels=c("Ns","Obrero o empleado de empresa particular","Obrero o empleado del gobierno",
                                      "Empleado doméstico","Trabajador por cuenta propia","Patrón o empleador",
                                      "Trabajador familiar sin remuneración",
                                      "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                      "Jornalero o peón ")),
         Jefe_segundo_empleo=factor(Jefe_segundo_empleo,
                             levels=c(0,1),
                             labels=c("No","Yes")),
         Jefe_Recibio_horasextra=factor(Jefe_Recibio_horasextra,
                             levels=c(0,1),
                             labels=c("No","Yes"))
  )


corrijo_na <- function(data, ...) {
  data <- data %>%
    mutate(Jefe_regimen_salud= ifelse(is.na(Jefe_regimen_salud), 3, Jefe_regimen_salud))
  return(data)
}
TRAIN <- corrijo_na(TRAIN)
TEST <- corrijo_na(TEST)

##-------------------------------------------------------------------------------##
#Para visualizar la distribución del desempleo en nuestra muestra, 
#generamos un gráfico de barras que muestra la proporción de individuos pobres y no pobres.

ggplot(TRAIN, aes(x = Pobre,y = after_stat(count / sum(count)), fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.05)) +
  scale_fill_manual(values = c("No" = "#CAFF70", "Yes"= "coral1")) +
  labs(x = "", y = "%")  

# se tiene un desbalance leve.

##-------------------------------------------------------------------------------##

##-------------------------------------------------------------------------------##

#MODEL TRAINIG: LOGIT ELASTIC NET DESBALANCEADO


# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
model1 <- train(form_modelo_logit1,
                data=TRAIN,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                preProcess = c("center", "scale"),  # Normaliza variables predictoras
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)
model1

model1$bestTune

##-----------------------------------------------------------------------------##
# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(model1, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_model1 <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_model1

# guardo en un data frame
df_ELNET <- data.frame(
  Model = "model_logit_ELNET",
  F1_Score = cm_model1$byClass["F1"]
  )

#  Elimino los nombres de las filas que no informan nada.
rownames(df_ELNET)<-NULL
df_ELNET

##-----------------------------------------------------------------------------##
# Preparacion para el envio a Kaggle:

predictSample <- TEST   %>% 
  mutate(pobre_lab = predict(model1, newdata = TEST, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##


# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)


##-----------------------------------------------------------------------------##




##-----------------------------------------------------------------------------##

#MODEL TRAINIG: ELASTIC NET UMBRAL OPTIMO 

# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET <- train(form_modelo_logit1,
                data=TRAIN,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                preProcess = c("center", "scale"),  # Normaliza variables predictoras
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.5),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)
logit_ELNET

logit_ELNET$bestTune

predicciones <- predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit_ELNET <- roc(response = TRAIN$Pobre,  
                     predictor = predicciones,  
                     levels = c("No", "Yes"),  
                     direction = "<")
logit_ELNET_best_threshold <- coords(roc_obj_logit_ELNET, x = "best", best.method = "closest.topleft")

# Mostrar el umbral óptimo
logit_ELNET_best_threshold
pred_clase <- ifelse(predicciones >= logit_ELNET_best_threshold[1], "Yes", "No")


# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_ELNET_nuevo_umbral <- TRAIN %>%
  mutate(pobre_prob_logit_ELNET_sens = predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_ELNET_sens >= logit_ELNET_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))

cm_logit_ELNET_Nuevo_umbral <- confusionMatrix(Logit_ELNET_nuevo_umbral$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_logit_ELNET_Nuevo_umbral)


# guardo en un data frame
df_logit_ELNET_best_threshold <- data.frame(
  logit_ELNET_best_threshold = "logit_ELNET_best_threshold",
  F1_Score = cm_logit_ELNET_Nuevo_umbral$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_best_threshold)<-NULL
df_logit_ELNET_best_threshold
##-----------------------------------------------------------------------------##

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(pobre_lab = predict(logit_ELNET, newdata = TEST, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el Nuevo Umbral
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_lab >= logit_ELNET_best_threshold$threshold, "Yes", "No"),
           levels = c("No", "Yes")))%>% select(id,clasificacion_nuevo_umbral)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_nuevo_umbral=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##


# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(logit_ELNET$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(logit_ELNET$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)


##-----------------------------------------------------------------------------##














##-----------------------------------------------------------------------------##
# LOGIT ELASTIC NET: 

#MODEL TRAINIG: ELASTIC NET curva PR

# Modelo usando Elastic Net, escogiendo los hiperparámetros usando cross validación
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET <- train(form_modelo_logit1,
                     data=TRAIN,
                     metric = "F",
                     method = "glmnet",
                     trControl = ctrl,
                     preProcess = c("center", "scale"),  # Normaliza variables predictoras
                     family="binomial",
                     tuneGrid=expand.grid(
                       alpha = seq(0,1,by=.5),
                       lambda =10^seq(-1, -3, length = 10)
                     )
                     
)
logit_ELNET

logit_ELNET$bestTune

predicciones <- predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"]

roc_obj_logit_ELNET <- roc(response = TRAIN$Pobre,  
                           predictor = predicciones,  
                           levels = c("No", "Yes"),  
                           direction = "<")

# calculamos la curva PR para  100 valores de umbral, y para cada uno de ellos calculo la precisión y el recall del modelo:
prec_recall<-data.frame(coords(roc_obj_logit_ELNET, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall

# Encontramos la columna del F1-score, ya que este nos da el mejor balance entre capturar la clase minoritaria (recall) 
# sin perder demasiada precisión.
prec_recall<- prec_recall  %>% mutate(F1=(2*precision*recall)/(precision+recall))
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
umbral_optimo <- prec_recall$threshold[which.max(prec_recall$F1)]




# Evaluando en el TRAIN Set aplicando el nuevo umbral
Logit_ELNET_prec_recall <- TRAIN %>%
  mutate(pobre_prob_logit_ELNET_sens = predict(logit_ELNET, newdata = TRAIN, type = "prob")[, "Yes"],
         clasificacion_nuevo_umbral = factor(
           ifelse(pobre_prob_logit_ELNET_sens >= umbral_optimo, "Yes", "No"),
           levels = c("No", "Yes")))

cm_Logit_ELNET_prec_recall <- confusionMatrix(Logit_ELNET_prec_recall$clasificacion_nuevo_umbral, TRAIN$Pobre, positive = "Yes",  mode = "prec_recall")
print(cm_Logit_ELNET_prec_recall)


# guardo en un data frame
df_Logit_ELNET_prec_recall <- data.frame(
  Logit_ELNET_prec_recall = "Logit_ELNET_prec_recall",
  F1_Score = cm_Logit_ELNET_prec_recall$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_Logit_ELNET_prec_recall)<-NULL
df_Logit_ELNET_prec_recall
##-----------------------------------------------------------------------------##

# Preparacion para el envio a Kaggle:

predictSample <- TEST %>%
  mutate(pobre_lab = predict(logit_ELNET, newdata = TEST, type = "prob")[, "Yes"],
         
         # Clasificamos los Casos Usando el prec_recall
         clasificacion_prec_recall = factor(
           ifelse(pobre_lab >= umbral_optimo, "Yes", "No"),
           levels = c("No", "Yes")))%>% select(id,clasificacion_prec_recall)
head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(clasificacion_prec_recall=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

##-----------------------------------------------------------------------------##


# Replace '.' with '_' in the numeric values converted to strings
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(logit_ELNET$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(logit_ELNET$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)
##-----------------------------------------------------------------------------##

















##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
#Rebalanceo de Clases (Class Rebalancing) con Re-ponderar observaciones:

# Constructir los ponderadores
pos_weight <- sum(TRAIN$Pobre == "No") / sum(TRAIN$Pobre == "Yes")
wts <- ifelse(TRAIN$Pobre == "Yes" , pos_weight, 1)
pos_weight



ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_reponderar_obs<- train(form_modelo_logit1,
                     data=TRAIN,
                     metric = "F",
                     weights    = wts,  # <-- pesos
                     method = "glmnet",
                     trControl = ctrl,
                     preProcess = c("center", "scale"),  # Normaliza variables predictoras
                     family="binomial",
                     tuneGrid=expand.grid(
                       alpha = seq(0,1,by=.5),
                       lambda =10^seq(-1, -3, length = 10)
                     )
                     
)
logit_ELNET_reponderar_obs

logit_ELNET_reponderar_obs$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_reponderar_obs, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_ELNET_reponderar_obs <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_ELNET_reponderar_obs

# guardo en un data frame
df_ELNET_reponderar_obs <- data.frame(
  Model = "logit_ELNET_reponderar_obs",
  F1_Score = df_ELNET_reponderar_obs$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_ELNET_reponderar_obs)<-NULL
df_ELNET_reponderar_obs






##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
# Rebalanceo de Clases Submuestreo (Down Sampling)

set.seed(1103)
downSampledTrain <- downSample(x = TRAIN,
                               y = TRAIN$Pobre,
                               yname = "Pobre")
dim(TRAIN)
dim(downSampledTrain)
table(downSampledTrain$Pobre)


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_down_sampling<- train(form_modelo_logit1,
                                   data=downSampledTrain,
                                   metric = "F",
                                   method = "glmnet",
                                   trControl = ctrl,
                                   preProcess = c("center", "scale"),  # Normaliza variables predictoras
                                   family="binomial",
                                   tuneGrid=expand.grid(
                                     alpha = seq(0,1,by=.5),
                                     lambda =10^seq(-1, -3, length = 10)
                                   )
                                   
)
logit_ELNET_down_sampling

logit_ELNET_down_sampling$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_down_sampling, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_ELNET_down_sampling <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_ELNET_down_sampling

# guardo en un data frame
df_logit_ELNET_down_sampling <- data.frame(
  Model = "logit_ELNET_down_sampling",
  F1_Score = cm_logit_ELNET_down_sampling$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_down_sampling)<-NULL
df_logit_ELNET_down_sampling












##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##

# Rebalanceo de Clases Submuestreo (Up Sampling)
set.seed(1103)
upSampledTrain <- upSample(x = TRAIN,
                           y = TRAIN$Pobre,
                           yname = "Pobre")
dim(TRAIN)
dim(upSampledTrain)
table(upSampledTrain$Pobre)


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Defino la ecuacion de mi primer modelo:
form_modelo_logit1=Pobre~Nper+num_ocupados+num_menores+num_adulto_mayor+arrienda+
  maxEducLevel+Jefe_H_mujer+Jefe_H_mujer*num_menores+Jefe_desocupado+Jefe_H_mujer*Jefe_desocupado+
  Jefe_regimen_salud+Jefe_Tipo_primer_empleo+
  Jefe_segundo_empleo

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

# install.packages("MLmetrics")
# require(MLmetrics)

set.seed(098063)
logit_ELNET_upsampled<- train(form_modelo_logit1,
                                  data=upSampledTrain,
                                  metric = "F",
                                  method = "glmnet",
                                  trControl = ctrl,
                                  preProcess = c("center", "scale"),  # Normaliza variables predictoras
                                  family="binomial",
                                  tuneGrid=expand.grid(
                                    alpha = seq(0,1,by=.5),
                                    lambda =10^seq(-1, -3, length = 10)
                                  )
                                  
)
logit_ELNET_upsampled

logit_ELNET_upsampled$bestTune

# Recupero antes del envio la matriz de confusion y el desempeño en el train set:

# Creación de la matriz de confusión:
predicciones <- predict(logit_ELNET_upsampled, newdata = TRAIN, type = "raw")
predicciones <- factor(predicciones, levels = c("No", "Yes"))
TRAIN$Pobre <- factor(TRAIN$Pobre, levels = c("No", "Yes"))

cm_logit_ELNET_upsampled <- confusionMatrix(predicciones, TRAIN$Pobre, positive = "Yes")
cm_logit_ELNET_upsampled

# guardo en un data frame
df_logit_ELNET_upsampled <- data.frame(
  Model = "logit_ELNET_upsampled",
  F1_Score = cm_logit_ELNET_upsampled$byClass["F1"]
)

#  Elimino los nombres de las filas que no informan nada.
rownames(df_logit_ELNET_upsampled)<-NULL
df_logit_ELNET_upsampled









##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
# Tabla de resultados metrica F1

# Normalizar los nombres de columnas en todos los dataframes
colnames(df_ELNET) <- c("Model", "F1_Score")
colnames(df_logit_ELNET_best_threshold) <- c("Model", "F1_Score")
colnames(df_Logit_ELNET_prec_recall) <- c("Model", "F1_Score")
colnames(df_ELNET_reponderar_obs) <- c("Model", "F1_Score")
colnames(df_logit_ELNET_down_sampling) <- c("Model", "F1_Score")
colnames(df_logit_ELNET_upsampled) <- c("Model", "F1_Score")

# Ahora podemos unir todos los dataframes en una sola tabla
df_desempeño_LOGIT_ELNET_F1 <- rbind(
  df_ELNET, 
  df_logit_ELNET_best_threshold, 
  df_Logit_ELNET_prec_recall, 
  df_ELNET_reponderar_obs, 
  df_logit_ELNET_down_sampling, 
  df_logit_ELNET_upsampled
)

# Ver tabla final
print(df_desempeño_LOGIT_ELNET_F1)

##-----------------------------------------------------------------------------##
df_desempeño_F1 <- rbind(
  df_desempeño_LOGIT_ELNET_F1,
  df_desempeño_LOGIT_F1)

print(df_desempeño_F1)

