
## 1. Diferencias sintaxis ----
## Punto en las diapositivas: 1.2. Un breve ejemplo 

## Ejemplos para ver las diferencias entre la sintáxis de Tidymodels frente a la de otras librerías

## Carga de librerías para los ejemplos
library(glmnet)
library(tidymodels)
library(h2o)


## Ejemplo 1 

## Con glmnet:
model <-
  glmnet(
    as.matrix(mtcars[2:11]),
    mtcars$mpg
  )

## Con Tidymodels:

model <-
  linear_reg(penalty = 0, mixture = 0) %>%
  set_engine("glmnet") %>%
  fit(mpg ~ ., mtcars)

## Ejemplo 2

## Con H2O:

h2o::h2o.init()

as.h2o(mtcars, "mtcars")

model <-
  h2o.glm(
    x = colnames(mtcars[2:11]),
    y = "mpg",
    "mtcars"
  )

h2o_end()
## Con Tidymodels:

model <-
  linear_reg() %>%
  set_engine("h2o") %>%
  fit(mpg ~ ., mtcars)

## CASO PRÁCTICO ----
## Punto en las diapositivas: 3.1. Caso práctico 

## Carga de librerías para el caso práctico
if(!require('pacman')){install.packages('pacman')}
pacman::p_load(
  readxl,      # Lectura de los datos
  tidyverse ,  # Acceso al entorno de procesamiento "tidyverse"
  tidymodels,  # Acceso al entorno de modelado "tidymodels"
  agua,        # Modelaje de entorno h2o 
  themis,      # Soporte de la librería "recipes" para añadir sobremuestreo
  skimr,       # Descriptiva rápida de datos  
  naniar,
  ranger      # Renderización en html para este formato
)

## Cargamos los datos (está en la carpeta Tidymodels1 > Presentación_Tidymodels > Data)
## Eliminamos las filas con NA en la variable respuesta y la transformamos a factor.
datos <- read_xlsx('Data/datos.xlsx') %>% 
  filter(!is.na(Var_respuesta) ) %>% 
  mutate(Var_respuesta= as_factor(Var_respuesta))

## Visión general del dataset
skimr::skim(datos)

## Visualización de la distribución y correlación entre los NA de cada variable
naniar::gg_miss_upset(datos, nsets = 10, nintersects = 50)

## 2. RSAMPLE ----
## Comenzamos con las funciones del paquete RSample

## 2.1. Split ----
## Partimos el dataset en 60-40%
rsample::initial_split(datos, prop = 0.6 )

## 2.2. Bootstrap ----
## Crea un tibble con *n* cantidad de objetos “split” y su correspondiente identificador.
## En las muestras tipo bootsrap, los individuos pueden aparecer más de una vez en el conjunto de datos.
rsample::bootstraps(datos,5)

## 2.3. Rolling_origin ----
## Remuestreo con rolling_origin para el ejemplo
rolling_origin(datos, initial = 20, assess = 20, skip = 40, cumulative = FALSE)

## Inciso: 
## Otros ejemplos de remuestreo con rolling_origin y sus ggplot para visualizarlos

rolling_origin(datos, initial = 20, assess = 20, skip = 40, cumulative = FALSE)
rolling_origin(datos, initial = 50, assess = 20, skip = 70, cumulative = FALSE)
rolling_origin(datos, initial = 20, assess = 20, skip = 40, cumulative = TRUE)
rolling_origin(datos, initial = 50, assess = 50, skip = 70, cumulative = TRUE)

ggpubr::ggarrange(
  rolling_origin(datos, initial = 20, assess = 20, skip = 40, cumulative = FALSE) %>% 
    tidy() %>% 
    ggplot(aes(x = Resample, y = Row, fill = Data)) +
    geom_tile() + 
    scale_fill_manual( values = c('cyan','orange')) +
    scale_y_continuous(breaks = seq(0, 300, by = 50)) +
    theme_minimal()
  ,
  rolling_origin(datos, initial = 50, assess = 20, skip = 70, cumulative = FALSE) %>% 
    tidy() %>% 
    ggplot(aes(x = Resample, y = Row, fill = Data)) +
    geom_tile()  + 
    scale_fill_manual( values = c('cyan','orange')) +
    scale_y_continuous(breaks = seq(0, 300, by = 50)) +
    theme_minimal()
  ,
  rolling_origin(datos, initial = 20, assess = 20, skip = 40, cumulative = T) %>% 
    tidy() %>% 
    ggplot(aes(x = Resample, y = Row, fill = Data)) +
    geom_tile()  + 
    scale_fill_manual( values = c('cyan','orange')) +
    scale_y_continuous(breaks = seq(0, 300, by = 50)) +
    theme_minimal()
  ,
  rolling_origin(datos, initial = 50, assess = 50, skip = 70, cumulative = T) %>% 
    tidy() %>% 
    ggplot(aes(x = Resample, y = Row, fill = Data)) +
    geom_tile()  + 
    scale_fill_manual( values = c('cyan','orange')) +
    scale_y_continuous(breaks = seq(0, 300, by = 50)) +
    theme_minimal()
  ,
  common.legend = T, legend = 'bottom'
)

## 2.4. Splits ----
## Punto en las diapositivas: 4.2. Estratificación

set.seed(4147) ## Recordar que las semillas se tienen que ir fijando en cada ejecución del código.

## Sin estratificar 
Split_datos_no_strat <- initial_split(datos, prop = 0.70 )
# Añade 3/4 de los datos en el conjunto de entrenamiento
# 🔴 Initial_split con datos estratificados 🔴
Split_datos_strat <- initial_split(datos, prop = 0.70, strata = Var_respuesta)

## Visualización con los resultados distintos de 'Split_datos_no_strat' y 'Split_datos_strat'
ggpubr::ggarrange(
  ggpubr::ggarrange(
    training(Split_datos_no_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      ungroup() %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4) +
      scale_fill_manual( values=c("azure4", "azure3"))
    ,
    testing(Split_datos_no_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ungroup() %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4) +
      scale_fill_manual( values=c("azure4", "azure3")),
    common.legend = T, legend = 'bottom'
  ),
  ggpubr::ggarrange(
    training(Split_datos_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      ungroup() %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4)+
      scale_fill_manual( values=c("#E86EB7", "#108064"))
    ,
    testing(Split_datos_strat) %>% 
      group_by(Var_respuesta) %>% 
      summarise(recuento = n()) %>% 
      mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
      ungroup() %>% 
      ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(prop_var_respuesta)), position = position_stack(vjust = 0.5), size = 4)+
      scale_fill_manual( values=c("#E86EB7", "#108064")), 
    common.legend = T,
    legend = 'bottom'
  ),
  labels = c('No estratificados', 'Estratificados'), label.x = 0.1
) %>%  
  plot()

## 2.5. Training y Test ----
## Punto en las diapositivas: 4.3. Deshacer la partición

# 🔴 Deshacemos las particiones con las funciones training y testing 🔴

Training_datos <- training(Split_datos_strat)
Testing_datos  <- testing(Split_datos_strat)

## 2.6. Folds ----
## Punto en las diapositivas: 4.4. Folds, creando conjuntos de validación

# 🔴 Creamos Folds 🔴

Folds_training <- vfold_cv(Training_datos, v = 5 ,strata = Var_respuesta)

## 3. PRIMERAS RECETAS ----
## Punto en las diapositivas: 5.2.1. Hagamos un ejemplo

## 3.1. Receta básica ----
Ejemplo_receta <- Training_datos %>% 
  # 🔴 1. Recipe (crea la fórmula generalizada del modelo) 
  recipe(  
    as.formula("Var_respuesta ~ Cognicion_01 +Cognicion_02+Escala_01+Escala_02"),
    data = . , 
    strata = Var_respuesta )  %>% 
  # 🔴 2. step
  # 🟡 En este caso no aplicamos ninguna transformación 
  # Step_log
  # 🔴 3. Prep (asienta la receta y generalízala) 
  prep() %>% 
  # 🔴 4. Bake (pon la receta en producción/comprueba qué tal ha funcionado)
  bake(., new_data=NULL) ## new_data es para cuando tengas que pasarle otro datset al modelo

## 3.2. Receta Imputación Múltiple ----
## Punto en las diapositivas: 5.2.2. Imputación múltiple

Imputacion_multiple <- Training_datos %>% 
  # 🔴 1. Recipe (crea la fórmula generalizada del modelo) 
  recipe(  
    as.formula("Var_respuesta ~ Cognicion_01 +Cognicion_02+Escala_01+Escala_02"),
    data = . , 
    strata = Var_respuesta )  %>% 
  # 🔴 2. Step (haz la transformación que requieras) 
  # 🟡 Este step será la imputación por regresión lineal con otras covariables
  step_impute_linear( Cognicion_02, impute_with = imp_vars(Cognicion_01, Escala_02) ) %>%
  # 🔴 3. Prep (asienta la receta y generalízala) 
  prep() %>%
  # 🔴 4. Bake (pon la receta en producción/comprueba qué tal ha funcionado)
  bake(., new_data=NULL) 

## 3.3. Receta PCA ----

PCA <- Training_datos %>% 
  # 🔴 1. Recipe (crea la fórmula generalizada del modelo) 
  recipe(  
    as.formula("Var_respuesta ~ Cognicion_01 + Cognicion_02 + Cognicion_03 + Escala_01 + Escala_02  + Escala_03"),
    data = . , 
    strata = Var_respuesta )  %>% 
  # 🔴 2. Step (haz la transformación que requieras) 
  # 🟡 Este step será la imputación por regresión lineal con otras covariables
  step_impute_bag(all_numeric()) %>% 
  step_pca( Cognicion_01 , Cognicion_02 , Cognicion_03, num_comp = 2,  id = "pca") %>%
  # 🔴 3. Prep (asienta la receta y generalízala) 
  prep() %>%
  # 🔴 4. Bake (pon la receta en producción/comprueba qué tal ha funcionado)
  tidy(id = "pca") 


## 4. Remuestreo ----
## Punto en las diapositivas: 5.2.4. Remuestrear


# 🔴 Librería que permite instalar algoritmos y hacer sobremuestreo en Tidymodels.
pacman::p_load(themis)

Training_datos_adasyn <- Training_datos %>% 
  # 1. Recipe (crea la fórmula generalizada del modelo) 
  recipe(  
    as.formula("Var_respuesta ~ Cognicion_01 +Cognicion_02+Escala_01+Escala_02"),
    data = . , 
    strata = Var_respuesta )  %>% 
  #  2. Step (haz la transformación que requieras) 
  #  step de  imputación por árboles manteniendo la estructura de todas las covariables
  step_impute_bag(all_numeric()) %>% 
  # 🔴 step para sobremuestrear el nivel menor en la variable respuesta.
  step_adasyn(Var_respuesta, over_ratio = 1, neighbors = 10) %>% 
  #  3. Prep (asienta la receta y generalízala) 
  prep() %>%
  #  4. Bake (pon la receta en producción/comprueba qué tal ha funcionado)
  bake(., new_data=NULL)

## Visualización de los remuestreos Adasyn
ggpubr::ggarrange(
  Training_datos %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    ungroup() %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = scales::percent(prop_var_respuesta)), 
      position = position_stack(vjust = 0.5), size = 4) +
    scale_fill_manual( values=c("azure4", "azure3"))
  ,
  Training_datos_adasyn %>% 
    group_by(Var_respuesta) %>% 
    summarise(recuento = n()) %>% 
    mutate(prop_var_respuesta = recuento / sum(recuento) ) %>% 
    ungroup() %>% 
    ggplot(aes(x = Var_respuesta, y = recuento, fill = Var_respuesta)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = scales::percent(prop_var_respuesta)),
      position = position_stack(vjust = 0.5), size = 4) +
    scale_fill_manual( values=c("azure4", "azure3")),
  common.legend = T, legend = 'bottom'
  ,
  labels = c('Normal', 'Sobre Muestreo\n        Adasyn'), label.x = 0.1
) %>%  
  plot()

## 5. Receta para el modelo ----
## Punto en las diapositivas: 5.3. Caso práctico: Receta

## Preparación del dataset
Receta_modelo_1_formula <- Training_datos %>% 
  dplyr::select(
    dplyr::matches('Cog'), dplyr::matches('escala')) %>% 
  names() %>% 
  paste(., collapse = ' + ') %>% 
  paste('Var_respuesta ~' ,.) %>% 
  as.formula() 

## Receta

# 🔴 1.) Iniciamos la receta
Receta_modelo_1 <- recipe(  
  formula = Receta_modelo_1_formula,
  data = Training_datos , 
  strata = Var_respuesta)  %>% 
  # 🔴 2.) Steps
  #  🟡 2.1) Eliminamos las variables que contengan más de un 20% de datos perdidos 
  step_filter_missing(all_predictors(),  threshold = 0.1) %>% 
  #  🟡 2.2) Imputamos las variables numéricas con un algoritmo de bagged trees
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  #  🟡 2.3) Normalizamos los datos (restamos la media)
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  #  🟡 2.4) Escalamos los datos (reducimos a escala entre 0 y 1)
  step_scale(all_numeric(),-all_outcomes()) %>% 
  #  🟡 2.5) Convertimos a Dummy las variables factor (no hará nada por que no tenemos variables factor)
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  #  🟡 2.6) Sobre muestreamos la variable para equilibrar grupos
  step_adasyn(Var_respuesta, over_ratio = 1, neighbors = 10) 

## 6. PARSNIP ----

# 🔴 Ver los posibles motores de un tipo de modelo
show_engines('logistic_reg') 

## 6.1. Ejemplos de Hiperparámetros ----
## Punto en las diapositivas: 6.2. Empezamos a modelar

logistic_reg(
  engine = "glmnet",
  #Hiperparámetros           
  penalty = NULL, mixture = NULL, mode = 'classification')

logistic_reg(
  engine = "glmnet",
  #Hiperparámetros           
  penalty = 0, mixture = 0, mode = 'classification')

logistic_reg(
  engine = "glmnet",
  #Hiperparámetros           
  penalty = tune(), mixture = tune(), mode = 'classification')


## 6.2. Random Forest para el modelo ----
## Punto en las diapositivas: 6.3. Caso práctico: modelo de Random Forest

Model_RandomForest <- 
  # 🔴 Especificamos el modelo que queremos, en este caso un random forest
  rand_forest(
    # 🟡 Ajustamos los hiperparámetros 
    mtry = tune(),  trees = tune(),  min_n = tune()) %>% 
  # 🔴 Ponemos el motor, es decir, la librería por la queremos que se ejecute el modelo
  set_engine("ranger", importance = "impurity") %>% 
  # 🔴 Ajustamos el modo del modelo, es decir, si queremos regresión o clasifiación 
  set_mode("classification")


## 7. WORKFLOWS ----
## Punto en las diapositivas: 7. Unificando todo: Workflow

WF_Receta_modelo_1_Random_forest <- 
  # 🔴 Activamos el workflow 
  workflow() %>% 
  # 🔴 Añadimos la receta
  add_recipe(Receta_modelo_1) %>% 
  # 🔴 Añadimos el modelo
  add_model(Model_RandomForest)

## 8. YARDSTICK ----
## Punto en las diapositivas: 8. Eligiendo métricas con Yardstick

Modelo_Metricas <- metric_set(accuracy, j_index, precision, sensitivity, specificity, roc_auc, f_meas,recall, mcc)

## 9. TUNE Y DIALS ----
## Punto en las diapositivas: 9. Eligiendo hiperparámetros: Tune y Dials

## Punto en las diapositivas: 9.2.1. Grid_regular()

## `Grid_regular` hará las combinaciones acorde al rango que le hayamos dado. 
## Por defecto hace la combinación entre el mínimo, el medio y el máximo de cada valor.
grid_regular(
  mtry(range  = c(2, 10)),
  min_n(range = c(2, 10)),
  trees(range = c(2, 10)),
  levels = 4) # Podemos ampliar con el parámetro levels: Ahora el mínimo será el cuantil 33 y el máximo el cuantil 66.


## Punto en las diapositivas: 9.2.2. Grid_max_entropy()
grid_max_entropy(
  mtry(range  = c(1, 4)),
  min_n(range = c(10, 30)),
  trees(range = c(1, 1000)),
  size = 10
)


## 9.1. Hiperparámetros modelo ----
## Punto en las diapositivas: 9.3. Hiperparámetros: Dial & Tune + Yardstick

WF_hiper_parametros <- 
  # 🔴 tune_grid()🔴
  tune_grid(
    # Receta de nuestro modelo, incluye el workflow 
    object = WF_Receta_modelo_1_Random_forest,
    # 🟡 Ponemos los Folds del conjunto de entrenamiento,  
    resamples = Folds_training,
    # 🟡 Grid con diferentes hiperparámetros 
    grid = grid_max_entropy(
      mtry(range  = c(1, 4)),
      min_n(range = c(10, 30)),
      trees(range = c(1, 1000)),
      size = 10),
    # 🟡 Métricas definidas anteriormente
    metrics = Modelo_Metricas, 
    # 🟡 Con esta opción podemos guardar las predicciones
    control = control_grid( save_pred = T)
  )

WF_hiperparametros_coleccion <- 
  WF_hiper_parametros %>% 
  collect_metrics() # 🔴 Esta es la función que retorna las métricas, importante tenerla en cuenta

autoplot(WF_hiper_parametros)


WF_hiper_parametros %>% 
  collect_metrics() %>% 
  pivot_longer(cols = c('mtry','trees', 'min_n'), names_to = 'tipo_parametro', values_to = 'valor_parametro') %>% 
  ggplot(aes(valor_parametro,mean, color=mean)) +
  geom_point() +
  facet_grid(.metric~tipo_parametro, scales = 'free' ) +
  scale_color_viridis_b()

WF_hiperparametros_mejor <- 
  WF_hiper_parametros %>% show_best(metric = "roc_auc",n=1)


## 10. fit() y predict() ----
## Punto en las diapositivas: 10. La conversión de todo: fit() y predict()

Modelo_fitted <- WF_Receta_modelo_1_Random_forest %>% 
  # 🔴 Finalizamos el flujo de trabajo con la mejor combinación de Hiperparámetros encontrada
  finalize_workflow(WF_hiperparametros_mejor) %>%
  # 🔴 Finalmente ajustamos con fit()
  fit(data= Training_datos)


Nuevo_caso <- tibble(
  'Cognicion_01' = 123,
  'Cognicion_02' = 102,
  'Cognicion_03' = 89,
  'Cognicion_04' = 100,
  'Cognicion_05' = 179,
  'Escala_01'    = 12,
  'Escala_02'    = 22,
  'Escala_03'    = 20
)

tibble(
  predict(Modelo_fitted, Nuevo_caso, type = "prob" ),
  predict(Modelo_fitted, Nuevo_caso, type = "class"))

## 10.1. last_fit() ----
## Punto en las diapositivas: 10.1. La función last_fit()

Modelo_1_final <-  WF_Receta_modelo_1_Random_forest %>% 
  # 🔴 Finalizamos el flujo de trabajo con la mejor combinación de hiperparámetros encontrada
  finalize_workflow(WF_hiperparametros_mejor) %>%
  # 🔴 Ajustamos con last_fit(), poniendo el split de datos inicial y las métricas para las predicciones
  last_fit(Split_datos_strat, metrics = Modelo_Metricas)


WF_Receta_modelo_1_Random_forest %>%
  # 🔴 Finalizamos el flujo de trabajo con la mejor combinación de hiperparámetros encontrada
  finalize_workflow(WF_hiperparametros_mejor) %>%
  # 🔴 Ajustamos con last_fit(), poniendo el split de datos inicial y las métricas para las predicciones
  last_fit(Split_datos_strat, metrics = Modelo_Metricas) %>%
  # 🔴 dentro del flujo de trabajo están la receta y el modelo ya ajustado
  extract_workflow()

Modelo_1_final %>%
  # 🔴 Con esta función podemos recopilar todas las métricas de un modelo
  collect_metrics() %>% 
  select(-.estimator,-.config) %>%  
  mutate(.estimate= round(.estimate,2))


options(yardstick.event_first = FALSE)

Modelo_1_final %>%  collect_predictions() %>% 
  # 🔴 Esta funcion permite calcular rapidamente la curva roc de un modelo ya ajustado
  roc_curve(truth = Var_respuesta,  .pred_0 ) %>% 
  ggplot(aes( x = 1- specificity ,y= sensitivity ) ) +
  geom_path() +
  geom_abline(intercept=0, slope=1, linetype=3) +
  labs(title= 'Cruva roc del modelo_1_final')


## 11. Threshold performance ----
## Punto en las diapositivas: 11. Threshold performance

pacman::p_load(probably)

Modelo_1_final_Threshold_performance <- 
  Modelo_1_final %>% 
  collect_predictions() %>% 
  threshold_perf(
    # Le decimos qué variable es la verdadera respuesta
    truth = Var_respuesta,  
    # Le decimos qué variable es la predicción
    .pred_1, 
    # Rango en el que se evaluará el umbral en cada punto
    thresholds = seq(0.5,1,0.01) )

Modelo_1_final_Threshold_performance %>% 
  ggplot(aes(x=.threshold,y=.estimate ,color=.metric)) +
  geom_line(size=1.1)+
  scale_color_manual(values=c('purple','blue','pink'))+
  geom_vline(xintercept = 
               Modelo_1_final_Threshold_performance %>%  
               filter(.metric=='j_index') %>%
               arrange(desc(.estimate)) %>% 
               slice(1) %>% 
               pull(.threshold), linetype = 'dashed', size=1.5)+
  theme_minimal()

## 12. Workflow_set() ----
## Punto en las diapositivas: 12. workflow_set() para unificarlas todas

Receta_modelo_Escalas_formula <- Training_datos %>% 
  dplyr::select(dplyr::matches('Esc')) %>% 
  names() %>% 
  paste(., collapse = ' + ') %>% 
  paste('Var_respuesta ~' ,.) %>% 
  as.formula() 

Receta_modelo_Cognicion_formula <- Training_datos %>% 
  dplyr::select(
    dplyr::matches('Cog')) %>% 
  names() %>% 
  paste(., collapse = ' + ') %>% 
  paste('Var_respuesta ~' ,.) %>% 
  as.formula() 


Receta_modelo_Escalas <- recipe(  
  formula = Receta_modelo_Escalas_formula,
  data = Training_datos , 
  strata = Var_respuesta)  %>% 
  # 🔴 2.) Steps
  #  🟡 2.1) Eliminamos las variables que contengan más de un 25% de datos perdidos 
  step_filter_missing(all_predictors(),  threshold = 0.25) %>% 
  #  🟡 2.2) Imputamos las variables numéricas con un algoritmo de bagged trees
  step_impute_bag(all_numeric(),-all_outcomes()) %>%
  # 2.3) Normalizamos los datos (restamos la media)
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  # 2.4) Escalamos los datos (reducimos a escala entre 0 y 1)
  step_scale(all_numeric(),-all_outcomes()) %>% 
  # 2.5) Convertimos a Dummy las variables factor (no hará nada por que no tenemos variables factor)
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  # 2.6) Sobremuestreamos la variable para equilibrar grupos
  step_adasyn(Var_respuesta, over_ratio = 1, neighbors = 10) 

# 🔴 1.) Iniciamos la receta
Receta_modelo_Cognicion <- recipe(  
  formula = Receta_modelo_Cognicion_formula,
  data = Training_datos , 
  strata = Var_respuesta)  %>% 
  # 🔴 2.) Steps
  #  🟡 2.1) Eliminamos las variables que contengan más de un 25% de datos perdidos 
  step_filter_missing(all_predictors(),  threshold = 0.25) %>%
  #  🟡 2.2) Imputamos las variables numéricas con un algoritmo de bagged trees
  step_impute_mean(all_numeric(),-all_outcomes()) %>%
  # 2.3) Normalizamos los datos (restamos la media)
  step_normalize(all_numeric(),-all_outcomes() ) %>% 
  # 2.4) Escalamos los datos (reducimos a escala entre 0 y 1)
  step_scale(all_numeric(),-all_outcomes()) %>% 
  # 2.5) Convertimos a Dummy las variables factor (no hará nada por que no tenemos variables factor)
  step_dummy(all_nominal(), -all_outcomes(), one_hot = T) %>% 
  # 2.6) Sobremuestreamos la variable para equilibrar grupos
  step_adasyn(Var_respuesta, over_ratio = 1, neighbors = 10)

Receta_list <- list(
  'Modelo_Escalas'    = Receta_modelo_Escalas,
  'Modelo_Cognicion'  = Receta_modelo_Cognicion
)

Model_list <- list(
  RandomForest     = Model_RandomForest
  # ,XGBoost          = Model_XGBoost
  # ,Bagged_Trees     = Model_Bagged_Tree
)


Wokflows_set <- workflow_set(
  preproc = Receta_list, 
  models = Model_list, 
  # 🔴 La opción Cross es para que haga todos los cruces de modelos con recetas
  cross = T)


Wokflows_set_map <- 
  Wokflows_set %>% 
  # 🔴 workflow_map() es una función que permite ajustar múltiples flujos de trabajo
  workflow_map(
    resamples = Folds_training, 
    fn = "tune_grid",
    grid = grid_max_entropy(
      mtry(range  = c(1, 4)),
      min_n(range = c(10, 30)),
      trees(range = c(1, 1000)),
      size = 10),
    # verbose = TRUE, 
    metrics = Modelo_Metricas, 
    control = control_grid( save_pred = T),
    # 🔴 para garantizar replicabildiad, podemos fijar la semilla en el proceso
    seed = 2465)

# 12.1. Modelo Escalas ----
Modelo_Escalas_RandomForest_hyperparametros <- Wokflows_set_map %>% 
  # 🔴 Extraemos el flujo correspondiente al modelo que queremos 
  extract_workflow_set_result('Modelo_Escalas_RandomForest') %>%
  # 🔴 podemos ver una tabla con los mejores resultados, acorde a una métrica
  show_best(metric = 'sensitivity', n=50) %>% 
  filter(row_number()==1) %>% 
  select(mtry,trees,min_n,.config )

Modelo_Escalas_RandomForest_final <- Wokflows_set_map %>% 
  # 🔴 Extraemos el flujo correspondiente al modelo que queremos 
  extract_workflow('Modelo_Escalas_RandomForest') %>% 
  finalize_workflow(Modelo_Escalas_RandomForest_hyperparametros) %>%
  last_fit(Split_datos_strat, metrics = Modelo_Metricas )

# 12.2. Modelo Cognición ----

Modelo_Cognicion_RandomForest_hyperparametros <- Wokflows_set_map %>% 
  extract_workflow_set_result('Modelo_Cognicion_RandomForest') %>%
  show_best(metric = 'sensitivity', n=50) %>% 
  filter(row_number()==1) %>% 
  select(mtry,trees,min_n,.config )

Modelo_Cognicion_RandomForest_final <- Wokflows_set_map %>% 
  extract_workflow('Modelo_Cognicion_RandomForest') %>% 
  finalize_workflow(Modelo_Cognicion_RandomForest_hyperparametros) %>%
  last_fit(Split_datos_strat, metrics = Modelo_Metricas )

## 12.3. Métricas de todos los modelos ----
## Punto en las diapositivas: 12.3. Métricas de todos los modelos
map2(
  list(
    Modelo_Escalas_RandomForest_final,
    Modelo_Cognicion_RandomForest_final
  ),
  list(
    'Modelo_Escala',
    'Modelo_Cognicion'
  ),
  ~ ..1 %>% 
    collect_metrics() %>% 
    mutate('Modelo'= ..2) %>% 
    select(.metric,.estimate,Modelo )) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = Modelo, values_from = .estimate)


## 12.4. Curvas ROC ----
## Punto en las diapositivas: 12.4. Curvas roc de todos los modelos

ROC_CURVE_training_todos_los_posibles_modelos <- Wokflows_set_map %>%  
  collect_predictions() %>%
  group_split(wflow_id) %>% 
  set_names(list('Escala','Cognicion')) %>% 
  map(~ .x %>% roc_curve(truth=Var_respuesta , .pred_0)) %>% 
  map2(., list('Escala','Cognicion'),
       ~ .x %>%  mutate(model= .y)) %>% 
  bind_rows() 

Plot_ROC_CURVE_training_todos_los_posibles_modelos <- ROC_CURVE_training_todos_los_posibles_modelos %>% 
  ggplot(aes( x = 1- specificity ,y= sensitivity, color= model ) ) +
  geom_path() +
  geom_abline(intercept=0, slope=1, linetype=3) +
  theme_minimal()



ROC_CURVE_training_modelos_finales <- list(
  list('Esc' ,Modelo_Escalas_RandomForest_hyperparametros$.config),
  list('Cog' ,Modelo_Cognicion_RandomForest_hyperparametros$.config)) %>% 
  map(~ Wokflows_set_map %>% 
        collect_predictions() %>% 
        filter(
          str_detect(wflow_id, .x[[1]]) & 
            .config== .x[[2]]
        )) %>% 
  
  map(~ .x %>%
        mutate(Model= str_extract(wflow_id, '^.{2}') ) %>% 
        select(Model,Var_respuesta, .pred_0, .pred_1, .pred_class)
  ) %>% 
  set_names(list('Escala','Cognicion') ) %>%
  map(~ .x %>% roc_curve(truth = Var_respuesta, .pred_0)) %>% 
  map2(., list('Escala','Cognicion'),
       ~ .x %>% mutate(model= .y)) %>% 
  bind_rows() 

Plot_ROC_CURVE_training_modelos_finales <- ROC_CURVE_training_modelos_finales %>% 
  ggplot(aes( x = 1- specificity ,y= sensitivity, color= model ) ) +
  geom_path() +
  geom_abline(intercept=0, slope=1, linetype=3) +
  theme_minimal()


ROC_CURVE_testing_modelos_finales <-list(
  Modelo_Escalas_RandomForest_final,
  Modelo_Cognicion_RandomForest_final
) %>% 
  map(~.x %>% 
        collect_predictions() %>% 
        roc_curve(Var_respuesta, .pred_0)) %>% 
  map2(., list('Escala','Cognicion'),
       ~ .x %>%  mutate(model= .y)) %>% 
  bind_rows()

Plot_ROC_CURVE_testing_modelos_finales <- ROC_CURVE_testing_modelos_finales %>% 
  ggplot(aes(x = 1- specificity ,y = sensitivity, color = model ) ) +
  geom_path()+
  geom_abline(intercept=0, slope=1, linetype=3) +
  theme_minimal()

ggpubr::ggarrange( Plot_ROC_CURVE_training_todos_los_posibles_modelos,
                   Plot_ROC_CURVE_training_modelos_finales, 
                   Plot_ROC_CURVE_testing_modelos_finales ,
                   nrow=1 , 
                   common.legend = T,
                   legend="bottom",
                   labels = c('Todos_training','Training','Test'))

## 13. SHAP values ----
## Punto en las diapositivas: 13. SHAP values 

h2o_start()

modelo_h20 <- rand_forest() %>% 
  set_engine("h2o", max_runtime_secs = 20) %>% 
  set_mode('classification') %>% 
  fit(Var_respuesta ~ ., data = Receta_modelo_Escalas %>% prep() %>% bake(new_data = NULL) )

entrada_h20 <- h2o::as.h2o(
  Receta_modelo_Escalas %>% prep() %>% bake(new_data = NULL) %>%
    dplyr::rename(.outcome = Var_respuesta))

modelo_h20 %>%  
  extract_fit_engine() %>% 
  h2o::h2o.shap_summary_plot(entrada_h20)

h2o_end()
