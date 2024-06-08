# source('Scripts/Lectura_datos_basales.R')

if(!require('pacman')){install.packages('pacman')}
pacman::p_load(tidyverse,survey, tableone )

datos_imputados <- readRDS('Taller_Propensity_Score/Ejemplo_propensity_score/Datos_imputados.rds') %>% 
  select(NHC, identificador, everything())

datos_imputados_transformados <- datos_imputados %>% 
  mutate(
    DIabetes = droplevels(DIabetes,c('Metformina', 'Altres','ADO', 'dieta', 'ADO+Insulina','metformina+altres','ADO')),
    SignesIndirectes_HTP = droplevels(SignesIndirectes_HTP,c('dubtós')),
    presenciaCSPH = case_when(presenciaCSPH =='indeterminat' ~ 'no CSPH', TRUE ~ presenciaCSPH)
  ) %>%
  select('NHC','identificador','Grup_IQ','edat_IQ','sexe_home',"IMC",'etiol_OH','Enol_Actiu',
         "Charlson_Index","plaquetes_preIQ","DIabetes",'Pughpunts_basal',
         'colaterals_shunts','MELD_basal','Alb_gL_preIQ','INR_preIQ','BB_mgdL_preIQ','Creat_mgdL_preIQ',
         'HCC_prev','SignesIndirectes_HTP')

variables_propensity <- c(
  'Grup_IQ','edat_IQ','sexe_home',"IMC",'etiol_OH','Enol_Actiu',
  "Charlson_Index","plaquetes_preIQ","DIabetes",'Pughpunts_basal',
  'colaterals_shunts','MELD_basal','Alb_gL_preIQ','INR_preIQ','BB_mgdL_preIQ','Creat_mgdL_preIQ',
  'HCC_prev','SignesIndirectes_HTP'
)


mod_propensity <- glm(
  formula = Grup_IQ ~  
    SignesIndirectes_HTP + 
    HCC_prev +
    DIabetes +
    sexe_home +
    Enol_Actiu + 
    plaquetes_preIQ +
    log(sqrt(Charlson_Index)) +
    Alb_gL_preIQ +
    log(IMC) +
    Pughpunts_basal +
    log(BB_mgdL_preIQ) +
    Creat_mgdL_preIQ +
    INR_preIQ
    , 
  data = datos_imputados_transformados, 
  family=binomial(link="logit") )

datos_imputados_propensity <- datos_imputados_transformados %>% 
  mutate(prediciones=predict(mod_propensity,datos_imputados_transformados,type="response") )

datos_imputados_propensity <- datos_imputados_propensity %>% 
  mutate(standarized_weights = case_when(
    Grup_IQ == 'Si IQ' ~ (177/(371))/ (  prediciones),
    Grup_IQ == 'No IQ' ~ (194/(371))/ (1-prediciones) 
  )) 

datos_imputados_propensity

# Filtrado de pacientes excluídos por no encajar en ningún propensity score ----

Pacientes_excluidos_propensity <- datos_imputados_propensity %>% 
  filter(!between(prediciones,0.1,0.9)) %>% 
  select(-c(prediciones,standarized_weights))

Pacientes_excluidos_propensity$identificador
# Filtrado de pacientes incluidos por  encajar en  propensity score ---- 

Pacientes_incluidos_propensity <- datos_imputados_propensity %>% 
  filter(between(prediciones,0.1,0.9)) %>% 
  select(-c(prediciones,standarized_weights))

Pacientes_incluidos_propensity

mod_propensity <- glm(
  formula = Grup_IQ ~  
    SignesIndirectes_HTP + 
    HCC_prev +
    DIabetes +
    sexe_home +
    Enol_Actiu + 
    plaquetes_preIQ +
    log(sqrt(Charlson_Index)) +
    Alb_gL_preIQ +
    log(IMC) +
    Pughpunts_basal +
    log(BB_mgdL_preIQ) +
    Creat_mgdL_preIQ +
    INR_preIQ
  , 
  data = Pacientes_incluidos_propensity, 
  family=binomial(link="logit") )

Pacientes_incluidos_propensity <- Pacientes_incluidos_propensity %>% 
  mutate(prediciones=predict(mod_propensity,Pacientes_incluidos_propensity,type="response") ) %>% 
  mutate(standarized_weights = case_when(
    Grup_IQ == 'Si IQ' ~ (177/(371))/ (  prediciones),
    Grup_IQ == 'No IQ' ~ (194/(371))/ (1-prediciones) 
  )) 

# datos_imputados_propensity %>%  write_rds(., 'Datos/Datos_propensity.rds')


# Datos propensity ----

Pacientes_incluidos_iptwdatos_propensity <- svydesign(
  ids = ~ 1, 
  data = Pacientes_incluidos_propensity,
  strata = ~Grup_IQ,
  weights = ~ Pacientes_incluidos_propensity$standarized_weights)

Pacientes_incluidos_Propensity_table_Weighted <- svyCreateTableOne(
  vars= Pacientes_incluidos_propensity %>% select(-c(Grup_IQ, NHC, identificador)) %>% names(),
  strata = "Grup_IQ", 
  data = Pacientes_incluidos_iptwdatos_propensity, 
  addOverall = T,
  includeNA = T,
  smd =TRUE)

print(Pacientes_incluidos_Propensity_table_Weighted, smd = TRUE)

# datos_imputados_propensity %>%
#   ggplot(aes(standarized_weights, fill= Grup_IQ  )) +
#   geom_density()+
#   labs(x= 'probabilidad de pertenecer grup Si en propensity')
# 
# 
# datos_imputados_propensity %>%
#   ggplot(aes(prediciones, fill= Grup_IQ )) +
#   geom_histogram(color='black',alpha=0.8) +
#   labs(x= 'probabilidad de pertenecer grup Si en propensity')


