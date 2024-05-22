
# https://juliasilge.com/blog/educational-attainment/


library(tidyverse)
library(tidymodels)
library(future)

education <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

glimpse(education)

# EDA

education %>% 
  filter(!is.na(income_flag)) %>% 
  mutate(income_flag = factor(income_flag, levels = c("Lower deprivation towns",
                                                      "Mid deprivation towns",
                                                      "Higher deprivation towns",
                                                      "Cities"))) %>% 
  ggplot(aes(education_score, income_flag, fill = size_flag)) +
  geom_boxplot(alpha = 0.2) +
  labs(y = NULL, fill = NULL)




set.seed(1235678)

edu_split <-  education %>% 
  # Aislamos aquellos lugares donde hay deprivacion en la ciudades
  filter(str_detect(income_flag, "deprivation towns")) %>% 
  # Guardamos algunas variables para modelar 
  select(education_score, income_flag, size_flag, coastal, university_flag, rgn11nm) %>% 
  initial_split(strata = education_score)

edu_train <- training(edu_split)
edu_test <- testing(edu_split)

set.seed(1235678)

edu_folds <- bootstraps(edu_train, strata = education_score)
edu_folds

# Preparamos para paralelizar

plan(multisession, workers = 4)

edu_wf <- workflow(
  education_score ~ ., 
  rand_forest(mode = "regression", trees = 500)
)

set.seed(1235678)
edu_res <- fit_resamples(edu_wf, edu_folds)
collect_metrics(edu_res)

edu_fit <- last_fit(edu_wf, edu_split)
collect_metrics(edu_fit)


collect_predictions(edu_fit) |>
  ggplot(aes(education_score, .pred)) +
  geom_abline(slope = 1, lty = 2, linewidth = 1.5, alpha = 0.7) +
  geom_point() +
  coord_fixed()





