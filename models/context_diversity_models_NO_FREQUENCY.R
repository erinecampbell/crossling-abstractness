library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# context diversity

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_CD_model <- glm(produces ~ age + chinese_CD_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_CD_effect <- ggeffect(chinese_beijing_CD_model, terms = "chinese_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_CD_model$coefficients[[3]])
chinese_beijing_CD_summary <- summary(chinese_beijing_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_CD_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_CD_interaction_model <- glm(produces ~ age * chinese_CD_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_CD_interaction_summary <- summary(chinese_beijing_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_CD_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_CD_model <- glm(produces ~ age + chinese_CD_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_CD_effect <- ggeffect(chinese_cantonese_CD_model, terms = "chinese_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_CD_model$coefficients[[3]])
chinese_cantonese_CD_summary <- summary(chinese_cantonese_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_CD_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_CD_interaction_model <- glm(produces ~ age * chinese_CD_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_CD_interaction_summary <- summary(chinese_cantonese_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_CD_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_CD_model <- glm(produces ~ age + chinese_CD_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_CD_effect <- ggeffect(chinese_taiwanese_CD_model, terms = "chinese_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_CD_model$coefficients[[3]])
chinese_taiwanese_CD_summary <- summary(chinese_taiwanese_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_CD_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_CD_interaction_model <- glm(produces ~ age * chinese_CD_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_CD_interaction_summary <- summary(chinese_taiwanese_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_CD_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_CD_model <- glm(as.factor(produces) ~ age + croatian_CD_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_CD_effect <- ggpredict(croatian_CD_model, terms = "croatian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_CD_model$coefficients[[3]])
croatian_CD_summary <- summary(croatian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_CD_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_CD_interaction_model <- glm(as.factor(produces) ~ age * croatian_CD_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_CD_interaction_summary <- summary(croatian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_CD_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_CD_model <- glm(as.factor(produces) ~ age + czech_CD_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_CD_effect <- ggpredict(czech_CD_model, terms = "czech_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_CD_model$coefficients[[3]])
czech_CD_summary <- summary(czech_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_CD_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_CD_interaction_model <- glm(as.factor(produces) ~ age * czech_CD_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_CD_interaction_summary <- summary(czech_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_CD_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_CD_model <- glm(as.factor(produces) ~ age + danish_CD_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_CD_effect <- ggpredict(danish_CD_model, terms = "danish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_CD_model$coefficients[[3]])
danish_CD_summary <- summary(danish_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_CD_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_CD_interaction_model <- glm(as.factor(produces) ~ age * danish_CD_rating +  lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_CD_interaction_summary <- summary(danish_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_CD_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_CD_model <- glm(as.factor(produces) ~ age + dutch_CD_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_CD_effect <- ggpredict(dutch_CD_model, terms = "dutch_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_CD_model$coefficients[[3]])  
dutch_CD_summary <- summary(dutch_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_CD_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_CD_interaction_model <- glm(as.factor(produces) ~ age * dutch_CD_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_CD_interaction_summary <- summary(dutch_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_CD_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_CD_model <- glm(produces ~ age + english_CD_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_CD_effect <- ggeffect(english_american_CD_model, terms = "english_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_CD_model$coefficients[[3]])
english_american_CD_summary <- summary(english_american_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_CD_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_CD_interaction_model <- glm(produces ~ age * english_CD_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_CD_interaction_summary <- summary(english_american_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_CD_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_CD_model <- glm(produces ~ age + english_CD_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_CD_effect <- ggeffect(english_australian_CD_model, terms = "english_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_CD_model$coefficients[[3]])
english_australian_CD_summary <- summary(english_australian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_CD_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_CD_interaction_model <- glm(produces ~ age * english_CD_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_CD_interaction_summary <- summary(english_australian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_CD_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_CD_model <- glm(produces ~ age + english_CD_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_CD_effect <- ggeffect(english_british_CD_model, terms = "english_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_CD_model$coefficients[[3]])
english_british_CD_summary <- summary(english_british_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_CD_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_CD_interaction_model <- glm(produces ~ age * english_CD_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_CD_interaction_summary <- summary(english_british_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_CD_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_CD_model <- glm(produces ~ age + english_CD_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_CD_effect <- ggeffect(english_irish_CD_model, terms = "english_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_CD_model$coefficients[[3]])
english_irish_CD_summary <- summary(english_irish_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_CD_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_CD_interaction_model <- glm(produces ~ age * english_CD_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_CD_interaction_summary <- summary(english_irish_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_CD_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_CD_model <- glm(as.factor(produces) ~ age + finnish_CD_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_CD_effect <- ggpredict(finnish_CD_model, terms = "finnish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_CD_model$coefficients[[3]]) 
finnish_CD_summary <- summary(finnish_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_CD_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_CD_interaction_model <- glm(as.factor(produces) ~ age * finnish_CD_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_CD_interaction_summary <- summary(finnish_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_CD_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_CD_model <- glm(as.factor(produces) ~ age + french_CD_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_CD_effect <- ggpredict(french_european_CD_model, terms = "french_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_CD_model$coefficients[[3]]) 
french_european_CD_summary <- summary(french_european_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_CD_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_CD_interaction_model <- glm(as.factor(produces) ~ age * french_CD_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_CD_interaction_summary <- summary(french_european_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_CD_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_CD_model <- glm(as.factor(produces) ~ age + french_CD_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_CD_effect <- ggpredict(french_quebecois_CD_model, terms = "french_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_CD_model$coefficients[[3]]) 
french_quebecois_CD_summary <- summary(french_quebecois_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_CD_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_CD_interaction_model <- glm(as.factor(produces) ~ age * french_CD_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_CD_interaction_summary <- summary(french_quebecois_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_CD_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_CD_model <- glm(as.factor(produces) ~ age + german_CD_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_CD_effect <- ggpredict(german_CD_model, terms = "german_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_CD_model$coefficients[[3]]) 
german_CD_summary <- summary(german_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_CD_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_CD_interaction_model <- glm(as.factor(produces) ~ age * german_CD_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_CD_interaction_summary <- summary(german_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_CD_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_CD_model <- glm(as.factor(produces) ~ age + greek_CD_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_CD_effect <- ggpredict(greek_CD_model, terms = "greek_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_CD_model$coefficients[[3]]) 
greek_CD_summary <- summary(greek_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_CD_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_CD_interaction_model <- glm(as.factor(produces) ~ age * greek_CD_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_CD_interaction_summary <- summary(greek_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_CD_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_CD_model <- glm(as.factor(produces) ~ age + hebrew_CD_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_CD_effect <- ggpredict(hebrew_CD_model, terms = "hebrew_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_CD_model$coefficients[[3]]) 
hebrew_CD_summary <- summary(hebrew_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_CD_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_CD_interaction_model <- glm(as.factor(produces) ~ age * hebrew_CD_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_CD_interaction_summary <- summary(hebrew_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_CD_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_CD_model <- glm(as.factor(produces) ~ age + hungarian_CD_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_CD_effect <- ggpredict(hungarian_CD_model, terms = "hungarian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_CD_model$coefficients[[3]])
hungarian_CD_summary <- summary(hungarian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_CD_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_CD_interaction_model <- glm(as.factor(produces) ~ age * hungarian_CD_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_CD_interaction_summary <- summary(hungarian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_CD_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_CD_model <- glm(as.factor(produces) ~ age + irish_CD_rating + + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_CD_effect <- ggpredict(irish_CD_model, terms = "irish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_CD_model$coefficients[[3]])
irish_CD_summary <- summary(irish_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_CD_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_CD_interaction_model <- glm(as.factor(produces) ~ age * irish_CD_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_CD_interaction_summary <- summary(irish_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_CD_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_CD_model <- glm(produces ~ age + italian_CD_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_CD_effect <- ggeffect(italian_CD_model, terms = "italian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_CD_model$coefficients[[3]])
italian_CD_summary <- summary(italian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_CD_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_CD_interaction_model <- glm(produces ~ age * italian_CD_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_CD_interaction_summary <- summary(italian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_CD_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_CD_model <- glm(produces ~ age + kigiriama_CD_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_CD_effect <- ggeffect(kigiriama_CD_model, terms = "kigiriama_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_CD_model <- glm(produces ~ age + kiswahili_CD_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_CD_effect <- ggeffect(kiswahili_CD_model, terms = "kiswahili_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_CD_model$coefficients[[3]])
kiswahili_CD_summary <- summary(kiswahili_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_CD_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_CD_interaction_model <- glm(produces ~ age * kiswahili_CD_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_CD_interaction_summary <- summary(kiswahili_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_CD_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_CD_model <- glm(produces ~ age + korean_CD_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_CD_effect <- ggeffect(korean_CD_model, terms = "korean_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_CD_model$coefficients[[3]])
korean_CD_summary <- summary(korean_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_CD_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_CD_interaction_model <- glm(produces ~ age * korean_CD_rating +lexical_category, data = korean_instrument_data, family = "binomial")
korean_CD_interaction_summary <- summary(korean_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_CD_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_CD_model <- glm(produces ~ age + latvian_CD_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_CD_effect <- ggeffect(latvian_CD_model, terms = "latvian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_CD_model$coefficients[[3]])
latvian_CD_summary <- summary(latvian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_CD_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_CD_interaction_model <- glm(produces ~ age * latvian_CD_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_CD_interaction_summary <- summary(latvian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_CD_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_CD_model <- glm(produces ~ age + norwegian_CD_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_CD_effect <- ggeffect(norwegian_CD_model, terms = "norwegian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_CD_model$coefficients[[3]])
norwegian_CD_summary <- summary(norwegian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_CD_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_CD_interaction_model <- glm(produces ~ age * norwegian_CD_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_CD_interaction_summary <- summary(norwegian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_CD_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_CD_model <- glm(produces ~ age + persian_CD_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_CD_effect <- ggeffect(persian_CD_model, terms = "persian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_CD_model$coefficients[[3]])
persian_CD_summary <- summary(persian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_CD_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_CD_interaction_model <- glm(produces ~ age * persian_CD_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_CD_interaction_summary <- summary(persian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_CD_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_CD_model <- glm(produces ~ age + portuguese_CD_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_CD_effect <- ggeffect(portuguese_CD_model, terms = "portuguese_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_CD_model <- glm(produces ~ age + russian_CD_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_CD_effect <- ggeffect(russian_CD_model, terms = "russian_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_CD_model$coefficients[[3]])
russian_CD_summary <- summary(russian_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_CD_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_CD_interaction_model <- glm(produces ~ age * russian_CD_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_CD_interaction_summary <- summary(russian_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_CD_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_CD_model <- glm(produces ~ age + slovak_CD_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_CD_effect <- ggeffect(slovak_CD_model, terms = "slovak_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_CD_model$coefficients[[3]])
slovak_CD_summary <- summary(slovak_CD_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_CD_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_CD_interaction_model <- glm(produces ~ age * slovak_CD_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_CD_interaction_summary <- summary(slovak_CD_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_CD_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_CD_model <- glm(produces ~ age + spanish_CD_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_CD_effect <- ggeffect(spanish_argentinian_CD_model, terms = "spanish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_CD_model$coefficients[[3]])
spanish_argentinian_CD_summary <- summary(spanish_argentinian_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_CD_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_CD_interaction_model <- glm(produces ~ age * spanish_CD_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_CD_interaction_summary <- summary(spanish_argentinian_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_CD_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_CD_model <- glm(produces ~ age + spanish_CD_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_CD_effect <- ggeffect(spanish_chilean_CD_model, terms = "spanish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_CD_model$coefficients[[3]])
spanish_chilean_CD_summary <- summary(spanish_chilean_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_CD_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_CD_interaction_model <- glm(produces ~ age * spanish_CD_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_CD_interaction_summary <- summary(spanish_chilean_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_CD_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_CD_model <- glm(produces ~ age + spanish_CD_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_CD_effect <- ggeffect(spanish_european_CD_model, terms = "spanish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_CD_model$coefficients[[3]])
spanish_european_CD_summary <- summary(spanish_european_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_CD_rating") %>%
  mutate(language = "spanish_european")
spanish_european_CD_interaction_model <- glm(produces ~ age * spanish_CD_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_CD_interaction_summary <- summary(spanish_european_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_CD_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_CD_model <- glm(produces ~ age + spanish_CD_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_CD_effect <- ggeffect(spanish_mexican_CD_model, terms = "spanish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_CD_model$coefficients[[3]])
spanish_mexican_CD_summary <- summary(spanish_mexican_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_CD_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_CD_interaction_model <- glm(produces ~ age * spanish_CD_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_CD_interaction_summary <- summary(spanish_mexican_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_CD_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_CD_model <- glm(produces ~ age + spanish_CD_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_CD_effect <- ggeffect(spanish_peruvian_CD_model, terms = "spanish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_CD_model$coefficients[[3]])
spanish_peruvian_CD_summary <- summary(spanish_peruvian_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_CD_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_CD_interaction_model <- glm(produces ~ age * spanish_CD_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_CD_interaction_summary <- summary(spanish_peruvian_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_CD_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_CD_model <- glm(produces ~ age + swedish_CD_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_CD_effect <- ggeffect(swedish_CD_model, terms = "swedish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_CD_model$coefficients[[3]])
swedish_CD_summary <- summary(swedish_CD_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_CD_rating") %>%
  mutate(language = "swedish")
swedish_CD_interaction_model <- glm(produces ~ age * swedish_CD_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_CD_interaction_summary <- summary(swedish_CD_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_CD_rating") %>%
  mutate(language = "swedish")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_CD_model <- glm(produces ~ age + turkish_CD_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_CD_effect <- ggeffect(turkish_CD_model, terms = "turkish_CD_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_CD_effects <- bind_rows(
                                      chinese_beijing_CD_effect,
                                      chinese_cantonese_CD_effect,
                                      chinese_taiwanese_CD_effect,
                                      croatian_CD_effect,
                                      czech_CD_effect,
                                      english_american_CD_effect,
                                      english_australian_CD_effect,
                                      english_british_CD_effect,
                                      english_irish_CD_effect,
                                      danish_CD_effect,
                                      dutch_CD_effect,
                                      italian_CD_effect,
                                      finnish_CD_effect,
                                      french_european_CD_effect,
                                      french_quebecois_CD_effect,
                                      german_CD_effect,
                                      greek_CD_effect,
                                      hebrew_CD_effect,
                                      hungarian_CD_effect,
                                      irish_CD_effect,
                                      kiswahili_CD_effect,
                                      korean_CD_effect,
                                      latvian_CD_effect,
                                      norwegian_CD_effect,
                                      persian_CD_effect,
                                      russian_CD_effect,
                                      slovak_CD_effect,
                                      spanish_argentinian_CD_effect,
                                      spanish_chilean_CD_effect,
                                      spanish_european_CD_effect,
                                      spanish_mexican_CD_effect,
                                      spanish_peruvian_CD_effect,
                                      swedish_CD_effect
                                      # , turkish_CD_effect
)
write_rds(all_CD_effects, "models/effects/all_CD_effects.rds")

all_CD_effects_plot <- ggplot(all_CD_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Context Diversity Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_CD_effects_plots.png", all_CD_effects_plot, height = 8, width = 5)


all_CD_summaries <- bind_rows(
                                        chinese_beijing_CD_summary,
                                        chinese_cantonese_CD_summary,
                                        chinese_taiwanese_CD_summary,
                                        croatian_CD_summary,
                                        czech_CD_summary,
                                        english_american_CD_summary,
                                        english_australian_CD_summary,
                                        english_british_CD_summary,
                                        english_irish_CD_summary,
                                        danish_CD_summary,
                                        dutch_CD_summary,
                                        italian_CD_summary,
                                        finnish_CD_summary,
                                        french_european_CD_summary,
                                        french_quebecois_CD_summary,
                                        german_CD_summary,
                                        greek_CD_summary,
                                        hebrew_CD_summary,
                                        hungarian_CD_summary,
                                        irish_CD_summary,
                                        kiswahili_CD_summary,
                                        korean_CD_summary,
                                        latvian_CD_summary,
                                        norwegian_CD_summary,
                                        persian_CD_summary,
                                        russian_CD_summary,
                                        slovak_CD_summary,
                                        spanish_argentinian_CD_summary,
                                        spanish_chilean_CD_summary,
                                        spanish_european_CD_summary,
                                        spanish_mexican_CD_summary,
                                        spanish_peruvian_CD_summary,
                                        swedish_CD_summary
                                        # , turkish_CD_summary
) %>%
  mutate(variable = "contextdiversity",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_CD_summaries, "models/effects/all_CD_summaries.rds")



all_CD_interaction_summaries <- bind_rows(
                                                    chinese_beijing_CD_interaction_summary,
                                                    chinese_cantonese_CD_interaction_summary,
                                                    chinese_taiwanese_CD_interaction_summary,
                                                    croatian_CD_interaction_summary,
                                                    czech_CD_interaction_summary,
                                                    english_american_CD_interaction_summary,
                                                    english_australian_CD_interaction_summary,
                                                    english_british_CD_interaction_summary,
                                                    english_irish_CD_interaction_summary,
                                                    danish_CD_interaction_summary,
                                                    dutch_CD_interaction_summary,
                                                    italian_CD_interaction_summary,
                                                    finnish_CD_interaction_summary,
                                                    french_european_CD_interaction_summary,
                                                    french_quebecois_CD_interaction_summary,
                                                    german_CD_interaction_summary,
                                                    greek_CD_interaction_summary,
                                                    hebrew_CD_interaction_summary,
                                                    hungarian_CD_interaction_summary,
                                                    irish_CD_interaction_summary,
                                                    kiswahili_CD_interaction_summary,
                                                    korean_CD_interaction_summary,
                                                    latvian_CD_interaction_summary,
                                                    norwegian_CD_interaction_summary,
                                                    persian_CD_interaction_summary,
                                                    russian_CD_interaction_summary,
                                                    slovak_CD_interaction_summary,
                                                    spanish_argentinian_CD_interaction_summary,
                                                    spanish_chilean_CD_interaction_summary,
                                                    spanish_european_CD_interaction_summary,
                                                    spanish_mexican_CD_interaction_summary,
                                                    spanish_peruvian_CD_interaction_summary,
                                                    swedish_CD_interaction_summary
                                                    # , turkish_CD_summary
) %>%
  mutate(variable = "age_contextdiversity",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_CD_interaction_summaries, "models/effects/all_CD_interaction_summaries.rds")
