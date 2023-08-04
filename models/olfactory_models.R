library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# olfactory
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_olfactory_model <- glm(as.factor(produces) ~ age + asl_olfactory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                        data = asl_instrument_data, family = "binomial")
asl_olfactory_effect <- ggpredict(asl_olfactory_model, terms = "asl_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_olfactory_model$coefficients[[3]])
asl_olfactory_summary <- summary(asl_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_olfactory_rating") %>%
  mutate(language = "asl") 
asl_olfactory_interaction_model <- glm(as.factor(produces) ~ age * asl_olfactory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                    data = asl_instrument_data, family = "binomial")
asl_olfactory_interaction_summary <- summary(asl_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_olfactory_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_olfactory_model <- glm(as.factor(produces) ~ age + bsl_olfactory_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_olfactory_effect <- ggpredict(bsl_olfactory_model, terms = "bsl_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_olfactory_model$coefficients[[3]]) 
bsl_olfactory_summary <- summary(bsl_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_olfactory_rating") %>%
  mutate(language = "bsl")
bsl_olfactory_interaction_model <- glm(as.factor(produces) ~ age * bsl_olfactory_rating + lexical_category, 
                                    data = bsl_instrument_data, family = "binomial")
bsl_olfactory_interaction_summary <- summary(bsl_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_olfactory_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_olfactory_model <- glm(produces ~ age + chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_olfactory_effect <- ggeffect(chinese_beijing_olfactory_model, terms = "chinese_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_olfactory_model$coefficients[[3]])
chinese_beijing_olfactory_summary <- summary(chinese_beijing_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_olfactory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_olfactory_interaction_model <- glm(produces ~ age * chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_olfactory_interaction_summary <- summary(chinese_beijing_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_olfactory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_olfactory_model <- glm(produces ~ age + chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_olfactory_effect <- ggeffect(chinese_cantonese_olfactory_model, terms = "chinese_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_olfactory_model$coefficients[[3]])
chinese_cantonese_olfactory_summary <- summary(chinese_cantonese_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_olfactory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_olfactory_interaction_model <- glm(produces ~ age * chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_olfactory_interaction_summary <- summary(chinese_cantonese_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_olfactory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_olfactory_model <- glm(produces ~ age + chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_olfactory_effect <- ggeffect(chinese_taiwanese_olfactory_model, terms = "chinese_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_olfactory_model$coefficients[[3]])
chinese_taiwanese_olfactory_summary <- summary(chinese_taiwanese_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_olfactory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_olfactory_interaction_model <- glm(produces ~ age * chinese_olfactory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_olfactory_interaction_summary <- summary(chinese_taiwanese_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_olfactory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_olfactory_model <- glm(as.factor(produces) ~ age + croatian_olfactory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_olfactory_effect <- ggpredict(croatian_olfactory_model, terms = "croatian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_olfactory_model$coefficients[[3]])
croatian_olfactory_summary <- summary(croatian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_olfactory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_olfactory_interaction_model <- glm(as.factor(produces) ~ age * croatian_olfactory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_olfactory_interaction_summary <- summary(croatian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_olfactory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_olfactory_model <- glm(as.factor(produces) ~ age + czech_olfactory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_olfactory_effect <- ggpredict(czech_olfactory_model, terms = "czech_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_olfactory_model$coefficients[[3]])
czech_olfactory_summary <- summary(czech_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_olfactory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_olfactory_interaction_model <- glm(as.factor(produces) ~ age * czech_olfactory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_olfactory_interaction_summary <- summary(czech_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_olfactory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_olfactory_model <- glm(as.factor(produces) ~ age + danish_olfactory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_olfactory_effect <- ggpredict(danish_olfactory_model, terms = "danish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_olfactory_model$coefficients[[3]])
danish_olfactory_summary <- summary(danish_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_olfactory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_olfactory_interaction_model <- glm(as.factor(produces) ~ age * danish_olfactory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_olfactory_interaction_summary <- summary(danish_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_olfactory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_olfactory_model <- glm(as.factor(produces) ~ age + dutch_olfactory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_olfactory_effect <- ggpredict(dutch_olfactory_model, terms = "dutch_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_olfactory_model$coefficients[[3]])  
dutch_olfactory_summary <- summary(dutch_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_olfactory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_olfactory_interaction_model <- glm(as.factor(produces) ~ age * dutch_olfactory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_olfactory_interaction_summary <- summary(dutch_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_olfactory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_olfactory_model <- glm(produces ~ age + english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_olfactory_effect <- ggeffect(english_american_olfactory_model, terms = "english_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_olfactory_model$coefficients[[3]])
english_american_olfactory_summary <- summary(english_american_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_olfactory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_olfactory_interaction_model <- glm(produces ~ age * english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_olfactory_interaction_summary <- summary(english_american_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_olfactory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_olfactory_model <- glm(produces ~ age + english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_olfactory_effect <- ggeffect(english_australian_olfactory_model, terms = "english_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_olfactory_model$coefficients[[3]])
english_australian_olfactory_summary <- summary(english_australian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_olfactory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_olfactory_interaction_model <- glm(produces ~ age * english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_olfactory_interaction_summary <- summary(english_australian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_olfactory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_olfactory_model <- glm(produces ~ age + english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_olfactory_effect <- ggeffect(english_british_olfactory_model, terms = "english_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_olfactory_model$coefficients[[3]])
english_british_olfactory_summary <- summary(english_british_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_olfactory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_olfactory_interaction_model <- glm(produces ~ age * english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_olfactory_interaction_summary <- summary(english_british_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_olfactory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_olfactory_model <- glm(produces ~ age + english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_olfactory_effect <- ggeffect(english_irish_olfactory_model, terms = "english_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_olfactory_model$coefficients[[3]])
english_irish_olfactory_summary <- summary(english_irish_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_olfactory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_olfactory_interaction_model <- glm(produces ~ age * english_olfactory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_olfactory_interaction_summary <- summary(english_irish_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_olfactory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_olfactory_model <- glm(as.factor(produces) ~ age + finnish_olfactory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_olfactory_effect <- ggpredict(finnish_olfactory_model, terms = "finnish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_olfactory_model$coefficients[[3]]) 
finnish_olfactory_summary <- summary(finnish_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_olfactory_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_olfactory_interaction_model <- glm(as.factor(produces) ~ age * finnish_olfactory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_olfactory_interaction_summary <- summary(finnish_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_olfactory_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_olfactory_model <- glm(as.factor(produces) ~ age + french_olfactory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_olfactory_effect <- ggpredict(french_european_olfactory_model, terms = "french_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_olfactory_model$coefficients[[3]]) 
french_european_olfactory_summary <- summary(french_european_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_olfactory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_olfactory_interaction_model <- glm(as.factor(produces) ~ age * french_olfactory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_olfactory_interaction_summary <- summary(french_european_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_olfactory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_olfactory_model <- glm(as.factor(produces) ~ age + french_olfactory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_olfactory_effect <- ggpredict(french_quebecois_olfactory_model, terms = "french_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_olfactory_model$coefficients[[3]]) 
french_quebecois_olfactory_summary <- summary(french_quebecois_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_olfactory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_olfactory_interaction_model <- glm(as.factor(produces) ~ age * french_olfactory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_olfactory_interaction_summary <- summary(french_quebecois_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_olfactory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_olfactory_model <- glm(as.factor(produces) ~ age + german_olfactory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_olfactory_effect <- ggpredict(german_olfactory_model, terms = "german_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_olfactory_model$coefficients[[3]]) 
german_olfactory_summary <- summary(german_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_olfactory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_olfactory_interaction_model <- glm(as.factor(produces) ~ age * german_olfactory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_olfactory_interaction_summary <- summary(german_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_olfactory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_olfactory_model <- glm(as.factor(produces) ~ age + greek_olfactory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_olfactory_effect <- ggpredict(greek_olfactory_model, terms = "greek_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_olfactory_model$coefficients[[3]]) 
greek_olfactory_summary <- summary(greek_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_olfactory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_olfactory_interaction_model <- glm(as.factor(produces) ~ age * greek_olfactory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_olfactory_interaction_summary <- summary(greek_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_olfactory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_olfactory_model <- glm(as.factor(produces) ~ age + hebrew_olfactory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_olfactory_effect <- ggpredict(hebrew_olfactory_model, terms = "hebrew_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_olfactory_model$coefficients[[3]]) 
hebrew_olfactory_summary <- summary(hebrew_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_olfactory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_olfactory_interaction_model <- glm(as.factor(produces) ~ age * hebrew_olfactory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_olfactory_interaction_summary <- summary(hebrew_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_olfactory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_olfactory_model <- glm(as.factor(produces) ~ age + hungarian_olfactory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_olfactory_effect <- ggpredict(hungarian_olfactory_model, terms = "hungarian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_olfactory_model$coefficients[[3]])
hungarian_olfactory_summary <- summary(hungarian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_olfactory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_olfactory_interaction_model <- glm(as.factor(produces) ~ age * hungarian_olfactory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_olfactory_interaction_summary <- summary(hungarian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_olfactory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_olfactory_model <- glm(as.factor(produces) ~ age + irish_olfactory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_olfactory_effect <- ggpredict(irish_olfactory_model, terms = "irish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_olfactory_model$coefficients[[3]])
irish_olfactory_summary <- summary(irish_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_olfactory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_olfactory_interaction_model <- glm(as.factor(produces) ~ age * irish_olfactory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_olfactory_interaction_summary <- summary(irish_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_olfactory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_olfactory_model <- glm(produces ~ age + italian_olfactory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_olfactory_effect <- ggeffect(italian_olfactory_model, terms = "italian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_olfactory_model$coefficients[[3]])
italian_olfactory_summary <- summary(italian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_olfactory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_olfactory_interaction_model <- glm(produces ~ age * italian_olfactory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_olfactory_interaction_summary <- summary(italian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_olfactory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_olfactory_model <- glm(produces ~ age + kigiriama_olfactory_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_olfactory_effect <- ggeffect(kigiriama_olfactory_model, terms = "kigiriama_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_olfactory_model <- glm(produces ~ age + kiswahili_olfactory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_olfactory_effect <- ggeffect(kiswahili_olfactory_model, terms = "kiswahili_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_olfactory_model$coefficients[[3]])
kiswahili_olfactory_summary <- summary(kiswahili_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_olfactory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_olfactory_interaction_model <- glm(produces ~ age * kiswahili_olfactory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_olfactory_interaction_summary <- summary(kiswahili_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_olfactory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_olfactory_model <- glm(produces ~ age + korean_olfactory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_olfactory_effect <- ggeffect(korean_olfactory_model, terms = "korean_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_olfactory_model$coefficients[[3]])
korean_olfactory_summary <- summary(korean_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_olfactory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_olfactory_interaction_model <- glm(produces ~ age * korean_olfactory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_olfactory_interaction_summary <- summary(korean_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_olfactory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_olfactory_model <- glm(produces ~ age + latvian_olfactory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_olfactory_effect <- ggeffect(latvian_olfactory_model, terms = "latvian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_olfactory_model$coefficients[[3]])
latvian_olfactory_summary <- summary(latvian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_olfactory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_olfactory_interaction_model <- glm(produces ~ age * latvian_olfactory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_olfactory_interaction_summary <- summary(latvian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_olfactory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_olfactory_model <- glm(produces ~ age + norwegian_olfactory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_olfactory_effect <- ggeffect(norwegian_olfactory_model, terms = "norwegian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_olfactory_model$coefficients[[3]])
norwegian_olfactory_summary <- summary(norwegian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_olfactory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_olfactory_interaction_model <- glm(produces ~ age * norwegian_olfactory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_olfactory_interaction_summary <- summary(norwegian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_olfactory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_olfactory_model <- glm(produces ~ age + persian_olfactory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_olfactory_effect <- ggeffect(persian_olfactory_model, terms = "persian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_olfactory_model$coefficients[[3]])
persian_olfactory_summary <- summary(persian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_olfactory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_olfactory_interaction_model <- glm(produces ~ age * persian_olfactory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_olfactory_interaction_summary <- summary(persian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_olfactory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_olfactory_model <- glm(produces ~ age + portuguese_olfactory_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_olfactory_effect <- ggeffect(portuguese_olfactory_model, terms = "portuguese_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_olfactory_model <- glm(produces ~ age + russian_olfactory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_olfactory_effect <- ggeffect(russian_olfactory_model, terms = "russian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_olfactory_model$coefficients[[3]])
russian_olfactory_summary <- summary(russian_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_olfactory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_olfactory_interaction_model <- glm(produces ~ age * russian_olfactory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_olfactory_interaction_summary <- summary(russian_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_olfactory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_olfactory_model <- glm(produces ~ age + slovak_olfactory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_olfactory_effect <- ggeffect(slovak_olfactory_model, terms = "slovak_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_olfactory_model$coefficients[[3]])
slovak_olfactory_summary <- summary(slovak_olfactory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_olfactory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_olfactory_interaction_model <- glm(produces ~ age * slovak_olfactory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_olfactory_interaction_summary <- summary(slovak_olfactory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_olfactory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_olfactory_model <- glm(produces ~ age + spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_olfactory_effect <- ggeffect(spanish_argentinian_olfactory_model, terms = "spanish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_olfactory_model$coefficients[[3]])
spanish_argentinian_olfactory_summary <- summary(spanish_argentinian_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_olfactory_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_olfactory_interaction_model <- glm(produces ~ age * spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_olfactory_interaction_summary <- summary(spanish_argentinian_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_olfactory_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_olfactory_model <- glm(produces ~ age + spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_olfactory_effect <- ggeffect(spanish_chilean_olfactory_model, terms = "spanish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_olfactory_model$coefficients[[3]])
spanish_chilean_olfactory_summary <- summary(spanish_chilean_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_olfactory_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_olfactory_interaction_model <- glm(produces ~ age * spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_olfactory_interaction_summary <- summary(spanish_chilean_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_olfactory_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_olfactory_model <- glm(produces ~ age + spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_olfactory_effect <- ggeffect(spanish_european_olfactory_model, terms = "spanish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_olfactory_model$coefficients[[3]])
spanish_european_olfactory_summary <- summary(spanish_european_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_olfactory_rating") %>%
  mutate(language = "spanish_european")
spanish_european_olfactory_interaction_model <- glm(produces ~ age * spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_olfactory_interaction_summary <- summary(spanish_european_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_olfactory_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_olfactory_model <- glm(produces ~ age + spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_olfactory_effect <- ggeffect(spanish_mexican_olfactory_model, terms = "spanish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_olfactory_model$coefficients[[3]])
spanish_mexican_olfactory_summary <- summary(spanish_mexican_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_olfactory_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_olfactory_interaction_model <- glm(produces ~ age * spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_olfactory_interaction_summary <- summary(spanish_mexican_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_olfactory_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_olfactory_model <- glm(produces ~ age + spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_olfactory_effect <- ggeffect(spanish_peruvian_olfactory_model, terms = "spanish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_olfactory_model$coefficients[[3]])
spanish_peruvian_olfactory_summary <- summary(spanish_peruvian_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_olfactory_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_olfactory_interaction_model <- glm(produces ~ age * spanish_olfactory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_olfactory_interaction_summary <- summary(spanish_peruvian_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_olfactory_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_olfactory_model <- glm(produces ~ age + swedish_olfactory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_olfactory_effect <- ggeffect(swedish_olfactory_model, terms = "swedish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_olfactory_model$coefficients[[3]])
swedish_olfactory_summary <- summary(swedish_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_olfactory_rating") %>%
  mutate(language = "swedish")
swedish_olfactory_interaction_model <- glm(produces ~ age * swedish_olfactory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_olfactory_interaction_model <- glm(produces ~ age * swedish_olfactory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_olfactory_interaction_summary <- summary(swedish_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_olfactory_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_olfactory_model <- glm(as.factor(produces) ~ age + arabic_olfactory_rating + lexical_category, 
                           data = arabic_instrument_data, family = "binomial")
arabic_olfactory_effect <- ggpredict(arabic_olfactory_model, terms = "arabic_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_olfactory_model$coefficients[[3]])
arabic_olfactory_summary <- summary(arabic_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_olfactory_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_olfactory_interaction_model <- glm(as.factor(produces) ~ age * arabic_olfactory_rating + lexical_category, 
                                       data = arabic_instrument_data, family = "binomial")
arabic_olfactory_interaction_summary <- summary(arabic_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_olfactory_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_olfactory_model <- glm(as.factor(produces) ~ age + catalan_olfactory_rating + lexical_category, 
                            data = catalan_instrument_data, family = "binomial")
catalan_olfactory_effect <- ggpredict(catalan_olfactory_model, terms = "catalan_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_olfactory_model$coefficients[[3]])
catalan_olfactory_summary <- summary(catalan_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_olfactory_rating") %>%
  mutate(language = "catalan") 
catalan_olfactory_interaction_model <- glm(as.factor(produces) ~ age * catalan_olfactory_rating  + lexical_category, 
                                        data = catalan_instrument_data, family = "binomial")
catalan_olfactory_interaction_summary <- summary(catalan_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_olfactory_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_olfactory_model <- glm(as.factor(produces) ~ age + estonian_olfactory_rating + lexical_category, 
                             data = estonian_instrument_data, family = "binomial")
estonian_olfactory_effect <- ggpredict(estonian_olfactory_model, terms = "estonian_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_olfactory_model$coefficients[[3]])
estonian_olfactory_summary <- summary(estonian_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_olfactory_rating") %>%
  mutate(language = "estonian") 
estonian_olfactory_interaction_model <- glm(as.factor(produces) ~ age * estonian_olfactory_rating  + lexical_category, 
                                         data = estonian_instrument_data, family = "binomial")
estonian_olfactory_interaction_summary <- summary(estonian_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_olfactory_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_olfactory_model <- glm(as.factor(produces) ~ age + japanese_olfactory_rating + lexical_category, 
                             data = japanese_instrument_data, family = "binomial")
japanese_olfactory_effect <- ggpredict(japanese_olfactory_model, terms = "japanese_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_olfactory_model$coefficients[[3]])
japanese_olfactory_summary <- summary(japanese_olfactory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_olfactory_rating") %>%
  mutate(language = "japanese") 
japanese_olfactory_interaction_model <- glm(as.factor(produces) ~ age * japanese_olfactory_rating  + lexical_category, 
                                         data = japanese_instrument_data, family = "binomial")
japanese_olfactory_interaction_summary <- summary(japanese_olfactory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_olfactory_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_olfactory_model <- glm(produces ~ age + turkish_olfactory_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_olfactory_effect <- ggeffect(turkish_olfactory_model, terms = "turkish_olfactory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_olfactory_effects <- bind_rows(asl_olfactory_effect,
                                bsl_olfactory_effect,
                                chinese_beijing_olfactory_effect,
                                chinese_cantonese_olfactory_effect,
                                chinese_taiwanese_olfactory_effect,
                                croatian_olfactory_effect,
                                czech_olfactory_effect,
                                english_american_olfactory_effect,
                                english_australian_olfactory_effect,
                                english_british_olfactory_effect,
                                english_irish_olfactory_effect,
                                danish_olfactory_effect,
                                dutch_olfactory_effect,
                                italian_olfactory_effect,
                                finnish_olfactory_effect,
                                french_european_olfactory_effect,
                                french_quebecois_olfactory_effect,
                                german_olfactory_effect,
                                greek_olfactory_effect,
                                hebrew_olfactory_effect,
                                hungarian_olfactory_effect,
                                irish_olfactory_effect,
                                kiswahili_olfactory_effect,
                                korean_olfactory_effect,
                                latvian_olfactory_effect,
                                norwegian_olfactory_effect,
                                persian_olfactory_effect,
                                russian_olfactory_effect,
                                slovak_olfactory_effect,
                                spanish_argentinian_olfactory_effect,
                                spanish_chilean_olfactory_effect,
                                spanish_european_olfactory_effect,
                                spanish_mexican_olfactory_effect,
                                spanish_peruvian_olfactory_effect,
                                swedish_olfactory_effect,
                                arabic_olfactory_effect,
                                catalan_olfactory_effect,
                                estonian_olfactory_effect,
                                japanese_olfactory_effect
                                # , turkish_olfactory_effect
)
write_rds(all_olfactory_effects, "models/effects/all_olfactory_effects.rds")

all_olfactory_effects_plot <- ggplot(all_olfactory_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "olfactory Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_olfactory_effects_plots.png", all_olfactory_effects_plot, height = 8, width = 5)


all_olfactory_summaries <- bind_rows(asl_olfactory_summary,
                                  bsl_olfactory_summary,
                                  chinese_beijing_olfactory_summary,
                                  chinese_cantonese_olfactory_summary,
                                  chinese_taiwanese_olfactory_summary,
                                  croatian_olfactory_summary,
                                  czech_olfactory_summary,
                                  english_american_olfactory_summary,
                                  english_australian_olfactory_summary,
                                  english_british_olfactory_summary,
                                  english_irish_olfactory_summary,
                                  danish_olfactory_summary,
                                  dutch_olfactory_summary,
                                  italian_olfactory_summary,
                                  finnish_olfactory_summary,
                                  french_european_olfactory_summary,
                                  french_quebecois_olfactory_summary,
                                  german_olfactory_summary,
                                  greek_olfactory_summary,
                                  hebrew_olfactory_summary,
                                  hungarian_olfactory_summary,
                                  irish_olfactory_summary,
                                  kiswahili_olfactory_summary,
                                  korean_olfactory_summary,
                                  latvian_olfactory_summary,
                                  norwegian_olfactory_summary,
                                  persian_olfactory_summary,
                                  russian_olfactory_summary,
                                  slovak_olfactory_summary,
                                  spanish_argentinian_olfactory_summary,
                                  spanish_chilean_olfactory_summary,
                                  spanish_european_olfactory_summary,
                                  spanish_mexican_olfactory_summary,
                                  spanish_peruvian_olfactory_summary,
                                  swedish_olfactory_summary,
                                  arabic_olfactory_summary,
                                  catalan_olfactory_summary,
                                  estonian_olfactory_summary,
                                  japanese_olfactory_summary
                                  # , turkish_olfactory_summary
) %>%
  mutate(variable = "olfactory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_olfactory_summaries, "models/effects/all_olfactory_summaries.rds")



all_olfactory_interaction_summaries <- bind_rows(asl_olfactory_interaction_summary,
                                              bsl_olfactory_interaction_summary,
                                              chinese_beijing_olfactory_interaction_summary,
                                              chinese_cantonese_olfactory_interaction_summary,
                                              chinese_taiwanese_olfactory_interaction_summary,
                                              croatian_olfactory_interaction_summary,
                                              czech_olfactory_interaction_summary,
                                              english_american_olfactory_interaction_summary,
                                              english_australian_olfactory_interaction_summary,
                                              english_british_olfactory_interaction_summary,
                                              english_irish_olfactory_interaction_summary,
                                              danish_olfactory_interaction_summary,
                                              dutch_olfactory_interaction_summary,
                                              italian_olfactory_interaction_summary,
                                              finnish_olfactory_interaction_summary,
                                              french_european_olfactory_interaction_summary,
                                              french_quebecois_olfactory_interaction_summary,
                                              german_olfactory_interaction_summary,
                                              greek_olfactory_interaction_summary,
                                              hebrew_olfactory_interaction_summary,
                                              hungarian_olfactory_interaction_summary,
                                              irish_olfactory_interaction_summary,
                                              kiswahili_olfactory_interaction_summary,
                                              korean_olfactory_interaction_summary,
                                              latvian_olfactory_interaction_summary,
                                              norwegian_olfactory_interaction_summary,
                                              persian_olfactory_interaction_summary,
                                              russian_olfactory_interaction_summary,
                                              slovak_olfactory_interaction_summary,
                                              spanish_argentinian_olfactory_interaction_summary,
                                              spanish_chilean_olfactory_interaction_summary,
                                              spanish_european_olfactory_interaction_summary,
                                              spanish_mexican_olfactory_interaction_summary,
                                              spanish_peruvian_olfactory_interaction_summary,
                                              swedish_olfactory_interaction_summary,
                                              arabic_olfactory_interaction_summary,
                                              catalan_olfactory_interaction_summary,
                                              estonian_olfactory_interaction_summary,
                                              japanese_olfactory_interaction_summary
                                              # , turkish_olfactory_summary
) %>%
  mutate(variable = "olfactory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_olfactory_interaction_summaries, "models/effects/all_olfactory_interaction_summaries.rds")
