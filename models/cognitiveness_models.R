library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# cognitiveness
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_cognitiveness_model <- glm(as.factor(produces) ~ age + asl_cognitiveness_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                  data = asl_instrument_data, family = "binomial")
asl_cognitiveness_effect <- ggpredict(asl_cognitiveness_model, terms = "asl_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_cognitiveness_model$coefficients[[3]])
asl_cognitiveness_summary <- summary(asl_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_cognitiveness_rating") %>%
  mutate(language = "asl") 
asl_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * asl_cognitiveness_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                              data = asl_instrument_data, family = "binomial")
asl_cognitiveness_interaction_summary <- summary(asl_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_cognitiveness_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_cognitiveness_model <- glm(as.factor(produces) ~ age + bsl_cognitiveness_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_cognitiveness_effect <- ggpredict(bsl_cognitiveness_model, terms = "bsl_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_cognitiveness_model$coefficients[[3]]) 
bsl_cognitiveness_summary <- summary(bsl_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_cognitiveness_rating") %>%
  mutate(language = "bsl")
bsl_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * bsl_cognitiveness_rating + lexical_category, 
                                              data = bsl_instrument_data, family = "binomial")
bsl_cognitiveness_interaction_summary <- summary(bsl_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_cognitiveness_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_cognitiveness_model <- glm(produces ~ age + chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_cognitiveness_effect <- ggeffect(chinese_beijing_cognitiveness_model, terms = "chinese_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_cognitiveness_model$coefficients[[3]])
chinese_beijing_cognitiveness_summary <- summary(chinese_beijing_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_cognitiveness_interaction_model <- glm(produces ~ age * chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_cognitiveness_interaction_summary <- summary(chinese_beijing_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_cognitiveness_model <- glm(produces ~ age + chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_cognitiveness_effect <- ggeffect(chinese_cantonese_cognitiveness_model, terms = "chinese_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_cognitiveness_model$coefficients[[3]])
chinese_cantonese_cognitiveness_summary <- summary(chinese_cantonese_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_cognitiveness_interaction_model <- glm(produces ~ age * chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_cognitiveness_interaction_summary <- summary(chinese_cantonese_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_cognitiveness_model <- glm(produces ~ age + chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_cognitiveness_effect <- ggeffect(chinese_taiwanese_cognitiveness_model, terms = "chinese_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_cognitiveness_model$coefficients[[3]])
chinese_taiwanese_cognitiveness_summary <- summary(chinese_taiwanese_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_cognitiveness_interaction_model <- glm(produces ~ age * chinese_cognitiveness_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_cognitiveness_interaction_summary <- summary(chinese_taiwanese_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_cognitiveness_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_cognitiveness_model <- glm(as.factor(produces) ~ age + croatian_cognitiveness_rating + croatian_freq_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_cognitiveness_effect <- ggpredict(croatian_cognitiveness_model, terms = "croatian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_cognitiveness_model$coefficients[[3]])
croatian_cognitiveness_summary <- summary(croatian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_cognitiveness_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * croatian_cognitiveness_rating + croatian_freq_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_cognitiveness_interaction_summary <- summary(croatian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_cognitiveness_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_cognitiveness_model <- glm(as.factor(produces) ~ age + czech_cognitiveness_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_cognitiveness_effect <- ggpredict(czech_cognitiveness_model, terms = "czech_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_cognitiveness_model$coefficients[[3]])
czech_cognitiveness_summary <- summary(czech_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_cognitiveness_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * czech_cognitiveness_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_cognitiveness_interaction_summary <- summary(czech_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_cognitiveness_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_cognitiveness_model <- glm(as.factor(produces) ~ age + danish_cognitiveness_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_cognitiveness_effect <- ggpredict(danish_cognitiveness_model, terms = "danish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_cognitiveness_model$coefficients[[3]])
danish_cognitiveness_summary <- summary(danish_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_cognitiveness_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * danish_cognitiveness_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_cognitiveness_interaction_summary <- summary(danish_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_cognitiveness_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_cognitiveness_model <- glm(as.factor(produces) ~ age + dutch_cognitiveness_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_cognitiveness_effect <- ggpredict(dutch_cognitiveness_model, terms = "dutch_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_cognitiveness_model$coefficients[[3]])  
dutch_cognitiveness_summary <- summary(dutch_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_cognitiveness_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * dutch_cognitiveness_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_cognitiveness_interaction_summary <- summary(dutch_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_cognitiveness_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_cognitiveness_model <- glm(produces ~ age + english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_cognitiveness_effect <- ggeffect(english_american_cognitiveness_model, terms = "english_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_cognitiveness_model$coefficients[[3]])
english_american_cognitiveness_summary <- summary(english_american_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_cognitiveness_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_cognitiveness_interaction_model <- glm(produces ~ age * english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_cognitiveness_interaction_summary <- summary(english_american_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_cognitiveness_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_cognitiveness_model <- glm(produces ~ age + english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_cognitiveness_effect <- ggeffect(english_australian_cognitiveness_model, terms = "english_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_cognitiveness_model$coefficients[[3]])
english_australian_cognitiveness_summary <- summary(english_australian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_cognitiveness_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_cognitiveness_interaction_model <- glm(produces ~ age * english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_cognitiveness_interaction_summary <- summary(english_australian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_cognitiveness_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_cognitiveness_model <- glm(produces ~ age + english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_cognitiveness_effect <- ggeffect(english_british_cognitiveness_model, terms = "english_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_cognitiveness_model$coefficients[[3]])
english_british_cognitiveness_summary <- summary(english_british_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_cognitiveness_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_cognitiveness_interaction_model <- glm(produces ~ age * english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_cognitiveness_interaction_summary <- summary(english_british_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_cognitiveness_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_cognitiveness_model <- glm(produces ~ age + english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_cognitiveness_effect <- ggeffect(english_irish_cognitiveness_model, terms = "english_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_cognitiveness_model$coefficients[[3]])
english_irish_cognitiveness_summary <- summary(english_irish_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_cognitiveness_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_cognitiveness_interaction_model <- glm(produces ~ age * english_cognitiveness_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_cognitiveness_interaction_summary <- summary(english_irish_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_cognitiveness_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_cognitiveness_model <- glm(as.factor(produces) ~ age + finnish_cognitiveness_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_cognitiveness_effect <- ggpredict(finnish_cognitiveness_model, terms = "finnish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_cognitiveness_model$coefficients[[3]]) 
finnish_cognitiveness_summary <- summary(finnish_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_cognitiveness_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * finnish_cognitiveness_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_cognitiveness_interaction_summary <- summary(finnish_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_cognitiveness_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_cognitiveness_model <- glm(as.factor(produces) ~ age + french_cognitiveness_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_cognitiveness_effect <- ggpredict(french_european_cognitiveness_model, terms = "french_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_cognitiveness_model$coefficients[[3]]) 
french_european_cognitiveness_summary <- summary(french_european_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_cognitiveness_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * french_cognitiveness_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_cognitiveness_interaction_summary <- summary(french_european_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_cognitiveness_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_cognitiveness_model <- glm(as.factor(produces) ~ age + french_cognitiveness_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_cognitiveness_effect <- ggpredict(french_quebecois_cognitiveness_model, terms = "french_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_cognitiveness_model$coefficients[[3]]) 
french_quebecois_cognitiveness_summary <- summary(french_quebecois_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_cognitiveness_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * french_cognitiveness_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_cognitiveness_interaction_summary <- summary(french_quebecois_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_cognitiveness_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_cognitiveness_model <- glm(as.factor(produces) ~ age + german_cognitiveness_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_cognitiveness_effect <- ggpredict(german_cognitiveness_model, terms = "german_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_cognitiveness_model$coefficients[[3]]) 
german_cognitiveness_summary <- summary(german_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_cognitiveness_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * german_cognitiveness_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_cognitiveness_interaction_summary <- summary(german_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_cognitiveness_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_cognitiveness_model <- glm(as.factor(produces) ~ age + greek_cognitiveness_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_cognitiveness_effect <- ggpredict(greek_cognitiveness_model, terms = "greek_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_cognitiveness_model$coefficients[[3]]) 
greek_cognitiveness_summary <- summary(greek_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_cognitiveness_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * greek_cognitiveness_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_cognitiveness_interaction_summary <- summary(greek_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_cognitiveness_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_cognitiveness_model <- glm(as.factor(produces) ~ age + hebrew_cognitiveness_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_cognitiveness_effect <- ggpredict(hebrew_cognitiveness_model, terms = "hebrew_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_cognitiveness_model$coefficients[[3]]) 
hebrew_cognitiveness_summary <- summary(hebrew_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_cognitiveness_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * hebrew_cognitiveness_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_cognitiveness_interaction_summary <- summary(hebrew_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_cognitiveness_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_cognitiveness_model <- glm(as.factor(produces) ~ age + hungarian_cognitiveness_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_cognitiveness_effect <- ggpredict(hungarian_cognitiveness_model, terms = "hungarian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_cognitiveness_model$coefficients[[3]])
hungarian_cognitiveness_summary <- summary(hungarian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_cognitiveness_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * hungarian_cognitiveness_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_cognitiveness_interaction_summary <- summary(hungarian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_cognitiveness_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_cognitiveness_model <- glm(as.factor(produces) ~ age + irish_cognitiveness_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_cognitiveness_effect <- ggpredict(irish_cognitiveness_model, terms = "irish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_cognitiveness_model$coefficients[[3]])
irish_cognitiveness_summary <- summary(irish_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_cognitiveness_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * irish_cognitiveness_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_cognitiveness_interaction_summary <- summary(irish_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_cognitiveness_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_cognitiveness_model <- glm(produces ~ age + italian_cognitiveness_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_cognitiveness_effect <- ggeffect(italian_cognitiveness_model, terms = "italian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_cognitiveness_model$coefficients[[3]])
italian_cognitiveness_summary <- summary(italian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_cognitiveness_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_cognitiveness_interaction_model <- glm(produces ~ age * italian_cognitiveness_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_cognitiveness_interaction_summary <- summary(italian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_cognitiveness_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

kigiriama_instrument_data <- read_rds("norms/kigiriama/kigiriama_instrument_data.rds")
kigiriama_cognitiveness_model <- glm(produces ~ age + kigiriama_cognitiveness_rating + lexical_category + word_length, data = kigiriama_instrument_data, family = "binomial")
kigiriama_cognitiveness_effect <- ggeffect(kigiriama_cognitiveness_model, terms = "kigiriama_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "kigiriama",
         variable_coefficient = kigiriama_cognitiveness_model$coefficients[[3]])
kigiriama_cognitiveness_summary <- summary(kigiriama_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kigiriama_cognitiveness_rating") %>%
  mutate(language = "kigiriama",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kigiriama_cognitiveness_interaction_model <- glm(produces ~ age * kigiriama_cognitiveness_rating + lexical_category + word_length, data = kigiriama_instrument_data, family = "binomial")
kigiriama_cognitiveness_interaction_summary <- summary(kigiriama_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kigiriama_cognitiveness_rating") %>%
  mutate(language = "kigiriama",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_cognitiveness_model <- glm(produces ~ age + kiswahili_cognitiveness_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_cognitiveness_effect <- ggeffect(kiswahili_cognitiveness_model, terms = "kiswahili_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_cognitiveness_model$coefficients[[3]])
kiswahili_cognitiveness_summary <- summary(kiswahili_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_cognitiveness_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_cognitiveness_interaction_model <- glm(produces ~ age * kiswahili_cognitiveness_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_cognitiveness_interaction_summary <- summary(kiswahili_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_cognitiveness_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_cognitiveness_model <- glm(produces ~ age + korean_cognitiveness_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_cognitiveness_effect <- ggeffect(korean_cognitiveness_model, terms = "korean_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_cognitiveness_model$coefficients[[3]])
korean_cognitiveness_summary <- summary(korean_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_cognitiveness_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_cognitiveness_interaction_model <- glm(produces ~ age * korean_cognitiveness_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_cognitiveness_interaction_summary <- summary(korean_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_cognitiveness_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_cognitiveness_model <- glm(produces ~ age + latvian_cognitiveness_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_cognitiveness_effect <- ggeffect(latvian_cognitiveness_model, terms = "latvian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_cognitiveness_model$coefficients[[3]])
latvian_cognitiveness_summary <- summary(latvian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_cognitiveness_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_cognitiveness_interaction_model <- glm(produces ~ age * latvian_cognitiveness_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_cognitiveness_interaction_summary <- summary(latvian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_cognitiveness_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_cognitiveness_model <- glm(produces ~ age + norwegian_cognitiveness_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_cognitiveness_effect <- ggeffect(norwegian_cognitiveness_model, terms = "norwegian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_cognitiveness_model$coefficients[[3]])
norwegian_cognitiveness_summary <- summary(norwegian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_cognitiveness_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_cognitiveness_interaction_model <- glm(produces ~ age * norwegian_cognitiveness_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_cognitiveness_interaction_summary <- summary(norwegian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_cognitiveness_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_cognitiveness_model <- glm(produces ~ age + persian_cognitiveness_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_cognitiveness_effect <- ggeffect(persian_cognitiveness_model, terms = "persian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_cognitiveness_model$coefficients[[3]])
persian_cognitiveness_summary <- summary(persian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_cognitiveness_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_cognitiveness_interaction_model <- glm(produces ~ age * persian_cognitiveness_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_cognitiveness_interaction_summary <- summary(persian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_cognitiveness_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
portuguese_cognitiveness_model <- glm(produces ~ age + portuguese_cognitiveness_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
portuguese_cognitiveness_effect <- ggeffect(portuguese_cognitiveness_model, terms = "portuguese_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = portuguese_cognitiveness_model$coefficients[[3]])
portuguese_cognitiveness_summary <- summary(portuguese_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "portuguese_cognitiveness_rating") %>%
  mutate(language = "portuguese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
portuguese_cognitiveness_interaction_model <- glm(produces ~ age * portuguese_cognitiveness_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
portuguese_cognitiveness_interaction_summary <- summary(portuguese_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:portuguese_cognitiveness_rating") %>%
  mutate(language = "portuguese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_cognitiveness_model <- glm(produces ~ age + russian_cognitiveness_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_cognitiveness_effect <- ggeffect(russian_cognitiveness_model, terms = "russian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_cognitiveness_model$coefficients[[3]])
russian_cognitiveness_summary <- summary(russian_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_cognitiveness_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_cognitiveness_interaction_model <- glm(produces ~ age * russian_cognitiveness_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_cognitiveness_interaction_summary <- summary(russian_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_cognitiveness_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_cognitiveness_model <- glm(produces ~ age + slovak_cognitiveness_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_cognitiveness_effect <- ggeffect(slovak_cognitiveness_model, terms = "slovak_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_cognitiveness_model$coefficients[[3]])
slovak_cognitiveness_summary <- summary(slovak_cognitiveness_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_cognitiveness_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_cognitiveness_interaction_model <- glm(produces ~ age * slovak_cognitiveness_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_cognitiveness_interaction_summary <- summary(slovak_cognitiveness_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_cognitiveness_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_cognitiveness_model <- glm(produces ~ age + spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_cognitiveness_effect <- ggeffect(spanish_argentinian_cognitiveness_model, terms = "spanish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_cognitiveness_model$coefficients[[3]])
spanish_argentinian_cognitiveness_summary <- summary(spanish_argentinian_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_cognitiveness_interaction_model <- glm(produces ~ age * spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_cognitiveness_interaction_summary <- summary(spanish_argentinian_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_cognitiveness_model <- glm(produces ~ age + spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_cognitiveness_effect <- ggeffect(spanish_chilean_cognitiveness_model, terms = "spanish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_cognitiveness_model$coefficients[[3]])
spanish_chilean_cognitiveness_summary <- summary(spanish_chilean_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_cognitiveness_interaction_model <- glm(produces ~ age * spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_cognitiveness_interaction_summary <- summary(spanish_chilean_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_cognitiveness_model <- glm(produces ~ age + spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_cognitiveness_effect <- ggeffect(spanish_european_cognitiveness_model, terms = "spanish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_cognitiveness_model$coefficients[[3]])
spanish_european_cognitiveness_summary <- summary(spanish_european_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_european")
spanish_european_cognitiveness_interaction_model <- glm(produces ~ age * spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_cognitiveness_interaction_summary <- summary(spanish_european_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_cognitiveness_model <- glm(produces ~ age + spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_cognitiveness_effect <- ggeffect(spanish_mexican_cognitiveness_model, terms = "spanish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_cognitiveness_model$coefficients[[3]])
spanish_mexican_cognitiveness_summary <- summary(spanish_mexican_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_cognitiveness_interaction_model <- glm(produces ~ age * spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_cognitiveness_interaction_summary <- summary(spanish_mexican_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_cognitiveness_model <- glm(produces ~ age + spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_cognitiveness_effect <- ggeffect(spanish_peruvian_cognitiveness_model, terms = "spanish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_cognitiveness_model$coefficients[[3]])
spanish_peruvian_cognitiveness_summary <- summary(spanish_peruvian_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_cognitiveness_interaction_model <- glm(produces ~ age * spanish_cognitiveness_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_cognitiveness_interaction_summary <- summary(spanish_peruvian_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_cognitiveness_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_cognitiveness_model <- glm(produces ~ age + swedish_cognitiveness_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_cognitiveness_effect <- ggeffect(swedish_cognitiveness_model, terms = "swedish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_cognitiveness_model$coefficients[[3]])
swedish_cognitiveness_summary <- summary(swedish_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_cognitiveness_rating") %>%
  mutate(language = "swedish")
swedish_cognitiveness_interaction_model <- glm(produces ~ age * swedish_cognitiveness_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_cognitiveness_interaction_model <- glm(produces ~ age * swedish_cognitiveness_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_cognitiveness_interaction_summary <- summary(swedish_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_cognitiveness_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_cognitiveness_model <- glm(as.factor(produces) ~ age + arabic_cognitiveness_rating + arabic_freq_rating + lexical_category, 
                                     data = arabic_instrument_data, family = "binomial")
arabic_cognitiveness_effect <- ggpredict(arabic_cognitiveness_model, terms = "arabic_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_cognitiveness_model$coefficients[[3]])
arabic_cognitiveness_summary <- summary(arabic_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_cognitiveness_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * arabic_cognitiveness_rating + arabic_freq_rating + lexical_category, 
                                                 data = arabic_instrument_data, family = "binomial")
arabic_cognitiveness_interaction_summary <- summary(arabic_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_cognitiveness_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_cognitiveness_model <- glm(as.factor(produces) ~ age + catalan_cognitiveness_rating + catalan_freq_rating+ lexical_category, 
                                      data = catalan_instrument_data, family = "binomial")
catalan_cognitiveness_effect <- ggpredict(catalan_cognitiveness_model, terms = "catalan_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_cognitiveness_model$coefficients[[3]])
catalan_cognitiveness_summary <- summary(catalan_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_cognitiveness_rating") %>%
  mutate(language = "catalan") 
catalan_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * catalan_cognitiveness_rating +catalan_freq_rating  + lexical_category, 
                                                  data = catalan_instrument_data, family = "binomial")
catalan_cognitiveness_interaction_summary <- summary(catalan_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_cognitiveness_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_cognitiveness_model <- glm(as.factor(produces) ~ age + estonian_cognitiveness_rating + estonian_freq_rating + lexical_category, 
                                       data = estonian_instrument_data, family = "binomial")
estonian_cognitiveness_effect <- ggpredict(estonian_cognitiveness_model, terms = "estonian_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_cognitiveness_model$coefficients[[3]])
estonian_cognitiveness_summary <- summary(estonian_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_cognitiveness_rating") %>%
  mutate(language = "estonian") 
estonian_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * estonian_cognitiveness_rating + estonian_freq_rating  + lexical_category, 
                                                   data = estonian_instrument_data, family = "binomial")
estonian_cognitiveness_interaction_summary <- summary(estonian_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_cognitiveness_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_cognitiveness_model <- glm(as.factor(produces) ~ age + japanese_cognitiveness_rating+ japanese_freq_rating  + lexical_category, 
                                       data = japanese_instrument_data, family = "binomial")
japanese_cognitiveness_effect <- ggpredict(japanese_cognitiveness_model, terms = "japanese_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_cognitiveness_model$coefficients[[3]])
japanese_cognitiveness_summary <- summary(japanese_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_cognitiveness_rating") %>%
  mutate(language = "japanese") 
japanese_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * japanese_cognitiveness_rating + japanese_freq_rating  + lexical_category, 
                                                   data = japanese_instrument_data, family = "binomial")
japanese_cognitiveness_interaction_summary <- summary(japanese_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_cognitiveness_rating") %>%
  mutate(language = "japanese")

turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
turkish_cognitiveness_model <- glm(as.factor(produces) ~ age + turkish_cognitiveness_rating+ turkish_freq_rating  + lexical_category, 
                                      data = turkish_instrument_data, family = "binomial")
turkish_cognitiveness_effect <- ggpredict(turkish_cognitiveness_model, terms = "turkish_cognitiveness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = turkish_cognitiveness_model$coefficients[[3]])
turkish_cognitiveness_summary <- summary(turkish_cognitiveness_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "turkish_cognitiveness_rating") %>%
  mutate(language = "turkish") 
turkish_cognitiveness_interaction_model <- glm(as.factor(produces) ~ age * turkish_cognitiveness_rating + turkish_freq_rating  + lexical_category, 
                                                  data = turkish_instrument_data, family = "binomial")
turkish_cognitiveness_interaction_summary <- summary(turkish_cognitiveness_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:turkish_cognitiveness_rating") %>%
  mutate(language = "turkish")

all_cognitiveness_effects <- bind_rows(asl_cognitiveness_effect,
                                          bsl_cognitiveness_effect,
                                          chinese_beijing_cognitiveness_effect,
                                          chinese_cantonese_cognitiveness_effect,
                                          chinese_taiwanese_cognitiveness_effect,
                                          croatian_cognitiveness_effect,
                                          czech_cognitiveness_effect,
                                          english_american_cognitiveness_effect,
                                          english_australian_cognitiveness_effect,
                                          english_british_cognitiveness_effect,
                                          english_irish_cognitiveness_effect,
                                          danish_cognitiveness_effect,
                                          dutch_cognitiveness_effect,
                                          italian_cognitiveness_effect,
                                          finnish_cognitiveness_effect,
                                          french_european_cognitiveness_effect,
                                          french_quebecois_cognitiveness_effect,
                                          german_cognitiveness_effect,
                                          greek_cognitiveness_effect,
                                          hebrew_cognitiveness_effect,
                                          hungarian_cognitiveness_effect,
                                          irish_cognitiveness_effect,
                                          kigiriama_cognitiveness_effect,
                                          kiswahili_cognitiveness_effect,
                                          korean_cognitiveness_effect,
                                          latvian_cognitiveness_effect,
                                          norwegian_cognitiveness_effect,
                                          persian_cognitiveness_effect,
                                          portuguese_cognitiveness_effect,
                                          russian_cognitiveness_effect,
                                          slovak_cognitiveness_effect,
                                          spanish_argentinian_cognitiveness_effect,
                                          spanish_chilean_cognitiveness_effect,
                                          spanish_european_cognitiveness_effect,
                                          spanish_mexican_cognitiveness_effect,
                                          spanish_peruvian_cognitiveness_effect,
                                          swedish_cognitiveness_effect,
                                          arabic_cognitiveness_effect,
                                          catalan_cognitiveness_effect,
                                          estonian_cognitiveness_effect,
                                          japanese_cognitiveness_effect, 
                                          turkish_cognitiveness_effect
)
write_rds(all_cognitiveness_effects, "models/effects/all_cognitiveness_effects.rds")

all_cognitiveness_effects_plot <- ggplot(all_cognitiveness_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "cognitiveness Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_cognitiveness_effects_plots.png", all_cognitiveness_effects_plot, height = 8, width = 5)


all_cognitiveness_summaries <- bind_rows(asl_cognitiveness_summary,
                                            bsl_cognitiveness_summary,
                                            chinese_beijing_cognitiveness_summary,
                                            chinese_cantonese_cognitiveness_summary,
                                            chinese_taiwanese_cognitiveness_summary,
                                            croatian_cognitiveness_summary,
                                            czech_cognitiveness_summary,
                                            english_american_cognitiveness_summary,
                                            english_australian_cognitiveness_summary,
                                            english_british_cognitiveness_summary,
                                            english_irish_cognitiveness_summary,
                                            danish_cognitiveness_summary,
                                            dutch_cognitiveness_summary,
                                            italian_cognitiveness_summary,
                                            finnish_cognitiveness_summary,
                                            french_european_cognitiveness_summary,
                                            french_quebecois_cognitiveness_summary,
                                            german_cognitiveness_summary,
                                            greek_cognitiveness_summary,
                                            hebrew_cognitiveness_summary,
                                            hungarian_cognitiveness_summary,
                                            irish_cognitiveness_summary,
                                            kigiriama_cognitiveness_summary,
                                            kiswahili_cognitiveness_summary,
                                            korean_cognitiveness_summary,
                                            latvian_cognitiveness_summary,
                                            norwegian_cognitiveness_summary,
                                            persian_cognitiveness_summary,
                                            portuguese_cognitiveness_summary,
                                            russian_cognitiveness_summary,
                                            slovak_cognitiveness_summary,
                                            spanish_argentinian_cognitiveness_summary,
                                            spanish_chilean_cognitiveness_summary,
                                            spanish_european_cognitiveness_summary,
                                            spanish_mexican_cognitiveness_summary,
                                            spanish_peruvian_cognitiveness_summary,
                                            swedish_cognitiveness_summary,
                                            arabic_cognitiveness_summary,
                                            catalan_cognitiveness_summary,
                                            estonian_cognitiveness_summary,
                                            japanese_cognitiveness_summary, 
                                            turkish_cognitiveness_summary
) %>%
  mutate(variable = "Cognitiveness",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_cognitiveness_summaries, "models/effects/all_cognitiveness_summaries.rds")



all_cognitiveness_interaction_summaries <- bind_rows(asl_cognitiveness_interaction_summary,
                                                        bsl_cognitiveness_interaction_summary,
                                                        chinese_beijing_cognitiveness_interaction_summary,
                                                        chinese_cantonese_cognitiveness_interaction_summary,
                                                        chinese_taiwanese_cognitiveness_interaction_summary,
                                                        croatian_cognitiveness_interaction_summary,
                                                        czech_cognitiveness_interaction_summary,
                                                        english_american_cognitiveness_interaction_summary,
                                                        english_australian_cognitiveness_interaction_summary,
                                                        english_british_cognitiveness_interaction_summary,
                                                        english_irish_cognitiveness_interaction_summary,
                                                        danish_cognitiveness_interaction_summary,
                                                        dutch_cognitiveness_interaction_summary,
                                                        italian_cognitiveness_interaction_summary,
                                                        finnish_cognitiveness_interaction_summary,
                                                        french_european_cognitiveness_interaction_summary,
                                                        french_quebecois_cognitiveness_interaction_summary,
                                                        german_cognitiveness_interaction_summary,
                                                        greek_cognitiveness_interaction_summary,
                                                        hebrew_cognitiveness_interaction_summary,
                                                        hungarian_cognitiveness_interaction_summary,
                                                        irish_cognitiveness_interaction_summary,
                                                        kigiriama_cognitiveness_interaction_summary,
                                                        kiswahili_cognitiveness_interaction_summary,
                                                        korean_cognitiveness_interaction_summary,
                                                        latvian_cognitiveness_interaction_summary,
                                                        norwegian_cognitiveness_interaction_summary,
                                                        persian_cognitiveness_interaction_summary,
                                                        portuguese_cognitiveness_interaction_summary,
                                                        russian_cognitiveness_interaction_summary,
                                                        slovak_cognitiveness_interaction_summary,
                                                        spanish_argentinian_cognitiveness_interaction_summary,
                                                        spanish_chilean_cognitiveness_interaction_summary,
                                                        spanish_european_cognitiveness_interaction_summary,
                                                        spanish_mexican_cognitiveness_interaction_summary,
                                                        spanish_peruvian_cognitiveness_interaction_summary,
                                                        swedish_cognitiveness_interaction_summary,
                                                        arabic_cognitiveness_interaction_summary,
                                                        catalan_cognitiveness_interaction_summary,
                                                        estonian_cognitiveness_interaction_summary,
                                                        japanese_cognitiveness_interaction_summary, 
                                                        turkish_cognitiveness_summary
) %>%
  mutate(variable = "Cognitiveness",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_cognitiveness_interaction_summaries, "models/effects/all_cognitiveness_interaction_summaries.rds")
