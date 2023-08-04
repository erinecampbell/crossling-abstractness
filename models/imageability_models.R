library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# imageability
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_imageability_model <- glm(as.factor(produces) ~ age + asl_imageability_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                        data = asl_instrument_data, family = "binomial")
asl_imageability_effect <- ggpredict(asl_imageability_model, terms = "asl_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_imageability_model$coefficients[[3]])
asl_imageability_summary <- summary(asl_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_imageability_rating") %>%
  mutate(language = "asl") 
asl_imageability_interaction_model <- glm(as.factor(produces) ~ age * asl_imageability_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                    data = asl_instrument_data, family = "binomial")
asl_imageability_interaction_summary <- summary(asl_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_imageability_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_imageability_model <- glm(as.factor(produces) ~ age + bsl_imageability_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_imageability_effect <- ggpredict(bsl_imageability_model, terms = "bsl_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_imageability_model$coefficients[[3]]) 
bsl_imageability_summary <- summary(bsl_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_imageability_rating") %>%
  mutate(language = "bsl")
bsl_imageability_interaction_model <- glm(as.factor(produces) ~ age * bsl_imageability_rating + lexical_category, 
                                    data = bsl_instrument_data, family = "binomial")
bsl_imageability_interaction_summary <- summary(bsl_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_imageability_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_imageability_model <- glm(produces ~ age + chinese_imageability_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_imageability_effect <- ggeffect(chinese_beijing_imageability_model, terms = "chinese_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_imageability_model$coefficients[[3]])
chinese_beijing_imageability_summary <- summary(chinese_beijing_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_imageability_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_imageability_interaction_model <- glm(produces ~ age * chinese_imageability_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_imageability_interaction_summary <- summary(chinese_beijing_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_imageability_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_imageability_model <- glm(produces ~ age + chinese_imageability_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_imageability_effect <- ggeffect(chinese_cantonese_imageability_model, terms = "chinese_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_imageability_model$coefficients[[3]])
chinese_cantonese_imageability_summary <- summary(chinese_cantonese_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_imageability_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_imageability_interaction_model <- glm(produces ~ age * chinese_imageability_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_imageability_interaction_summary <- summary(chinese_cantonese_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_imageability_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_imageability_model <- glm(produces ~ age + chinese_imageability_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_imageability_effect <- ggeffect(chinese_taiwanese_imageability_model, terms = "chinese_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_imageability_model$coefficients[[3]])
chinese_taiwanese_imageability_summary <- summary(chinese_taiwanese_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_imageability_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_imageability_interaction_model <- glm(produces ~ age * chinese_imageability_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_imageability_interaction_summary <- summary(chinese_taiwanese_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_imageability_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_imageability_model <- glm(as.factor(produces) ~ age + croatian_imageability_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_imageability_effect <- ggpredict(croatian_imageability_model, terms = "croatian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_imageability_model$coefficients[[3]])
croatian_imageability_summary <- summary(croatian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_imageability_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_imageability_interaction_model <- glm(as.factor(produces) ~ age * croatian_imageability_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_imageability_interaction_summary <- summary(croatian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_imageability_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_imageability_model <- glm(as.factor(produces) ~ age + czech_imageability_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_imageability_effect <- ggpredict(czech_imageability_model, terms = "czech_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_imageability_model$coefficients[[3]])
czech_imageability_summary <- summary(czech_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_imageability_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_imageability_interaction_model <- glm(as.factor(produces) ~ age * czech_imageability_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_imageability_interaction_summary <- summary(czech_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_imageability_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_imageability_model <- glm(as.factor(produces) ~ age + danish_imageability_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_imageability_effect <- ggpredict(danish_imageability_model, terms = "danish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_imageability_model$coefficients[[3]])
danish_imageability_summary <- summary(danish_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_imageability_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_imageability_interaction_model <- glm(as.factor(produces) ~ age * danish_imageability_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_imageability_interaction_summary <- summary(danish_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_imageability_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_imageability_model <- glm(as.factor(produces) ~ age + dutch_imageability_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_imageability_effect <- ggpredict(dutch_imageability_model, terms = "dutch_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_imageability_model$coefficients[[3]])  
dutch_imageability_summary <- summary(dutch_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_imageability_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_imageability_interaction_model <- glm(as.factor(produces) ~ age * dutch_imageability_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_imageability_interaction_summary <- summary(dutch_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_imageability_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_imageability_model <- glm(produces ~ age + english_imageability_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_imageability_effect <- ggeffect(english_american_imageability_model, terms = "english_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_imageability_model$coefficients[[3]])
english_american_imageability_summary <- summary(english_american_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_imageability_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_imageability_interaction_model <- glm(produces ~ age * english_imageability_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_imageability_interaction_summary <- summary(english_american_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_imageability_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_imageability_model <- glm(produces ~ age + english_imageability_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_imageability_effect <- ggeffect(english_australian_imageability_model, terms = "english_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_imageability_model$coefficients[[3]])
english_australian_imageability_summary <- summary(english_australian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_imageability_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_imageability_interaction_model <- glm(produces ~ age * english_imageability_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_imageability_interaction_summary <- summary(english_australian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_imageability_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_imageability_model <- glm(produces ~ age + english_imageability_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_imageability_effect <- ggeffect(english_british_imageability_model, terms = "english_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_imageability_model$coefficients[[3]])
english_british_imageability_summary <- summary(english_british_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_imageability_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_imageability_interaction_model <- glm(produces ~ age * english_imageability_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_imageability_interaction_summary <- summary(english_british_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_imageability_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_imageability_model <- glm(produces ~ age + english_imageability_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_imageability_effect <- ggeffect(english_irish_imageability_model, terms = "english_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_imageability_model$coefficients[[3]])
english_irish_imageability_summary <- summary(english_irish_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_imageability_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_imageability_interaction_model <- glm(produces ~ age * english_imageability_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_imageability_interaction_summary <- summary(english_irish_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_imageability_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_imageability_model <- glm(as.factor(produces) ~ age + finnish_imageability_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_imageability_effect <- ggpredict(finnish_imageability_model, terms = "finnish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_imageability_model$coefficients[[3]]) 
finnish_imageability_summary <- summary(finnish_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_imageability_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_imageability_interaction_model <- glm(as.factor(produces) ~ age * finnish_imageability_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_imageability_interaction_summary <- summary(finnish_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_imageability_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_imageability_model <- glm(as.factor(produces) ~ age + french_imageability_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_imageability_effect <- ggpredict(french_european_imageability_model, terms = "french_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_imageability_model$coefficients[[3]]) 
french_european_imageability_summary <- summary(french_european_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_imageability_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_imageability_interaction_model <- glm(as.factor(produces) ~ age * french_imageability_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_imageability_interaction_summary <- summary(french_european_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_imageability_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_imageability_model <- glm(as.factor(produces) ~ age + french_imageability_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_imageability_effect <- ggpredict(french_quebecois_imageability_model, terms = "french_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_imageability_model$coefficients[[3]]) 
french_quebecois_imageability_summary <- summary(french_quebecois_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_imageability_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_imageability_interaction_model <- glm(as.factor(produces) ~ age * french_imageability_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_imageability_interaction_summary <- summary(french_quebecois_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_imageability_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_imageability_model <- glm(as.factor(produces) ~ age + german_imageability_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_imageability_effect <- ggpredict(german_imageability_model, terms = "german_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_imageability_model$coefficients[[3]]) 
german_imageability_summary <- summary(german_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_imageability_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_imageability_interaction_model <- glm(as.factor(produces) ~ age * german_imageability_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_imageability_interaction_summary <- summary(german_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_imageability_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_imageability_model <- glm(as.factor(produces) ~ age + greek_imageability_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_imageability_effect <- ggpredict(greek_imageability_model, terms = "greek_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_imageability_model$coefficients[[3]]) 
greek_imageability_summary <- summary(greek_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_imageability_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_imageability_interaction_model <- glm(as.factor(produces) ~ age * greek_imageability_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_imageability_interaction_summary <- summary(greek_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_imageability_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_imageability_model <- glm(as.factor(produces) ~ age + hebrew_imageability_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_imageability_effect <- ggpredict(hebrew_imageability_model, terms = "hebrew_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_imageability_model$coefficients[[3]]) 
hebrew_imageability_summary <- summary(hebrew_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_imageability_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_imageability_interaction_model <- glm(as.factor(produces) ~ age * hebrew_imageability_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_imageability_interaction_summary <- summary(hebrew_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_imageability_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_imageability_model <- glm(as.factor(produces) ~ age + hungarian_imageability_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_imageability_effect <- ggpredict(hungarian_imageability_model, terms = "hungarian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_imageability_model$coefficients[[3]])
hungarian_imageability_summary <- summary(hungarian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_imageability_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_imageability_interaction_model <- glm(as.factor(produces) ~ age * hungarian_imageability_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_imageability_interaction_summary <- summary(hungarian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_imageability_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_imageability_model <- glm(as.factor(produces) ~ age + irish_imageability_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_imageability_effect <- ggpredict(irish_imageability_model, terms = "irish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_imageability_model$coefficients[[3]])
irish_imageability_summary <- summary(irish_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_imageability_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_imageability_interaction_model <- glm(as.factor(produces) ~ age * irish_imageability_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_imageability_interaction_summary <- summary(irish_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_imageability_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_imageability_model <- glm(produces ~ age + italian_imageability_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_imageability_effect <- ggeffect(italian_imageability_model, terms = "italian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_imageability_model$coefficients[[3]])
italian_imageability_summary <- summary(italian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_imageability_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_imageability_interaction_model <- glm(produces ~ age * italian_imageability_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_imageability_interaction_summary <- summary(italian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_imageability_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_imageability_model <- glm(produces ~ age + kigiriama_imageability_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_imageability_effect <- ggeffect(kigiriama_imageability_model, terms = "kigiriama_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_imageability_model <- glm(produces ~ age + kiswahili_imageability_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_imageability_effect <- ggeffect(kiswahili_imageability_model, terms = "kiswahili_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_imageability_model$coefficients[[3]])
kiswahili_imageability_summary <- summary(kiswahili_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_imageability_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_imageability_interaction_model <- glm(produces ~ age * kiswahili_imageability_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_imageability_interaction_summary <- summary(kiswahili_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_imageability_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_imageability_model <- glm(produces ~ age + korean_imageability_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_imageability_effect <- ggeffect(korean_imageability_model, terms = "korean_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_imageability_model$coefficients[[3]])
korean_imageability_summary <- summary(korean_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_imageability_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_imageability_interaction_model <- glm(produces ~ age * korean_imageability_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_imageability_interaction_summary <- summary(korean_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_imageability_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_imageability_model <- glm(produces ~ age + latvian_imageability_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_imageability_effect <- ggeffect(latvian_imageability_model, terms = "latvian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_imageability_model$coefficients[[3]])
latvian_imageability_summary <- summary(latvian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_imageability_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_imageability_interaction_model <- glm(produces ~ age * latvian_imageability_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_imageability_interaction_summary <- summary(latvian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_imageability_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_imageability_model <- glm(produces ~ age + norwegian_imageability_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_imageability_effect <- ggeffect(norwegian_imageability_model, terms = "norwegian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_imageability_model$coefficients[[3]])
norwegian_imageability_summary <- summary(norwegian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_imageability_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_imageability_interaction_model <- glm(produces ~ age * norwegian_imageability_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_imageability_interaction_summary <- summary(norwegian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_imageability_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_imageability_model <- glm(produces ~ age + persian_imageability_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_imageability_effect <- ggeffect(persian_imageability_model, terms = "persian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_imageability_model$coefficients[[3]])
persian_imageability_summary <- summary(persian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_imageability_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_imageability_interaction_model <- glm(produces ~ age * persian_imageability_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_imageability_interaction_summary <- summary(persian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_imageability_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_imageability_model <- glm(produces ~ age + portuguese_imageability_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_imageability_effect <- ggeffect(portuguese_imageability_model, terms = "portuguese_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_imageability_model <- glm(produces ~ age + russian_imageability_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_imageability_effect <- ggeffect(russian_imageability_model, terms = "russian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_imageability_model$coefficients[[3]])
russian_imageability_summary <- summary(russian_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_imageability_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_imageability_interaction_model <- glm(produces ~ age * russian_imageability_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_imageability_interaction_summary <- summary(russian_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_imageability_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_imageability_model <- glm(produces ~ age + slovak_imageability_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_imageability_effect <- ggeffect(slovak_imageability_model, terms = "slovak_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_imageability_model$coefficients[[3]])
slovak_imageability_summary <- summary(slovak_imageability_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_imageability_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_imageability_interaction_model <- glm(produces ~ age * slovak_imageability_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_imageability_interaction_summary <- summary(slovak_imageability_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_imageability_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_imageability_model <- glm(produces ~ age + spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_imageability_effect <- ggeffect(spanish_argentinian_imageability_model, terms = "spanish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_imageability_model$coefficients[[3]])
spanish_argentinian_imageability_summary <- summary(spanish_argentinian_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_imageability_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_imageability_interaction_model <- glm(produces ~ age * spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_imageability_interaction_summary <- summary(spanish_argentinian_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_imageability_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_imageability_model <- glm(produces ~ age + spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_imageability_effect <- ggeffect(spanish_chilean_imageability_model, terms = "spanish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_imageability_model$coefficients[[3]])
spanish_chilean_imageability_summary <- summary(spanish_chilean_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_imageability_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_imageability_interaction_model <- glm(produces ~ age * spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_imageability_interaction_summary <- summary(spanish_chilean_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_imageability_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_imageability_model <- glm(produces ~ age + spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_imageability_effect <- ggeffect(spanish_european_imageability_model, terms = "spanish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_imageability_model$coefficients[[3]])
spanish_european_imageability_summary <- summary(spanish_european_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_imageability_rating") %>%
  mutate(language = "spanish_european")
spanish_european_imageability_interaction_model <- glm(produces ~ age * spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_imageability_interaction_summary <- summary(spanish_european_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_imageability_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_imageability_model <- glm(produces ~ age + spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_imageability_effect <- ggeffect(spanish_mexican_imageability_model, terms = "spanish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_imageability_model$coefficients[[3]])
spanish_mexican_imageability_summary <- summary(spanish_mexican_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_imageability_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_imageability_interaction_model <- glm(produces ~ age * spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_imageability_interaction_summary <- summary(spanish_mexican_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_imageability_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_imageability_model <- glm(produces ~ age + spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_imageability_effect <- ggeffect(spanish_peruvian_imageability_model, terms = "spanish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_imageability_model$coefficients[[3]])
spanish_peruvian_imageability_summary <- summary(spanish_peruvian_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_imageability_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_imageability_interaction_model <- glm(produces ~ age * spanish_imageability_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_imageability_interaction_summary <- summary(spanish_peruvian_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_imageability_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_imageability_model <- glm(produces ~ age + swedish_imageability_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_imageability_effect <- ggeffect(swedish_imageability_model, terms = "swedish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_imageability_model$coefficients[[3]])
swedish_imageability_summary <- summary(swedish_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_imageability_rating") %>%
  mutate(language = "swedish")
swedish_imageability_interaction_model <- glm(produces ~ age * swedish_imageability_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_imageability_interaction_model <- glm(produces ~ age * swedish_imageability_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_imageability_interaction_summary <- summary(swedish_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_imageability_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_imageability_model <- glm(as.factor(produces) ~ age + arabic_imageability_rating + lexical_category, 
                           data = arabic_instrument_data, family = "binomial")
arabic_imageability_effect <- ggpredict(arabic_imageability_model, terms = "arabic_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_imageability_model$coefficients[[3]])
arabic_imageability_summary <- summary(arabic_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_imageability_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_imageability_interaction_model <- glm(as.factor(produces) ~ age * arabic_imageability_rating + lexical_category, 
                                       data = arabic_instrument_data, family = "binomial")
arabic_imageability_interaction_summary <- summary(arabic_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_imageability_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_imageability_model <- glm(as.factor(produces) ~ age + catalan_imageability_rating + lexical_category, 
                            data = catalan_instrument_data, family = "binomial")
catalan_imageability_effect <- ggpredict(catalan_imageability_model, terms = "catalan_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_imageability_model$coefficients[[3]])
catalan_imageability_summary <- summary(catalan_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_imageability_rating") %>%
  mutate(language = "catalan") 
catalan_imageability_interaction_model <- glm(as.factor(produces) ~ age * catalan_imageability_rating  + lexical_category, 
                                        data = catalan_instrument_data, family = "binomial")
catalan_imageability_interaction_summary <- summary(catalan_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_imageability_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_imageability_model <- glm(as.factor(produces) ~ age + estonian_imageability_rating + lexical_category, 
                             data = estonian_instrument_data, family = "binomial")
estonian_imageability_effect <- ggpredict(estonian_imageability_model, terms = "estonian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_imageability_model$coefficients[[3]])
estonian_imageability_summary <- summary(estonian_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_imageability_rating") %>%
  mutate(language = "estonian") 
estonian_imageability_interaction_model <- glm(as.factor(produces) ~ age * estonian_imageability_rating  + lexical_category, 
                                         data = estonian_instrument_data, family = "binomial")
estonian_imageability_interaction_summary <- summary(estonian_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_imageability_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_imageability_model <- glm(as.factor(produces) ~ age + japanese_imageability_rating + lexical_category, 
                             data = japanese_instrument_data, family = "binomial")
japanese_imageability_effect <- ggpredict(japanese_imageability_model, terms = "japanese_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_imageability_model$coefficients[[3]])
japanese_imageability_summary <- summary(japanese_imageability_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_imageability_rating") %>%
  mutate(language = "japanese") 
japanese_imageability_interaction_model <- glm(as.factor(produces) ~ age * japanese_imageability_rating  + lexical_category, 
                                         data = japanese_instrument_data, family = "binomial")
japanese_imageability_interaction_summary <- summary(japanese_imageability_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_imageability_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_imageability_model <- glm(produces ~ age + turkish_imageability_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_imageability_effect <- ggeffect(turkish_imageability_model, terms = "turkish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_imageability_effects <- bind_rows(asl_imageability_effect,
                                bsl_imageability_effect,
                                chinese_beijing_imageability_effect,
                                chinese_cantonese_imageability_effect,
                                chinese_taiwanese_imageability_effect,
                                croatian_imageability_effect,
                                czech_imageability_effect,
                                english_american_imageability_effect,
                                english_australian_imageability_effect,
                                english_british_imageability_effect,
                                english_irish_imageability_effect,
                                danish_imageability_effect,
                                dutch_imageability_effect,
                                italian_imageability_effect,
                                finnish_imageability_effect,
                                french_european_imageability_effect,
                                french_quebecois_imageability_effect,
                                german_imageability_effect,
                                greek_imageability_effect,
                                hebrew_imageability_effect,
                                hungarian_imageability_effect,
                                irish_imageability_effect,
                                kiswahili_imageability_effect,
                                korean_imageability_effect,
                                latvian_imageability_effect,
                                norwegian_imageability_effect,
                                persian_imageability_effect,
                                russian_imageability_effect,
                                slovak_imageability_effect,
                                spanish_argentinian_imageability_effect,
                                spanish_chilean_imageability_effect,
                                spanish_european_imageability_effect,
                                spanish_mexican_imageability_effect,
                                spanish_peruvian_imageability_effect,
                                swedish_imageability_effect,
                                arabic_imageability_effect,
                                catalan_imageability_effect,
                                estonian_imageability_effect,
                                japanese_imageability_effect
                                # , turkish_imageability_effect
)
write_rds(all_imageability_effects, "models/effects/all_imageability_effects.rds")

all_imageability_effects_plot <- ggplot(all_imageability_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "imageability Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_imageability_effects_plots.png", all_imageability_effects_plot, height = 8, width = 5)


all_imageability_summaries <- bind_rows(asl_imageability_summary,
                                  bsl_imageability_summary,
                                  chinese_beijing_imageability_summary,
                                  chinese_cantonese_imageability_summary,
                                  chinese_taiwanese_imageability_summary,
                                  croatian_imageability_summary,
                                  czech_imageability_summary,
                                  english_american_imageability_summary,
                                  english_australian_imageability_summary,
                                  english_british_imageability_summary,
                                  english_irish_imageability_summary,
                                  danish_imageability_summary,
                                  dutch_imageability_summary,
                                  italian_imageability_summary,
                                  finnish_imageability_summary,
                                  french_european_imageability_summary,
                                  french_quebecois_imageability_summary,
                                  german_imageability_summary,
                                  greek_imageability_summary,
                                  hebrew_imageability_summary,
                                  hungarian_imageability_summary,
                                  irish_imageability_summary,
                                  kiswahili_imageability_summary,
                                  korean_imageability_summary,
                                  latvian_imageability_summary,
                                  norwegian_imageability_summary,
                                  persian_imageability_summary,
                                  russian_imageability_summary,
                                  slovak_imageability_summary,
                                  spanish_argentinian_imageability_summary,
                                  spanish_chilean_imageability_summary,
                                  spanish_european_imageability_summary,
                                  spanish_mexican_imageability_summary,
                                  spanish_peruvian_imageability_summary,
                                  swedish_imageability_summary,
                                  arabic_imageability_summary,
                                  catalan_imageability_summary,
                                  estonian_imageability_summary,
                                  japanese_imageability_summary
                                  # , turkish_imageability_summary
) %>%
  mutate(variable = "imageability",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_imageability_summaries, "models/effects/all_imageability_summaries.rds")



all_imageability_interaction_summaries <- bind_rows(asl_imageability_interaction_summary,
                                              bsl_imageability_interaction_summary,
                                              chinese_beijing_imageability_interaction_summary,
                                              chinese_cantonese_imageability_interaction_summary,
                                              chinese_taiwanese_imageability_interaction_summary,
                                              croatian_imageability_interaction_summary,
                                              czech_imageability_interaction_summary,
                                              english_american_imageability_interaction_summary,
                                              english_australian_imageability_interaction_summary,
                                              english_british_imageability_interaction_summary,
                                              english_irish_imageability_interaction_summary,
                                              danish_imageability_interaction_summary,
                                              dutch_imageability_interaction_summary,
                                              italian_imageability_interaction_summary,
                                              finnish_imageability_interaction_summary,
                                              french_european_imageability_interaction_summary,
                                              french_quebecois_imageability_interaction_summary,
                                              german_imageability_interaction_summary,
                                              greek_imageability_interaction_summary,
                                              hebrew_imageability_interaction_summary,
                                              hungarian_imageability_interaction_summary,
                                              irish_imageability_interaction_summary,
                                              kiswahili_imageability_interaction_summary,
                                              korean_imageability_interaction_summary,
                                              latvian_imageability_interaction_summary,
                                              norwegian_imageability_interaction_summary,
                                              persian_imageability_interaction_summary,
                                              russian_imageability_interaction_summary,
                                              slovak_imageability_interaction_summary,
                                              spanish_argentinian_imageability_interaction_summary,
                                              spanish_chilean_imageability_interaction_summary,
                                              spanish_european_imageability_interaction_summary,
                                              spanish_mexican_imageability_interaction_summary,
                                              spanish_peruvian_imageability_interaction_summary,
                                              swedish_imageability_interaction_summary,
                                              arabic_imageability_interaction_summary,
                                              catalan_imageability_interaction_summary,
                                              estonian_imageability_interaction_summary,
                                              japanese_imageability_interaction_summary
                                              # , turkish_imageability_summary
) %>%
  mutate(variable = "imageability",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_imageability_interaction_summaries, "models/effects/all_imageability_interaction_summaries.rds")
