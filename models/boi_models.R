library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# boi
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_boi_model <- glm(as.factor(produces) ~ age + asl_boi_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                          data = asl_instrument_data, family = "binomial")
asl_boi_effect <- ggpredict(asl_boi_model, terms = "asl_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_boi_model$coefficients[[3]])
asl_boi_summary <- summary(asl_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_boi_rating") %>%
  mutate(language = "asl") 
asl_boi_interaction_model <- glm(as.factor(produces) ~ age * asl_boi_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                      data = asl_instrument_data, family = "binomial")
asl_boi_interaction_summary <- summary(asl_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_boi_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_boi_model <- glm(as.factor(produces) ~ age + bsl_boi_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_boi_effect <- ggpredict(bsl_boi_model, terms = "bsl_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_boi_model$coefficients[[3]]) 
bsl_boi_summary <- summary(bsl_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_boi_rating") %>%
  mutate(language = "bsl")
bsl_boi_interaction_model <- glm(as.factor(produces) ~ age * bsl_boi_rating + lexical_category, 
                                      data = bsl_instrument_data, family = "binomial")
bsl_boi_interaction_summary <- summary(bsl_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_boi_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_boi_model <- glm(produces ~ age + chinese_boi_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_boi_effect <- ggeffect(chinese_beijing_boi_model, terms = "chinese_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_boi_model$coefficients[[3]])
chinese_beijing_boi_summary <- summary(chinese_beijing_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_boi_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_boi_interaction_model <- glm(produces ~ age * chinese_boi_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_boi_interaction_summary <- summary(chinese_beijing_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_boi_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_boi_model <- glm(produces ~ age + chinese_boi_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_boi_effect <- ggeffect(chinese_cantonese_boi_model, terms = "chinese_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_boi_model$coefficients[[3]])
chinese_cantonese_boi_summary <- summary(chinese_cantonese_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_boi_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_boi_interaction_model <- glm(produces ~ age * chinese_boi_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_boi_interaction_summary <- summary(chinese_cantonese_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_boi_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_boi_model <- glm(produces ~ age + chinese_boi_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_boi_effect <- ggeffect(chinese_taiwanese_boi_model, terms = "chinese_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_boi_model$coefficients[[3]])
chinese_taiwanese_boi_summary <- summary(chinese_taiwanese_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_boi_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_boi_interaction_model <- glm(produces ~ age * chinese_boi_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_boi_interaction_summary <- summary(chinese_taiwanese_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_boi_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_boi_model <- glm(as.factor(produces) ~ age + croatian_boi_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_boi_effect <- ggpredict(croatian_boi_model, terms = "croatian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_boi_model$coefficients[[3]])
croatian_boi_summary <- summary(croatian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_boi_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_boi_interaction_model <- glm(as.factor(produces) ~ age * croatian_boi_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_boi_interaction_summary <- summary(croatian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_boi_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_boi_model <- glm(as.factor(produces) ~ age + czech_boi_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_boi_effect <- ggpredict(czech_boi_model, terms = "czech_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_boi_model$coefficients[[3]])
czech_boi_summary <- summary(czech_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_boi_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_boi_interaction_model <- glm(as.factor(produces) ~ age * czech_boi_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_boi_interaction_summary <- summary(czech_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_boi_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_boi_model <- glm(as.factor(produces) ~ age + danish_boi_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_boi_effect <- ggpredict(danish_boi_model, terms = "danish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_boi_model$coefficients[[3]])
danish_boi_summary <- summary(danish_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_boi_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_boi_interaction_model <- glm(as.factor(produces) ~ age * danish_boi_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_boi_interaction_summary <- summary(danish_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_boi_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_boi_model <- glm(as.factor(produces) ~ age + dutch_boi_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_boi_effect <- ggpredict(dutch_boi_model, terms = "dutch_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_boi_model$coefficients[[3]])  
dutch_boi_summary <- summary(dutch_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_boi_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_boi_interaction_model <- glm(as.factor(produces) ~ age * dutch_boi_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_boi_interaction_summary <- summary(dutch_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_boi_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_boi_model <- glm(produces ~ age + english_boi_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_boi_effect <- ggeffect(english_american_boi_model, terms = "english_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_boi_model$coefficients[[3]])
english_american_boi_summary <- summary(english_american_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_boi_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_boi_interaction_model <- glm(produces ~ age * english_boi_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_boi_interaction_summary <- summary(english_american_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_boi_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_boi_model <- glm(produces ~ age + english_boi_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_boi_effect <- ggeffect(english_australian_boi_model, terms = "english_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_boi_model$coefficients[[3]])
english_australian_boi_summary <- summary(english_australian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_boi_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_boi_interaction_model <- glm(produces ~ age * english_boi_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_boi_interaction_summary <- summary(english_australian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_boi_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_boi_model <- glm(produces ~ age + english_boi_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_boi_effect <- ggeffect(english_british_boi_model, terms = "english_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_boi_model$coefficients[[3]])
english_british_boi_summary <- summary(english_british_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_boi_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_boi_interaction_model <- glm(produces ~ age * english_boi_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_boi_interaction_summary <- summary(english_british_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_boi_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_boi_model <- glm(produces ~ age + english_boi_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_boi_effect <- ggeffect(english_irish_boi_model, terms = "english_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_boi_model$coefficients[[3]])
english_irish_boi_summary <- summary(english_irish_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_boi_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_boi_interaction_model <- glm(produces ~ age * english_boi_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_boi_interaction_summary <- summary(english_irish_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_boi_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_boi_model <- glm(as.factor(produces) ~ age + finnish_boi_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_boi_effect <- ggpredict(finnish_boi_model, terms = "finnish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_boi_model$coefficients[[3]]) 
finnish_boi_summary <- summary(finnish_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_boi_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_boi_interaction_model <- glm(as.factor(produces) ~ age * finnish_boi_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_boi_interaction_summary <- summary(finnish_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_boi_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_boi_model <- glm(as.factor(produces) ~ age + french_boi_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_boi_effect <- ggpredict(french_european_boi_model, terms = "french_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_boi_model$coefficients[[3]]) 
french_european_boi_summary <- summary(french_european_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_boi_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_boi_interaction_model <- glm(as.factor(produces) ~ age * french_boi_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_boi_interaction_summary <- summary(french_european_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_boi_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_boi_model <- glm(as.factor(produces) ~ age + french_boi_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_boi_effect <- ggpredict(french_quebecois_boi_model, terms = "french_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_boi_model$coefficients[[3]]) 
french_quebecois_boi_summary <- summary(french_quebecois_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_boi_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_boi_interaction_model <- glm(as.factor(produces) ~ age * french_boi_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_boi_interaction_summary <- summary(french_quebecois_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_boi_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_boi_model <- glm(as.factor(produces) ~ age + german_boi_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_boi_effect <- ggpredict(german_boi_model, terms = "german_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_boi_model$coefficients[[3]]) 
german_boi_summary <- summary(german_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_boi_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_boi_interaction_model <- glm(as.factor(produces) ~ age * german_boi_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_boi_interaction_summary <- summary(german_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_boi_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_boi_model <- glm(as.factor(produces) ~ age + greek_boi_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_boi_effect <- ggpredict(greek_boi_model, terms = "greek_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_boi_model$coefficients[[3]]) 
greek_boi_summary <- summary(greek_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_boi_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_boi_interaction_model <- glm(as.factor(produces) ~ age * greek_boi_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_boi_interaction_summary <- summary(greek_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_boi_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_boi_model <- glm(as.factor(produces) ~ age + hebrew_boi_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_boi_effect <- ggpredict(hebrew_boi_model, terms = "hebrew_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_boi_model$coefficients[[3]]) 
hebrew_boi_summary <- summary(hebrew_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_boi_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_boi_interaction_model <- glm(as.factor(produces) ~ age * hebrew_boi_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_boi_interaction_summary <- summary(hebrew_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_boi_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_boi_model <- glm(as.factor(produces) ~ age + hungarian_boi_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_boi_effect <- ggpredict(hungarian_boi_model, terms = "hungarian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_boi_model$coefficients[[3]])
hungarian_boi_summary <- summary(hungarian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_boi_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_boi_interaction_model <- glm(as.factor(produces) ~ age * hungarian_boi_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_boi_interaction_summary <- summary(hungarian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_boi_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_boi_model <- glm(as.factor(produces) ~ age + irish_boi_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_boi_effect <- ggpredict(irish_boi_model, terms = "irish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_boi_model$coefficients[[3]])
irish_boi_summary <- summary(irish_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_boi_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_boi_interaction_model <- glm(as.factor(produces) ~ age * irish_boi_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_boi_interaction_summary <- summary(irish_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_boi_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_boi_model <- glm(produces ~ age + italian_boi_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_boi_effect <- ggeffect(italian_boi_model, terms = "italian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_boi_model$coefficients[[3]])
italian_boi_summary <- summary(italian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_boi_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_boi_interaction_model <- glm(produces ~ age * italian_boi_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_boi_interaction_summary <- summary(italian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_boi_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_boi_model <- glm(produces ~ age + kigiriama_boi_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_boi_effect <- ggeffect(kigiriama_boi_model, terms = "kigiriama_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_boi_model <- glm(produces ~ age + kiswahili_boi_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_boi_effect <- ggeffect(kiswahili_boi_model, terms = "kiswahili_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_boi_model$coefficients[[3]])
kiswahili_boi_summary <- summary(kiswahili_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_boi_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_boi_interaction_model <- glm(produces ~ age * kiswahili_boi_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_boi_interaction_summary <- summary(kiswahili_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_boi_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_boi_model <- glm(produces ~ age + korean_boi_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_boi_effect <- ggeffect(korean_boi_model, terms = "korean_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_boi_model$coefficients[[3]])
korean_boi_summary <- summary(korean_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_boi_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_boi_interaction_model <- glm(produces ~ age * korean_boi_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_boi_interaction_summary <- summary(korean_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_boi_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_boi_model <- glm(produces ~ age + latvian_boi_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_boi_effect <- ggeffect(latvian_boi_model, terms = "latvian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_boi_model$coefficients[[3]])
latvian_boi_summary <- summary(latvian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_boi_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_boi_interaction_model <- glm(produces ~ age * latvian_boi_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_boi_interaction_summary <- summary(latvian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_boi_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_boi_model <- glm(produces ~ age + norwegian_boi_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_boi_effect <- ggeffect(norwegian_boi_model, terms = "norwegian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_boi_model$coefficients[[3]])
norwegian_boi_summary <- summary(norwegian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_boi_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_boi_interaction_model <- glm(produces ~ age * norwegian_boi_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_boi_interaction_summary <- summary(norwegian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_boi_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_boi_model <- glm(produces ~ age + persian_boi_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_boi_effect <- ggeffect(persian_boi_model, terms = "persian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_boi_model$coefficients[[3]])
persian_boi_summary <- summary(persian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_boi_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_boi_interaction_model <- glm(produces ~ age * persian_boi_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_boi_interaction_summary <- summary(persian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_boi_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_boi_model <- glm(produces ~ age + portuguese_boi_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_boi_effect <- ggeffect(portuguese_boi_model, terms = "portuguese_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_boi_model <- glm(produces ~ age + russian_boi_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_boi_effect <- ggeffect(russian_boi_model, terms = "russian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_boi_model$coefficients[[3]])
russian_boi_summary <- summary(russian_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_boi_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_boi_interaction_model <- glm(produces ~ age * russian_boi_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_boi_interaction_summary <- summary(russian_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_boi_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_boi_model <- glm(produces ~ age + slovak_boi_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_boi_effect <- ggeffect(slovak_boi_model, terms = "slovak_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_boi_model$coefficients[[3]])
slovak_boi_summary <- summary(slovak_boi_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_boi_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_boi_interaction_model <- glm(produces ~ age * slovak_boi_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_boi_interaction_summary <- summary(slovak_boi_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_boi_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_boi_model <- glm(produces ~ age + spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_boi_effect <- ggeffect(spanish_argentinian_boi_model, terms = "spanish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_boi_model$coefficients[[3]])
spanish_argentinian_boi_summary <- summary(spanish_argentinian_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_boi_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_boi_interaction_model <- glm(produces ~ age * spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_boi_interaction_summary <- summary(spanish_argentinian_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_boi_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_boi_model <- glm(produces ~ age + spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_boi_effect <- ggeffect(spanish_chilean_boi_model, terms = "spanish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_boi_model$coefficients[[3]])
spanish_chilean_boi_summary <- summary(spanish_chilean_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_boi_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_boi_interaction_model <- glm(produces ~ age * spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_boi_interaction_summary <- summary(spanish_chilean_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_boi_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_boi_model <- glm(produces ~ age + spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_boi_effect <- ggeffect(spanish_european_boi_model, terms = "spanish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_boi_model$coefficients[[3]])
spanish_european_boi_summary <- summary(spanish_european_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_boi_rating") %>%
  mutate(language = "spanish_european")
spanish_european_boi_interaction_model <- glm(produces ~ age * spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_boi_interaction_summary <- summary(spanish_european_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_boi_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_boi_model <- glm(produces ~ age + spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_boi_effect <- ggeffect(spanish_mexican_boi_model, terms = "spanish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_boi_model$coefficients[[3]])
spanish_mexican_boi_summary <- summary(spanish_mexican_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_boi_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_boi_interaction_model <- glm(produces ~ age * spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_boi_interaction_summary <- summary(spanish_mexican_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_boi_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_boi_model <- glm(produces ~ age + spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_boi_effect <- ggeffect(spanish_peruvian_boi_model, terms = "spanish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_boi_model$coefficients[[3]])
spanish_peruvian_boi_summary <- summary(spanish_peruvian_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_boi_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_boi_interaction_model <- glm(produces ~ age * spanish_boi_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_boi_interaction_summary <- summary(spanish_peruvian_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_boi_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_boi_model <- glm(produces ~ age + swedish_boi_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_boi_effect <- ggeffect(swedish_boi_model, terms = "swedish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_boi_model$coefficients[[3]])
swedish_boi_summary <- summary(swedish_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_boi_rating") %>%
  mutate(language = "swedish")
swedish_boi_interaction_model <- glm(produces ~ age * swedish_boi_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_boi_interaction_model <- glm(produces ~ age * swedish_boi_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_boi_interaction_summary <- summary(swedish_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_boi_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_boi_model <- glm(as.factor(produces) ~ age + arabic_boi_rating + lexical_category, 
                             data = arabic_instrument_data, family = "binomial")
arabic_boi_effect <- ggpredict(arabic_boi_model, terms = "arabic_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_boi_model$coefficients[[3]])
arabic_boi_summary <- summary(arabic_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_boi_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_boi_interaction_model <- glm(as.factor(produces) ~ age * arabic_boi_rating + lexical_category, 
                                         data = arabic_instrument_data, family = "binomial")
arabic_boi_interaction_summary <- summary(arabic_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_boi_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_boi_model <- glm(as.factor(produces) ~ age + catalan_boi_rating + lexical_category, 
                              data = catalan_instrument_data, family = "binomial")
catalan_boi_effect <- ggpredict(catalan_boi_model, terms = "catalan_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_boi_model$coefficients[[3]])
catalan_boi_summary <- summary(catalan_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_boi_rating") %>%
  mutate(language = "catalan") 
catalan_boi_interaction_model <- glm(as.factor(produces) ~ age * catalan_boi_rating  + lexical_category, 
                                          data = catalan_instrument_data, family = "binomial")
catalan_boi_interaction_summary <- summary(catalan_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_boi_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_boi_model <- glm(as.factor(produces) ~ age + estonian_boi_rating + lexical_category, 
                               data = estonian_instrument_data, family = "binomial")
estonian_boi_effect <- ggpredict(estonian_boi_model, terms = "estonian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_boi_model$coefficients[[3]])
estonian_boi_summary <- summary(estonian_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_boi_rating") %>%
  mutate(language = "estonian") 
estonian_boi_interaction_model <- glm(as.factor(produces) ~ age * estonian_boi_rating  + lexical_category, 
                                           data = estonian_instrument_data, family = "binomial")
estonian_boi_interaction_summary <- summary(estonian_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_boi_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_boi_model <- glm(as.factor(produces) ~ age + japanese_boi_rating + lexical_category, 
                               data = japanese_instrument_data, family = "binomial")
japanese_boi_effect <- ggpredict(japanese_boi_model, terms = "japanese_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_boi_model$coefficients[[3]])
japanese_boi_summary <- summary(japanese_boi_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_boi_rating") %>%
  mutate(language = "japanese") 
japanese_boi_interaction_model <- glm(as.factor(produces) ~ age * japanese_boi_rating  + lexical_category, 
                                           data = japanese_instrument_data, family = "binomial")
japanese_boi_interaction_summary <- summary(japanese_boi_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_boi_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_boi_model <- glm(produces ~ age + turkish_boi_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_boi_effect <- ggeffect(turkish_boi_model, terms = "turkish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_boi_effects <- bind_rows(asl_boi_effect,
                                  bsl_boi_effect,
                                  chinese_beijing_boi_effect,
                                  chinese_cantonese_boi_effect,
                                  chinese_taiwanese_boi_effect,
                                  croatian_boi_effect,
                                  czech_boi_effect,
                                  english_american_boi_effect,
                                  english_australian_boi_effect,
                                  english_british_boi_effect,
                                  english_irish_boi_effect,
                                  danish_boi_effect,
                                  dutch_boi_effect,
                                  italian_boi_effect,
                                  finnish_boi_effect,
                                  french_european_boi_effect,
                                  french_quebecois_boi_effect,
                                  german_boi_effect,
                                  greek_boi_effect,
                                  hebrew_boi_effect,
                                  hungarian_boi_effect,
                                  irish_boi_effect,
                                  kiswahili_boi_effect,
                                  korean_boi_effect,
                                  latvian_boi_effect,
                                  norwegian_boi_effect,
                                  persian_boi_effect,
                                  russian_boi_effect,
                                  slovak_boi_effect,
                                  spanish_argentinian_boi_effect,
                                  spanish_chilean_boi_effect,
                                  spanish_european_boi_effect,
                                  spanish_mexican_boi_effect,
                                  spanish_peruvian_boi_effect,
                                  swedish_boi_effect,
                                  arabic_boi_effect,
                                  catalan_boi_effect,
                                  estonian_boi_effect,
                                  japanese_boi_effect
                                  # , turkish_boi_effect
)
write_rds(all_boi_effects, "models/effects/all_boi_effects.rds")

all_boi_effects_plot <- ggplot(all_boi_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Body Object Interaction Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_boi_effects_plots.png", all_boi_effects_plot, height = 8, width = 5)


all_boi_summaries <- bind_rows(asl_boi_summary,
                                    bsl_boi_summary,
                                    chinese_beijing_boi_summary,
                                    chinese_cantonese_boi_summary,
                                    chinese_taiwanese_boi_summary,
                                    croatian_boi_summary,
                                    czech_boi_summary,
                                    english_american_boi_summary,
                                    english_australian_boi_summary,
                                    english_british_boi_summary,
                                    english_irish_boi_summary,
                                    danish_boi_summary,
                                    dutch_boi_summary,
                                    italian_boi_summary,
                                    finnish_boi_summary,
                                    french_european_boi_summary,
                                    french_quebecois_boi_summary,
                                    german_boi_summary,
                                    greek_boi_summary,
                                    hebrew_boi_summary,
                                    hungarian_boi_summary,
                                    irish_boi_summary,
                                    kiswahili_boi_summary,
                                    korean_boi_summary,
                                    latvian_boi_summary,
                                    norwegian_boi_summary,
                                    persian_boi_summary,
                                    russian_boi_summary,
                                    slovak_boi_summary,
                                    spanish_argentinian_boi_summary,
                                    spanish_chilean_boi_summary,
                                    spanish_european_boi_summary,
                                    spanish_mexican_boi_summary,
                                    spanish_peruvian_boi_summary,
                                    swedish_boi_summary,
                                    arabic_boi_summary,
                                    catalan_boi_summary,
                                    estonian_boi_summary,
                                    japanese_boi_summary
                                    # , turkish_boi_summary
) %>%
  mutate(variable = "Body Object Interaction",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_boi_summaries, "models/effects/all_boi_summaries.rds")



all_boi_interaction_summaries <- bind_rows(asl_boi_interaction_summary,
                                                bsl_boi_interaction_summary,
                                                chinese_beijing_boi_interaction_summary,
                                                chinese_cantonese_boi_interaction_summary,
                                                chinese_taiwanese_boi_interaction_summary,
                                                croatian_boi_interaction_summary,
                                                czech_boi_interaction_summary,
                                                english_american_boi_interaction_summary,
                                                english_australian_boi_interaction_summary,
                                                english_british_boi_interaction_summary,
                                                english_irish_boi_interaction_summary,
                                                danish_boi_interaction_summary,
                                                dutch_boi_interaction_summary,
                                                italian_boi_interaction_summary,
                                                finnish_boi_interaction_summary,
                                                french_european_boi_interaction_summary,
                                                french_quebecois_boi_interaction_summary,
                                                german_boi_interaction_summary,
                                                greek_boi_interaction_summary,
                                                hebrew_boi_interaction_summary,
                                                hungarian_boi_interaction_summary,
                                                irish_boi_interaction_summary,
                                                kiswahili_boi_interaction_summary,
                                                korean_boi_interaction_summary,
                                                latvian_boi_interaction_summary,
                                                norwegian_boi_interaction_summary,
                                                persian_boi_interaction_summary,
                                                russian_boi_interaction_summary,
                                                slovak_boi_interaction_summary,
                                                spanish_argentinian_boi_interaction_summary,
                                                spanish_chilean_boi_interaction_summary,
                                                spanish_european_boi_interaction_summary,
                                                spanish_mexican_boi_interaction_summary,
                                                spanish_peruvian_boi_interaction_summary,
                                                swedish_boi_interaction_summary,
                                                arabic_boi_interaction_summary,
                                                catalan_boi_interaction_summary,
                                                estonian_boi_interaction_summary,
                                                japanese_boi_interaction_summary
                                                # , turkish_boi_summary
) %>%
  mutate(variable = "Body Object Interaction",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_boi_interaction_summaries, "models/effects/all_boi_interaction_summaries.rds")
