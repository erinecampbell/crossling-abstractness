library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# auditory
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_auditory_model <- glm(as.factor(produces) ~ age + asl_auditory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                     data = asl_instrument_data, family = "binomial")
asl_auditory_effect <- ggpredict(asl_auditory_model, terms = "asl_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_auditory_model$coefficients[[3]])
asl_auditory_summary <- summary(asl_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_auditory_rating") %>%
  mutate(language = "asl") 
asl_auditory_interaction_model <- glm(as.factor(produces) ~ age * asl_auditory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                          data = asl_instrument_data, family = "binomial")
asl_auditory_interaction_summary <- summary(asl_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_auditory_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_auditory_model <- glm(as.factor(produces) ~ age + bsl_auditory_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_auditory_effect <- ggpredict(bsl_auditory_model, terms = "bsl_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_auditory_model$coefficients[[3]]) 
bsl_auditory_summary <- summary(bsl_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_auditory_rating") %>%
  mutate(language = "bsl")
bsl_auditory_interaction_model <- glm(as.factor(produces) ~ age * bsl_auditory_rating + lexical_category, 
                                      data = bsl_instrument_data, family = "binomial")
bsl_auditory_interaction_summary <- summary(bsl_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_auditory_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_auditory_model <- glm(produces ~ age + chinese_auditory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_auditory_effect <- ggeffect(chinese_beijing_auditory_model, terms = "chinese_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_auditory_model$coefficients[[3]])
chinese_beijing_auditory_summary <- summary(chinese_beijing_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_auditory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_auditory_interaction_model <- glm(produces ~ age * chinese_auditory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_auditory_interaction_summary <- summary(chinese_beijing_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_auditory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_auditory_model <- glm(produces ~ age + chinese_auditory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_auditory_effect <- ggeffect(chinese_cantonese_auditory_model, terms = "chinese_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_auditory_model$coefficients[[3]])
chinese_cantonese_auditory_summary <- summary(chinese_cantonese_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_auditory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_auditory_interaction_model <- glm(produces ~ age * chinese_auditory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_auditory_interaction_summary <- summary(chinese_cantonese_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_auditory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_auditory_model <- glm(produces ~ age + chinese_auditory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_auditory_effect <- ggeffect(chinese_taiwanese_auditory_model, terms = "chinese_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_auditory_model$coefficients[[3]])
chinese_taiwanese_auditory_summary <- summary(chinese_taiwanese_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_auditory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_auditory_interaction_model <- glm(produces ~ age * chinese_auditory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_auditory_interaction_summary <- summary(chinese_taiwanese_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_auditory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_auditory_model <- glm(as.factor(produces) ~ age + croatian_auditory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_auditory_effect <- ggpredict(croatian_auditory_model, terms = "croatian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_auditory_model$coefficients[[3]])
croatian_auditory_summary <- summary(croatian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_auditory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_auditory_interaction_model <- glm(as.factor(produces) ~ age * croatian_auditory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_auditory_interaction_summary <- summary(croatian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_auditory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_auditory_model <- glm(as.factor(produces) ~ age + czech_auditory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_auditory_effect <- ggpredict(czech_auditory_model, terms = "czech_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_auditory_model$coefficients[[3]])
czech_auditory_summary <- summary(czech_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_auditory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_auditory_interaction_model <- glm(as.factor(produces) ~ age * czech_auditory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_auditory_interaction_summary <- summary(czech_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_auditory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_auditory_model <- glm(as.factor(produces) ~ age + danish_auditory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_auditory_effect <- ggpredict(danish_auditory_model, terms = "danish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_auditory_model$coefficients[[3]])
danish_auditory_summary <- summary(danish_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_auditory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_auditory_interaction_model <- glm(as.factor(produces) ~ age * danish_auditory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_auditory_interaction_summary <- summary(danish_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_auditory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_auditory_model <- glm(as.factor(produces) ~ age + dutch_auditory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_auditory_effect <- ggpredict(dutch_auditory_model, terms = "dutch_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_auditory_model$coefficients[[3]])  
dutch_auditory_summary <- summary(dutch_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_auditory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_auditory_interaction_model <- glm(as.factor(produces) ~ age * dutch_auditory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_auditory_interaction_summary <- summary(dutch_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_auditory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_auditory_model <- glm(produces ~ age + english_auditory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_auditory_effect <- ggeffect(english_american_auditory_model, terms = "english_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_auditory_model$coefficients[[3]])
english_american_auditory_summary <- summary(english_american_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_auditory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_auditory_interaction_model <- glm(produces ~ age * english_auditory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_auditory_interaction_summary <- summary(english_american_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_auditory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_auditory_model <- glm(produces ~ age + english_auditory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_auditory_effect <- ggeffect(english_australian_auditory_model, terms = "english_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_auditory_model$coefficients[[3]])
english_australian_auditory_summary <- summary(english_australian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_auditory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_auditory_interaction_model <- glm(produces ~ age * english_auditory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_auditory_interaction_summary <- summary(english_australian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_auditory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_auditory_model <- glm(produces ~ age + english_auditory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_auditory_effect <- ggeffect(english_british_auditory_model, terms = "english_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_auditory_model$coefficients[[3]])
english_british_auditory_summary <- summary(english_british_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_auditory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_auditory_interaction_model <- glm(produces ~ age * english_auditory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_auditory_interaction_summary <- summary(english_british_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_auditory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_auditory_model <- glm(produces ~ age + english_auditory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_auditory_effect <- ggeffect(english_irish_auditory_model, terms = "english_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_auditory_model$coefficients[[3]])
english_irish_auditory_summary <- summary(english_irish_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_auditory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_auditory_interaction_model <- glm(produces ~ age * english_auditory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_auditory_interaction_summary <- summary(english_irish_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_auditory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_auditory_model <- glm(as.factor(produces) ~ age + finnish_auditory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_auditory_effect <- ggpredict(finnish_auditory_model, terms = "finnish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_auditory_model$coefficients[[3]]) 
finnish_auditory_summary <- summary(finnish_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_auditory_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_auditory_interaction_model <- glm(as.factor(produces) ~ age * finnish_auditory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_auditory_interaction_summary <- summary(finnish_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_auditory_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_auditory_model <- glm(as.factor(produces) ~ age + french_auditory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_auditory_effect <- ggpredict(french_european_auditory_model, terms = "french_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_auditory_model$coefficients[[3]]) 
french_european_auditory_summary <- summary(french_european_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_auditory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_auditory_interaction_model <- glm(as.factor(produces) ~ age * french_auditory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_auditory_interaction_summary <- summary(french_european_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_auditory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_auditory_model <- glm(as.factor(produces) ~ age + french_auditory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_auditory_effect <- ggpredict(french_quebecois_auditory_model, terms = "french_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_auditory_model$coefficients[[3]]) 
french_quebecois_auditory_summary <- summary(french_quebecois_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_auditory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_auditory_interaction_model <- glm(as.factor(produces) ~ age * french_auditory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_auditory_interaction_summary <- summary(french_quebecois_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_auditory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_auditory_model <- glm(as.factor(produces) ~ age + german_auditory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_auditory_effect <- ggpredict(german_auditory_model, terms = "german_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_auditory_model$coefficients[[3]]) 
german_auditory_summary <- summary(german_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_auditory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_auditory_interaction_model <- glm(as.factor(produces) ~ age * german_auditory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_auditory_interaction_summary <- summary(german_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_auditory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_auditory_model <- glm(as.factor(produces) ~ age + greek_auditory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_auditory_effect <- ggpredict(greek_auditory_model, terms = "greek_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_auditory_model$coefficients[[3]]) 
greek_auditory_summary <- summary(greek_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_auditory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_auditory_interaction_model <- glm(as.factor(produces) ~ age * greek_auditory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_auditory_interaction_summary <- summary(greek_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_auditory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_auditory_model <- glm(as.factor(produces) ~ age + hebrew_auditory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_auditory_effect <- ggpredict(hebrew_auditory_model, terms = "hebrew_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_auditory_model$coefficients[[3]]) 
hebrew_auditory_summary <- summary(hebrew_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_auditory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_auditory_interaction_model <- glm(as.factor(produces) ~ age * hebrew_auditory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_auditory_interaction_summary <- summary(hebrew_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_auditory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_auditory_model <- glm(as.factor(produces) ~ age + hungarian_auditory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_auditory_effect <- ggpredict(hungarian_auditory_model, terms = "hungarian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_auditory_model$coefficients[[3]])
hungarian_auditory_summary <- summary(hungarian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_auditory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_auditory_interaction_model <- glm(as.factor(produces) ~ age * hungarian_auditory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_auditory_interaction_summary <- summary(hungarian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_auditory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_auditory_model <- glm(as.factor(produces) ~ age + irish_auditory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_auditory_effect <- ggpredict(irish_auditory_model, terms = "irish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_auditory_model$coefficients[[3]])
irish_auditory_summary <- summary(irish_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_auditory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_auditory_interaction_model <- glm(as.factor(produces) ~ age * irish_auditory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_auditory_interaction_summary <- summary(irish_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_auditory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_auditory_model <- glm(produces ~ age + italian_auditory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_auditory_effect <- ggeffect(italian_auditory_model, terms = "italian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_auditory_model$coefficients[[3]])
italian_auditory_summary <- summary(italian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_auditory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_auditory_interaction_model <- glm(produces ~ age * italian_auditory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_auditory_interaction_summary <- summary(italian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_auditory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_auditory_model <- glm(produces ~ age + kigiriama_auditory_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_auditory_effect <- ggeffect(kigiriama_auditory_model, terms = "kigiriama_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_auditory_model <- glm(produces ~ age + kiswahili_auditory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_auditory_effect <- ggeffect(kiswahili_auditory_model, terms = "kiswahili_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_auditory_model$coefficients[[3]])
kiswahili_auditory_summary <- summary(kiswahili_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_auditory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_auditory_interaction_model <- glm(produces ~ age * kiswahili_auditory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_auditory_interaction_summary <- summary(kiswahili_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_auditory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_auditory_model <- glm(produces ~ age + korean_auditory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_auditory_effect <- ggeffect(korean_auditory_model, terms = "korean_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_auditory_model$coefficients[[3]])
korean_auditory_summary <- summary(korean_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_auditory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_auditory_interaction_model <- glm(produces ~ age * korean_auditory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_auditory_interaction_summary <- summary(korean_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_auditory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_auditory_model <- glm(produces ~ age + latvian_auditory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_auditory_effect <- ggeffect(latvian_auditory_model, terms = "latvian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_auditory_model$coefficients[[3]])
latvian_auditory_summary <- summary(latvian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_auditory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_auditory_interaction_model <- glm(produces ~ age * latvian_auditory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_auditory_interaction_summary <- summary(latvian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_auditory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_auditory_model <- glm(produces ~ age + norwegian_auditory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_auditory_effect <- ggeffect(norwegian_auditory_model, terms = "norwegian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_auditory_model$coefficients[[3]])
norwegian_auditory_summary <- summary(norwegian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_auditory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_auditory_interaction_model <- glm(produces ~ age * norwegian_auditory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_auditory_interaction_summary <- summary(norwegian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_auditory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_auditory_model <- glm(produces ~ age + persian_auditory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_auditory_effect <- ggeffect(persian_auditory_model, terms = "persian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_auditory_model$coefficients[[3]])
persian_auditory_summary <- summary(persian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_auditory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_auditory_interaction_model <- glm(produces ~ age * persian_auditory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_auditory_interaction_summary <- summary(persian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_auditory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_auditory_model <- glm(produces ~ age + portuguese_auditory_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_auditory_effect <- ggeffect(portuguese_auditory_model, terms = "portuguese_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_auditory_model <- glm(produces ~ age + russian_auditory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_auditory_effect <- ggeffect(russian_auditory_model, terms = "russian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_auditory_model$coefficients[[3]])
russian_auditory_summary <- summary(russian_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_auditory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_auditory_interaction_model <- glm(produces ~ age * russian_auditory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_auditory_interaction_summary <- summary(russian_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_auditory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_auditory_model <- glm(produces ~ age + slovak_auditory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_auditory_effect <- ggeffect(slovak_auditory_model, terms = "slovak_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_auditory_model$coefficients[[3]])
slovak_auditory_summary <- summary(slovak_auditory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_auditory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_auditory_interaction_model <- glm(produces ~ age * slovak_auditory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_auditory_interaction_summary <- summary(slovak_auditory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_auditory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_auditory_model <- glm(produces ~ age + spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_auditory_effect <- ggeffect(spanish_argentinian_auditory_model, terms = "spanish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_auditory_model$coefficients[[3]])
spanish_argentinian_auditory_summary <- summary(spanish_argentinian_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_auditory_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_auditory_interaction_model <- glm(produces ~ age * spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_auditory_interaction_summary <- summary(spanish_argentinian_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_auditory_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_auditory_model <- glm(produces ~ age + spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_auditory_effect <- ggeffect(spanish_chilean_auditory_model, terms = "spanish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_auditory_model$coefficients[[3]])
spanish_chilean_auditory_summary <- summary(spanish_chilean_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_auditory_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_auditory_interaction_model <- glm(produces ~ age * spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_auditory_interaction_summary <- summary(spanish_chilean_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_auditory_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_auditory_model <- glm(produces ~ age + spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_auditory_effect <- ggeffect(spanish_european_auditory_model, terms = "spanish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_auditory_model$coefficients[[3]])
spanish_european_auditory_summary <- summary(spanish_european_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_auditory_rating") %>%
  mutate(language = "spanish_european")
spanish_european_auditory_interaction_model <- glm(produces ~ age * spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_auditory_interaction_summary <- summary(spanish_european_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_auditory_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_auditory_model <- glm(produces ~ age + spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_auditory_effect <- ggeffect(spanish_mexican_auditory_model, terms = "spanish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_auditory_model$coefficients[[3]])
spanish_mexican_auditory_summary <- summary(spanish_mexican_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_auditory_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_auditory_interaction_model <- glm(produces ~ age * spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_auditory_interaction_summary <- summary(spanish_mexican_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_auditory_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_auditory_model <- glm(produces ~ age + spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_auditory_effect <- ggeffect(spanish_peruvian_auditory_model, terms = "spanish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_auditory_model$coefficients[[3]])
spanish_peruvian_auditory_summary <- summary(spanish_peruvian_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_auditory_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_auditory_interaction_model <- glm(produces ~ age * spanish_auditory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_auditory_interaction_summary <- summary(spanish_peruvian_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_auditory_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_auditory_model <- glm(produces ~ age + swedish_auditory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_auditory_effect <- ggeffect(swedish_auditory_model, terms = "swedish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_auditory_model$coefficients[[3]])
swedish_auditory_summary <- summary(swedish_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_auditory_rating") %>%
  mutate(language = "swedish")
swedish_auditory_interaction_model <- glm(produces ~ age * swedish_auditory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_auditory_interaction_model <- glm(produces ~ age * swedish_auditory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_auditory_interaction_summary <- summary(swedish_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_auditory_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_auditory_model <- glm(as.factor(produces) ~ age + arabic_auditory_rating + lexical_category, 
                          data = arabic_instrument_data, family = "binomial")
arabic_auditory_effect <- ggpredict(arabic_auditory_model, terms = "arabic_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_auditory_model$coefficients[[3]])
arabic_auditory_summary <- summary(arabic_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_auditory_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_auditory_interaction_model <- glm(as.factor(produces) ~ age * arabic_auditory_rating + lexical_category, 
                                      data = arabic_instrument_data, family = "binomial")
arabic_auditory_interaction_summary <- summary(arabic_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_auditory_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_auditory_model <- glm(as.factor(produces) ~ age + catalan_auditory_rating + lexical_category, 
                          data = catalan_instrument_data, family = "binomial")
catalan_auditory_effect <- ggpredict(catalan_auditory_model, terms = "catalan_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_auditory_model$coefficients[[3]])
catalan_auditory_summary <- summary(catalan_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_auditory_rating") %>%
  mutate(language = "catalan") 
catalan_auditory_interaction_model <- glm(as.factor(produces) ~ age * catalan_auditory_rating  + lexical_category, 
                                      data = catalan_instrument_data, family = "binomial")
catalan_auditory_interaction_summary <- summary(catalan_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_auditory_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_auditory_model <- glm(as.factor(produces) ~ age + estonian_auditory_rating + lexical_category, 
                              data = estonian_instrument_data, family = "binomial")
estonian_auditory_effect <- ggpredict(estonian_auditory_model, terms = "estonian_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_auditory_model$coefficients[[3]])
estonian_auditory_summary <- summary(estonian_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_auditory_rating") %>%
  mutate(language = "estonian") 
estonian_auditory_interaction_model <- glm(as.factor(produces) ~ age * estonian_auditory_rating  + lexical_category, 
                                          data = estonian_instrument_data, family = "binomial")
estonian_auditory_interaction_summary <- summary(estonian_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_auditory_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_auditory_model <- glm(as.factor(produces) ~ age + japanese_auditory_rating + lexical_category, 
                              data = japanese_instrument_data, family = "binomial")
japanese_auditory_effect <- ggpredict(japanese_auditory_model, terms = "japanese_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_auditory_model$coefficients[[3]])
japanese_auditory_summary <- summary(japanese_auditory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_auditory_rating") %>%
  mutate(language = "japanese") 
japanese_auditory_interaction_model <- glm(as.factor(produces) ~ age * japanese_auditory_rating  + lexical_category, 
                                          data = japanese_instrument_data, family = "binomial")
japanese_auditory_interaction_summary <- summary(japanese_auditory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_auditory_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_auditory_model <- glm(produces ~ age + turkish_auditory_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_auditory_effect <- ggeffect(turkish_auditory_model, terms = "turkish_auditory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_auditory_effects <- bind_rows(asl_auditory_effect,
                             bsl_auditory_effect,
                             chinese_beijing_auditory_effect,
                             chinese_cantonese_auditory_effect,
                             chinese_taiwanese_auditory_effect,
                             croatian_auditory_effect,
                             czech_auditory_effect,
                             english_american_auditory_effect,
                             english_australian_auditory_effect,
                             english_british_auditory_effect,
                             english_irish_auditory_effect,
                             danish_auditory_effect,
                             dutch_auditory_effect,
                             italian_auditory_effect,
                             finnish_auditory_effect,
                             french_european_auditory_effect,
                             french_quebecois_auditory_effect,
                             german_auditory_effect,
                             greek_auditory_effect,
                             hebrew_auditory_effect,
                             hungarian_auditory_effect,
                             irish_auditory_effect,
                             kiswahili_auditory_effect,
                             korean_auditory_effect,
                             latvian_auditory_effect,
                             norwegian_auditory_effect,
                             persian_auditory_effect,
                             russian_auditory_effect,
                             slovak_auditory_effect,
                             spanish_argentinian_auditory_effect,
                             spanish_chilean_auditory_effect,
                             spanish_european_auditory_effect,
                             spanish_mexican_auditory_effect,
                             spanish_peruvian_auditory_effect,
                             swedish_auditory_effect,
                             arabic_auditory_effect,
                             catalan_auditory_effect,
                             estonian_auditory_effect,
                             japanese_auditory_effect
                             # , turkish_auditory_effect
)
write_rds(all_auditory_effects, "models/effects/all_auditory_effects.rds")

all_auditory_effects_plot <- ggplot(all_auditory_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Auditory Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_auditory_effects_plots.png", all_auditory_effects_plot, height = 8, width = 5)


all_auditory_summaries <- bind_rows(asl_auditory_summary,
                               bsl_auditory_summary,
                               chinese_beijing_auditory_summary,
                               chinese_cantonese_auditory_summary,
                               chinese_taiwanese_auditory_summary,
                               croatian_auditory_summary,
                               czech_auditory_summary,
                               english_american_auditory_summary,
                               english_australian_auditory_summary,
                               english_british_auditory_summary,
                               english_irish_auditory_summary,
                               danish_auditory_summary,
                               dutch_auditory_summary,
                               italian_auditory_summary,
                               finnish_auditory_summary,
                               french_european_auditory_summary,
                               french_quebecois_auditory_summary,
                               german_auditory_summary,
                               greek_auditory_summary,
                               hebrew_auditory_summary,
                               hungarian_auditory_summary,
                               irish_auditory_summary,
                               kiswahili_auditory_summary,
                               korean_auditory_summary,
                               latvian_auditory_summary,
                               norwegian_auditory_summary,
                               persian_auditory_summary,
                               russian_auditory_summary,
                               slovak_auditory_summary,
                               spanish_argentinian_auditory_summary,
                               spanish_chilean_auditory_summary,
                               spanish_european_auditory_summary,
                               spanish_mexican_auditory_summary,
                               spanish_peruvian_auditory_summary,
                               swedish_auditory_summary,
                               arabic_auditory_summary,
                               catalan_auditory_summary,
                               estonian_auditory_summary,
                               japanese_auditory_summary
                               # , turkish_auditory_summary
) %>%
  mutate(variable = "auditory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_auditory_summaries, "models/effects/all_auditory_summaries.rds")



all_auditory_interaction_summaries <- bind_rows(asl_auditory_interaction_summary,
                                    bsl_auditory_interaction_summary,
                                    chinese_beijing_auditory_interaction_summary,
                                    chinese_cantonese_auditory_interaction_summary,
                                    chinese_taiwanese_auditory_interaction_summary,
                                    croatian_auditory_interaction_summary,
                                    czech_auditory_interaction_summary,
                                    english_american_auditory_interaction_summary,
                                    english_australian_auditory_interaction_summary,
                                    english_british_auditory_interaction_summary,
                                    english_irish_auditory_interaction_summary,
                                    danish_auditory_interaction_summary,
                                    dutch_auditory_interaction_summary,
                                    italian_auditory_interaction_summary,
                                    finnish_auditory_interaction_summary,
                                    french_european_auditory_interaction_summary,
                                    french_quebecois_auditory_interaction_summary,
                                    german_auditory_interaction_summary,
                                    greek_auditory_interaction_summary,
                                    hebrew_auditory_interaction_summary,
                                    hungarian_auditory_interaction_summary,
                                    irish_auditory_interaction_summary,
                                    kiswahili_auditory_interaction_summary,
                                    korean_auditory_interaction_summary,
                                    latvian_auditory_interaction_summary,
                                    norwegian_auditory_interaction_summary,
                                    persian_auditory_interaction_summary,
                                    russian_auditory_interaction_summary,
                                    slovak_auditory_interaction_summary,
                                    spanish_argentinian_auditory_interaction_summary,
                                    spanish_chilean_auditory_interaction_summary,
                                    spanish_european_auditory_interaction_summary,
                                    spanish_mexican_auditory_interaction_summary,
                                    spanish_peruvian_auditory_interaction_summary,
                                    swedish_auditory_interaction_summary,
                                    arabic_auditory_interaction_summary,
                                    catalan_auditory_interaction_summary,
                                    estonian_auditory_interaction_summary,
                                    japanese_auditory_interaction_summary
                                    # , turkish_auditory_summary
) %>%
  mutate(variable = "age_auditory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_auditory_interaction_summaries, "models/effects/all_auditory_interaction_summaries.rds")
