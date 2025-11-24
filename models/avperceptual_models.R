library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# avperceptual
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_avperceptual_model <- glm(as.factor(produces) ~ age + asl_avperceptual_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                               data = asl_instrument_data, family = "binomial")
asl_avperceptual_effect <- ggpredict(asl_avperceptual_model, terms = "asl_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_avperceptual_model$coefficients[[3]])
asl_avperceptual_summary <- summary(asl_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_avperceptual_rating") %>%
  mutate(language = "asl") 
asl_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * asl_avperceptual_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                           data = asl_instrument_data, family = "binomial")
asl_avperceptual_interaction_summary <- summary(asl_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_avperceptual_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_avperceptual_model <- glm(as.factor(produces) ~ age + bsl_avperceptual_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_avperceptual_effect <- ggpredict(bsl_avperceptual_model, terms = "bsl_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_avperceptual_model$coefficients[[3]]) 
bsl_avperceptual_summary <- summary(bsl_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_avperceptual_rating") %>%
  mutate(language = "bsl")
bsl_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * bsl_avperceptual_rating + lexical_category, 
                                           data = bsl_instrument_data, family = "binomial")
bsl_avperceptual_interaction_summary <- summary(bsl_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_avperceptual_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_avperceptual_model <- glm(produces ~ age + chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_avperceptual_effect <- ggeffect(chinese_beijing_avperceptual_model, terms = "chinese_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_avperceptual_model$coefficients[[3]])
chinese_beijing_avperceptual_summary <- summary(chinese_beijing_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_avperceptual_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_avperceptual_interaction_model <- glm(produces ~ age * chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_avperceptual_interaction_summary <- summary(chinese_beijing_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_avperceptual_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_avperceptual_model <- glm(produces ~ age + chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_avperceptual_effect <- ggeffect(chinese_cantonese_avperceptual_model, terms = "chinese_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_avperceptual_model$coefficients[[3]])
chinese_cantonese_avperceptual_summary <- summary(chinese_cantonese_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_avperceptual_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_avperceptual_interaction_model <- glm(produces ~ age * chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_avperceptual_interaction_summary <- summary(chinese_cantonese_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_avperceptual_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_avperceptual_model <- glm(produces ~ age + chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_avperceptual_effect <- ggeffect(chinese_taiwanese_avperceptual_model, terms = "chinese_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_avperceptual_model$coefficients[[3]])
chinese_taiwanese_avperceptual_summary <- summary(chinese_taiwanese_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_avperceptual_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_avperceptual_interaction_model <- glm(produces ~ age * chinese_avperceptual_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_avperceptual_interaction_summary <- summary(chinese_taiwanese_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_avperceptual_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_avperceptual_model <- glm(as.factor(produces) ~ age + croatian_avperceptual_rating + croatian_freq_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_avperceptual_effect <- ggpredict(croatian_avperceptual_model, terms = "croatian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_avperceptual_model$coefficients[[3]])
croatian_avperceptual_summary <- summary(croatian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_avperceptual_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * croatian_avperceptual_rating + croatian_freq_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_avperceptual_interaction_summary <- summary(croatian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_avperceptual_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_avperceptual_model <- glm(as.factor(produces) ~ age + czech_avperceptual_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_avperceptual_effect <- ggpredict(czech_avperceptual_model, terms = "czech_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_avperceptual_model$coefficients[[3]])
czech_avperceptual_summary <- summary(czech_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_avperceptual_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * czech_avperceptual_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_avperceptual_interaction_summary <- summary(czech_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_avperceptual_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_avperceptual_model <- glm(as.factor(produces) ~ age + danish_avperceptual_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_avperceptual_effect <- ggpredict(danish_avperceptual_model, terms = "danish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_avperceptual_model$coefficients[[3]])
danish_avperceptual_summary <- summary(danish_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_avperceptual_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * danish_avperceptual_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_avperceptual_interaction_summary <- summary(danish_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_avperceptual_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_avperceptual_model <- glm(as.factor(produces) ~ age + dutch_avperceptual_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_avperceptual_effect <- ggpredict(dutch_avperceptual_model, terms = "dutch_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_avperceptual_model$coefficients[[3]])  
dutch_avperceptual_summary <- summary(dutch_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_avperceptual_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * dutch_avperceptual_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_avperceptual_interaction_summary <- summary(dutch_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_avperceptual_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_avperceptual_model <- glm(produces ~ age + english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_avperceptual_effect <- ggeffect(english_american_avperceptual_model, terms = "english_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_avperceptual_model$coefficients[[3]])
english_american_avperceptual_summary <- summary(english_american_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_avperceptual_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_avperceptual_interaction_model <- glm(produces ~ age * english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_avperceptual_interaction_summary <- summary(english_american_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_avperceptual_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_avperceptual_model <- glm(produces ~ age + english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_avperceptual_effect <- ggeffect(english_australian_avperceptual_model, terms = "english_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_avperceptual_model$coefficients[[3]])
english_australian_avperceptual_summary <- summary(english_australian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_avperceptual_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_avperceptual_interaction_model <- glm(produces ~ age * english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_avperceptual_interaction_summary <- summary(english_australian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_avperceptual_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_avperceptual_model <- glm(produces ~ age + english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_avperceptual_effect <- ggeffect(english_british_avperceptual_model, terms = "english_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_avperceptual_model$coefficients[[3]])
english_british_avperceptual_summary <- summary(english_british_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_avperceptual_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_avperceptual_interaction_model <- glm(produces ~ age * english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_avperceptual_interaction_summary <- summary(english_british_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_avperceptual_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_avperceptual_model <- glm(produces ~ age + english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_avperceptual_effect <- ggeffect(english_irish_avperceptual_model, terms = "english_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_avperceptual_model$coefficients[[3]])
english_irish_avperceptual_summary <- summary(english_irish_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_avperceptual_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_avperceptual_interaction_model <- glm(produces ~ age * english_avperceptual_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_avperceptual_interaction_summary <- summary(english_irish_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_avperceptual_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_avperceptual_model <- glm(as.factor(produces) ~ age + finnish_avperceptual_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_avperceptual_effect <- ggpredict(finnish_avperceptual_model, terms = "finnish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_avperceptual_model$coefficients[[3]]) 
finnish_avperceptual_summary <- summary(finnish_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_avperceptual_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * finnish_avperceptual_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_avperceptual_interaction_summary <- summary(finnish_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_avperceptual_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_avperceptual_model <- glm(as.factor(produces) ~ age + french_avperceptual_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_avperceptual_effect <- ggpredict(french_european_avperceptual_model, terms = "french_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_avperceptual_model$coefficients[[3]]) 
french_european_avperceptual_summary <- summary(french_european_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_avperceptual_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * french_avperceptual_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_avperceptual_interaction_summary <- summary(french_european_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_avperceptual_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_avperceptual_model <- glm(as.factor(produces) ~ age + french_avperceptual_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_avperceptual_effect <- ggpredict(french_quebecois_avperceptual_model, terms = "french_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_avperceptual_model$coefficients[[3]]) 
french_quebecois_avperceptual_summary <- summary(french_quebecois_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_avperceptual_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * french_avperceptual_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_avperceptual_interaction_summary <- summary(french_quebecois_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_avperceptual_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_avperceptual_model <- glm(as.factor(produces) ~ age + german_avperceptual_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_avperceptual_effect <- ggpredict(german_avperceptual_model, terms = "german_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_avperceptual_model$coefficients[[3]]) 
german_avperceptual_summary <- summary(german_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_avperceptual_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * german_avperceptual_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_avperceptual_interaction_summary <- summary(german_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_avperceptual_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_avperceptual_model <- glm(as.factor(produces) ~ age + greek_avperceptual_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_avperceptual_effect <- ggpredict(greek_avperceptual_model, terms = "greek_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_avperceptual_model$coefficients[[3]]) 
greek_avperceptual_summary <- summary(greek_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_avperceptual_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * greek_avperceptual_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_avperceptual_interaction_summary <- summary(greek_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_avperceptual_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_avperceptual_model <- glm(as.factor(produces) ~ age + hebrew_avperceptual_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_avperceptual_effect <- ggpredict(hebrew_avperceptual_model, terms = "hebrew_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_avperceptual_model$coefficients[[3]]) 
hebrew_avperceptual_summary <- summary(hebrew_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_avperceptual_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * hebrew_avperceptual_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_avperceptual_interaction_summary <- summary(hebrew_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_avperceptual_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_avperceptual_model <- glm(as.factor(produces) ~ age + hungarian_avperceptual_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_avperceptual_effect <- ggpredict(hungarian_avperceptual_model, terms = "hungarian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_avperceptual_model$coefficients[[3]])
hungarian_avperceptual_summary <- summary(hungarian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_avperceptual_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * hungarian_avperceptual_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_avperceptual_interaction_summary <- summary(hungarian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_avperceptual_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_avperceptual_model <- glm(as.factor(produces) ~ age + irish_avperceptual_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_avperceptual_effect <- ggpredict(irish_avperceptual_model, terms = "irish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_avperceptual_model$coefficients[[3]])
irish_avperceptual_summary <- summary(irish_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_avperceptual_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * irish_avperceptual_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_avperceptual_interaction_summary <- summary(irish_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_avperceptual_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_avperceptual_model <- glm(produces ~ age + italian_avperceptual_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_avperceptual_effect <- ggeffect(italian_avperceptual_model, terms = "italian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_avperceptual_model$coefficients[[3]])
italian_avperceptual_summary <- summary(italian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_avperceptual_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_avperceptual_interaction_model <- glm(produces ~ age * italian_avperceptual_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_avperceptual_interaction_summary <- summary(italian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_avperceptual_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

kigiriama_instrument_data <- read_rds("norms/kigiriama/kigiriama_instrument_data.rds")
kigiriama_avperceptual_model <- glm(produces ~ age + kigiriama_avperceptual_rating + lexical_category + word_length, data = kigiriama_instrument_data, family = "binomial")
kigiriama_avperceptual_effect <- ggeffect(kigiriama_avperceptual_model, terms = "kigiriama_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "kigiriama",
         variable_coefficient = kigiriama_avperceptual_model$coefficients[[3]])
kigiriama_avperceptual_effect <- ggeffect(kigiriama_avperceptual_model, terms = "kigiriama_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "kigiriama",
         variable_coefficient = kigiriama_avperceptual_model$coefficients[[3]])
kigiriama_avperceptual_summary <- summary(kigiriama_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kigiriama_avperceptual_rating") %>%
  mutate(language = "kigiriama",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kigiriama_avperceptual_interaction_model <- glm(produces ~ age * kigiriama_avperceptual_rating + lexical_category + word_length, data = kigiriama_instrument_data, family = "binomial")
kigiriama_avperceptual_interaction_summary <- summary(kigiriama_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kigiriama_avperceptual_rating") %>%
  mutate(language = "kigiriama",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_avperceptual_model <- glm(produces ~ age + kiswahili_avperceptual_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_avperceptual_effect <- ggeffect(kiswahili_avperceptual_model, terms = "kiswahili_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_avperceptual_model$coefficients[[3]])
kiswahili_avperceptual_summary <- summary(kiswahili_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_avperceptual_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_avperceptual_interaction_model <- glm(produces ~ age * kiswahili_avperceptual_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_avperceptual_interaction_summary <- summary(kiswahili_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_avperceptual_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_avperceptual_model <- glm(produces ~ age + korean_avperceptual_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_avperceptual_effect <- ggeffect(korean_avperceptual_model, terms = "korean_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_avperceptual_model$coefficients[[3]])
korean_avperceptual_summary <- summary(korean_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_avperceptual_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_avperceptual_interaction_model <- glm(produces ~ age * korean_avperceptual_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_avperceptual_interaction_summary <- summary(korean_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_avperceptual_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_avperceptual_model <- glm(produces ~ age + latvian_avperceptual_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_avperceptual_effect <- ggeffect(latvian_avperceptual_model, terms = "latvian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_avperceptual_model$coefficients[[3]])
latvian_avperceptual_summary <- summary(latvian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_avperceptual_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_avperceptual_interaction_model <- glm(produces ~ age * latvian_avperceptual_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_avperceptual_interaction_summary <- summary(latvian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_avperceptual_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_avperceptual_model <- glm(produces ~ age + norwegian_avperceptual_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_avperceptual_effect <- ggeffect(norwegian_avperceptual_model, terms = "norwegian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_avperceptual_model$coefficients[[3]])
norwegian_avperceptual_summary <- summary(norwegian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_avperceptual_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_avperceptual_interaction_model <- glm(produces ~ age * norwegian_avperceptual_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_avperceptual_interaction_summary <- summary(norwegian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_avperceptual_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_avperceptual_model <- glm(produces ~ age + persian_avperceptual_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_avperceptual_effect <- ggeffect(persian_avperceptual_model, terms = "persian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_avperceptual_model$coefficients[[3]])
persian_avperceptual_summary <- summary(persian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_avperceptual_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_avperceptual_interaction_model <- glm(produces ~ age * persian_avperceptual_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_avperceptual_interaction_summary <- summary(persian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_avperceptual_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
portuguese_avperceptual_model <- glm(produces ~ age + portuguese_avperceptual_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
portuguese_avperceptual_effect <- ggeffect(portuguese_avperceptual_model, terms = "portuguese_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = portuguese_avperceptual_model$coefficients[[3]])
portuguese_avperceptual_summary <- summary(portuguese_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "portuguese_avperceptual_rating") %>%
  mutate(language = "portuguese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
portuguese_avperceptual_interaction_model <- glm(produces ~ age * portuguese_avperceptual_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
portuguese_avperceptual_interaction_summary <- summary(portuguese_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:portuguese_avperceptual_rating") %>%
  mutate(language = "portuguese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_avperceptual_model <- glm(produces ~ age + russian_avperceptual_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_avperceptual_effect <- ggeffect(russian_avperceptual_model, terms = "russian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_avperceptual_model$coefficients[[3]])
russian_avperceptual_summary <- summary(russian_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_avperceptual_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_avperceptual_interaction_model <- glm(produces ~ age * russian_avperceptual_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_avperceptual_interaction_summary <- summary(russian_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_avperceptual_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_avperceptual_model <- glm(produces ~ age + slovak_avperceptual_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_avperceptual_effect <- ggeffect(slovak_avperceptual_model, terms = "slovak_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_avperceptual_model$coefficients[[3]])
slovak_avperceptual_summary <- summary(slovak_avperceptual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_avperceptual_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_avperceptual_interaction_model <- glm(produces ~ age * slovak_avperceptual_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_avperceptual_interaction_summary <- summary(slovak_avperceptual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_avperceptual_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_avperceptual_model <- glm(produces ~ age + spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_avperceptual_effect <- ggeffect(spanish_argentinian_avperceptual_model, terms = "spanish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_avperceptual_model$coefficients[[3]])
spanish_argentinian_avperceptual_summary <- summary(spanish_argentinian_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_avperceptual_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_avperceptual_interaction_model <- glm(produces ~ age * spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_avperceptual_interaction_summary <- summary(spanish_argentinian_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_avperceptual_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_avperceptual_model <- glm(produces ~ age + spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_avperceptual_effect <- ggeffect(spanish_chilean_avperceptual_model, terms = "spanish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_avperceptual_model$coefficients[[3]])
spanish_chilean_avperceptual_summary <- summary(spanish_chilean_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_avperceptual_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_avperceptual_interaction_model <- glm(produces ~ age * spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_avperceptual_interaction_summary <- summary(spanish_chilean_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_avperceptual_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_avperceptual_model <- glm(produces ~ age + spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_avperceptual_effect <- ggeffect(spanish_european_avperceptual_model, terms = "spanish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_avperceptual_model$coefficients[[3]])
spanish_european_avperceptual_summary <- summary(spanish_european_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_avperceptual_rating") %>%
  mutate(language = "spanish_european")
spanish_european_avperceptual_interaction_model <- glm(produces ~ age * spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_avperceptual_interaction_summary <- summary(spanish_european_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_avperceptual_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_avperceptual_model <- glm(produces ~ age + spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_avperceptual_effect <- ggeffect(spanish_mexican_avperceptual_model, terms = "spanish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_avperceptual_model$coefficients[[3]])
spanish_mexican_avperceptual_summary <- summary(spanish_mexican_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_avperceptual_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_avperceptual_interaction_model <- glm(produces ~ age * spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_avperceptual_interaction_summary <- summary(spanish_mexican_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_avperceptual_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_avperceptual_model <- glm(produces ~ age + spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_avperceptual_effect <- ggeffect(spanish_peruvian_avperceptual_model, terms = "spanish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_avperceptual_model$coefficients[[3]])
spanish_peruvian_avperceptual_summary <- summary(spanish_peruvian_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_avperceptual_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_avperceptual_interaction_model <- glm(produces ~ age * spanish_avperceptual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_avperceptual_interaction_summary <- summary(spanish_peruvian_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_avperceptual_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_avperceptual_model <- glm(produces ~ age + swedish_avperceptual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_avperceptual_effect <- ggeffect(swedish_avperceptual_model, terms = "swedish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_avperceptual_model$coefficients[[3]])
swedish_avperceptual_summary <- summary(swedish_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_avperceptual_rating") %>%
  mutate(language = "swedish")
swedish_avperceptual_interaction_model <- glm(produces ~ age * swedish_avperceptual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_avperceptual_interaction_model <- glm(produces ~ age * swedish_avperceptual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_avperceptual_interaction_summary <- summary(swedish_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_avperceptual_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_avperceptual_model <- glm(as.factor(produces) ~ age + arabic_avperceptual_rating + arabic_freq_rating + lexical_category, 
                                  data = arabic_instrument_data, family = "binomial")
arabic_avperceptual_effect <- ggpredict(arabic_avperceptual_model, terms = "arabic_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_avperceptual_model$coefficients[[3]])
arabic_avperceptual_summary <- summary(arabic_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_avperceptual_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * arabic_avperceptual_rating + arabic_freq_rating + lexical_category, 
                                              data = arabic_instrument_data, family = "binomial")
arabic_avperceptual_interaction_summary <- summary(arabic_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_avperceptual_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_avperceptual_model <- glm(as.factor(produces) ~ age + catalan_avperceptual_rating + catalan_freq_rating + lexical_category, 
                                   data = catalan_instrument_data, family = "binomial")
catalan_avperceptual_effect <- ggpredict(catalan_avperceptual_model, terms = "catalan_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_avperceptual_model$coefficients[[3]])
catalan_avperceptual_summary <- summary(catalan_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_avperceptual_rating") %>%
  mutate(language = "catalan") 
catalan_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * catalan_avperceptual_rating  + catalan_freq_rating + lexical_category, 
                                               data = catalan_instrument_data, family = "binomial")
catalan_avperceptual_interaction_summary <- summary(catalan_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_avperceptual_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_avperceptual_model <- glm(as.factor(produces) ~ age + estonian_avperceptual_rating + estonian_freq_rating + lexical_category, 
                                    data = estonian_instrument_data, family = "binomial")
estonian_avperceptual_effect <- ggpredict(estonian_avperceptual_model, terms = "estonian_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_avperceptual_model$coefficients[[3]])
estonian_avperceptual_summary <- summary(estonian_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_avperceptual_rating") %>%
  mutate(language = "estonian") 
estonian_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * estonian_avperceptual_rating + estonian_freq_rating  + lexical_category, 
                                                data = estonian_instrument_data, family = "binomial")
estonian_avperceptual_interaction_summary <- summary(estonian_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_avperceptual_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_avperceptual_model <- glm(as.factor(produces) ~ age + japanese_avperceptual_rating + japanese_freq_rating + lexical_category, 
                                    data = japanese_instrument_data, family = "binomial")
japanese_avperceptual_effect <- ggpredict(japanese_avperceptual_model, terms = "japanese_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_avperceptual_model$coefficients[[3]])
japanese_avperceptual_summary <- summary(japanese_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_avperceptual_rating") %>%
  mutate(language = "japanese") 
japanese_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * japanese_avperceptual_rating  + japanese_freq_rating + lexical_category, 
                                                data = japanese_instrument_data, family = "binomial")
japanese_avperceptual_interaction_summary <- summary(japanese_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_avperceptual_rating") %>%
  mutate(language = "japanese")

turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
turkish_avperceptual_model <- glm(as.factor(produces) ~ age + turkish_avperceptual_rating + turkish_freq_rating + lexical_category, 
                                   data = turkish_instrument_data, family = "binomial")
turkish_avperceptual_effect <- ggpredict(turkish_avperceptual_model, terms = "turkish_avperceptual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = turkish_avperceptual_model$coefficients[[3]])
turkish_avperceptual_summary <- summary(turkish_avperceptual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "turkish_avperceptual_rating") %>%
  mutate(language = "turkish") 
turkish_avperceptual_interaction_model <- glm(as.factor(produces) ~ age * turkish_avperceptual_rating  + turkish_freq_rating + lexical_category, 
                                               data = turkish_instrument_data, family = "binomial")
turkish_avperceptual_interaction_summary <- summary(turkish_avperceptual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:turkish_avperceptual_rating") %>%
  mutate(language = "turkish")

all_avperceptual_effects <- bind_rows(asl_avperceptual_effect,
                                       bsl_avperceptual_effect,
                                       chinese_beijing_avperceptual_effect,
                                       chinese_cantonese_avperceptual_effect,
                                       chinese_taiwanese_avperceptual_effect,
                                       croatian_avperceptual_effect,
                                       czech_avperceptual_effect,
                                       english_american_avperceptual_effect,
                                       english_australian_avperceptual_effect,
                                       english_british_avperceptual_effect,
                                       english_irish_avperceptual_effect,
                                       danish_avperceptual_effect,
                                       dutch_avperceptual_effect,
                                       italian_avperceptual_effect,
                                       finnish_avperceptual_effect,
                                       french_european_avperceptual_effect,
                                       french_quebecois_avperceptual_effect,
                                       german_avperceptual_effect,
                                       greek_avperceptual_effect,
                                       hebrew_avperceptual_effect,
                                       hungarian_avperceptual_effect,
                                       irish_avperceptual_effect,
                                       kigiriama_avperceptual_effect,
                                       kiswahili_avperceptual_effect,
                                       korean_avperceptual_effect,
                                       latvian_avperceptual_effect,
                                       norwegian_avperceptual_effect,
                                       persian_avperceptual_effect,
                                       portuguese_avperceptual_effect,
                                       russian_avperceptual_effect,
                                       slovak_avperceptual_effect,
                                       spanish_argentinian_avperceptual_effect,
                                       spanish_chilean_avperceptual_effect,
                                       spanish_european_avperceptual_effect,
                                       spanish_mexican_avperceptual_effect,
                                       spanish_peruvian_avperceptual_effect,
                                       swedish_avperceptual_effect,
                                       arabic_avperceptual_effect,
                                       catalan_avperceptual_effect,
                                       estonian_avperceptual_effect,
                                       japanese_avperceptual_effect, 
                                       turkish_avperceptual_effect
)
write_rds(all_avperceptual_effects, "models/effects/all_avperceptual_effects.rds")

all_avperceptual_effects_plot <- ggplot(all_avperceptual_effects)  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Body Object Interaction Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_avperceptual_effects_plots.png", all_avperceptual_effects_plot, height = 8, width = 5)


all_avperceptual_summaries <- bind_rows(asl_avperceptual_summary,
                                         bsl_avperceptual_summary,
                                         chinese_beijing_avperceptual_summary,
                                         chinese_cantonese_avperceptual_summary,
                                         chinese_taiwanese_avperceptual_summary,
                                         croatian_avperceptual_summary,
                                         czech_avperceptual_summary,
                                         english_american_avperceptual_summary,
                                         english_australian_avperceptual_summary,
                                         english_british_avperceptual_summary,
                                         english_irish_avperceptual_summary,
                                         danish_avperceptual_summary,
                                         dutch_avperceptual_summary,
                                         italian_avperceptual_summary,
                                         finnish_avperceptual_summary,
                                         french_european_avperceptual_summary,
                                         french_quebecois_avperceptual_summary,
                                         german_avperceptual_summary,
                                         greek_avperceptual_summary,
                                         hebrew_avperceptual_summary,
                                         hungarian_avperceptual_summary,
                                         irish_avperceptual_summary,
                                         kigiriama_avperceptual_summary,
                                         kiswahili_avperceptual_summary,
                                         korean_avperceptual_summary,
                                         latvian_avperceptual_summary,
                                         norwegian_avperceptual_summary,
                                         persian_avperceptual_summary,
                                         portuguese_avperceptual_summary,
                                         russian_avperceptual_summary,
                                         slovak_avperceptual_summary,
                                         spanish_argentinian_avperceptual_summary,
                                         spanish_chilean_avperceptual_summary,
                                         spanish_european_avperceptual_summary,
                                         spanish_mexican_avperceptual_summary,
                                         spanish_peruvian_avperceptual_summary,
                                         swedish_avperceptual_summary,
                                         arabic_avperceptual_summary,
                                         catalan_avperceptual_summary,
                                         estonian_avperceptual_summary,
                                         japanese_avperceptual_summary, 
                                         turkish_avperceptual_summary
) %>%
  mutate(variable = "Average Perceptual",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_avperceptual_summaries, "models/effects/all_avperceptual_summaries.rds")



all_avperceptual_interaction_summaries <- bind_rows(asl_avperceptual_interaction_summary,
                                                     bsl_avperceptual_interaction_summary,
                                                     chinese_beijing_avperceptual_interaction_summary,
                                                     chinese_cantonese_avperceptual_interaction_summary,
                                                     chinese_taiwanese_avperceptual_interaction_summary,
                                                     croatian_avperceptual_interaction_summary,
                                                     czech_avperceptual_interaction_summary,
                                                     english_american_avperceptual_interaction_summary,
                                                     english_australian_avperceptual_interaction_summary,
                                                     english_british_avperceptual_interaction_summary,
                                                     english_irish_avperceptual_interaction_summary,
                                                     danish_avperceptual_interaction_summary,
                                                     dutch_avperceptual_interaction_summary,
                                                     italian_avperceptual_interaction_summary,
                                                     finnish_avperceptual_interaction_summary,
                                                     french_european_avperceptual_interaction_summary,
                                                     french_quebecois_avperceptual_interaction_summary,
                                                     german_avperceptual_interaction_summary,
                                                     greek_avperceptual_interaction_summary,
                                                     hebrew_avperceptual_interaction_summary,
                                                     hungarian_avperceptual_interaction_summary,
                                                     irish_avperceptual_interaction_summary,
                                                     kigiriama_avperceptual_interaction_summary,
                                                     kiswahili_avperceptual_interaction_summary,
                                                     korean_avperceptual_interaction_summary,
                                                     latvian_avperceptual_interaction_summary,
                                                     norwegian_avperceptual_interaction_summary,
                                                     persian_avperceptual_interaction_summary,
                                                     portuguese_avperceptual_interaction_summary,
                                                     russian_avperceptual_interaction_summary,
                                                     slovak_avperceptual_interaction_summary,
                                                     spanish_argentinian_avperceptual_interaction_summary,
                                                     spanish_chilean_avperceptual_interaction_summary,
                                                     spanish_european_avperceptual_interaction_summary,
                                                     spanish_mexican_avperceptual_interaction_summary,
                                                     spanish_peruvian_avperceptual_interaction_summary,
                                                     swedish_avperceptual_interaction_summary,
                                                     arabic_avperceptual_interaction_summary,
                                                     catalan_avperceptual_interaction_summary,
                                                     estonian_avperceptual_interaction_summary,
                                                     japanese_avperceptual_interaction_summary, 
                                                     turkish_avperceptual_summary
) %>%
  mutate(variable = "Average Perceptual",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_avperceptual_interaction_summaries, "models/effects/all_avperceptual_interaction_summaries.rds")
