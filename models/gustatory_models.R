library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# gustatory
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_gustatory_model <- glm(as.factor(produces) ~ age + asl_gustatory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                  data = asl_instrument_data, family = "binomial")
asl_gustatory_effect <- ggpredict(asl_gustatory_model, terms = "asl_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_gustatory_model$coefficients[[3]])
asl_gustatory_summary <- summary(asl_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_gustatory_rating") %>%
  mutate(language = "asl") 
asl_gustatory_interaction_model <- glm(as.factor(produces) ~ age * asl_gustatory_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                              data = asl_instrument_data, family = "binomial")
asl_gustatory_interaction_summary <- summary(asl_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_gustatory_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_gustatory_model <- glm(as.factor(produces) ~ age + bsl_gustatory_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_gustatory_effect <- ggpredict(bsl_gustatory_model, terms = "bsl_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_gustatory_model$coefficients[[3]]) 
bsl_gustatory_summary <- summary(bsl_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_gustatory_rating") %>%
  mutate(language = "bsl")
bsl_gustatory_interaction_model <- glm(as.factor(produces) ~ age * bsl_gustatory_rating + lexical_category, 
                                              data = bsl_instrument_data, family = "binomial")
bsl_gustatory_interaction_summary <- summary(bsl_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_gustatory_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_gustatory_model <- glm(produces ~ age + chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_gustatory_effect <- ggeffect(chinese_beijing_gustatory_model, terms = "chinese_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_gustatory_model$coefficients[[3]])
chinese_beijing_gustatory_summary <- summary(chinese_beijing_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_gustatory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_gustatory_interaction_model <- glm(produces ~ age * chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_gustatory_interaction_summary <- summary(chinese_beijing_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_gustatory_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_gustatory_model <- glm(produces ~ age + chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_gustatory_effect <- ggeffect(chinese_cantonese_gustatory_model, terms = "chinese_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_gustatory_model$coefficients[[3]])
chinese_cantonese_gustatory_summary <- summary(chinese_cantonese_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_gustatory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_gustatory_interaction_model <- glm(produces ~ age * chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_gustatory_interaction_summary <- summary(chinese_cantonese_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_gustatory_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_gustatory_model <- glm(produces ~ age + chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_gustatory_effect <- ggeffect(chinese_taiwanese_gustatory_model, terms = "chinese_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_gustatory_model$coefficients[[3]])
chinese_taiwanese_gustatory_summary <- summary(chinese_taiwanese_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_gustatory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_gustatory_interaction_model <- glm(produces ~ age * chinese_gustatory_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_gustatory_interaction_summary <- summary(chinese_taiwanese_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_gustatory_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_gustatory_model <- glm(as.factor(produces) ~ age + croatian_gustatory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_gustatory_effect <- ggpredict(croatian_gustatory_model, terms = "croatian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_gustatory_model$coefficients[[3]])
croatian_gustatory_summary <- summary(croatian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_gustatory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_gustatory_interaction_model <- glm(as.factor(produces) ~ age * croatian_gustatory_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_gustatory_interaction_summary <- summary(croatian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_gustatory_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_gustatory_model <- glm(as.factor(produces) ~ age + czech_gustatory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_gustatory_effect <- ggpredict(czech_gustatory_model, terms = "czech_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_gustatory_model$coefficients[[3]])
czech_gustatory_summary <- summary(czech_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_gustatory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_gustatory_interaction_model <- glm(as.factor(produces) ~ age * czech_gustatory_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_gustatory_interaction_summary <- summary(czech_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_gustatory_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_gustatory_model <- glm(as.factor(produces) ~ age + danish_gustatory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_gustatory_effect <- ggpredict(danish_gustatory_model, terms = "danish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_gustatory_model$coefficients[[3]])
danish_gustatory_summary <- summary(danish_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_gustatory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_gustatory_interaction_model <- glm(as.factor(produces) ~ age * danish_gustatory_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_gustatory_interaction_summary <- summary(danish_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_gustatory_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_gustatory_model <- glm(as.factor(produces) ~ age + dutch_gustatory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_gustatory_effect <- ggpredict(dutch_gustatory_model, terms = "dutch_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_gustatory_model$coefficients[[3]])  
dutch_gustatory_summary <- summary(dutch_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_gustatory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_gustatory_interaction_model <- glm(as.factor(produces) ~ age * dutch_gustatory_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_gustatory_interaction_summary <- summary(dutch_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_gustatory_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_gustatory_model <- glm(produces ~ age + english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_gustatory_effect <- ggeffect(english_american_gustatory_model, terms = "english_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_gustatory_model$coefficients[[3]])
english_american_gustatory_summary <- summary(english_american_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_gustatory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_gustatory_interaction_model <- glm(produces ~ age * english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_gustatory_interaction_summary <- summary(english_american_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_gustatory_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_gustatory_model <- glm(produces ~ age + english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_gustatory_effect <- ggeffect(english_australian_gustatory_model, terms = "english_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_gustatory_model$coefficients[[3]])
english_australian_gustatory_summary <- summary(english_australian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_gustatory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_gustatory_interaction_model <- glm(produces ~ age * english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_gustatory_interaction_summary <- summary(english_australian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_gustatory_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_gustatory_model <- glm(produces ~ age + english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_gustatory_effect <- ggeffect(english_british_gustatory_model, terms = "english_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_gustatory_model$coefficients[[3]])
english_british_gustatory_summary <- summary(english_british_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_gustatory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_gustatory_interaction_model <- glm(produces ~ age * english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_gustatory_interaction_summary <- summary(english_british_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_gustatory_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_gustatory_model <- glm(produces ~ age + english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_gustatory_effect <- ggeffect(english_irish_gustatory_model, terms = "english_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_gustatory_model$coefficients[[3]])
english_irish_gustatory_summary <- summary(english_irish_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_gustatory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_gustatory_interaction_model <- glm(produces ~ age * english_gustatory_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_gustatory_interaction_summary <- summary(english_irish_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_gustatory_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_gustatory_model <- glm(as.factor(produces) ~ age + finnish_gustatory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_gustatory_effect <- ggpredict(finnish_gustatory_model, terms = "finnish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_gustatory_model$coefficients[[3]]) 
finnish_gustatory_summary <- summary(finnish_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_gustatory_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_gustatory_interaction_model <- glm(as.factor(produces) ~ age * finnish_gustatory_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_gustatory_interaction_summary <- summary(finnish_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_gustatory_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_gustatory_model <- glm(as.factor(produces) ~ age + french_gustatory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_gustatory_effect <- ggpredict(french_european_gustatory_model, terms = "french_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_gustatory_model$coefficients[[3]]) 
french_european_gustatory_summary <- summary(french_european_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_gustatory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_gustatory_interaction_model <- glm(as.factor(produces) ~ age * french_gustatory_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_gustatory_interaction_summary <- summary(french_european_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_gustatory_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_gustatory_model <- glm(as.factor(produces) ~ age + french_gustatory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_gustatory_effect <- ggpredict(french_quebecois_gustatory_model, terms = "french_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_gustatory_model$coefficients[[3]]) 
french_quebecois_gustatory_summary <- summary(french_quebecois_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_gustatory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_gustatory_interaction_model <- glm(as.factor(produces) ~ age * french_gustatory_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_gustatory_interaction_summary <- summary(french_quebecois_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_gustatory_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_gustatory_model <- glm(as.factor(produces) ~ age + german_gustatory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_gustatory_effect <- ggpredict(german_gustatory_model, terms = "german_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_gustatory_model$coefficients[[3]]) 
german_gustatory_summary <- summary(german_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_gustatory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_gustatory_interaction_model <- glm(as.factor(produces) ~ age * german_gustatory_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_gustatory_interaction_summary <- summary(german_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_gustatory_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_gustatory_model <- glm(as.factor(produces) ~ age + greek_gustatory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_gustatory_effect <- ggpredict(greek_gustatory_model, terms = "greek_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_gustatory_model$coefficients[[3]]) 
greek_gustatory_summary <- summary(greek_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_gustatory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_gustatory_interaction_model <- glm(as.factor(produces) ~ age * greek_gustatory_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_gustatory_interaction_summary <- summary(greek_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_gustatory_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_gustatory_model <- glm(as.factor(produces) ~ age + hebrew_gustatory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_gustatory_effect <- ggpredict(hebrew_gustatory_model, terms = "hebrew_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_gustatory_model$coefficients[[3]]) 
hebrew_gustatory_summary <- summary(hebrew_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_gustatory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_gustatory_interaction_model <- glm(as.factor(produces) ~ age * hebrew_gustatory_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_gustatory_interaction_summary <- summary(hebrew_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_gustatory_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_gustatory_model <- glm(as.factor(produces) ~ age + hungarian_gustatory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_gustatory_effect <- ggpredict(hungarian_gustatory_model, terms = "hungarian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_gustatory_model$coefficients[[3]])
hungarian_gustatory_summary <- summary(hungarian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_gustatory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_gustatory_interaction_model <- glm(as.factor(produces) ~ age * hungarian_gustatory_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_gustatory_interaction_summary <- summary(hungarian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_gustatory_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_gustatory_model <- glm(as.factor(produces) ~ age + irish_gustatory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_gustatory_effect <- ggpredict(irish_gustatory_model, terms = "irish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_gustatory_model$coefficients[[3]])
irish_gustatory_summary <- summary(irish_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_gustatory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_gustatory_interaction_model <- glm(as.factor(produces) ~ age * irish_gustatory_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_gustatory_interaction_summary <- summary(irish_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_gustatory_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_gustatory_model <- glm(produces ~ age + italian_gustatory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_gustatory_effect <- ggeffect(italian_gustatory_model, terms = "italian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_gustatory_model$coefficients[[3]])
italian_gustatory_summary <- summary(italian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_gustatory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_gustatory_interaction_model <- glm(produces ~ age * italian_gustatory_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_gustatory_interaction_summary <- summary(italian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_gustatory_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_gustatory_model <- glm(produces ~ age + kigiriama_gustatory_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_gustatory_effect <- ggeffect(kigiriama_gustatory_model, terms = "kigiriama_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_gustatory_model <- glm(produces ~ age + kiswahili_gustatory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_gustatory_effect <- ggeffect(kiswahili_gustatory_model, terms = "kiswahili_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_gustatory_model$coefficients[[3]])
kiswahili_gustatory_summary <- summary(kiswahili_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_gustatory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_gustatory_interaction_model <- glm(produces ~ age * kiswahili_gustatory_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_gustatory_interaction_summary <- summary(kiswahili_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_gustatory_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_gustatory_model <- glm(produces ~ age + korean_gustatory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_gustatory_effect <- ggeffect(korean_gustatory_model, terms = "korean_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_gustatory_model$coefficients[[3]])
korean_gustatory_summary <- summary(korean_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_gustatory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_gustatory_interaction_model <- glm(produces ~ age * korean_gustatory_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_gustatory_interaction_summary <- summary(korean_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_gustatory_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_gustatory_model <- glm(produces ~ age + latvian_gustatory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_gustatory_effect <- ggeffect(latvian_gustatory_model, terms = "latvian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_gustatory_model$coefficients[[3]])
latvian_gustatory_summary <- summary(latvian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_gustatory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_gustatory_interaction_model <- glm(produces ~ age * latvian_gustatory_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_gustatory_interaction_summary <- summary(latvian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_gustatory_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_gustatory_model <- glm(produces ~ age + norwegian_gustatory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_gustatory_effect <- ggeffect(norwegian_gustatory_model, terms = "norwegian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_gustatory_model$coefficients[[3]])
norwegian_gustatory_summary <- summary(norwegian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_gustatory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_gustatory_interaction_model <- glm(produces ~ age * norwegian_gustatory_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_gustatory_interaction_summary <- summary(norwegian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_gustatory_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_gustatory_model <- glm(produces ~ age + persian_gustatory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_gustatory_effect <- ggeffect(persian_gustatory_model, terms = "persian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_gustatory_model$coefficients[[3]])
persian_gustatory_summary <- summary(persian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_gustatory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_gustatory_interaction_model <- glm(produces ~ age * persian_gustatory_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_gustatory_interaction_summary <- summary(persian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_gustatory_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_gustatory_model <- glm(produces ~ age + portuguese_gustatory_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_gustatory_effect <- ggeffect(portuguese_gustatory_model, terms = "portuguese_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_gustatory_model <- glm(produces ~ age + russian_gustatory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_gustatory_effect <- ggeffect(russian_gustatory_model, terms = "russian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_gustatory_model$coefficients[[3]])
russian_gustatory_summary <- summary(russian_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_gustatory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_gustatory_interaction_model <- glm(produces ~ age * russian_gustatory_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_gustatory_interaction_summary <- summary(russian_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_gustatory_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_gustatory_model <- glm(produces ~ age + slovak_gustatory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_gustatory_effect <- ggeffect(slovak_gustatory_model, terms = "slovak_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_gustatory_model$coefficients[[3]])
slovak_gustatory_summary <- summary(slovak_gustatory_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_gustatory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_gustatory_interaction_model <- glm(produces ~ age * slovak_gustatory_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_gustatory_interaction_summary <- summary(slovak_gustatory_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_gustatory_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_gustatory_model <- glm(produces ~ age + spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_gustatory_effect <- ggeffect(spanish_argentinian_gustatory_model, terms = "spanish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_gustatory_model$coefficients[[3]])
spanish_argentinian_gustatory_summary <- summary(spanish_argentinian_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_gustatory_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_gustatory_interaction_model <- glm(produces ~ age * spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_gustatory_interaction_summary <- summary(spanish_argentinian_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_gustatory_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_gustatory_model <- glm(produces ~ age + spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_gustatory_effect <- ggeffect(spanish_chilean_gustatory_model, terms = "spanish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_gustatory_model$coefficients[[3]])
spanish_chilean_gustatory_summary <- summary(spanish_chilean_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_gustatory_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_gustatory_interaction_model <- glm(produces ~ age * spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_gustatory_interaction_summary <- summary(spanish_chilean_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_gustatory_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_gustatory_model <- glm(produces ~ age + spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_gustatory_effect <- ggeffect(spanish_european_gustatory_model, terms = "spanish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_gustatory_model$coefficients[[3]])
spanish_european_gustatory_summary <- summary(spanish_european_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_gustatory_rating") %>%
  mutate(language = "spanish_european")
spanish_european_gustatory_interaction_model <- glm(produces ~ age * spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_gustatory_interaction_summary <- summary(spanish_european_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_gustatory_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_gustatory_model <- glm(produces ~ age + spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_gustatory_effect <- ggeffect(spanish_mexican_gustatory_model, terms = "spanish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_gustatory_model$coefficients[[3]])
spanish_mexican_gustatory_summary <- summary(spanish_mexican_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_gustatory_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_gustatory_interaction_model <- glm(produces ~ age * spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_gustatory_interaction_summary <- summary(spanish_mexican_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_gustatory_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_gustatory_model <- glm(produces ~ age + spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_gustatory_effect <- ggeffect(spanish_peruvian_gustatory_model, terms = "spanish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_gustatory_model$coefficients[[3]])
spanish_peruvian_gustatory_summary <- summary(spanish_peruvian_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_gustatory_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_gustatory_interaction_model <- glm(produces ~ age * spanish_gustatory_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_gustatory_interaction_summary <- summary(spanish_peruvian_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_gustatory_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_gustatory_model <- glm(produces ~ age + swedish_gustatory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_gustatory_effect <- ggeffect(swedish_gustatory_model, terms = "swedish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_gustatory_model$coefficients[[3]])
swedish_gustatory_summary <- summary(swedish_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_gustatory_rating") %>%
  mutate(language = "swedish")
swedish_gustatory_interaction_model <- glm(produces ~ age * swedish_gustatory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_gustatory_interaction_model <- glm(produces ~ age * swedish_gustatory_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_gustatory_interaction_summary <- summary(swedish_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_gustatory_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_gustatory_model <- glm(as.factor(produces) ~ age + arabic_gustatory_rating + lexical_category, 
                                     data = arabic_instrument_data, family = "binomial")
arabic_gustatory_effect <- ggpredict(arabic_gustatory_model, terms = "arabic_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_gustatory_model$coefficients[[3]])
arabic_gustatory_summary <- summary(arabic_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_gustatory_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_gustatory_interaction_model <- glm(as.factor(produces) ~ age * arabic_gustatory_rating + lexical_category, 
                                                 data = arabic_instrument_data, family = "binomial")
arabic_gustatory_interaction_summary <- summary(arabic_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_gustatory_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_gustatory_model <- glm(as.factor(produces) ~ age + catalan_gustatory_rating + lexical_category, 
                                      data = catalan_instrument_data, family = "binomial")
catalan_gustatory_effect <- ggpredict(catalan_gustatory_model, terms = "catalan_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_gustatory_model$coefficients[[3]])
catalan_gustatory_summary <- summary(catalan_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_gustatory_rating") %>%
  mutate(language = "catalan") 
catalan_gustatory_interaction_model <- glm(as.factor(produces) ~ age * catalan_gustatory_rating  + lexical_category, 
                                                  data = catalan_instrument_data, family = "binomial")
catalan_gustatory_interaction_summary <- summary(catalan_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_gustatory_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_gustatory_model <- glm(as.factor(produces) ~ age + estonian_gustatory_rating + lexical_category, 
                                       data = estonian_instrument_data, family = "binomial")
estonian_gustatory_effect <- ggpredict(estonian_gustatory_model, terms = "estonian_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_gustatory_model$coefficients[[3]])
estonian_gustatory_summary <- summary(estonian_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_gustatory_rating") %>%
  mutate(language = "estonian") 
estonian_gustatory_interaction_model <- glm(as.factor(produces) ~ age * estonian_gustatory_rating  + lexical_category, 
                                                   data = estonian_instrument_data, family = "binomial")
estonian_gustatory_interaction_summary <- summary(estonian_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_gustatory_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_gustatory_model <- glm(as.factor(produces) ~ age + japanese_gustatory_rating + lexical_category, 
                                       data = japanese_instrument_data, family = "binomial")
japanese_gustatory_effect <- ggpredict(japanese_gustatory_model, terms = "japanese_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_gustatory_model$coefficients[[3]])
japanese_gustatory_summary <- summary(japanese_gustatory_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_gustatory_rating") %>%
  mutate(language = "japanese") 
japanese_gustatory_interaction_model <- glm(as.factor(produces) ~ age * japanese_gustatory_rating  + lexical_category, 
                                                   data = japanese_instrument_data, family = "binomial")
japanese_gustatory_interaction_summary <- summary(japanese_gustatory_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_gustatory_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_gustatory_model <- glm(produces ~ age + turkish_gustatory_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_gustatory_effect <- ggeffect(turkish_gustatory_model, terms = "turkish_gustatory_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_gustatory_effects <- bind_rows(asl_gustatory_effect,
                                          bsl_gustatory_effect,
                                          chinese_beijing_gustatory_effect,
                                          chinese_cantonese_gustatory_effect,
                                          chinese_taiwanese_gustatory_effect,
                                          croatian_gustatory_effect,
                                          czech_gustatory_effect,
                                          english_american_gustatory_effect,
                                          english_australian_gustatory_effect,
                                          english_british_gustatory_effect,
                                          english_irish_gustatory_effect,
                                          danish_gustatory_effect,
                                          dutch_gustatory_effect,
                                          italian_gustatory_effect,
                                          finnish_gustatory_effect,
                                          french_european_gustatory_effect,
                                          french_quebecois_gustatory_effect,
                                          german_gustatory_effect,
                                          greek_gustatory_effect,
                                          hebrew_gustatory_effect,
                                          hungarian_gustatory_effect,
                                          irish_gustatory_effect,
                                          kiswahili_gustatory_effect,
                                          korean_gustatory_effect,
                                          latvian_gustatory_effect,
                                          norwegian_gustatory_effect,
                                          persian_gustatory_effect,
                                          russian_gustatory_effect,
                                          slovak_gustatory_effect,
                                          spanish_argentinian_gustatory_effect,
                                          spanish_chilean_gustatory_effect,
                                          spanish_european_gustatory_effect,
                                          spanish_mexican_gustatory_effect,
                                          spanish_peruvian_gustatory_effect,
                                          swedish_gustatory_effect,
                                          arabic_gustatory_effect,
                                          catalan_gustatory_effect,
                                          estonian_gustatory_effect,
                                          japanese_gustatory_effect
                                          # , turkish_gustatory_effect
)
write_rds(all_gustatory_effects, "models/effects/all_gustatory_effects.rds")

all_gustatory_effects_plot <- ggplot(all_gustatory_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "gustatory Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_gustatory_effects_plots.png", all_gustatory_effects_plot, height = 8, width = 5)


all_gustatory_summaries <- bind_rows(asl_gustatory_summary,
                                            bsl_gustatory_summary,
                                            chinese_beijing_gustatory_summary,
                                            chinese_cantonese_gustatory_summary,
                                            chinese_taiwanese_gustatory_summary,
                                            croatian_gustatory_summary,
                                            czech_gustatory_summary,
                                            english_american_gustatory_summary,
                                            english_australian_gustatory_summary,
                                            english_british_gustatory_summary,
                                            english_irish_gustatory_summary,
                                            danish_gustatory_summary,
                                            dutch_gustatory_summary,
                                            italian_gustatory_summary,
                                            finnish_gustatory_summary,
                                            french_european_gustatory_summary,
                                            french_quebecois_gustatory_summary,
                                            german_gustatory_summary,
                                            greek_gustatory_summary,
                                            hebrew_gustatory_summary,
                                            hungarian_gustatory_summary,
                                            irish_gustatory_summary,
                                            kiswahili_gustatory_summary,
                                            korean_gustatory_summary,
                                            latvian_gustatory_summary,
                                            norwegian_gustatory_summary,
                                            persian_gustatory_summary,
                                            russian_gustatory_summary,
                                            slovak_gustatory_summary,
                                            spanish_argentinian_gustatory_summary,
                                            spanish_chilean_gustatory_summary,
                                            spanish_european_gustatory_summary,
                                            spanish_mexican_gustatory_summary,
                                            spanish_peruvian_gustatory_summary,
                                            swedish_gustatory_summary,
                                            arabic_gustatory_summary,
                                            catalan_gustatory_summary,
                                            estonian_gustatory_summary,
                                            japanese_gustatory_summary
                                            # , turkish_gustatory_summary
) %>%
  mutate(variable = "Gustatory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_gustatory_summaries, "models/effects/all_gustatory_summaries.rds")



all_gustatory_interaction_summaries <- bind_rows(asl_gustatory_interaction_summary,
                                                        bsl_gustatory_interaction_summary,
                                                        chinese_beijing_gustatory_interaction_summary,
                                                        chinese_cantonese_gustatory_interaction_summary,
                                                        chinese_taiwanese_gustatory_interaction_summary,
                                                        croatian_gustatory_interaction_summary,
                                                        czech_gustatory_interaction_summary,
                                                        english_american_gustatory_interaction_summary,
                                                        english_australian_gustatory_interaction_summary,
                                                        english_british_gustatory_interaction_summary,
                                                        english_irish_gustatory_interaction_summary,
                                                        danish_gustatory_interaction_summary,
                                                        dutch_gustatory_interaction_summary,
                                                        italian_gustatory_interaction_summary,
                                                        finnish_gustatory_interaction_summary,
                                                        french_european_gustatory_interaction_summary,
                                                        french_quebecois_gustatory_interaction_summary,
                                                        german_gustatory_interaction_summary,
                                                        greek_gustatory_interaction_summary,
                                                        hebrew_gustatory_interaction_summary,
                                                        hungarian_gustatory_interaction_summary,
                                                        irish_gustatory_interaction_summary,
                                                        kiswahili_gustatory_interaction_summary,
                                                        korean_gustatory_interaction_summary,
                                                        latvian_gustatory_interaction_summary,
                                                        norwegian_gustatory_interaction_summary,
                                                        persian_gustatory_interaction_summary,
                                                        russian_gustatory_interaction_summary,
                                                        slovak_gustatory_interaction_summary,
                                                        spanish_argentinian_gustatory_interaction_summary,
                                                        spanish_chilean_gustatory_interaction_summary,
                                                        spanish_european_gustatory_interaction_summary,
                                                        spanish_mexican_gustatory_interaction_summary,
                                                        spanish_peruvian_gustatory_interaction_summary,
                                                        swedish_gustatory_interaction_summary,
                                                        arabic_gustatory_interaction_summary,
                                                        catalan_gustatory_interaction_summary,
                                                        estonian_gustatory_interaction_summary,
                                                        japanese_gustatory_interaction_summary
                                                        # , turkish_gustatory_summary
) %>%
  mutate(variable = "Gustatory",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_gustatory_interaction_summaries, "models/effects/all_gustatory_interaction_summaries.rds")
