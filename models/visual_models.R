library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# visual
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_visual_model <- glm(as.factor(produces) ~ age + asl_visual_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                        data = asl_instrument_data, family = "binomial")
asl_visual_effect <- ggpredict(asl_visual_model, terms = "asl_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_visual_model$coefficients[[3]])
asl_visual_summary <- summary(asl_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_visual_rating") %>%
  mutate(language = "asl") 
asl_visual_interaction_model <- glm(as.factor(produces) ~ age * asl_visual_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                    data = asl_instrument_data, family = "binomial")
asl_visual_interaction_summary <- summary(asl_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_visual_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_visual_model <- glm(as.factor(produces) ~ age + bsl_visual_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_visual_effect <- ggpredict(bsl_visual_model, terms = "bsl_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_visual_model$coefficients[[3]]) 
bsl_visual_summary <- summary(bsl_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_visual_rating") %>%
  mutate(language = "bsl")
bsl_visual_interaction_model <- glm(as.factor(produces) ~ age * bsl_visual_rating + lexical_category, 
                                    data = bsl_instrument_data, family = "binomial")
bsl_visual_interaction_summary <- summary(bsl_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_visual_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_visual_model <- glm(produces ~ age + chinese_visual_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_visual_effect <- ggeffect(chinese_beijing_visual_model, terms = "chinese_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_visual_model$coefficients[[3]])
chinese_beijing_visual_summary <- summary(chinese_beijing_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_visual_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_visual_interaction_model <- glm(produces ~ age * chinese_visual_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_visual_interaction_summary <- summary(chinese_beijing_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_visual_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_visual_model <- glm(produces ~ age + chinese_visual_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_visual_effect <- ggeffect(chinese_cantonese_visual_model, terms = "chinese_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_visual_model$coefficients[[3]])
chinese_cantonese_visual_summary <- summary(chinese_cantonese_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_visual_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_visual_interaction_model <- glm(produces ~ age * chinese_visual_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_visual_interaction_summary <- summary(chinese_cantonese_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_visual_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_visual_model <- glm(produces ~ age + chinese_visual_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_visual_effect <- ggeffect(chinese_taiwanese_visual_model, terms = "chinese_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_visual_model$coefficients[[3]])
chinese_taiwanese_visual_summary <- summary(chinese_taiwanese_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_visual_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_visual_interaction_model <- glm(produces ~ age * chinese_visual_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_visual_interaction_summary <- summary(chinese_taiwanese_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_visual_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_visual_model <- glm(as.factor(produces) ~ age + croatian_visual_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_visual_effect <- ggpredict(croatian_visual_model, terms = "croatian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_visual_model$coefficients[[3]])
croatian_visual_summary <- summary(croatian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_visual_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_visual_interaction_model <- glm(as.factor(produces) ~ age * croatian_visual_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_visual_interaction_summary <- summary(croatian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_visual_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_visual_model <- glm(as.factor(produces) ~ age + czech_visual_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_visual_effect <- ggpredict(czech_visual_model, terms = "czech_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_visual_model$coefficients[[3]])
czech_visual_summary <- summary(czech_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_visual_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_visual_interaction_model <- glm(as.factor(produces) ~ age * czech_visual_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_visual_interaction_summary <- summary(czech_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_visual_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_visual_model <- glm(as.factor(produces) ~ age + danish_visual_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_visual_effect <- ggpredict(danish_visual_model, terms = "danish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_visual_model$coefficients[[3]])
danish_visual_summary <- summary(danish_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_visual_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_visual_interaction_model <- glm(as.factor(produces) ~ age * danish_visual_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_visual_interaction_summary <- summary(danish_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_visual_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_visual_model <- glm(as.factor(produces) ~ age + dutch_visual_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_visual_effect <- ggpredict(dutch_visual_model, terms = "dutch_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_visual_model$coefficients[[3]])  
dutch_visual_summary <- summary(dutch_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_visual_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_visual_interaction_model <- glm(as.factor(produces) ~ age * dutch_visual_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_visual_interaction_summary <- summary(dutch_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_visual_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_visual_model <- glm(produces ~ age + english_visual_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_visual_effect <- ggeffect(english_american_visual_model, terms = "english_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_visual_model$coefficients[[3]])
english_american_visual_summary <- summary(english_american_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_visual_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_visual_interaction_model <- glm(produces ~ age * english_visual_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_visual_interaction_summary <- summary(english_american_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_visual_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_visual_model <- glm(produces ~ age + english_visual_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_visual_effect <- ggeffect(english_australian_visual_model, terms = "english_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_visual_model$coefficients[[3]])
english_australian_visual_summary <- summary(english_australian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_visual_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_visual_interaction_model <- glm(produces ~ age * english_visual_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_visual_interaction_summary <- summary(english_australian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_visual_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_visual_model <- glm(produces ~ age + english_visual_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_visual_effect <- ggeffect(english_british_visual_model, terms = "english_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_visual_model$coefficients[[3]])
english_british_visual_summary <- summary(english_british_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_visual_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_visual_interaction_model <- glm(produces ~ age * english_visual_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_visual_interaction_summary <- summary(english_british_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_visual_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_visual_model <- glm(produces ~ age + english_visual_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_visual_effect <- ggeffect(english_irish_visual_model, terms = "english_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_visual_model$coefficients[[3]])
english_irish_visual_summary <- summary(english_irish_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_visual_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_visual_interaction_model <- glm(produces ~ age * english_visual_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_visual_interaction_summary <- summary(english_irish_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_visual_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_visual_model <- glm(as.factor(produces) ~ age + finnish_visual_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_visual_effect <- ggpredict(finnish_visual_model, terms = "finnish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_visual_model$coefficients[[3]]) 
finnish_visual_summary <- summary(finnish_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_visual_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_visual_interaction_model <- glm(as.factor(produces) ~ age * finnish_visual_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_visual_interaction_summary <- summary(finnish_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_visual_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_visual_model <- glm(as.factor(produces) ~ age + french_visual_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_visual_effect <- ggpredict(french_european_visual_model, terms = "french_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_visual_model$coefficients[[3]]) 
french_european_visual_summary <- summary(french_european_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_visual_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_visual_interaction_model <- glm(as.factor(produces) ~ age * french_visual_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_visual_interaction_summary <- summary(french_european_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_visual_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_visual_model <- glm(as.factor(produces) ~ age + french_visual_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_visual_effect <- ggpredict(french_quebecois_visual_model, terms = "french_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_visual_model$coefficients[[3]]) 
french_quebecois_visual_summary <- summary(french_quebecois_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_visual_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_visual_interaction_model <- glm(as.factor(produces) ~ age * french_visual_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_visual_interaction_summary <- summary(french_quebecois_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_visual_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_visual_model <- glm(as.factor(produces) ~ age + german_visual_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_visual_effect <- ggpredict(german_visual_model, terms = "german_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_visual_model$coefficients[[3]]) 
german_visual_summary <- summary(german_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_visual_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_visual_interaction_model <- glm(as.factor(produces) ~ age * german_visual_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_visual_interaction_summary <- summary(german_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_visual_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_visual_model <- glm(as.factor(produces) ~ age + greek_visual_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_visual_effect <- ggpredict(greek_visual_model, terms = "greek_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_visual_model$coefficients[[3]]) 
greek_visual_summary <- summary(greek_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_visual_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_visual_interaction_model <- glm(as.factor(produces) ~ age * greek_visual_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_visual_interaction_summary <- summary(greek_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_visual_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_visual_model <- glm(as.factor(produces) ~ age + hebrew_visual_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_visual_effect <- ggpredict(hebrew_visual_model, terms = "hebrew_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_visual_model$coefficients[[3]]) 
hebrew_visual_summary <- summary(hebrew_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_visual_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_visual_interaction_model <- glm(as.factor(produces) ~ age * hebrew_visual_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_visual_interaction_summary <- summary(hebrew_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_visual_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_visual_model <- glm(as.factor(produces) ~ age + hungarian_visual_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_visual_effect <- ggpredict(hungarian_visual_model, terms = "hungarian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_visual_model$coefficients[[3]])
hungarian_visual_summary <- summary(hungarian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_visual_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_visual_interaction_model <- glm(as.factor(produces) ~ age * hungarian_visual_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_visual_interaction_summary <- summary(hungarian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_visual_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_visual_model <- glm(as.factor(produces) ~ age + irish_visual_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_visual_effect <- ggpredict(irish_visual_model, terms = "irish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_visual_model$coefficients[[3]])
irish_visual_summary <- summary(irish_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_visual_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_visual_interaction_model <- glm(as.factor(produces) ~ age * irish_visual_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_visual_interaction_summary <- summary(irish_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_visual_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_visual_model <- glm(produces ~ age + italian_visual_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_visual_effect <- ggeffect(italian_visual_model, terms = "italian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_visual_model$coefficients[[3]])
italian_visual_summary <- summary(italian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_visual_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_visual_interaction_model <- glm(produces ~ age * italian_visual_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_visual_interaction_summary <- summary(italian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_visual_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_visual_model <- glm(produces ~ age + kigiriama_visual_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_visual_effect <- ggeffect(kigiriama_visual_model, terms = "kigiriama_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_visual_model <- glm(produces ~ age + kiswahili_visual_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_visual_effect <- ggeffect(kiswahili_visual_model, terms = "kiswahili_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_visual_model$coefficients[[3]])
kiswahili_visual_summary <- summary(kiswahili_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_visual_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_visual_interaction_model <- glm(produces ~ age * kiswahili_visual_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_visual_interaction_summary <- summary(kiswahili_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_visual_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_visual_model <- glm(produces ~ age + korean_visual_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_visual_effect <- ggeffect(korean_visual_model, terms = "korean_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_visual_model$coefficients[[3]])
korean_visual_summary <- summary(korean_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_visual_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_visual_interaction_model <- glm(produces ~ age * korean_visual_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_visual_interaction_summary <- summary(korean_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_visual_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_visual_model <- glm(produces ~ age + latvian_visual_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_visual_effect <- ggeffect(latvian_visual_model, terms = "latvian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_visual_model$coefficients[[3]])
latvian_visual_summary <- summary(latvian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_visual_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_visual_interaction_model <- glm(produces ~ age * latvian_visual_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_visual_interaction_summary <- summary(latvian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_visual_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_visual_model <- glm(produces ~ age + norwegian_visual_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_visual_effect <- ggeffect(norwegian_visual_model, terms = "norwegian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_visual_model$coefficients[[3]])
norwegian_visual_summary <- summary(norwegian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_visual_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_visual_interaction_model <- glm(produces ~ age * norwegian_visual_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_visual_interaction_summary <- summary(norwegian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_visual_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_visual_model <- glm(produces ~ age + persian_visual_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_visual_effect <- ggeffect(persian_visual_model, terms = "persian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_visual_model$coefficients[[3]])
persian_visual_summary <- summary(persian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_visual_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_visual_interaction_model <- glm(produces ~ age * persian_visual_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_visual_interaction_summary <- summary(persian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_visual_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_visual_model <- glm(produces ~ age + portuguese_visual_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_visual_effect <- ggeffect(portuguese_visual_model, terms = "portuguese_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_visual_model <- glm(produces ~ age + russian_visual_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_visual_effect <- ggeffect(russian_visual_model, terms = "russian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_visual_model$coefficients[[3]])
russian_visual_summary <- summary(russian_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_visual_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_visual_interaction_model <- glm(produces ~ age * russian_visual_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_visual_interaction_summary <- summary(russian_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_visual_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_visual_model <- glm(produces ~ age + slovak_visual_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_visual_effect <- ggeffect(slovak_visual_model, terms = "slovak_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_visual_model$coefficients[[3]])
slovak_visual_summary <- summary(slovak_visual_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_visual_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_visual_interaction_model <- glm(produces ~ age * slovak_visual_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_visual_interaction_summary <- summary(slovak_visual_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_visual_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_visual_model <- glm(produces ~ age + spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_visual_effect <- ggeffect(spanish_argentinian_visual_model, terms = "spanish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_visual_model$coefficients[[3]])
spanish_argentinian_visual_summary <- summary(spanish_argentinian_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_visual_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_visual_interaction_model <- glm(produces ~ age * spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_visual_interaction_summary <- summary(spanish_argentinian_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_visual_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_visual_model <- glm(produces ~ age + spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_visual_effect <- ggeffect(spanish_chilean_visual_model, terms = "spanish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_visual_model$coefficients[[3]])
spanish_chilean_visual_summary <- summary(spanish_chilean_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_visual_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_visual_interaction_model <- glm(produces ~ age * spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_visual_interaction_summary <- summary(spanish_chilean_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_visual_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_visual_model <- glm(produces ~ age + spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_visual_effect <- ggeffect(spanish_european_visual_model, terms = "spanish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_visual_model$coefficients[[3]])
spanish_european_visual_summary <- summary(spanish_european_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_visual_rating") %>%
  mutate(language = "spanish_european")
spanish_european_visual_interaction_model <- glm(produces ~ age * spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_visual_interaction_summary <- summary(spanish_european_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_visual_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_visual_model <- glm(produces ~ age + spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_visual_effect <- ggeffect(spanish_mexican_visual_model, terms = "spanish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_visual_model$coefficients[[3]])
spanish_mexican_visual_summary <- summary(spanish_mexican_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_visual_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_visual_interaction_model <- glm(produces ~ age * spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_visual_interaction_summary <- summary(spanish_mexican_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_visual_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_visual_model <- glm(produces ~ age + spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_visual_effect <- ggeffect(spanish_peruvian_visual_model, terms = "spanish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_visual_model$coefficients[[3]])
spanish_peruvian_visual_summary <- summary(spanish_peruvian_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_visual_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_visual_interaction_model <- glm(produces ~ age * spanish_visual_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_visual_interaction_summary <- summary(spanish_peruvian_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_visual_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_visual_model <- glm(produces ~ age + swedish_visual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_visual_effect <- ggeffect(swedish_visual_model, terms = "swedish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_visual_model$coefficients[[3]])
swedish_visual_summary <- summary(swedish_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_visual_rating") %>%
  mutate(language = "swedish")
swedish_visual_interaction_model <- glm(produces ~ age * swedish_visual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_visual_interaction_model <- glm(produces ~ age * swedish_visual_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_visual_interaction_summary <- summary(swedish_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_visual_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_visual_model <- glm(as.factor(produces) ~ age + arabic_visual_rating + lexical_category, 
                           data = arabic_instrument_data, family = "binomial")
arabic_visual_effect <- ggpredict(arabic_visual_model, terms = "arabic_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_visual_model$coefficients[[3]])
arabic_visual_summary <- summary(arabic_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_visual_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_visual_interaction_model <- glm(as.factor(produces) ~ age * arabic_visual_rating + lexical_category, 
                                       data = arabic_instrument_data, family = "binomial")
arabic_visual_interaction_summary <- summary(arabic_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_visual_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_visual_model <- glm(as.factor(produces) ~ age + catalan_visual_rating + lexical_category, 
                            data = catalan_instrument_data, family = "binomial")
catalan_visual_effect <- ggpredict(catalan_visual_model, terms = "catalan_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_visual_model$coefficients[[3]])
catalan_visual_summary <- summary(catalan_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_visual_rating") %>%
  mutate(language = "catalan") 
catalan_visual_interaction_model <- glm(as.factor(produces) ~ age * catalan_visual_rating  + lexical_category, 
                                        data = catalan_instrument_data, family = "binomial")
catalan_visual_interaction_summary <- summary(catalan_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_visual_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_visual_model <- glm(as.factor(produces) ~ age + estonian_visual_rating + lexical_category, 
                             data = estonian_instrument_data, family = "binomial")
estonian_visual_effect <- ggpredict(estonian_visual_model, terms = "estonian_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_visual_model$coefficients[[3]])
estonian_visual_summary <- summary(estonian_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_visual_rating") %>%
  mutate(language = "estonian") 
estonian_visual_interaction_model <- glm(as.factor(produces) ~ age * estonian_visual_rating  + lexical_category, 
                                         data = estonian_instrument_data, family = "binomial")
estonian_visual_interaction_summary <- summary(estonian_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_visual_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_visual_model <- glm(as.factor(produces) ~ age + japanese_visual_rating + lexical_category, 
                             data = japanese_instrument_data, family = "binomial")
japanese_visual_effect <- ggpredict(japanese_visual_model, terms = "japanese_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_visual_model$coefficients[[3]])
japanese_visual_summary <- summary(japanese_visual_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_visual_rating") %>%
  mutate(language = "japanese") 
japanese_visual_interaction_model <- glm(as.factor(produces) ~ age * japanese_visual_rating  + lexical_category, 
                                         data = japanese_instrument_data, family = "binomial")
japanese_visual_interaction_summary <- summary(japanese_visual_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_visual_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_visual_model <- glm(produces ~ age + turkish_visual_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_visual_effect <- ggeffect(turkish_visual_model, terms = "turkish_visual_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_visual_effects <- bind_rows(asl_visual_effect,
                                bsl_visual_effect,
                                chinese_beijing_visual_effect,
                                chinese_cantonese_visual_effect,
                                chinese_taiwanese_visual_effect,
                                croatian_visual_effect,
                                czech_visual_effect,
                                english_american_visual_effect,
                                english_australian_visual_effect,
                                english_british_visual_effect,
                                english_irish_visual_effect,
                                danish_visual_effect,
                                dutch_visual_effect,
                                italian_visual_effect,
                                finnish_visual_effect,
                                french_european_visual_effect,
                                french_quebecois_visual_effect,
                                german_visual_effect,
                                greek_visual_effect,
                                hebrew_visual_effect,
                                hungarian_visual_effect,
                                irish_visual_effect,
                                kiswahili_visual_effect,
                                korean_visual_effect,
                                latvian_visual_effect,
                                norwegian_visual_effect,
                                persian_visual_effect,
                                russian_visual_effect,
                                slovak_visual_effect,
                                spanish_argentinian_visual_effect,
                                spanish_chilean_visual_effect,
                                spanish_european_visual_effect,
                                spanish_mexican_visual_effect,
                                spanish_peruvian_visual_effect,
                                swedish_visual_effect,
                                arabic_visual_effect,
                                catalan_visual_effect,
                                estonian_visual_effect,
                                japanese_visual_effect
                                # , turkish_visual_effect
)
write_rds(all_visual_effects, "models/effects/all_visual_effects.rds")

all_visual_effects_plot <- ggplot(all_visual_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "visual Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_visual_effects_plots.png", all_visual_effects_plot, height = 8, width = 5)


all_visual_summaries <- bind_rows(asl_visual_summary,
                                  bsl_visual_summary,
                                  chinese_beijing_visual_summary,
                                  chinese_cantonese_visual_summary,
                                  chinese_taiwanese_visual_summary,
                                  croatian_visual_summary,
                                  czech_visual_summary,
                                  english_american_visual_summary,
                                  english_australian_visual_summary,
                                  english_british_visual_summary,
                                  english_irish_visual_summary,
                                  danish_visual_summary,
                                  dutch_visual_summary,
                                  italian_visual_summary,
                                  finnish_visual_summary,
                                  french_european_visual_summary,
                                  french_quebecois_visual_summary,
                                  german_visual_summary,
                                  greek_visual_summary,
                                  hebrew_visual_summary,
                                  hungarian_visual_summary,
                                  irish_visual_summary,
                                  kiswahili_visual_summary,
                                  korean_visual_summary,
                                  latvian_visual_summary,
                                  norwegian_visual_summary,
                                  persian_visual_summary,
                                  russian_visual_summary,
                                  slovak_visual_summary,
                                  spanish_argentinian_visual_summary,
                                  spanish_chilean_visual_summary,
                                  spanish_european_visual_summary,
                                  spanish_mexican_visual_summary,
                                  spanish_peruvian_visual_summary,
                                  swedish_visual_summary,
                                  arabic_visual_summary,
                                  catalan_visual_summary,
                                  estonian_visual_summary,
                                  japanese_visual_summary
                                  # , turkish_visual_summary
) %>%
  mutate(variable = "visual",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_visual_summaries, "models/effects/all_visual_summaries.rds")



all_visual_interaction_summaries <- bind_rows(asl_visual_interaction_summary,
                                              bsl_visual_interaction_summary,
                                              chinese_beijing_visual_interaction_summary,
                                              chinese_cantonese_visual_interaction_summary,
                                              chinese_taiwanese_visual_interaction_summary,
                                              croatian_visual_interaction_summary,
                                              czech_visual_interaction_summary,
                                              english_american_visual_interaction_summary,
                                              english_australian_visual_interaction_summary,
                                              english_british_visual_interaction_summary,
                                              english_irish_visual_interaction_summary,
                                              danish_visual_interaction_summary,
                                              dutch_visual_interaction_summary,
                                              italian_visual_interaction_summary,
                                              finnish_visual_interaction_summary,
                                              french_european_visual_interaction_summary,
                                              french_quebecois_visual_interaction_summary,
                                              german_visual_interaction_summary,
                                              greek_visual_interaction_summary,
                                              hebrew_visual_interaction_summary,
                                              hungarian_visual_interaction_summary,
                                              irish_visual_interaction_summary,
                                              kiswahili_visual_interaction_summary,
                                              korean_visual_interaction_summary,
                                              latvian_visual_interaction_summary,
                                              norwegian_visual_interaction_summary,
                                              persian_visual_interaction_summary,
                                              russian_visual_interaction_summary,
                                              slovak_visual_interaction_summary,
                                              spanish_argentinian_visual_interaction_summary,
                                              spanish_chilean_visual_interaction_summary,
                                              spanish_european_visual_interaction_summary,
                                              spanish_mexican_visual_interaction_summary,
                                              spanish_peruvian_visual_interaction_summary,
                                              swedish_visual_interaction_summary,
                                              arabic_visual_interaction_summary,
                                              catalan_visual_interaction_summary,
                                              estonian_visual_interaction_summary,
                                              japanese_visual_interaction_summary
                                              # , turkish_visual_summary
) %>%
  mutate(variable = "visual",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_visual_interaction_summaries, "models/effects/all_visual_interaction_summaries.rds")
