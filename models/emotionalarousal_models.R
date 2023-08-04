library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# emotionalarousal
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_emotionalarousal_model <- glm(as.factor(produces) ~ age + asl_emotionalarousal_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                              data = asl_instrument_data, family = "binomial")
asl_emotionalarousal_effect <- ggpredict(asl_emotionalarousal_model, terms = "asl_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_emotionalarousal_model$coefficients[[3]])
asl_emotionalarousal_summary <- summary(asl_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_emotionalarousal_rating") %>%
  mutate(language = "asl") 
asl_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * asl_emotionalarousal_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                          data = asl_instrument_data, family = "binomial")
asl_emotionalarousal_interaction_summary <- summary(asl_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_emotionalarousal_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_emotionalarousal_model <- glm(as.factor(produces) ~ age + bsl_emotionalarousal_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_emotionalarousal_effect <- ggpredict(bsl_emotionalarousal_model, terms = "bsl_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_emotionalarousal_model$coefficients[[3]]) 
bsl_emotionalarousal_summary <- summary(bsl_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_emotionalarousal_rating") %>%
  mutate(language = "bsl")
bsl_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * bsl_emotionalarousal_rating + lexical_category, 
                                          data = bsl_instrument_data, family = "binomial")
bsl_emotionalarousal_interaction_summary <- summary(bsl_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_emotionalarousal_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_emotionalarousal_model <- glm(produces ~ age + chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_emotionalarousal_effect <- ggeffect(chinese_beijing_emotionalarousal_model, terms = "chinese_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_emotionalarousal_model$coefficients[[3]])
chinese_beijing_emotionalarousal_summary <- summary(chinese_beijing_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_emotionalarousal_interaction_model <- glm(produces ~ age * chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_emotionalarousal_interaction_summary <- summary(chinese_beijing_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_emotionalarousal_model <- glm(produces ~ age + chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_emotionalarousal_effect <- ggeffect(chinese_cantonese_emotionalarousal_model, terms = "chinese_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_emotionalarousal_model$coefficients[[3]])
chinese_cantonese_emotionalarousal_summary <- summary(chinese_cantonese_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_emotionalarousal_interaction_model <- glm(produces ~ age * chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_emotionalarousal_interaction_summary <- summary(chinese_cantonese_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_emotionalarousal_model <- glm(produces ~ age + chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_emotionalarousal_effect <- ggeffect(chinese_taiwanese_emotionalarousal_model, terms = "chinese_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_emotionalarousal_model$coefficients[[3]])
chinese_taiwanese_emotionalarousal_summary <- summary(chinese_taiwanese_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_emotionalarousal_interaction_model <- glm(produces ~ age * chinese_emotionalarousal_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_emotionalarousal_interaction_summary <- summary(chinese_taiwanese_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_emotionalarousal_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_emotionalarousal_model <- glm(as.factor(produces) ~ age + croatian_emotionalarousal_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_emotionalarousal_effect <- ggpredict(croatian_emotionalarousal_model, terms = "croatian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_emotionalarousal_model$coefficients[[3]])
croatian_emotionalarousal_summary <- summary(croatian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_emotionalarousal_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * croatian_emotionalarousal_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_emotionalarousal_interaction_summary <- summary(croatian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_emotionalarousal_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_emotionalarousal_model <- glm(as.factor(produces) ~ age + czech_emotionalarousal_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_emotionalarousal_effect <- ggpredict(czech_emotionalarousal_model, terms = "czech_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_emotionalarousal_model$coefficients[[3]])
czech_emotionalarousal_summary <- summary(czech_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_emotionalarousal_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * czech_emotionalarousal_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_emotionalarousal_interaction_summary <- summary(czech_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_emotionalarousal_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_emotionalarousal_model <- glm(as.factor(produces) ~ age + danish_emotionalarousal_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_emotionalarousal_effect <- ggpredict(danish_emotionalarousal_model, terms = "danish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_emotionalarousal_model$coefficients[[3]])
danish_emotionalarousal_summary <- summary(danish_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_emotionalarousal_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * danish_emotionalarousal_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_emotionalarousal_interaction_summary <- summary(danish_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_emotionalarousal_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_emotionalarousal_model <- glm(as.factor(produces) ~ age + dutch_emotionalarousal_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_emotionalarousal_effect <- ggpredict(dutch_emotionalarousal_model, terms = "dutch_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_emotionalarousal_model$coefficients[[3]])  
dutch_emotionalarousal_summary <- summary(dutch_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_emotionalarousal_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * dutch_emotionalarousal_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_emotionalarousal_interaction_summary <- summary(dutch_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_emotionalarousal_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_emotionalarousal_model <- glm(produces ~ age + english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_emotionalarousal_effect <- ggeffect(english_american_emotionalarousal_model, terms = "english_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_emotionalarousal_model$coefficients[[3]])
english_american_emotionalarousal_summary <- summary(english_american_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_emotionalarousal_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_emotionalarousal_interaction_model <- glm(produces ~ age * english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_emotionalarousal_interaction_summary <- summary(english_american_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_emotionalarousal_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_emotionalarousal_model <- glm(produces ~ age + english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_emotionalarousal_effect <- ggeffect(english_australian_emotionalarousal_model, terms = "english_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_emotionalarousal_model$coefficients[[3]])
english_australian_emotionalarousal_summary <- summary(english_australian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_emotionalarousal_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_emotionalarousal_interaction_model <- glm(produces ~ age * english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_emotionalarousal_interaction_summary <- summary(english_australian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_emotionalarousal_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_emotionalarousal_model <- glm(produces ~ age + english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_emotionalarousal_effect <- ggeffect(english_british_emotionalarousal_model, terms = "english_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_emotionalarousal_model$coefficients[[3]])
english_british_emotionalarousal_summary <- summary(english_british_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_emotionalarousal_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_emotionalarousal_interaction_model <- glm(produces ~ age * english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_emotionalarousal_interaction_summary <- summary(english_british_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_emotionalarousal_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_emotionalarousal_model <- glm(produces ~ age + english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_emotionalarousal_effect <- ggeffect(english_irish_emotionalarousal_model, terms = "english_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_emotionalarousal_model$coefficients[[3]])
english_irish_emotionalarousal_summary <- summary(english_irish_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_emotionalarousal_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_emotionalarousal_interaction_model <- glm(produces ~ age * english_emotionalarousal_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_emotionalarousal_interaction_summary <- summary(english_irish_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_emotionalarousal_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_emotionalarousal_model <- glm(as.factor(produces) ~ age + finnish_emotionalarousal_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_emotionalarousal_effect <- ggpredict(finnish_emotionalarousal_model, terms = "finnish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_emotionalarousal_model$coefficients[[3]]) 
finnish_emotionalarousal_summary <- summary(finnish_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_emotionalarousal_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * finnish_emotionalarousal_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_emotionalarousal_interaction_summary <- summary(finnish_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_emotionalarousal_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_emotionalarousal_model <- glm(as.factor(produces) ~ age + french_emotionalarousal_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_emotionalarousal_effect <- ggpredict(french_european_emotionalarousal_model, terms = "french_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_emotionalarousal_model$coefficients[[3]]) 
french_european_emotionalarousal_summary <- summary(french_european_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_emotionalarousal_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * french_emotionalarousal_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_emotionalarousal_interaction_summary <- summary(french_european_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_emotionalarousal_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_emotionalarousal_model <- glm(as.factor(produces) ~ age + french_emotionalarousal_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_emotionalarousal_effect <- ggpredict(french_quebecois_emotionalarousal_model, terms = "french_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_emotionalarousal_model$coefficients[[3]]) 
french_quebecois_emotionalarousal_summary <- summary(french_quebecois_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_emotionalarousal_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * french_emotionalarousal_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_emotionalarousal_interaction_summary <- summary(french_quebecois_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_emotionalarousal_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_emotionalarousal_model <- glm(as.factor(produces) ~ age + german_emotionalarousal_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_emotionalarousal_effect <- ggpredict(german_emotionalarousal_model, terms = "german_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_emotionalarousal_model$coefficients[[3]]) 
german_emotionalarousal_summary <- summary(german_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_emotionalarousal_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * german_emotionalarousal_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_emotionalarousal_interaction_summary <- summary(german_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_emotionalarousal_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_emotionalarousal_model <- glm(as.factor(produces) ~ age + greek_emotionalarousal_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_emotionalarousal_effect <- ggpredict(greek_emotionalarousal_model, terms = "greek_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_emotionalarousal_model$coefficients[[3]]) 
greek_emotionalarousal_summary <- summary(greek_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_emotionalarousal_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * greek_emotionalarousal_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_emotionalarousal_interaction_summary <- summary(greek_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_emotionalarousal_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_emotionalarousal_model <- glm(as.factor(produces) ~ age + hebrew_emotionalarousal_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_emotionalarousal_effect <- ggpredict(hebrew_emotionalarousal_model, terms = "hebrew_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_emotionalarousal_model$coefficients[[3]]) 
hebrew_emotionalarousal_summary <- summary(hebrew_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_emotionalarousal_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * hebrew_emotionalarousal_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_emotionalarousal_interaction_summary <- summary(hebrew_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_emotionalarousal_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_emotionalarousal_model <- glm(as.factor(produces) ~ age + hungarian_emotionalarousal_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_emotionalarousal_effect <- ggpredict(hungarian_emotionalarousal_model, terms = "hungarian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_emotionalarousal_model$coefficients[[3]])
hungarian_emotionalarousal_summary <- summary(hungarian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_emotionalarousal_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * hungarian_emotionalarousal_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_emotionalarousal_interaction_summary <- summary(hungarian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_emotionalarousal_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_emotionalarousal_model <- glm(as.factor(produces) ~ age + irish_emotionalarousal_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_emotionalarousal_effect <- ggpredict(irish_emotionalarousal_model, terms = "irish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_emotionalarousal_model$coefficients[[3]])
irish_emotionalarousal_summary <- summary(irish_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_emotionalarousal_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * irish_emotionalarousal_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_emotionalarousal_interaction_summary <- summary(irish_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_emotionalarousal_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_emotionalarousal_model <- glm(produces ~ age + italian_emotionalarousal_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_emotionalarousal_effect <- ggeffect(italian_emotionalarousal_model, terms = "italian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_emotionalarousal_model$coefficients[[3]])
italian_emotionalarousal_summary <- summary(italian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_emotionalarousal_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_emotionalarousal_interaction_model <- glm(produces ~ age * italian_emotionalarousal_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_emotionalarousal_interaction_summary <- summary(italian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_emotionalarousal_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_emotionalarousal_model <- glm(produces ~ age + kigiriama_emotionalarousal_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_emotionalarousal_effect <- ggeffect(kigiriama_emotionalarousal_model, terms = "kigiriama_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_emotionalarousal_model <- glm(produces ~ age + kiswahili_emotionalarousal_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_emotionalarousal_effect <- ggeffect(kiswahili_emotionalarousal_model, terms = "kiswahili_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_emotionalarousal_model$coefficients[[3]])
kiswahili_emotionalarousal_summary <- summary(kiswahili_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_emotionalarousal_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_emotionalarousal_interaction_model <- glm(produces ~ age * kiswahili_emotionalarousal_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_emotionalarousal_interaction_summary <- summary(kiswahili_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_emotionalarousal_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_emotionalarousal_model <- glm(produces ~ age + korean_emotionalarousal_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_emotionalarousal_effect <- ggeffect(korean_emotionalarousal_model, terms = "korean_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_emotionalarousal_model$coefficients[[3]])
korean_emotionalarousal_summary <- summary(korean_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_emotionalarousal_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_emotionalarousal_interaction_model <- glm(produces ~ age * korean_emotionalarousal_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_emotionalarousal_interaction_summary <- summary(korean_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_emotionalarousal_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_emotionalarousal_model <- glm(produces ~ age + latvian_emotionalarousal_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_emotionalarousal_effect <- ggeffect(latvian_emotionalarousal_model, terms = "latvian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_emotionalarousal_model$coefficients[[3]])
latvian_emotionalarousal_summary <- summary(latvian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_emotionalarousal_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_emotionalarousal_interaction_model <- glm(produces ~ age * latvian_emotionalarousal_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_emotionalarousal_interaction_summary <- summary(latvian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_emotionalarousal_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_emotionalarousal_model <- glm(produces ~ age + norwegian_emotionalarousal_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_emotionalarousal_effect <- ggeffect(norwegian_emotionalarousal_model, terms = "norwegian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_emotionalarousal_model$coefficients[[3]])
norwegian_emotionalarousal_summary <- summary(norwegian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_emotionalarousal_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_emotionalarousal_interaction_model <- glm(produces ~ age * norwegian_emotionalarousal_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_emotionalarousal_interaction_summary <- summary(norwegian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_emotionalarousal_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_emotionalarousal_model <- glm(produces ~ age + persian_emotionalarousal_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_emotionalarousal_effect <- ggeffect(persian_emotionalarousal_model, terms = "persian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_emotionalarousal_model$coefficients[[3]])
persian_emotionalarousal_summary <- summary(persian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_emotionalarousal_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_emotionalarousal_interaction_model <- glm(produces ~ age * persian_emotionalarousal_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_emotionalarousal_interaction_summary <- summary(persian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_emotionalarousal_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_emotionalarousal_model <- glm(produces ~ age + portuguese_emotionalarousal_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_emotionalarousal_effect <- ggeffect(portuguese_emotionalarousal_model, terms = "portuguese_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_emotionalarousal_model <- glm(produces ~ age + russian_emotionalarousal_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_emotionalarousal_effect <- ggeffect(russian_emotionalarousal_model, terms = "russian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_emotionalarousal_model$coefficients[[3]])
russian_emotionalarousal_summary <- summary(russian_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_emotionalarousal_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_emotionalarousal_interaction_model <- glm(produces ~ age * russian_emotionalarousal_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_emotionalarousal_interaction_summary <- summary(russian_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_emotionalarousal_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_emotionalarousal_model <- glm(produces ~ age + slovak_emotionalarousal_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_emotionalarousal_effect <- ggeffect(slovak_emotionalarousal_model, terms = "slovak_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_emotionalarousal_model$coefficients[[3]])
slovak_emotionalarousal_summary <- summary(slovak_emotionalarousal_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_emotionalarousal_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_emotionalarousal_interaction_model <- glm(produces ~ age * slovak_emotionalarousal_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_emotionalarousal_interaction_summary <- summary(slovak_emotionalarousal_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_emotionalarousal_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_emotionalarousal_model <- glm(produces ~ age + spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_emotionalarousal_effect <- ggeffect(spanish_argentinian_emotionalarousal_model, terms = "spanish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_emotionalarousal_model$coefficients[[3]])
spanish_argentinian_emotionalarousal_summary <- summary(spanish_argentinian_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_emotionalarousal_interaction_model <- glm(produces ~ age * spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_emotionalarousal_interaction_summary <- summary(spanish_argentinian_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_emotionalarousal_model <- glm(produces ~ age + spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_emotionalarousal_effect <- ggeffect(spanish_chilean_emotionalarousal_model, terms = "spanish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_emotionalarousal_model$coefficients[[3]])
spanish_chilean_emotionalarousal_summary <- summary(spanish_chilean_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_emotionalarousal_interaction_model <- glm(produces ~ age * spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_emotionalarousal_interaction_summary <- summary(spanish_chilean_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_emotionalarousal_model <- glm(produces ~ age + spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_emotionalarousal_effect <- ggeffect(spanish_european_emotionalarousal_model, terms = "spanish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_emotionalarousal_model$coefficients[[3]])
spanish_european_emotionalarousal_summary <- summary(spanish_european_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_european")
spanish_european_emotionalarousal_interaction_model <- glm(produces ~ age * spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_emotionalarousal_interaction_summary <- summary(spanish_european_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_emotionalarousal_model <- glm(produces ~ age + spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_emotionalarousal_effect <- ggeffect(spanish_mexican_emotionalarousal_model, terms = "spanish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_emotionalarousal_model$coefficients[[3]])
spanish_mexican_emotionalarousal_summary <- summary(spanish_mexican_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_emotionalarousal_interaction_model <- glm(produces ~ age * spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_emotionalarousal_interaction_summary <- summary(spanish_mexican_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_emotionalarousal_model <- glm(produces ~ age + spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_emotionalarousal_effect <- ggeffect(spanish_peruvian_emotionalarousal_model, terms = "spanish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_emotionalarousal_model$coefficients[[3]])
spanish_peruvian_emotionalarousal_summary <- summary(spanish_peruvian_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_emotionalarousal_interaction_model <- glm(produces ~ age * spanish_emotionalarousal_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_emotionalarousal_interaction_summary <- summary(spanish_peruvian_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_emotionalarousal_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_emotionalarousal_model <- glm(produces ~ age + swedish_emotionalarousal_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_emotionalarousal_effect <- ggeffect(swedish_emotionalarousal_model, terms = "swedish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_emotionalarousal_model$coefficients[[3]])
swedish_emotionalarousal_summary <- summary(swedish_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_emotionalarousal_rating") %>%
  mutate(language = "swedish")
swedish_emotionalarousal_interaction_model <- glm(produces ~ age * swedish_emotionalarousal_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_emotionalarousal_interaction_model <- glm(produces ~ age * swedish_emotionalarousal_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_emotionalarousal_interaction_summary <- summary(swedish_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_emotionalarousal_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_emotionalarousal_model <- glm(as.factor(produces) ~ age + arabic_emotionalarousal_rating + lexical_category, 
                                 data = arabic_instrument_data, family = "binomial")
arabic_emotionalarousal_effect <- ggpredict(arabic_emotionalarousal_model, terms = "arabic_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_emotionalarousal_model$coefficients[[3]])
arabic_emotionalarousal_summary <- summary(arabic_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_emotionalarousal_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * arabic_emotionalarousal_rating + lexical_category, 
                                             data = arabic_instrument_data, family = "binomial")
arabic_emotionalarousal_interaction_summary <- summary(arabic_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_emotionalarousal_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_emotionalarousal_model <- glm(as.factor(produces) ~ age + catalan_emotionalarousal_rating + lexical_category, 
                                  data = catalan_instrument_data, family = "binomial")
catalan_emotionalarousal_effect <- ggpredict(catalan_emotionalarousal_model, terms = "catalan_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_emotionalarousal_model$coefficients[[3]])
catalan_emotionalarousal_summary <- summary(catalan_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_emotionalarousal_rating") %>%
  mutate(language = "catalan") 
catalan_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * catalan_emotionalarousal_rating  + lexical_category, 
                                              data = catalan_instrument_data, family = "binomial")
catalan_emotionalarousal_interaction_summary <- summary(catalan_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_emotionalarousal_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_emotionalarousal_model <- glm(as.factor(produces) ~ age + estonian_emotionalarousal_rating + lexical_category, 
                                   data = estonian_instrument_data, family = "binomial")
estonian_emotionalarousal_effect <- ggpredict(estonian_emotionalarousal_model, terms = "estonian_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_emotionalarousal_model$coefficients[[3]])
estonian_emotionalarousal_summary <- summary(estonian_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_emotionalarousal_rating") %>%
  mutate(language = "estonian") 
estonian_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * estonian_emotionalarousal_rating  + lexical_category, 
                                               data = estonian_instrument_data, family = "binomial")
estonian_emotionalarousal_interaction_summary <- summary(estonian_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_emotionalarousal_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_emotionalarousal_model <- glm(as.factor(produces) ~ age + japanese_emotionalarousal_rating + lexical_category, 
                                   data = japanese_instrument_data, family = "binomial")
japanese_emotionalarousal_effect <- ggpredict(japanese_emotionalarousal_model, terms = "japanese_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_emotionalarousal_model$coefficients[[3]])
japanese_emotionalarousal_summary <- summary(japanese_emotionalarousal_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_emotionalarousal_rating") %>%
  mutate(language = "japanese") 
japanese_emotionalarousal_interaction_model <- glm(as.factor(produces) ~ age * japanese_emotionalarousal_rating  + lexical_category, 
                                               data = japanese_instrument_data, family = "binomial")
japanese_emotionalarousal_interaction_summary <- summary(japanese_emotionalarousal_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_emotionalarousal_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_emotionalarousal_model <- glm(produces ~ age + turkish_emotionalarousal_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_emotionalarousal_effect <- ggeffect(turkish_emotionalarousal_model, terms = "turkish_emotionalarousal_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_emotionalarousal_effects <- bind_rows(asl_emotionalarousal_effect,
                                      bsl_emotionalarousal_effect,
                                      chinese_beijing_emotionalarousal_effect,
                                      chinese_cantonese_emotionalarousal_effect,
                                      chinese_taiwanese_emotionalarousal_effect,
                                      croatian_emotionalarousal_effect,
                                      czech_emotionalarousal_effect,
                                      english_american_emotionalarousal_effect,
                                      english_australian_emotionalarousal_effect,
                                      english_british_emotionalarousal_effect,
                                      english_irish_emotionalarousal_effect,
                                      danish_emotionalarousal_effect,
                                      dutch_emotionalarousal_effect,
                                      italian_emotionalarousal_effect,
                                      finnish_emotionalarousal_effect,
                                      french_european_emotionalarousal_effect,
                                      french_quebecois_emotionalarousal_effect,
                                      german_emotionalarousal_effect,
                                      greek_emotionalarousal_effect,
                                      hebrew_emotionalarousal_effect,
                                      hungarian_emotionalarousal_effect,
                                      irish_emotionalarousal_effect,
                                      kiswahili_emotionalarousal_effect,
                                      korean_emotionalarousal_effect,
                                      latvian_emotionalarousal_effect,
                                      norwegian_emotionalarousal_effect,
                                      persian_emotionalarousal_effect,
                                      russian_emotionalarousal_effect,
                                      slovak_emotionalarousal_effect,
                                      spanish_argentinian_emotionalarousal_effect,
                                      spanish_chilean_emotionalarousal_effect,
                                      spanish_european_emotionalarousal_effect,
                                      spanish_mexican_emotionalarousal_effect,
                                      spanish_peruvian_emotionalarousal_effect,
                                      swedish_emotionalarousal_effect,
                                      arabic_emotionalarousal_effect,
                                      catalan_emotionalarousal_effect,
                                      estonian_emotionalarousal_effect,
                                      japanese_emotionalarousal_effect
                                      # , turkish_emotionalarousal_effect
)
write_rds(all_emotionalarousal_effects, "models/effects/all_emotionalarousal_effects.rds")

all_emotionalarousal_effects_plot <- ggplot(all_emotionalarousal_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "emotionalarousal Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_emotionalarousal_effects_plots.png", all_emotionalarousal_effects_plot, height = 8, width = 5)


all_emotionalarousal_summaries <- bind_rows(asl_emotionalarousal_summary,
                                        bsl_emotionalarousal_summary,
                                        chinese_beijing_emotionalarousal_summary,
                                        chinese_cantonese_emotionalarousal_summary,
                                        chinese_taiwanese_emotionalarousal_summary,
                                        croatian_emotionalarousal_summary,
                                        czech_emotionalarousal_summary,
                                        english_american_emotionalarousal_summary,
                                        english_australian_emotionalarousal_summary,
                                        english_british_emotionalarousal_summary,
                                        english_irish_emotionalarousal_summary,
                                        danish_emotionalarousal_summary,
                                        dutch_emotionalarousal_summary,
                                        italian_emotionalarousal_summary,
                                        finnish_emotionalarousal_summary,
                                        french_european_emotionalarousal_summary,
                                        french_quebecois_emotionalarousal_summary,
                                        german_emotionalarousal_summary,
                                        greek_emotionalarousal_summary,
                                        hebrew_emotionalarousal_summary,
                                        hungarian_emotionalarousal_summary,
                                        irish_emotionalarousal_summary,
                                        kiswahili_emotionalarousal_summary,
                                        korean_emotionalarousal_summary,
                                        latvian_emotionalarousal_summary,
                                        norwegian_emotionalarousal_summary,
                                        persian_emotionalarousal_summary,
                                        russian_emotionalarousal_summary,
                                        slovak_emotionalarousal_summary,
                                        spanish_argentinian_emotionalarousal_summary,
                                        spanish_chilean_emotionalarousal_summary,
                                        spanish_european_emotionalarousal_summary,
                                        spanish_mexican_emotionalarousal_summary,
                                        spanish_peruvian_emotionalarousal_summary,
                                        swedish_emotionalarousal_summary,
                                        arabic_emotionalarousal_summary,
                                        catalan_emotionalarousal_summary,
                                        estonian_emotionalarousal_summary,
                                        japanese_emotionalarousal_summary
                                        # , turkish_emotionalarousal_summary
) %>%
  mutate(variable = "Emotional Arousal",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_emotionalarousal_summaries, "models/effects/all_emotionalarousal_summaries.rds")



all_emotionalarousal_interaction_summaries <- bind_rows(asl_emotionalarousal_interaction_summary,
                                                    bsl_emotionalarousal_interaction_summary,
                                                    chinese_beijing_emotionalarousal_interaction_summary,
                                                    chinese_cantonese_emotionalarousal_interaction_summary,
                                                    chinese_taiwanese_emotionalarousal_interaction_summary,
                                                    croatian_emotionalarousal_interaction_summary,
                                                    czech_emotionalarousal_interaction_summary,
                                                    english_american_emotionalarousal_interaction_summary,
                                                    english_australian_emotionalarousal_interaction_summary,
                                                    english_british_emotionalarousal_interaction_summary,
                                                    english_irish_emotionalarousal_interaction_summary,
                                                    danish_emotionalarousal_interaction_summary,
                                                    dutch_emotionalarousal_interaction_summary,
                                                    italian_emotionalarousal_interaction_summary,
                                                    finnish_emotionalarousal_interaction_summary,
                                                    french_european_emotionalarousal_interaction_summary,
                                                    french_quebecois_emotionalarousal_interaction_summary,
                                                    german_emotionalarousal_interaction_summary,
                                                    greek_emotionalarousal_interaction_summary,
                                                    hebrew_emotionalarousal_interaction_summary,
                                                    hungarian_emotionalarousal_interaction_summary,
                                                    irish_emotionalarousal_interaction_summary,
                                                    kiswahili_emotionalarousal_interaction_summary,
                                                    korean_emotionalarousal_interaction_summary,
                                                    latvian_emotionalarousal_interaction_summary,
                                                    norwegian_emotionalarousal_interaction_summary,
                                                    persian_emotionalarousal_interaction_summary,
                                                    russian_emotionalarousal_interaction_summary,
                                                    slovak_emotionalarousal_interaction_summary,
                                                    spanish_argentinian_emotionalarousal_interaction_summary,
                                                    spanish_chilean_emotionalarousal_interaction_summary,
                                                    spanish_european_emotionalarousal_interaction_summary,
                                                    spanish_mexican_emotionalarousal_interaction_summary,
                                                    spanish_peruvian_emotionalarousal_interaction_summary,
                                                    swedish_emotionalarousal_interaction_summary,
                                                    arabic_emotionalarousal_interaction_summary,
                                                    catalan_emotionalarousal_interaction_summary,
                                                    estonian_emotionalarousal_interaction_summary,
                                                    japanese_emotionalarousal_interaction_summary
                                                    # , turkish_emotionalarousal_summary
) %>%
  mutate(variable = "Emotional Arousal",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_emotionalarousal_interaction_summaries, "models/effects/all_emotionalarousal_interaction_summaries.rds")
