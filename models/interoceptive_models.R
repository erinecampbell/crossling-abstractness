library(lme4)
library(ggeffects)
library(ggggeffects)
library(readr)
library(tidyverse)


# interoceptive
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_interoceptive_model <- glm(as.factor(produces) ~ age + asl_interoceptive_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                        data = asl_instrument_data, family = "binomial")
asl_interoceptive_effect <- ggpredict(asl_interoceptive_model, terms = "asl_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = asl_interoceptive_model$coefficients[[3]])
asl_interoceptive_summary <- summary(asl_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_interoceptive_rating") %>%
  mutate(language = "asl") 
asl_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * asl_interoceptive_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                    data = asl_instrument_data, family = "binomial")
asl_interoceptive_interaction_summary <- summary(asl_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_interoceptive_rating") %>%
  mutate(language = "asl") 

bsl_instrument_data <- read_rds("norms/bsl/bsl_instrument_data.rds")
bsl_interoceptive_model <- glm(as.factor(produces) ~ age + bsl_interoceptive_rating + lexical_category, data = bsl_instrument_data, family = "binomial")
bsl_interoceptive_effect <- ggpredict(bsl_interoceptive_model, terms = "bsl_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language",
         variable_coefficient = bsl_interoceptive_model$coefficients[[3]]) 
bsl_interoceptive_summary <- summary(bsl_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "bsl_interoceptive_rating") %>%
  mutate(language = "bsl")
bsl_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * bsl_interoceptive_rating + lexical_category, 
                                    data = bsl_instrument_data, family = "binomial")
bsl_interoceptive_interaction_summary <- summary(bsl_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:bsl_interoceptive_rating") %>%
  mutate(language = "bsl") 

mandarin_beijing_instrument_data <- read_rds("norms/chinese/mandarin_beijing_instrument_data.rds")
chinese_beijing_interoceptive_model <- glm(produces ~ age + chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_interoceptive_effect <- ggeffect(chinese_beijing_interoceptive_model, terms = "chinese_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Beijing)",
         variable_coefficient = chinese_beijing_interoceptive_model$coefficients[[3]])
chinese_beijing_interoceptive_summary <- summary(chinese_beijing_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_interoceptive_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_beijing_interoceptive_interaction_model <- glm(produces ~ age * chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = mandarin_beijing_instrument_data, family = "binomial")
chinese_beijing_interoceptive_interaction_summary <- summary(chinese_beijing_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_interoceptive_rating") %>%
  mutate(language = "chinese_beijing",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

cantonese_instrument_data <- read_rds("norms/chinese/cantonese_instrument_data.rds")
chinese_cantonese_interoceptive_model <- glm(produces ~ age + chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_interoceptive_effect <- ggeffect(chinese_cantonese_interoceptive_model, terms = "chinese_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Cantonese",
         variable_coefficient = chinese_cantonese_interoceptive_model$coefficients[[3]])
chinese_cantonese_interoceptive_summary <- summary(chinese_cantonese_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_interoceptive_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_cantonese_interoceptive_interaction_model <- glm(produces ~ age * chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = cantonese_instrument_data, family = "binomial")
chinese_cantonese_interoceptive_interaction_summary <- summary(chinese_cantonese_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_interoceptive_rating") %>%
  mutate(language = "chinese_cantonese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

mandarin_taiwanese_instrument_data <- read_rds("norms/chinese/mandarin_taiwanese_instrument_data.rds")
chinese_taiwanese_interoceptive_model <- glm(produces ~ age + chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_interoceptive_effect <- ggeffect(chinese_taiwanese_interoceptive_model, terms = "chinese_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Mandarin (Taiwanese)",
         variable_coefficient = chinese_taiwanese_interoceptive_model$coefficients[[3]])
chinese_taiwanese_interoceptive_summary <- summary(chinese_taiwanese_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "chinese_interoceptive_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
chinese_taiwanese_interoceptive_interaction_model <- glm(produces ~ age * chinese_interoceptive_rating + chinese_freq_rating + lexical_category, data = mandarin_taiwanese_instrument_data, family = "binomial")
chinese_taiwanese_interoceptive_interaction_summary <- summary(chinese_taiwanese_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:chinese_interoceptive_rating") %>%
  mutate(language = "chinese_taiwanese",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

croatian_instrument_data <- read_rds("norms/croatian/croatian_instrument_data.rds")
croatian_interoceptive_model <- glm(as.factor(produces) ~ age + croatian_interoceptive_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_interoceptive_effect <- ggpredict(croatian_interoceptive_model, terms = "croatian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian",
         variable_coefficient = croatian_interoceptive_model$coefficients[[3]])
croatian_interoceptive_summary <- summary(croatian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "croatian_interoceptive_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
croatian_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * croatian_interoceptive_rating + croatian_frequency_rating + lexical_category + word_length, data = croatian_instrument_data, family = "binomial")
croatian_interoceptive_interaction_summary <- summary(croatian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:croatian_interoceptive_rating") %>%
  mutate(language = "croatian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

czech_instrument_data <- read_rds("norms/czech/czech_instrument_data.rds")
czech_interoceptive_model <- glm(as.factor(produces) ~ age + czech_interoceptive_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_interoceptive_effect <- ggpredict(czech_interoceptive_model, terms = "czech_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech",
         variable_coefficient = czech_interoceptive_model$coefficients[[3]])
czech_interoceptive_summary <- summary(czech_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "czech_interoceptive_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
czech_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * czech_interoceptive_rating + czech_freq_rating + lexical_category + word_length, data = czech_instrument_data, family = "binomial")
czech_interoceptive_interaction_summary <- summary(czech_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:czech_interoceptive_rating") %>%
  mutate(language = "czech",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

danish_instrument_data <- read_rds("norms/danish/danish_instrument_data.rds")
danish_interoceptive_model <- glm(as.factor(produces) ~ age + danish_interoceptive_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_interoceptive_effect <- ggpredict(danish_interoceptive_model, terms = "danish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish",
         variable_coefficient = danish_interoceptive_model$coefficients[[3]])
danish_interoceptive_summary <- summary(danish_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "danish_interoceptive_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
danish_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * danish_interoceptive_rating + danish_freq_rating + lexical_category + word_length, data = danish_instrument_data, family = "binomial")
danish_interoceptive_interaction_summary <- summary(danish_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:danish_interoceptive_rating") %>%
  mutate(language = "danish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

dutch_instrument_data <- read_rds("norms/dutch/dutch_instrument_data.rds")
dutch_interoceptive_model <- glm(as.factor(produces) ~ age + dutch_interoceptive_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_interoceptive_effect <- ggpredict(dutch_interoceptive_model, terms = "dutch_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch",
         variable_coefficient = dutch_interoceptive_model$coefficients[[3]])  
dutch_interoceptive_summary <- summary(dutch_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "dutch_interoceptive_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
dutch_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * dutch_interoceptive_rating + dutch_freq_rating + lexical_category + word_length, data = dutch_instrument_data, family = "binomial")
dutch_interoceptive_interaction_summary <- summary(dutch_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:dutch_interoceptive_rating") %>%
  mutate(language = "dutch",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_interoceptive_model <- glm(produces ~ age + english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_interoceptive_effect <- ggeffect(english_american_interoceptive_model, terms = "english_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)",
         variable_coefficient = english_american_interoceptive_model$coefficients[[3]])
english_american_interoceptive_summary <- summary(english_american_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_interoceptive_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_american_interoceptive_interaction_model <- glm(produces ~ age * english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_interoceptive_interaction_summary <- summary(english_american_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_interoceptive_rating") %>%
  mutate(language = "english_american",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_interoceptive_model <- glm(produces ~ age + english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_interoceptive_effect <- ggeffect(english_australian_interoceptive_model, terms = "english_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)",
         variable_coefficient = english_australian_interoceptive_model$coefficients[[3]])
english_australian_interoceptive_summary <- summary(english_australian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_interoceptive_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_australian_interoceptive_interaction_model <- glm(produces ~ age * english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_interoceptive_interaction_summary <- summary(english_australian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_interoceptive_rating") %>%
  mutate(language = "english_australian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_interoceptive_model <- glm(produces ~ age + english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_interoceptive_effect <- ggeffect(english_british_interoceptive_model, terms = "english_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)",
         variable_coefficient = english_british_interoceptive_model$coefficients[[3]])
english_british_interoceptive_summary <- summary(english_british_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_interoceptive_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_british_interoceptive_interaction_model <- glm(produces ~ age * english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_interoceptive_interaction_summary <- summary(english_british_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_interoceptive_rating") %>%
  mutate(language = "english_british",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_interoceptive_model <- glm(produces ~ age + english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_interoceptive_effect <- ggeffect(english_irish_interoceptive_model, terms = "english_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)",
         variable_coefficient = english_irish_interoceptive_model$coefficients[[3]])
english_irish_interoceptive_summary <- summary(english_irish_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "english_interoceptive_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
english_irish_interoceptive_interaction_model <- glm(produces ~ age * english_interoceptive_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_interoceptive_interaction_summary <- summary(english_irish_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:english_interoceptive_rating") %>%
  mutate(language = "english_irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

finnish_instrument_data <- read_rds("norms/finnish/finnish_instrument_data.rds")
finnish_interoceptive_model <- glm(as.factor(produces) ~ age + finnish_interoceptive_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_interoceptive_effect <- ggpredict(finnish_interoceptive_model, terms = "finnish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish",
         variable_coefficient = finnish_interoceptive_model$coefficients[[3]]) 
finnish_interoceptive_summary <- summary(finnish_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "finnish_interoceptive_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
finnish_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * finnish_interoceptive_rating + finnish_freq_rating + lexical_category + word_length, data = finnish_instrument_data, family = "binomial")
finnish_interoceptive_interaction_summary <- summary(finnish_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:finnish_interoceptive_interaction_rating") %>%
  mutate(language = "finnish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

french_european_instrument_data <- read_rds("norms/french/french_european_instrument_data.rds")
french_european_interoceptive_model <- glm(as.factor(produces) ~ age + french_interoceptive_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_interoceptive_effect <- ggpredict(french_european_interoceptive_model, terms = "french_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (European)",
         variable_coefficient = french_european_interoceptive_model$coefficients[[3]]) 
french_european_interoceptive_summary <- summary(french_european_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_interoceptive_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_european_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * french_interoceptive_rating + french_freq_rating + lexical_category + word_length, data = french_european_instrument_data, family = "binomial")
french_european_interoceptive_interaction_summary <- summary(french_european_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_interoceptive_rating") %>%
  mutate(language = "french_european",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)


french_quebecois_instrument_data <- read_rds("norms/french/french_quebecois_instrument_data.rds")
french_quebecois_interoceptive_model <- glm(as.factor(produces) ~ age + french_interoceptive_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_interoceptive_effect <- ggpredict(french_quebecois_interoceptive_model, terms = "french_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "French (Quebecois)",
         variable_coefficient = french_quebecois_interoceptive_model$coefficients[[3]]) 
french_quebecois_interoceptive_summary <- summary(french_quebecois_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "french_interoceptive_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
french_quebecois_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * french_interoceptive_rating + french_freq_rating + lexical_category + word_length, data = french_quebecois_instrument_data, family = "binomial")
french_quebecois_interoceptive_interaction_summary <- summary(french_quebecois_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:french_interoceptive_rating") %>%
  mutate(language = "french_quebecois",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

german_instrument_data <- read_rds("norms/german/german_instrument_data.rds")
german_interoceptive_model <- glm(as.factor(produces) ~ age + german_interoceptive_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_interoceptive_effect <- ggpredict(german_interoceptive_model, terms = "german_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German",
         variable_coefficient = german_interoceptive_model$coefficients[[3]]) 
german_interoceptive_summary <- summary(german_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "german_interoceptive_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
german_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * german_interoceptive_rating + german_freq_rating + lexical_category + word_length, data = as.data.frame(german_instrument_data), family = "binomial")
german_interoceptive_interaction_summary <- summary(german_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:german_interoceptive_rating") %>%
  mutate(language = "german",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

greek_instrument_data <- read_rds("norms/greek/greek_instrument_data.rds")
greek_interoceptive_model <- glm(as.factor(produces) ~ age + greek_interoceptive_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_interoceptive_effect <- ggpredict(greek_interoceptive_model, terms = "greek_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek",
         variable_coefficient = greek_interoceptive_model$coefficients[[3]]) 
greek_interoceptive_summary <- summary(greek_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "greek_interoceptive_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
greek_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * greek_interoceptive_rating + greek_freq_rating + lexical_category + word_length, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_interoceptive_interaction_summary <- summary(greek_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:greek_interoceptive_rating") %>%
  mutate(language = "greek",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hebrew_instrument_data <- read_rds("norms/hebrew/hebrew_instrument_data.rds")
hebrew_interoceptive_model <- glm(as.factor(produces) ~ age + hebrew_interoceptive_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_interoceptive_effect <- ggpredict(hebrew_interoceptive_model, terms = "hebrew_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew",
         variable_coefficient = hebrew_interoceptive_model$coefficients[[3]]) 
hebrew_interoceptive_summary <- summary(hebrew_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hebrew_interoceptive_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hebrew_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * hebrew_interoceptive_rating + hebrew_freq_rating + lexical_category + word_length, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_interoceptive_interaction_summary <- summary(hebrew_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hebrew_interoceptive_rating") %>%
  mutate(language = "hebrew",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

hungarian_instrument_data <- read_rds("norms/hungarian/hungarian_instrument_data.rds")
hungarian_interoceptive_model <- glm(as.factor(produces) ~ age + hungarian_interoceptive_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_interoceptive_effect <- ggpredict(hungarian_interoceptive_model, terms = "hungarian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian",
         variable_coefficient = hungarian_interoceptive_model$coefficients[[3]])
hungarian_interoceptive_summary <- summary(hungarian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "hungarian_interoceptive_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
hungarian_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * hungarian_interoceptive_rating + hungarian_freq_rating + lexical_category + word_length, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_interoceptive_interaction_summary <- summary(hungarian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:hungarian_interoceptive_rating") %>%
  mutate(language = "hungarian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

irish_instrument_data <- read_rds("norms/irish/irish_instrument_data.rds")
irish_interoceptive_model <- glm(as.factor(produces) ~ age + irish_interoceptive_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_interoceptive_effect <- ggpredict(irish_interoceptive_model, terms = "irish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish",
         variable_coefficient = irish_interoceptive_model$coefficients[[3]])
irish_interoceptive_summary <- summary(irish_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "irish_interoceptive_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
irish_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * irish_interoceptive_rating + irish_freq_rating + lexical_category + word_length, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_interoceptive_interaction_summary <- summary(irish_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:irish_interoceptive_rating") %>%
  mutate(language = "irish",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

italian_instrument_data <- read_rds("norms/italian/italian_instrument_data.rds")
italian_interoceptive_model <- glm(produces ~ age + italian_interoceptive_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_interoceptive_effect <- ggeffect(italian_interoceptive_model, terms = "italian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian",
         variable_coefficient = italian_interoceptive_model$coefficients[[3]])
italian_interoceptive_summary <- summary(italian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "italian_interoceptive_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
italian_interoceptive_interaction_model <- glm(produces ~ age * italian_interoceptive_rating + italian_freq_rating + lexical_category + word_length, data = italian_instrument_data, family = "binomial")
italian_interoceptive_interaction_summary <- summary(italian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:italian_interoceptive_rating") %>%
  mutate(language = "italian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

# kigiriama_interoceptive_model <- glm(produces ~ age + kigiriama_interoceptive_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_interoceptive_effect <- ggeffect(kigiriama_interoceptive_model, terms = "kigiriama_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_instrument_data <- read_rds("norms/kiswahili/kiswahili_instrument_data.rds")
kiswahili_interoceptive_model <- glm(produces ~ age + kiswahili_interoceptive_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_interoceptive_effect <- ggeffect(kiswahili_interoceptive_model, terms = "kiswahili_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili",
         variable_coefficient = kiswahili_interoceptive_model$coefficients[[3]])
kiswahili_interoceptive_summary <- summary(kiswahili_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "kiswahili_interoceptive_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
kiswahili_interoceptive_interaction_model <- glm(produces ~ age * kiswahili_interoceptive_rating + kiswahili_freq_rating + lexical_category + word_length, data = kiswahili_instrument_data, family = "binomial")
kiswahili_interoceptive_interaction_summary <- summary(kiswahili_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:kiswahili_interoceptive_rating") %>%
  mutate(language = "kiswahili",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

korean_instrument_data <- read_rds("norms/korean/korean_instrument_data.rds")
korean_interoceptive_model <- glm(produces ~ age + korean_interoceptive_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_interoceptive_effect <- ggeffect(korean_interoceptive_model, terms = "korean_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean",
         variable_coefficient = korean_interoceptive_model$coefficients[[3]])
korean_interoceptive_summary <- summary(korean_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "korean_interoceptive_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
korean_interoceptive_interaction_model <- glm(produces ~ age * korean_interoceptive_rating + korean_freq_rating + lexical_category, data = korean_instrument_data, family = "binomial")
korean_interoceptive_interaction_summary <- summary(korean_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:korean_interoceptive_rating") %>%
  mutate(language = "korean",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

latvian_instrument_data <- read_rds("norms/latvian/latvian_instrument_data.rds")
latvian_interoceptive_model <- glm(produces ~ age + latvian_interoceptive_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_interoceptive_effect <- ggeffect(latvian_interoceptive_model, terms = "latvian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian",
         variable_coefficient = latvian_interoceptive_model$coefficients[[3]])
latvian_interoceptive_summary <- summary(latvian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "latvian_interoceptive_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
latvian_interoceptive_interaction_model <- glm(produces ~ age * latvian_interoceptive_rating + latvian_freq_rating + lexical_category + word_length, data = latvian_instrument_data, family = "binomial")
latvian_interoceptive_interaction_summary <- summary(latvian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:latvian_interoceptive_rating") %>%
  mutate(language = "latvian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

norwegian_instrument_data <- read_rds("norms/norwegian/norwegian_instrument_data.rds")
norwegian_interoceptive_model <- glm(produces ~ age + norwegian_interoceptive_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_interoceptive_effect <- ggeffect(norwegian_interoceptive_model, terms = "norwegian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian",
         variable_coefficient = norwegian_interoceptive_model$coefficients[[3]])
norwegian_interoceptive_summary <- summary(norwegian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "norwegian_interoceptive_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
norwegian_interoceptive_interaction_model <- glm(produces ~ age * norwegian_interoceptive_rating + norwegian_freq_rating + lexical_category + word_length, data = norwegian_instrument_data, family = "binomial")
norwegian_interoceptive_interaction_summary <- summary(norwegian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:norwegian_interoceptive_rating") %>%
  mutate(language = "norwegian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

persian_instrument_data <- read_rds("norms/persian/persian_instrument_data.rds")
persian_interoceptive_model <- glm(produces ~ age + persian_interoceptive_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_interoceptive_effect <- ggeffect(persian_interoceptive_model, terms = "persian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Farsi",
         variable_coefficient = persian_interoceptive_model$coefficients[[3]])
persian_interoceptive_summary <- summary(persian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "persian_interoceptive_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
persian_interoceptive_interaction_model <- glm(produces ~ age * persian_interoceptive_rating + persian_freq_rating + lexical_category + word_length, data = persian_instrument_data, family = "binomial")
persian_interoceptive_interaction_summary <- summary(persian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:persian_interoceptive_rating") %>%
  mutate(language = "persian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
# portuguese_instrument_data <- read_rds("norms/portuguese/portuguese_instrument_data.rds")
# portuguese_interoceptive_model <- glm(produces ~ age + portuguese_interoceptive_rating + portuguese_freq_rating + lexical_category + word_length, data = portuguese_instrument_data, family = "binomial")
# portuguese_interoceptive_effect <- ggeffect(portuguese_interoceptive_model, terms = "portuguese_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Portuguese (European)")

russian_instrument_data <- read_rds("norms/russian/russian_instrument_data.rds")
russian_interoceptive_model <- glm(produces ~ age + russian_interoceptive_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_interoceptive_effect <- ggeffect(russian_interoceptive_model, terms = "russian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian",
         variable_coefficient = russian_interoceptive_model$coefficients[[3]])
russian_interoceptive_summary <- summary(russian_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "russian_interoceptive_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
russian_interoceptive_interaction_model <- glm(produces ~ age * russian_interoceptive_rating + russian_freq_rating + lexical_category + word_length, data = russian_instrument_data, family = "binomial")
russian_interoceptive_interaction_summary <- summary(russian_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:russian_interoceptive_rating") %>%
  mutate(language = "russian",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

slovak_instrument_data <- read_rds("norms/slovak/slovak_instrument_data.rds")
slovak_interoceptive_model <- glm(produces ~ age + slovak_interoceptive_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_interoceptive_effect <- ggeffect(slovak_interoceptive_model, terms = "slovak_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak",
         variable_coefficient = slovak_interoceptive_model$coefficients[[3]])
slovak_interoceptive_summary <- summary(slovak_interoceptive_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "slovak_interoceptive_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)
slovak_interoceptive_interaction_model <- glm(produces ~ age * slovak_interoceptive_rating + slovak_freq_rating + lexical_category + word_length, data = slovak_instrument_data, family = "binomial")
slovak_interoceptive_interaction_summary <- summary(slovak_interoceptive_interaction_model)$coefficients %>% as.data.frame() %>%
  filter(row.names(.) == "age:slovak_interoceptive_rating") %>%
  mutate(language = "slovak",
         effect_size = Estimate,
         standard_error = `Std. Error`,
         p_value = `Pr(>|z|)`)

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_interoceptive_model <- glm(produces ~ age + spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_interoceptive_effect <- ggeffect(spanish_argentinian_interoceptive_model, terms = "spanish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)",
         variable_coefficient = spanish_argentinian_interoceptive_model$coefficients[[3]])
spanish_argentinian_interoceptive_summary <- summary(spanish_argentinian_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_interoceptive_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_interoceptive_interaction_model <- glm(produces ~ age * spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_interoceptive_interaction_summary <- summary(spanish_argentinian_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_interoceptive_rating") %>%
  mutate(language = "spanish_argentinian")

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_interoceptive_model <- glm(produces ~ age + spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_interoceptive_effect <- ggeffect(spanish_chilean_interoceptive_model, terms = "spanish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)",
         variable_coefficient = spanish_chilean_interoceptive_model$coefficients[[3]])
spanish_chilean_interoceptive_summary <- summary(spanish_chilean_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_interoceptive_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_interoceptive_interaction_model <- glm(produces ~ age * spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_interoceptive_interaction_summary <- summary(spanish_chilean_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_interoceptive_rating") %>%
  mutate(language = "spanish_chilean")

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_interoceptive_model <- glm(produces ~ age + spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_interoceptive_effect <- ggeffect(spanish_european_interoceptive_model, terms = "spanish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)",
         variable_coefficient = spanish_european_interoceptive_model$coefficients[[3]])
spanish_european_interoceptive_summary <- summary(spanish_european_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_interoceptive_rating") %>%
  mutate(language = "spanish_european")
spanish_european_interoceptive_interaction_model <- glm(produces ~ age * spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_interoceptive_interaction_summary <- summary(spanish_european_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_interoceptive_rating") %>%
  mutate(language = "spanish_european")

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_interoceptive_model <- glm(produces ~ age + spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_interoceptive_effect <- ggeffect(spanish_mexican_interoceptive_model, terms = "spanish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)",
         variable_coefficient = spanish_mexican_interoceptive_model$coefficients[[3]])
spanish_mexican_interoceptive_summary <- summary(spanish_mexican_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_interoceptive_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_interoceptive_interaction_model <- glm(produces ~ age * spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_interoceptive_interaction_summary <- summary(spanish_mexican_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_interoceptive_rating") %>%
  mutate(language = "spanish_mexican")

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_interoceptive_model <- glm(produces ~ age + spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_interoceptive_effect <- ggeffect(spanish_peruvian_interoceptive_model, terms = "spanish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)",
         variable_coefficient = spanish_peruvian_interoceptive_model$coefficients[[3]])
spanish_peruvian_interoceptive_summary <- summary(spanish_peruvian_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_interoceptive_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_interoceptive_interaction_model <- glm(produces ~ age * spanish_interoceptive_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_interoceptive_interaction_summary <- summary(spanish_peruvian_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_interoceptive_rating") %>%
  mutate(language = "spanish_peruvian")

swedish_instrument_data <- read_rds("norms/swedish/swedish_instrument_data.rds")
swedish_interoceptive_model <- glm(produces ~ age + swedish_interoceptive_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_interoceptive_effect <- ggeffect(swedish_interoceptive_model, terms = "swedish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish",
         variable_coefficient = swedish_interoceptive_model$coefficients[[3]])
swedish_interoceptive_summary <- summary(swedish_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "swedish_interoceptive_rating") %>%
  mutate(language = "swedish")
swedish_interoceptive_interaction_model <- glm(produces ~ age * swedish_interoceptive_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_interoceptive_interaction_model <- glm(produces ~ age * swedish_interoceptive_rating + swedish_freq_rating + lexical_category + word_length, data = swedish_instrument_data, family = "binomial")
swedish_interoceptive_interaction_summary <- summary(swedish_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:swedish_interoceptive_rating") %>%
  mutate(language = "swedish")

arabic_instrument_data <- read_rds("norms/arabic/arabic_instrument_data.rds")
arabic_interoceptive_model <- glm(as.factor(produces) ~ age + arabic_interoceptive_rating + lexical_category, 
                           data = arabic_instrument_data, family = "binomial")
arabic_interoceptive_effect <- ggpredict(arabic_interoceptive_model, terms = "arabic_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Arabic (Saudi)",
         variable_coefficient = arabic_interoceptive_model$coefficients[[3]])
arabic_interoceptive_summary <- summary(arabic_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "arabic_interoceptive_rating") %>%
  mutate(language = "Arabic (Saudi)") 
arabic_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * arabic_interoceptive_rating + lexical_category, 
                                       data = arabic_instrument_data, family = "binomial")
arabic_interoceptive_interaction_summary <- summary(arabic_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:arabic_interoceptive_rating") %>%
  mutate(language = "Arabic (Saudi)") 

catalan_instrument_data <- read_rds("norms/catalan/catalan_instrument_data.rds")
catalan_interoceptive_model <- glm(as.factor(produces) ~ age + catalan_interoceptive_rating + lexical_category, 
                            data = catalan_instrument_data, family = "binomial")
catalan_interoceptive_effect <- ggpredict(catalan_interoceptive_model, terms = "catalan_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = catalan_interoceptive_model$coefficients[[3]])
catalan_interoceptive_summary <- summary(catalan_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "catalan_interoceptive_rating") %>%
  mutate(language = "catalan") 
catalan_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * catalan_interoceptive_rating  + lexical_category, 
                                        data = catalan_instrument_data, family = "binomial")
catalan_interoceptive_interaction_summary <- summary(catalan_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:catalan_interoceptive_rating") %>%
  mutate(language = "catalan") 

estonian_instrument_data <- read_rds("norms/estonian/estonian_instrument_data.rds")
estonian_interoceptive_model <- glm(as.factor(produces) ~ age + estonian_interoceptive_rating + lexical_category, 
                             data = estonian_instrument_data, family = "binomial")
estonian_interoceptive_effect <- ggpredict(estonian_interoceptive_model, terms = "estonian_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = estonian_interoceptive_model$coefficients[[3]])
estonian_interoceptive_summary <- summary(estonian_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "estonian_interoceptive_rating") %>%
  mutate(language = "estonian") 
estonian_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * estonian_interoceptive_rating  + lexical_category, 
                                         data = estonian_instrument_data, family = "binomial")
estonian_interoceptive_interaction_summary <- summary(estonian_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:estonian_interoceptive_rating") %>%
  mutate(language = "estonian")

japanese_instrument_data <- read_rds("norms/japanese/japanese_instrument_data.rds")
japanese_interoceptive_model <- glm(as.factor(produces) ~ age + japanese_interoceptive_rating + lexical_category, 
                             data = japanese_instrument_data, family = "binomial")
japanese_interoceptive_effect <- ggpredict(japanese_interoceptive_model, terms = "japanese_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language",
         variable_coefficient = japanese_interoceptive_model$coefficients[[3]])
japanese_interoceptive_summary <- summary(japanese_interoceptive_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "japanese_interoceptive_rating") %>%
  mutate(language = "japanese") 
japanese_interoceptive_interaction_model <- glm(as.factor(produces) ~ age * japanese_interoceptive_rating  + lexical_category, 
                                         data = japanese_instrument_data, family = "binomial")
japanese_interoceptive_interaction_summary <- summary(japanese_interoceptive_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:japanese_interoceptive_rating") %>%
  mutate(language = "japanese")

# turkish_instrument_data <- read_rds("norms/turkish/turkish_instrument_data.rds")
# turkish_interoceptive_model <- glm(produces ~ age + turkish_interoceptive_rating + turkish_freq_rating + lexical_category + word_length, data = turkish_instrument_data, family = "binomial")
# turkish_interoceptive_effect <- ggeffect(turkish_interoceptive_model, terms = "turkish_interoceptive_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Turkish")

all_interoceptive_effects <- bind_rows(asl_interoceptive_effect,
                                bsl_interoceptive_effect,
                                chinese_beijing_interoceptive_effect,
                                chinese_cantonese_interoceptive_effect,
                                chinese_taiwanese_interoceptive_effect,
                                croatian_interoceptive_effect,
                                czech_interoceptive_effect,
                                english_american_interoceptive_effect,
                                english_australian_interoceptive_effect,
                                english_british_interoceptive_effect,
                                english_irish_interoceptive_effect,
                                danish_interoceptive_effect,
                                dutch_interoceptive_effect,
                                italian_interoceptive_effect,
                                finnish_interoceptive_effect,
                                french_european_interoceptive_effect,
                                french_quebecois_interoceptive_effect,
                                german_interoceptive_effect,
                                greek_interoceptive_effect,
                                hebrew_interoceptive_effect,
                                hungarian_interoceptive_effect,
                                irish_interoceptive_effect,
                                kiswahili_interoceptive_effect,
                                korean_interoceptive_effect,
                                latvian_interoceptive_effect,
                                norwegian_interoceptive_effect,
                                persian_interoceptive_effect,
                                russian_interoceptive_effect,
                                slovak_interoceptive_effect,
                                spanish_argentinian_interoceptive_effect,
                                spanish_chilean_interoceptive_effect,
                                spanish_european_interoceptive_effect,
                                spanish_mexican_interoceptive_effect,
                                spanish_peruvian_interoceptive_effect,
                                swedish_interoceptive_effect,
                                arabic_interoceptive_effect,
                                catalan_interoceptive_effect,
                                estonian_interoceptive_effect,
                                japanese_interoceptive_effect
                                # , turkish_interoceptive_effect
)
write_rds(all_interoceptive_effects, "models/effects/all_interoceptive_effects.rds")

all_interoceptive_effects_plot <- ggplot(all_interoceptive_effects %>% filter(language!="Kiswahili"))  + 
  geom_smooth(size = 1, aes(x=x, y=predicted,color=language)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "interoceptive Rating") +
  theme_classic()+
  theme(legend.position = "none", text=element_text(size=18))
ggsave("models/plots/all_interoceptive_effects_plots.png", all_interoceptive_effects_plot, height = 8, width = 5)


all_interoceptive_summaries <- bind_rows(asl_interoceptive_summary,
                                  bsl_interoceptive_summary,
                                  chinese_beijing_interoceptive_summary,
                                  chinese_cantonese_interoceptive_summary,
                                  chinese_taiwanese_interoceptive_summary,
                                  croatian_interoceptive_summary,
                                  czech_interoceptive_summary,
                                  english_american_interoceptive_summary,
                                  english_australian_interoceptive_summary,
                                  english_british_interoceptive_summary,
                                  english_irish_interoceptive_summary,
                                  danish_interoceptive_summary,
                                  dutch_interoceptive_summary,
                                  italian_interoceptive_summary,
                                  finnish_interoceptive_summary,
                                  french_european_interoceptive_summary,
                                  french_quebecois_interoceptive_summary,
                                  german_interoceptive_summary,
                                  greek_interoceptive_summary,
                                  hebrew_interoceptive_summary,
                                  hungarian_interoceptive_summary,
                                  irish_interoceptive_summary,
                                  kiswahili_interoceptive_summary,
                                  korean_interoceptive_summary,
                                  latvian_interoceptive_summary,
                                  norwegian_interoceptive_summary,
                                  persian_interoceptive_summary,
                                  russian_interoceptive_summary,
                                  slovak_interoceptive_summary,
                                  spanish_argentinian_interoceptive_summary,
                                  spanish_chilean_interoceptive_summary,
                                  spanish_european_interoceptive_summary,
                                  spanish_mexican_interoceptive_summary,
                                  spanish_peruvian_interoceptive_summary,
                                  swedish_interoceptive_summary,
                                  arabic_interoceptive_summary,
                                  catalan_interoceptive_summary,
                                  estonian_interoceptive_summary,
                                  japanese_interoceptive_summary
                                  # , turkish_interoceptive_summary
) %>%
  mutate(variable = "interoceptive",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_interoceptive_summaries, "models/effects/all_interoceptive_summaries.rds")



all_interoceptive_interaction_summaries <- bind_rows(asl_interoceptive_interaction_summary,
                                              bsl_interoceptive_interaction_summary,
                                              chinese_beijing_interoceptive_interaction_summary,
                                              chinese_cantonese_interoceptive_interaction_summary,
                                              chinese_taiwanese_interoceptive_interaction_summary,
                                              croatian_interoceptive_interaction_summary,
                                              czech_interoceptive_interaction_summary,
                                              english_american_interoceptive_interaction_summary,
                                              english_australian_interoceptive_interaction_summary,
                                              english_british_interoceptive_interaction_summary,
                                              english_irish_interoceptive_interaction_summary,
                                              danish_interoceptive_interaction_summary,
                                              dutch_interoceptive_interaction_summary,
                                              italian_interoceptive_interaction_summary,
                                              finnish_interoceptive_interaction_summary,
                                              french_european_interoceptive_interaction_summary,
                                              french_quebecois_interoceptive_interaction_summary,
                                              german_interoceptive_interaction_summary,
                                              greek_interoceptive_interaction_summary,
                                              hebrew_interoceptive_interaction_summary,
                                              hungarian_interoceptive_interaction_summary,
                                              irish_interoceptive_interaction_summary,
                                              kiswahili_interoceptive_interaction_summary,
                                              korean_interoceptive_interaction_summary,
                                              latvian_interoceptive_interaction_summary,
                                              norwegian_interoceptive_interaction_summary,
                                              persian_interoceptive_interaction_summary,
                                              russian_interoceptive_interaction_summary,
                                              slovak_interoceptive_interaction_summary,
                                              spanish_argentinian_interoceptive_interaction_summary,
                                              spanish_chilean_interoceptive_interaction_summary,
                                              spanish_european_interoceptive_interaction_summary,
                                              spanish_mexican_interoceptive_interaction_summary,
                                              spanish_peruvian_interoceptive_interaction_summary,
                                              swedish_interoceptive_interaction_summary,
                                              arabic_interoceptive_interaction_summary,
                                              catalan_interoceptive_interaction_summary,
                                              estonian_interoceptive_interaction_summary,
                                              japanese_interoceptive_interaction_summary
                                              # , turkish_interoceptive_summary
) %>%
  mutate(variable = "interoceptive",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 

write_rds(all_interoceptive_interaction_summaries, "models/effects/all_interoceptive_interaction_summaries.rds")
