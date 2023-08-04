library(lme4)
library(ggeffects)
library(ggggeffects)

# iconicity
asl_instrument_data <- read_rds("norms/asl/asl_instrument_data.rds")
asl_iconicity_model <- glm(as.factor(produces) ~ age + asl_iconicity_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                          data = asl_instrument_data, family = "binomial")
asl_iconicity_effect <- ggpredict(asl_iconicity_model, terms = "asl_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language")
asl_iconicity_summary <- summary(asl_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "asl_iconicity_rating") %>%
  mutate(language = "asl") 
asl_iconicity_interaction_model <- glm(as.factor(produces) ~ age * asl_iconicity_rating + asl_frequency_rating + asl_phoncomp_rating + lexical_category, 
                                       data = asl_instrument_data, family = "binomial")
asl_iconicity_interaction_summary <- summary(asl_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:asl_iconicity_rating") %>%
  mutate(language = "asl") 


american_english_instrument_data <- read_rds("norms/english/american_english_instrument_data.rds")
english_american_iconicity_model <- glm(produces ~ age + english_iconicity_rating + english_freq_rating + lexical_category + word_length, data = american_english_instrument_data, family = "binomial")
english_american_iconicity_effect <- ggeffect(english_american_iconicity_model, terms = "english_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (American)")
english_american_iconicity_summary <- summary(english_american_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "english_iconicity_rating") %>%
  mutate(language = "english_american")
english_american_iconicity_interaction_model <- glm(as.factor(produces) ~ age * english_iconicity_rating + english_freq_rating +  lexical_category + word_length, 
                                       data = american_english_instrument_data, family = "binomial")
english_american_iconicity_interaction_summary <- summary(english_american_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:english_iconicity_rating") %>%
  mutate(language = "english_american") 

australian_english_instrument_data <- read_rds("norms/english/australian_english_instrument_data.rds")
english_australian_iconicity_model <- glm(produces ~ age + english_iconicity_rating + english_freq_rating + lexical_category + word_length, data = australian_english_instrument_data, family = "binomial")
english_australian_iconicity_effect <- ggeffect(english_australian_iconicity_model, terms = "english_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Australian)")
english_australian_iconicity_summary <- summary(english_australian_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "english_iconicity_rating") %>%
  mutate(language = "english_australian")
english_australian_iconicity_interaction_model <- glm(as.factor(produces) ~ age * english_iconicity_rating + english_freq_rating +  lexical_category + word_length, 
                                                    data = australian_english_instrument_data, family = "binomial")
english_australian_iconicity_interaction_summary <- summary(english_australian_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:english_iconicity_rating") %>%
  mutate(language = "english_australian") 

british_english_instrument_data <- read_rds("norms/english/british_english_instrument_data.rds")
english_british_iconicity_model <- glm(produces ~ age + english_iconicity_rating + english_freq_rating + lexical_category + word_length, data = british_english_instrument_data, family = "binomial")
english_british_iconicity_effect <- ggeffect(english_british_iconicity_model, terms = "english_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (British)")
english_british_iconicity_summary <- summary(english_british_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "english_iconicity_rating") %>%
  mutate(language = "english_british")
english_british_iconicity_interaction_model <- glm(as.factor(produces) ~ age * english_iconicity_rating + english_freq_rating +  lexical_category + word_length, 
                                                    data = british_english_instrument_data, family = "binomial")
english_british_iconicity_interaction_summary <- summary(english_british_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:english_iconicity_rating") %>%
  mutate(language = "english_british") 

irish_english_instrument_data <- read_rds("norms/english/irish_english_instrument_data.rds")
english_irish_iconicity_model <- glm(produces ~ age + english_iconicity_rating + english_freq_rating + lexical_category + word_length, data = irish_english_instrument_data, family = "binomial")
english_irish_iconicity_effect <- ggeffect(english_irish_iconicity_model, terms = "english_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "English (Irish)")
english_irish_iconicity_summary <- summary(english_irish_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "english_iconicity_rating") %>%
  mutate(language = "english_irish")
english_irish_iconicity_interaction_model <- glm(as.factor(produces) ~ age * english_iconicity_rating + english_freq_rating +  lexical_category + word_length, 
                                                    data = irish_english_instrument_data, family = "binomial")
english_irish_iconicity_interaction_summary <- summary(english_irish_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:english_iconicity_rating") %>%
  mutate(language = "english_irish") 
# bsl_iconicity_model <- glm(as.factor(produces) ~ age + bsl_iconicity_rating, data = bsl_instrument_data, family = "binomial")
# bsl_iconicity_effect <- ggpredict(bsl_iconicity_model, terms = "bsl_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "British Sign Language")

spanish_argentinian_instrument_data <- read_rds("norms/spanish/spanish_argentinian_instrument_data.rds")
spanish_argentinian_iconicity_model <- glm(produces ~ age + spanish_iconicity_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_iconicity_effect <- ggeffect(spanish_argentinian_iconicity_model, terms = "spanish_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Argentinian)")
spanish_argentinian_iconicity_summary <- summary(spanish_argentinian_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_iconicity_rating") %>%
  mutate(language = "spanish_argentinian")
spanish_argentinian_iconicity_interaction_model <- glm(as.factor(produces) ~ age * spanish_iconicity_rating + spanish_freq_rating +  lexical_category + word_length, 
                                                    data = spanish_argentinian_instrument_data, family = "binomial")
spanish_argentinian_iconicity_interaction_summary <- summary(spanish_argentinian_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_iconicity_rating") %>%
  mutate(language = "spanish_argentinian") 

spanish_chilean_instrument_data <- read_rds("norms/spanish/spanish_chilean_instrument_data.rds")
spanish_chilean_iconicity_model <- glm(produces ~ age + spanish_iconicity_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_iconicity_effect <- ggeffect(spanish_chilean_iconicity_model, terms = "spanish_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Chilean)")
spanish_chilean_iconicity_summary <- summary(spanish_chilean_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_iconicity_rating") %>%
  mutate(language = "spanish_chilean")
spanish_chilean_iconicity_interaction_model <- glm(as.factor(produces) ~ age * spanish_iconicity_rating + spanish_freq_rating +  lexical_category + word_length, 
                                                       data = spanish_chilean_instrument_data, family = "binomial")
spanish_chilean_iconicity_interaction_summary <- summary(spanish_chilean_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_iconicity_rating") %>%
  mutate(language = "spanish_chilean") 

spanish_european_instrument_data <- read_rds("norms/spanish/spanish_european_instrument_data.rds")
spanish_european_iconicity_model <- glm(produces ~ age + spanish_iconicity_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_european_instrument_data, family = "binomial")
spanish_european_iconicity_effect <- ggeffect(spanish_european_iconicity_model, terms = "spanish_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (European)")
spanish_european_iconicity_summary <- summary(spanish_european_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_iconicity_rating") %>%
  mutate(language = "spanish_european")
spanish_european_iconicity_interaction_model <- glm(as.factor(produces) ~ age * spanish_iconicity_rating + spanish_freq_rating +  lexical_category + word_length, 
                                                       data = spanish_european_instrument_data, family = "binomial")
spanish_european_iconicity_interaction_summary <- summary(spanish_european_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_iconicity_rating") %>%
  mutate(language = "spanish_european") 

spanish_mexican_instrument_data <- read_rds("norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_iconicity_model <- glm(produces ~ age + spanish_iconicity_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_iconicity_effect <- ggeffect(spanish_mexican_iconicity_model, terms = "spanish_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Mexican)")
spanish_mexican_iconicity_summary <- summary(spanish_mexican_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_iconicity_rating") %>%
  mutate(language = "spanish_mexican")
spanish_mexican_iconicity_interaction_model <- glm(as.factor(produces) ~ age * spanish_iconicity_rating + spanish_freq_rating +  lexical_category + word_length, 
                                                       data = spanish_mexican_instrument_data, family = "binomial")
spanish_mexican_iconicity_interaction_summary <- summary(spanish_mexican_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_iconicity_rating") %>%
  mutate(language = "spanish_mexican") 

spanish_peruvian_instrument_data <- read_rds("norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_iconicity_model <- glm(produces ~ age + spanish_iconicity_rating + spanish_freq_rating + lexical_category + word_length, data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_iconicity_effect <- ggeffect(spanish_peruvian_iconicity_model, terms = "spanish_iconicity_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Spanish (Peruvian)")
spanish_peruvian_iconicity_summary <- summary(spanish_peruvian_iconicity_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "spanish_iconicity_rating") %>%
  mutate(language = "spanish_peruvian")
spanish_peruvian_iconicity_interaction_model <- glm(as.factor(produces) ~ age * spanish_iconicity_rating + spanish_freq_rating +  lexical_category + word_length, 
                                                       data = spanish_peruvian_instrument_data, family = "binomial")
spanish_peruvian_iconicity_interaction_summary <- summary(spanish_peruvian_iconicity_interaction_model)$coefficients %>% 
  as.data.frame() %>%
  filter(row.names(.) == "age:spanish_iconicity_rating") %>%
  mutate(language = "spanish_peruvian") 

all_iconicity_effects <- bind_rows(asl_iconicity_effect,
                                  # bsl_iconicity_effect,
                                  english_american_iconicity_effect,
                                  english_australian_iconicity_effect,
                                  english_british_iconicity_effect,
                                  english_irish_iconicity_effect,
                                  spanish_argentinian_iconicity_effect,
                                  spanish_chilean_iconicity_effect,
                                  spanish_european_iconicity_effect,
                                  spanish_mexican_iconicity_effect,
                                  spanish_peruvian_iconicity_effect
)

all_iconicity_effects_plot <- ggplot(all_iconicity_effects)  + 
  geom_smooth(size = 1, aes(color=language, fill=language, x=x, y=predicted)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Iconicity Rating") +
  theme_classic()
ggsave("models/plots/all_iconicity_effects_plots.png", all_iconicity_effects_plot, height = 8, width = 8)


all_iconicity_summaries <- bind_rows(asl_iconicity_summary,
                                   # bsl_iconicity_summary,
                                   english_american_iconicity_summary,
                                   english_australian_iconicity_summary,
                                   english_british_iconicity_summary,
                                   english_irish_iconicity_summary,
                                   spanish_argentinian_iconicity_summary,
                                   spanish_chilean_iconicity_summary,
                                   spanish_european_iconicity_summary,
                                   spanish_mexican_iconicity_summary,
                                   spanish_peruvian_iconicity_summary
) %>%
  mutate(variable = "iconicity",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 
write_rds(all_iconicity_summaries, "models/effects/all_iconicity_summaries.rds")


all_iconicity_interaction_summaries <- bind_rows(asl_iconicity_interaction_summary,
                                     # bsl_iconicity_summary,
                                     english_american_iconicity_interaction_summary,
                                     english_australian_iconicity_interaction_summary,
                                     english_british_iconicity_interaction_summary,
                                     english_irish_iconicity_interaction_summary,
                                     spanish_argentinian_iconicity_interaction_summary,
                                     spanish_chilean_iconicity_interaction_summary,
                                     spanish_european_iconicity_interaction_summary,
                                     spanish_mexican_iconicity_interaction_summary,
                                     spanish_peruvian_iconicity_interaction_summary
) %>%
  mutate(variable = "age_iconicity",
         significant = case_when(`Pr(>|z|)` < .05 ~ "significant",
                                 TRUE ~ "ns")) 
write_rds(all_iconicity_interaction_summaries, "models/effects/all_iconicity_interaction_summaries.rds")
