library(lme4)
library(ggeffects)
library(ggggeffects)


# concreteness
asl_concreteness_model <- glm(as.factor(produces) ~ age + asl_concreteness_rating + asl_frequency_rating, data = asl_instrument_data, family = "binomial")
asl_concreteness_effect <- ggpredict(asl_concreteness_model, terms = "asl_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language")

bsl_concreteness_model <- glm(as.factor(produces) ~ age + bsl_concreteness_rating, data = bsl_instrument_data, family = "binomial")
bsl_concreteness_effect <- ggpredict(bsl_concreteness_model, terms = "bsl_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language")

croatian_concreteness_model <- glm(as.factor(produces) ~ age + croatian_concreteness_rating, data = croatian_instrument_data, family = "binomial")
croatian_concreteness_effect <- ggpredict(croatian_concreteness_model, terms = "croatian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian")

czech_concreteness_model <- glm(as.factor(produces) ~ age + czech_concreteness_rating, data = czech_instrument_data, family = "binomial")
czech_concreteness_effect <- ggpredict(czech_concreteness_model, terms = "czech_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech")

danish_concreteness_model <- glm(as.factor(produces) ~ age + danish_concreteness_rating, data = danish_instrument_data, family = "binomial")
danish_concreteness_effect <- ggpredict(danish_concreteness_model, terms = "danish_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish")

dutch_concreteness_model <- glm(as.factor(produces) ~ age + dutch_concreteness_rating, data = dutch_instrument_data, family = "binomial")
dutch_concreteness_effect <- ggpredict(dutch_concreteness_model, terms = "dutch_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch")                                                                                                                                                                                                                                                                                                

finnish_concreteness_model <- glm(as.factor(produces) ~ age + finnish_concreteness_rating, data = finnish_instrument_data, family = "binomial")
finnish_concreteness_effect <- ggpredict(finnish_concreteness_model, terms = "finnish_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish") 

german_concreteness_model <- glm(as.factor(produces) ~ age + german_concreteness_rating, data = as.data.frame(german_instrument_data), family = "binomial")
german_concreteness_effect <- ggpredict(german_concreteness_model, terms = "german_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German") 

greek_concreteness_model <- glm(as.factor(produces) ~ age + greek_concreteness_rating, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_concreteness_effect <- ggpredict(greek_concreteness_model, terms = "greek_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek") 

hebrew_concreteness_model <- glm(as.factor(produces) ~ age + hebrew_concreteness_rating, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_concreteness_effect <- ggpredict(hebrew_concreteness_model, terms = "hebrew_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew") 

hungarian_concreteness_model <- glm(as.factor(produces) ~ age + hungarian_concreteness_rating, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_concreteness_effect <- ggpredict(hungarian_concreteness_model, terms = "hungarian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian")

irish_concreteness_model <- glm(as.factor(produces) ~ age + irish_concreteness_rating, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_concreteness_effect <- ggpredict(irish_concreteness_model, terms = "irish_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish")

italian_concreteness_model <- glm(produces ~ age + italian_concreteness_rating, data = italian_instrument_data, family = "binomial")
italian_concreteness_effect <- ggeffect(italian_concreteness_model, terms = "italian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian")

# kigiriama_concreteness_model <- glm(produces ~ age + kigiriama_concreteness_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_concreteness_effect <- ggeffect(kigiriama_concreteness_model, terms = "kigiriama_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_concreteness_model <- glm(produces ~ age + kiswahili_concreteness_rating, data = kiswahili_instrument_data, family = "binomial")
kiswahili_concreteness_effect <- ggeffect(kiswahili_concreteness_model, terms = "kiswahili_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili")

korean_concreteness_model <- glm(produces ~ age + korean_concreteness_rating, data = korean_instrument_data, family = "binomial")
korean_concreteness_effect <- ggeffect(korean_concreteness_model, terms = "korean_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean")

latvian_concreteness_model <- glm(produces ~ age + latvian_concreteness_rating, data = latvian_instrument_data, family = "binomial")
latvian_concreteness_effect <- ggeffect(latvian_concreteness_model, terms = "latvian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian")

norwegian_concreteness_model <- glm(produces ~ age + norwegian_concreteness_rating, data = norwegian_instrument_data, family = "binomial")
norwegian_concreteness_effect <- ggeffect(norwegian_concreteness_model, terms = "norwegian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian")

persian_concreteness_model <- glm(produces ~ age + persian_concreteness_rating, data = persian_instrument_data, family = "binomial")
persian_concreteness_effect <- ggeffect(persian_concreteness_model, terms = "persian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Persian")

russian_concreteness_model <- glm(produces ~ age + russian_concreteness_rating, data = russian_instrument_data, family = "binomial")
russian_concreteness_effect <- ggeffect(russian_concreteness_model, terms = "russian_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian")

slovak_concreteness_model <- glm(produces ~ age + slovak_concreteness_rating, data = slovak_instrument_data, family = "binomial")
slovak_concreteness_effect <- ggeffect(slovak_concreteness_model, terms = "slovak_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak")

swedish_concreteness_model <- glm(produces ~ age + swedish_concreteness_rating, data = swedish_instrument_data, family = "binomial")
swedish_concreteness_effect <- ggeffect(swedish_concreteness_model, terms = "swedish_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish")

turkish_concreteness_model <- glm(produces ~ age + turkish_concreteness_rating, data = turkish_instrument_data, family = "binomial")
turkish_concreteness_effect <- ggeffect(turkish_concreteness_model, terms = "turkish_concreteness_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Turkish")

all_concreteness_effects <- bind_rows(asl_concreteness_effect,
                             bsl_concreteness_effect,
                             croatian_concreteness_effect,
                             czech_concreteness_effect,
                             danish_concreteness_effect,
                             dutch_concreteness_effect,
                             italian_concreteness_effect,
                             finnish_concreteness_effect,
                             german_concreteness_effect,
                             greek_concreteness_effect,
                             hebrew_concreteness_effect,
                             hungarian_concreteness_effect,
                             irish_concreteness_effect,
                             kiswahili_concreteness_effect,
                             korean_concreteness_effect,
                             latvian_concreteness_effect,
                             norwegian_concreteness_effect,
                             persian_concreteness_effect,
                             russian_concreteness_effect,
                             slovak_concreteness_effect,
                             swedish_concreteness_effect,
                             turkish_concreteness_effect
                             )

ggplot(all_concreteness_effects)  + 
  geom_smooth(size = 1, aes(color=language, fill=language, x=x, y=predicted)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Concreteness Rating") +
  theme_classic()
