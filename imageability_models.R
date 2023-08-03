library(lme4)
library(ggeffects)
library(ggggeffects)


# imageability
asl_imageability_model <- glm(as.factor(produces) ~ age + asl_imageability_rating + asl_frequency_rating, data = asl_instrument_data, family = "binomial")
asl_imageability_effect <- ggpredict(asl_imageability_model, terms = "asl_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language")

bsl_imageability_model <- glm(as.factor(produces) ~ age + bsl_imageability_rating, data = bsl_instrument_data, family = "binomial")
bsl_imageability_effect <- ggpredict(bsl_imageability_model, terms = "bsl_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language")

croatian_imageability_model <- glm(as.factor(produces) ~ age + croatian_imageability_rating, data = croatian_instrument_data, family = "binomial")
croatian_imageability_effect <- ggpredict(croatian_imageability_model, terms = "croatian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian")

czech_imageability_model <- glm(as.factor(produces) ~ age + czech_imageability_rating, data = czech_instrument_data, family = "binomial")
czech_imageability_effect <- ggpredict(czech_imageability_model, terms = "czech_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech")

danish_imageability_model <- glm(as.factor(produces) ~ age + danish_imageability_rating, data = danish_instrument_data, family = "binomial")
danish_imageability_effect <- ggpredict(danish_imageability_model, terms = "danish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish")

dutch_imageability_model <- glm(as.factor(produces) ~ age + dutch_imageability_rating, data = dutch_instrument_data, family = "binomial")
dutch_imageability_effect <- ggpredict(dutch_imageability_model, terms = "dutch_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch")                                                                                                                                                                                                                                                                                                

finnish_imageability_model <- glm(as.factor(produces) ~ age + finnish_imageability_rating, data = finnish_instrument_data, family = "binomial")
finnish_imageability_effect <- ggpredict(finnish_imageability_model, terms = "finnish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish") 

german_imageability_model <- glm(as.factor(produces) ~ age + german_imageability_rating, data = as.data.frame(german_instrument_data), family = "binomial")
german_imageability_effect <- ggpredict(german_imageability_model, terms = "german_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German") 

greek_imageability_model <- glm(as.factor(produces) ~ age + greek_imageability_rating, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_imageability_effect <- ggpredict(greek_imageability_model, terms = "greek_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek") 

hebrew_imageability_model <- glm(as.factor(produces) ~ age + hebrew_imageability_rating, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_imageability_effect <- ggpredict(hebrew_imageability_model, terms = "hebrew_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew") 

hungarian_imageability_model <- glm(as.factor(produces) ~ age + hungarian_imageability_rating, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_imageability_effect <- ggpredict(hungarian_imageability_model, terms = "hungarian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian")

irish_imageability_model <- glm(as.factor(produces) ~ age + irish_imageability_rating, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_imageability_effect <- ggpredict(irish_imageability_model, terms = "irish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish")

italian_imageability_model <- glm(produces ~ age + italian_imageability_rating, data = italian_instrument_data, family = "binomial")
italian_imageability_effect <- ggeffect(italian_imageability_model, terms = "italian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian")

# kigiriama_imageability_model <- glm(produces ~ age + kigiriama_imageability_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_imageability_effect <- ggeffect(kigiriama_imageability_model, terms = "kigiriama_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_imageability_model <- glm(produces ~ age + kiswahili_imageability_rating, data = kiswahili_instrument_data, family = "binomial")
kiswahili_imageability_effect <- ggeffect(kiswahili_imageability_model, terms = "kiswahili_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili")

korean_imageability_model <- glm(produces ~ age + korean_imageability_rating, data = korean_instrument_data, family = "binomial")
korean_imageability_effect <- ggeffect(korean_imageability_model, terms = "korean_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean")

latvian_imageability_model <- glm(produces ~ age + latvian_imageability_rating, data = latvian_instrument_data, family = "binomial")
latvian_imageability_effect <- ggeffect(latvian_imageability_model, terms = "latvian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian")

norwegian_imageability_model <- glm(produces ~ age + norwegian_imageability_rating, data = norwegian_instrument_data, family = "binomial")
norwegian_imageability_effect <- ggeffect(norwegian_imageability_model, terms = "norwegian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian")

persian_imageability_model <- glm(produces ~ age + persian_imageability_rating, data = persian_instrument_data, family = "binomial")
persian_imageability_effect <- ggeffect(persian_imageability_model, terms = "persian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Persian")

russian_imageability_model <- glm(produces ~ age + russian_imageability_rating, data = russian_instrument_data, family = "binomial")
russian_imageability_effect <- ggeffect(russian_imageability_model, terms = "russian_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian")

slovak_imageability_model <- glm(produces ~ age + slovak_imageability_rating, data = slovak_instrument_data, family = "binomial")
slovak_imageability_effect <- ggeffect(slovak_imageability_model, terms = "slovak_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak")

swedish_imageability_model <- glm(produces ~ age + swedish_imageability_rating, data = swedish_instrument_data, family = "binomial")
swedish_imageability_effect <- ggeffect(swedish_imageability_model, terms = "swedish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish")

turkish_imageability_model <- glm(produces ~ age + turkish_imageability_rating, data = turkish_instrument_data, family = "binomial")
turkish_imageability_effect <- ggeffect(turkish_imageability_model, terms = "turkish_imageability_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Turkish")

all_imageability_effects <- bind_rows(asl_imageability_effect,
                             bsl_imageability_effect,
                             croatian_imageability_effect,
                             czech_imageability_effect,
                             danish_imageability_effect,
                             dutch_imageability_effect,
                             italian_imageability_effect,
                             finnish_imageability_effect,
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
                             swedish_imageability_effect,
                             turkish_imageability_effect
                             )

ggplot(all_imageability_effects)  + 
  geom_smooth(size = 1, aes(color=language, fill=language, x=x, y=predicted)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "imageability Rating") +
  theme_classic()
