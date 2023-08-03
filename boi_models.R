library(lme4)
library(ggeffects)
library(ggggeffects)


# boi
asl_boi_model <- glm(as.factor(produces) ~ age + asl_boi_rating + asl_frequency_rating, data = asl_instrument_data, family = "binomial")
asl_boi_effect <- ggpredict(asl_boi_model, terms = "asl_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "American Sign Language")

bsl_boi_model <- glm(as.factor(produces) ~ age + bsl_boi_rating, data = bsl_instrument_data, family = "binomial")
bsl_boi_effect <- ggpredict(bsl_boi_model, terms = "bsl_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "British Sign Language")

croatian_boi_model <- glm(as.factor(produces) ~ age + croatian_boi_rating, data = croatian_instrument_data, family = "binomial")
croatian_boi_effect <- ggpredict(croatian_boi_model, terms = "croatian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Croatian")

czech_boi_model <- glm(as.factor(produces) ~ age + czech_boi_rating, data = czech_instrument_data, family = "binomial")
czech_boi_effect <- ggpredict(czech_boi_model, terms = "czech_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Czech")

danish_boi_model <- glm(as.factor(produces) ~ age + danish_boi_rating, data = danish_instrument_data, family = "binomial")
danish_boi_effect <- ggpredict(danish_boi_model, terms = "danish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Danish")

dutch_boi_model <- glm(as.factor(produces) ~ age + dutch_boi_rating, data = dutch_instrument_data, family = "binomial")
dutch_boi_effect <- ggpredict(dutch_boi_model, terms = "dutch_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Dutch")                                                                                                                                                                                                                                                                                                

finnish_boi_model <- glm(as.factor(produces) ~ age + finnish_boi_rating, data = finnish_instrument_data, family = "binomial")
finnish_boi_effect <- ggpredict(finnish_boi_model, terms = "finnish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Finnish") 

german_boi_model <- glm(as.factor(produces) ~ age + german_boi_rating, data = as.data.frame(german_instrument_data), family = "binomial")
german_boi_effect <- ggpredict(german_boi_model, terms = "german_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "German") 

greek_boi_model <- glm(as.factor(produces) ~ age + greek_boi_rating, data = as.data.frame(greek_instrument_data), family = "binomial")
greek_boi_effect <- ggpredict(greek_boi_model, terms = "greek_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Greek") 

hebrew_boi_model <- glm(as.factor(produces) ~ age + hebrew_boi_rating, data = as.data.frame(hebrew_instrument_data), family = "binomial")
hebrew_boi_effect <- ggpredict(hebrew_boi_model, terms = "hebrew_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hebrew") 

hungarian_boi_model <- glm(as.factor(produces) ~ age + hungarian_boi_rating, data = as.data.frame(hungarian_instrument_data), family = "binomial")
hungarian_boi_effect <- ggpredict(hungarian_boi_model, terms = "hungarian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Hungarian")

irish_boi_model <- glm(as.factor(produces) ~ age + irish_boi_rating, data = as.data.frame(irish_instrument_data), family = "binomial")
irish_boi_effect <- ggpredict(irish_boi_model, terms = "irish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Irish")

italian_boi_model <- glm(produces ~ age + italian_boi_rating, data = italian_instrument_data, family = "binomial")
italian_boi_effect <- ggeffect(italian_boi_model, terms = "italian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Italian")

# kigiriama_boi_model <- glm(produces ~ age + kigiriama_boi_rating, data = kigiriama_instrument_data, family = "binomial")
# kigiriama_boi_effect <- ggeffect(kigiriama_boi_model, terms = "kigiriama_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
#   mutate(language = "Kigiriama")

kiswahili_boi_model <- glm(produces ~ age + kiswahili_boi_rating, data = kiswahili_instrument_data, family = "binomial")
kiswahili_boi_effect <- ggeffect(kiswahili_boi_model, terms = "kiswahili_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Kiswahili")

korean_boi_model <- glm(produces ~ age + korean_boi_rating, data = korean_instrument_data, family = "binomial")
korean_boi_effect <- ggeffect(korean_boi_model, terms = "korean_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Korean")

latvian_boi_model <- glm(produces ~ age + latvian_boi_rating, data = latvian_instrument_data, family = "binomial")
latvian_boi_effect <- ggeffect(latvian_boi_model, terms = "latvian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Latvian")

norwegian_boi_model <- glm(produces ~ age + norwegian_boi_rating, data = norwegian_instrument_data, family = "binomial")
norwegian_boi_effect <- ggeffect(norwegian_boi_model, terms = "norwegian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Norwegian")

persian_boi_model <- glm(produces ~ age + persian_boi_rating, data = persian_instrument_data, family = "binomial")
persian_boi_effect <- ggeffect(persian_boi_model, terms = "persian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Persian")

russian_boi_model <- glm(produces ~ age + russian_boi_rating, data = russian_instrument_data, family = "binomial")
russian_boi_effect <- ggeffect(russian_boi_model, terms = "russian_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Russian")

slovak_boi_model <- glm(produces ~ age + slovak_boi_rating, data = slovak_instrument_data, family = "binomial")
slovak_boi_effect <- ggeffect(slovak_boi_model, terms = "slovak_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Slovak")

swedish_boi_model <- glm(produces ~ age + swedish_boi_rating, data = swedish_instrument_data, family = "binomial")
swedish_boi_effect <- ggeffect(swedish_boi_model, terms = "swedish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Swedish")

turkish_boi_model <- glm(produces ~ age + turkish_boi_rating, data = turkish_instrument_data, family = "binomial")
turkish_boi_effect <- ggeffect(turkish_boi_model, terms = "turkish_boi_rating", ci.lvl = 0.95, verbose = TRUE) %>%
  mutate(language = "Turkish")

all_boi_effects <- bind_rows(asl_boi_effect,
                             bsl_boi_effect,
                             croatian_boi_effect,
                             czech_boi_effect,
                             danish_boi_effect,
                             dutch_boi_effect, 
                             italian_boi_effect,
                             finnish_boi_effect,
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
                             swedish_boi_effect,
                             turkish_boi_effect
                             )

ggplot(all_boi_effects)  + 
  geom_smooth(size = 1, aes(color=language, fill=language, x=x, y=predicted)) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=language, x=x, y=predicted)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability \nof Word Production", x = "Body-Object Interaction Rating") +
  theme_classic()

