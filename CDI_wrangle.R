library(dplyr)
library(wordbankr)
library(rmeta)

cor_test_info <- function(listaoa, listproperty, language) {
  result <- cor.test(listaoa,listproperty)
  
  data.frame(language = language,
             R = result$estimate,
             p_value = result$p.value,
             df = result$parameter,
             conf_int_lower = result$conf.int[1],
             conf_int_upper = result$conf.int[2])
}


# cdi wrangle

american_english_instrument_data <- get_instrument_data("English (American)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
    drop_na(produces)
american_english_aoas <- fit_aoa(
   instrument_data = american_english_instrument_data,
   measure = "produces",
   method = "glm",
   proportion = 0.5,
   age_min = 8,
  age_max = max(american_english_instrument_data$age, na.rm = TRUE)
  )
american_english_data <-
  left_join(american_english_aoas, english_iconicity, by=c("item_definition"="english_word")) %>%
  left_join(english_perceptual, by=c("item_definition"="english_word")) %>%
  left_join(english_concreteness, by=c("item_definition"="english_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

australian_english_instrument_data <- get_instrument_data("English (Australian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
australian_english_aoas <- fit_aoa(
  instrument_data = australian_english_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(australian_english_instrument_data$age, na.rm = TRUE)
)
australian_english_data <-
  left_join(australian_english_aoas, english_iconicity, by=c("item_definition"="english_word")) %>%
  left_join(english_perceptual, by=c("item_definition"="english_word")) %>%
  left_join(english_concreteness, by=c("item_definition"="english_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

# asl
asl_instrument_data <- get_instrument_data("American Sign Language", form = "CDITwo", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_aoas <- fit_aoa(
  instrument_data = asl_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(asl_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
asl_data <-
  left_join(asl_aoas, asl_iconicity, by=c("item_definition"="asl_word")) %>%
  left_join(english_perceptual, by=c("english_gloss"="english_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

# croatian
croatian_instrument_data <- get_instrument_data("Croatian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
croatian_aoas <- fit_aoa(
  instrument_data = croatian_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(croatian_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
croatian_data <-
  left_join(croatian_aoas, croatian_ratings, by=c("item_definition"="croatian_word"))

# dutch
dutch_instrument_data <- get_instrument_data("Dutch", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_aoas <- fit_aoa(
  instrument_data = dutch_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(dutch_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
dutch_data <-
  left_join(dutch_aoas, dutch_perceptual, by=c("item_definition"="dutch_word")) %>%
  left_join(dutch_perceptual, by=c("item_definition"="dutch_word")) %>%
  left_join(dutch_concreteness, by=c("item_definition"="dutch_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

# portuguese
portuguese_instrument_data <- get_instrument_data("Portuguese (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
portuguese_aoas <- fit_aoa(
  instrument_data = portuguese_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(portuguese_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
portuguese_data <-
  left_join(portuguese_aoas, portuguese_concreteness, by=c("item_definition"="portuguese_word")) %>%
  distinct(item_definition,.keep_all = TRUE)


#spanish
peruvian_spanish_instrument_data <- get_instrument_data("Spanish (Peruvian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
peruvian_spanish_aoas <- fit_aoa(
  instrument_data = spanish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(peruvian_spanish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
peruvian_spanish_data <-
  left_join(peruvian_spanish_aoas, spanish_iconicity, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_perceptual, by=c("item_definition"="spanish_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

mexican_spanish_instrument_data <- get_instrument_data("Spanish (Mexican)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mexican_spanish_aoas <- fit_aoa(
  instrument_data = mexican_spanish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(mexican_spanish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
mexican_spanish_data <-
  left_join(mexican_spanish_aoas, spanish_iconicity, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_perceptual, by=c("item_definition"="spanish_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

european_spanish_instrument_data <- get_instrument_data("Spanish (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
european_spanish_aoas <- fit_aoa(
  instrument_data = european_spanish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(european_spanish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
european_spanish_data <-
  left_join(european_spanish_aoas, spanish_iconicity, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_perceptual, by=c("item_definition"="spanish_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

argentinian_spanish_instrument_data <- get_instrument_data("Spanish (Argentinian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
argentinian_spanish_aoas <- fit_aoa(
  instrument_data = argentinian_spanish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(argentinian_spanish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
argentinian_spanish_data <-
  left_join(argentinian_spanish_aoas, spanish_iconicity, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_perceptual, by=c("item_definition"="spanish_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

# italian
italian_instrument_data <- get_instrument_data("Italian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
italian_aoas <- fit_aoa(
  instrument_data = italian_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(italian_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
italian_data <-
  left_join(italian_aoas, italian_perceptual, by=c("item_definition"="italian_word")) %>%
  left_join(italian_ratings, by=c("item_definition"="italian_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

# french
quebec_french_instrument_data <- get_instrument_data("French (Quebecois)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
quebec_french_aoas <- fit_aoa(
  instrument_data = quebec_french_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(quebec_french_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
quebec_french_data <-
  left_join(quebec_french_aoas, french_perceptual, by=c("item_definition"="french_word")) %>%
  left_join(french_imageability, by=c("item_definition"="french_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

french_instrument_data <- get_instrument_data("French (French)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_aoas <- fit_aoa(
  instrument_data = french_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(french_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
french_data <-
  left_join(french_aoas, french_perceptual, by=c("item_definition"="french_word")) %>%
  left_join(french_imageability, by=c("item_definition"="french_word")) %>%
  distinct(item_definition,.keep_all = TRUE)


# russian

russian_instrument_data <- get_instrument_data("Russian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
russian_aoas <- fit_aoa(
  instrument_data = russian_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(russian_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
russian_data <-
  left_join(russian_aoas, russian_perceptual, by=c("item_definition"="WORD")) %>%
  distinct(item_definition,.keep_all = TRUE)


# chinese

beijing_instrument_data <- get_instrument_data("Mandarin (Beijing)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
beijing_aoas <- fit_aoa(
  instrument_data = beijing_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(beijing_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
beijing_data <-
  left_join(beijing_aoas, chinese_perceptual, by=c("item_definition"="traditional")) %>%
  distinct(item_definition,.keep_all = TRUE)

taiwanese_instrument_data <- get_instrument_data("Mandarin (Taiwanese)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
taiwanese_aoas <- fit_aoa(
  instrument_data = taiwanese_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(taiwanese_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
taiwanese_data <-
  left_join(taiwanese_aoas, chinese_perceptual, by=c("item_definition"="traditional")) %>%
  distinct(item_definition,.keep_all = TRUE)


turkish_instrument_data <- get_instrument_data("Turkish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
turkish_aoas <- fit_aoa(
  instrument_data = turkish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(turkish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
turkish_data <- read_csv("norms/turkish/aoas_and_concreteness.csv")

iconicity_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_iconicity_rating, "American English"),
cor_test_info(australian_english_data$aoa,australian_english_data$english_iconicity_rating, "Australian English"),
cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_iconicity_rating, "Mexican Spanish"),
cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_iconicity_rating, "Peruvian Spanish"),
cor_test_info(argentinian_spanish_data$aoa,argentinian_spanish_data$spanish_iconicity_rating, "Argentinian Spanish"),
cor_test_info(asl_data$aoa,asl_data$asl_iconicity_rating, "ASL")
)

ggplot(iconicity_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Iconicity effects on AoA")


visual_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_visual_rating, "American English"),
                             cor_test_info(australian_english_data$aoa,australian_english_data$english_visual_rating, "Australian English"),
                             cor_test_info(italian_data$aoa,italian_data$italian_visual_rating, "Italian"),
                          cor_test_info(dutch_data$aoa,dutch_data$dutch_visual_rating.x, "Dutch"),
                          cor_test_info(beijing_data$aoa,beijing_data$chinese_visual_rating, "Mandarin (Beijing)"),
                          cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_visual_rating, "Mandarin (Taiwanese)"),
                          cor_test_info(russian_data$aoa,russian_data$russian_visual_rating, "Russian")
)

ggplot(visual_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Visual strength effects on AoA")

auditory_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_auditory_rating, "American English"),
                          cor_test_info(australian_english_data$aoa,australian_english_data$english_auditory_rating, "Australian English"),
                          cor_test_info(italian_data$aoa,italian_data$italian_auditory_rating, "Italian"),
                          cor_test_info(dutch_data$aoa,dutch_data$dutch_auditory_rating.x, "Dutch"),
                          cor_test_info(beijing_data$aoa,beijing_data$chinese_auditory_rating, "Mandarin (Beijing)"),
                          cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_auditory_rating, "Mandarin (Taiwanese)"),
                          cor_test_info(russian_data$aoa,russian_data$russian_auditory_rating, "Russian")
                          
)

ggplot(auditory_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Auditory strength effects on AoA")

tactile_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_haptic_rating, "American English"),
                            cor_test_info(australian_english_data$aoa,australian_english_data$english_haptic_rating, "Australian English"),
                            cor_test_info(italian_data$aoa,italian_data$italian_haptic_rating, "Italian"),
                           cor_test_info(dutch_data$aoa,dutch_data$dutch_haptic_rating.x, "Dutch"),
                            cor_test_info(beijing_data$aoa,beijing_data$chinese_haptic_rating, "Mandarin (Beijing)"),
                            cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_haptic_rating, "Mandarin (Taiwanese)"),
                           cor_test_info(russian_data$aoa,russian_data$russian_haptic_rating, "Russian")
                           
)

ggplot(tactile_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Tactile strength effects on AoA")


olfactory_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_olfactory_rating, "American English"),
                           cor_test_info(australian_english_data$aoa,australian_english_data$english_olfactory_rating, "Australian English"),
                           cor_test_info(italian_data$aoa,italian_data$italian_olfactory_rating, "Italian"),
                           cor_test_info(dutch_data$aoa,dutch_data$dutch_olfactory_rating.x, "Dutch"),
                           cor_test_info(beijing_data$aoa,beijing_data$chinese_olfactory_rating, "Mandarin (Beijing)"),
                           cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_olfactory_rating, "Mandarin (Taiwanese)"),
                           cor_test_info(russian_data$aoa,russian_data$russian_olfactory_rating, "Russian")
                           
)

ggplot(olfactory_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Olfactory strength effects on AoA")

gustatory_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_gustatory_rating, "American English"),
                             cor_test_info(australian_english_data$aoa,australian_english_data$english_gustatory_rating, "Australian English"),
                             cor_test_info(dutch_data$aoa,dutch_data$dutch_gustatory_rating.x, "Dutch"),
                             cor_test_info(italian_data$aoa,italian_data$italian_gustatory_rating, "Italian"),
                             cor_test_info(beijing_data$aoa,beijing_data$chinese_gustatory_rating, "Mandarin (Beijing)"),
                             cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_gustatory_rating, "Mandarin (Taiwanese)"),
                             cor_test_info(russian_data$aoa,russian_data$russian_gustatory_rating, "Russian")
                             
)

ggplot(gustatory_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Gustatory strength effects on AoA")

concreteness_corrs <- bind_rows(cor_test_info(american_english_data$aoa,american_english_data$english_concreteness_rating, "American English"),
                             cor_test_info(australian_english_data$aoa,australian_english_data$english_concreteness_rating, "Australian English"),
                             cor_test_info(portuguese_data$aoa,portuguese_data$portuguese_concreteness_rating, "European Portuguese"),
                             cor_test_info(dutch_data$aoa,dutch_data$dutch_concreteness_rating, "Dutch"),
                             cor_test_info(turkish_data$aoa,turkish_data$turkish_concreteness_rating, "Turkish"),
                             cor_test_info(croatian_data$aoa,croatian_data$croatian_concreteness_rating, "Croatian"),
                             cor_test_info(italian_data$aoa,italian_data$italian_concreteness_rating, "Italian")
                             
                             # ,
                             # cor_test_info(italian_data$aoa,italian_data$italian_gustatory_rating, "Italian"),
                             # cor_test_info(beijing_data$aoa,beijing_data$chinese_gustatory_rating, "Mandarin (Beijing)"),
                             # cor_test_info(taiwanese_data$aoa,taiwanese_data$chinese_gustatory_rating, "Mandarin (Taiwanese)")
)

ggplot(concreteness_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Concreteness effects on AoA")
# american_english_aoas <- fit_aoa(
#   instrument_data = american_english_instrument_data,
#   measure = "produces",
#   method = "glm",
#   proportion = 0.5,
#   age_min = min(instrument_data$age, na.rm = TRUE),
#   age_max = max(instrument_data$age, na.rm = TRUE)
# )

imageability_corrs <- bind_rows(  
  cor_test_info(dutch_data$aoa,dutch_data$dutch_imageability_rating.x, "Dutch"),
  cor_test_info(portuguese_data$aoa,portuguese_data$portuguese_imageability_rating, "European Portuguese"),
  cor_test_info(french_data$aoa,french_data$french_imageability_rating, "European French"),
  cor_test_info(quebec_french_data$aoa,quebec_french_data$french_imageability_rating, "Quebecois French"),
  cor_test_info(croatian_data$aoa,croatian_data$croatian_imageability_rating, "Croatian"),
  cor_test_info(italian_data$aoa,italian_data$italian_imagery_rating, "Italian"),
  cor_test_info(russian_data$aoa,russian_data$russian_imageability_rating, "Russian")
  
  
)

ggplot(imageability_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Imageability effects on AoA")
