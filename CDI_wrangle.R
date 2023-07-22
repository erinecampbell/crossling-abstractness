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
  left_join(english_bois, by=c("item_definition"="english_word")) %>%
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
  left_join(english_bois, by=c("item_definition"="english_word")) %>%
  distinct(item_definition,.keep_all = TRUE)


english_dict <- american_english_data %>%
  mutate(english_word = item_definition) %>%
  select(uni_lemma, english_word, matches("^english_.*_rating$"))

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

asl_dict <- asl_data %>%
  mutate(asl_word = item_definition) %>%
  select(uni_lemma, asl_word, matches("^asl_.*_rating$"))

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

croatian_dict <- croatian_data %>%
  mutate(croatian_word = item_definition) %>%
  select(uni_lemma, croatian_word, matches("^croatian_.*_rating$"))

# danish
danish_instrument_data <- get_instrument_data("Danish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
danish_aoas <- fit_aoa(
  instrument_data = danish_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(danish_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
danish_data <- danish_aoas %>%
  distinct(item_definition,.keep_all = TRUE) 
danish_dict <- danish_data %>%
  mutate(danish_word = item_definition) %>%
  select(uni_lemma, danish_word, matches("^danish_.*_rating$"))

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

dutch_dict <- dutch_data %>%
  mutate(dutch_word = item_definition) %>%
  select(uni_lemma, dutch_word, matches("^dutch_.*_rating$"))

#german
german_instrument_data <- get_instrument_data("German", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
german_aoas <- fit_aoa(
  instrument_data = german_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(german_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
german_data <- german_aoas %>%
  distinct(item_definition,.keep_all = TRUE)

german_dict <- german_data %>%
  mutate(german_word = item_definition) %>%
  select(uni_lemma, german_word, matches("^german_.*_rating$"))

# korean
korean_instrument_data <- get_instrument_data("Korean", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
korean_aoas <- fit_aoa(
  instrument_data = korean_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(korean_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
korean_data <- korean_aoas %>%
  distinct(item_definition,.keep_all = TRUE)

korean_dict <- korean_data %>%
  mutate(korean_word = item_definition) %>%
  select(uni_lemma, korean_word, matches("^korean_.*_rating$"))

# norwegian
norwegian_instrument_data <- get_instrument_data("Norwegian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
norwegian_aoas <- fit_aoa(
  instrument_data = norwegian_instrument_data,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = 8,
  age_max = max(norwegian_instrument_data$age, na.rm = TRUE)
) %>% 
  mutate(item_definition = tolower(item_definition))
norwegian_data <-
  left_join(norwegian_aoas, norwegian_imageability, by=c("item_definition"="norwegian_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

norwegian_dict <- norwegian_data %>%
  mutate(norwegian_word = item_definition) %>%
  select(uni_lemma, norwegian_word, matches("^norwegian_.*_rating$"))


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

portuguese_dict <- portuguese_data %>%
  mutate(portuguese_word = item_definition) %>%
  select(uni_lemma, portuguese_word, matches("^portuguese_.*_rating$"))


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
  left_join(spanish_emotions, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_imageability, by=c("item_definition"="spanish_word")) %>%
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
  left_join(spanish_emotions, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_imageability, by=c("item_definition"="spanish_word")) %>%
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
  left_join(spanish_emotions, by=c("item_definition"="spanish_word")) %>%
  left_join(spanish_imageability, by=c("item_definition"="spanish_word")) %>%
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
  left_join(spanish_imageability, by=c("item_definition"="spanish_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

spanish_dict <- bind_rows(argentinian_spanish_data, peruvian_spanish_data, european_spanish_data, mexican_spanish_data) %>%
  mutate(spanish_word = item_definition) %>%
  select(uni_lemma, spanish_word, matches("^spanish_.*_rating$")) %>%
  distinct(spanish_word, .keep_all = TRUE)
  

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

italian_dict <- italian_data %>%
  mutate(italian_word = item_definition) %>%
  select(uni_lemma, italian_word, matches("^italian_.*_rating$"))

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
  left_join(french_bois, by=c("item_definition"="french_word")) %>%
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
  left_join(french_bois, by=c("item_definition"="french_word")) %>%
  distinct(item_definition,.keep_all = TRUE)

french_dict <- bind_rows(french_data, quebec_french_data) %>%
  mutate(french_word = item_definition) %>%
  select(uni_lemma, french_word, matches("^french_.*_rating$")) %>%
  distinct(french_word, .keep_all = TRUE)

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

russian_dict <- russian_data %>%
  mutate(russian_word = item_definition) %>%
  select(uni_lemma, russian_word, matches("^russian_.*_rating$")) 

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

chinese_dict <- bind_rows(beijing_data, taiwanese_data) %>%
  mutate(chinese_word = item_definition) %>%
  select(uni_lemma, chinese_word, matches("^chinese_.*_rating$")) %>%
  distinct(chinese_word, .keep_all = TRUE)


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

turkish_dict <- turkish_data %>%
  mutate(turkish_word = item_definition) %>%
  select(uni_lemma, turkish_word, matches("^turkish_.*_rating$")) %>%
  distinct(turkish_word, .keep_all = TRUE)

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
                          cor_test_info(russian_data$aoa,russian_data$russian_auditory_rating, "Russian"),
                          cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_auditory_rating, "Peruvian Spanish"),
                          cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_auditory_rating, "Mexican Spanish"),
                          cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_auditory_rating, "European Spanish")
                          
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
                           cor_test_info(russian_data$aoa,russian_data$russian_olfactory_rating, "Russian"),
                           cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_olfactory_rating, "Peruvian Spanish"),
                           cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_olfactory_rating, "Mexican Spanish"),
                           cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_olfactory_rating, "European Spanish")
                           
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
                             cor_test_info(russian_data$aoa,russian_data$russian_gustatory_rating, "Russian"),
                             cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_gustatory_rating, "Peruvian Spanish"),
                             cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_gustatory_rating, "Mexican Spanish"),
                             cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_gustatory_rating, "European Spanish")
                             
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
                             cor_test_info(italian_data$aoa,italian_data$italian_concreteness_rating, "Italian"),
                             cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_concreteness_rating, "Peruvian Spanish"),
                             cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_concreteness_rating, "Mexican Spanish"),
                             cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_concreteness_rating, "European Spanish")
                             
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
  cor_test_info(italian_data$aoa,italian_data$italian_imageability_rating, "Italian"),
  cor_test_info(russian_data$aoa,russian_data$russian_imageability_rating, "Russian"),
  cor_test_info(norwegian_data$aoa,norwegian_data$norwegian_imageability_rating, "Norwegian"),
  cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_imageability_rating, "Peruvian Spanish"),
  cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_imageability_rating, "Mexican Spanish"),
  cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_imageability_rating, "European Spanish")
)

ggplot(imageability_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Imageability effects on AoA")



boi_corrs <- bind_rows(  
  cor_test_info(american_english_data$aoa,american_english_data$english_boi_rating, "American English"),
  cor_test_info(australian_english_data$aoa,australian_english_data$english_boi_rating, "Australian English"),
  cor_test_info(french_data$aoa,french_data$french_boi_rating, "European French"),
  cor_test_info(quebec_french_data$aoa,quebec_french_data$french_boi_rating, "Quebecois French"),
  cor_test_info(russian_data$aoa,russian_data$russian_boi_rating, "Russian"),
  cor_test_info(argentinian_spanish_data$aoa,argentinian_spanish_data$spanish_boi_rating, "Argentinian Spanish"),
  cor_test_info(peruvian_spanish_data$aoa,peruvian_spanish_data$spanish_boi_rating, "Peruvian Spanish"),
  cor_test_info(mexican_spanish_data$aoa,mexican_spanish_data$spanish_boi_rating, "Mexican Spanish"),
  cor_test_info(european_spanish_data$aoa,european_spanish_data$spanish_boi_rating, "European Spanish")
)

ggplot(boi_corrs, aes(x=R,y=language,color=language)) +
  geom_vline(xintercept=0)+
  geom_point()+
  geom_linerange(aes(xmin=conf_int_lower,xmax=conf_int_upper))+
  theme_minimal()+
  theme(legend.position = "none") +
  ggtitle("Body-Object Interaction effects on AoA")






master_dictionary <- full_join(english_dict, asl_dict) %>%
  distinct(uni_lemma,  asl_word, .keep_all = TRUE) %>%
  full_join(chinese_dict) %>%
  distinct(uni_lemma, chinese_word, .keep_all = TRUE) %>%
  full_join(croatian_dict) %>%
  distinct(uni_lemma,  croatian_word, .keep_all = TRUE) %>%
  full_join(danish_dict) %>%
  distinct(uni_lemma,  dutch_word, .keep_all = TRUE) %>%
   full_join(dutch_dict) %>%
  distinct(uni_lemma,  dutch_word, .keep_all = TRUE) %>%
  full_join(french_dict) %>%
  distinct(uni_lemma,  french_word,.keep_all = TRUE) %>%
  full_join(german_dict) %>%
  distinct(uni_lemma,  german_word, .keep_all = TRUE) %>%
  full_join(italian_dict) %>%
  distinct(uni_lemma,  italian_word, .keep_all = TRUE) %>%
  full_join(korean_dict) %>%
  distinct(uni_lemma,  korean_word, .keep_all = TRUE) %>%
  full_join(norwegian_dict) %>%
  distinct(uni_lemma,  norwegian_word, .keep_all = TRUE) %>%
  full_join(portuguese_dict) %>%
  distinct(uni_lemma,  portuguese_word, .keep_all = TRUE) %>%
  full_join(russian_dict) %>%
  distinct(uni_lemma,  russian_word, .keep_all = TRUE) %>%  
  full_join(spanish_dict) %>%
  distinct(uni_lemma,  spanish_word, .keep_all = TRUE) %>%  
  full_join(turkish_dict) %>%
  distinct(uni_lemma, turkish_word, .keep_all = TRUE) 

master_cdi_word_list <- master_dictionary %>%
  select(uni_lemma, matches("^.*_word$"))
  

master_dictionary_with_ratings <- master_dictionary
rowwise() %>%
  mutate(average_imageability_rating = mean(c_across(matches("^.*_imageability_rating$")), na.rm = TRUE),
         average_visual_rating = mean(c_across(matches("^.*_visual_rating$")), na.rm = TRUE),
         average_auditory_rating = mean(c_across(matches("^.*_auditory_rating$")), na.rm = TRUE),
         average_gustatory_rating = mean(c_across(matches("^.*_gustatory_rating$")), na.rm = TRUE),
         average_olfactory_rating = mean(c_across(matches("^.*_olfactory_rating$")), na.rm = TRUE),
         average_interoceptive_rating = mean(c_across(matches("^.*_interoceptive_rating$")), na.rm = TRUE),
         average_haptic_rating = mean(c_across(matches("^.*_haptic_rating$")), na.rm = TRUE),
         average_concreteness_rating = mean(c_across(matches("^.*_concreteness_rating$")), na.rm = TRUE),
         average_maxperceptual_rating = mean(c_across(matches("^.*_maxperceptual_rating$")), na.rm = TRUE),
         average_boi_rating = mean(c_across(matches("^.*_boi_rating$")), na.rm = TRUE)) %>%
  ungroup() 
  
