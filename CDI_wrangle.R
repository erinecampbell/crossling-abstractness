library(wordbankr)
library(rmeta)

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
  distinct(item_definition,.keep_all = TRUE)

# spanish


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
  distinct(item_definition,.keep_all = TRUE)


cor_test_info <- function(listaoa, listproperty, language) {
  result <- cor.test(listaoa,listproperty)
  
  data.frame(language = language,
            R = result$estimate,
             p_value = result$p.value,
             df = result$parameter,
             conf_int_lower = result$conf.int[1],
             conf_int_upper = result$conf.int[2])
}


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
  theme(legend.position = "none")


# american_english_aoas <- fit_aoa(
#   instrument_data = american_english_instrument_data,
#   measure = "produces",
#   method = "glm",
#   proportion = 0.5,
#   age_min = min(instrument_data$age, na.rm = TRUE),
#   age_max = max(instrument_data$age, na.rm = TRUE)
# )

