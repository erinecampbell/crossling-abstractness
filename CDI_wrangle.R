library(tidyverse)
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

# asl ----

asl_instrument_data_FormA <- get_instrument_data("American Sign Language", form = "FormA", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_instrument_data_FormBOne <- get_instrument_data("American Sign Language", form = "FormBOne", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_instrument_data_FormBTwo <-  get_instrument_data("American Sign Language", form = "FormBTwo", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_instrument_data_FormC <-  get_instrument_data("American Sign Language", form = "FormC", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_instrument_data_CDITwo <-  get_instrument_data("American Sign Language", form = "CDITwo", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
asl_instrument_data <- bind_rows(asl_instrument_data_FormA, 
                                 asl_instrument_data_FormBOne, 
                                 asl_instrument_data_FormBTwo,
                                 asl_instrument_data_FormC,
                                 asl_instrument_data_CDITwo) %>%
  mutate(produces = as.factor(produces))%>%
  as.data.frame() %>%
  left_join(asl_ratings_subset, by = c("item_definition" = "American Sign Language"))

n_asl <- (asl_instrument_data %>% distinct(child_id) %>% nrow())
 # 139
# bsl ----
bsl_instrument_data_WG <-  get_instrument_data("British Sign Language", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
bsl_instrument_data <- bsl_instrument_data_WG %>%
  left_join(bsl_ratings_subset, by = c("item_definition" = "British Sign Language")) %>%
  mutate(produces = as.factor(produces))
n_bsl <- (bsl_instrument_data %>% distinct(child_id) %>% nrow())

# chinese ----
cantonese_instrument_data <- get_instrument_data("Cantonese", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)%>%
  mutate(produces = as.factor(produces))  %>%
  left_join(chinese_ratings_subset, by = c("item_definition" = "Cantonese"))

mandarin_beijing_instrument_data_IC <- get_instrument_data("Mandarin (Beijing)", form = "IC", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_beijing_instrument_data_TC <- get_instrument_data("Mandarin (Beijing)", form = "TC", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_beijing_instrument_data_WS <- get_instrument_data("Mandarin (Beijing)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_beijing_instrument_data <- bind_rows(mandarin_beijing_instrument_data_WS, 
                                              mandarin_beijing_instrument_data_IC, 
                                              mandarin_beijing_instrument_data_TC)%>%
  mutate(produces = as.factor(produces)) %>%
  left_join(chinese_ratings_subset, by = c("item_definition" = "Mandarin (Beijing)"))


mandarin_taiwanese_instrument_data_WG <- get_instrument_data("Mandarin (Taiwanese)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_taiwanese_instrument_data_WS <- get_instrument_data("Mandarin (Taiwanese)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_taiwanese_instrument_data <- bind_rows(mandarin_taiwanese_instrument_data_WG, 
                                                mandarin_taiwanese_instrument_data_WS) %>%
  mutate(produces = as.factor(produces))%>%
  left_join(chinese_ratings_subset, by = c("item_definition" = "Mandarin (Taiwanese)"))

n_chinese <-  (cantonese_instrument_data %>% distinct(child_id) %>% nrow()) + # 1208
  (mandarin_beijing_instrument_data %>% distinct(child_id) %>% nrow()) + # 1938
  (mandarin_taiwanese_instrument_data %>% distinct(child_id) %>% nrow())  #1654
# 5800

# croatian ----
croatian_instrument_data_WG <- get_instrument_data("Croatian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
croatian_instrument_data_WS <- get_instrument_data("Croatian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
croatian_instrument_data <- bind_rows(croatian_instrument_data_WG, 
                                      croatian_instrument_data_WS) %>%
  left_join(croatian_ratings_subset, by = c("item_definition" = "Croatian")) 
n_croatian <- croatian_instrument_data %>% distinct(child_id) %>% nrow() #627
# czech ----
czech_instrument_data <- get_instrument_data("Czech", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(czech_ratings_subset, by = c("item_definition" = "Czech")) 
n_czech <- czech_instrument_data %>% distinct(child_id) %>% nrow() 
# danish ---- 
danish_instrument_data_WG <- get_instrument_data("Danish", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
danish_instrument_data_WS <- get_instrument_data("Danish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
danish_instrument_data <- bind_rows(danish_instrument_data_WG, 
                                      danish_instrument_data_WS) %>%
  mutate(produces = as.factor(produces)) %>%
  left_join(danish_ratings_subset, by = c("item_definition" = "Danish"))
n_danish <- danish_instrument_data %>% distinct(child_id) %>% nrow() #6112

# dutch ----
dutch_instrument_data_WG <- get_instrument_data("Dutch", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_WS <- get_instrument_data("Dutch", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_Swingley <- get_instrument_data("Dutch", form = "Swingley", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_FormOne <- get_instrument_data("Dutch", form = "FormOne", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_FormTwoA <- get_instrument_data("Dutch", form = "FormTwoA", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_FormTwoB <- get_instrument_data("Dutch", form = "FormTwoB", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
dutch_instrument_data_FormThree <- get_instrument_data("Dutch", form = "FormThree", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)


dutch_instrument_data <- bind_rows(dutch_instrument_data_WG, 
                                    dutch_instrument_data_WS,
                                   dutch_instrument_data_Swingley,
                                   dutch_instrument_data_FormOne,
                                   dutch_instrument_data_FormTwoA,
                                   dutch_instrument_data_FormTwoB,
                                   dutch_instrument_data_FormThree)%>%
  left_join(dutch_ratings_subset, by = c("item_definition" = "Dutch")) %>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_dutch <- dutch_instrument_data %>% distinct(child_id) %>% nrow()
 #1936
# english ---- 

american_english_instrument_data_WG <- get_instrument_data("English (American)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
    drop_na(produces)
american_english_instrument_data_WGShort <- get_instrument_data("English (American)", form = "WGShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
american_english_instrument_data_WS <-  get_instrument_data("English (American)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
      drop_na(produces)
american_english_instrument_data_WSShort <-  get_instrument_data("English (American)", form = "WSShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
american_english_instrument_data <- bind_rows(american_english_instrument_data_WG, 
                                              american_english_instrument_data_WS, 
                                              american_english_instrument_data_WGShort,
                                              american_english_instrument_data_WSShort)%>%
    left_join(english_ratings_subset, by = c("item_definition" = "English (American)")) %>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()


australian_english_instrument_data_WS <-  get_instrument_data("English (Australian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
australian_english_instrument_data <- australian_english_instrument_data_WS %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Australian)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()


british_english_instrument_data_TEDSTwos <- get_instrument_data("English (British)", form = "TEDS Twos", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_TEDSThrees <- get_instrument_data("English (British)", form = "TEDS Threes", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_OxfordCDI <-  get_instrument_data("English (British)", form = "Oxford CDI", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_OxfordShort <-  get_instrument_data("English (British)", form = "OxfordShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data <- bind_rows(british_english_instrument_data_TEDSTwos, 
                                             british_english_instrument_data_TEDSThrees, 
                                             british_english_instrument_data_OxfordCDI,
                                             british_english_instrument_data_OxfordShort) %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (British)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()


irish_english_instrument_data_WS <-  get_instrument_data("English (Irish)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
irish_english_instrument_data <- irish_english_instrument_data_WS%>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Irish)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()

n_english <- (american_english_instrument_data %>% distinct(child_id) %>% nrow()) + #10234
  (australian_english_instrument_data %>% distinct(child_id) %>% nrow()) + #1497
  (british_english_instrument_data %>% distinct(child_id) %>% nrow()) + #22589
  (irish_english_instrument_data %>% distinct(child_id) %>% nrow()) #48
 #34368
# finnish ----
finnish_instrument_data_WS <- get_instrument_data("Finnish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
finnish_instrument_data_WSShort <- get_instrument_data("Finnish", form = "WSShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)


finnish_instrument_data <- bind_rows(finnish_instrument_data_WS, 
                                     finnish_instrument_data_WSShort) %>%
  left_join(finnish_ratings_subset, by = c("item_definition" = "Finnish"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_finnish <- finnish_instrument_data %>% distinct(child_id) %>% nrow() #9

# french ----
french_european_instrument_data_WG <- get_instrument_data("French (French)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_european_instrument_data_WS <- get_instrument_data("French (French)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_european_instrument_data <- bind_rows(french_european_instrument_data_WG, 
                                     french_european_instrument_data_WS) %>%
  left_join(french_ratings_subset, by = c("item_definition" = "French (French)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()

french_quebecois_instrument_data_WG <- get_instrument_data("French (Quebecois)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_quebecois_instrument_data_WS <- get_instrument_data("French (Quebecois)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_quebecois_instrument_data <- bind_rows(french_quebecois_instrument_data_WG, 
                                             french_quebecois_instrument_data_WS) %>%
  left_join(french_ratings_subset, by = c("item_definition" = "French (French)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()


n_french<- (french_european_instrument_data %>% distinct(child_id) %>% nrow()) + #747
(french_quebecois_instrument_data %>% distinct(child_id) %>% nrow())

# german ----
german_instrument_data <- get_instrument_data("German", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(german_ratings_subset, by = c("item_definition" = "German"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_german <- german_instrument_data %>% distinct(child_id) %>% nrow() # 1181

# greek ----
greek_instrument_data <- get_instrument_data("Greek (Cypriot)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(greek_ratings_subset, by = c("item_definition" = "Greek (Cypriot)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_greek <- greek_instrument_data %>% distinct(child_id) %>% nrow() # 176

# hebrew ----
hebrew_instrument_data_WG <- get_instrument_data("Hebrew", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
hebrew_instrument_data_WS <- get_instrument_data("Hebrew", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

hebrew_instrument_data <- bind_rows(hebrew_instrument_data_WG, 
                                     hebrew_instrument_data_WS) %>%
  left_join(hebrew_ratings_subset, by = c("item_definition" = "Hebrew"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_hebrew <- hebrew_instrument_data %>% distinct(child_id) %>% nrow() # 557

# hungarian ----
hungarian_instrument_data <- get_instrument_data("Hungarian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(hungarian_ratings_subset, by = c("item_definition" = "Hungarian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_hungarian <- hungarian_instrument_data %>% distinct(child_id) %>% nrow() # 363

# irish ----
irish_instrument_data <- get_instrument_data("Irish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(irish_ratings_subset, by = c("item_definition" = "Irish"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_irish <- irish_instrument_data %>% distinct(child_id) %>% nrow() # 48


# italian ----
italian_instrument_data_WG <- get_instrument_data("Italian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
italian_instrument_data_WS <- get_instrument_data("Italian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

italian_instrument_data <- bind_rows(italian_instrument_data_WG, 
                                    italian_instrument_data_WS) %>%
  left_join(italian_ratings_subset, by = c("item_definition" = "Italian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_italian <- italian_instrument_data %>% distinct(child_id) %>% nrow() # 1400


# kigiriama ----
kigiriama_instrument_data_WG <- get_instrument_data("Kigiriama", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
kigiriama_instrument_data_WS <- get_instrument_data("Kigiriama", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

kigiriama_instrument_data <- bind_rows(kigiriama_instrument_data_WG, 
                                     kigiriama_instrument_data_WS)%>%
  left_join(kigiriama_ratings_subset, by = c("item_definition" = "Kigiriama"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_kigiriama <- kigiriama_instrument_data %>% distinct(child_id) %>% nrow() # 207


# kiswahili ----
kiswahili_instrument_data_WG <- get_instrument_data("Kiswahili", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
kiswahili_instrument_data_WS <- get_instrument_data("Kiswahili", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

kiswahili_instrument_data <- bind_rows(kiswahili_instrument_data_WG, 
                                       kiswahili_instrument_data_WS) %>%
  left_join(kiswahili_ratings_subset, by = c("item_definition" = "Kiswahili"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_kiswahili <- kiswahili_instrument_data %>% distinct(child_id) %>% nrow() # 130

# korean ----
korean_instrument_data_WG <- get_instrument_data("Korean", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
korean_instrument_data_WS <- get_instrument_data("Korean", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

korean_instrument_data <- bind_rows(korean_instrument_data_WG, 
                                       korean_instrument_data_WS) %>%
  left_join(korean_ratings_subset, by = c("item_definition" = "Korean"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_korean <- korean_instrument_data %>% distinct(child_id) %>% nrow() # 1971
# latvian ----
latvian_instrument_data_WG <- get_instrument_data("Latvian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
latvian_instrument_data_WS <- get_instrument_data("Latvian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

latvian_instrument_data <- bind_rows(latvian_instrument_data_WG, 
                                    latvian_instrument_data_WS) %>%
  left_join(latvian_ratings_subset, by = c("item_definition" = "Latvian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_latvian <- latvian_instrument_data %>% distinct(child_id) %>% nrow() # 683

# norwegian ----
norwegian_instrument_data_WG <- get_instrument_data("Norwegian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
norwegian_instrument_data_WS <- get_instrument_data("Norwegian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

norwegian_instrument_data <- bind_rows(norwegian_instrument_data_WG, 
                                    norwegian_instrument_data_WS) %>%
  left_join(norwegian_ratings_subset, by = c("item_definition" = "Norwegian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_norwegian <- norwegian_instrument_data %>% distinct(child_id) %>% nrow() # 7358


# persian ----
persian_instrument_data_WG <- get_instrument_data("Persian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
persian_instrument_data_WS <- get_instrument_data("Persian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

persian_instrument_data <- bind_rows(persian_instrument_data_WG, 
                                       persian_instrument_data_WS) %>%
  left_join(persian_ratings_subset, by = c("item_definition" = "Persian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_persian <- persian_instrument_data %>% distinct(child_id) %>% nrow() # 163


# portuguese ----
portuguese_instrument_data_WG <- get_instrument_data("Portuguese (European)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
portuguese_instrument_data_WS <- get_instrument_data("Portuguese (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

portuguese_instrument_data <- bind_rows(portuguese_instrument_data_WG, 
                                     portuguese_instrument_data_WS) %>%
  left_join(portuguese_ratings_subset, by = c("item_definition" = "Portuguese (European)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_portuguese <- portuguese_instrument_data %>% distinct(child_id) %>% nrow() # 4326


# russian ----
russian_instrument_data_WG <- get_instrument_data("Russian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
russian_instrument_data_WS <- get_instrument_data("Russian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

russian_instrument_data <- bind_rows(russian_instrument_data_WG, 
                                     russian_instrument_data_WS) %>%
  left_join(russian_ratings_subset, by = c("item_definition" = "Russian"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_russian <- russian_instrument_data %>% distinct(child_id) %>% nrow() # 1805

# slovak ----
slovak_instrument_data_WG <- get_instrument_data("Slovak", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
slovak_instrument_data_WS <- get_instrument_data("Slovak", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

slovak_instrument_data <- bind_rows(slovak_instrument_data_WG, 
                                     slovak_instrument_data_WS) %>%
  left_join(slovak_ratings_subset, by = c("item_definition" = "Slovak"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_slovak <- slovak_instrument_data %>% distinct(child_id) %>% nrow() # 1712

# spanish ----

spanish_argentinian_instrument_data <- get_instrument_data("Spanish (Argentinian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Argentinian)"))
spanish_chilean_instrument_data <- get_instrument_data("Spanish (Chilean)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Chilean)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()

spanish_european_instrument_data_WG <- get_instrument_data("Spanish (European)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_european_instrument_data_WS <- get_instrument_data("Spanish (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

spanish_european_instrument_data <- bind_rows(spanish_european_instrument_data_WG, 
                                              spanish_european_instrument_data_WS) %>%
  mutate(produces = as.factor(produces))%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (European)"))


spanish_mexican_instrument_data_WG <-  get_instrument_data("Spanish (Mexican)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_mexican_instrument_data_WS <-  get_instrument_data("Spanish (Mexican)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_mexican_instrument_data <- bind_rows(spanish_mexican_instrument_data_WG, 
                                              spanish_mexican_instrument_data_WS) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Mexican)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()

spanish_peruvian_instrument_data_WG <-  get_instrument_data("Spanish (Peruvian)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_peruvian_instrument_data_WS <-  get_instrument_data("Spanish (Peruvian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_peruvian_instrument_data <- bind_rows(spanish_peruvian_instrument_data_WG, 
                                              spanish_peruvian_instrument_data_WS) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Peruvian)"))%>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()

n_spanish <- (spanish_argentinian_instrument_data %>% distinct(child_id) %>% nrow() ) + #783
  (spanish_chilean_instrument_data %>% distinct(child_id) %>% nrow() ) + #48
  (spanish_european_instrument_data %>% distinct(child_id) %>% nrow() ) + #1005
  (spanish_mexican_instrument_data %>% distinct(child_id) %>% nrow() ) + #2283
  (spanish_peruvian_instrument_data %>% distinct(child_id) %>% nrow() )  #192
# 4311  
  
# swedish ----
swedish_instrument_data_WG <- get_instrument_data("Swedish", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
swedish_instrument_data_WS <- get_instrument_data("Swedish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

swedish_instrument_data <- bind_rows(swedish_instrument_data_WG, 
                                     swedish_instrument_data_WS) %>%
  left_join(swedish_ratings_subset, by = c("item_definition" = "Swedish")) %>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_swedish <- swedish_instrument_data %>% distinct(child_id) %>% nrow() # 1357

# turkish ----
turkish_instrument_data_WG <- get_instrument_data("Turkish", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
turkish_instrument_data_WS <- get_instrument_data("Turkish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

turkish_instrument_data <- bind_rows(turkish_instrument_data_WG, 
                                     turkish_instrument_data_WS) %>%
  left_join(turkish_ratings_subset, by = c("item_definition" = "Turkish")) %>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
n_turkish <- turkish_instrument_data %>% distinct(child_id) %>% nrow() # 3537



# GENERAL STUFF ----
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
  

