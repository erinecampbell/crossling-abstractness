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

# arabic
arabic_instrument_data_WG <- get_instrument_data("Arabic (Saudi)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
arabic_instrument_data_WS <- get_instrument_data("Arabic (Saudi)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

arabic_instrument_data <- bind_rows(arabic_instrument_data_WG, 
                                    arabic_instrument_data_WS) %>%
  mutate(produces = as.factor(produces))%>%
  as.data.frame() %>%
  left_join(arabic_ratings_subset, by = c("item_definition" = "Arabic (Saudi)"))
write_rds(arabic_instrument_data, "norms/arabic/arabic_instrument_data.rds")

n_arabic <- (arabic_instrument_data %>% distinct(child_id) %>% nrow())


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
write_rds(asl_instrument_data, "norms/asl/asl_instrument_data.rds")
asl_FormA_aoas <- fit_aoa(
  asl_instrument_data_FormA,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(asl_instrument_data_FormA$age, na.rm = TRUE),
  age_max = max(asl_instrument_data_FormA$age, na.rm = TRUE)
)
asl_FormBOne_aoas <- fit_aoa(
  asl_instrument_data_FormBOne,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(asl_instrument_data_FormBOne$age, na.rm = TRUE),
  age_max = max(asl_instrument_data_FormBOne$age, na.rm = TRUE)
)
asl_FormBTwo_aoas <- fit_aoa(
  asl_instrument_data_FormBTwo,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(asl_instrument_data_FormBTwo$age, na.rm = TRUE),
  age_max = max(asl_instrument_data_FormBTwo$age, na.rm = TRUE)
)
asl_FormC_aoas <- fit_aoa(
  asl_instrument_data_FormC,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(asl_instrument_data_FormC$age, na.rm = TRUE),
  age_max = max(asl_instrument_data_FormC$age, na.rm = TRUE)
)
asl_CDITwo_aoas <- fit_aoa(
  asl_instrument_data_CDITwo,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(asl_instrument_data_CDITwo$age, na.rm = TRUE),
  age_max = max(asl_instrument_data_CDITwo$age, na.rm = TRUE)
)
asl_aoas <- bind_rows(asl_FormA_aoas,
                      asl_FormBOne_aoas,
                      asl_FormBTwo_aoas,
                      asl_FormC_aoas,
                      asl_CDITwo_aoas) %>%
  left_join(asl_ratings_subset, by = c("item_definition" = "American Sign Language"))

n_asl <- (asl_instrument_data %>% distinct(child_id) %>% nrow()) # 139
# bsl ----
bsl_instrument_data_WG <- get_instrument_data("British Sign Language", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) 
bsl_instrument_data <- bsl_instrument_data_WG %>%
  left_join(bsl_ratings_subset, by = c("item_definition" = "British Sign Language")) %>%
  mutate(produces = as.factor(produces)) %>%
  as.data.frame()
# bsl_aoas <- fit_aoa(
#   (bsl_instrument_data),
#   measure = "produces",
#   method = "glm",
#   proportion = 0.5,
#   age_min = min(bsl_instrument_data$age, na.rm = TRUE),
#   age_max = max(bsl_instrument_data$age, na.rm = TRUE)
# )
write_rds(bsl_instrument_data, "norms/bsl/bsl_instrument_data.rds")

bsl_WG_aoas <- fit_aoa(
  bsl_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(bsl_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(bsl_instrument_data_WG$age, na.rm = TRUE)
)
bsl_aoas <-  bsl_WG_aoas %>%
  left_join(bsl_ratings_subset, by = c("item_definition" = "British Sign Language"))

n_bsl <- (bsl_instrument_data %>% distinct(child_id) %>% nrow())

# catalan ----
catalan_instrument_data_WG <- get_instrument_data("Catalan", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
catalan_instrument_data_WS <- get_instrument_data("Catalan", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

catalan_instrument_data <- bind_rows(catalan_instrument_data_WG, 
                                    catalan_instrument_data_WS) %>%
  mutate(produces = as.factor(produces))%>%
  as.data.frame() %>%
  left_join(catalan_ratings_subset, by = c("item_definition" = "Catalan"))
write_rds(catalan_instrument_data, "norms/catalan/catalan_instrument_data.rds")

n_catalan <- (catalan_instrument_data %>% distinct(child_id) %>% nrow())


# chinese ----
cantonese_instrument_data_WS <- get_instrument_data("Cantonese", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

cantonese_instrument_data <- cantonese_instrument_data_WS%>%
  left_join(chinese_ratings_subset, by = c("item_definition" = "Cantonese")) %>% 
  as.data.frame()%>%
  mutate(produces = as.factor(produces)) 
write_rds(cantonese_instrument_data, "norms/chinese/cantonese_instrument_data.rds")

cantonese_aoas <- fit_aoa(
  cantonese_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(cantonese_instrument_data$age, na.rm = TRUE),
  age_max = max(cantonese_instrument_data$age, na.rm = TRUE)
) %>%
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
  left_join(chinese_ratings_subset, by = c("item_definition" = "Mandarin (Beijing)")) %>% 
  as.data.frame()
write_rds(mandarin_beijing_instrument_data, "norms/chinese/mandarin_beijing_instrument_data.rds")



mandarin_taiwanese_instrument_data_WG <- get_instrument_data("Mandarin (Taiwanese)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_taiwanese_instrument_data_WS <- get_instrument_data("Mandarin (Taiwanese)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
mandarin_taiwanese_instrument_data <- bind_rows(mandarin_taiwanese_instrument_data_WG, 
                                                mandarin_taiwanese_instrument_data_WS) %>%
  mutate(produces = as.factor(produces))%>%
  left_join(chinese_ratings_subset, by = c("item_definition" = "Mandarin (Taiwanese)")) %>% 
  as.data.frame()
write_rds(mandarin_taiwanese_instrument_data, "norms/chinese/mandarin_taiwanese_instrument_data.rds")


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
  left_join(croatian_ratings_subset, by = c("item_definition" = "Croatian")) %>%
  as.data.frame() %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition))
write_rds(croatian_instrument_data, "norms/croatian/croatian_instrument_data.rds")


croatian_WG_aoas <- fit_aoa(
  croatian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(croatian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(croatian_instrument_data_WG$age, na.rm = TRUE)
)
croatian_WS_aoas <- fit_aoa(
  croatian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(croatian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(croatian_instrument_data_WS$age, na.rm = TRUE)
)
croatian_aoas <- bind_rows(croatian_WG_aoas,
                         croatian_WS_aoas) %>%
  left_join(croatian_ratings_subset, by = c("item_definition" = "Croatian"))

n_croatian <- croatian_instrument_data %>% distinct(child_id) %>% nrow() #627
# czech ----
czech_instrument_data_WS <- get_instrument_data("Czech", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) 

czech_instrument_data <-czech_instrument_data_WS %>%
  left_join(czech_ratings_subset, by = c("item_definition" = "Czech")) %>%
  as.data.frame() %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition))


write_rds(czech_instrument_data, "norms/czech/czech_instrument_data.rds")

czech_WS_aoas <- fit_aoa(
  czech_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(czech_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(czech_instrument_data_WS$age, na.rm = TRUE)
)
czech_aoas <-  czech_WS_aoas %>%
  left_join(czech_ratings_subset, by = c("item_definition" = "Czech"))

n_czech <- czech_instrument_data %>% distinct(child_id) %>% nrow() #493
# danish ---- 
danish_instrument_data_WG <- get_instrument_data("Danish", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
danish_instrument_data_WS <- get_instrument_data("Danish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
danish_instrument_data <- bind_rows(danish_instrument_data_WG, 
                                      danish_instrument_data_WS) %>%
  mutate(produces = as.factor(produces)) %>%
  left_join(danish_ratings_subset, by = c("item_definition" = "Danish")) %>%
  as.data.frame() %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition))
write_rds(danish_instrument_data, "norms/danish/danish_instrument_data.rds")

danish_WG_aoas <- fit_aoa(
  danish_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(danish_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(danish_instrument_data_WG$age, na.rm = TRUE)
)
danish_WS_aoas <- fit_aoa(
  danish_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(danish_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(danish_instrument_data_WS$age, na.rm = TRUE)
)
danish_aoas <- bind_rows(danish_WG_aoas,
                         danish_WS_aoas) %>%
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


dutch_instrument_data_no_ratings <- bind_rows(dutch_instrument_data_WG, 
                                    dutch_instrument_data_WS,
                                   dutch_instrument_data_Swingley,
                                   dutch_instrument_data_FormOne,
                                   dutch_instrument_data_FormTwoA,
                                   dutch_instrument_data_FormTwoB,
                                   dutch_instrument_data_FormThree)

dutch_instrument_data <- dutch_instrument_data_no_ratings %>%
  left_join(dutch_ratings_subset, by = c("item_definition" = "Dutch")) %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(dutch_instrument_data, "norms/dutch/dutch_instrument_data.rds")


dutch_aoas <- fit_aoa(
  dutch_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(dutch_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(dutch_instrument_data_no_ratings$age, na.rm = TRUE)
) %>%
  left_join(dutch_ratings_subset, by = c("item_definition" = "Dutch"))


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
american_english_instrument_data_no_ratings <- bind_rows(american_english_instrument_data_WG, 
                                              american_english_instrument_data_WS, 
                                              american_english_instrument_data_WGShort,
                                              american_english_instrument_data_WSShort)
american_english_instrument_data <- american_english_instrument_data_no_ratings %>%
    left_join(english_ratings_subset, by = c("item_definition" = "English (American)")) %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(american_english_instrument_data, "norms/english/american_english_instrument_data.rds")

american_english_aoas <- fit_aoa(
  american_english_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(american_english_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(american_english_instrument_data_no_ratings$age, na.rm = TRUE)
)  %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (American)"))

australian_english_instrument_data_WS <-  get_instrument_data("English (Australian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
australian_english_instrument_data <- australian_english_instrument_data_WS %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Australian)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(australian_english_instrument_data, "norms/english/australian_english_instrument_data.rds")

australian_english_aoas <- fit_aoa(
  australian_english_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(australian_english_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(australian_english_instrument_data_WS$age, na.rm = TRUE)
)  %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Australian)"))


british_english_instrument_data_TEDSTwos <- get_instrument_data("English (British)", form = "TEDS Twos", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_TEDSThrees <- get_instrument_data("English (British)", form = "TEDS Threes", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_OxfordCDI <-  get_instrument_data("English (British)", form = "Oxford CDI", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_OxfordShort <-  get_instrument_data("English (British)", form = "OxfordShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
british_english_instrument_data_no_ratings <- bind_rows(british_english_instrument_data_TEDSTwos, 
                                             british_english_instrument_data_TEDSThrees, 
                                             british_english_instrument_data_OxfordCDI,
                                             british_english_instrument_data_OxfordShort) 
british_english_instrument_data <- british_english_instrument_data_no_ratings%>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (British)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(british_english_instrument_data, "norms/english/british_english_instrument_data.rds")

british_english_aoas <- fit_aoa(
  british_english_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(british_english_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(british_english_instrument_data_no_ratings$age, na.rm = TRUE)
)  %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (British)"))


irish_english_instrument_data_WS <-  get_instrument_data("English (Irish)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
irish_english_instrument_data <- irish_english_instrument_data_WS%>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Irish)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(irish_english_instrument_data, "norms/english/irish_english_instrument_data.rds")


irish_english_aoas <- fit_aoa(
  irish_english_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(irish_english_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(irish_english_instrument_data_WS$age, na.rm = TRUE)
)  %>%
  left_join(english_ratings_subset, by = c("item_definition" = "English (Irish)"))



n_english <- (american_english_instrument_data %>% distinct(child_id) %>% nrow()) + #10234
  (australian_english_instrument_data %>% distinct(child_id) %>% nrow()) + #1497
  (british_english_instrument_data %>% distinct(child_id) %>% nrow()) + #22589
  (irish_english_instrument_data %>% distinct(child_id) %>% nrow()) #48
 #34368

# estonian----

estonian_instrument_data_WS <- get_instrument_data("Estonian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

estonian_instrument_data <-estonian_instrument_data_WS %>%
  mutate(produces = as.factor(produces))%>%
  as.data.frame() %>%
  left_join(estonian_ratings_subset, by = c("item_definition" = "Estonian"))
write_rds(estonian_instrument_data, "norms/estonian/estonian_instrument_data.rds")

n_estonian <- (estonian_instrument_data %>% distinct(child_id) %>% nrow())

# finnish ----
finnish_instrument_data_WGProd <- get_instrument_data("Finnish", form = "WGProd", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
finnish_instrument_data_WGShort <- get_instrument_data("Finnish", form = "WGProdShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
finnish_instrument_data_WS <- get_instrument_data("Finnish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
finnish_instrument_data_WSShort <- get_instrument_data("Finnish", form = "WSShort", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)


finnish_instrument_data <- bind_rows(finnish_instrument_data_WGProd,
                                     finnish_instrument_data_WGShort,
                                     finnish_instrument_data_WS, 
                                     finnish_instrument_data_WSShort) %>%
  left_join(finnish_ratings_subset, by = c("item_definition" = "Finnish"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(finnish_instrument_data, "norms/finnish/finnish_instrument_data.rds")


finnish_WS_aoas <- fit_aoa(
  finnish_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(finnish_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(finnish_instrument_data_WS$age, na.rm = TRUE)
)
finnish_WSShort_aoas <- fit_aoa(
  finnish_instrument_data_WSShort,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(finnish_instrument_data_WSShort$age, na.rm = TRUE),
  age_max = max(finnish_instrument_data_WSShort$age, na.rm = TRUE)
)
finnish_aoas <- bind_rows(finnish_WS_aoas,
                         finnish_WSShort_aoas) %>%
  left_join(finnish_ratings_subset, by = c("item_definition" = "Finnish"))


n_finnish <- finnish_instrument_data %>% distinct(child_id) %>% nrow() #9

# french ----
french_european_instrument_data_WG <- get_instrument_data("French (French)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_european_instrument_data_WS <- get_instrument_data("French (French)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_european_instrument_data <- bind_rows(french_european_instrument_data_WG, 
                                     french_european_instrument_data_WS) %>%
  left_join(french_ratings_subset, by = c("item_definition" = "French (French)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(french_european_instrument_data, "norms/french/french_european_instrument_data.rds")

french_european_WG_aoas <- fit_aoa(
  french_european_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(french_european_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(french_european_instrument_data_WG$age, na.rm = TRUE)
)
french_european_WS_aoas <- fit_aoa(
  french_european_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(french_european_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(french_european_instrument_data_WS$age, na.rm = TRUE)
)
french_european_aoas <- bind_rows(french_european_WG_aoas,
                         french_european_WS_aoas) %>%
  left_join(french_ratings_subset, by = c("item_definition" = "French (French)"))

french_quebecois_instrument_data_WG <- get_instrument_data("French (Quebecois)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_quebecois_instrument_data_WS <- get_instrument_data("French (Quebecois)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
french_quebecois_instrument_data <- bind_rows(french_quebecois_instrument_data_WG, 
                                             french_quebecois_instrument_data_WS) %>%
  left_join(french_ratings_subset, by = c("item_definition" = "French (French)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(french_quebecois_instrument_data, "norms/french/french_quebecois_instrument_data.rds")

n_french<- (french_european_instrument_data %>% distinct(child_id) %>% nrow()) + #747
(french_quebecois_instrument_data %>% distinct(child_id) %>% nrow())

# german ----
german_instrument_data_WS <- get_instrument_data("German", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) 
german_instrument_data <- german_instrument_data_WS%>%
  left_join(german_ratings_subset, by = c("item_definition" = "German"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(german_instrument_data, "norms/german/german_instrument_data.rds")

german_WS_aoas <- fit_aoa(
  german_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(german_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(german_instrument_data_WS$age, na.rm = TRUE)
)
german_aoas <-  german_WS_aoas %>%
  left_join(german_ratings_subset, by = c("item_definition" = "German"))

n_german <- german_instrument_data %>% distinct(child_id) %>% nrow() # 1181

# greek ----
greek_instrument_data_WS <- get_instrument_data("Greek (Cypriot)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

greek_instrument_data <- greek_instrument_data_WS%>%
  left_join(greek_ratings_subset, by = c("item_definition" = "Greek (Cypriot)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(greek_instrument_data, "norms/greek/greek_instrument_data.rds")

greek_WS_aoas <- fit_aoa(
  greek_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(greek_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(greek_instrument_data_WS$age, na.rm = TRUE)
)
greek_aoas <-  greek_WS_aoas %>%
  left_join(greek_ratings_subset, by = c("item_definition" = "Greek (Cypriot)"))

n_greek <- greek_instrument_data %>% distinct(child_id) %>% nrow() # 176

# hebrew ----
hebrew_instrument_data_WG <- get_instrument_data("Hebrew", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
hebrew_instrument_data_WS <- get_instrument_data("Hebrew", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

hebrew_instrument_data <- bind_rows(hebrew_instrument_data_WG, 
                                     hebrew_instrument_data_WS) %>%
  left_join(hebrew_ratings_subset, by = c("item_definition" = "Hebrew"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(hebrew_instrument_data, "norms/hebrew/hebrew_instrument_data.rds")



hebrew_WG_aoas <- fit_aoa(
  hebrew_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(hebrew_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(hebrew_instrument_data_WG$age, na.rm = TRUE)
)
hebrew_WS_aoas <- fit_aoa(
  hebrew_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(hebrew_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(hebrew_instrument_data_WS$age, na.rm = TRUE)
)
hebrew_aoas <- bind_rows(hebrew_WG_aoas,
                          hebrew_WS_aoas) %>%
  left_join(hebrew_ratings_subset, by = c("item_definition" = "Hebrew"))

n_hebrew <- hebrew_instrument_data %>% distinct(child_id) %>% nrow() # 557

# hungarian ----
hungarian_instrument_data_WS <- get_instrument_data("Hungarian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

hungarian_instrument_data <- hungarian_instrument_data_WS %>%
  left_join(hungarian_ratings_subset, by = c("item_definition" = "Hungarian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(hungarian_instrument_data, "norms/hungarian/hungarian_instrument_data.rds")


hungarian_WS_aoas <- fit_aoa(
  hungarian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(hungarian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(hungarian_instrument_data_WS$age, na.rm = TRUE)
)
hungarian_aoas <-  hungarian_WS_aoas %>%
  left_join(hungarian_ratings_subset, by = c("item_definition" = "Hungarian"))


n_hungarian <- hungarian_instrument_data %>% distinct(child_id) %>% nrow() # 363

# irish ----
irish_instrument_data <- get_instrument_data("Irish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  left_join(irish_ratings_subset, by = c("item_definition" = "Irish"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(irish_instrument_data, "norms/irish/irish_instrument_data.rds")

n_irish <- irish_instrument_data %>% distinct(child_id) %>% nrow() # 48

# japanese----
japanese_instrument_data_WG <- get_instrument_data("Japanese", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
japanese_instrument_data_WS <- get_instrument_data("Japanese", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

japanese_instrument_data <- bind_rows(japanese_instrument_data_WG, 
                                     japanese_instrument_data_WS) %>%
  left_join(japanese_ratings_subset, by = c("item_definition" = "Japanese"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(japanese_instrument_data, "norms/japanese/japanese_instrument_data.rds")
n_japanese <- japanese_instrument_data %>% distinct(child_id) %>% nrow() # 846


# italian ----
italian_instrument_data_WG <- get_instrument_data("Italian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
italian_instrument_data_WS <- get_instrument_data("Italian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

italian_instrument_data <- bind_rows(italian_instrument_data_WG, 
                                    italian_instrument_data_WS) %>%
  left_join(italian_ratings_subset, by = c("item_definition" = "Italian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(italian_instrument_data, "norms/italian/italian_instrument_data.rds")

italian_WG_aoas <- fit_aoa(
  italian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(italian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(italian_instrument_data_WG$age, na.rm = TRUE)
)
italian_WS_aoas <- fit_aoa(
  italian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(italian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(italian_instrument_data_WS$age, na.rm = TRUE)
)
italian_aoas <- bind_rows(italian_WG_aoas,
                          italian_WS_aoas) %>%
  left_join(italian_ratings_subset, by = c("item_definition" = "Italian"))

n_italian <- italian_instrument_data %>% distinct(child_id) %>% nrow() # 1400


# kigiriama ----
kigiriama_instrument_data_WG <- get_instrument_data("Kigiriama", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
kigiriama_instrument_data_WS <- get_instrument_data("Kigiriama", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

kigiriama_instrument_data <- bind_rows(kigiriama_instrument_data_WG, 
                                     kigiriama_instrument_data_WS)%>%
  left_join(kigiriama_ratings_subset, by = c("item_definition" = "Kigiriama"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
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
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(kiswahili_instrument_data, "norms/kiswahili/kiswahili_instrument_data.rds")

n_kiswahili <- kiswahili_instrument_data %>% distinct(child_id) %>% nrow() # 130

# korean ----
korean_instrument_data_WG <- get_instrument_data("Korean", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
korean_instrument_data_WS <- get_instrument_data("Korean", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

korean_instrument_data <- bind_rows(korean_instrument_data_WG, 
                                       korean_instrument_data_WS) %>%
  left_join(korean_ratings_subset, by = c("item_definition" = "Korean"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(korean_instrument_data, "norms/korean/korean_instrument_data.rds")

korean_WG_aoas <- fit_aoa(
  korean_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(korean_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(korean_instrument_data_WG$age, na.rm = TRUE)
)
korean_WS_aoas <- fit_aoa(
  korean_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(korean_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(korean_instrument_data_WS$age, na.rm = TRUE)
)
korean_aoas <- bind_rows(korean_WG_aoas,
                          korean_WS_aoas) %>%
  left_join(korean_ratings_subset, by = c("item_definition" = "Korean"))

n_korean <- korean_instrument_data %>% distinct(child_id) %>% nrow() # 1971
# latvian ----
latvian_instrument_data_WG <- get_instrument_data("Latvian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
latvian_instrument_data_WS <- get_instrument_data("Latvian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

latvian_instrument_data <- bind_rows(latvian_instrument_data_WG, 
                                    latvian_instrument_data_WS) %>%
  left_join(latvian_ratings_subset, by = c("item_definition" = "Latvian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(latvian_instrument_data, "norms/latvian/latvian_instrument_data.rds")

latvian_WG_aoas <- fit_aoa(
  latvian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(latvian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(latvian_instrument_data_WG$age, na.rm = TRUE)
)
latvian_WS_aoas <- fit_aoa(
  latvian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(latvian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(latvian_instrument_data_WS$age, na.rm = TRUE)
)
latvian_aoas <- bind_rows(latvian_WG_aoas,
                          latvian_WS_aoas) %>%
  left_join(latvian_ratings_subset, by = c("item_definition" = "Latvian"))

n_latvian <- latvian_instrument_data %>% distinct(child_id) %>% nrow() # 683

# norwegian ----
norwegian_instrument_data_WG <- get_instrument_data("Norwegian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  as.data.frame()
norwegian_instrument_data_WS <- get_instrument_data("Norwegian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) %>%
  as.data.frame()

norwegian_instrument_data <- bind_rows(norwegian_instrument_data_WG, 
                                    norwegian_instrument_data_WS) %>%
  as.data.frame() %>%
  left_join(norwegian_ratings_subset, by = c("item_definition" = "Norwegian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) 

write_rds(norwegian_instrument_data, "norms/norwegian/norwegian_instrument_data.rds")

norwegian_WG_aoas <- fit_aoa(
  norwegian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(norwegian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(norwegian_instrument_data_WG$age, na.rm = TRUE)
)
norwegian_WS_aoas <- fit_aoa(
  norwegian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(norwegian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(norwegian_instrument_data_WS$age, na.rm = TRUE)
)
norwegian_aoas <- bind_rows(norwegian_WG_aoas,
                         norwegian_WS_aoas) %>%
  left_join(norwegian_ratings_subset, by = c("item_definition" = "Norwegian"))

n_norwegian <- norwegian_instrument_data %>% distinct(child_id) %>% nrow() # 7358


# persian ----
persian_instrument_data_WG <- get_instrument_data("Persian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
persian_instrument_data_WS <- get_instrument_data("Persian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

persian_instrument_data <- bind_rows(persian_instrument_data_WG, 
                                       persian_instrument_data_WS) %>%
  as.data.frame() %>%
  left_join(persian_ratings_subset, by = c("item_definition" = "Persian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) 
write_rds(persian_instrument_data, "norms/persian/persian_instrument_data.rds")


persian_WG_aoas <- fit_aoa(
  persian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(persian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(persian_instrument_data_WG$age, na.rm = TRUE)
)
persian_WS_aoas <- fit_aoa(
  persian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(persian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(persian_instrument_data_WS$age, na.rm = TRUE)
)
persian_aoas <- bind_rows(persian_WG_aoas,
                          persian_WS_aoas) %>%
  left_join(persian_ratings_subset, by = c("item_definition" = "Persian"))

n_persian <- persian_instrument_data %>% distinct(child_id) %>% nrow() # 163


# portuguese ----
portuguese_instrument_data_WG <- get_instrument_data("Portuguese (European)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
portuguese_instrument_data_WS <- get_instrument_data("Portuguese (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

portuguese_instrument_data <- bind_rows(portuguese_instrument_data_WG, 
                                     portuguese_instrument_data_WS) %>%
  left_join(portuguese_ratings_subset, by = c("item_definition" = "Portuguese (European)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(portuguese_instrument_data, "norms/portuguese/portuguese_instrument_data.rds")


portuguese_WG_aoas <- fit_aoa(
  portuguese_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(portuguese_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(portuguese_instrument_data_WG$age, na.rm = TRUE)
)
portuguese_WS_aoas <- fit_aoa(
  portuguese_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(portuguese_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(portuguese_instrument_data_WS$age, na.rm = TRUE)
)
portuguese_aoas <- bind_rows(portuguese_WG_aoas,
                         portuguese_WS_aoas) %>%
  left_join(portuguese_ratings_subset, by = c("item_definition" = "Portuguese (European)"))
n_portuguese <- portuguese_instrument_data %>% distinct(child_id) %>% nrow() # 4326


# russian ----
russian_instrument_data_WG <- get_instrument_data("Russian", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
russian_instrument_data_WS <- get_instrument_data("Russian", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

russian_instrument_data <- bind_rows(russian_instrument_data_WG, 
                                     russian_instrument_data_WS) %>%
  left_join(russian_ratings_subset, by = c("item_definition" = "Russian"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(russian_instrument_data, "norms/russian/russian_instrument_data.rds")


russian_WG_aoas <- fit_aoa(
  russian_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(russian_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(russian_instrument_data_WG$age, na.rm = TRUE)
)
russian_WS_aoas <- fit_aoa(
  russian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(russian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(russian_instrument_data_WS$age, na.rm = TRUE)
)
russian_aoas <- bind_rows(russian_WG_aoas,
                         russian_WS_aoas) %>%
  left_join(russian_ratings_subset, by = c("item_definition" = "Russian"))

n_russian <- russian_instrument_data %>% distinct(child_id) %>% nrow() # 1805

# slovak ----
slovak_instrument_data_WG <- get_instrument_data("Slovak", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
slovak_instrument_data_WS <- get_instrument_data("Slovak", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

slovak_instrument_data <- bind_rows(slovak_instrument_data_WG, 
                                     slovak_instrument_data_WS) %>%
  left_join(slovak_ratings_subset, by = c("item_definition" = "Slovak"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(slovak_instrument_data, "norms/slovak/slovak_instrument_data.rds")

slovak_WG_aoas <- fit_aoa(
  slovak_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(slovak_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(slovak_instrument_data_WG$age, na.rm = TRUE)
)
slovak_WS_aoas <- fit_aoa(
  slovak_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(slovak_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(slovak_instrument_data_WS$age, na.rm = TRUE)
)
slovak_aoas <- bind_rows(slovak_WG_aoas,
                          slovak_WS_aoas) %>%
  left_join(slovak_ratings_subset, by = c("item_definition" = "Slovak"))


n_slovak <- slovak_instrument_data %>% distinct(child_id) %>% nrow() # 1712

# spanish ----

spanish_argentinian_instrument_data_WS <- get_instrument_data("Spanish (Argentinian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces) 

spanish_argentinian_instrument_data <- spanish_argentinian_instrument_data_WS%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Argentinian)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(spanish_argentinian_instrument_data, "norms/spanish/spanish_argentinian_instrument_data.rds")

spanish_argentinian_aoas <- fit_aoa(
  spanish_argentinian_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(spanish_argentinian_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(spanish_argentinian_instrument_data_WS$age, na.rm = TRUE)
) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Argentinian)"))

spanish_chilean_instrument_data_WG <- get_instrument_data("Spanish (Chilean)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

spanish_chilean_instrument_data <- spanish_chilean_instrument_data_WG%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Chilean)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(spanish_chilean_instrument_data, "norms/spanish/spanish_chilean_instrument_data.rds")

spanish_chilean_aoas <- fit_aoa(
  spanish_chilean_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(spanish_chilean_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(spanish_chilean_instrument_data_WG$age, na.rm = TRUE)
) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Chilean)"))


spanish_european_instrument_data_WG <- get_instrument_data("Spanish (European)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_european_instrument_data_WS <- get_instrument_data("Spanish (European)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

spanish_european_instrument_data_no_ratings <- bind_rows(spanish_european_instrument_data_WG, 
                                              spanish_european_instrument_data_WS) 

spanish_european_instrument_data <- spanish_european_instrument_data_no_ratings %>%
  mutate(produces = as.factor(produces))%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (European)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(spanish_european_instrument_data, "norms/spanish/spanish_european_instrument_data.rds")

spanish_european_aoas <- fit_aoa(
  spanish_european_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(spanish_european_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(spanish_european_instrument_data_no_ratings$age, na.rm = TRUE)
) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (European)"))


spanish_mexican_instrument_data_WG <-  get_instrument_data("Spanish (Mexican)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_mexican_instrument_data_WS <-  get_instrument_data("Spanish (Mexican)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_mexican_instrument_data_no_ratings <- bind_rows(spanish_mexican_instrument_data_WG, 
                                              spanish_mexican_instrument_data_WS) 
spanish_mexican_instrument_data<- spanish_mexican_instrument_data_no_ratings%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Mexican)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(spanish_mexican_instrument_data, "norms/spanish/spanish_mexican_instrument_data.rds")
spanish_mexican_aoas <- fit_aoa(
  spanish_mexican_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(spanish_mexican_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(spanish_mexican_instrument_data_no_ratings$age, na.rm = TRUE)
) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Mexican)"))


spanish_peruvian_instrument_data_WG <-  get_instrument_data("Spanish (Peruvian)", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_peruvian_instrument_data_WS <-  get_instrument_data("Spanish (Peruvian)", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
spanish_peruvian_instrument_data_no_ratings <- bind_rows(spanish_peruvian_instrument_data_WG, 
                                              spanish_peruvian_instrument_data_WS) 
spanish_peruvian_instrument_data <- spanish_peruvian_instrument_data_no_ratings%>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Peruvian)"))%>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(spanish_peruvian_instrument_data, "norms/spanish/spanish_peruvian_instrument_data.rds")
spanish_peruvian_aoas <- fit_aoa(
  spanish_peruvian_instrument_data_no_ratings,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(spanish_peruvian_instrument_data_no_ratings$age, na.rm = TRUE),
  age_max = max(spanish_peruvian_instrument_data_no_ratings$age, na.rm = TRUE)
) %>%
  left_join(spanish_ratings_subset, by = c("item_definition" = "Spanish (Peruvian)"))


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
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(swedish_instrument_data, "norms/swedish/swedish_instrument_data.rds")

swedish_WG_aoas <- fit_aoa(
  swedish_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(swedish_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(swedish_instrument_data_WG$age, na.rm = TRUE)
)
swedish_WS_aoas <- fit_aoa(
  swedish_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(swedish_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(swedish_instrument_data_WS$age, na.rm = TRUE)
)
swedish_aoas <- bind_rows(swedish_WG_aoas,
                          swedish_WS_aoas) %>%
  left_join(swedish_ratings_subset, by = c("item_definition" = "Swedish"))




n_swedish <- swedish_instrument_data %>% distinct(child_id) %>% nrow() # 1357

# turkish ----
turkish_instrument_data_WG <- get_instrument_data("Turkish", form = "WG", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)
turkish_instrument_data_WS <- get_instrument_data("Turkish", form = "WS", administration_info = TRUE,item_info = TRUE) %>%
  drop_na(produces)

turkish_instrument_data <- bind_rows(turkish_instrument_data_WG, 
                                     turkish_instrument_data_WS) %>%
  left_join(turkish_ratings_subset, by = c("item_definition" = "Turkish")) %>%
  mutate(produces = as.factor(produces),
         word_length = str_length(item_definition)) %>%
  as.data.frame()
write_rds(turkish_instrument_data, "norms/turkish/turkish_instrument_data.rds")


turkish_WG_aoas <- fit_aoa(
  turkish_instrument_data_WG,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(turkish_instrument_data_WG$age, na.rm = TRUE),
  age_max = max(turkish_instrument_data_WG$age, na.rm = TRUE)
)
turkish_WS_aoas <- fit_aoa(
  turkish_instrument_data_WS,
  measure = "produces",
  method = "glm",
  proportion = 0.5,
  age_min = min(turkish_instrument_data_WS$age, na.rm = TRUE),
  age_max = max(turkish_instrument_data_WS$age, na.rm = TRUE)
)
turkish_aoas <- bind_rows(turkish_WG_aoas,
                          turkish_WS_aoas) %>%
  left_join(turkish_ratings_subset, by = c("item_definition" = "Turkish"))



turkish_aoas <- bind_rows(turkish_WG_aoas,
                          turkish_WS_aoas) %>%
  left_join(turkish_ratings_subset, by = c("item_definition" = "Turkish"))
n_turkish <- turkish_instrument_data %>% distinct(child_id) %>% nrow() # 3537


# N participants ----
n_participants <- n_arabic+
  n_asl +
n_bsl +
  n_catalan +
  n_chinese +
  n_croatian +
  n_czech +
  n_danish +
  n_dutch +
  n_english +
  n_estonian +
  n_finnish +
  n_french +
  n_german +
  n_greek +
  n_hebrew +
  n_hungarian +
  n_irish +
  n_italian +
  n_japanese +
  n_kigiriama +
  n_kiswahili +
  n_latvian +
  n_norwegian +
  n_persian +
  n_portuguese +
  n_russian +
  n_slovak +
  n_spanish +
  n_swedish +
  n_turkish
write_rds(n_participants, "models/n_participants.rds")

# all_instrument_data <- bind_rows(asl_instrument_data,
#                       bsl_instrument_data,
#                       cantonese_instrument_data,
#                       mandarin_beijing_instrument_data,
#                       mandarin_taiwanese_instrument_data,
#                       croatian_instrument_data,
#                       danish_instrument_data,
#                       dutch_instrument_data,
#                       american_english_instrument_data,
#                       australian_english_instrument_data,
#                       british_english_instrument_data,
#                       irish_english_instrument_data,
#                       finnish_instrument_data,
#                       french_european_instrument_data,
#                       french_quebecois_instrument_data,
#                       german_instrument_data,
#                       greek_instrument_data,
#                       hebrew_instrument_data,
#                       hungarian_instrument_data,
#                       irish_instrument_data,
#                       italian_instrument_data,
#                       kiswahili_instrument_data,
#                       korean_instrument_data,
#                       latvian_instrument_data,
#                       norwegian_instrument_data,
#                       persian_instrument_data,
#                       portuguese_instrument_data,
#                       russian_instrument_data,
#                       slovak_instrument_data,
#                       spanish_argentinian_instrument_data,
#                       spanish_chilean_instrument_data,
#                       spanish_european_instrument_data,
#                       spanish_peruvian_instrument_data,
#                       spanish_mexican_instrument_data,
#                       swedish_instrument_data,
#                       turkish_instrument_data)
