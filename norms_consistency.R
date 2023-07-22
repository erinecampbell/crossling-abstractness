library(tidyverse)
library(readxl)
library(corrplot)
library(ggrepel)


rescale_ratings <- function(value, old_min,old_max,new_min = 1, new_max = 10){
  
  position <- (value - old_min) / (old_max - old_min)
  
  
  new_value <- position * (new_max - new_min) + new_min
  
  new_value
}

# make a long table with one column language, one column rating type, one column rating, one column word, one column unilemma


# english ----

english_iconicity<- read.csv("norms/english/iconicity_ratings_cleaned.csv") %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10)) %>%
  mutate(language = "english_word",
         rating_type = "english_iconicity_rating") %>%
  select(word, rating, language, rating_type)

english_perceptual <- read_csv("norms/english/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  dplyr::rename(
    english_auditory_rating = Auditory.mean,
    english_visual_rating = Visual.mean,
    english_olfactory_rating = Olfactory.mean,
    english_gustatory_rating = Gustatory.mean,
    english_haptic_rating = Haptic.mean,
    english_interoceptive_rating = Interoceptive.mean,
    english_dominant_perceptual_rating = Dominant.perceptual,
    english_exclusivity_rating = Exclusivity.perceptual,
    english_maxperceptual_rating = Max_strength.perceptual) %>%
  mutate(word = tolower(Word)) %>%
  mutate_at(vars(c(english_auditory_rating:english_interoceptive_rating,english_maxperceptual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(english_auditory_rating:english_interoceptive_rating,english_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)

english_concreteness <- read.csv("norms/english/brysbaert_concreteness.csv") %>%
  mutate(word = Word,
         rating = rescale_ratings(Conc.M,1,5,1,10),
         rating_type="english_concreteness_rating") %>%
  select(word, language, rating, rating_type)
english_bois <- read_csv("norms/english/CBOI_mean_sd.csv") %>%
  mutate(word = tolower(Word),
         rating = rescale_ratings(CBOI_Mean, 1, 7 , 1, 10),
         rating_type = "english_boi_rating") %>%
  select(word, language, rating, rating_type)

mega_english <- bind_rows(english_bois, english_concreteness, english_iconicity,english_perceptual) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating)

# dutch ----
dutch_perceptual <- read_xlsx("norms/dutch/SpeedBrysbaert_Norms.xlsx") %>%
  dplyr::rename(word = Woord, 
                dutch_auditory_rating = Horen,
                dutch_visual_rating = Zien,
                dutch_olfactory_rating = Ruiken,
                dutch_gustatory_rating = Proeven,
                dutch_haptic_rating = Voelen,
                dutch_interoceptive_rating = Sensaties,
                dutch_dominant_perceptual_rating = Modality,
                dutch_exclusivity_rating = ModalityExclusivity,
                dutch_maxperceptual_rating = MaxPercStrength,
                dutch_imageability_rating = Imageability) %>%
  mutate(language = "dutch_word") %>%
  mutate_at(vars(c(dutch_auditory_rating:dutch_interoceptive_rating,dutch_maxperceptual_rating)), ~ rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(dutch_auditory_rating:dutch_interoceptive_rating,dutch_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)

dutch_concreteness <- read.csv("norms/dutch/verheyen_concreteness2019.csv") %>%
  mutate(word = Words,
         language = "dutch_word",
         rating = rescale_ratings(mean,1,7,1,10),
         rating_type="dutch_concreteness_rating") %>%
  select(word, language, rating, rating_type)

mega_dutch <- bind_rows(dutch_concreteness, dutch_perceptual) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating)

# french ----

french_perceptual <- read_excel("norms/french/europeanfrench/perceptualinteroceptive_miceli2021.xlsx")%>%
  dplyr::rename(word = MOT, 
                english_gloss = WORD,
                french_auditory_rating = Auditory_Mean,
                french_visual_rating = Visual_Mean,
                french_olfactory_rating = Olfactory_Mean,
                french_gustatory_rating = Gustatory_Mean,
                french_haptic_rating = Haptic_Mean,
                french_dominant_perceptual_rating = `Dominant Modality`,
                french_exclusivity_rating = `Modality Exclusivity (%)`) %>%
  mutate(english_gloss = tolower(english_gloss)) %>%
  mutate_at(vars(c(french_auditory_rating:french_haptic_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10))  %>%
  pivot_longer(cols = french_auditory_rating:french_haptic_rating, names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

french_concreteness <- read_excel("norms/french/europeanfrench/concreteness_bonin2018.xlsx") %>%
  mutate(word = items,
         rating = rescale_ratings(Concreteness_mean,1,5,1,10),
         rating_type = "french_concreteness_rating") %>%
  select(word, rating, rating_type)

french_imageability <- read_excel("norms/french/europeanfrench/Desrochers-BRM-2009/Desrochers-Thompson_2009_Ratings.xls") %>%
  mutate(word = NOUN,
         rating = rescale_ratings(IMAGE_Mean, 1,7,1,10),
         rating_type = "french_imageability_rating") %>%
  select(word, rating, rating_type)

french_bois <- read_csv("norms/french/europeanfrench/miceli2021_BOI.csv") %>%
  mutate(word = tolower(MOT),
         rating = rescale_ratings(Mean_BOI, 0,5,1,10),
         rating_type = "french_boi_rating") %>%
  select(word, rating, rating_type)

mega_french <- bind_rows(french_bois, 
                         french_concreteness, 
                         french_imageability, 
                         french_perceptual)  %>%
  distinct(rating,rating_type,.keep_all = TRUE) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating)

# spanish ----
spanish_iconicity <- read_xlsx("norms/spanish/europeanspanish/word_ratings.xlsx") %>%
  dplyr::rename(rating = `ico-m`) %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         language = "spanish_word",
         rating_type = "spanish_iconicity")  %>%
  select(word, rating, language, rating_type)

# asl ----

asl_iconicity<- read.csv("norms/asl/ASL-LEX_Data.csv") %>%
  dplyr::rename(english_gloss = english_gloss_clean,
                word = Entry.ID) %>%
  mutate(rating = case_when(
    Deaf.Signer.Iconicity == "N/A" ~ Non.Signer.Iconicity,
    Deaf.Signer.Iconicity != "N/A" ~ Deaf.Signer.Iconicity
  )) %>%
  filter(rating != "N/A") %>%
  mutate(rating = as.numeric(rating)) %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         rating_type = "asl_iconicity",
         language = "asl_word") %>%
  select(word, rating, language, rating_type)










# emotion ratings ----






# perceptual ratings ----


## italian
italian_perceptual <- read_delim("norms/italian/Italian_Perceptual_Norms.txt", delim=" ")%>%
  dplyr::rename(word = Ita_Word, 
                english_gloss = Eng_Word,
                italian_auditory_rating = Auditory,
                italian_visual_rating = Visual,
                italian_olfactory_rating = Olfactory,
                italian_gustatory_rating = Gustatory,
                italian_haptic_rating = Haptic,
                italian_dominant_perceptual_rating = mod_e,
                italian_maxperceptual_rating = max,
                italian_exclusivity_rating = mod_exc) %>%
  mutate(english_gloss = tolower(english_gloss),
         language = "italian_word") %>%
  mutate_at(vars(c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)
## mandarin
chinese_perceptual <- read_excel("norms/chinese/SensorimotorNormsforChineseNouns.xlsx") %>%
  dplyr::rename(word = pinyin, 
                english_gloss = English_translation,
                chinese_auditory_rating = auditory,
                chinese_visual_rating = visual,
                chinese_olfactory_rating = olfactory,
                chinese_gustatory_rating = gustatory,
                chinese_haptic_rating = tactile,
                chinese_interoceptive_rating = interoceptive,
                chinese_dominant_perceptual_rating = dominant_modality,
                chinese_maxperceptual_rating = max_perceptual,
                chinese_exclusivity_rating = modality_exclusivity) %>%
  mutate(english_gloss = tolower(english_gloss),
         language = "chinese_word")  %>%
  mutate_at(vars(c(chinese_auditory_rating:chinese_interoceptive_rating,chinese_maxperceptual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(chinese_auditory_rating:chinese_interoceptive_rating,chinese_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)
## russian
russian_perceptual <- read_excel("norms/russian/miklashevsky2018.xlsx") %>%
  dplyr::rename(word = Transliteration, 
                english_gloss = English_Translation,
                russian_auditory_rating = Aud_Mean,
                russian_visual_rating = Vis_Mean,
                russian_olfactory_rating = Olf_Mean,
                russian_gustatory_rating = Gus_Mean,
                russian_haptic_rating = Hap_Mean,
                russian_imageability_rating = Img_Mean) %>%
  mutate(english_gloss = tolower(english_gloss),
         language = "russian_word",
         russian_boi_rating = rescale_ratings(Man_Mean, 1,7,1,10),
         russian_imageability_rating = rescale_ratings(russian_imageability_rating, 1,7,1,10))  %>%
  mutate_at(vars(c(russian_auditory_rating:russian_haptic_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(russian_auditory_rating:russian_haptic_rating,russian_imageability_rating,russian_boi_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)
## spanish
spanish_perceptual <- read_excel("norms/spanish/europeanspanish/sensoryexperienceratings_diezalamo2019.xlsx") %>%
  dplyr::rename(spanish_word = word, 
                english_gloss = `translation into English`,
                rating = SER_m) %>%
  mutate(english_gloss = tolower(english_gloss),
         word = tolower(spanish_word),
         language = "spanish_word",
         rating_type = "spanish_sensory_experience_rating") %>%
  select(word, language, rating, rating_type)

## mega perceptual
mega_perceptual <- bind_rows(
                             chinese_perceptual,
                             italian_perceptual,
                             russian_perceptual,
                             spanish_perceptual)

# concreteness ----



spanish_concreteness <- read_xlsx("norms/spanish/europeanspanish/Hinojosa et al_Supplementary materials.xlsx") %>%
  mutate(word = Word,
                english_gloss = Word_english,
                rating = Con_Mn,
         rating_type = "spanish_concreteness",
         language = "spanish_word") %>%
  select(word, language, rating, rating_type)
# chinese_concreteness <-  read_excel("norms/chinese/yao_2017.pdf") %>%
portuguese_concreteness <- soares_norms <- read_csv("norms/portuguese/european/soares_norms.csv") %>%
  rename(word = `Word (Portuguese)`,
         portuguese_concreteness_rating = Conc_M,
         portuguese_imageability_rating = Imag_M) %>%
  mutate(language = "portuguese_word")  %>% 
  mutate_at(vars(c(portuguese_concreteness_rating,portuguese_imageability_rating)), ~ 
                                                     rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(portuguese_concreteness_rating,portuguese_imageability_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)
italian_ratings <- read_csv("norms/italian/italian_ratings.csv", 
                            skip = 1) %>%
  rename(word = Ita_Word,
         italian_concreteness_rating = M_Con,
         italian_imageability_rating = M_Ima) %>%
  mutate(language = "italian_word")  %>% 
  mutate_at(vars(c(italian_concreteness_rating,italian_imageability_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 9, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_concreteness_rating,italian_imageability_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)

mega_concreteness <- bind_rows(english_concreteness, dutch_concreteness,
                      portuguese_concreteness,
                               spanish_concreteness)
  
  # imageability ----

croatian_ratings <- read_excel("norms/croatian/Pretraga-22.3.2023.xlsx") %>%
  mutate(KONKRETNOST = as.numeric(KONKRETNOST),
         PREDOČIVOST = as.numeric(PREDOČIVOST),
         RIJEČ = tolower(RIJEČ)) %>%
  group_by(RIJEČ) %>%
  summarise(croatian_concreteness_rating = mean(KONKRETNOST, na.rm=TRUE),
            croatian_imageability_rating = mean(PREDOČIVOST, na.rm=TRUE)) %>%
  mutate(word = RIJEČ,
         language = "croatian_word",
         croatian_concreteness_rating = rescale_ratings(croatian_concreteness_rating,0,5,1,10),
         croatian_imageability_rating = rescale_ratings(croatian_imageability_rating, 1,5,1,10)) %>%
  pivot_longer(cols = c(croatian_concreteness_rating,croatian_imageability_rating), 
               names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, language, rating, rating_type)
norwegian_imageability <- read_excel("norms/norwegian/norwegian_imageability.xlsx") %>%
  mutate(word = Word,
         language = "norwegian_word",
         rating_type = "norwegian_imageability_rating",
         rating = rescale_ratings(Imageability,1,8,1,10)) %>%
  select(word, language, rating, rating_type)
spanish_imageability <- read_excel("norms/spanish/ConceptAttributesSpanish.xlsx") %>%
  mutate(word = tolower(Spanish),
         spanish_boi_rating = rescale_ratings(BOI, 1,7,1,10),
         spanish_gustatory_rating = rescale_ratings(taste_m,1,8,1,10),
         spanish_auditory_rating = rescale_ratings(sound_m,1,8,1,10),
         spanish_olfactory_rating = rescale_ratings(smell_m, 1,8,1,10),
         spanish_imageability_rating = rescale_ratings(Imageability, 1,7,1,10),
         language = "spanish_word") %>%
  pivot_longer(cols = c (spanish_boi_rating,
                         spanish_gustatory_rating,
                         spanish_auditory_rating, 
                         spanish_olfactory_rating,
                         spanish_imageability_rating),
               names_to = "rating_type",
               values_to = "rating") %>%
  select(word, language, rating, rating_type)

mega_imageability <- bind_rows(croatian_ratings, norwegian_imageability, spanish_imageability)

# BOIs ----

spanish_bois <- read_excel("norms/spanish/Appendix_2_BOI_Spanish.xlsx") %>%
  mutate(word = tolower(Word),
         language = "spanish_word",
         rating = rescale_ratings(Mean, 1,7,1,10),
         rating_type = "spanish_boi_rating") %>%
  select(word, language, rating, rating_type)


mega_bois <- bind_rows( english_bois, spanish_bois)

# mega mega ----
mega_ratings <- bind_rows(mega_bois, mega_concreteness,
                          mega_iconicity, mega_imageability, mega_perceptual) %>%
  distinct()

CDI_instrument_list <- wordbankr::get_instruments()


  
  # Assuming you have a dataframe named CDI_instrument_list
  # with columns "language" and "form"
  
  # Create an empty dataframe to store the results
CDI_mega_list <- data.frame()

# Loop through each row in CDI_instrument_list
for (i in 1:nrow(CDI_instrument_list)) {
  # Get the language and form for the current row
  language <- CDI_instrument_list$language[i]
  form <- CDI_instrument_list$form[i]
  
  # Display a message for each iteration with the current language and form
  cat("Processing Language:", language, ", Form:", form, "\n")
  
  # Call get_instrument_data and bind the result to result_df
  current_result <- get_instrument_data(language, form = form, administration_info = FALSE, item_info = TRUE) %>%
    drop_na(produces) %>%
    distinct(uni_lemma, language, form, .keep_all = TRUE)
  
  CDI_mega_list <- bind_rows(CDI_mega_list, current_result)
}
write.csv(CDI_mega_list, "norms/CDI_mega_list.csv")
# Print the final result dataframe
CDI_mega_word_list <- CDI_mega_list %>% 
  select(item_definition,uni_lemma,language,form, item_kind, lexical_category) %>% 
  distinct()
CDI_mega_dictionary <- CDI_mega_word_list %>% 
  select(-form) %>%
  arrange(str_length(item_definition)) %>%
  distinct(uni_lemma, language, .keep_all = TRUE) %>%
  pivot_wider(names_from = language, values_from = item_definition) %>%
  mutate("English (all)" = case_when(!is.na(`English (American)`) ~ `English (American)`,
                                     !is.na(`English (British)`) ~ `English (British)`,
                                     !is.na(`English (Australian)`) ~ `English (Australian)`,
                                     !is.na(`English (Irish)`) ~ `English (Irish)`,
                                     TRUE ~ uni_lemma),
         "French (all)" = case_when(!is.na(`French (French)`) ~ `French (French)`,
                                    !is.na(`French (Quebecois)`) ~ `French (Quebecois)`,      
                                     TRUE ~ uni_lemma))
CDI_mega_word_list_with_ratings <- CDI_mega_dictionary %>% 
  left_join(mega_english, by=c("English (all)" = "word")) %>%
  left_join(mega_dutch, by=c("Dutch" = "word")) %>%
  left_join(mega_french, by = c("French (all)" = "word"))
