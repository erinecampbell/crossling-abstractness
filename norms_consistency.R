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


# asl ----
asl_ratings<- read.csv("norms/asl/ASL-LEX_Data.csv") %>%
  dplyr::rename(english_gloss = english_gloss_clean,
                word = Entry.ID) %>%
  mutate(asl_iconicity_rating = case_when(
    Deaf.Signer.Iconicity == "N/A" ~ Non.Signer.Iconicity,
    Deaf.Signer.Iconicity != "N/A" ~ Deaf.Signer.Iconicity
  )) %>%
  filter(asl_iconicity_rating != "N/A") %>%
  mutate(asl_iconicity_rating = as.numeric(asl_iconicity_rating),
         asl_frequency_rating = as.numeric(Frequency),
         word = toupper(word)) %>%
  mutate(asl_iconicity_rating = rescale_ratings(asl_iconicity_rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         asl_frequency_rating = rescale_ratings(asl_frequency_rating, old_min=1,old_max = 7,new_min = 1, new_max = 10)) %>%
  select(word, asl_iconicity_rating, asl_frequency_rating)

# croatian ----

croatian_ratings <- read_excel("norms/croatian/Pretraga-22.3.2023.xlsx") %>%
  mutate(KONKRETNOST = as.numeric(KONKRETNOST),
         PREDOČIVOST = as.numeric(PREDOČIVOST),
         RIJEČ = tolower(RIJEČ)) %>%
  group_by(RIJEČ) %>%
  summarise(croatian_concreteness_rating = mean(KONKRETNOST, na.rm=TRUE),
            croatian_imageability_rating = mean(PREDOČIVOST, na.rm=TRUE)) %>%
  mutate(word = RIJEČ,
         croatian_concreteness_rating = rescale_ratings(croatian_concreteness_rating,0,5,1,10),
         croatian_imageability_rating = rescale_ratings(croatian_imageability_rating, 1,5,1,10)) %>%
  select(word, croatian_concreteness_rating, croatian_imageability_rating)


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
  mutate_at(vars(c(dutch_auditory_rating:dutch_interoceptive_rating,dutch_maxperceptual_rating,dutch_imageability_rating)), ~ rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(dutch_auditory_rating:dutch_interoceptive_rating,dutch_maxperceptual_rating,dutch_imageability_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

dutch_concreteness <- read.csv("norms/dutch/verheyen_concreteness2019.csv") %>%
  mutate(word = Words,
         rating = rescale_ratings(mean,1,7,1,10),
         rating_type="dutch_concreteness_rating") %>%
  select(word, rating, rating_type)

mega_dutch <- bind_rows(dutch_concreteness, dutch_perceptual) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating)

# english ----

english_iconicity<- read.csv("norms/english/iconicity_ratings_cleaned.csv") %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10)) %>%
  mutate(rating_type = "english_iconicity_rating") %>%
  select(word, rating, rating_type)

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
  select(word, rating, rating_type)

english_concreteness <- read.csv("norms/english/brysbaert_concreteness.csv") %>%
  mutate(word = Word,
         rating = rescale_ratings(Conc.M,1,5,1,10),
         rating_type="english_concreteness_rating") %>%
  select(word, rating, rating_type)
english_bois <- read_csv("norms/english/CBOI_mean_sd.csv") %>%
  mutate(word = tolower(Word),
         rating = rescale_ratings(CBOI_Mean, 1, 7 , 1, 10),
         rating_type = "english_boi_rating") %>%
  select(word, rating, rating_type)

mega_english <- bind_rows(english_bois, english_concreteness, english_iconicity,english_perceptual) %>%
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













# italian ----
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
  mutate(english_gloss = tolower(english_gloss)) %>%
  mutate_at(vars(c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

italian_ratings <- read_csv("norms/italian/italian_ratings.csv", 
                            skip = 1) %>%
  rename(word = Ita_Word,
         italian_concreteness_rating = M_Con,
         italian_imageability_rating = M_Ima) %>%
  mutate_at(vars(c(italian_concreteness_rating,italian_imageability_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 9, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_concreteness_rating,italian_imageability_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

mega_italian <- bind_rows(italian_perceptual, italian_ratings) %>%
  filter(!is.na(word)) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating)



# mandarin ----
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
  mutate(english_gloss = tolower(english_gloss))  %>%
  mutate_at(vars(c(chinese_auditory_rating:chinese_interoceptive_rating,chinese_maxperceptual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(chinese_auditory_rating:chinese_interoceptive_rating,chinese_maxperceptual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type) 

# norwegian ----
norwegian_imageability <- read_excel("norms/norwegian/norwegian_imageability.xlsx") %>%
  mutate(word = Word,
         norwegian_imageability_rating = rescale_ratings(Imageability,1,8,1,10)) %>%
  select(word, norwegian_imageability_rating)

# portuguese ----
portuguese_concreteness <- soares_norms <- read_csv("norms/portuguese/european/soares_norms.csv") %>%
  rename(word = `Word (Portuguese)`,
         portuguese_concreteness_rating = Conc_M,
         portuguese_imageability_rating = Imag_M) %>%
  mutate_at(vars(c(portuguese_concreteness_rating,portuguese_imageability_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) 

#  russian ----
russian_perceptual <- read_excel("norms/russian/miklashevsky2018.xlsx") %>%
  dplyr::rename(word = WORD, 
                english_gloss = English_Translation,
                russian_auditory_rating = Aud_Mean,
                russian_visual_rating = Vis_Mean,
                russian_olfactory_rating = Olf_Mean,
                russian_gustatory_rating = Gus_Mean,
                russian_haptic_rating = Hap_Mean,
                russian_imageability_rating = Img_Mean) %>%
  mutate(english_gloss = tolower(english_gloss),
         russian_boi_rating = rescale_ratings(Man_Mean, 1,7,1,10),
         russian_imageability_rating = rescale_ratings(russian_imageability_rating, 1,7,1,10))  %>%
  mutate_at(vars(c(russian_auditory_rating:russian_haptic_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(russian_auditory_rating:russian_haptic_rating,russian_imageability_rating,russian_boi_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  dplyr::select(word, rating, rating_type) %>%  
  pivot_wider(names_from = rating_type,
              values_from = rating)

# spanish ----
spanish_perceptual <- read_excel("norms/spanish/europeanspanish/sensoryexperienceratings_diezalamo2019.xlsx") %>%
  dplyr::rename(spanish_word = word, 
                english_gloss = `translation into English`,
                rating = SER_m) %>%
  mutate(english_gloss = tolower(english_gloss),
         word = tolower(spanish_word),
         rating_type = "spanish_sensory_experience_rating") %>%
  select(word, rating, rating_type)

spanish_concreteness <- read_xlsx("norms/spanish/europeanspanish/Hinojosa et al_Supplementary materials.xlsx") %>%
  mutate(word = Word,
         english_gloss = Word_english,
         rating = Con_Mn,
         rating_type = "spanish_concreteness") %>%
  select(word, rating, rating_type)

spanish_imageability <- read_excel("norms/spanish/ConceptAttributesSpanish.xlsx") %>%
  mutate(word = tolower(Spanish),
         spanish_gustatory_rating = rescale_ratings(taste_m,1,8,1,10),
         spanish_auditory_rating = rescale_ratings(sound_m,1,8,1,10),
         spanish_olfactory_rating = rescale_ratings(smell_m, 1,8,1,10),
         spanish_imageability_rating = rescale_ratings(Imageability, 1,7,1,10)) %>%
  pivot_longer(cols = c (spanish_gustatory_rating,
                         spanish_auditory_rating, 
                         spanish_olfactory_rating,
                         spanish_imageability_rating),
               names_to = "rating_type",
               values_to = "rating") %>%
  select(word, rating, rating_type) 

spanish_bois <- read_excel("norms/spanish/Appendix_2_BOI_Spanish.xlsx") %>%
  mutate(word = tolower(Word),
         rating = rescale_ratings(Mean, 1,7,1,10),
         rating_type = "spanish_boi_rating") %>%
  select(word, rating, rating_type)

spanish_iconicity <- read_xlsx("norms/spanish/europeanspanish/word_ratings.xlsx") %>%
  dplyr::rename(rating = `ico-m`) %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         rating_type = "spanish_iconicity")  %>%
  select(word, rating, rating_type)

mega_spanish <- bind_rows(spanish_bois, 
                          spanish_concreteness, 
                          spanish_iconicity, 
                          spanish_imageability, 
                          spanish_perceptual) %>%  
  pivot_wider(names_from = rating_type,
              values_from = rating)





# chinese_concreteness <-  read_excel("norms/chinese/yao_2017.pdf") %>%







# get CDI words ----

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






# mega mega ----


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
                                     TRUE ~ uni_lemma),
         "Spanish (all)" = case_when(!is.na(`Spanish (Mexican)`) ~ `Spanish (Mexican)`,
                                     !is.na(`Spanish (Argentinian)`) ~ `Spanish (Argentinian)`,
                                     !is.na(`Spanish (Chilean)`) ~ `Spanish (Chilean)`,
                                     !is.na(`Spanish (European)`) ~ `Spanish (European)`,
                                     !is.na(`Spanish (Peruvian)`) ~ `Spanish (Peruvian)`,
                                     TRUE ~ uni_lemma))
CDI_mega_word_list_with_ratings <- CDI_mega_dictionary %>% 
  left_join(mega_english, by=c("English (all)" = "word")) %>%
  left_join(mega_dutch, by=c("Dutch" = "word")) %>%
  left_join(mega_french, by = c("French (all)" = "word")) %>%
  left_join(mega_spanish, by = c("Spanish (all)" = "word")) %>%
  left_join(russian_perceptual, by = c("Russian" = "word")) %>%
  left_join(mega_italian, by = c("Italian" = "word")) %>%
  left_join(portuguese_concreteness, by = c(`Portuguese (European)` = "word")) %>%
  left_join(asl_ratings, by = c(`American Sign Language` = "word")) %>%
  left_join(croatian_ratings, by = c("Croatian" = "word")) %>%
  left_join(norwegian_imageability, by = c("Norwegian" = "word")) %>%
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
  ungroup() %>%
  # add concreteness ratings
  mutate(asl_imageability_rating = average_imageability_rating,
         bsl_imageability_rating = average_imageability_rating,
         croatian_imageability_rating = case_when(!is.na(croatian_imageability_rating) ~ croatian_imageability_rating,
                                                  TRUE ~ average_imageability_rating),
         czech_imageability_rating = average_imageability_rating,
         danish_imageability_rating = average_imageability_rating,
         dutch_imageability_rating = case_when(!is.na(dutch_imageability_rating) ~ dutch_imageability_rating,
                                               TRUE ~ average_imageability_rating),
         english_imageability_rating = average_imageability_rating,
         finnish_imageability_rating = average_imageability_rating,
         french_imageability_rating = case_when(!is.na(french_imageability_rating) ~ french_imageability_rating,
                                               TRUE ~ average_imageability_rating),
         german_imageability_rating = average_imageability_rating,
         greek_imageability_rating = average_imageability_rating,
         hebrew_imageability_rating = average_imageability_rating,
         hungarian_imageability_rating = average_imageability_rating,
         irish_imageability_rating = average_imageability_rating,
         italian_imageability_rating = case_when(!is.na(italian_imageability_rating) ~ italian_imageability_rating,
                                                 TRUE ~ average_imageability_rating),
         kirigiama_imageability_rating = average_imageability_rating,
         kiswahili_imageability_rating = average_imageability_rating,
         korean_imageability_rating = average_imageability_rating,
         latvian_imageability_rating = average_imageability_rating,
         norwegian_imageability_rating = case_when(!is.na(norwegian_imageability_rating) ~ norwegian_imageability_rating,
                                                   TRUE ~ average_imageability_rating),
         persian_imageability_rating = average_imageability_rating,
         portuguese_imageability_rating = average_imageability_rating,
         russian_imageability_rating = average_imageability_rating,
         slovak_imageability_rating = average_imageability_rating,
         spanish_imageability_rating = case_when(!is.na(spanish_imageability_rating) ~ spanish_imageability_rating,
                                                 TRUE ~ average_imageability_rating),
         swedish_imageability_rating = average_imageability_rating,
         turkish_imageability_rating = average_imageability_rating
  ) %>%
  # add concreteness ratings
  mutate(asl_concreteness_rating = average_concreteness_rating,
         bsl_concreteness_rating = average_concreteness_rating,
         croatian_concreteness_rating = case_when(!is.na(croatian_concreteness_rating) ~ croatian_concreteness_rating,
                                                  TRUE ~ average_concreteness_rating),
         czech_concreteness_rating = average_concreteness_rating,
         danish_concreteness_rating = average_concreteness_rating,
         dutch_concreteness_rating = case_when(!is.na(dutch_concreteness_rating) ~ dutch_concreteness_rating,
                                               TRUE ~ average_concreteness_rating),
         english_concreteness_rating = case_when(!is.na(english_concreteness_rating) ~ english_concreteness_rating,
                                                 TRUE ~ average_concreteness_rating),
         finnish_concreteness_rating = average_concreteness_rating,
         french_concreteness_rating = case_when(!is.na(french_concreteness_rating) ~ french_concreteness_rating,
                                                TRUE ~ average_concreteness_rating),
         german_concreteness_rating = average_concreteness_rating,
         greek_concreteness_rating = average_concreteness_rating,
         hebrew_concreteness_rating = average_concreteness_rating,
         hungarian_concreteness_rating = average_concreteness_rating,
         irish_concreteness_rating = average_concreteness_rating,
         italian_concreteness_rating = case_when(!is.na(italian_concreteness_rating) ~ italian_concreteness_rating,
                                                 TRUE ~ average_concreteness_rating),
         kirigiama_concreteness_rating = average_concreteness_rating,
         kiswahili_concreteness_rating = average_concreteness_rating,
         korean_concreteness_rating = average_concreteness_rating,
         latvian_concreteness_rating = average_concreteness_rating,
         norwegian_concreteness_rating = average_concreteness_rating,
         persian_concreteness_rating = average_concreteness_rating,
         portuguese_concreteness_rating = case_when(!is.na(portuguese_concreteness_rating) ~ portuguese_concreteness_rating,
                                                    TRUE ~ average_concreteness_rating),
         russian_concreteness_rating = average_concreteness_rating,
         slovak_concreteness_rating = average_concreteness_rating,
         spanish_concreteness_rating = average_concreteness_rating,
         swedish_concreteness_rating = average_concreteness_rating,
         turkish_concreteness_rating = average_concreteness_rating
  ) %>%
  # add boi ratings
  mutate(asl_boi_rating = average_boi_rating,
         bsl_boi_rating = average_boi_rating,
         croatian_boi_rating = average_boi_rating,
         czech_boi_rating = average_boi_rating,
         danish_boi_rating = average_boi_rating,
         dutch_boi_rating = average_boi_rating,
         english_boi_rating = case_when(!is.na(english_boi_rating) ~ english_boi_rating,
                                                 TRUE ~ average_boi_rating),
         finnish_boi_rating = average_boi_rating,
         french_boi_rating = case_when(!is.na(french_boi_rating) ~ french_boi_rating,
                                                TRUE ~ average_boi_rating),
         german_boi_rating = average_boi_rating,
         greek_boi_rating = average_boi_rating,
         hebrew_boi_rating = average_boi_rating,
         hungarian_boi_rating = average_boi_rating,
         irish_boi_rating = average_boi_rating,
         italian_boi_rating = average_boi_rating,
         kirigiama_boi_rating = average_boi_rating,
         kiswahili_boi_rating = average_boi_rating,
         korean_boi_rating = average_boi_rating,
         latvian_boi_rating = average_boi_rating,
         norwegian_boi_rating = average_boi_rating,
         persian_boi_rating = average_boi_rating,
         portuguese_boi_rating = average_boi_rating,
         russian_boi_rating = case_when(!is.na(russian_boi_rating) ~ russian_boi_rating,
                                        TRUE ~ average_boi_rating),
         slovak_boi_rating = average_boi_rating,
         spanish_boi_rating = case_when(!is.na(spanish_boi_rating) ~ spanish_boi_rating,
                                        TRUE ~ average_boi_rating),
         swedish_boi_rating = average_boi_rating,
         turkish_boi_rating = average_boi_rating
    
  )
  


asl_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `American Sign Language`,
         matches("^asl_.*_rating$"))
bsl_ratings_subset<- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `British Sign Language`,
         matches("^bsl_.*_rating$"))
chinese_ratings_subset <- CDI_mega_word_list_with_ratings %>%
  select(uni_lemma,
         Cantonese,
         `Mandarin (Beijing)`,
         `Mandarin (Taiwanese)`,
         matches("^chinese_.*_rating$"))
croatian_ratings_subset<- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Croatian`,
         matches("^croatian_.*_rating$"))
czech_ratings_subset<- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Czech`,
         matches("^czech_.*_rating$"))
danish_ratings_subset<- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Danish`,
         matches("^danish_.*_rating$"))
dutch_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Dutch`,
         matches("^dutch_.*_rating$"))
english_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `English (American)`, 
         `English (Australian)`, 
         `English (British)`, 
         `English (Irish)`,
         matches("^english_.*_rating$"))
finnish_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Finnish`,
         matches("^finnish_.*_rating$"))
french_ratings_subset <- CDI_mega_word_list_with_ratings %>%
  select(uni_lemma, 
         `French (French)`,
         `French (Quebecois)`,
         matches("^french_.*_rating$"))
german_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `German`,
         matches("^german_.*_rating$"))
greek_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Greek (Cypriot)`,
         matches("^greek_.*_rating$"))
hebrew_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Hebrew`,
         matches("^hebrew_.*_rating$"))
hungarian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Hungarian`,
         matches("^hungarian_.*_rating$"))
irish_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Irish`,
         matches("^irish_.*_rating$"))
italian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Italian`,
         matches("^italian_.*_rating$"))
kigiriama_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Kigiriama`,
         matches("^kigiriama_.*_rating$"))
kiswahili_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Kiswahili`,
         matches("^kiswahili_.*_rating$"))
korean_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Korean`,
         matches("^korean_.*_rating$"))
latvian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Latvian`,
         matches("^latvian_.*_rating$"))
norwegian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Norwegian`,
         matches("^norwegian_.*_rating$"))
persian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Persian`,
         matches("^persian_.*_rating$"))
portuguese_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Portuguese (European)`,
         matches("^portuguese_.*_rating$"))
russian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Russian`,
         matches("^russian_.*_rating$"))
slovak_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Slovak`,
         matches("^slovak_.*_rating$"))
spanish_ratings_subset <- CDI_mega_word_list_with_ratings %>%
  select(uni_lemma, 
         `Spanish (Argentinian)`,
         `Spanish (Chilean)`,
         `Spanish (European)`,
         `Spanish (Mexican)`,
         `Spanish (Peruvian)`,
         matches("^spanish_.*_rating$"))
swedish_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Swedish`,
         matches("^swedish_.*_rating$"))
turkish_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Turkish`,
         matches("^turkish_.*_rating$"))
