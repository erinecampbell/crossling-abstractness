library(tidyverse)
library(readxl)
library(corrplot)
library(ggrepel)
library(wordbankr)


rescale_ratings <- function(value, old_min,old_max,new_min = 1, new_max = 10){
  
  position <- (value - old_min) / (old_max - old_min)
  
  
  new_value <- position * (new_max - new_min) + new_min
  
  new_value
}

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
         asl_phoncomp_rating = as.numeric(Phonological.Complexity),
         word = toupper(word)) %>%
  mutate(asl_iconicity_rating = rescale_ratings(asl_iconicity_rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         asl_frequency_rating = rescale_ratings(asl_frequency_rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         asl_phoncomp_rating = rescale_ratings(asl_phoncomp_rating, old_min = 0, old_max = 7)) %>%
  select(word, asl_iconicity_rating, asl_frequency_rating, asl_phoncomp_rating)

# croatian ----

croatian_ratings <- read_excel("norms/croatian/Pretraga-22.3.2023.xlsx") %>%
  mutate(KONKRETNOST = as.numeric(KONKRETNOST),
         PREDOČIVOST = as.numeric(PREDOČIVOST),
         UČESTALOST = as.numeric(UČESTALOST),
         RIJEČ = tolower(RIJEČ)) %>%
  group_by(RIJEČ) %>%
  summarise(croatian_concreteness_rating = mean(KONKRETNOST, na.rm=TRUE),
            croatian_imageability_rating = mean(PREDOČIVOST, na.rm=TRUE),
            croatian_frequency_rating = mean(UČESTALOST, na.rm = TRUE)) %>%
  mutate(word = RIJEČ,
         croatian_concreteness_rating = rescale_ratings(croatian_concreteness_rating,0,5,1,10),
         croatian_imageability_rating = rescale_ratings(croatian_imageability_rating, 1,5,1,10),
         croatian_frequency_rating = rescale_ratings(croatian_frequency_rating, 0,5,1,10)) %>%
  select(word, croatian_concreteness_rating, croatian_imageability_rating, croatian_frequency_rating)


# dutch ----
dutch_frequency <- read_csv("norms/dutch/SUBTLEX_dutch_filtered.csv") %>%
  mutate(word = tolower(Word)) 

dutch_emotionalarousal <- read_csv("norms/dutch/verheyen_emotionalarousal2019.csv") %>%
  dplyr::rename(word = Words,
                dutch_emotionalarousal_rating = mean) %>%
  select(word, dutch_emotionalarousal_rating)

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
              values_from = rating) %>%
  left_join(dutch_emotionalarousal)

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
  mutate_at(vars(c(english_auditory_rating:english_interoceptive_rating,english_maxperceptual_rating,english_olfactory_rating,english_visual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(english_auditory_rating:english_interoceptive_rating,english_maxperceptual_rating,english_olfactory_rating,english_visual_rating), names_to = "rating_type", 
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

english_socialness <- read.csv("norms/english/SocialnessNorms_DiveicaPexmanBinney2021.csv") %>%
  select(Word, Mean) %>%
  mutate(word = tolower(Word),
         rating = rescale_ratings(Mean, 1, 7 , 1, 10),
         rating_type = "english_socialness_rating")



mega_english <- bind_rows(english_bois, english_concreteness, english_iconicity,english_perceptual, english_socialness) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) 
# estonian ----

mega_estonian <- read_csv("norms/estonian/data.csv") %>%
  dplyr::rename(word = lemma,
                estonian_concreteness_rating = concreteness_index,
                estonian_emotionalarousal_rating = activation_index,
                estonian_visual_rating = seeing_index,
                estonian_auditory_rating = hearing_index,
                estonian_gustatory_rating = tasting_index,
                estonian_olfactory_rating = smelling_index,
                estonian_haptic_rating = touching_index) %>%
  mutate_at(vars(c(estonian_concreteness_rating, estonian_emotionalarousal_rating, estonian_auditory_rating, estonian_visual_rating, estonian_olfactory_rating, 
                   estonian_gustatory_rating, estonian_haptic_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 10, new_min = 1, new_max = 10))  %>%
  select(word, estonian_concreteness_rating, estonian_emotionalarousal_rating, estonian_visual_rating,
         estonian_auditory_rating, estonian_gustatory_rating, estonian_olfactory_rating, estonian_haptic_rating)
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
  mutate_at(vars(c(french_auditory_rating, french_visual_rating, french_olfactory_rating, 
                   french_gustatory_rating, french_haptic_rating,french_exclusivity_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10))  %>%
  pivot_longer(cols = c(french_auditory_rating, french_visual_rating, french_olfactory_rating, 
                        french_gustatory_rating, french_haptic_rating,french_exclusivity_rating), names_to = "rating_type", 
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

french_emotionalarousal <- read.csv("norms/french/europeanfrench/FANCatdatabase_emotionalarousal.csv") %>%
  dplyr::rename(word = French,
                french_emotionalarousal_rating = ArousalMean) %>%
  mutate(french_emotionalarousal_rating = rescale_ratings(french_emotionalarousal_rating, 2,8,1,10))

mega_french <- bind_rows(french_bois, 
                         french_concreteness, 
                         french_imageability, 
                         french_emotionalarousal,
                         french_perceptual)  %>%
  distinct(rating,rating_type,.keep_all = TRUE) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) 

# greek


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
  mutate_at(vars(c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating,italian_olfactory_rating, italian_visual_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_auditory_rating:italian_haptic_rating,italian_maxperceptual_rating,italian_olfactory_rating, italian_visual_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

italian_ratings <- read_csv("norms/italian/italian_ratings.csv", 
                            skip = 1) %>%
  rename(word = Ita_Word,
         italian_concreteness_rating = M_Con,
         italian_imageability_rating = M_Ima,
         italian_emotionalarousal_rating = M_Aro) %>%
  mutate_at(vars(c(italian_concreteness_rating,italian_imageability_rating, italian_emotionalarousal_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 9, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(italian_concreteness_rating,italian_imageability_rating, italian_emotionalarousal_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)



mega_italian <- bind_rows(italian_perceptual, italian_ratings) %>%
  filter(!is.na(word)) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) 

# japanese ----

japanese_iconicity_ratings <- read_csv("norms/japanese/Japanese iconicity ratings.csv") %>%
  group_by(word,wordCode) %>%
  summarise(japanese_iconicity_rating = mean(rating, na.rm=TRUE)) %>%
  select(-wordCode) %>%
  mutate(japanese_iconicity_rating = rescale_ratings(japanese_iconicity_rating, old_min = -5, old_max = 5, new_min = 1, new_max = 10))


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
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) 

# chinese_concreteness <-  read_excel("norms/chinese/yao_2017.pdf") %>%


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
  mutate_at(vars(c(russian_auditory_rating:russian_haptic_rating,russian_visual_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(russian_auditory_rating:russian_haptic_rating,russian_imageability_rating,russian_boi_rating,russian_visual_rating), names_to = "rating_type", 
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
         rating_type = "spanish_iconicity_rating")  %>%
  select(word, rating, rating_type)


mega_spanish <- bind_rows(spanish_bois, 
                          spanish_concreteness, 
                          spanish_iconicity, 
                          spanish_imageability, 
                          spanish_perceptual) %>%  
  pivot_wider(names_from = rating_type,
              values_from = rating) 

# swedish ----
mega_swedish <- read_excel("norms/swedish/blomberg_2015.xlsx") %>%
  mutate_at(vars(c(swedish_imageability_rating,swedish_emotionalarousal_rating)), ~ 
              rescale_ratings(., old_min = 100, old_max = 700, new_min = 1, new_max = 10)) 

# turkish ----
turkish_emotionalarousal <- read_csv("norms/turkish/torkamani-azar_emotionalarousal2019.csv", 
                            skip = 5) %>%
  dplyr::rename(word = `Turkish Word`,
                turkish_emotionalarousal_rating = AroMn) %>% 
  mutate(turkish_emotionalarousal_rating = rescale_ratings(turkish_emotionalarousal_rating,1,9,1,10))



# wikipedia data ----

croatian_CD <- read_delim("norms/croatian/hr_wordfreq.tsv", delim = " ",col_names = c("Croatian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(croatian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         croatian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Croatian, croatian_CD_rating, croatian_freq_rating)

czech_CD <- read_delim("norms/czech/cs_wordfreq.tsv", delim = " ",col_names = c("Czech", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(czech_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         czech_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Czech, czech_CD_rating, czech_freq_rating)

danish_CD <- read_delim("norms/danish/da_wordfreq.tsv", delim = " ",col_names = c("Danish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(danish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         danish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Danish, danish_CD_rating, danish_freq_rating)

dutch_CD <- read_delim("norms/dutch/nl_wordfreq.tsv", delim = " ",col_names = c("Dutch", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(dutch_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         dutch_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Dutch, dutch_CD_rating, dutch_freq_rating)

english_CD <- read_delim("norms/english/en_wordfreq.tsv", delim = " ",col_names = c("English (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(english_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         english_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`English (all)`, english_CD_rating,english_freq_rating)

finnish_CD <- read_delim("norms/finnish/fi_wordfreq.tsv", delim = " ",col_names = c("Finnish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(finnish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         finnish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Finnish, finnish_CD_rating, finnish_freq_rating)

french_CD <- read_delim("norms/french/fr_wordfreq.tsv", delim = " ",col_names = c("French (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(french_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         french_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`French (all)`, french_CD_rating, french_freq_rating)

german_CD <- read_delim("norms/german/de_wordfreq.tsv", delim = " ",col_names = c("German", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(german_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         german_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(German, german_CD_rating, german_freq_rating)

greek_CD <- read_delim("norms/greek/el_wordfreq.tsv", delim = " ",col_names = c("Greek (Cypriot)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(greek_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         greek_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Greek (Cypriot)`, greek_CD_rating, greek_freq_rating)

hebrew_CD <- read_delim("norms/hebrew/he_wordfreq.tsv", delim = " ",col_names = c("Hebrew", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(hebrew_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         hebrew_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Hebrew, hebrew_CD_rating, hebrew_freq_rating)

hungarian_CD <- read_delim("norms/hungarian/hu_wordfreq.tsv", delim = " ",col_names = c("Hungarian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(hungarian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         hungarian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Hungarian, hungarian_CD_rating, hungarian_freq_rating)

irish_CD <- read_delim("norms/irish/ga_wordfreq.tsv", delim = " ",col_names = c("Irish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(irish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         irish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Irish, irish_CD_rating, irish_freq_rating)

italian_CD <- read_delim("norms/italian/it_wordfreq.tsv", delim = " ",col_names = c("Italian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(italian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         italian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Italian, italian_CD_rating, italian_freq_rating)

chinese_CD <- read_delim("norms/chinese/zh_wordfreq.tsv", delim = " ",col_names = c("Chinese (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(chinese_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         chinese_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Chinese (all)`, chinese_CD_rating, chinese_freq_rating)

kiswahili_CD <- read_delim("norms/kiswahili/sw_wordfreq.tsv", delim = " ",col_names = c("Kiswahili", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(kiswahili_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         kiswahili_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Kiswahili, kiswahili_CD_rating, kiswahili_freq_rating)

korean_CD <- read_delim("norms/korean/ko_wordfreq.tsv", delim = " ",col_names = c("Korean", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(korean_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         korean_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Korean, korean_CD_rating, korean_freq_rating)

latvian_CD <- read_delim("norms/latvian/lv_wordfreq.tsv", delim = " ",col_names = c("Latvian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(latvian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         latvian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Latvian, latvian_CD_rating, latvian_freq_rating)

norwegian_CD <- read_delim("norms/norwegian/no_wordfreq.tsv", delim = " ",col_names = c("Norwegian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(norwegian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         norwegian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Norwegian, norwegian_CD_rating, norwegian_freq_rating)

persian_CD <- read_delim("norms/persian/fa_wordfreq.tsv", delim = " ",col_names = c("Persian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(persian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         persian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Persian, persian_CD_rating, persian_freq_rating)

portuguese_CD <- read_delim("norms/portuguese/pt_wordfreq.tsv", delim = " ",col_names = c("Portuguese (European)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(portuguese_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         portuguese_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Portuguese (European)`, portuguese_CD_rating, portuguese_freq_rating)

russian_CD <- read_delim("norms/russian/ru_wordfreq.tsv", delim = " ",col_names = c("Russian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(russian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         russian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Russian, russian_CD_rating, russian_freq_rating)

slovak_CD <- read_delim("norms/slovak/sk_wordfreq.tsv", delim = " ",col_names = c("Slovak", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(slovak_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         slovak_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Slovak, slovak_CD_rating, slovak_freq_rating)

spanish_CD <- read_delim("norms/spanish/es_wordfreq.tsv", delim = " ",col_names = c("Spanish (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(spanish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         spanish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Spanish (all)`, spanish_CD_rating, spanish_freq_rating)

swedish_CD <- read_delim("norms/swedish/sv_wordfreq.tsv", delim = " ",col_names = c("Swedish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(swedish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         swedish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Swedish, swedish_CD_rating, swedish_freq_rating)

turkish_CD <- read_delim("norms/turkish/tr_wordfreq.tsv", delim = " ",col_names = c("Turkish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(turkish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         turkish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Turkish, turkish_CD_rating, turkish_freq_rating)



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
                                     TRUE ~ uni_lemma),
         "Chinese (all)" = case_when(!is.na(`Mandarin (Beijing)`) ~ `Mandarin (Beijing)`,
                                    !is.na(`Mandarin (Taiwanese)`) ~ `Mandarin (Taiwanese)`, 
                                    !is.na(`Cantonese`) ~ `Cantonese`, 
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
  left_join(chinese_perceptual, by = c("Chinese (all)" = "word")) %>%
  left_join(mega_swedish, by = c("Swedish" = "word")) %>%
  left_join(mega_estonian, by=c("Estonian" = "word")) %>%
  left_join(turkish_emotionalarousal, by = c("Turkish" = "word")) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
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
         average_boi_rating = mean(c_across(matches("^.*_boi_rating$")), na.rm = TRUE),
         average_socialness_rating = mean(c_across(matches("^.*_socialness_rating$")), na.rm = TRUE),
         average_emotionalarousal_rating = mean(c_across(matches("^.*_emotionalarousal_rating$")), na.rm = TRUE)) %>%
  ungroup() %>%
  # add imageability ratings
  mutate(arabic_imageability_rating = average_imageability_rating,
    asl_imageability_rating = average_imageability_rating,
         bsl_imageability_rating = average_imageability_rating,
    catalan_imageability_rating = average_imageability_rating,
         chinese_imageability_rating = average_imageability_rating,
         croatian_imageability_rating = case_when(!is.na(croatian_imageability_rating) ~ croatian_imageability_rating,
                                                  TRUE ~ average_imageability_rating),
         czech_imageability_rating = average_imageability_rating,
         danish_imageability_rating = average_imageability_rating,
         dutch_imageability_rating = case_when(!is.na(dutch_imageability_rating) ~ dutch_imageability_rating,
                                               TRUE ~ average_imageability_rating),
         english_imageability_rating = average_imageability_rating,
          estonian_imageability_rating = average_imageability_rating,
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
    japanese_imageability_rating = average_imageability_rating,
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
    swedish_imageability_rating = case_when(!is.na(swedish_imageability_rating) ~ swedish_imageability_rating,
                                            TRUE ~ average_imageability_rating),
         turkish_imageability_rating = average_imageability_rating
  ) %>%
  # add concreteness ratings
  mutate(arabic_concreteness_rating = average_concreteness_rating,
    asl_concreteness_rating = average_concreteness_rating,
         bsl_concreteness_rating = average_concreteness_rating,
    catalan_concreteness_rating = average_concreteness_rating,
         chinese_concreteness_rating = average_concreteness_rating,
         croatian_concreteness_rating = case_when(!is.na(croatian_concreteness_rating) ~ croatian_concreteness_rating,
                                                  TRUE ~ average_concreteness_rating),
         czech_concreteness_rating = average_concreteness_rating,
         danish_concreteness_rating = average_concreteness_rating,
         dutch_concreteness_rating = case_when(!is.na(dutch_concreteness_rating) ~ dutch_concreteness_rating,
                                               TRUE ~ average_concreteness_rating),
         english_concreteness_rating = case_when(!is.na(english_concreteness_rating) ~ english_concreteness_rating,
                                                 TRUE ~ average_concreteness_rating),
    estonian_concreteness_rating = case_when(!is.na(estonian_concreteness_rating) ~ estonian_concreteness_rating,
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
    japanese_concreteness_rating = average_concreteness_rating,
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
  mutate(arabic_boi_rating = average_boi_rating,
         asl_boi_rating = average_boi_rating,
         bsl_boi_rating = average_boi_rating,
         catalan_boi_rating = average_boi_rating,
         chinese_boi_rating = average_boi_rating,
         croatian_boi_rating = average_boi_rating,
         czech_boi_rating = average_boi_rating,
         danish_boi_rating = average_boi_rating,
         dutch_boi_rating = average_boi_rating,
         english_boi_rating = case_when(!is.na(english_boi_rating) ~ english_boi_rating,
                                                 TRUE ~ average_boi_rating),
         estonian_boi_rating = average_boi_rating,
         finnish_boi_rating = average_boi_rating,
         french_boi_rating = case_when(!is.na(french_boi_rating) ~ french_boi_rating,
                                                TRUE ~ average_boi_rating),
         german_boi_rating = average_boi_rating,
         greek_boi_rating = average_boi_rating,
         hebrew_boi_rating = average_boi_rating,
         hungarian_boi_rating = average_boi_rating,
         irish_boi_rating = average_boi_rating,
         italian_boi_rating = average_boi_rating,
         japanese_boi_rating = average_boi_rating, 
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
    
  ) %>%
  # add auditory ratings
  mutate(arabic_auditory_rating = average_auditory_rating, 
         asl_auditory_rating = average_auditory_rating,
         bsl_auditory_rating = average_auditory_rating,
         catalan_auditory_rating = average_auditory_rating,
         chinese_auditory_rating = case_when(!is.na(chinese_auditory_rating) ~ chinese_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         croatian_auditory_rating = average_auditory_rating,
         czech_auditory_rating = average_auditory_rating,
         danish_auditory_rating = average_auditory_rating,
         dutch_auditory_rating = case_when(!is.na(dutch_auditory_rating) ~ dutch_auditory_rating,
                                           TRUE ~ average_auditory_rating),
         english_auditory_rating = case_when(!is.na(english_auditory_rating) ~ english_auditory_rating,
                                        TRUE ~ average_auditory_rating),
         estonian_auditory_rating = case_when(!is.na(estonian_auditory_rating) ~ estonian_auditory_rating,
                                              TRUE ~ average_auditory_rating),
         finnish_auditory_rating = average_auditory_rating,
         french_auditory_rating = case_when(!is.na(french_auditory_rating) ~ french_auditory_rating,
                                       TRUE ~ average_auditory_rating),
         german_auditory_rating = average_auditory_rating,
         greek_auditory_rating = average_auditory_rating,
         hebrew_auditory_rating = average_auditory_rating,
         hungarian_auditory_rating = average_auditory_rating,
         irish_auditory_rating = average_auditory_rating,
         italian_auditory_rating = case_when(!is.na(italian_auditory_rating) ~ italian_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         japanese_auditory_rating = average_auditory_rating,
         kirigiama_auditory_rating = average_auditory_rating,
         kiswahili_auditory_rating = average_auditory_rating,
         korean_auditory_rating = average_auditory_rating,
         latvian_auditory_rating = average_auditory_rating,
         norwegian_auditory_rating = average_auditory_rating,
         persian_auditory_rating = average_auditory_rating,
         portuguese_auditory_rating = average_auditory_rating,
         russian_auditory_rating = case_when(!is.na(russian_auditory_rating) ~ russian_auditory_rating,
                                        TRUE ~ average_auditory_rating),
         slovak_auditory_rating = average_auditory_rating,
         spanish_auditory_rating = case_when(!is.na(spanish_auditory_rating) ~ spanish_auditory_rating,
                                        TRUE ~ average_auditory_rating),
         swedish_auditory_rating = average_auditory_rating,
         turkish_auditory_rating = average_auditory_rating
         
  ) %>%
  # add gustatory ratings
  mutate(arabic_gustatory_rating = average_gustatory_rating, 
         asl_gustatory_rating = average_gustatory_rating,
         bsl_gustatory_rating = average_gustatory_rating,
         catalan_gustatory_rating = average_gustatory_rating,
         chinese_gustatory_rating = case_when(!is.na(chinese_gustatory_rating) ~ chinese_gustatory_rating,
                                           TRUE ~ average_gustatory_rating),
         croatian_gustatory_rating = average_gustatory_rating,
         czech_gustatory_rating = average_gustatory_rating,
         danish_gustatory_rating = average_gustatory_rating,
         dutch_gustatory_rating = case_when(!is.na(dutch_gustatory_rating) ~ dutch_gustatory_rating,
                                           TRUE ~ average_gustatory_rating),
         english_gustatory_rating = case_when(!is.na(english_gustatory_rating) ~ english_gustatory_rating,
                                             TRUE ~ average_gustatory_rating),
         estonian_gustatory_rating = case_when(!is.na(estonian_gustatory_rating) ~ estonian_gustatory_rating,
                                               TRUE ~ average_gustatory_rating),
         finnish_gustatory_rating = average_gustatory_rating,
         french_gustatory_rating = case_when(!is.na(french_gustatory_rating) ~ french_gustatory_rating,
                                            TRUE ~ average_gustatory_rating),
         german_gustatory_rating = average_gustatory_rating,
         greek_gustatory_rating = average_gustatory_rating,
         hebrew_gustatory_rating = average_gustatory_rating,
         hungarian_gustatory_rating = average_gustatory_rating,
         irish_gustatory_rating = average_gustatory_rating,
         italian_gustatory_rating = case_when(!is.na(italian_gustatory_rating) ~ italian_gustatory_rating,
                                             TRUE ~ average_gustatory_rating),
         japanese_gustatory_rating = average_gustatory_rating,
         kirigiama_gustatory_rating = average_gustatory_rating,
         kiswahili_gustatory_rating = average_gustatory_rating,
         korean_gustatory_rating = average_gustatory_rating,
         latvian_gustatory_rating = average_gustatory_rating,
         norwegian_gustatory_rating = average_gustatory_rating,
         persian_gustatory_rating = average_gustatory_rating,
         portuguese_gustatory_rating = average_gustatory_rating,
         russian_gustatory_rating = case_when(!is.na(russian_gustatory_rating) ~ russian_gustatory_rating,
                                             TRUE ~ average_gustatory_rating),
         slovak_gustatory_rating = average_gustatory_rating,
         spanish_gustatory_rating = case_when(!is.na(spanish_gustatory_rating) ~ spanish_gustatory_rating,
                                             TRUE ~ average_gustatory_rating),
         swedish_gustatory_rating = average_gustatory_rating,
         turkish_gustatory_rating = average_gustatory_rating
         
  ) %>%
  # add haptic ratings
  mutate(arabic_haptic_rating = average_haptic_rating,
         asl_haptic_rating = average_haptic_rating,
         bsl_haptic_rating = average_haptic_rating,
         catalan_haptic_rating = average_haptic_rating,
         chinese_haptic_rating = case_when(!is.na(chinese_haptic_rating) ~ chinese_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         croatian_haptic_rating = average_haptic_rating,
         czech_haptic_rating = average_haptic_rating,
         danish_haptic_rating = average_haptic_rating,
         dutch_haptic_rating = case_when(!is.na(dutch_haptic_rating) ~ dutch_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         english_haptic_rating = case_when(!is.na(english_haptic_rating) ~ english_haptic_rating,
                                             TRUE ~ average_haptic_rating),
         estonian_haptic_rating = case_when(!is.na(estonian_haptic_rating) ~ estonian_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         finnish_haptic_rating = average_haptic_rating,
         french_haptic_rating = case_when(!is.na(french_haptic_rating) ~ french_haptic_rating,
                                            TRUE ~ average_haptic_rating),
         german_haptic_rating = average_haptic_rating,
         greek_haptic_rating = average_haptic_rating,
         hebrew_haptic_rating = average_haptic_rating,
         hungarian_haptic_rating = average_haptic_rating,
         irish_haptic_rating = average_haptic_rating,
         italian_haptic_rating = case_when(!is.na(italian_haptic_rating) ~ italian_haptic_rating,
                                             TRUE ~ average_haptic_rating),
         japanese_haptic_rating = average_haptic_rating,
         kirigiama_haptic_rating = average_haptic_rating,
         kiswahili_haptic_rating = average_haptic_rating,
         korean_haptic_rating = average_haptic_rating,
         latvian_haptic_rating = average_haptic_rating,
         norwegian_haptic_rating = average_haptic_rating,
         persian_haptic_rating = average_haptic_rating,
         portuguese_haptic_rating = average_haptic_rating,
         russian_haptic_rating = case_when(!is.na(russian_haptic_rating) ~ russian_haptic_rating,
                                             TRUE ~ average_haptic_rating),
         slovak_haptic_rating = average_haptic_rating,
         spanish_haptic_rating = average_haptic_rating,
         swedish_haptic_rating = average_haptic_rating,
         turkish_haptic_rating = average_haptic_rating
  ) %>%
  # add interoceptive ratings
  mutate(arabic_interoceptive_rating = average_interoceptive_rating, 
         asl_interoceptive_rating = average_interoceptive_rating,
         bsl_interoceptive_rating = average_interoceptive_rating,
         catalan_interoceptive_rating = average_interoceptive_rating,
         chinese_interoceptive_rating = case_when(!is.na(chinese_interoceptive_rating) ~ chinese_interoceptive_rating,
                                           TRUE ~ average_interoceptive_rating),
         croatian_interoceptive_rating = average_interoceptive_rating,
         czech_interoceptive_rating = average_interoceptive_rating,
         danish_interoceptive_rating = average_interoceptive_rating,
         dutch_interoceptive_rating = case_when(!is.na(dutch_interoceptive_rating) ~ dutch_interoceptive_rating,
                                           TRUE ~ average_interoceptive_rating),
         english_interoceptive_rating = case_when(!is.na(english_interoceptive_rating) ~ english_interoceptive_rating,
                                             TRUE ~ average_interoceptive_rating),
         estonian_interoceptive_rating = average_interoceptive_rating,
         finnish_interoceptive_rating = average_interoceptive_rating,
         french_interoceptive_rating =  average_interoceptive_rating,
         german_interoceptive_rating = average_interoceptive_rating,
         greek_interoceptive_rating = average_interoceptive_rating,
         hebrew_interoceptive_rating = average_interoceptive_rating,
         hungarian_interoceptive_rating = average_interoceptive_rating,
         irish_interoceptive_rating = average_interoceptive_rating,
         italian_interoceptive_rating = average_interoceptive_rating,
         japanese_interoceptive_rating = average_interoceptive_rating,
         kirigiama_interoceptive_rating = average_interoceptive_rating,
         kiswahili_interoceptive_rating = average_interoceptive_rating,
         korean_interoceptive_rating = average_interoceptive_rating,
         latvian_interoceptive_rating = average_interoceptive_rating,
         norwegian_interoceptive_rating = average_interoceptive_rating,
         persian_interoceptive_rating = average_interoceptive_rating,
         portuguese_interoceptive_rating = average_interoceptive_rating,
         russian_interoceptive_rating = average_interoceptive_rating,
         slovak_interoceptive_rating = average_interoceptive_rating,
         spanish_interoceptive_rating = average_interoceptive_rating,
         swedish_interoceptive_rating = average_interoceptive_rating,
         turkish_interoceptive_rating = average_interoceptive_rating
         
  ) %>%
  # add olfactory ratings
  mutate(arabic_olfactory_rating = average_olfactory_rating, 
         asl_olfactory_rating = average_olfactory_rating,
         bsl_olfactory_rating = average_olfactory_rating,
         catalan_olfactory_rating = average_olfactory_rating,
         chinese_olfactory_rating = case_when(!is.na(chinese_olfactory_rating) ~ chinese_olfactory_rating,
                                           TRUE ~ average_olfactory_rating),
         croatian_olfactory_rating = average_olfactory_rating,
         czech_olfactory_rating = average_olfactory_rating,
         danish_olfactory_rating = average_olfactory_rating,
         dutch_olfactory_rating = case_when(!is.na(dutch_olfactory_rating) ~ dutch_olfactory_rating,
                                           TRUE ~ average_olfactory_rating),
         english_olfactory_rating = case_when(!is.na(english_olfactory_rating) ~ english_olfactory_rating,
                                             TRUE ~ average_olfactory_rating),
         estonian_olfactory_rating = case_when(!is.na(estonian_olfactory_rating) ~ estonian_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         finnish_olfactory_rating = average_olfactory_rating,
         french_olfactory_rating = case_when(!is.na(french_olfactory_rating) ~ french_olfactory_rating,
                                            TRUE ~ average_olfactory_rating),
         german_olfactory_rating = average_olfactory_rating,
         greek_olfactory_rating = average_olfactory_rating,
         hebrew_olfactory_rating = average_olfactory_rating,
         hungarian_olfactory_rating = average_olfactory_rating,
         irish_olfactory_rating = average_olfactory_rating,
         italian_olfactory_rating = case_when(!is.na(italian_olfactory_rating) ~ italian_olfactory_rating,
                                             TRUE ~ average_olfactory_rating),
         japanese_olfactory_rating = average_olfactory_rating,
         kirigiama_olfactory_rating = average_olfactory_rating,
         kiswahili_olfactory_rating = average_olfactory_rating,
         korean_olfactory_rating = average_olfactory_rating,
         latvian_olfactory_rating = average_olfactory_rating,
         norwegian_olfactory_rating = average_olfactory_rating,
         persian_olfactory_rating = average_olfactory_rating,
         portuguese_olfactory_rating = average_olfactory_rating,
         russian_olfactory_rating = case_when(!is.na(russian_olfactory_rating) ~ russian_olfactory_rating,
                                             TRUE ~ average_olfactory_rating),
         slovak_olfactory_rating = average_olfactory_rating,
         spanish_olfactory_rating = case_when(!is.na(spanish_olfactory_rating) ~ spanish_olfactory_rating,
                                             TRUE ~ average_olfactory_rating),
         swedish_olfactory_rating = average_olfactory_rating,
         turkish_olfactory_rating = average_olfactory_rating
         
  ) %>%
  # add visual ratings
  mutate(arabic_visual_rating = average_visual_rating,
         asl_visual_rating = average_visual_rating,
         bsl_visual_rating = average_visual_rating,
         catalan_visual_rating = average_visual_rating,
         chinese_visual_rating = case_when(!is.na(chinese_visual_rating) ~ chinese_visual_rating,
                                         TRUE ~ average_visual_rating),
         croatian_visual_rating = average_visual_rating,
         czech_visual_rating = average_visual_rating,
         danish_visual_rating = average_visual_rating,
         dutch_visual_rating = case_when(!is.na(dutch_visual_rating) ~ dutch_visual_rating,
                                           TRUE ~ average_visual_rating),
         english_visual_rating = case_when(!is.na(english_visual_rating) ~ english_visual_rating,
                                             TRUE ~ average_visual_rating),
         estonian_visual_rating = case_when(!is.na(estonian_visual_rating) ~ estonian_visual_rating,
                                           TRUE ~ average_visual_rating),
         finnish_visual_rating = average_visual_rating,
         french_visual_rating = case_when(!is.na(french_visual_rating) ~ french_visual_rating,
                                            TRUE ~ average_visual_rating),
         german_visual_rating = average_visual_rating,
         greek_visual_rating = average_visual_rating,
         hebrew_visual_rating = average_visual_rating,
         hungarian_visual_rating = average_visual_rating,
         irish_visual_rating = average_visual_rating,
         italian_visual_rating = case_when(!is.na(italian_visual_rating) ~ italian_visual_rating,
                                             TRUE ~ average_visual_rating),
         japanese_visual_rating = average_visual_rating,
         kirigiama_visual_rating = average_visual_rating,
         kiswahili_visual_rating = average_visual_rating,
         korean_visual_rating = average_visual_rating,
         latvian_visual_rating = average_visual_rating,
         norwegian_visual_rating = average_visual_rating,
         persian_visual_rating = average_visual_rating,
         portuguese_visual_rating = average_visual_rating,
         russian_visual_rating = case_when(!is.na(russian_visual_rating) ~ russian_visual_rating,
                                             TRUE ~ average_visual_rating),
         slovak_visual_rating = average_visual_rating,
         spanish_visual_rating = average_visual_rating,
         swedish_visual_rating = average_visual_rating,
         turkish_visual_rating = average_visual_rating
         
  ) %>%
  # add socialness ratings
  mutate(arabic_socialness_rating = average_socialness_rating,
         asl_socialness_rating = average_socialness_rating,
         bsl_socialness_rating = average_socialness_rating,
         catalan_socialness_rating = average_socialness_rating,
         chinese_socialness_rating = average_socialness_rating,
         croatian_socialness_rating = average_socialness_rating,
         czech_socialness_rating = average_socialness_rating,
         danish_socialness_rating = average_socialness_rating,
         dutch_socialness_rating = average_socialness_rating,
         english_socialness_rating = case_when(!is.na(english_socialness_rating) ~ english_socialness_rating,
                                                  TRUE ~ average_socialness_rating),
         estonian_socialness_rating = average_socialness_rating,
         finnish_socialness_rating = average_socialness_rating,
         french_socialness_rating =  average_socialness_rating,
         german_socialness_rating = average_socialness_rating,
         greek_socialness_rating = average_socialness_rating,
         hebrew_socialness_rating = average_socialness_rating,
         hungarian_socialness_rating = average_socialness_rating,
         irish_socialness_rating = average_socialness_rating,
         italian_socialness_rating = average_socialness_rating,
         japanese_socialness_rating = average_socialness_rating,
         kirigiama_socialness_rating = average_socialness_rating,
         kiswahili_socialness_rating = average_socialness_rating,
         korean_socialness_rating = average_socialness_rating,
         latvian_socialness_rating = average_socialness_rating,
         norwegian_socialness_rating = average_socialness_rating,
         persian_socialness_rating = average_socialness_rating,
         portuguese_socialness_rating = average_socialness_rating,
         russian_socialness_rating = average_socialness_rating,
         slovak_socialness_rating = average_socialness_rating,
         spanish_socialness_rating = average_socialness_rating,
         swedish_socialness_rating = average_socialness_rating,
         turkish_socialness_rating = average_socialness_rating
  )%>%
  # add emotionalarousal ratings
  mutate(arabic_emotionalarousal_rating = average_emotionalarousal_rating,
    asl_emotionalarousal_rating = average_emotionalarousal_rating,
         bsl_emotionalarousal_rating = average_emotionalarousal_rating,
    catalan_emotionalarousal_rating = average_emotionalarousal_rating,
         chinese_emotionalarousal_rating = average_emotionalarousal_rating,
         croatian_emotionalarousal_rating = average_emotionalarousal_rating,
         czech_emotionalarousal_rating = average_emotionalarousal_rating,
         danish_emotionalarousal_rating = average_emotionalarousal_rating,
         dutch_emotionalarousal_rating = case_when(!is.na(dutch_emotionalarousal_rating) ~ dutch_emotionalarousal_rating,
                                                   TRUE ~ average_emotionalarousal_rating),
         english_emotionalarousal_rating = average_emotionalarousal_rating,
    estonian_emotionalarousal_rating = case_when(!is.na(estonian_emotionalarousal_rating) ~ estonian_emotionalarousal_rating,
                                                 TRUE ~ average_emotionalarousal_rating),
         finnish_emotionalarousal_rating = average_emotionalarousal_rating,
         french_emotionalarousal_rating =  case_when(!is.na(french_emotionalarousal_rating) ~ french_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         german_emotionalarousal_rating = average_emotionalarousal_rating,
         greek_emotionalarousal_rating = average_emotionalarousal_rating,
         hebrew_emotionalarousal_rating = average_emotionalarousal_rating,
         hungarian_emotionalarousal_rating = average_emotionalarousal_rating,
         irish_emotionalarousal_rating = average_emotionalarousal_rating,
         italian_emotionalarousal_rating = case_when(!is.na(italian_emotionalarousal_rating) ~ italian_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
    japanese_emotionalarousal_rating = average_emotionalarousal_rating,
         kirigiama_emotionalarousal_rating = average_emotionalarousal_rating,
         kiswahili_emotionalarousal_rating = average_emotionalarousal_rating,
         korean_emotionalarousal_rating = average_emotionalarousal_rating,
         latvian_emotionalarousal_rating = average_emotionalarousal_rating,
         norwegian_emotionalarousal_rating = average_emotionalarousal_rating,
         persian_emotionalarousal_rating = average_emotionalarousal_rating,
         portuguese_emotionalarousal_rating = average_emotionalarousal_rating,
         russian_emotionalarousal_rating = average_emotionalarousal_rating,
         slovak_emotionalarousal_rating = average_emotionalarousal_rating,
         spanish_emotionalarousal_rating = average_emotionalarousal_rating,
         swedish_emotionalarousal_rating = case_when(!is.na(swedish_emotionalarousal_rating) ~ swedish_emotionalarousal_rating,
                                                  TRUE ~ average_emotionalarousal_rating),
         turkish_emotionalarousal_rating = case_when(!is.na(turkish_emotionalarousal_rating) ~ turkish_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating)
  )%>%
  # add CD and frequency
  #need to add arabic_CD
  #need to add catalan_CD
  left_join(croatian_CD) %>%
  left_join(chinese_CD) %>%
  left_join(czech_CD) %>%
  left_join(danish_CD) %>% 
  left_join(dutch_CD) %>%
  left_join(english_CD) %>%
  #need to add estonian_CD
  left_join(finnish_CD) %>%
  left_join(french_CD) %>%
  left_join(german_CD) %>%
  left_join(greek_CD) %>%
  left_join(hebrew_CD) %>%
  left_join(hungarian_CD) %>%
  left_join(irish_CD) %>%
  left_join(italian_CD) %>%
  #need to add japanese_CD
  left_join(kiswahili_CD) %>%
  left_join(korean_CD) %>%
  left_join(latvian_CD) %>%
  left_join(norwegian_CD) %>%
  left_join(persian_CD) %>%
  left_join(portuguese_CD) %>%
  left_join(russian_CD) %>%
  left_join(slovak_CD) %>%
  left_join(spanish_CD) %>%
  left_join(swedish_CD) %>%
  left_join(turkish_CD) %>%
  as.data.frame() %>%
  distinct() %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rowwise() %>%
  mutate(average_CD_rating = mean(c_across(matches("^.*_CD_rating$")), na.rm = TRUE),
         average_freq_rating = mean(c_across(matches("^.*_freq_rating$")), na.rm = TRUE)) %>%
  ungroup() 

write_rds(CDI_mega_word_list_with_ratings, "norms/CDI_mega_word_list_with_ratings.rds")
# CDI_mega_word_list_with_ratings <- read_rds("norms/CDI_mega_word_list_with_ratings.rds")







# write out ratings dfs ----
arabic_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Arabic (Saudi)`,
         matches("^arabic_.*_rating$"))
asl_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `American Sign Language`,
         matches("^asl_.*_rating$"))
bsl_ratings_subset<- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `British Sign Language`,
         matches("^bsl_.*_rating$"))
catalan_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Catalan`,
         matches("^catalan_.*_rating$"))
chinese_ratings_subset <- CDI_mega_word_list_with_ratings %>%
  select(uni_lemma,
         `Chinese (all)`,
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
  select(uni_lemma, `English (all)`,`English (American)`, 
         `English (Australian)`, 
         `English (British)`, 
         `English (Irish)`,
         matches("^english_.*_rating$")) 
estonian_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Estonian`,
         matches("^estonian_.*_rating$"))
finnish_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Finnish`,
         matches("^finnish_.*_rating$")) 
french_ratings_subset <- CDI_mega_word_list_with_ratings %>%
  select(uni_lemma, 
         `French (all)`,
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
japanese_ratings_subset <- CDI_mega_word_list_with_ratings %>% 
  select(uni_lemma, `Japanese`,
         matches("^japanese_.*_rating$"))
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
         `Spanish (all)`,
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

