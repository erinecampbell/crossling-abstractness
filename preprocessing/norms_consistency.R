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
  mutate(word = tolower(word)) %>%
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
  select(word, asl_iconicity_rating, asl_frequency_rating, asl_phoncomp_rating) %>%
  filter(word %in% CDI_mega_dictionary$`American Sign Language`)

# bsl ----
bsl_ratings <- read_csv("norms/bsl/data_ratings.csv") %>%
  filter(signer == "signer",
         country == "en") %>%
  pivot_longer(cols = AEROPLANE:WRITE, names_to = "word", values_to = "rating") %>%
  pivot_wider(names_from = "condition", values_from = "rating") %>%
  select(id, word, iconicity, concreteness) %>%
  group_by(word) %>%
  summarise(iconicity_rating = mean(iconicity, na.rm=TRUE),
            concreteness_rating = mean(concreteness, na.rm=TRUE)) %>%
  mutate(word = tolower(word),
         bsl_iconicity_rating = rescale_ratings(iconicity_rating, old_min = -300, old_max = 300, new_min = 1, new_max = 10),
         bsl_concreteness_rating = rescale_ratings(concreteness_rating, old_min = -300, old_max = 300, new_min = 1, new_max = 10)) %>%
  select(word, bsl_iconicity_rating, bsl_concreteness_rating)

# croatian ----

croatian_ratings <- read_excel("norms/croatian/megahr.xlsx") %>%
  mutate(word = leksem,
         croatian_concreteness_rating = rescale_ratings(k.M,1,5,1,10),
        croatian_imageability_rating = rescale_ratings(p.M, 1,5,1,10)
         ) %>%
  select(word, croatian_concreteness_rating, croatian_imageability_rating) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_croatian)


# dutch ----

dutch_emotionalarousal <- read_csv("norms/dutch/verheyen_emotionalarousal2019.csv") %>%
  dplyr::mutate(word = Words,
                dutch_emotionalarousal_rating = rescale_ratings(mean,1,7,1,10)) %>%
  select(word, dutch_emotionalarousal_rating) 

dutch_perceptual <- read_xlsx("norms/dutch/SpeedBrysbaert_Norms.xlsx") %>%
  dplyr::rename(word = Woord, 
                dutch_auditory_rating = Horen,
                dutch_visual_rating = Zien,
                dutch_olfactory_rating = Ruiken,
                dutch_gustatory_rating = Proeven,
                dutch_haptic_rating = Voelen,
                dutch_interoceptive_rating = Sensaties) %>%
  mutate_at(vars(c(dutch_auditory_rating:dutch_interoceptive_rating)), ~ rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10)) %>%
  pivot_longer(cols = c(dutch_auditory_rating:dutch_interoceptive_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

dutch_concreteness <- read_csv("norms/dutch/verheyen_dutch_concreteness.csv") %>%
  mutate(word = Words,
         rating = rescale_ratings(mean,1,7,1,10),
         rating_type="dutch_concreteness_rating") %>%
  select(word, rating, rating_type)

dutch_imageability <- read_csv("norms/dutch/verheyen_dutch_imageability.csv") %>%
  mutate(word = Words,
         dutch_imageability_rating = rescale_ratings(mean,1,7,1,10)) %>%
  select(word, dutch_imageability_rating)


mega_dutch <- bind_rows(dutch_concreteness, dutch_perceptual) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) %>%
  left_join(dutch_emotionalarousal) %>%
  left_join(dutch_imageability) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_dutch)

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

english_emotionalarousal <- read_delim("norms/english/arousal-NRC-VAD-Lexicon.csv", delim=",", col_names = c("word", "rating")) %>%
  mutate(rating = rescale_ratings(rating, old_min = 0, old_max = 1, new_min = 1, new_max = 10),
         rating_type = "english_emotionalarousal_rating")

english_concreteness <- read_excel("norms/english/13428_2013_403_MOESM1_ESM.xlsx") %>%
  mutate(word = Word,
         rating = rescale_ratings(Conc.M,1,5,1,10),
         rating_type="english_concreteness_rating") %>%
  select(word, rating, rating_type)
english_bois <- read_csv("norms/english/CBOI_mean_sd.csv") %>%
  mutate(word = tolower(Word),
         rating = rescale_ratings(Mean, 1, 7 , 1, 10),
         rating_type = "english_boi_rating") %>%
  select(word, rating, rating_type)

mega_english <- bind_rows(english_bois, english_concreteness, english_iconicity,english_perceptual, english_emotionalarousal) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) %>%
  filter(word %in% CDI_mega_dictionary$`English (all)`)
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
         estonian_auditory_rating, estonian_gustatory_rating, estonian_olfactory_rating, estonian_haptic_rating)%>%
  filter(word %in% CDI_mega_dictionary$dictionary_estonian)
# french ----

french_perceptual <- read_excel("norms/french/miceli2019_messagedfromresearchgate.xlsx")%>%
  dplyr::rename(word = MOT, 
                english_gloss = WORD,
                french_auditory_rating = Auditory_Mean,
                french_visual_rating = Visual_Mean,
                french_olfactory_rating = Olfactory_Mean,
                french_gustatory_rating = Gustatory_Mean,
                french_haptic_rating = Haptic_Mean,
                french_dominant_perceptual_rating = `Dominant Modality`,
                french_exclusivity_rating = `Modality Exclusivity (%)`) %>%
  mutate(english_gloss = tolower(english_gloss),
         word = tolower(word)) %>%
  mutate_at(vars(c(french_auditory_rating, french_visual_rating, french_olfactory_rating, 
                   french_gustatory_rating, french_haptic_rating,french_exclusivity_rating)), ~ 
              rescale_ratings(., old_min = 0, old_max = 5, new_min = 1, new_max = 10))  %>%
  pivot_longer(cols = c(french_auditory_rating, french_visual_rating, french_olfactory_rating, 
                        french_gustatory_rating, french_haptic_rating,french_exclusivity_rating), names_to = "rating_type", 
               values_to = "rating") %>%
  select(word, rating, rating_type)

french_concreteness <- read_excel("norms/french/concreteness_bonin2018.xlsx") %>%
  dplyr::rename(Concreteness_mean = `concreteness mean`) %>%
  mutate(word = items,
         rating = rescale_ratings(Concreteness_mean,1,5,1,10),
         rating_type = "french_concreteness_rating") %>%
  select(word, rating, rating_type)

french_imageability <- read_excel("norms/french/Desrochers-Thompson_2009_Ratings.xls") %>%
  mutate(word = NOUN,
         rating = rescale_ratings(IMAGE_Mean, 1,7,1,10),
         rating_type = "french_imageability_rating") %>%
  select(word, rating, rating_type)

french_bois <- read_excel("norms/french/BOI mean ratings.xlsx", 
                          sheet = "All_Stimuli")%>%
  mutate(word = tolower(FR),
         rating = rescale_ratings(`BOI Mean`, 1,7,1,10),
         rating_type = "french_boi_rating") %>%
  select(word, rating, rating_type)

french_emotionalarousal <- read.csv("norms/french/FANCatdatabase_emotionalarousal.csv") %>%
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
              values_from = rating) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_french)

# greek ----
greek_affective <- read_excel("norms/greek/greek_affective_lexicon.xlsx", skip = 1) %>%
  dplyr::rename(word = `Greek word`,
                greek_emotionalarousal_rating = Arousal,
                greek_emotionalvalence_rating = Valence,
                greek_emotionaldominance_rating = Dominance) %>%
    mutate(greek_emotionalarousal_rating = rescale_ratings(greek_emotionalarousal_rating, -1,1,1,10),
           greek_emotionalvalence_rating = rescale_ratings(greek_emotionalvalence_rating, -1,1,1,10),
           greek_emotionaldominance_rating = rescale_ratings(greek_emotionaldominance_rating, -1,1,1,10)) %>%
    select(word, greek_emotionalarousal_rating, greek_emotionaldominance_rating, greek_emotionalvalence_rating)

greek_imagery <- read_excel("norms/greek/G-CWP_DATABASE.xlsx", 
                            sheet = "Imagery norms", skip = 3) %>%
  select(ItemL, `M...4`) %>%
  dplyr::rename(word = `ItemL`,
                greek_imagery_rating = `M...4`) %>%
  mutate(greek_imagery_rating = rescale_ratings(greek_imagery_rating, 1,7,1,10))

greek_concreteness <- read_excel("norms/greek/G-CWP_DATABASE.xlsx", 
                            sheet = "Concreteness norms", skip = 3) %>%
  select(ItemL, `M...4`) %>%
  dplyr::rename(word = `ItemL`,
                greek_concreteness_rating = `M...4`) %>%
  mutate(greek_concreteness_rating = rescale_ratings(greek_concreteness_rating, 1,7,1,10))

mega_greek <- full_join(greek_imagery, greek_concreteness) %>%
  full_join(greek_affective) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_greek)

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


mega_italian <- italian_perceptual %>%
  filter(!is.na(word)) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_italian)

# japanese ----

japanese_CDI_romaji  <- read_csv("norms/japanese/japanese_CDI_romaji.csv")

japanese_iconicity_ratings <- read_csv("norms/japanese/Japanese iconicity ratings.csv") %>%
  group_by(word,wordCode) %>%
  summarise(japanese_iconicity_rating = mean(rating, na.rm=TRUE)) %>%
  select(-wordCode) %>%
  mutate(japanese_iconicity_rating = rescale_ratings(japanese_iconicity_rating, old_min = -5, old_max = 5, new_min = 1, new_max = 10),
         word = str_remove_all(word, "\\([^)]*\\)"),
         word = str_remove(word, "_.*"),
         word = str_remove(word, ",.*")) %>%
  left_join(japanese_CDI_romaji, by = c("word" = "romaji")) %>%
  rename(romaji = word,
         word = item_definition) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_japanese)




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

chinese_emotionalarousal <- read_csv("norms/chinese/13428_2021_1607_MOESM1_ESM.csv") %>%
  rename(word = Word) %>%
  mutate(chinese_emotionalarousal_rating = rescale_ratings(Arousal_Mean, old_min = 0, old_max = 4, new_min = 1, new_max = 10)) %>%
  select(word, chinese_emotionalarousal_rating)

chinese_imageability <- read_csv("norms/chinese/12144_2022_3404_MOESM2_ESM.csv")%>%
  rename(word = Word) %>%
  mutate(chinese_imageability_rating = rescale_ratings(IMA_M, old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  select(word, chinese_imageability_rating)

mega_chinese <- full_join(chinese_perceptual, 
                          chinese_emotionalarousal) %>%
  full_join(chinese_imageability) %>%
  filter(word %in% CDI_mega_dictionary$`Chinese (all)`)


# norwegian ----
norwegian_imageability <- read_delim("norms/norwegian/ordforradet_utf8.csv", delim = "\t", col_names = TRUE, locale = locale(encoding = "UTF-8"))  %>%
  mutate(word = str_remove(Word, "^(å|en|et)\\s"),
         norwegian_imageability_rating = rescale_ratings(Imageability,1,8,1,10)) %>%
  select(word, norwegian_imageability_rating)  %>%
  filter(word %in% CDI_mega_dictionary$dictionary_norwegian)

# portuguese ----
portuguese_concreteness <- read_excel("norms/portuguese/13428_2016_767_MOESM1_ESM.xlsx", 
                                                      sheet = "MWP norms") %>%
  rename(word = `Word (Portuguese)`,
         portuguese_concreteness_rating = Conc_M,
         portuguese_imageability_rating = Imag_M) %>%
  mutate_at(vars(c(portuguese_concreteness_rating,portuguese_imageability_rating)), ~ 
              rescale_ratings(., old_min = 1, old_max = 7, new_min = 1, new_max = 10)) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_portuguese)

#  russian ----
russian_perceptual <- read_excel("norms/russian/10936_2017_9548_MOESM1_ESM.xlsx") %>%
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
              values_from = rating) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_russian)


# spanish ----
spanish_perceptual <- read_excel("norms/spanish/ConceptAttributesSpanish.xlsx") %>%
  dplyr::mutate(word = tolower(Spanish),
                spanish_auditory_rating = rescale_ratings(sound_m, 1,8,1,10),
                spanish_olfactory_rating = rescale_ratings(smell_m, 1, 8, 1, 10),
                spanish_gustatory_rating = rescale_ratings (taste_m, 1,8,1,10),
                spanish_boi_rating = rescale_ratings(BOI, 1,7, 1, 10),
                spanish_concreteness_rating = rescale_ratings(Concreteness, 1, 7, 1, 10),
                spanish_imageability_rating = rescale_ratings(Imageability, 1,7,1,10)) %>%
  select(word, spanish_auditory_rating, spanish_olfactory_rating, spanish_gustatory_rating, 
         spanish_boi_rating, spanish_concreteness_rating, spanish_imageability_rating) %>%
  pivot_longer(cols = c(spanish_auditory_rating:spanish_imageability_rating), 
               names_to = "rating_type",
               values_to = "rating")

spanish_iconicity <- read_xlsx("norms/spanish/word_ratings.xlsx") %>%
  dplyr::rename(rating = `ico-m`) %>%
  mutate(rating = rescale_ratings(rating, old_min=1,old_max = 7,new_min = 1, new_max = 10),
         rating_type = "spanish_iconicity_rating")  %>%
  select(word, rating, rating_type)


spanish_affective <- read_csv("norms/spanish/Lexical-variables.csv") %>%
  dplyr::mutate(word = tolower(Word),
                spanish_emotionalarousal_rating = rescale_ratings(Arousal, 1,9,1,10),
                spanish_emotionalvalence_rating = rescale_ratings(Valence, 1, 9, 1, 10)) %>%
  select(word, spanish_emotionalarousal_rating, spanish_emotionalvalence_rating) %>%
  pivot_longer(cols = c(spanish_emotionalarousal_rating, spanish_emotionalvalence_rating), 
               names_to = "rating_type",
               values_to = "rating")

spanish_boi <- read_excel("norms/spanish/Appendix_2_BOI_Spanish.xlsx") %>%
  dplyr::mutate(word = tolower(Word),
                rating = rescale_ratings(Mean, 1,7,1,10),
                rating_type="spanish_boi_rating") %>%
  dplyr::select(word, rating, rating_type)


mega_spanish <- bind_rows(spanish_boi, 
                          spanish_iconicity, 
                          spanish_affective, 
                          spanish_perceptual) %>%  
  distinct(word, rating_type, .keep_all = TRUE) %>%
  pivot_wider(names_from = rating_type,
              values_from = rating) %>%
  filter(word %in% CDI_mega_dictionary$`Spanish (all)`)

# swedish ----
mega_swedish <- read_excel("norms/swedish/blomberg_2015.xlsx") %>%
  bind_rows((read_excel("norms/swedish/blomberg_dissertation_appendixC_selectedwords.xlsx"))) %>%
  mutate_at(vars(c(swedish_imageability_rating,swedish_emotionalarousal_rating)), ~ 
              rescale_ratings(., old_min = 100, old_max = 700, new_min = 1, new_max = 10)) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_swedish)

# turkish ----
turkish_emotionalarousal <- read_csv("norms/turkish/torkamani-azar_emotionalarousal2019.csv", 
                            skip = 5) %>%
  dplyr::rename(word = `Turkish Word`,
                turkish_emotionalarousal_rating = AroMn) %>% 
  mutate(turkish_emotionalarousal_rating1 = rescale_ratings(turkish_emotionalarousal_rating,1,9,1,10)) %>%
  select(word, turkish_emotionalarousal_rating1)

mega_turkish <- read_excel("norms/turkish/TACO_dataset.xlsx") %>%
  mutate(word = tolower(`TU words`),
         turkish_imageability_rating = rescale_ratings(IMG_MEAN, old_min = 1, old_max = 7, new_min = 1, new_max = 10),
         turkish_emotionalarousal_rating2 =  rescale_ratings(Arousal_MEAN, old_min = 1, old_max = 9, new_min = 1, new_max = 10)) %>%
  select(word, turkish_imageability_rating, turkish_emotionalarousal_rating2) %>%
  full_join(turkish_emotionalarousal) %>%
  rowwise() %>%
  mutate(turkish_emotionalarousal_rating = mean(c(turkish_emotionalarousal_rating1, turkish_emotionalarousal_rating2), na.rm = TRUE)) %>%
  select(word, turkish_imageability_rating, turkish_emotionalarousal_rating) %>%
  filter(word %in% CDI_mega_dictionary$dictionary_turkish)
  
# wikipedia data ----

arabic_CD <- read_delim("norms/arabic/ar_wordfreq.tsv", delim = " ",col_names = c("Arabic (Saudi)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(arabic_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         arabic_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Arabic (Saudi)`, arabic_CD_rating, arabic_freq_rating) %>%
  filter(`Arabic (Saudi)` %in% CDI_mega_dictionary$`Arabic (Saudi)`)

catalan_CD <- read_delim("norms/catalan/ca_wordfreq.tsv", delim = " ",col_names = c("Catalan", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(catalan_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         catalan_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Catalan, catalan_CD_rating, catalan_freq_rating)  %>%
  filter(Catalan %in% CDI_mega_dictionary$Catalan)

croatian_CD <- read_delim("norms/croatian/hr_wordfreq.tsv", delim = " ",col_names = c("Croatian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(croatian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         croatian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Croatian, croatian_CD_rating, croatian_freq_rating)  %>%
  filter(Croatian %in% CDI_mega_dictionary$Croatian)

czech_CD <- read_delim("norms/czech/cs_wordfreq.tsv", delim = " ",col_names = c("Czech", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(czech_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         czech_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Czech, czech_CD_rating, czech_freq_rating)  %>%
  filter(Czech %in% CDI_mega_dictionary$Czech)

danish_CD <- read_delim("norms/danish/da_wordfreq.tsv", delim = " ",col_names = c("Danish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(danish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         danish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Danish, danish_CD_rating, danish_freq_rating)  %>%
  filter(Danish %in% CDI_mega_dictionary$Danish)

dutch_CD <- read_delim("norms/dutch/nl_wordfreq.tsv", delim = " ",col_names = c("Dutch", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(dutch_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         dutch_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Dutch, dutch_CD_rating, dutch_freq_rating)  %>%
  filter(Dutch %in% CDI_mega_dictionary$Dutch)

english_CD <- read_delim("norms/english/en_wordfreq.tsv", delim = " ",col_names = c("English (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(english_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         english_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`English (all)`, english_CD_rating,english_freq_rating)  %>%
  filter(`English (all)` %in% CDI_mega_dictionary$`English (all)`)

estonian_CD <- read_delim("norms/estonian/et_wordfreq.tsv", delim = " ",col_names = c("Estonian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(estonian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         estonian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Estonian, estonian_CD_rating, estonian_freq_rating) %>%
  filter(Estonian %in% CDI_mega_dictionary$Estonian)

finnish_CD <- read_delim("norms/finnish/fi_wordfreq.tsv", delim = " ",col_names = c("Finnish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(finnish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         finnish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Finnish, finnish_CD_rating, finnish_freq_rating) %>%
  filter(Finnish %in% CDI_mega_dictionary$Finnish)

french_CD <- read_delim("norms/french/fr_wordfreq.tsv", delim = " ",col_names = c("French (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(french_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         french_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`French (all)`, french_CD_rating, french_freq_rating) %>%
  filter(`French (all)` %in% CDI_mega_dictionary$`French (all)`)

german_CD <- read_delim("norms/german/de_wordfreq.tsv", delim = " ",col_names = c("German", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(german_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         german_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(German, german_CD_rating, german_freq_rating) %>%
  filter(German %in% CDI_mega_dictionary$German)

greek_CD <- read_delim("norms/greek/el_wordfreq.tsv", delim = " ",col_names = c("Greek (Cypriot)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(greek_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         greek_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Greek (Cypriot)`, greek_CD_rating, greek_freq_rating) %>%
  filter(`Greek (Cypriot)` %in% CDI_mega_dictionary$`Greek (Cypriot)`)

hebrew_CD <- read_delim("norms/hebrew/he_wordfreq.tsv", delim = " ",col_names = c("Hebrew", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(hebrew_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         hebrew_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Hebrew, hebrew_CD_rating, hebrew_freq_rating) %>%
  filter(Hebrew %in% CDI_mega_dictionary$Hebrew)

hungarian_CD <- read_delim("norms/hungarian/hu_wordfreq.tsv", delim = " ",col_names = c("Hungarian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(hungarian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         hungarian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Hungarian, hungarian_CD_rating, hungarian_freq_rating) %>%
  filter(Hungarian %in% CDI_mega_dictionary$Hungarian)

irish_CD <- read_delim("norms/irish/ga_wordfreq.tsv", delim = " ",col_names = c("Irish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(irish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         irish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Irish, irish_CD_rating, irish_freq_rating) %>%
  filter(Irish %in% CDI_mega_dictionary$Irish)

italian_CD <- read_delim("norms/italian/it_wordfreq.tsv", delim = " ",col_names = c("Italian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(italian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         italian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Italian, italian_CD_rating, italian_freq_rating) %>%
  filter(Italian %in% CDI_mega_dictionary$Italian)

chinese_CD <- read_delim("norms/chinese/zh_wordfreq.tsv", delim = " ",col_names = c("Chinese (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(chinese_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         chinese_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Chinese (all)`, chinese_CD_rating, chinese_freq_rating) %>%
  filter(`Chinese (all)` %in% CDI_mega_dictionary$`Chinese (all)`)

japanese_CD <- read_delim("norms/arabic/ar_wordfreq.tsv", delim = " ",col_names = c("Japanese", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(japanese_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         japanese_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Japanese, japanese_CD_rating, japanese_freq_rating) %>%
  filter(Japanese %in% CDI_mega_dictionary$Japanese)

kiswahili_CD <- read_delim("norms/kiswahili/sw_wordfreq.tsv", delim = " ",col_names = c("Kiswahili", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(kiswahili_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         kiswahili_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Kiswahili, kiswahili_CD_rating, kiswahili_freq_rating) %>%
  filter(Kiswahili %in% CDI_mega_dictionary$Kiswahili)

korean_CD <- read_delim("norms/korean/ko_wordfreq.tsv", delim = " ",col_names = c("Korean", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(korean_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         korean_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Korean, korean_CD_rating, korean_freq_rating) %>%
  filter(Korean %in% CDI_mega_dictionary$Korean)

latvian_CD <- read_delim("norms/latvian/lv_wordfreq.tsv", delim = " ",col_names = c("Latvian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(latvian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         latvian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Latvian, latvian_CD_rating, latvian_freq_rating) %>%
  filter(Latvian %in% CDI_mega_dictionary$Latvian)

norwegian_CD <- read_delim("norms/norwegian/no_wordfreq.tsv", delim = " ",col_names = c("Norwegian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(norwegian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         norwegian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Norwegian, norwegian_CD_rating, norwegian_freq_rating) %>%
  filter(Norwegian %in% CDI_mega_dictionary$Norwegian)

persian_CD <- read_delim("norms/persian/fa_wordfreq.tsv", delim = " ",col_names = c("Persian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(persian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         persian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Persian, persian_CD_rating, persian_freq_rating) %>%
  filter(Persian %in% CDI_mega_dictionary$Persian)

portuguese_CD <- read_delim("norms/portuguese/pt_wordfreq.tsv", delim = " ",col_names = c("Portuguese (European)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(portuguese_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         portuguese_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Portuguese (European)`, portuguese_CD_rating, portuguese_freq_rating) %>%
  filter(`Portuguese (European)` %in% CDI_mega_dictionary$`Portuguese (European)`)

russian_CD <- read_delim("norms/russian/ru_wordfreq.tsv", delim = " ",col_names = c("Russian", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(russian_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         russian_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Russian, russian_CD_rating, russian_freq_rating) %>%
  filter(Russian %in% CDI_mega_dictionary$Russian)

slovak_CD <- read_delim("norms/slovak/sk_wordfreq.tsv", delim = " ",col_names = c("Slovak", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(slovak_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         slovak_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Slovak, slovak_CD_rating, slovak_freq_rating) %>%
  filter(Slovak %in% CDI_mega_dictionary$Slovak)

spanish_CD <- read_delim("norms/spanish/es_wordfreq.tsv", delim = " ",col_names = c("Spanish (all)", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(spanish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         spanish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(`Spanish (all)`, spanish_CD_rating, spanish_freq_rating) %>%
  filter(`Spanish (all)` %in% CDI_mega_dictionary$`Spanish (all)`)

swedish_CD <- read_delim("norms/swedish/sv_wordfreq.tsv", delim = " ",col_names = c("Swedish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(swedish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         swedish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Swedish, swedish_CD_rating, swedish_freq_rating) %>%
  filter(Swedish %in% CDI_mega_dictionary$Swedish)

turkish_CD <- read_delim("norms/turkish/tr_wordfreq.tsv", delim = " ",col_names = c("Turkish", "word_count", "document_count")) %>%
  mutate(perc_CD = document_count/(max(document_count,na.rm=TRUE))) %>%
  mutate(turkish_CD_rating = rescale_ratings(log(perc_CD), min(log(perc_CD),na.rm=TRUE),max(log(perc_CD),na.rm=TRUE),1,10),
         turkish_freq_rating = rescale_ratings(log(word_count), min(log(word_count),na.rm=TRUE),max(log(word_count),na.rm=TRUE),1,10)) %>%
  select(Turkish, turkish_CD_rating, turkish_freq_rating) %>%
  filter(Turkish %in% CDI_mega_dictionary$Turkish)



# mega mega ----

CDI_mega_word_list_with_averageratings <- CDI_mega_dictionary %>% 
  left_join(mega_english, by=c("English (all)" = "word")) %>%
  left_join(mega_dutch, by=c("dictionary_dutch" = "word")) %>%
  left_join(mega_french, by = c("dictionary_french" = "word")) %>%
  left_join(mega_spanish, by = c("Spanish (all)" = "word")) %>%
  left_join(russian_perceptual, by = c("dictionary_russian" = "word")) %>%
  left_join(mega_italian, by = c("dictionary_italian" = "word")) %>%
  left_join(portuguese_concreteness, by = c(`Portuguese (European)` = "word")) %>%
  left_join(asl_ratings, by = c(`American Sign Language` = "word")) %>%
  left_join(croatian_ratings, by = c("Croatian" = "word")) %>%
  left_join(norwegian_imageability, by = c("dictionary_norwegian" = "word")) %>%
  left_join(mega_chinese, by = c("Chinese (all)" = "word")) %>%
  left_join(mega_swedish, by = c("Swedish" = "word")) %>%
  left_join(mega_estonian, by=c("Estonian" = "word")) %>%
  left_join(mega_turkish, by = c("Turkish" = "word")) %>%
  left_join(japanese_iconicity_ratings, by = c("Japanese" = "word")) %>%
  left_join(bsl_ratings, by = c("British Sign Language" = "word")) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  distinct() %>%
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
         average_emotionalarousal_rating = mean(c_across(matches("^.*_emotionalarousal_rating$")), na.rm = TRUE)) %>%
  ungroup()
write_rds(CDI_mega_word_list_with_averageratings, "norms/CDI_mega_word_list_with_averageratings.rds")








# write out ratings dfs ----
arabic_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Arabic (Saudi)`,
         matches("^arabic_.*_rating$"), 
         matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(arabic_concreteness_rating = average_concreteness_rating,
         arabic_emotionalarousal_rating = average_emotionalarousal_rating,
         arabic_imageability_rating = average_imageability_rating,
         arabic_visual_rating = average_visual_rating,
         arabic_olfactory_rating = average_olfactory_rating, 
         arabic_interoceptive_rating = average_interoceptive_rating, 
         arabic_haptic_rating = average_haptic_rating,
         arabic_gustatory_rating = average_gustatory_rating, 
         arabic_boi_rating = average_boi_rating,
         arabic_auditory_rating = average_auditory_rating,
  ) %>%
  filter(`Arabic (Saudi)` != "") %>%
  left_join(arabic_CD) 
write_csv(arabic_ratings_subset, "norms/arabic/arabic_ratings_subset.csv")


asl_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `American Sign Language`,
         matches("^asl_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(asl_auditory_rating = average_auditory_rating,
         asl_boi_rating = average_boi_rating,
         asl_imageability_rating = average_imageability_rating,
         asl_concreteness_rating = average_concreteness_rating,
         asl_emotionalarousal_rating = average_emotionalarousal_rating,
         asl_gustatory_rating = average_gustatory_rating,
         asl_haptic_rating = average_haptic_rating,
         asl_interoceptive_rating = average_interoceptive_rating,
         asl_olfactory_rating = average_olfactory_rating,
         asl_visual_rating = average_visual_rating) %>%
  filter(`American Sign Language` != "") 
write_csv(asl_ratings_subset, "norms/asl/asl_ratings_subset.csv")
asl_ratings_correlations <- cor(asl_ratings_subset %>% 
                                  select(matches("^asl_.*_rating$")),use = "na.or.complete")
write_rds(asl_ratings_correlations, "norms/asl/asl_ratings_correlations.rds")
corrplot(asl_ratings, "shade",type="upper",tl.col="black", addCoef.col = "white")


bsl_ratings_subset<- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `British Sign Language`,
         matches("^bsl_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(bsl_auditory_rating = average_auditory_rating,
         bsl_boi_rating = average_boi_rating,
         bsl_imageability_rating = average_imageability_rating,
         bsl_concreteness_rating = case_when(!is.na(bsl_concreteness_rating) ~ bsl_concreteness_rating,
                                              TRUE ~ average_concreteness_rating),
         bsl_emotionalarousal_rating = average_emotionalarousal_rating,
         bsl_gustatory_rating = average_gustatory_rating,
         bsl_haptic_rating = average_haptic_rating,
         bsl_interoceptive_rating = average_interoceptive_rating,
         bsl_olfactory_rating = average_olfactory_rating,
         bsl_visual_rating = average_visual_rating) %>%
  filter(`British Sign Language` != "")
write_csv(bsl_ratings_subset, "norms/bsl/bsl_ratings_subset.csv")
write_rds(bsl_ratings_subset, "norms/bsl/bsl_ratings_subset.rds")



catalan_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Catalan`,
         matches("^catalan_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(catalan_auditory_rating = average_auditory_rating,
         catalan_boi_rating = average_boi_rating,
         catalan_imageability_rating = average_imageability_rating,
         catalan_concreteness_rating = average_concreteness_rating,
         catalan_emotionalarousal_rating = average_emotionalarousal_rating,
         catalan_gustatory_rating = average_gustatory_rating,
         catalan_haptic_rating = average_haptic_rating,
         catalan_interoceptive_rating = average_interoceptive_rating,
         catalan_olfactory_rating = average_olfactory_rating,
         catalan_visual_rating = average_visual_rating) %>%
  left_join(catalan_CD) %>%
  filter(Catalan != "")
write_csv(catalan_ratings_subset, "norms/catalan/catalan_ratings_subset.csv")
write_rds(catalan_ratings_subset, "norms/catalan/catalan_ratings_subset.rds")




chinese_ratings_subset <- CDI_mega_word_list_with_averageratings %>%
  select(uni_lemma,
         `Chinese (all)`,
         Cantonese,
         `Mandarin (Beijing)`,
         `Mandarin (Taiwanese)`,
         matches("^chinese_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(chinese_visual_rating = case_when(!is.na(chinese_visual_rating) ~ chinese_visual_rating,
                                           TRUE ~ average_visual_rating),
         chinese_emotionalarousal_rating = case_when(!is.na(chinese_emotionalarousal_rating) ~ chinese_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         chinese_olfactory_rating = case_when(!is.na(chinese_olfactory_rating) ~ chinese_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         chinese_interoceptive_rating = case_when(!is.na(chinese_interoceptive_rating) ~ chinese_interoceptive_rating,
                                                  TRUE ~ average_interoceptive_rating),
         chinese_haptic_rating = case_when(!is.na(chinese_haptic_rating) ~ chinese_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         chinese_gustatory_rating = case_when(!is.na(chinese_gustatory_rating) ~ chinese_gustatory_rating,
                                              TRUE ~ average_gustatory_rating),
         chinese_auditory_rating = case_when(!is.na(chinese_auditory_rating) ~ chinese_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         chinese_boi_rating = average_boi_rating,
         chinese_concreteness_rating = average_concreteness_rating,
         chinese_imageability_rating = case_when(!is.na(chinese_imageability_rating) ~ chinese_imageability_rating,
                                                 TRUE ~ average_imageability_rating)) %>%
  left_join(chinese_CD) %>%
  filter(`Chinese (all)` != "")
write_csv(chinese_ratings_subset, "norms/chinese/chinese_ratings_subset.csv")
write_rds(chinese_ratings_subset, "norms/chinese/chinese_ratings_subset.rds")

croatian_ratings_subset<- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Croatian`,
         matches("^croatian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(croatian_auditory_rating = average_auditory_rating,
         croatian_boi_rating = average_boi_rating,
         croatian_imageability_rating = case_when(!is.na(croatian_imageability_rating) ~ croatian_imageability_rating,
                                                  TRUE ~ average_imageability_rating),
         croatian_concreteness_rating = case_when(!is.na(croatian_concreteness_rating) ~ croatian_concreteness_rating,
                                                  TRUE ~ average_concreteness_rating),
         croatian_emotionalarousal_rating = average_emotionalarousal_rating,
         croatian_gustatory_rating = average_gustatory_rating,
         croatian_haptic_rating = average_haptic_rating,
         croatian_interoceptive_rating = average_interoceptive_rating,
         croatian_olfactory_rating = average_olfactory_rating,
         croatian_visual_rating = average_visual_rating) %>%
  left_join(croatian_CD) %>%
  filter(Croatian != "")
write_csv(croatian_ratings_subset, "norms/croatian/croatian_ratings_subset.csv")
write_rds(croatian_ratings_subset, "norms/croatian/croatian_ratings_subset.rds")

czech_ratings_subset<- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Czech`,
         matches("^czech_.*_rating$"), matches("^average_.*_rating$")) %>%
  mutate(czech_auditory_rating = average_auditory_rating,
         czech_boi_rating = average_boi_rating,
         czech_imageability_rating = average_imageability_rating,
         czech_concreteness_rating = average_concreteness_rating,
         czech_emotionalarousal_rating = average_emotionalarousal_rating,
         czech_gustatory_rating = average_gustatory_rating,
         czech_haptic_rating = average_haptic_rating,
         czech_interoceptive_rating = average_interoceptive_rating,
         czech_olfactory_rating = average_olfactory_rating,
         czech_visual_rating = average_visual_rating) %>%
  left_join(czech_CD) %>%
  filter(Czech != "")
write_csv(czech_ratings_subset, "norms/czech/czech_ratings_subset.csv")
write_rds(czech_ratings_subset, "norms/czech/czech_ratings_subset.rds")


danish_ratings_subset<- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Danish`,
         matches("^danish_.*_rating$"), matches("^average_.*_rating$")) %>%
  mutate(danish_auditory_rating = average_auditory_rating,
         danish_boi_rating = average_boi_rating,
         danish_imageability_rating = average_imageability_rating,
         danish_concreteness_rating = average_concreteness_rating,
         danish_emotionalarousal_rating = average_emotionalarousal_rating,
         danish_gustatory_rating = average_gustatory_rating,
         danish_haptic_rating = average_haptic_rating,
         danish_interoceptive_rating = average_interoceptive_rating,
         danish_olfactory_rating = average_olfactory_rating,
         danish_visual_rating = average_visual_rating) %>%
  left_join(danish_CD) %>%
  filter(Danish != "")
write_csv(danish_ratings_subset, "norms/danish/danish_ratings_subset.csv")
write_rds(danish_ratings_subset, "norms/danish/danish_ratings_subset.rds")

dutch_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Dutch`,
         matches("^dutch_.*_rating$"), matches("^average_.*_rating$")) %>%
  mutate(dutch_auditory_rating = case_when(!is.na(dutch_auditory_rating) ~ dutch_auditory_rating,
                                           TRUE ~ average_auditory_rating),
         dutch_boi_rating = average_boi_rating,
         dutch_imageability_rating = case_when(!is.na(dutch_imageability_rating) ~ dutch_imageability_rating,
                                               TRUE ~ average_imageability_rating),
         dutch_concreteness_rating = case_when(!is.na(dutch_concreteness_rating) ~ dutch_concreteness_rating,
                                               TRUE ~ average_concreteness_rating),
         dutch_emotionalarousal_rating = case_when(!is.na(dutch_emotionalarousal_rating) ~ dutch_emotionalarousal_rating,
                                                   TRUE ~ average_emotionalarousal_rating),
         dutch_gustatory_rating = case_when(!is.na(dutch_gustatory_rating) ~ dutch_gustatory_rating,
                                            TRUE ~ average_gustatory_rating),
         dutch_haptic_rating = case_when(!is.na(dutch_haptic_rating) ~ dutch_haptic_rating,
                                         TRUE ~ average_haptic_rating),
         dutch_interoceptive_rating = case_when(!is.na(dutch_interoceptive_rating) ~ dutch_interoceptive_rating,
                                                TRUE ~ average_interoceptive_rating),
         dutch_olfactory_rating = case_when(!is.na(dutch_olfactory_rating) ~ dutch_olfactory_rating,
                                            TRUE ~ average_olfactory_rating),
         dutch_visual_rating = case_when(!is.na(dutch_visual_rating) ~ dutch_visual_rating,
                                         TRUE ~ average_visual_rating)) %>%
  left_join(dutch_CD) %>%
  distinct() %>%
  filter(Dutch != "")
write_csv(dutch_ratings_subset, "norms/dutch/dutch_ratings_subset.csv")
write_rds(dutch_ratings_subset, "norms/dutch/dutch_ratings_subset.rds")

english_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `English (all)`,`English (American)`, 
         `English (Australian)`, 
         `English (British)`, 
         `English (Irish)`,
         matches("^english_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(english_auditory_rating = case_when(!is.na(english_auditory_rating) ~ english_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         english_boi_rating = case_when(!is.na(english_boi_rating) ~ english_boi_rating,
                                        TRUE ~ average_boi_rating),
         english_imageability_rating = average_imageability_rating,
         english_concreteness_rating = case_when(!is.na(english_concreteness_rating) ~ english_concreteness_rating,
                                                 TRUE ~ average_concreteness_rating),
         english_emotionalarousal_rating = case_when(!is.na(english_emotionalarousal_rating) ~ english_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         english_gustatory_rating = case_when(!is.na(english_gustatory_rating) ~ english_gustatory_rating,
                                              TRUE ~ average_gustatory_rating),
         english_haptic_rating = case_when(!is.na(english_haptic_rating) ~ english_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         english_interoceptive_rating = case_when(!is.na(english_interoceptive_rating) ~ english_interoceptive_rating,
                                                  TRUE ~ average_interoceptive_rating),
         english_olfactory_rating = case_when(!is.na(english_olfactory_rating) ~ english_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         english_visual_rating = case_when(!is.na(english_visual_rating) ~ english_visual_rating,
                                           TRUE ~ average_visual_rating)) %>%
  left_join(english_CD) %>%
  filter(`English (all)` != "")
write_csv(english_ratings_subset, "norms/english/english_ratings_subset.csv")
write_rds(english_ratings_subset, "norms/english/english_ratings_subset.rds")


estonian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Estonian`,
         matches("^estonian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(estonian_auditory_rating = case_when(!is.na(estonian_auditory_rating) ~ estonian_auditory_rating,
                                              TRUE ~ average_auditory_rating),
         estonian_boi_rating = average_boi_rating,
         estonian_imageability_rating = average_imageability_rating,
         estonian_concreteness_rating = case_when(!is.na(estonian_concreteness_rating) ~ estonian_concreteness_rating,
                                                  TRUE ~ average_concreteness_rating),         
         estonian_emotionalarousal_rating = case_when(!is.na(estonian_emotionalarousal_rating) ~ estonian_emotionalarousal_rating,
                                                      TRUE ~ average_emotionalarousal_rating),         
         estonian_haptic_rating = case_when(!is.na(estonian_haptic_rating) ~ estonian_haptic_rating,
                                            TRUE ~ average_haptic_rating),
         estonian_gustatory_rating = case_when(!is.na(estonian_gustatory_rating) ~ estonian_gustatory_rating,
                                               TRUE ~ average_gustatory_rating),
         estonian_interoceptive_rating = average_interoceptive_rating,
         estonian_olfactory_rating = case_when(!is.na(estonian_olfactory_rating) ~ estonian_olfactory_rating,
                                               TRUE ~ average_olfactory_rating),
         estonian_visual_rating = case_when(!is.na(estonian_visual_rating) ~ estonian_visual_rating,
                                            TRUE ~ average_visual_rating)) %>%
  left_join(estonian_CD) %>%
  filter(Estonian != "")
write_csv(estonian_ratings_subset, "norms/estonian/estonian_ratings_subset.csv")
write_rds(estonian_ratings_subset, "norms/estonian/estonian_ratings_subset.rds")

finnish_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Finnish`,
         matches("^finnish_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(finnish_auditory_rating = average_auditory_rating,
         finnish_boi_rating = average_boi_rating,
         finnish_imageability_rating = average_imageability_rating,
         finnish_concreteness_rating = average_concreteness_rating,
         finnish_emotionalarousal_rating = average_emotionalarousal_rating,
         finnish_gustatory_rating = average_gustatory_rating,
         finnish_haptic_rating = average_haptic_rating,
         finnish_interoceptive_rating = average_interoceptive_rating,
         finnish_olfactory_rating = average_olfactory_rating,
         finnish_visual_rating = average_visual_rating) %>%
  left_join(finnish_CD) %>%
  filter(Finnish != "")
write_csv(finnish_ratings_subset, "norms/finnish/finnish_ratings_subset.csv")
write_rds(finnish_ratings_subset, "norms/finnish/finnish_ratings_subset.rds")


french_ratings_subset <- CDI_mega_word_list_with_averageratings %>%
  select(uni_lemma, 
         `French (all)`,
         `French (French)`,
         `French (Quebecois)`,
         matches("^french_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(french_auditory_rating = case_when(!is.na(french_auditory_rating) ~ french_auditory_rating,
                                            TRUE ~ average_auditory_rating),
         french_boi_rating = case_when(!is.na(french_boi_rating) ~ french_boi_rating,
                                       TRUE ~ average_boi_rating),
         french_imageability_rating = case_when(!is.na(french_imageability_rating) ~ french_imageability_rating,
                                                TRUE ~ average_imageability_rating),
         french_concreteness_rating = case_when(!is.na(french_concreteness_rating) ~ french_concreteness_rating,
                                                TRUE ~ average_concreteness_rating),
         french_emotionalarousal_rating =  case_when(!is.na(french_emotionalarousal_rating) ~ french_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         french_gustatory_rating = case_when(!is.na(french_gustatory_rating) ~ french_gustatory_rating,
                                             TRUE ~ average_gustatory_rating),
         french_haptic_rating = case_when(!is.na(french_haptic_rating) ~ french_haptic_rating,
                                          TRUE ~ average_haptic_rating),
         french_interoceptive_rating = average_interoceptive_rating,
         french_olfactory_rating = case_when(!is.na(french_olfactory_rating) ~ french_olfactory_rating,
                                             TRUE ~ average_olfactory_rating),
         french_visual_rating = case_when(!is.na(french_visual_rating) ~ french_visual_rating,
                                             TRUE ~ average_visual_rating),) %>%
  left_join(french_CD) %>%
  filter(`French (all)` != "")
write_csv(french_ratings_subset, "norms/french/french_ratings_subset.csv")
write_rds(french_ratings_subset, "norms/french/french_ratings_subset.rds")

german_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `German`,
         matches("^german_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(german_auditory_rating = average_auditory_rating,
         german_boi_rating = average_boi_rating,
         german_imageability_rating = average_imageability_rating,
         german_concreteness_rating = average_concreteness_rating,
         german_emotionalarousal_rating = average_emotionalarousal_rating,
         german_gustatory_rating = average_gustatory_rating,
         german_haptic_rating = average_haptic_rating,
         german_interoceptive_rating = average_interoceptive_rating,
         german_olfactory_rating = average_olfactory_rating,
         german_visual_rating = average_visual_rating) %>%
  left_join(german_CD) %>%
  filter(German != "")
write_csv(german_ratings_subset, "norms/german/german_ratings_subset.csv")
write_rds(german_ratings_subset, "norms/german/german_ratings_subset.rds")


greek_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Greek (Cypriot)`,
         matches("^greek_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(greek_auditory_rating = average_auditory_rating,
         greek_boi_rating = average_boi_rating,
         greek_imageability_rating = average_imageability_rating,
         greek_concreteness_rating = average_concreteness_rating,
         greek_emotionalarousal_rating = average_emotionalarousal_rating,
         greek_gustatory_rating = average_gustatory_rating,
         greek_haptic_rating = average_haptic_rating,
         greek_interoceptive_rating = average_interoceptive_rating,
         greek_olfactory_rating = average_olfactory_rating,
         greek_visual_rating = average_visual_rating) %>%
  left_join(greek_CD) %>%
  filter(`Greek (Cypriot)` != "")
write_csv(greek_ratings_subset, "norms/greek/greek_ratings_subset.csv")
write_rds(greek_ratings_subset, "norms/greek/greek_ratings_subset.rds")

hebrew_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Hebrew`,
         matches("^hebrew_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(hebrew_auditory_rating = average_auditory_rating,
         hebrew_boi_rating = average_boi_rating,
         hebrew_imageability_rating = average_imageability_rating,
         hebrew_concreteness_rating = average_concreteness_rating,
         hebrew_emotionalarousal_rating = average_emotionalarousal_rating,
         hebrew_gustatory_rating = average_gustatory_rating,
         hebrew_haptic_rating = average_haptic_rating,
         hebrew_interoceptive_rating = average_interoceptive_rating,
         hebrew_olfactory_rating = average_olfactory_rating,
         hebrew_visual_rating = average_visual_rating) %>%
  left_join(hebrew_CD) %>%
  filter(Hebrew != "")
write_csv(hebrew_ratings_subset, "norms/hebrew/hebrew_ratings_subset.csv")
write_rds(hebrew_ratings_subset, "norms/hebrew/hebrew_ratings_subset.rds")

hungarian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Hungarian`,
         matches("^hungarian_.*_rating$"), matches("^average_.*_rating$"))  %>%
  distinct() %>%
  mutate(hungarian_auditory_rating = average_auditory_rating,
         hungarian_boi_rating = average_boi_rating,
         hungarian_imageability_rating = average_imageability_rating,
         hungarian_concreteness_rating = average_concreteness_rating,
         hungarian_emotionalarousal_rating = average_emotionalarousal_rating,
         hungarian_gustatory_rating = average_gustatory_rating,
         hungarian_haptic_rating = average_haptic_rating,
         hungarian_interoceptive_rating = average_interoceptive_rating,
         hungarian_olfactory_rating = average_olfactory_rating,
         hungarian_visual_rating = average_visual_rating) %>%
  left_join(hungarian_CD) %>%
  filter(Hungarian != "")
write_csv(hungarian_ratings_subset, "norms/hungarian/hungarian_ratings_subset.csv")
write_rds(hungarian_ratings_subset, "norms/hungarian/hungarian_ratings_subset.rds")


irish_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Irish`,
         matches("^irish_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(irish_auditory_rating = average_auditory_rating,
         irish_boi_rating = average_boi_rating,
         irish_imageability_rating = average_imageability_rating,
         irish_concreteness_rating = average_concreteness_rating,
         irish_emotionalarousal_rating = average_emotionalarousal_rating,
         irish_gustatory_rating = average_gustatory_rating,
         irish_haptic_rating = average_haptic_rating,
         irish_interoceptive_rating = average_interoceptive_rating,
         irish_olfactory_rating = average_olfactory_rating,
         irish_visual_rating = average_visual_rating) %>%
  left_join(irish_CD) %>%
  filter(Irish != "")
write_csv(irish_ratings_subset, "norms/irish/irish_ratings_subset.csv")
write_rds(irish_ratings_subset, "norms/irish/irish_ratings_subset.rds")


italian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Italian`,
         matches("^italian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(italian_auditory_rating = case_when(!is.na(italian_auditory_rating) ~ italian_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         italian_boi_rating = average_boi_rating,
         italian_imageability_rating = average_imageability_rating,
         italian_concreteness_rating = average_concreteness_rating,
         italian_emotionalarousal_rating = average_emotionalarousal_rating,
         italian_gustatory_rating = case_when(!is.na(italian_gustatory_rating) ~ italian_gustatory_rating,
                                              TRUE ~ average_gustatory_rating),
         italian_haptic_rating = case_when(!is.na(italian_haptic_rating) ~ italian_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         italian_interoceptive_rating = average_interoceptive_rating,
         italian_olfactory_rating = case_when(!is.na(italian_olfactory_rating) ~ italian_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         italian_visual_rating = case_when(!is.na(italian_visual_rating) ~ italian_visual_rating,
                                           TRUE ~ average_visual_rating)) %>%
  left_join(italian_CD) %>%
  filter(Italian != "")
write_csv(italian_ratings_subset, "norms/italian/italian_ratings_subset.csv")
write_rds(italian_ratings_subset, "norms/italian/italian_ratings_subset.rds")

japanese_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Japanese`,
         matches("^japanese_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(japanese_auditory_rating = average_auditory_rating,
         japanese_boi_rating = average_boi_rating,
         japanese_imageability_rating = average_imageability_rating,
         japanese_concreteness_rating = average_concreteness_rating,
         japanese_emotionalarousal_rating = average_emotionalarousal_rating,
         japanese_gustatory_rating = average_gustatory_rating,
         japanese_haptic_rating = average_haptic_rating,
         japanese_interoceptive_rating = average_interoceptive_rating,
         japanese_olfactory_rating = average_olfactory_rating,
         japanese_visual_rating = average_visual_rating) %>%
  left_join(japanese_CD) %>%
  filter(Japanese != "")
write_csv(japanese_ratings_subset, "norms/japanese/japanese_ratings_subset.csv")
write_rds(japanese_ratings_subset, "norms/japanese/japanese_ratings_subset.rds")


kigiriama_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Kigiriama`,
         matches("^kigiriama_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(kigiriama_auditory_rating = average_auditory_rating,
         kigiriama_boi_rating = average_boi_rating,
         kigiriama_imageability_rating = average_imageability_rating,
         kigiriama_concreteness_rating = average_concreteness_rating,
         kigiriama_emotionalarousal_rating = average_emotionalarousal_rating,
         kigiriama_gustatory_rating = average_gustatory_rating,
         kigiriama_haptic_rating = average_haptic_rating,
         kigiriama_interoceptive_rating = average_interoceptive_rating,
         kigiriama_olfactory_rating = average_olfactory_rating,
         kigiriama_visual_rating = average_visual_rating) %>%
  filter(Kigiriama != "")
write_csv(kigiriama_ratings_subset, "norms/kigiriama/kigiriama_ratings_subset.csv")
write_rds(kigiriama_ratings_subset, "norms/kigiriama/kigiriama_ratings_subset.rds")


kiswahili_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Kiswahili`,
         matches("^kiswahili_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(kiswahili_auditory_rating = average_auditory_rating,
         kiswahili_boi_rating = average_boi_rating,
         kiswahili_imageability_rating = average_imageability_rating,
         kiswahili_concreteness_rating = average_concreteness_rating,
         kiswahili_emotionalarousal_rating = average_emotionalarousal_rating,
         kiswahili_gustatory_rating = average_gustatory_rating,
         kiswahili_haptic_rating = average_haptic_rating,
         kiswahili_interoceptive_rating = average_interoceptive_rating,
         kiswahili_olfactory_rating = average_olfactory_rating,
         kiswahili_visual_rating = average_visual_rating) %>%
  left_join(kiswahili_CD) %>%
  filter(Kiswahili != "")
write_csv(kiswahili_ratings_subset, "norms/kiswahili/kiswahili_ratings_subset.csv")
write_rds(kiswahili_ratings_subset, "norms/kiswahili/kiswahili_ratings_subset.rds")


korean_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Korean`,
         matches("^korean_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(korean_auditory_rating = average_auditory_rating,
         korean_boi_rating = average_boi_rating,
         korean_imageability_rating = average_imageability_rating,
         korean_concreteness_rating = average_concreteness_rating,
         korean_emotionalarousal_rating = average_emotionalarousal_rating,
         korean_gustatory_rating = average_gustatory_rating,
         korean_haptic_rating = average_haptic_rating,
         korean_interoceptive_rating = average_interoceptive_rating,
         korean_olfactory_rating = average_olfactory_rating,
         korean_visual_rating = average_visual_rating) %>%
  left_join(korean_CD) %>%
  filter(Korean != "")
write_csv(korean_ratings_subset, "norms/korean/korean_ratings_subset.csv")
write_rds(korean_ratings_subset, "norms/korean/korean_ratings_subset.rds")

latvian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Latvian`,
         matches("^latvian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(latvian_auditory_rating = average_auditory_rating,
         latvian_boi_rating = average_boi_rating,
         latvian_imageability_rating = average_imageability_rating,
         latvian_concreteness_rating = average_concreteness_rating,
         latvian_emotionalarousal_rating = average_emotionalarousal_rating,
         latvian_gustatory_rating = average_gustatory_rating,
         latvian_haptic_rating = average_haptic_rating,
         latvian_interoceptive_rating = average_interoceptive_rating,
         latvian_olfactory_rating = average_olfactory_rating,
         latvian_visual_rating = average_visual_rating) %>%
  left_join(latvian_CD) %>%
  filter(Latvian != "")
write_csv(latvian_ratings_subset, "norms/latvian/latvian_ratings_subset.csv")
write_rds(latvian_ratings_subset, "norms/latvian/latvian_ratings_subset.rds")

norwegian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Norwegian`,
         matches("^norwegian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(norwegian_auditory_rating = average_auditory_rating,
         norwegian_boi_rating = average_boi_rating,
         norwegian_imageability_rating = case_when(!is.na(norwegian_imageability_rating) ~ norwegian_imageability_rating,
                                                   TRUE ~ average_imageability_rating),
         norwegian_concreteness_rating = average_concreteness_rating,
         norwegian_emotionalarousal_rating = average_emotionalarousal_rating,
         norwegian_gustatory_rating = average_gustatory_rating,
         norwegian_haptic_rating = average_haptic_rating,
         norwegian_interoceptive_rating = average_interoceptive_rating,
         norwegian_olfactory_rating = average_olfactory_rating,
         norwegian_visual_rating = average_visual_rating) %>%
  left_join(norwegian_CD) %>%
  filter(Norwegian != "")
write_csv(norwegian_ratings_subset, "norms/norwegian/norwegian_ratings_subset.csv")
write_rds(norwegian_ratings_subset, "norms/norwegian/norwegian_ratings_subset.rds")
  
persian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Persian`,
         matches("^persian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(persian_auditory_rating = average_auditory_rating,
         persian_boi_rating = average_boi_rating,
         persian_imageability_rating = average_imageability_rating,
         persian_concreteness_rating = average_concreteness_rating,
         persian_emotionalarousal_rating = average_emotionalarousal_rating,
         persian_gustatory_rating = average_gustatory_rating,
         persian_haptic_rating = average_haptic_rating,
         persian_interoceptive_rating = average_interoceptive_rating,
         persian_olfactory_rating = average_olfactory_rating,
         persian_visual_rating = average_visual_rating) %>%
  left_join(persian_CD) %>%
  filter(Persian != "")
write_csv(persian_ratings_subset, "norms/persian/persian_ratings_subset.csv")
write_rds(persian_ratings_subset, "norms/persian/persian_ratings_subset.rds")

  
portuguese_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Portuguese (European)`,
         matches("^portuguese_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(portuguese_auditory_rating = average_auditory_rating,
         portuguese_boi_rating = average_boi_rating,
         portuguese_imageability_rating = average_imageability_rating,
         portuguese_concreteness_rating = case_when(!is.na(portuguese_concreteness_rating) ~ portuguese_concreteness_rating,
                                                    TRUE ~ average_concreteness_rating),
         portuguese_emotionalarousal_rating = average_emotionalarousal_rating,
         portuguese_gustatory_rating = average_gustatory_rating,
         portuguese_haptic_rating = average_haptic_rating,
         portuguese_interoceptive_rating = average_interoceptive_rating,
         portuguese_olfactory_rating = average_olfactory_rating,
         portuguese_visual_rating = average_visual_rating) %>%
  left_join(portuguese_CD) %>%
  filter(`Portuguese (European)` != "")
write_csv(portuguese_ratings_subset, "norms/portuguese/portuguese_ratings_subset.csv")
write_rds(portuguese_ratings_subset, "norms/portuguese/portuguese_ratings_subset.rds")


russian_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Russian`,
         matches("^russian_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate( russian_auditory_rating = case_when(!is.na(russian_auditory_rating) ~ russian_auditory_rating,
                                              TRUE ~ average_auditory_rating),
         russian_boi_rating = case_when(!is.na(russian_boi_rating) ~ russian_boi_rating,
                                        TRUE ~ average_boi_rating),
         russian_imageability_rating = average_imageability_rating,
         russian_concreteness_rating = average_concreteness_rating,
         russian_emotionalarousal_rating = average_emotionalarousal_rating,
         russian_gustatory_rating = case_when(!is.na(russian_gustatory_rating) ~ russian_gustatory_rating,
                                              TRUE ~ average_gustatory_rating),
         russian_haptic_rating = case_when(!is.na(russian_haptic_rating) ~ russian_haptic_rating,
                                           TRUE ~ average_haptic_rating),
         russian_interoceptive_rating = average_interoceptive_rating,
         russian_olfactory_rating = case_when(!is.na(russian_olfactory_rating) ~ russian_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         russian_visual_rating = case_when(!is.na(russian_visual_rating) ~ russian_visual_rating,
                                           TRUE ~ average_visual_rating)) %>%
  left_join(russian_CD) %>%
  filter(Russian != "")
write_csv(russian_ratings_subset, "norms/russian/russian_ratings_subset.csv")
write_rds(russian_ratings_subset, "norms/russian/russian_ratings_subset.rds")
  
slovak_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Slovak`,
         matches("^slovak_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(slovak_auditory_rating = average_auditory_rating,
         slovak_boi_rating = average_boi_rating,
         slovak_imageability_rating = average_imageability_rating,
         slovak_concreteness_rating = average_concreteness_rating,
         slovak_emotionalarousal_rating = average_emotionalarousal_rating,
         slovak_gustatory_rating = average_gustatory_rating,
         slovak_haptic_rating = average_haptic_rating,
         slovak_interoceptive_rating = average_interoceptive_rating,
         slovak_olfactory_rating = average_olfactory_rating,
         slovak_visual_rating = average_visual_rating) %>%
  left_join(slovak_CD) %>%
  filter(Slovak != "")
write_csv(slovak_ratings_subset, "norms/slovak/slovak_ratings_subset.csv")
write_rds(slovak_ratings_subset, "norms/slovak/slovak_ratings_subset.rds")


spanish_ratings_subset <- CDI_mega_word_list_with_averageratings %>%
  select(uni_lemma, 
         `Spanish (all)`,
         `Spanish (Argentinian)`,
         `Spanish (Chilean)`,
         `Spanish (European)`,
         `Spanish (Mexican)`,
         `Spanish (Peruvian)`,
         matches("^spanish_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(spanish_auditory_rating = case_when(!is.na(spanish_auditory_rating) ~ spanish_auditory_rating,
                                             TRUE ~ average_auditory_rating),
         spanish_boi_rating = case_when(!is.na(spanish_boi_rating) ~ spanish_boi_rating,
                                        TRUE ~ average_boi_rating),
         spanish_imageability_rating = case_when(!is.na(spanish_imageability_rating) ~ spanish_imageability_rating,
                                                 TRUE ~ average_imageability_rating),
         spanish_concreteness_rating = average_concreteness_rating,
         spanish_emotionalarousal_rating = average_emotionalarousal_rating,
         spanish_gustatory_rating = case_when(!is.na(spanish_gustatory_rating) ~ spanish_gustatory_rating,
                                              TRUE ~ average_gustatory_rating),
         spanish_haptic_rating = average_haptic_rating,
         spanish_interoceptive_rating = average_interoceptive_rating,
         spanish_olfactory_rating = case_when(!is.na(spanish_olfactory_rating) ~ spanish_olfactory_rating,
                                              TRUE ~ average_olfactory_rating),
         spanish_visual_rating = average_visual_rating) %>%
  left_join(spanish_CD) %>%
  filter(`Spanish (all)` != "")
write_csv(spanish_ratings_subset, "norms/spanish/spanish_ratings_subset.csv")
write_rds(spanish_ratings_subset, "norms/spanish/spanish_ratings_subset.rds")

swedish_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Swedish`,
         matches("^swedish_.*_rating$"), matches("^average_.*_rating$")) %>%
  distinct() %>%
  mutate(swedish_concreteness_rating = average_concreteness_rating,
         swedish_emotionalarousal_rating = case_when(!is.na(swedish_emotionalarousal_rating) ~ swedish_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         swedish_imageability_rating = case_when(!is.na(swedish_imageability_rating) ~ swedish_imageability_rating,
                                                 TRUE ~ average_imageability_rating),
         swedish_visual_rating = average_visual_rating,
         swedish_olfactory_rating = average_olfactory_rating, 
         swedish_interoceptive_rating = average_interoceptive_rating, 
         swedish_haptic_rating = average_haptic_rating,
         swedish_gustatory_rating = average_gustatory_rating, 
         swedish_boi_rating = average_boi_rating,
         swedish_auditory_rating = average_auditory_rating
  ) %>%
  filter(`Swedish` != "") %>%
  left_join(swedish_CD) 
write_csv(swedish_ratings_subset, "norms/swedish/swedish_ratings_subset.csv")
write_rds(swedish_ratings_subset, "norms/swedish/swedish_ratings_subset.rds")



turkish_ratings_subset <- CDI_mega_word_list_with_averageratings %>% 
  select(uni_lemma, `Turkish`,
         matches("^turkish_.*_rating$"), matches("^average_.*_rating$"))  %>%
  distinct() %>%
  mutate(turkish_concreteness_rating = average_concreteness_rating,
         turkish_emotionalarousal_rating = case_when(!is.na(turkish_emotionalarousal_rating) ~ turkish_emotionalarousal_rating,
                                                     TRUE ~ average_emotionalarousal_rating),
         turkish_imageability_rating = case_when(!is.na(turkish_imageability_rating) ~ turkish_imageability_rating,
                                                 TRUE ~ average_imageability_rating),
         turkish_visual_rating = average_visual_rating,
         turkish_olfactory_rating = average_olfactory_rating, 
         turkish_interoceptive_rating = average_interoceptive_rating, 
         turkish_haptic_rating = average_haptic_rating,
         turkish_gustatory_rating = average_gustatory_rating, 
         turkish_boi_rating = average_boi_rating,
         turkish_auditory_rating = average_auditory_rating,
  ) %>%
  filter(`Turkish` != "") %>%
  left_join(turkish_CD) 
write_csv(turkish_ratings_subset, "norms/turkish/turkish_ratings_subset.csv")
write_rds(turkish_ratings_subset, "norms/turkish/turkish_ratings_subset.rds")


### frequency 

frequency_ratings <- CDI_mega_word_list_with_averageratings %>%
  distinct() %>%
  # add CD and frequency
  left_join(catalan_CD) %>%
  left_join(croatian_CD) %>%
  left_join(chinese_CD) %>%
  left_join(czech_CD) %>%
  left_join(danish_CD) %>% 
  left_join(dutch_CD) %>%
  left_join(english_CD) %>%
  left_join(estonian_CD)  %>%
  left_join(finnish_CD) %>%
  left_join(french_CD) %>%
  left_join(german_CD) %>%
  left_join(greek_CD) %>%
  left_join(hebrew_CD) %>%
  left_join(hungarian_CD) %>%
  left_join(irish_CD) %>%
  left_join(italian_CD) %>%
  left_join(japanese_CD)  %>%
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

write_rds(frequency_ratings, "norms/average_frequency_ratings.rds")

