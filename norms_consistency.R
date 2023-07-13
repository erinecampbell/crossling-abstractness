library(tidyverse)
library(readxl)
library(corrplot)

# iconicity ----
english_iconicity<- read.csv("norms/english/iconicity_ratings_cleaned.csv") %>%
  dplyr::rename(english_iconicity_rating = rating,
                english_word=word)

asl_iconicity<- read.csv("norms/asl/ASL-LEX_Data.csv") %>%
  dplyr::rename(english_gloss = english_gloss_clean,
                asl_word = Entry.ID) %>%
  mutate(asl_iconicity_rating = case_when(
    Deaf.Signer.Iconicity == "N/A" ~ Non.Signer.Iconicity,
    Deaf.Signer.Iconicity != "N/A" ~ Deaf.Signer.Iconicity
  )) %>%
  filter(asl_iconicity_rating != "N/A") %>%
  mutate(asl_iconicity_rating = as.numeric(asl_iconicity_rating)) %>%
  select(asl_word, english_gloss, asl_iconicity_rating)

spanish_iconicity <- read_xlsx("norms/spanish/europeanspanish/word_ratings.xlsx") %>%
  dplyr::rename(spanish_word = word, 
                spanish_iconicity_rating = `ico-m`) 

mega_iconicity <- 
  english_iconicity %>%
  left_join(asl_iconicity, by = c("english_word" = "english_gloss")) %>%
  left_join(spanish_iconicity, by = c("english_word" = "english_gloss"))
cor.test(mega_iconicity$english_iconicity_rating, mega_iconicity$spanish_iconicity_rating)


spanish_emotions <- read_xlsx("norms/spanish/europeanspanish/Hinojosa et al_Supplementary materials.xlsx") %>%
  dplyr::rename(spanish_word = Word,
                english_gloss = Word_english)
  



# perceptual ratings ----

## dutch
dutch_perceptual <- read_xlsx("norms/dutch/SpeedBrysbaert_Norms.xlsx") %>%
  dplyr::rename(dutch_word = Woord, 
                dutch_auditory_rating = Horen,
                dutch_visual_rating = Zien,
                dutch_olfactory_rating = Ruiken,
                dutch_gustatory_rating = Proeven,
                dutch_haptic_rating = Voelen,
                dutch_interoceptive_rating = Sensaties,
                dutch_dominant_perceptual_rating = Modality,
                dutch_exclusivity_rating = ModalityExclusivity,
                dutch_maxperceptual_rating = MaxPercStrength,
                dutch_imageability_rating = Imageability) 
## english
english_perceptual <- read_csv("norms/english/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  dplyr::rename(english_word = Word, 
                english_auditory_rating = Auditory.mean,
                english_visual_rating = Visual.mean,
                english_olfactory_rating = Olfactory.mean,
                english_gustatory_rating = Gustatory.mean,
                english_haptic_rating = Haptic.mean,
                english_interoceptive_rating = Interoceptive.mean,
                english_dominant_perceptual_rating = Dominant.perceptual,
                english_exclusivity_rating = Exclusivity.perceptual,
                english_maxperceptual_rating = Max_strength.perceptual) %>%
  mutate(english_word = tolower(english_word))
## french
french_perceptual <- read_excel("norms/french/europeanfrench/perceptualinteroceptive_miceli2021.xlsx")%>%
  dplyr::rename(french_word = MOT, 
                english_gloss = WORD,
                french_auditory_rating = Auditory_Mean,
                french_visual_rating = Visual_Mean,
                french_olfactory_rating = Olfactory_Mean,
                french_gustatory_rating = Gustatory_Mean,
                french_haptic_rating = Haptic_Mean,
                french_dominant_perceptual_rating = `Dominant Modality`,
                french_exclusivity_rating = `Modality Exclusivity (%)`) %>%
  mutate(english_gloss = tolower(english_gloss))
## italian
italian_perceptual <- read_delim("norms/italian/Italian_Perceptual_Norms.txt", delim=" ")%>%
  dplyr::rename(italian_word = Ita_Word, 
                english_gloss = Eng_Word,
                italian_auditory_rating = Auditory,
                italian_visual_rating = Visual,
                italian_olfactory_rating = Olfactory,
                italian_gustatory_rating = Gustatory,
                italian_haptic_rating = Haptic,
                italian_dominant_perceptual_rating = mod_e,
                italian_maxperceptual_rating = max,
                italian_exclusivity_rating = mod_exc) %>%
  mutate(english_gloss = tolower(english_gloss))
## mandarin
chinese_perceptual <- read_excel("norms/chinese/SensorimotorNormsforChineseNouns.xlsx") %>%
  dplyr::rename(chinese_word = pinyin, 
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
  mutate(english_gloss = tolower(english_gloss))
## russian
russian_perceptual <- read_excel("norms/russian/miklashevsky2018.xlsx") %>%
  dplyr::rename(russian_word = Transliteration, 
                english_gloss = English_Translation,
                russian_auditory_rating = Aud_Mean,
                russian_visual_rating = Vis_Mean,
                russian_olfactory_rating = Olf_Mean,
                russian_gustatory_rating = Gus_Mean,
                russian_haptic_rating = Hap_Mean,
                russian_imageability_rating = Img_Mean) %>%
  mutate(english_gloss = tolower(english_gloss))
## spanish
spanish_perceptual <- read_excel("norms/spanish/europeanspanish/sensoryexperienceratings_diezalamo2019.xlsx") %>%
  dplyr::rename(spanish_word = word, 
                english_gloss = `translation into English`,
                spanish_maxperceptual_rating = SER_m) %>%
  mutate(english_gloss = tolower(english_gloss),
         spanish_word = tolower(spanish_word))

## mega perceptual
mega_perceptual <- left_join(english_perceptual, french_perceptual, 
                             by = c("english_word" = "english_gloss")) %>%
  left_join(italian_perceptual, 
            by = c("english_word" = "english_gloss")) %>%
  left_join(chinese_perceptual, 
            by = c("english_word" = "english_gloss")) %>%
  left_join(russian_perceptual, 
            by = c("english_word" = "english_gloss")) %>%
  left_join(spanish_perceptual, 
            by = c("english_word" = "english_gloss")) %>%
  left_join(dutch_perceptual,
            by = c())

visual_corrs <- cor(mega_perceptual[,c(7,47,72,88,145)], use="pairwise.complete.obs")
corrplot(visual_corrs, "shade", addCoef.col = "white", "upper")
ggplot(mega_perceptual, aes(x=english_visual_rating,y=french_visual_rating,color=(english_visual_rating - french_visual_rating))) +
  geom_abline(intercept =c(0,0), slope = 1) +
  coord_cartesian(ylim=c(0,5))+
  geom_point() +
  geom_label_repel(aes(label=english_word)) +
  theme_classic()

auditory_corrs <- cor(mega_perceptual[,c(2,49,68,89,147)], use="pairwise.complete.obs")
corrplot(auditory_corrs, "shade", addCoef.col = "white", "upper")

olfactory_corrs <- cor(mega_perceptual[,c(6,55,71,91,149)], use="pairwise.complete.obs")
corrplot(olfactory_corrs, "shade", addCoef.col = "white", "upper")

haptic_corrs <- cor(mega_perceptual[,c(4,51,70,92,153)], use="pairwise.complete.obs")
corrplot(haptic_corrs, "shade", addCoef.col = "white", "upper")

gustatory_corrs <- cor(mega_perceptual[,c(3,53,69,90,151)], use="pairwise.complete.obs")
corrplot(gustatory_corrs, "shade", addCoef.col = "white", "upper")

interoceptive_corrs <- cor(mega_perceptual[,c(5,93)], use="pairwise.complete.obs")
corrplot(interoceptive_corrs, "shade", addCoef.col = "white", "upper")


# concreteness

english_concreteness <- read.csv("norms/english/brysbaert_concreteness.csv") %>%
  rename(english_word = Word,
         english_concreteness_rating = Conc.M)
dutch_concreteness <- read.csv("norms/dutch/verheyen_concreteness2019.csv") %>%
  rename(dutch_word = Words,
         dutch_concreteness_rating = mean)
french_concreteness <- read_excel("norms/french/europeanfrench/concreteness_bonin2018.xlsx") %>%
  rename(french_word = items,
         french_concreteness_rating = Concreteness_mean)
# chinese_concreteness <-  read_excel("norms/chinese/yao_2017.pdf") %>%
portuguese_concreteness <- soares_norms <- read_csv("norms/portuguese/european/soares_norms.csv") %>%
  rename(portuguese_word = `Word (Portuguese)`,
         portuguese_concreteness_rating = Conc_M,
         portuguese_imageability_rating = Imag_M)
italian_ratings <- read_csv("norms/italian/italian_ratings.csv", 
                            skip = 1) %>%
  rename(italian_word = Ita_Word,
         italian_concreteness_rating = M_Con,
         italian_imageability_rating = M_Ima)

# imageability
french_imageability <- read_excel("norms/french/europeanfrench/Desrochers-BRM-2009/Desrochers-Thompson_2009_Ratings.xls") %>%
  rename(french_word = NOUN,
         french_imageability_rating = IMAGE_Mean)
croatian_ratings <- read_excel("norms/croatian/Pretraga-22.3.2023.xlsx") %>%
  mutate(KONKRETNOST = as.numeric(KONKRETNOST),
         PREDOČIVOST = as.numeric(PREDOČIVOST),
         RIJEČ = tolower(RIJEČ)) %>%
  group_by(RIJEČ) %>%
    summarise(croatian_concreteness_rating = mean(KONKRETNOST, na.rm=TRUE),
              croatian_imageability_rating = mean(PREDOČIVOST, na.rm=TRUE)) %>%
  rename(croatian_word = RIJEČ) 
