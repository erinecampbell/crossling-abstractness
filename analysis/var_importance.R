library(ranger)
library(glue)

# asl ----
asl_aoas_for_rf <- asl_aoas %>%
  filter(!is.na(aoa),
         !is.na(asl_auditory_rating),
         !is.na(asl_boi_rating),
         !is.na(asl_concreteness_rating),
         !is.na(asl_gustatory_rating),
         !is.na(asl_iconicity_rating),
         !is.na(asl_imageability_rating),
         !is.na(asl_interoceptive_rating),
         !is.na(asl_olfactory_rating),
         !is.na(asl_visual_rating),
         !is.na(asl_frequency_rating),
         !is.na(asl_phoncomp_rating)) %>% 
  rename_with(~str_remove(., 'asl_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_asl_aoa_words <- asl_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_asl <- 
ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
         iconicity + imageability + interoceptive + olfactory + visual + 
         frequency + lexical_category + phoncomp, 
       data = asl_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
asl_rf_importance <- importance(rf_asl) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "American Sign Language")%>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100) %>%
  arrange(-prop_importance) %>%
  mutate(importance_ranking = seq(1:n()),
         n_words = n_asl_aoa_words)

# bsl----
bsl_aoas_for_rf <- bsl_aoas %>%
  filter(!is.na(aoa),
         !is.na(bsl_auditory_rating),
         !is.na(bsl_boi_rating),
         !is.na(bsl_concreteness_rating),
         !is.na(bsl_gustatory_rating),
         !is.na(bsl_imageability_rating),
         !is.na(bsl_interoceptive_rating),
         !is.na(bsl_olfactory_rating),
         !is.na(bsl_visual_rating)) %>% 
  rename_with(~str_remove(., 'bsl_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_bsl_aoa_words <- bsl_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_bsl <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           lexical_category, 
         data = bsl_aoas_for_rf, mtry = 5/3, importance = "impurity" )
bsl_rf_importance <- importance(rf_bsl) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "British Sign Language") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%
  arrange(-prop_importance) %>%
  mutate(importance_ranking = seq(1:n()),
         n_words = n_bsl_aoa_words)
# cantonese----
cantonese_aoas_for_rf <- cantonese_aoas %>%
  filter(!is.na(aoa),
         !is.na(chinese_auditory_rating),
         !is.na(chinese_boi_rating),
         !is.na(chinese_concreteness_rating),
         !is.na(chinese_gustatory_rating),
         !is.na(chinese_imageability_rating),
         !is.na(chinese_interoceptive_rating),
         !is.na(chinese_olfactory_rating),
         !is.na(chinese_visual_rating),
         !is.na(chinese_frequency_rating)) %>% 
  rename_with(~str_remove(., 'chinese_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_cantonese_aoa_words <- cantonese_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_cantonese <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = cantonese_aoas_for_rf, mtry = 5/3, importance = "impurity" )
cantonese_rf_importance <- importance(rf_cantonese) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Cantonese") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%
  arrange(-prop_importance) %>%
  mutate(importance_ranking = seq(1:n()))
# croatian----
croatian_aoas_for_rf <- croatian_aoas %>%
  filter(!is.na(aoa),
         !is.na(croatian_auditory_rating),
         !is.na(croatian_boi_rating),
         !is.na(croatian_concreteness_rating),
         !is.na(croatian_gustatory_rating),
         !is.na(croatian_imageability_rating),
         !is.na(croatian_interoceptive_rating),
         !is.na(croatian_olfactory_rating),
         !is.na(croatian_visual_rating),
         !is.na(croatian_frequency_rating)) %>% 
  rename_with(~str_remove(., 'croatian_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_croatian_aoa_words <- croatian_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_croatian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = croatian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
croatian_rf_importance <- importance(rf_croatian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Croatian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%
  arrange(-prop_importance) %>%
  mutate(importance_ranking = seq(1:n()))
# czech----
czech_aoas_for_rf <- czech_aoas %>%
  filter(!is.na(aoa),
         !is.na(czech_auditory_rating),
         !is.na(czech_boi_rating),
         !is.na(czech_concreteness_rating),
         !is.na(czech_gustatory_rating),
         !is.na(czech_imageability_rating),
         !is.na(czech_interoceptive_rating),
         !is.na(czech_olfactory_rating),
         !is.na(czech_visual_rating),
         !is.na(czech_frequency_rating)) %>% 
  rename_with(~str_remove(., 'czech_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_czech_aoa_words <- czech_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_czech <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = czech_aoas_for_rf, mtry = 5/3, importance = "impurity" )
czech_rf_importance <- importance(rf_czech) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Czech") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)
# danish----
danish_aoas_for_rf <- danish_aoas %>%
  filter(!is.na(aoa),
         !is.na(danish_auditory_rating),
         !is.na(danish_boi_rating),
         !is.na(danish_concreteness_rating),
         !is.na(danish_gustatory_rating),
         !is.na(danish_imageability_rating),
         !is.na(danish_interoceptive_rating),
         !is.na(danish_olfactory_rating),
         !is.na(danish_visual_rating),
         !is.na(danish_frequency_rating)) %>% 
  rename_with(~str_remove(., 'danish_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_danish_aoa_words <- danish_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_danish <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = danish_aoas_for_rf, mtry = 5/3, importance = "impurity" )
danish_rf_importance <- importance(rf_danish) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Danish") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# dutch----
dutch_aoas_for_rf <- dutch_aoas %>%
  filter(!is.na(aoa),
         !is.na(dutch_auditory_rating),
         !is.na(dutch_boi_rating),
         !is.na(dutch_concreteness_rating),
         !is.na(dutch_gustatory_rating),
         !is.na(dutch_imageability_rating),
         !is.na(dutch_interoceptive_rating),
         !is.na(dutch_olfactory_rating),
         !is.na(dutch_visual_rating),
         !is.na(dutch_frequency_rating)) %>% 
  rename_with(~str_remove(., 'dutch_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_dutch_aoa_words <- dutch_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_dutch <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = dutch_aoas_for_rf, mtry = 5/3, importance = "impurity" )
dutch_rf_importance <- importance(rf_dutch) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Dutch") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

# english----
american_english_aoas_for_rf <- american_english_aoas %>%
  filter(!is.na(aoa),
         !is.na(english_auditory_rating),
         !is.na(english_boi_rating),
         !is.na(english_concreteness_rating),
         !is.na(english_gustatory_rating),
         !is.na(english_imageability_rating),
         !is.na(english_interoceptive_rating),
         !is.na(english_olfactory_rating),
         !is.na(english_visual_rating),
         !is.na(english_american_frequency_rating),
         !is.na(english_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'english_')) %>% 
  rename_with(~str_remove(., '_rating')) %>%
  rename(frequency=american_frequency)
n_american_english_aoa_words <- american_english_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_american_english <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = american_english_aoas_for_rf, mtry = 5/3, importance = "impurity" )
american_english_rf_importance <- importance(rf_american_english) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "English (American)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
australian_english_aoas_for_rf <- australian_english_aoas %>%
  filter(!is.na(aoa),
         !is.na(english_auditory_rating),
         !is.na(english_boi_rating),
         !is.na(english_concreteness_rating),
         !is.na(english_gustatory_rating),
         !is.na(english_imageability_rating),
         !is.na(english_interoceptive_rating),
         !is.na(english_olfactory_rating),
         !is.na(english_visual_rating),
         !is.na(english_british_frequency_rating),
         !is.na(english_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'english_')) %>% 
  rename_with(~str_remove(., '_rating')) %>%
  rename(frequency=british_frequency)
n_australian_english_aoa_words <- australian_english_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_australian_english <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity+
           frequency + lexical_category, 
         data = australian_english_aoas_for_rf, mtry = 5/3, importance = "impurity" )
australian_english_rf_importance <- importance(rf_australian_english) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "English (Australian)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

british_english_aoas_for_rf <- british_english_aoas %>%
  filter(!is.na(aoa),
         !is.na(english_auditory_rating),
         !is.na(english_boi_rating),
         !is.na(english_concreteness_rating),
         !is.na(english_gustatory_rating),
         !is.na(english_imageability_rating),
         !is.na(english_interoceptive_rating),
         !is.na(english_olfactory_rating),
         !is.na(english_visual_rating),
         !is.na(english_british_frequency_rating),
         !is.na(english_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'english_')) %>% 
  rename_with(~str_remove(., '_rating')) %>%
  rename(frequency=british_frequency)
n_british_english_aoa_words <- british_english_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_british_english <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = british_english_aoas_for_rf, mtry = 5/3, importance = "impurity" )
british_english_rf_importance <- importance(rf_british_english) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "English (British)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

irish_english_aoas_for_rf <- irish_english_aoas %>%
  filter(!is.na(aoa),
         !is.na(english_auditory_rating),
         !is.na(english_boi_rating),
         !is.na(english_concreteness_rating),
         !is.na(english_gustatory_rating),
         !is.na(english_imageability_rating),
         !is.na(english_interoceptive_rating),
         !is.na(english_olfactory_rating),
         !is.na(english_visual_rating),
         !is.na(english_british_frequency_rating),
         !is.na(english_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'english_')) %>% 
  rename_with(~str_remove(., '_rating')) %>%
  rename(frequency=british_frequency)
rf_irish_english <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = irish_english_aoas_for_rf, mtry = 5/3, importance = "impurity" )
n_irish_english_aoa_words <- irish_english_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
irish_english_rf_importance <- importance(rf_irish_english) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "English (Irish)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

# finnish----
finnish_aoas_for_rf <- finnish_aoas %>%
  filter(!is.na(aoa),
         !is.na(finnish_auditory_rating),
         !is.na(finnish_boi_rating),
         !is.na(finnish_concreteness_rating),
         !is.na(finnish_gustatory_rating),
         !is.na(finnish_imageability_rating),
         !is.na(finnish_interoceptive_rating),
         !is.na(finnish_olfactory_rating),
         !is.na(finnish_visual_rating),
         !is.na(finnish_frequency_rating)) %>% 
  rename_with(~str_remove(., 'finnish_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_finnish_aoa_words <- finnish_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_finnish <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = finnish_aoas_for_rf, mtry = 5/3, importance = "impurity" )
finnish_rf_importance <- importance(rf_finnish) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Finnish") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# french----
french_european_aoas_for_rf <- french_european_aoas %>%
  filter(!is.na(aoa),
         !is.na(french_auditory_rating),
         !is.na(french_boi_rating),
         !is.na(french_concreteness_rating),
         !is.na(french_gustatory_rating),
         !is.na(french_imageability_rating),
         !is.na(french_interoceptive_rating),
         !is.na(french_olfactory_rating),
         !is.na(french_visual_rating),
         !is.na(french_frequency_rating)) %>% 
  rename_with(~str_remove(., 'french_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_french_european_aoa_words <- french_european_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_french_european <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = french_european_aoas_for_rf, mtry = 5/3, importance = "impurity" )
french_european_rf_importance <- importance(rf_french_european) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "French (European)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# german----
german_aoas_for_rf <- german_aoas %>%
  filter(!is.na(aoa),
         !is.na(german_auditory_rating),
         !is.na(german_boi_rating),
         !is.na(german_concreteness_rating),
         !is.na(german_gustatory_rating),
         !is.na(german_imageability_rating),
         !is.na(german_interoceptive_rating),
         !is.na(german_olfactory_rating),
         !is.na(german_visual_rating),
         !is.na(german_frequency_rating),) %>% 
  rename_with(~str_remove(., 'german_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_german_aoa_words <- german_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_german <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = german_aoas_for_rf, mtry = 5/3, importance = "impurity" )
german_rf_importance <- importance(rf_german) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "German") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# greek----
greek_aoas_for_rf <- greek_aoas %>%
  filter(!is.na(aoa),
         !is.na(greek_auditory_rating),
         !is.na(greek_boi_rating),
         !is.na(greek_concreteness_rating),
         !is.na(greek_gustatory_rating),
         !is.na(greek_imageability_rating),
         !is.na(greek_interoceptive_rating),
         !is.na(greek_olfactory_rating),
         !is.na(greek_visual_rating),
         !is.na(greek_frequency_rating),) %>% 
  rename_with(~str_remove(., 'greek_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_greek_aoa_words <- greek_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_greek <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = greek_aoas_for_rf, mtry = 5/3, importance = "impurity" )
greek_rf_importance <- importance(rf_greek) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Greek (Cypriot)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# hebrew----
hebrew_aoas_for_rf <- hebrew_aoas %>%
  filter(!is.na(aoa),
         !is.na(hebrew_auditory_rating),
         !is.na(hebrew_boi_rating),
         !is.na(hebrew_concreteness_rating),
         !is.na(hebrew_gustatory_rating),
         !is.na(hebrew_imageability_rating),
         !is.na(hebrew_interoceptive_rating),
         !is.na(hebrew_olfactory_rating),
         !is.na(hebrew_visual_rating),
         !is.na(hebrew_frequency_rating),) %>% 
  rename_with(~str_remove(., 'hebrew_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_hebrew_aoa_words <- hebrew_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_hebrew <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = hebrew_aoas_for_rf, mtry = 5/3, importance = "impurity" )
hebrew_rf_importance <- importance(rf_hebrew) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Hebrew") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# hungarian----
hungarian_aoas_for_rf <- hungarian_aoas %>%
  filter(!is.na(aoa),
         !is.na(hungarian_auditory_rating),
         !is.na(hungarian_boi_rating),
         !is.na(hungarian_concreteness_rating),
         !is.na(hungarian_gustatory_rating),
         !is.na(hungarian_imageability_rating),
         !is.na(hungarian_interoceptive_rating),
         !is.na(hungarian_olfactory_rating),
         !is.na(hungarian_visual_rating),
         !is.na(hungarian_frequency_rating),) %>% 
  rename_with(~str_remove(., 'hungarian_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_hungarian_aoa_words <- asl_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_hungarian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = hungarian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
hungarian_rf_importance <- importance(rf_hungarian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Hungarian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# italian----
italian_aoas_for_rf <- italian_aoas %>%
  filter(!is.na(aoa),
         !is.na(italian_auditory_rating),
         !is.na(italian_boi_rating),
         !is.na(italian_concreteness_rating),
         !is.na(italian_gustatory_rating),
         !is.na(italian_imageability_rating),
         !is.na(italian_interoceptive_rating),
         !is.na(italian_olfactory_rating),
         !is.na(italian_visual_rating),
         !is.na(italian_frequency_rating),) %>% 
  rename_with(~str_remove(., 'italian_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_italian_aoa_words <- italian_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_italian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = italian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
italian_rf_importance <- importance(rf_italian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Italian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# korean----
korean_aoas_for_rf <- korean_aoas %>%
  filter(!is.na(aoa),
         !is.na(korean_auditory_rating),
         !is.na(korean_boi_rating),
         !is.na(korean_concreteness_rating),
         !is.na(korean_gustatory_rating),
         !is.na(korean_imageability_rating),
         !is.na(korean_interoceptive_rating),
         !is.na(korean_olfactory_rating),
         !is.na(korean_visual_rating),
         !is.na(korean_frequency_rating),) %>% 
  rename_with(~str_remove(., 'korean_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_korean_aoa_words <- korean_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_korean <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = korean_aoas_for_rf, mtry = 5/3, importance = "impurity" )
korean_rf_importance <- importance(rf_korean) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Korean") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# latvian----
latvian_aoas_for_rf <- latvian_aoas %>%
  filter(!is.na(aoa),
         !is.na(latvian_auditory_rating),
         !is.na(latvian_boi_rating),
         !is.na(latvian_concreteness_rating),
         !is.na(latvian_gustatory_rating),
         !is.na(latvian_imageability_rating),
         !is.na(latvian_interoceptive_rating),
         !is.na(latvian_olfactory_rating),
         !is.na(latvian_visual_rating),
         !is.na(latvian_frequency_rating)) %>% 
  rename_with(~str_remove(., 'latvian_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_latvian_aoa_words <- latvian_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_latvian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = latvian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
latvian_rf_importance <- importance(rf_latvian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Latvian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# norwegian----
norwegian_aoas_for_rf <- norwegian_aoas %>%
  filter(!is.na(aoa),
         !is.na(norwegian_auditory_rating),
         !is.na(norwegian_boi_rating),
         !is.na(norwegian_concreteness_rating),
         !is.na(norwegian_gustatory_rating),
         !is.na(norwegian_imageability_rating),
         !is.na(norwegian_interoceptive_rating),
         !is.na(norwegian_olfactory_rating),
         !is.na(norwegian_visual_rating),
         !is.na(norwegian_frequency_rating)) %>% 
  rename_with(~str_remove(., 'norwegian_')) %>% 
  rename_with(~str_remove(., '_rating'))
n_norwegian_aoa_words <- norwegian_aoas_for_rf %>% 
  distinct(item_definition) %>%
  nrow()
rf_norwegian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = norwegian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
norwegian_rf_importance <- importance(rf_norwegian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Norwegian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# persian----
persian_aoas_for_rf <- persian_aoas %>%
  filter(!is.na(aoa),
         !is.na(persian_auditory_rating),
         !is.na(persian_boi_rating),
         !is.na(persian_concreteness_rating),
         !is.na(persian_gustatory_rating),
         !is.na(persian_imageability_rating),
         !is.na(persian_interoceptive_rating),
         !is.na(persian_olfactory_rating),
         !is.na(persian_visual_rating)) %>% 
  rename_with(~str_remove(., 'persian_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_persian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           lexical_category, 
         data = persian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
persian_rf_importance <- importance(rf_persian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Persian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))
# portuguese----
portuguese_aoas_for_rf <- portuguese_aoas %>%
  filter(!is.na(aoa),
         !is.na(portuguese_auditory_rating),
         !is.na(portuguese_boi_rating),
         !is.na(portuguese_concreteness_rating),
         !is.na(portuguese_gustatory_rating),
         !is.na(portuguese_imageability_rating),
         !is.na(portuguese_interoceptive_rating),
         !is.na(portuguese_olfactory_rating),
         !is.na(portuguese_visual_rating)) %>% 
  rename_with(~str_remove(., 'portuguese_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_portuguese <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           lexical_category, 
         data = portuguese_aoas_for_rf, mtry = 5/3, importance = "impurity" )
portuguese_rf_importance <- importance(rf_portuguese) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Portuguese (European)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

# russian----
russian_aoas_for_rf <- russian_aoas %>%
  filter(!is.na(aoa),
         !is.na(russian_auditory_rating),
         !is.na(russian_boi_rating),
         !is.na(russian_concreteness_rating),
         !is.na(russian_gustatory_rating),
         !is.na(russian_imageability_rating),
         !is.na(russian_interoceptive_rating),
         !is.na(russian_olfactory_rating),
         !is.na(russian_visual_rating),
         !is.na(russian_frequency_rating)) %>% 
  rename_with(~str_remove(., 'russian_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_russian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = russian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
russian_rf_importance <- importance(rf_russian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Russian") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))




# slovak----
slovak_aoas_for_rf <- slovak_aoas %>%
  filter(!is.na(aoa),
         !is.na(slovak_auditory_rating),
         !is.na(slovak_boi_rating),
         !is.na(slovak_concreteness_rating),
         !is.na(slovak_gustatory_rating),
         !is.na(slovak_imageability_rating),
         !is.na(slovak_interoceptive_rating),
         !is.na(slovak_olfactory_rating),
         !is.na(slovak_visual_rating),
         !is.na(slovak_frequency_rating)) %>% 
  rename_with(~str_remove(., 'slovak_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_slovak <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = slovak_aoas_for_rf, mtry = 5/3, importance = "impurity" )
slovak_rf_importance <- importance(rf_slovak) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Slovak") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))



# swedish----
swedish_aoas_for_rf <- swedish_aoas %>%
  filter(!is.na(aoa),
         !is.na(swedish_auditory_rating),
         !is.na(swedish_boi_rating),
         !is.na(swedish_concreteness_rating),
         !is.na(swedish_gustatory_rating),
         !is.na(swedish_imageability_rating),
         !is.na(swedish_interoceptive_rating),
         !is.na(swedish_olfactory_rating),
         !is.na(swedish_visual_rating),
         !is.na(swedish_frequency_rating)) %>% 
  rename_with(~str_remove(., 'swedish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_swedish <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = swedish_aoas_for_rf, mtry = 5/3, importance = "impurity" )
swedish_rf_importance <- importance(rf_swedish) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Swedish") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

# spanish----

spanish_argentinian_aoas_for_rf <- spanish_argentinian_aoas %>%
  filter(!is.na(aoa),
         !is.na(spanish_auditory_rating),
         !is.na(spanish_boi_rating),
         !is.na(spanish_concreteness_rating),
         !is.na(spanish_gustatory_rating),
         !is.na(spanish_imageability_rating),
         !is.na(spanish_interoceptive_rating),
         !is.na(spanish_olfactory_rating),
         !is.na(spanish_visual_rating),
         !is.na(spanish_frequency_rating),
         !is.na(spanish_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'spanish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_spanish_argentinian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = spanish_argentinian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
spanish_argentinian_rf_importance <- importance(rf_spanish_argentinian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Spanish (Argentinian)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

spanish_chilean_aoas_for_rf <- spanish_chilean_aoas %>%
  filter(!is.na(aoa),
         !is.na(spanish_auditory_rating),
         !is.na(spanish_boi_rating),
         !is.na(spanish_concreteness_rating),
         !is.na(spanish_gustatory_rating),
         !is.na(spanish_imageability_rating),
         !is.na(spanish_interoceptive_rating),
         !is.na(spanish_olfactory_rating),
         !is.na(spanish_visual_rating),
         !is.na(spanish_frequency_rating),
         !is.na(spanish_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'spanish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_spanish_chilean <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = spanish_chilean_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
spanish_chilean_rf_importance <- importance(rf_spanish_chilean) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Spanish (Chilean)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))


spanish_european_aoas_for_rf <- spanish_european_aoas %>%
  filter(!is.na(aoa),
         !is.na(spanish_auditory_rating),
         !is.na(spanish_boi_rating),
         !is.na(spanish_concreteness_rating),
         !is.na(spanish_gustatory_rating),
         !is.na(spanish_imageability_rating),
         !is.na(spanish_interoceptive_rating),
         !is.na(spanish_olfactory_rating),
         !is.na(spanish_visual_rating),
         !is.na(spanish_frequency_rating),
         !is.na(spanish_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'spanish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_spanish_european <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = spanish_european_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
spanish_european_rf_importance <- importance(rf_spanish_european) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Spanish (European)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

spanish_mexican_aoas_for_rf <- spanish_mexican_aoas %>%
  filter(!is.na(aoa),
         !is.na(spanish_auditory_rating),
         !is.na(spanish_boi_rating),
         !is.na(spanish_concreteness_rating),
         !is.na(spanish_gustatory_rating),
         !is.na(spanish_imageability_rating),
         !is.na(spanish_interoceptive_rating),
         !is.na(spanish_olfactory_rating),
         !is.na(spanish_visual_rating),
         !is.na(spanish_frequency_rating),
         !is.na(spanish_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'spanish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_spanish_mexican <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity+
           frequency + lexical_category, 
         data = spanish_mexican_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
spanish_mexican_rf_importance <- importance(rf_spanish_mexican) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Spanish (Mexican)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))

spanish_peruvian_aoas_for_rf <- spanish_peruvian_aoas %>%
  filter(!is.na(aoa),
         !is.na(spanish_auditory_rating),
         !is.na(spanish_boi_rating),
         !is.na(spanish_concreteness_rating),
         !is.na(spanish_gustatory_rating),
         !is.na(spanish_imageability_rating),
         !is.na(spanish_interoceptive_rating),
         !is.na(spanish_olfactory_rating),
         !is.na(spanish_visual_rating),
         !is.na(spanish_frequency_rating),
         !is.na(spanish_iconicity_rating)) %>% 
  rename_with(~str_remove(., 'spanish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_spanish_peruvian <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + iconicity +
           frequency + lexical_category, 
         data = spanish_peruvian_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
spanish_peruvian_rf_importance <- importance(rf_spanish_peruvian) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Spanish (Peruvian)") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))


# turkish----
turkish_aoas_for_rf <- turkish_aoas %>%
  filter(!is.na(aoa),
         !is.na(turkish_auditory_rating),
         !is.na(turkish_boi_rating),
         !is.na(turkish_concreteness_rating),
         !is.na(turkish_gustatory_rating),
         !is.na(turkish_imageability_rating),
         !is.na(turkish_interoceptive_rating),
         !is.na(turkish_olfactory_rating),
         !is.na(turkish_visual_rating),
         !is.na(turkish_frequency_rating)) %>% 
  rename_with(~str_remove(., 'turkish_')) %>% 
  rename_with(~str_remove(., '_rating'))
rf_turkish <- 
  ranger(formula = aoa ~ auditory + boi + concreteness + gustatory + 
           imageability + interoceptive + olfactory + visual + 
           frequency + lexical_category, 
         data = turkish_aoas_for_rf, mtry = 5/3, importance = "impurity" )
print(rf_asl)
turkish_rf_importance <- importance(rf_turkish) %>% 
  data.frame() %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable")%>%
  mutate(language = "Turkish") %>%
  mutate(prop_importance = (importance / sum(importance,na.rm = TRUE))*100)  %>%   arrange(-prop_importance) %>%   mutate(importance_ranking = seq(1:n()))



# all ----

rf_importance_all <- bind_rows(asl_rf_importance,
                               bsl_rf_importance,
                               american_english_rf_importance,
                               australian_english_rf_importance,
                               british_english_rf_importance,
                               irish_english_rf_importance,
                               cantonese_rf_importance,
                               croatian_rf_importance,
                               czech_rf_importance,
                               danish_rf_importance,
                               dutch_rf_importance,
                               french_european_rf_importance,
                               finnish_rf_importance,
                               german_rf_importance,
                               greek_rf_importance,
                               hebrew_rf_importance,
                               hungarian_rf_importance,
                               italian_rf_importance,
                               korean_rf_importance,
                               latvian_rf_importance,
                               norwegian_rf_importance,
                               persian_rf_importance,
                               portuguese_rf_importance,
                               russian_rf_importance,
                               slovak_rf_importance,
                               spanish_argentinian_rf_importance,
                               spanish_chilean_rf_importance,
                               spanish_european_rf_importance,
                               spanish_mexican_rf_importance,
                               spanish_peruvian_rf_importance,
                               swedish_rf_importance,
                               turkish_rf_importance)


rf_importance_plot <-  
  ggplot(rf_importance_all) +
  geom_col(aes(x=language, fill=fct_reorder((variable),prop_importance), 
               color=fct_reorder((variable),prop_importance), y=prop_importance), 
           position="stack",alpha=.5) +
  theme_minimal() +
  coord_flip()
rf_importance_plot
