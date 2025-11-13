library(tidyverse)
library(wordbankr)
library(polyglotr)

# fyi: running this script requires internet access
# get CDI words ----

CDI_instrument_list <- wordbankr::get_instruments()


# Create an empty dataframe to store the results of wordbank loop
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


CDI_dictionary <- CDI_mega_word_list %>% 
  select(-form) %>%
  arrange(str_length(item_definition)) %>%
  distinct(uni_lemma, language, .keep_all = TRUE) %>%
  # mutate(item_definition = tolower(item_definition)) %>%
  pivot_wider(names_from = language, values_from = item_definition) %>%
  mutate("English (all)" = case_when(!is.na(`English (American)`) ~ `English (American)`,
                                     !is.na(`English (British)`) ~ `English (British)`,
                                     !is.na(`English (Australian)`) ~ `English (Australian)`,
                                     !is.na(`English (Irish)`) ~ `English (Irish)`,
                                     TRUE ~ uni_lemma),
         "French (all)" = case_when(!is.na(`French (French)`) ~ `French (French)`,
                                    !is.na(`French (Quebecois)`) ~ `French (Quebecois)`,      
                                    TRUE ~ NA),
         "Spanish (all)" = case_when(!is.na(`Spanish (Mexican)`) ~ `Spanish (Mexican)`,
                                     !is.na(`Spanish (Argentinian)`) ~ `Spanish (Argentinian)`,
                                     !is.na(`Spanish (Chilean)`) ~ `Spanish (Chilean)`,
                                     !is.na(`Spanish (European)`) ~ `Spanish (European)`,
                                     !is.na(`Spanish (Peruvian)`) ~ `Spanish (Peruvian)`,
                                     TRUE ~ NA),
         "Chinese (all)" = case_when(!is.na(`Mandarin (Beijing)`) ~ `Mandarin (Beijing)`,
                                     !is.na(`Mandarin (Taiwanese)`) ~ `Mandarin (Taiwanese)`, 
                                     !is.na(`Cantonese`) ~ `Cantonese`, 
                                     TRUE ~ NA)) %>%
  mutate(`Portuguese (European)` = tolower(`Portuguese (European)`),
         Turkish = tolower(Turkish))
write_csv(CDI_dictionary, "norms/CDI_dictionary.csv")

# all of this next section has to be run together
# we only need to do this step for languages that have norms associated with them
# must be connected to stable internet for the google_translate function to run

# for bsl, we can just use the words from british english

CDI_translation_dictionary <- CDI_dictionary %>% 
  mutate(uni_lemma = case_when(uni_lemma == "1PL" ~ "we",
                               uni_lemma == "1PL.POSS" ~ "our",
                               uni_lemma == "1PL.REFL" ~ "ourselves",
                               uni_lemma == "1SG" ~ "I",
                               uni_lemma == "1SG.POSS" ~ "my",
                               uni_lemma == "1SG.REFL" ~ "myself",
                               uni_lemma == "2PL" ~ "y'all",
                               uni_lemma == "2PL.POSS" ~ "y'all's",
                               uni_lemma == "2PL.REFL" ~ "yourselves",
                               uni_lemma == "2SG" ~ "you",
                               uni_lemma == "2SG.POSS" ~ "your",
                               uni_lemma == "2SG.REFL" ~ "yourself",
                               uni_lemma == "3PL" ~ "they",
                               uni_lemma == "3PL.POSS" ~ "their",
                               uni_lemma == "3PL.REFL" ~ "themselves",
                               uni_lemma == "3SG" ~ "she",  # just alternated gender here for the 3rd person singulars
                               uni_lemma == "3SG.POSS" ~ "his",
                               uni_lemma == "3SG.REFL" ~ "herself",
                               uni_lemma == "i" ~ "I",
                               TRUE ~ uni_lemma))

CDI_mega_dictionary <- CDI_translation_dictionary %>% 
  mutate(dictionary_croatian = case_when(!is.na(Croatian) ~ Croatian,
                                         is.na(Croatian) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "hr")))
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_dutch = case_when(!is.na(Dutch) ~ Dutch,
                                         is.na(Dutch) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "nl")))                                   
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_estonian = case_when(!is.na(`English (all)`) ~ `English (all)`,
                                         is.na(`English (all)`) ~ uni_lemma))  

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_estonian = case_when(!is.na(Estonian) ~ Estonian,
                                       is.na(Estonian) ~ google_translate(text = uni_lemma, 
                                                                       source_language = "en", 
                                                                       target_language = "et")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_french = case_when(!is.na(`French (all)`) ~ `French (all)`,
                                         is.na(`French (all)`) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "fr")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_greek = case_when(!is.na(`Greek (Cypriot)`) ~ `Greek (Cypriot)`,
                                       is.na(`Greek (Cypriot)`) ~ google_translate(text = uni_lemma, 
                                                                                source_language = "en", 
                                                                                target_language = "el")))                                   
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_italian = case_when(!is.na(Italian) ~ Italian,
                                      is.na(Italian) ~ google_translate(text = uni_lemma, 
                                                                      source_language = "en", 
                                                                      target_language = "it")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_japanese = case_when(!is.na(Japanese) ~ Japanese,
                                        is.na(Japanese) ~ google_translate(text = uni_lemma, 
                                                                          source_language = "en", 
                                                                          target_language = "ja")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_chinese = case_when(!is.na(`Chinese (all)`) ~ `Chinese (all)`,
                                         is.na(`Chinese (all)`) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "zh"))) 

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_japanese = case_when(!is.na(Japanese) ~ Japanese,
                                         is.na(Japanese) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "ja"))) 

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_norwegian = case_when(!is.na(Norwegian) ~ Norwegian,
                                         is.na(Norwegian) ~ google_translate(text = uni_lemma, 
                                                                            source_language = "en", 
                                                                            target_language = "no")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_portuguese = case_when(!is.na(`Portuguese (European)`) ~ `Portuguese (European)`,
                                          is.na(`Portuguese (European)`) ~ google_translate(text = uni_lemma, 
                                                                              source_language = "en", 
                                                                              target_language = "pt")))                                   

CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_russian = case_when(!is.na(Russian) ~ Russian,
                                           is.na(Russian) ~ google_translate(text = uni_lemma, 
                                                                                source_language = "en", 
                                                                                target_language = "ru")))                                   
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_spanish = case_when(!is.na(`Spanish (all)`) ~ `Spanish (all)`,
                                       is.na(`Spanish (all)`) ~ google_translate(text = uni_lemma, 
                                                                                source_language = "en", 
                                                                                target_language = "es")))
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_swedish = case_when(!is.na(Swedish) ~ Swedish,
                                        is.na(Swedish) ~ google_translate(text = uni_lemma, 
                                                                          source_language = "en", 
                                                                          target_language = "sv")))                                   
CDI_mega_dictionary <- CDI_mega_dictionary %>% 
  mutate(dictionary_turkish = case_when(!is.na(Turkish) ~ Turkish,
                                        is.na(Turkish) ~ google_translate(text = uni_lemma, 
                                                                          source_language = "en", 
                                                                          target_language = "tr")))                                   


write_csv(CDI_mega_dictionary, "norms/CDI_mega_dictionary.csv")
