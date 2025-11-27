library(corrplot)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggdendro)

CDI_mega_word_list_with_averageratings <- read_rds("norms/CDI_mega_word_list_with_averageratings.rds")


average_ratings <-CDI_mega_word_list_with_averageratings %>% 
  remove_rownames() %>%
  distinct(uni_lemma,.keep_all = TRUE) %>%
  filter(!is.na(uni_lemma)) %>%
  column_to_rownames("uni_lemma") %>%
  select(matches("^average_.*_rating$"))%>%
  # select(matches("^average_.*_rating$"), english_socialness_rating, -average_socialness_rating)%>%
  # rename("average_socialness_rating" = "english_socialness_rating") %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  rename_with(~str_remove(., 'average_')) %>% 
  rename_with(~str_remove(., '_rating')) 

average_ratings_matrix <- cor(average_ratings, use = "pairwise.complete.obs") 
# corrplot(average_ratings_matrix, "color",type="upper", order = "alphabet",
#          tl.col="black", addCoef.col = "white", col=colorRampPalette(c("#CC0000","white","#3B9AB2"))(200))


hclustered_ratings <- hclust(as.dist(average_ratings_matrix), method = "ward.D2")
# plot(hclustered_ratings, hang= 1)
# using dendrogram objects
# triangular dendrogram
average_ratings_dendro <- ggdendrogram(hclustered_ratings, theme_dendro = TRUE, rotate = TRUE)

