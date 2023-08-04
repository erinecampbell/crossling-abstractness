
all_auditory_summaries <- read_rds("models/effects/all_auditory_summaries.rds") 
all_boi_summaries <- read_rds("models/effects/all_boi_summaries.rds") 
all_concreteness_summaries <- read_rds("models/effects/all_concreteness_summaries.rds")
all_contextdiversity_summaries <- read_rds("models/effects/all_CD_summaries.rds") 
all_emotionalarousal_summaries <- read_rds("models/effects/all_emotionalarousal_summaries.rds") 
all_gustatory_summaries <- read_rds("models/effects/all_gustatory_summaries.rds")
all_haptic_summaries <- read_rds("models/effects/all_haptic_summaries.rds") 
all_iconicity_summaries <- read_rds("models/effects/all_iconicity_summaries.rds") 
all_imageability_summaries <- read_rds("models/effects/all_imageability_summaries.rds") 
all_interoceptive_summaries <- read_rds("models/effects/all_interoceptive_summaries.rds") 
all_olfactory_summaries <- read_rds("models/effects/all_olfactory_summaries.rds") 
all_socialness_summaries <- read_rds("models/effects/all_socialness_summaries.rds")
all_visual_summaries <- read_rds("models/effects/all_visual_summaries.rds") 

all_summaries <- bind_rows(all_auditory_summaries,
                         all_boi_summaries,
                         all_concreteness_summaries,
                         all_contextdiversity_summaries,
                         all_emotionalarousal_summaries,
                         all_gustatory_summaries,
                         all_haptic_summaries,
                         all_iconicity_summaries,
                         all_imageability_summaries,
                         all_interoceptive_summaries,
                         all_olfactory_summaries,
                         all_socialness_summaries,
                         all_visual_summaries
                         ) %>%
  mutate(variable_category = case_when(variable %in% c("auditory", "Body Object Interaction", "Concreteness", "Gustatory", "haptic", "imageability", "interoceptive", "olfactory", "visual") ~ "sensorimotor",
                                       variable %in% c("Emotional Arousal", "socialness") ~ "social-emotional",
                                       variable == "iconicity" ~ "form-meaning relationship",
                                       variable == "contextdiversity" ~ "word usage"),
         effect_size = Estimate,
         standard_error = `Std. Error`)

summary_of_maineffects <- ggplot(all_summaries, aes(y=reorder(variable, Estimate, FUN=mean), x=Estimate, color=variable_category, fill=variable_category)) +
  geom_vline(xintercept = 0)+
  geom_point(aes(shape = significant), alpha=.3, size = 3) +
  # geom_density_ridges(alpha=.5)+
  #add density or half violin
  stat_summary(fun.data = "mean_cl_boot", color = 'black') + #take this out bc spread will be shown by density
  scale_shape_manual(values = c(significant = 19, ns = 1)) +
  theme_minimal() +
  ylab("") +
  theme(text=element_text(size=14)) +
  scale_color_manual(values = c("#3B9AB2","#CC0000", "#EBCC2A", "#081D58"))+
  scale_fill_manual(values = c("#3B9AB2","#CC0000", "#EBCC2A", "#081D58"))+
  stat_summary(fun.data = "mean_cl_boot", color = 'black') +
  ggtitle("Variable Effects in Each Language") + 
  guides(shape="none")
  
summary_of_maineffects
ggsave(plot=summary_of_maineffects,filename="models/plots/summary_of_maineffects.png", device="png")


summaries_summary <- all_summaries %>%
  group_by(variable) %>%
  summarise(mean_effect_size = mean(effect_size),
            min_effect_size = min(effect_size),
            min_effect_size_language = language[which.min(effect_size)],
            max_effect_size = max(effect_size),
            max_effect_size_language = language[which.max(effect_size)],
            n_datasets = n())





all_auditory_interaction_summaries <- read_rds("models/effects/all_auditory_interaction_summaries.rds") 
all_boi_interaction_summaries <- read_rds("models/effects/all_boi_interaction_summaries.rds") 
all_concreteness_interaction_summaries <- read_rds("models/effects/all_concreteness_interaction_summaries.rds")
all_contextdiversity_interaction_summaries <- read_rds("models/effects/all_CD_interaction_summaries.rds") 
all_emotionalarousal_interaction_summaries <- read_rds("models/effects/all_emotionalarousal_interaction_summaries.rds") 
all_gustatory_interaction_summaries <- read_rds("models/effects/all_gustatory_interaction_summaries.rds")
all_haptic_interaction_summaries <- read_rds("models/effects/all_haptic_interaction_summaries.rds") 
all_iconicity_interaction_summaries <- read_rds("models/effects/all_iconicity_interaction_summaries.rds") 
all_imageability_interaction_summaries <- read_rds("models/effects/all_imageability_interaction_summaries.rds") 
all_interoceptive_interaction_summaries <- read_rds("models/effects/all_interoceptive_interaction_summaries.rds") 
all_olfactory_interaction_summaries <- read_rds("models/effects/all_olfactory_interaction_summaries.rds") 
all_socialness_interaction_summaries <- read_rds("models/effects/all_socialness_interaction_summaries.rds")
all_visual_interaction_summaries <- read_rds("models/effects/all_visual_interaction_summaries.rds") 

all_interaction_summaries <- bind_rows(all_auditory_interaction_summaries,
                           all_boi_interaction_summaries,
                           all_concreteness_interaction_summaries,
                           all_contextdiversity_interaction_summaries,
                           all_emotionalarousal_interaction_summaries,
                           all_gustatory_interaction_summaries,
                           all_haptic_interaction_summaries,
                           all_iconicity_interaction_summaries,
                           all_imageability_interaction_summaries,
                           all_interoceptive_interaction_summaries,
                           all_olfactory_interaction_summaries,
                           all_socialness_interaction_summaries,
                           all_visual_interaction_summaries
) %>%
  mutate(variable = str_remove_all(variable, "age_"),
    variable_category = case_when(variable %in% c("auditory", "Body Object Interaction", "Concreteness", "Gustatory", "haptic", "imageability", "interoceptive", "olfactory", "visual") ~ "sensorimotor",
                                       variable %in% c("Emotional Arousal", "socialness") ~ "social-emotional",
                                       variable == "iconicity" ~ "form-meaning relationship",
                                       variable == "contextdiversity" ~ "word usage"),
         effect_size = Estimate,
         standard_error = `Std. Error`,
    variable = paste0("Age * ", variable))

summary_of_interactions <- ggplot(all_interaction_summaries, aes(y=reorder(variable, Estimate, FUN=mean), x=Estimate, color=variable_category)) +
  geom_vline(xintercept = 0)+
  geom_point(aes(shape = significant), alpha=.3, size = 3) +
  stat_summary(fun.data = "mean_cl_boot", color = 'black') +
  scale_shape_manual(values = c(significant = 19, ns = 1)) +
  theme_minimal() +
  ylab("") +
  theme(text=element_text(size=14)) +
  scale_color_manual(values = c("#3B9AB2","#CC0000", "#EBCC2A", "#081D58")) +
  ggtitle("Age Interactions of Abstractness Variables on\nLikelihood of Child Producing Word")
summary_of_interactions
ggsave(plot=summary_of_interactions,filename="models/plots/summary_of_interactions.png", device="png")


interactions_summary <- all_interaction_summaries %>%
  group_by(variable) %>%
  summarise(mean_effect_size = mean(effect_size),
            min_effect_size = min(effect_size),
            min_effect_size_language = language[which.min(effect_size)],
            max_effect_size = max(effect_size),
            max_effect_size_language = language[which.max(effect_size)],
            n_datasets = n())


by_language_effects <- ggplot(all_interaction_summaries, aes(y=language, x=Estimate, color=variable_category)) +
  geom_vline(xintercept = 0)+
  geom_point(aes(alpha = significant), size = 3) +
  scale_alpha_manual(values = c(significant = .8, ns = .2)) +
  theme_minimal() +
  ylab("") +
  theme(legend.position = "none", text=element_text(size=14)) +
  scale_color_manual(values = c("#3B9AB2","#CC0000", "#EBCC2A", "#081D58"))
ggplot(all_summaries, aes(y=language, x=Estimate, color=variable)) +
  geom_vline(xintercept = 0)+
  geom_point(aes(alpha = significant), size = 3) +
  scale_alpha_manual(values = c(significant = .8, ns = .1)) +
  theme_minimal() +
  ylab("") +
  theme(text=element_text(size=14)) 

by_language_effects
by_language_effects +
  facet_wrap(.~language)

# what are our goals for this plot? ^
#rank the variables within language
# a histogram of the rankings, colored by category

