library(tidyverse)
library(plotly)
library(htmlwidgets)
library(scales)

ppbc_dat <- read_csv(
  file.path('..', "raw_data", "ManuscriptSummaries", "predicted_prob_by_class.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names() |>
  mutate(occurrence = as.factor(occurrence))

aex <- filter(ppbc_dat, species == 'Blephilia ciliata')

ggplot(aex, aes(predicted_prob, colour = occurrence, fill = occurrence)) + 
  geom_density() + 
  ylim(0,1)

ggplot(aex, aes(x = predicted_prob, fill = occurrence)) +
  ggdist::stat_halfeye(alpha = 0.6) +
  scale_fill_manual(values = c('#8B2635', '#dce3f0')) + 
  labs(x = "LOO predicted probability", fill = "Observed") + 
  theme_minimal() + 
  labs(
    title    = "Predicted Probability by Class",
    subtitle = "LOO predicted probability distributions for pseudo-absences (0) and presences (1)",
    caption  = "Less overlap between distributions indicates the model better\nseparates suitable from unsuitable habitat. A high-performing model shows\ntwo distinct, separated peaks."
    )
