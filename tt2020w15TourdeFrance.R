# Tidy Tuesday 2020 Week 15
# Tour de France
# data provided by Alastair Rushworth



library(remotes)
devtools::install_github("alastairrushworth/tdf")
library(tdf)
library(tidyverse)

glimpse(editions)

library(ggthemes)
library(ggplot2)
library(ggrepl)

caption <- paste (strwrap ("TidyTuesday wk 15 April 2020 @RegisOconnor"), collapse = "\n")

editions %>%
  ggplot(aes(y =weight / height ^2, x = distance/time_overall, color = edition)) +
  geom_point(na.rm = TRUE, size = 3) +
  geom_smooth(aes(y = weight / height ^2, x = distance/time_overall))+
  xlab('Average Speed - km / h') +
  ylab('BMI') +
  ggtitle('Tour de France Winners BMI vs. speed') +
  theme(legend.position = "none")  +
  geom_label_repel(data = editions %>% sample_n(10), 
                   aes(label = winner_name), size = 2.5,  
                   nudge_y = 4, na.rm = TRUE,
                   segment.alpha = 0.2) + 
  labs(caption = caption) +
  theme_tufte()
 

  