# TidyTuesday w5 Incarceration
# created 1/27/2016 R 3.5

library(tidyverse)

library(RCurl) # to load csv from git hub
x <- getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/prison_population.csv")
y <- read.csv(text=x)


# extract NY data
df1 <- y %>%
  filter(state =="NY") %>%
  mutate(county = as.factor(as.character(county_name))) %>%
  select(-county_name, -region, -division, -state)
 
# segregate the gender based records
df2 <- df1 %>%
  filter(pop_category == "Male" | pop_category == "Female") %>%

# segregate the ethnicity based records
df3 <- df1 %>%
  filter(pop_category != "Male") %>%
  filter(pop_category != "Female") %>%
  filter(pop_category != "Total")

# There are only 3 counties in this data classified as urban.
# This next extract will pull them - Erie, Monroe & NYC
# also removing data from 1970-86 cuz of NAs

df4 <- df3 %>%
  filter(urbanicity == "urban") %>%
  mutate(ratio      =   prison_population/population) %>%
  filter(year > 1989) %>%
  filter(pop_category != "Other") %>%
  rename(Ethnicity = pop_category)



# 6 plots, 3 lines per plot, to compare erie to monroe & nyc over time.

par(mfrow=c(1,3), mar=c(2,0,0,0))

g <- ggplot(df4)
p <- g +  geom_point(aes(y=ratio, x=year, color = Ethnicity)) + 
  geom_line(aes(y=ratio, x=year, color=Ethnicity)) +
  theme_bw(base_family = "Times") +
  scale_x_continuous() + xlab("Year")  + facet_grid(.~county) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  ylab("Ratio of Incarcerated to Ethnic Popluation") +
    labs(title    = "How do Erie County incarceration rates by ethnicity compare to Monroe and New York County?",
         subtitle = "% Incarcerated, by Ethnicity & County, 1990-2015",
         caption  = "January 2019 | Source: The Vera Institute, https://github.com/vera-institute/incarceration_trends | @REGISOCONNOR") +
  theme(legend.position = "bottom") +
  theme(plot.caption=element_text(hjust=.1))

print(p)






