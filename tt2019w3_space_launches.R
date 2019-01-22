# Tidy Tuesday 2019 w3
# Space Launches

library(tidyverse)
library(cowplot)
library(magick)   # for image in final chart
library(RColorBrewer) # to get other colors


# First, load tidytuesday data and an image for the final chart
urlfilelaunch   <- 'https://raw.githubusercontent.com/TheEconomist/graphic-detail-data/master/data/2018-10-20_space-launches/launches.csv'
urlfileagency   <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/agencies.csv'
challengercrew  <- "http://en.es-static.us/upl/2013/01/space_shuttle_challenger_crew.jpeg"


df      <- read_csv(url(urlfilelaunch))
dfa     <- read_csv(url(urlfileagency))

str(df)
summary(df)
table(df$tag)

# to start the cleaning, lets change the format of some of the character variables to factors

dfl1 <- df %>%
  mutate(type = as.factor(type),
         agency = as.factor(agency),
         state_code = as.factor(state_code),
         category = as.factor(category),
         agency_type = as.factor(agency_type))

# Now, let's filter information about the Space Shuttle flights and add a name for each shuttle


dfl2 <- dfl1 %>%
  filter(agency == "US", type == "Space Shuttle") %>%
  arrange(launch_date)

dfl2$name <- NA

for (i in 1:nrow(dfl2))
if (grepl("OV-099", dfl2[i, "mission"]) == TRUE) {
  dfl2[i, "name"] <- "Challenger"
} else {
  if (grepl("OV-102", dfl2[i, "mission"]) == TRUE) {
    dfl2[i, "name"] <- "Columbia"
  } else {
    if (grepl("OV-103", dfl2[i, "mission"]) == TRUE) {
      dfl2[i, "name"] <- "Discovery" 
      } else {
          if (grepl("OV-104", dfl2[i, "mission"]) == TRUE) {
            dfl2[i, "name"] <- "Atlantis" 
          } else {
            dfl2[i, "name"] <-  "Endeavor"
            }
          }
        }
      }


# convert name to a factor

dfl3 <- dfl2 %>%
  mutate(name = as.factor(name))

# Now, lets make the violin chart and add an image of the crew of the Challenger

g <- ggplot(dfl3, aes(name, launch_year, fill=name))
p <- g + geom_violin(scale = "area") + 
  scale_fill_brewer(palette = "Blues") + theme_minimal() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .4, fill="black") +
  labs(title ="Space Shuttle Launches", subtitle= "1981-2011",
       caption = "Source: raw.githubusercontent.com/TheEconomist/graphic-detail-data/master/data/2018-10-20_space-launches",
       SIZE=8) +
  theme(plot.caption = element_text(hjust=.1)) +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_color_brewer(palette="Blues")

p2 <-  ggdraw() + draw_image(challengercrew, scale = 0.9) +
  draw_label("In Memory of the Challenger Crew", x=.5, y=.9, hjust=0.5, vjust=0.5)
p3 <- add_sub(p2, "Perished January 28, 1986", x=.5, y=3, hjust=0.5, vjust = 0.5)


  
    

plot_grid(p, p3)
print(p)

