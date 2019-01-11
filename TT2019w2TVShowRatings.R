# Written January 2019 for #TidyTuesday TV Ratings


# Data source https://www.economist.com/graphic-detail/2018/11/24/tvs-golden-age-is-real
# https://www.imdb.com/

library(dplyr)
library(stringi)
library(tidyr)
library(ggplot2)


# Upload csv file
df1 <- read.csv("C:/Users/OConnor Analytics/Working Files/TidyTuesday/2019/January/TTGithubfiles/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# Exploratory
# look at the type of variables
sapply(df1, class) 

# convert date to date format
df1$date <- as.Date(df1$date)

# why do titleID and title have a different # of factors?
# genres has 97 factor levels - lets disaggregate into separate columns with regex, then gather

# first, count the genre.count to see how many genre elements there are in each row.

df1$genre.count <- stri_count_regex(df1$genres, ",") + 1 
summary(df1$genre.count) # max of 2 genre.count / 3 terms per genres



# create df2 for ease of revision

df2 <- df1


# extract each term, remove commas, convert NA's to "none", and turn these into a 23 factors combined
# the none factor will be removed later. 
df2$extract1 <- gsub(",", "", stri_extract(df2$genres, regex="^[:alpha:]*,?"))
df2$extract2 <- gsub(",", "", stri_extract(df2$genres, regex=",{1}[:alpha:]*,?"))
df2$extract3 <- gsub(",", "", stri_extract(df2$genres, regex=",{1}[:alpha:]*$"))


for (i in 1:nrow(df2))
if (is.na(df2$extract1[i])) {
  df2$extract1[i] <- "none"
}

for (i in 1:nrow(df2))
  if (is.na(df2$extract2[i])) {
    df2$extract2[i] <- "none"
  }

for (i in 1:nrow(df2))
  if (is.na(df2$extract3[i])) {
    df2$extract3[i] <- "none"
  }

df2$extract1     <- as.factor(df2$extract1)
df2$extract2     <- as.factor(df2$extract2)
df2$extract3     <- as.factor(df2$extract3)


# Create a list of the genres in 3 steps. 
# We will use this to create new columns, and then reshape the data
# 1) make a character vector of extract levels & 2) convert to factor
# 3)compress to a character vector of levels and        

genre.list <- levels(as.factor(c(levels(df2$extract1), levels(df2$extract2), levels(df2$extract3))))

# make new df3 for ease of revision
df3 <- df2

# create a new variable for each of the 22 levels
df3[,genre.list]  <- "zzz"


# move these new columns to the front of df3.
# this will make it easy to go through a logic test later.

df3 <- df3 %>%
  select(Action:Western, extract1:extract3, everything()) 


# now, start logic testing the match between col names 1:22 vs. extracts1:3
# This looks for matches between genre.list column names and the extracts1:3
# TRUE indicates a match

for (i in 1:length(genre.list))
  for (j in 1:nrow(df3))
    if  (colnames(df3[i])    == as.character(df3$extract1[j])) {
      df3[j,i] <- TRUE
    } else {
     if (colnames(df3[i])    == as.character(df3$extract2[j])) {
        df3[j,i] <- TRUE
     } else {
        if (colnames(df3[i]) == as.character(df3$extract3[j])) {
          df3[j,i] <- TRUE
        } else {
          df3[j,i] <- FALSE
        }
      }
    }
 


# Its time to reshape!
# Make a new df to ease of revision
# gather genres into one column, match them to extracts split from genres

df4 <- df3

df4 <- df4 %>%
  select(Action:Western, titleId:genre.count) %>%
  gather(genre, truefalse, -titleId, -seasonNumber, -title,-date, -av_rating, -share,
         -genres, -genre.count) %>%
  mutate(genre = as.factor(genre))%>%
  filter(truefalse == TRUE) %>%
  filter(genre != "none") %>%
  mutate(genre = factor(genre)) %>% # to eliminate the "none" factor
  arrange(desc(genre), share) %>%
  mutate(genre.count = as.factor(genre.count))

# This plot will look at the distribution of
# share vs. rating by # of genre's tagged to the TV show.


# using ggplot2

g <- ggplot(df4, aes(date, av_rating))
p <- g + geom_point(aes(color=genre.count)) + labs(title="TV Shows Ratings by Number of Genres, 1990-2018",
                                                   caption ="Source:www.economist.com/graphic-detail/2018/11/24/tvs-golden-age-is-real",
                                                   subtitle="Created January 2019") +
 ylab("Average Rating") + xlab("Year") + theme(legend.position="bottom")
        
print(p)

