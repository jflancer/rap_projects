# Jake Flancer
# 6/16/18

library(geniusr)
library(plyr)
library(tidyverse)
library(tidytext)

# Acess Token: ulC2q7eNhlAnuodOFrBsoa1PzHw1fKo5IXX8STrW3mXfeIdfnnwvHsQUSGOCWYWr
genius_token(force = T)

# Kanye Albums
# Kids See Ghosts 425948
# Watch the Thrones 350432
# Yeezus 34024
# TLOP 120604
# Graduation 4345
# Ye 424304
# MBDTF 4343
# The College Dropout 4342
# 808s 4339
# Late Registration 4352

kanye_albums <- c(425948,
                  350432,
                  34024,
                  120604,
                  4345,
                  424304,
                  4343,
                  4342,
                  4339,
                  4352)

scrape_album_url <- function(song_list) {
  a <- song_list$song_lyrics_url
  b <- data.frame()
  for(i in 1:length(a)){
    info <- tryCatch(scrape_lyrics_url(a[i]), error = function(e){data.frame()})
    if(nrow(info) != 0){b <- rbind(b,info)}
  }
  return(b)
}


album_info <- lapply(kanye_albums, FUN = get_album_meta)
tracklists <- lapply(kanye_albums, FUN = scrape_tracklist)
song_lyrics <- lapply(tracklists, FUN = scrape_album_url)
names(song_lyrics) <- sapply(album_info, FUN = function(x){x$album_name})


song_data <- ldply(song_lyrics, data.frame)
colnames(song_data)[1] <- "album_name"
song_data <- as.tibble(song_data)

song_tidy <- song_data %>%
  unnest_tokens(word, line)

data("stop_words")

song_tidy <- song_tidy %>%
  anti_join(stop_words)


words <- song_tidy %>%
  dplyr::count(word) #%>%
#  filter(n > 100) %>%
#  mutate(word = reorder(word,n))# %>%
  # ggplot(aes(word,n)) +
  # geom_col() +
  # xlab(NULL) +
  # coord_flip() +
  # ggtitle("Yeezy Taught Me")

song_tidy$word <- recode(song_tidy$word, homie = "friend", 
                         mama = "mother", 
                         bout = "about",
                         amazing = "amazingly",
                         free = "freedom",
                         diamonds = "diamond",
                         friends = "friend",
                         ghosts = "ghost",
                         feelin = "feeling",
                         momma = "mother",
                         died = "die",
                         children = "child",
                         kids = "child",
                         heavens = "heaven",
                         promised = "promise",
                         rich = "wealth",
                         dawg = "friend",
                         wife = "spouse",
                         fly = "cool",
                         died = "die",
                         slaves = "slave")

song_tidy <- subset(song_tidy, !(word %in% c("bitch","ass","damn","hell")))

song_tidy[which(substr(song_tidy$album_name,1,3) == "Wat"),]$album_name <- "Watch the Throne"


frequency <- song_tidy %>%
  dplyr::count(album_name, word) %>%
  group_by(album_name) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n)

nrc <- get_sentiments("nrc")

words <- words %>%
  left_join(nrc)

feelings <- song_tidy %>%
  inner_join(nrc) %>%
  group_by(album_name) %>%
  dplyr::count(sentiment)

feelings_prop <- feelings %>%
  group_by(album_name) %>%
  dplyr::mutate(proportion = n/sum(n))

feelings_prop$album_name_fact <- ordered(feelings_prop$album_name, 
                                        levels = c("The College Dropout",
                                                   "Late Registration",
                                                   "Graduation",
                                                   "808s & Heartbreak",
                                                   "My Beautiful Dark Twisted Fantasy",
                                                   "Watch the Throne",
                                                   "Yeezus",
                                                   "The Life of Pablo",
                                                   "ye",
                                                   "KIDS SEE GHOSTS"))

ggplot(feelings_prop, aes(sentiment, proportion, fill = sentiment)) +
  geom_col(position = "stack") +
  facet_wrap(~album_name_fact, nrow = 2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  coord_polar() +
  ggtitle("I hate being Bi-Polar its awesome", subtitle = "Sentiment Analysis of Kanye West's Albums using NRC Lexicon") +
  scale_fill_manual(values = c("firebrick","khaki1","darkolivegreen4","gray18","darkorchid1","coral1","goldenrod1","darkgrey","yellow2","royalblue"))

temp <- data.frame(album_numb = 1:10, album_name = levels(feelings_prop$album_name_fact))

#Moving Averages
moving_data <- song_tidy %>%
  right_join(temp) %>%
  group_by(album_name) %>%
  dplyr::mutate(line_number = row_number()) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(album_numb) %>%
  mutate(moving_avg = (lag(score) + lag(score,2) + lag(score,3) + lag(score,4) + lag(score,5) + lag(score,6) + lag(score,7) + lag(score,8) + lag(score,9) + lag(score,10))/10,
#  mutate(moving_avg = (lag(score) + lag(score,2) + lag(score,3) + lag(score,4) + lag(score,5))/5,
           ordering = as.POSIXct(line_number, origin = "0001-01-01"))

moving_data$album_name <- ordered(moving_data$album_name, 
                                                        levels = c("The College Dropout",
                                                                   "Late Registration",
                                                                   "Graduation",
                                                                   "808s & Heartbreak",
                                                                   "My Beautiful Dark Twisted Fantasy",
                                                                   "Watch the Throne",
                                                                   "Yeezus",
                                                                   "The Life of Pablo",
                                                                   "ye",
                                                                   "KIDS SEE GHOSTS"))
moving_data$moving_avg[is.na(moving_data$moving_avg)] <- 0

song_start <- moving_data$ordering[!duplicated(moving_data$song_name)]
song_album <- moving_data$album_name[!duplicated(moving_data$song_name)]
song_names <- unique(moving_data$song_name)
song_labeller <- data.frame(ordering = song_start, song_name = song_names, ht = rep(3,length(song_names), album_name = song_album))

moving_data2 <- left_join(moving_data, song_labeller, by = c("ordering","song_name")) %>%
  mutate(song_start = ifelse(is.na(ht),NA,ordering))

ggplot(moving_data2, aes(ordering, moving_avg)) +
  #geom_line(alpha = 0.4, color = "darkgrey", size = 0.75) +
  geom_smooth(method = "loess", span = 0.2, color = "red", se = F, size = 2) +
  geom_vline(aes(xintercept = song_start), alpha = 0.4, color = "darkblue") +
  # geom_text(aes(x=ordering,y=ht,label=song_name), alpha = 0.2) +
  facet_wrap(~album_name, nrow =2, scales = "free_x") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  ylab("Sentiment from 5 (Positive) to -5 (Negative)") +
  ylim(-5,5) +
  ggtitle("I hate being Bi-Polar its awesome",
          subtitle = "Moving Average Sentiment of Kanye West's Albums using AFINN Lexicon\nSong Starts in Blue")



#Most common words
song_data %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  dplyr::count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(30,n) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip() +
  ggtitle("I hate being Bi-Polar its awesome",
          subtitle = "Kanye West's 30 Most Used Words") +
  theme_classic() +
  xlab(NULL) +
  ylab(NULL)



uniques <- song_data %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  group_by(album_name, song_name) %>%
  summarise(words = n())

ggplot(uniques, aes(song_name, words)) +
  geom_col() +
  facet_wrap(~album_name, nrow = 2, scales = "free") +
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank()) +
  coord_flip()