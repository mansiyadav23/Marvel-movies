library(twitteR)
library(tm)
library(dplyr)
library(tidytext)
library(textdata)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(textreadr)
library(janeaustenr)
library(pdftools)

###### setting the working directory and uploading the files
setwd("/Users/mansiyadav/Desktop/Hult/Dual Degree/Dual degree Mod B/Text Analytics/Individual assignment/assignment_1")
my_file_2 <- list.files(path="/Users/mansiyadav/Desktop/Hult/Dual Degree/Dual degree Mod B/Text Analytics/Individual assignment/assignment_1")

###### Putting the files in a dataframe
my_file_text_2 <- as.data.frame(do.call(rbind, lapply(my_file_2, function(x) pdf_text(x))))
# View(my_file_text_2)

####### transposing the dataframe
my_file_movies_2 <- as.data.frame(t(as.matrix(my_file_text_2)))
# View(my_file_movies_2)

####### Separating the questions
cap_america <- my_file_movies_2$V1
iron_man <- my_file_movies_2$V2
thor <- my_file_movies_2$V3

####### Creating a dataframe for cap america
mydf_cap_america <- data_frame(line=1:168, text=cap_america)
#print(mydf_cap_america)

####### Creating a dataframe for iron man
mydf_iron_man <- data_frame(line=1:168, text=iron_man)
#print(mydf_iron_man)

####### Creating a dataframe for thor
mydf_thor <- data_frame(line=1:168, text=thor)
#print(mydf_thor)

########################################################################################################################################################################################################################################################################################################################################################################################################################################

# making my custom stop words
my_cust_stop <- data_frame( #Creating a new stop words dictionary
  word = c("uh","um","isn","yeah","col","don't","i'm","gonna","26","03",
           "4th","cont'd","ext","dr","sir","hey","time","lot","10","nice",
           "call","blue","wait","it's","int"),
  lexicon= rep("custom",each=25)
)
### view my custom stop words
#my_cust_stop

##### Tokenizing and removing stop words for captain america movie dataframe
data(stop_words)



###### Tokenizing and removing stop words for cap america movie dataframe
frequencies_tokens_m1 <- mydf_cap_america %>% #movie 1 dataframe
  mutate_if(is.factor, as.character) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(my_cust_stop) %>%
  count(word, sort=TRUE)
frequencies_tokens_m1

###### Tokenizing and removing stop words for iron man movie dataframe
frequencies_tokens_m2 <- mydf_iron_man %>% #movie 2 dataframe
  mutate_if(is.factor, as.character) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(my_cust_stop) %>%
  count(word, sort=TRUE)
frequencies_tokens_m2

###### Tokenizing and removing stop words for thor movie dataframe
frequencies_tokens_m3 <- mydf_thor %>% #movie 3 dataframe
  mutate_if(is.factor, as.character) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(my_cust_stop) %>%
  count(word, sort=TRUE)
frequencies_tokens_m3

########################################################################################################################################################################################################################################################################################################################################################################################################################################



###### Plotting token frequency for THE MOVIES

library(ggplot2)

#### Frequency histogram for cap america
freq_hist_m1 <- frequencies_tokens_m1 %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 400) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_m1)

#### Frequency histogram for iron man
freq_hist_m2 <- frequencies_tokens_m2 %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 45) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_m2)

#### Frequency histogram for thor
freq_hist_m3 <- frequencies_tokens_m3 %>%
  mutate(word = reorder(word, n)) %>%
  filter(n > 130) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_m3)

###################################################################################################################################################################################################################################################################################################################################################################################

####### Creating a location for the data frame
combined_movies <- bind_rows(
  mutate(frequencies_tokens_m1, movie = 'Captain America'),
  mutate(frequencies_tokens_m2, movie = 'Iron Man'),
  mutate(frequencies_tokens_m3, movie = 'Thor')
)


movie_words <- combined_movies %>%
  bind_tf_idf(word, movie, n)

movie_words 

movie_words %>%
  arrange(desc(tf_idf))


##### TF-IDF
movie_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(6) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()



#############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#### get sentiments
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

#### Word Clouds    

#### Word Cloud for cap america   
frequencies_tokens_m1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("black", "black"),
                   max.words=100)

#### Word Cloud for iron man   
frequencies_tokens_m2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("black", "black"),
                   max.words=100)

#### Word Cloud for thor
frequencies_tokens_m3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("black", "black"),
                   max.words=100)




###################################################################################################################################################################################################################################################################################################################################################################################

##### Word Pie            
library(reshape2)

##### Word Pie for cap america
frequencies_tokens_m1 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale=c(0.7,0.7), 
                   fixed.asp = TRUE,
                   title.size = 0.9
  )

##### Word Pie for iron man
frequencies_tokens_m2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale=c(0.7,0.7), 
                   fixed.asp = TRUE,
                   title.size = 0.9
  )

##### Word Pie for thor
frequencies_tokens_m3 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, 
                   scale=c(0.7,0.7), 
                   fixed.asp = TRUE,
                   title.size = 0.9
  )

#############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################














