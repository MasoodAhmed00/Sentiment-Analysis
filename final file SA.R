# Install required packages (if not already installed)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tm")) install.packages("tm")
if (!require("wordcloud2")) install.packages("wordcloud2")
if (!require("syuzhet")) install.packages("syuzhet")
if(!require(reshape2))install.packages("reshape2",dependencies = T)

library(tidyverse)
library(tm)
library(wordcloud2)
library(syuzhet)
library(reshape2)

#Getting data (select the "twcs"file)
org_data <- read.csv(file.choose(), header = T)

#Select the "sample" file
data <- read.csv(file.choose(),header=TRUE)
################################################
# Calculate the frequency of each unique author_id
author_id_freq <- table(org_data$author_id)

# Convert author_id_freq to data frame for filtering
author_id_df <- as.data.frame(author_id_freq)

# Filter out author_ids with frequency greater than or equal to 25 and are non-numeric
filtered_author_ids <- subset(author_id_df, Var1 != "" & Var1 != "NA" & Var1 != "NULL" & !grepl("^\\d+$", Var1) & Freq >= 30000)

# Create a bar plot
barplot(filtered_author_ids$Freq,
        names.arg = filtered_author_ids$Var1,
        las = 2,
        col = rainbow(nrow(filtered_author_ids)),main = "Companies with frequency of tweets")



# Filter data where author_id is equal to "AdobeCare"
abc_data <- subset(org_data, author_id == "AdobeCare")


# Convert to POSIXct object
abc_data$created_at <- as.POSIXct(abc_data$created_at, format = "%a %b %d %H:%M:%S %z %Y")
# Create a new column for time
abc_data$time <- format(abc_data$created_at, format = "%H:%M:%S")

#To add year and month column to the dataframe , from the publish date column
abc_data$year<-format(abc_data$created_at,"%Y")
abc_data$month<-format(abc_data$created_at,"%m")
abc_data$month <- as.integer(abc_data$month)
# Convert numeric month values to month names
abc_data$month <- month.name[abc_data$month]

#Convert the year column to factor datatype
abc_data$year<-as.factor(abc_data$year)

# Bar plot showing tweet counts for each month, grouped by year
ggplot(abc_data, aes(x = factor(month), fill = factor(year))) +
  geom_bar() +
  labs(x = "Month", y = "Number of Tweets", title = "Tweet Counts by Month and Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~year)


# Build corpus
library(tm)
corpus <- iconv(abc_data$text)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean Text
#During the cleaning phase, the initial step is to transform all the text into lowercase. 
#This conversion can be carried out using the `tm_map` function.
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

# To remove remove Punctuations 
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# To remove numbers from data
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# Removing stopwords
clean_data <- tm_map(corpus, removeWords, stopwords('english'))
inspect(clean_data[1:5])


# To remove URL from data
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
clean_data <- tm_map(clean_data, content_transformer(removeURL))
inspect(clean_data[1:5])


#To remove extra spaces from data
clean_data <- tm_map(clean_data, stripWhitespace)
inspect(clean_data[1:5])

#
tdm <- TermDocumentMatrix(clean_data)
tdm


#
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


# Bar Plot of Word Frequencies that occur more than 1000 times
w <- rowSums(tdm)
w <- subset(w, w>=1000)
barplot(w,
        las = 2,
      col = rainbow(50),main = "Bar Plot of Word Frequencies")


# Install and load the required packages to create wordcloud
if(!require("wordcloud")) install.packages("wordcloud", dependencies = TRUE)
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)


# Wordcloud using wordcloud2 package
if(!require("wordcloud2")) install.packages("wordcloud2", dependencies = TRUE)
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Sentiment Analysis using "TM libraray"
tweets <- iconv(abc_data$text)
s <- get_nrc_sentiment(tweets)
head(s)
#Convert colSums(s) to a data frame
data <- data.frame(word = names(colSums(s)), count = colSums(s))
# Reorder word based on count
data$word <- factor(data$word, levels = data$word[order(-data$count)])
# Create the bar plot using ggplot2
ggplot(data, aes(x = word, y = count)) +
  geom_bar(stat = "identity", fill = rainbow(10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Words", y = "Count", title = "Sentiment Scores Tweets")

# Analyzing sentiments using the syuzhet package based on the NRC sentiment dictionary
text_df <- tibble(text=str_to_lower((abc_data$text)))
emotions <- get_nrc_sentiment(text_df$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))


# Craete bar plot showing counts for each of differnt emotions and Positive/negative rating
ggplot(emo_sum, aes(x= reorder(emotion, -count),y=count,color="black",fill=rainbow(10)))+
  geom_bar(stat="identity",show.legend = FALSE)+
  labs(title="Different emotions with frequency",x="Emotions")
  
#Sentiment analysis with the tidytext package using the "bing" lexicon
library(tidytext)
bing_word_counts <- text_df %>% unnest_tokens(output=word,input=text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment, sort = TRUE)

# Select the top 10 words by sentiment
bing_top_10_words_by_sentiment <- bing_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by=n,n = 10) %>% 
  ungroup() %>% 
  mutate(word= reorder(word, n))
bing_top_10_words_by_sentiment

# Create a bar plot showing contribution of words to sentiment
bing_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill= sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free_y")+
  theme_light()+
  labs(y= "Contribution to sentiment", x=NULL)+
  coord_flip()

#Sentiment analysis with the tidytext package using the "loughran" lexicon
loughran_word_counts <- text_df %>% unnest_tokens(output=word,input=text) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word,sentiment, sort = TRUE)

# Select the top 10 words by sentiment
loughran_top_10_words_by_sentiment <- loughran_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by=n,n = 10) %>% 
  ungroup() %>% 
  mutate(word= reorder(word, n))
loughran_top_10_words_by_sentiment


# Create a bar plot showing contribution of words to sentiment
loughran_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill= sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free_y")+
  theme_light()+
  labs(y= "Contribution to sentiment", x=NULL)+
  coord_flip()

#############################################################################
# Convert created_at to datetime
data$created_at <- as.POSIXct(data$created_at, format = "%a %b %d %H:%M:%S %z %Y")

# Arrange data by created_at
data <- data %>% arrange(created_at)

# Calculate time difference between consecutive tweets in minutes and round off
data$time_difference_minutes <- c(NA, round(diff(data$created_at) / 60))

# Show the result
print(data)

# Load required libraries
library(ggplot2)

# Create a histogram of time differences between consecutive tweets
ggplot(data, aes(x = time_difference_minutes)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Time Differences Between Consecutive Tweets",
       x = "Time Difference (Minutes)",
       y = "Frequency") +
  theme_minimal()


##################################################################
# Load required libraries
library(dplyr)
library(syuzhet)

# Convert created_at to datetime
data$created_at <- as.POSIXct(data$created_at, format = "%a %b %d %H:%M:%S %z %Y")

# Perform sentiment analysis on the text data
sentiment_scores <- get_sentiment(data$text, method = "afinn")

# Merge sentiment scores with the original data
data_with_sentiments <- cbind(data, sentiment_scores)

# Classify sentiment into categories: positive, negative, neutral
data_with_sentiments$sentiment_category <- ifelse(data_with_sentiments$sentiment > 0, "Positive",
                                                  ifelse(data_with_sentiments$sentiment < 0, "Negative", "Neutral"))

# Calculate time difference between tweet and reply
data_with_sentiments <- data_with_sentiments %>%
  mutate(reply_time = as.numeric(difftime(created_at, lag(created_at), units = "mins")))

# Analyze the average reply time based on sentiment category
reply_time_summary <- data_with_sentiments %>%
  filter(!is.na(in_response_to_tweet_id)) %>%
  group_by(sentiment_category) %>%
  summarise(avg_reply_time = mean(reply_time, na.rm = TRUE))

# Convert negative values in avg_reply_time column to positive
reply_time_summary$avg_reply_time <- abs(reply_time_summary$avg_reply_time)

# Print the summary
print(reply_time_summary)
##########################################

# Load required library
library(ggplot2)

# Plot the average reply time based on sentiment category
ggplot(reply_time_summary, aes(x = sentiment_category, y = avg_reply_time)) +
  geom_bar(stat = "identity", fill = c("red","blue","green")) +
  labs(title = "Average Reply Time by Sentiment Category",
       x = "Sentiment Category",
       y = "Average Reply Time (minutes)") +
  theme_minimal()



