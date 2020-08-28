Untitled
================

### Load Libraries

``` r
library(tidyverse)
library(tidytext)
library(SnowballC)
library(wordcloud)
library(tidyquant)
```

### Load Data

#### Reviews of Restaurants

``` r
review_data <- read_tsv('Restaurant_Reviews.tsv',quote = '')

# glimpse of the data

glimpse(review_data)
```

    ## Observations: 1,000
    ## Variables: 2
    ## $ Review <chr> "Wow... Loved this place.", "Crust is not good.", "Not ...
    ## $ Liked  <int> 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0...

Review column is the review given by customers and Liked column represents whether the customer liked or did not liked the service (1 is liked and 0 is did not liked).

``` r
# look at few reviews

head(review_data) %>% 
  knitr::kable()
```

| Review                                                                                  |  Liked|
|:----------------------------------------------------------------------------------------|------:|
| Wow... Loved this place.                                                                |      1|
| Crust is not good.                                                                      |      0|
| Not tasty and the texture was just nasty.                                               |      0|
| Stopped by during the late May bank holiday off Rick Steve recommendation and loved it. |      1|
| The selection on the menu was great and so were the prices.                             |      1|
| Now I am getting angry and I want my damn pho.                                          |      0|

### Sentiment Analysis

``` r
# getting the sentiments of the reviews

review_sentiment <- review_data %>% 
  
  # add the customer id
  mutate(customer_id = row_number()) %>% 
  unnest_tokens(output = word, input = Review) %>% 
  
  # reaarange the columns
  select(customer_id,everything()) %>% 
  
  # remove the stop words
  anti_join(get_stopwords(), by = 'word') %>% 
  
  # convert the words to their stem
  mutate(word = wordStem(word, language = 'english')) %>% 
  
  # get the sentiments of the words - either positve or negative
  inner_join(get_sentiments('bing'), by  = 'word')
  

head(review_sentiment, 10) %>% 
  knitr::kable()
```

|  customer\_id|  Liked| word      | sentiment |
|-------------:|------:|:----------|:----------|
|             1|      1| wow       | positive  |
|             1|      1| love      | positive  |
|             2|      0| good      | positive  |
|             4|      1| recommend | positive  |
|             4|      1| love      | positive  |
|             5|      1| great     | positive  |
|             6|      0| damn      | negative  |
|             7|      0| fresh     | positive  |
|             8|      0| like      | positive  |
|             8|      0| warmer    | positive  |

#### Top words used in the reviews

``` r
# top words used in the reviews

review_sentiment %>% 
  count(word, sort = TRUE) %>% 
  top_n(30) %>% 
  ggplot(aes(fct_reorder(word,n),n))+
  geom_col(aes(fill = word), show.legend = FALSE)+
  coord_flip()+
  labs(x = '', y = 'count')+
  theme_tq()
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### Top words used in the reviews for Liked and Not Liked

``` r
 review_sentiment %>% 
  mutate(Liked = as.factor(Liked)) %>% 
  group_by(Liked) %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  ggplot(aes(fct_reorder(word,n), n,fill = Liked))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  facet_wrap(~ Liked, scales = 'free')+
  labs(title = 'Top words for Liked and Not Liked Services',
       x = '', y = 'count')+
  theme_tq()+
  theme(legend.position = 'none')
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

The words good, like and best are showing up high for the Not Liked reviews, which means those words were probably used with other words to describe the service. So, we may have to look for combination of words rather than single words.

#### Top words that contributed to positive and negative sentiments of the reviews

``` r
# top positive and negative words used for the review

review_sentiment %>% 
  count(word,sentiment, sort = T) %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(word,n), y = n, fill = word))+
  geom_col(show.legend = FALSE) +
  coord_flip()+
  facet_wrap(~ sentiment, scales = 'free_y')+
  labs(y = 'count', x = '')+
  theme_tq()+
  theme(legend.position = 'none')
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### Wordcloud of the words used in the reviews

``` r
word_count <- review_sentiment %>% 
  count(word,sentiment) 

wordcloud(words = word_count$word, freq = word_count$n,max.words = 200,
         colors = brewer.pal(8,'Dark2'),fixed.asp = T, random.order = F
          )
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

#### Comparison cloud of the words representing positive and negative sentiments.

``` r
review_sentiment %>% 
  count(word,sentiment, sort = T) %>% 
  spread(key = sentiment, value = n, fill = 0) %>% 
  column_to_rownames('word') %>% 
  as.matrix() %>% 
  comparison.cloud(max.words = 100,
                   title.colors = c('red','green'),
                   title.size = 2,
                   match.colors = F,colors = c('red','royalblue'))
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-9-1.png)

### Bigram Analysis

#### Creating Bigrams

``` r
bigram_review <- review_data %>% 
  mutate(customer_id = row_number()) %>% 
  select(customer_id, everything()) %>% 
  unnest_tokens(input = Review, output = bigram, token = 'ngrams', n = 2) %>% 
  separate(bigram, into = c('word1','word2')) %>%
  
  #remove stop words
  filter(!word1 %in% get_stopwords(source = 'smart')$word,
         !word2 %in% get_stopwords(source = 'smart')$word) %>% 
  unite(bigram, word1,word2, sep = ' ') %>% 
  count(Liked,bigram, sort = T)

head(bigram_review,10) %>% 
  knitr::kable()
```

|  Liked| bigram           |    n|
|------:|:-----------------|----:|
|      1| great food       |    7|
|      1| great place      |    6|
|      1| 5 stars          |    5|
|      1| good food        |    5|
|      1| great service    |    5|
|      0| 10 minutes       |    4|
|      0| customer service |    4|
|      1| good service     |    4|
|      0| 20 minutes       |    3|
|      0| 30 minutes       |    3|

``` r
bigram_review %>% 
  mutate(Liked = as.factor(Liked)) %>% 
  group_by(Liked) %>% 
  slice(1:20) %>% 
  ungroup() %>% 
  ggplot(aes(fct_reorder(bigram,n), n))+
  geom_col(aes(fill = Liked), show.legend = F)+
  coord_flip()+
  facet_wrap(~ Liked, scales = 'free')+
  theme_tq()+
  labs(title = 'Top Bigrams of Liked and Not Liked Reviews',
       x = '', y = 'count')
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-11-1.png)

#### Cpmaprison Cloud of the Bigrams

``` r
bigram_review %>% 
  mutate(Liked = ifelse(Liked == 1, 'Liked','Not Liked')) %>% 
  spread(Liked, n, fill = 0) %>% 
  column_to_rownames('bigram') %>% 
  as.matrix() %>% 
  comparison.cloud(max.words = 50, scale = c(2,0.3),
                   title.size = 1)
```

![](Text_analysis_files/figure-markdown_github/unnamed-chunk-12-1.png)
