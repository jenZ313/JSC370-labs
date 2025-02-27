Lab 08 - Text Mining/NLP
================

# Learning goals

- Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
  ngrams from text
- Use dplyr and ggplot2 to analyze and visualize text data
- Try a theme model using `topicmodels`

# Lab description

For this lab we will be working with the medical record transcriptions
from <https://www.mtsamples.com/> available at
<https://github.com/JSC370/JSC370-2025/tree/main/data/medical_transcriptions>.

# Deliverables

1.  Questions 1-7 answered, knit to pdf or html output uploaded to
    Quercus.

2.  Render the Rmarkdown document using `github_document` and add it to
    your github site. Add link to github site in your html.

### Setup packages

You should load in `tidyverse`, (or `data.table`), `tidytext`,
`wordcloud2`, `tm`, and `topicmodels`.

## Read in the Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
library(tidytext)
library(tidyverse)
library(wordcloud2)
library(tm)
library(topicmodels)

mt_samples <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/medical_transcriptions/mtsamples.csv")
mt_samples <- mt_samples |>
  select(description, medical_specialty, transcription)

head(mt_samples)
```

------------------------------------------------------------------------

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
medical specialties are in the data. Are these categories related?
overlapping? evenly distributed? Make a bar plot.

``` r
mt_samples |>
  count(medical_specialty, sort = TRUE) |>
  ggplot(aes(fct_reorder(medical_specialty, n), n)) + 
  geom_col(fill="plum4") + 
  coord_flip() +
  theme_minimal()
```

They are 30 specialties, not related and not evenly distributed.

------------------------------------------------------------------------

## Question 2: Tokenize

- Tokenize the the words in the `transcription` column
- Count the number of times each token appears
- Visualize the top 20 most frequent words with a bar plot
- Create a word cloud of the top 20 most frequent words

### Explain what we see from this result. Does it makes sense? What insights (if any) do we get?

``` r
tokens <- mt_samples |>
  select(transcription) |>
  unnest_tokens(word, transcription) |>
  group_by(word) |>
  summarize(word_frequncy=n()) |>
  arrange(across(word_frequncy, desc)) |>
  head(20)



tokens |> 
  ggplot(aes(fct_reorder(word,word_frequncy), word_frequncy)) + 
  geom_bar(stat="identity", fill="plum4") +
  coord_flip() +
  theme_minimal()

tokens |>
  count(word, sort=TRUE) |>
  wordcloud2(size=0.5, color="random-light")
```

The result does make sense but is not very helpful since it contains
mostly meaningless words like “the” and “on”, the only useful word here
is “patient”.

------------------------------------------------------------------------

## Question 3: Stopwords

- Redo Question 2 but remove stopwords
- Check `stopwords()` library and `stop_words` in `tidytext`
- Use regex to remove numbers as well
- Try customizing your stopwords list to include 3-4 additional words
  that do not appear informative

### What do we see when you remove stopwords and then when you filter further? Does it give us a better idea of what the text is about?

``` r
head(stopwords("english"))
length(stopwords("english"))
head(stop_words)

tokens <- mt_samples |>
  select(transcription) |>
  unnest_tokens(word, transcription) |>
  group_by(word) |>
  summarize(word_frequncy=n()) |>
  arrange(across(word_frequncy, desc)) |>
  filter(!word %in% stopwords("english"), !grepl('^[0-9]|also|used|using|noted',word, ignore.case = TRUE)) |>
  head(20)


tokens |> 
  ggplot(aes(fct_reorder(word,word_frequncy), word_frequncy)) + 
  geom_bar(stat="identity", fill="plum4") +
  coord_flip() +
  theme_minimal()

tokens |>
  count(word, sort=TRUE) |>
  wordcloud2(size=0.4, color="random-light")
```

The result is much better without stopwords, it shows meaningful words
like “skin” and “anesthesia” that are not shown in the previous part. It
still contains words like “also” and “used”, which I further filtered.

------------------------------------------------------------------------

## Question 4: ngrams

Repeat question 2, but this time tokenize into bi-grams. How does the
result change if you look at tri-grams? Note we need to remove stopwords
a little differently. You don’t need to recreate the wordclouds.

``` r
sw_start <- paste0("^", paste(stopwords("english"), collapse=" |^"), "$")
sw_end <- paste0("", paste(stopwords("english"), collapse="$| "), "$")

tokens_bigram <- mt_samples |>
  select(transcription) |>
  unnest_tokens(ngram, transcription, token = "ngrams", n = 2) |>
  filter(!grepl(sw_start, ngram, ignore.case = TRUE)) |>
  filter(!grepl(sw_end, ngram, ignore.case = TRUE)) |>
  filter(!grepl('[[:digit:]]+', ngram, ignore.case = TRUE)) |>
  group_by(ngram) |>
  summarize(word_frequncy=n()) |>
  arrange(across(word_frequncy, desc)) |>
  head(20)

tokens_bigram |> 
  ggplot(aes(ngram, word_frequncy)) + 
  geom_bar(stat="identity", fill="plum4") +
  coord_flip() +
  theme_minimal()

tokens_bigram |>
  count(ngram, sort=TRUE) |>
  wordcloud2(size=0.2, color="random-light")
```

------------------------------------------------------------------------

## Question 5: Examining words

Using the results from the bigram, pick a word and count the words that
appear before and after it, and create a plot of the top 20.

``` r
library(stringr)
tokens_bigram |>
  filter(str_detect(ngram, regex("\\spatients$|^patient\\s"))) |> 
  mutate(word = str_remove(ngram, "patient"),
         word = str_remove_all(word, " ")) |> 
  group_by(word) |>
  summarise(word_frequncy = n()) 
```

## Since the bigram is limited to top 20, there is only 1 bigram related to “patient”.

## Question 6: Words by Specialties

Which words are most used in each of the specialties? You can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the 5
most used words?

``` r
mt_samples |>
   unnest_tokens(word, transcription) |>
   filter(!word %in% stopwords("english"), 
          !grepl("\\d+", word, ignore.case = TRUE)) |>
   group_by(medical_specialty, word) |>
   summarise(word_frequncy = n(), .groups = "drop") |>
   slice_max(order_by = word_frequncy, n = 5)
```

For “Surgery”, the top 5 are patient, left, right, procedure and placed.

## Question 7: Topic Models

See if there are any themes in the data by using a topic model (LDA).

- you first need to create a document term matrix
- then you can try the LDA function in `topicmodels`. Try different k
  values.
- create a facet plot of the results from the LDA (see code from
  lecture)

``` r
transcripts_dtm <- mt_samples |>
  select(transcription) |>
  unnest_tokens(word, transcription) |>
  filter(!word %in% stopwords("english"), !grepl('^[0-9]|also|used|using|noted',word, ignore.case = TRUE)) |>
  DocumentTermMatrix()


transcripts_dtm <- as.matrix(transcripts_dtm)   

transcripts_lda <- LDA(transcripts_dtm, k=5,
                       control=list(seed=1234))


transcripts_top_terms <-
  tidy(transcripts_lda, matrix="beta") |>
  filter(!str_detect(term, "^[0-9]+$")) |>
  group_by(topic) |>
  slice_max(beta,n=10) |>
  ungroup()|>
  arrange(topic,beta)
transcripts_top_terms|>
  mutate(term=reorder_within(term,beta,topic))|>
  ggplot(aes(beta,term,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic,scales="free")+
  scale_y_reordered()+
  theme_minimal()



transcripts_lda <- LDA(transcripts_dtm, k=3,
                       control=list(seed=1234))


transcripts_top_terms <-
  tidy(transcripts_lda, matrix="beta") |>
  filter(!str_detect(term, "^[0-9]+$")) |>
  group_by(topic) |>
  slice_max(beta,n=10) |>
  ungroup()|>
  arrange(topic,beta)
transcripts_top_terms|>
  mutate(term=reorder_within(term,beta,topic))|>
  ggplot(aes(beta,term,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic,scales="free")+
  scale_y_reordered()+
  theme_minimal()
```
