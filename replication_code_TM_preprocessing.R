## author: Arthur Pecherskikh (apecherskikh@eu.spb.ru)
## code to reproduce the analysis of the editorial board interlocking (EBI) network of Russian sociological journals


################
## libraries: ##
################

library(tidyverse)
## text analysis:
library(stringr)
library(quanteda.textstats)
library(stopwords)
library(ggtext)
library(quanteda)
#library(topicmodels)
#library(ldatuning)
#library(tm)
#library(stm)
## networks:
#library(igraph)
#library(intergraph)
#library(netUtils)


#################
## load files: ##
#################

journals59 <- read.csv("abstracts2010_2023_1.csv") %>% 
  bind_rows(read.csv("abstracts2010_2023_2.csv")) %>% 
  bind_rows(read.csv("abstracts2010_2023_3.csv")) %>% 
  bind_rows(read.csv("abstracts2010_2023_4.csv")) %>% 
  bind_rows(read.csv("abstracts2010_2023_5.csv"))


####################
## preprocessing: ##
####################
#setwd("C:/Users/Arthur Pecherskikh/Desktop/ЕУ/0 - диссертация/темы на 59 журналах + финальный анализ")
journals59 <- journals59 %>% 
  filter(remove_from_analysis == 0) %>% 
  select(-remove_from_analysis) %>% 
  filter(!str_detect(journal_name, "4М")) %>% 
  filter(year != 2024)

####################
## lemmatization: ##
####################

## this is yandex' mystem - see documentation here: https://yandex.ru/dev/mystem/
## the code may take some time, so be careful with running it

## a better approach is to lemmatize smaller corporas of texts as it was done during the data collection.
## I do not attach file with already lemmatized data to not overload the github repo (its size would be large!)

Sys.time()
journals59$abstract_lem = system2("mystem",
                                  c("-d",
                                    "-l",
                                    "-e utf-8",
                                    "-g",
                                    "-c"),
                                  input = journals59$article_abstract,
                                  stdout = TRUE)
Sys.time()

##########################
## collocations search: ##
##########################

abstracts <- journals59 %>% 
  mutate(text_for_coll = str_squish(
    str_remove_all(abstract_lem,
                   "[[:punct:]]|\\{|\\}")))


## the next code takes approzimately 40 mins. on my computer, so be careful with running it

t <- Sys.time()
coll_abstract <- textstat_collocations(abstracts$text_for_coll)
Sys.time() - t

########################
## collocations back: ##
########################

# 1. tokenize initial texts:

abstracts_lem <- abstracts %>% 
  rename(lem_abstract = abstract_lem) %>% 
  mutate(abstract_lem = str_squish(text_for_coll)) %>% 
  unnest_tokens(token, abstract_lem) %>% 
  filter(!token %in% stopwords("ru")) %>% 
  mutate(token = str_replace_all(token, "ё", "е"),
         token_length = nchar(token)) %>% 
  
  filter(token_length > 2) %>%
  select(-token_length)

# 2. frequency lists:

abstracts_lem$potential_coll = ""
abstracts_lem[1:(nrow(abstracts_lem) - 1),17] = abstracts_lem[c(2:nrow(abstracts_lem)),16]


abstracts_lem$potential_coll = paste0(abstracts_lem$token,
                                      " ",
                                      abstracts_lem$potential_coll)


# 3. choose collocations:

collocations_subset50 <- coll_abstract %>%
  select(collocation, count) %>% 
  mutate(collocation2 = collocation) %>% 
  unnest_tokens(collocation2, collocation2) %>% 
  filter(!collocation2 %in% stopwords("ru")) %>% 
  count(collocation, count) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  arrange(desc(count)) %>% 
  
  mutate(collocation2 = collocation) %>% 
  unnest_tokens(collocation2, collocation2) %>% 
  mutate(word_length = nchar(collocation2)) %>% 
  filter(word_length > 2) %>% 
  count(collocation, count) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  arrange(desc(count)) %>% 
  filter(count > 50) %>% 
  #select(-count) %>% 
  mutate(is_coll = 1) ## 4,496

#xlsx::write.xlsx(collocations_subset50, "collocations_subset50_59j.xlsx")


# 4. speech parts to leave only nouns:

collocations_subset50.1 <- collocations_subset50 %>%
  mutate(collocation2 = collocation) %>%
  filter(!str_detect(collocation, "[[:digit:]]|q|w|e|r|t|y|u|i|o|p|a|s|d|f|g|h|j|k|l|z|x|c|v|b|n|m|Q|W|E|R|T|Y|U|I|O|P|A|S|D|F|G|H|J|K|L|Z|X|C|V|B|N|M")) %>% 
  unnest_tokens(token, collocation2)

collocations_subset50.2 <- collocations_subset50.1 %>% 
  count(token) %>% 
  mutate(token_info = NA)

Sys.time()
collocations_subset50.2$token_info = system2("mystem",
                                             c("-d",
                                               "-i ", ### turn speech part detection on
                                               "-l",
                                               "-e utf-8",
                                               "-g",
                                               "-c"),
                                             input = collocations_subset50.2$token,
                                             stdout = TRUE) #%>% 
Sys.time()  ## this is quick


collocations_subset50.3 <- collocations_subset50.2 %>% 
  mutate(speech_part = token_info) %>% 
  separate(speech_part, c("word", "other"), "{") %>% 
  separate(other, c("speech_part", "other"), "\\,") %>% 
  separate(speech_part, c("word2", "speech_part"), "=") %>% 
  
  select(-other, -word2) %>% 
  select(token, speech_part)


collocations_ready <- collocations_subset50.1 %>% 
  left_join(collocations_subset50.3) %>% 
  filter(!speech_part %in% c(NA, "V", "SPRO", "PR", "NUM", "APRO", "ANUM", "ADV")) %>% 
  count(collocation) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  mutate(is_coll = 1)

# 5. put collocations in text:

abstracts_lem2 <- abstracts_lem %>% 
  left_join(collocations_ready %>% 
              rename(potential_coll = collocation),
            by = "potential_coll") %>% 
  mutate(is_coll = ifelse(is.na(is_coll) == T,
                          0,
                          1)) %>% 
  mutate(nrow = c(1:nrow(.)))

collocations_size <- abstracts_lem2 %>% 
  filter(is_coll == 1) %>% 
  
  mutate(diff = c((abstracts_lem2 %>%
                     filter(is_coll == 1))[2:nrow(abstracts_lem2 %>%
                                                    filter(is_coll == 1)),
                                           19], 456968)) %>% 
  mutate(diff = diff - nrow)


abstracts_prepared <- abstracts_lem2 %>% 
  filter(!nrow %in% ((collocations_size %>%
                        filter(diff != 1) %>%
                        mutate(row_to_remove = nrow + 1))$row_to_remove)) %>% 
  
  mutate(final_token = ifelse(is_coll == 1,
                              potential_coll,
                              token)) %>% 
  select(journal_name, journal_issue, article_name, article_link, year, author, author_affiliation, article_abstract, article_doi, initial_sample, final_token)


## 6. final preparation - speech parts and filtering

unique_words <- abstracts_prepared %>% 
  count(final_token) %>% 
  arrange(desc(n)) %>%  ## 82k.
  mutate(is_coll = ifelse(str_detect(final_token, " "),
                          1,
                          0)) ## 3,239 coll.

not_coll <- unique_words %>% 
  filter(is_coll == 0) %>% 
  select(-is_coll)

Sys.time()
not_coll$token_info = system2("mystem",
                              c("-d",
                                "-i ", ### turn speech parts on
                                "-l",
                                "-e utf-8",
                                "-g",
                                "-c"),
                              input = not_coll$final_token,
                              stdout = TRUE) #%>% 
Sys.time() ## 14 sec.

not_coll <- not_coll %>% 
  mutate(speech_part = token_info) %>% 
  separate(speech_part, c("word", "other"), "{") %>% 
  separate(other, c("speech_part", "other"), "\\,") %>% 
  separate(speech_part, c("word2", "speech_part"), "=") %>% 
  
  select(-other, -word2) %>% 
  select(final_token, speech_part)

coll <- unique_words %>% 
  filter(is_coll == 1) %>% 
  select(-is_coll) %>% 
  mutate(final_token2 = final_token) %>% 
  unnest_tokens(separated_tokens, final_token2)

coll$token_info = system2("mystem",
                          c("-d",
                            "-i ",
                            "-l",
                            "-e utf-8",
                            "-g",
                            "-c"),
                          input = coll$separated_tokens,
                          stdout = TRUE)

coll <- coll %>% 
  mutate(speech_part = token_info) %>% 
  separate(speech_part, c("word", "other"), "{") %>% 
  separate(other, c("speech_part", "other"), "\\,") %>% 
  separate(speech_part, c("word2", "speech_part"), "=") %>% 
  
  select(-other, -word2) %>% 
  
  select(final_token, speech_part) %>% 
  group_by(final_token) %>% 
  summarise(speech_part = toString(speech_part)) 

detected_nouns <- coll %>% 
  mutate(remove = ifelse(str_detect(speech_part, "S") != T,
                         1,
                         0)) %>% 
  bind_rows(not_coll %>% 
              mutate(remove = ifelse(speech_part == "S",
                                     0,
                                     1))) %>%
  select(-speech_part)

## new prepared object:

abstracts_prepared2 <- abstracts_prepared %>% 
  left_join(detected_nouns,
            by = "final_token") %>% 
  filter(remove == 0)


## удаляем лишнее:

rm(coll, detected_nouns, not_coll, unique_words)


#################################
## convert data to stm format: ##
#################################

tokens_by_issues <- abstracts_prepared2 %>% 
  count(journal_name, journal_issue, year, final_token)


n_issues <- tokens_by_issues %>% 
  count(journal_issue, journal_name, year) %>% ## 1,868 -- total number of issues
  nrow()

tokens_to_remove <- tokens_by_issues %>% 
  count(final_token) %>% 
  filter(n < (n_issues/100))  ## remove appearance less than in 1% of issues

tokens_to_remove2 <- tokens_by_issues %>% 
  count(final_token) %>% 
  filter(n > 1327) ## 2% from the above distribution


matrix <- tokens_by_issues %>%
  filter(!final_token %in% tokens_to_remove$final_token) %>%
  filter(!final_token %in% tokens_to_remove2$final_token) %>%
  filter(final_token %in% (tokens_by_issues %>%
                             filter(!final_token %in% tokens_to_remove$final_token) %>%
                             filter(!final_token %in% tokens_to_remove2$final_token) %>%
                             count(final_token) %>%
                             filter(n > 180))$final_token) %>% ## > 80 quantile 
  pivot_wider(names_from = "final_token",
              values_from = "n") ## 2,911 x 1,174

## articles' metadata:
ids <- matrix[,1:3] %>% 
  mutate(id = c(1:nrow(matrix)))
matrix <- matrix[,c(-1,-2, -3)]


### remove na:
matrix[is.na(matrix)] <- 0

### create dfm:
dfm_data <- quanteda::as.dfm(matrix)


### create stm:
stm_data <- convert(dfm_data, to = "stm")

##################
## session info ##
##################

##sessionInfo() %>% report::report()

#Analyses were conducted using the R Statistical language (version 4.3.1; R Core Team, 2023) on Windows 10 x64 (build 19045), using the packages:


# stopwords (version 2.3; Benoit K et al., 2021),
# quanteda (version 4.0.2; Benoit K et al., 2018),
# quanteda.textstats (version 0.97; Benoit K et al., 2018),
# lubridate (version 1.9.3; Grolemund G, Wickham H, 2011),
# tibble (version 3.2.1; Müller K, Wickham H, 2023),
# forcats (version 1.0.0; Wickham H, 2023),
# stringr (version 1.5.1; Wickham H, 2023),
# tidyverse (version 2.0.0; Wickham H et al., 2019),
# dplyr (version 1.1.2; Wickham H et al., 2023),
# purrr (version 1.0.2; Wickham H, Henry L, 2023),
# readr (version 2.1.5; Wickham H et al., 2024),
# tidyr (version 1.3.1; Wickham H et al., 2024) and
# ggtext (version 0.1.2; Wilke C, Wiernik B, 2022).


