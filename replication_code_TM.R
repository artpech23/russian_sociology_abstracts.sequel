## author: Arthur Pecherskikh (apecherskikh@eu.spb.ru)
## code to reproduce the analysis of the editorial board interlocking (EBI) network of Russian sociological journals

################
## libraries: ##
################

library(tidyverse)
library(ggtext)

#######################
## load environment: ##
#######################

load("TM_solution.RData")

## `ctmFit16` - the model of interest (16 topics),
## `ebi`      - the list of journals with their network positions (cluster and core/periphery)
## `ids`      - identifiers for journals' issues (with years)
## `stm_data` - the object for topic modelling. It is created here: https://github.com/artpech23/russian_sociology_abstracts.sequel/blob/main/replication_code_TM_preprocessing.R


###################################
## how the topics were modelled: ##
###################################

## the loop below performs correlated topic modelling (Blei & Lafetty 2007).
## the next code takes a lot of time, so be careful with running it

t <- Sys.time() 

for(i in seq(10, 70, 5)) { ## To check the validity, tou can change it to 16 topics which were selected
  model <- stm(stm_data$documents,
               stm_data$vocab,
               K = i,
               max.em.its = 300,
               data = stm_data$meta,
               init.type = "Spectral",
               seed = 400,
               verbose = F)
  
  assign(paste0("ctmFit", i), model)
  print(str_c("ctmFit", i, " | ", Sys.time()))
  
  Sys.sleep(3) ## may be increased to avoid device heating
  
}

Sys.time() - t


#########################
## models' comparison: ##
#########################

## the quality assessment was done via semantic cohearance and exclusivity
## if you select a fewer number of models to produce above, you can then use this code to vizualise your results

for_plot <- ls() %>% 
  data.frame(object = .) %>% 
  filter(str_detect(object, "ctm"))

comparison_data <- data.frame()

for(i in c(1:nrow(for_plot))){
  
  comparison_data <- comparison_data %>% 
    bind_rows(data.frame(model = for_plot[i,1],
                         exclusivity_score = get(for_plot[i,1],
                                                 envir = globalenv()) %>%
                           exclusivity(),
                         sem.coh._score = get(for_plot[i,1],
                                              envir = globalenv()) %>%
                           semanticCoherence(stm_data$documents)))
  
}

comparison_data %>% 
  ggplot(aes(sem.coh._score, exclusivity_score)) +
  geom_point() +
  facet_wrap(~model) +
  
  labs(x = "семантическая связность<br>(*больше = лучше*)",
       y = "эксклюзивность<br>(*больше = не всегда лучше*)",
       title = "<span style='font-size:12pt'>**Сравнение моделей CTM с разным числом моделируемых тем (*k*)**<br><span style='font-size:10pt'>точки -- темы | *max. EM iterations* = 300") +
  theme(plot.title = element_markdown(),
        strip.text = element_markdown(),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())


#######################################
## topics across years and clusters: ##
#######################################

ctmFit16$theta %>% 
  data.frame() %>%
  `colnames<-`(c("опросы и обследования", "семья и гендер", "педагогика", "демография", "геополитика", "город", "медицина", "философия", "управление", "высшее образование", "соц. теория", "рынок труда", "интернет и медиа", "государство и управление", "молодежь", "регионы")) %>% 
  bind_cols(ids) %>%
  
  
  pivot_longer(cols = c(-journal_name, -journal_issue, -year, -id),
               names_to = "topic",
               values_to = "share") %>% 
  
  ## 
  filter(share > 0.1) %>% 
  
  ## join
  mutate(journal_name = tolower(journal_name)) %>% 
  left_join(ids %>% 
              mutate(journal_name = tolower(journal_name)) %>%
              left_join(ebi %>%
                          select(journal_name, cluster)) %>% 
              select(id, cluster)) %>%
  
  ## frequencies:
  count(year, topic, cluster) %>% 
  
  
  left_join(ids %>%
              count(journal_name, journal_issue, year) %>%
              count(year) %>%
              rename(n_year = n)) %>% 
  
  mutate(cluster_share = n / n_year) %>% 
  mutate(cluster = str_replace_all(cluster, "1", "west")) %>% 
  mutate(cluster = str_replace_all(cluster, "2", "region")) %>% 
  mutate(cluster = str_replace_all(cluster, "3", "RAS + SocIs")) %>% 
  
  
  ggplot(aes(year, cluster_share, color = cluster)) +
  geom_line(stat = "identity", size = 1.01) +
  #scale_color_manual(values = c("purple", "darkgreen", "orange")) +
  scale_color_manual(values = c("#30638E", "coral1", "#988F2A")) +
  facet_wrap(~topic) +
  #theme_minimal() +
  labs(title = "**Динамика тем по кластерам**",
       subtitle = "<i>кластеры: <span style='color:#988F2A;'>западнический<span>, <span style='color:#30638E;'>центральный</span>, <span style='color:coral1;'>восточный</span></i>",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(2010,2015,2020)) +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.text.y = element_text(size = 8))


##########################
## hot and cold topics: ##
##########################


## which topics become significantly less/more popular?


trend_testing <- ctmFit16$theta %>% 
  data.frame() %>%
  `colnames<-`(c("опросы и обследования", "семья и гендер", "педагогика", "демография", "геополитика", "город", "медицина", "философия", "управление", "высшее образование", "соц. теория", "рынок труда", "интернет и медиа", "государство и управление", "молодежь", "регионы")) %>% 
  bind_cols(ids)


## example of the test: 
Kendall::Kendall(trend_testing$year, trend_testing$`опросы и обследования`)


## draw:

ctmFit16$theta %>% 
  data.frame() %>%
  `colnames<-`(c("опросы и обследования", "семья и гендер", "педагогика", "демография", "геополитика", "город", "медицина", "философия", "управление", "высшее образование", "соц. теория", "рынок труда", "интернет и медиа", "государство и управление", "молодежь", "регионы")) %>% 
  bind_cols(ids) %>%
  
  pivot_longer(cols = c(-journal_name, -journal_issue, -year, -id),
               names_to = "topic",
               values_to = "share") %>% 
  
  ## determine whether issue is about the topic
  filter(share > 0.1) %>% 
  
  
  ## frequencies:
  count(year, topic) %>% 
  
  
  left_join(ids %>%
              count(journal_name, journal_issue, year) %>%
              count(year) %>%
              rename(n_year = n)) %>% 
  
  mutate(yearly_share = n / n_year) %>%
  mutate(topic_label = ifelse(year == 2023,
                              topic,
                              NA)) %>% 
  
  filter(!topic %in% c("педагогика", "высшее образование", "рынок труда", "молодежь")) %>% 
  
  mutate(topic_category = ifelse(topic %in% c("геополитика", "философия", "управление", "соц. теория", "государство и управление"),
                                 "«холодные» темы",
                                 "«горячие» темы")) %>% 
  
##############
#### hot: ####
##############

  mutate(topic_label = str_replace_all(topic_label,
                                     "интернет и медиа",
                                     "интернет и медиа\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "опросы и обследования",
                                       "опросы и\nобследования\n\n\n\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "регионы",
                                       "\nрегионы")) %>%
  
  mutate(topic_label = str_replace_all(topic_label,
                                       "город",
                                       "город\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "демография",
                                       "\nдемография")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "семья и гендер",
                                       "\n семья и гендер")) %>%
  
###############
#### cold: ####
###############

  mutate(topic_label = str_replace_all(topic_label,
                                       "философия",
                                       "философия\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "геополитика",
                                       "   геополитика\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "государство и управление",
                                       "\n\n\n\nгосударство и\nуправление")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "соц. теория",
                                       "                    соц. теория\n")) %>%
  mutate(topic_label = str_replace_all(topic_label,
                                       "управление",
                                       "                    управление")) %>%
  
  ggplot(aes(year, yearly_share, color = topic
  )) +
  geom_line(stat = "identity", size = 1.01) +
  facet_wrap(~topic_category) +
  scale_x_continuous(limits = c(2010, 2025.3),
                     breaks = seq(2010, 2024, 2)) +
  labs(
    title = "**«Горячие» и «холодные» темы тем по кластерам**",
    x = "",
    y = "") +
  geom_text(aes(label = topic_label), size = 3) +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_text(size = 20,
                                  color = "black",
                                  family = "Times New Roman")
  )


###################
## session Info: ##
###################

# Description below created via sessionInfo() %>% report::report()

# Analyses were conducted using the R Statistical language (version 4.3.1; R Core Team, 2023) on Windows 10 x64 (build 19045), using the packages

# lubridate (version 1.9.3; Grolemund G, Wickham H, 2011),
# tibble (version 3.2.1; Müller K, Wickham H, 2023),
# ggplot2 (version 3.5.1; Wickham H, 2016),
# forcats (version 1.0.0; Wickham H, 2023),
# stringr (version 1.5.1; Wickham H, 2023),
# tidyverse (version 2.0.0; Wickham H et al., 2019),
# dplyr (version 1.1.2; Wickham H et al., 2023),
# purrr (version 1.0.2; Wickham H, Henry L, 2023),
# readr (version 2.1.5; Wickham H et al., 2024),
# tidyr (version 1.3.1; Wickham H et al., 2024) and 
# ggtext (version 0.1.2; Wilke C, Wiernik B, 2022).
