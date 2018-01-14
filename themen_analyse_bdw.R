library(tidyverse)
library(ggplot2)
library(utf8)

source("multiplot.R")

artikel_list2 <-  read_csv2("bdw_artikel.csv")

Athemen_list <- group_by(artikel_list2,Bereich, Titelthema)
alle_themen <- summarise(themen_list, count = n()) %>% arrange(desc(count))
titelthemen <- summarise(themen_list, count = n()) %>% filter(Titelthema == TRUE) %>% arrange(desc(count))

ggplot(artikel_list2, aes(Bereich)) + geom_bar() + theme(axis.text.x = element_text(angle = 60)) + scale_y_continuous(labels = function (x) floor(x))

artikel_list2 %>% filter(Titelthema == TRUE) %>% ggplot(aes(Bereich)) + geom_bar() + theme(axis.text.x = element_text(angle = 60)) + scale_y_continuous(labels = function (x) floor(x))


#for all
p1 <- artikel_list2 %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle('Themen BdW') + theme(plot.title = element_text(hjust = 0.5))

p2 <- artikel_list2 %>%
  filter(Titelthema == TRUE) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle('Titel Themen BdW') + theme(plot.title = element_text(hjust = 0.5))

multiplot(p1,p2, cols=1)

# for year 2015
jahrgang <- "2015"
plottitel = paste('Themen BdW ',jahrgang, sep = " ")
plottitelthemen = paste('Themen BdW ',jahrgang, sep = " ")
artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitel) + theme(plot.title = element_text(hjust = 0.5))

artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  filter(Titelthema == TRUE) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitelthemen) + theme(plot.title = element_text(hjust = 0.5))


# for year 2016
jahrgang <- "2016"
plottitel = paste('Themen BdW ',jahrgang, sep = " ")
artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitel) + theme(plot.title = element_text(hjust = 0.5))

plottitelthemen = paste('Themen BdW ',jahrgang, sep = " ")
artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  filter(Titelthema == TRUE) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitelthemen ) + theme(plot.title = element_text(hjust = 0.5))

# for year 2017
jahrgang <- "2017"
plottitel = paste('Themen BdW ',jahrgang, sep = " ")
artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitel) + theme(plot.title = element_text(hjust = 0.5))

plottitelthemen = paste('Themen BdW ',jahrgang, sep = " ")
artikel_list2 %>%
  filter(str_detect(Ausgabe, jahrgang)) %>%
  filter(Titelthema == TRUE) %>%
  count(Bereich) %>%
  mutate(Bereich = fct_reorder(Bereich, n, .desc = TRUE)) %>%
  ggplot(aes(x = Bereich, y = n)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_y_continuous(labels = function (x) floor(x)) + ggtitle(plottitelthemen ) + theme(plot.title = element_text(hjust = 0.5))

