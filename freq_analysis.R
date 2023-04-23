#Importing Libraries
library(tidyverse)
library(ggtext)
library(magrittr)
library(flextable)

#Setting Root Directory and Adding Font
setwd("E:/Charles University/Thesis/masters-thesis-code")
rm(list=ls())
windowsFonts(georgia = windowsFont("georgia"))

#Uploading Data as Tibble
anno_df <- read_csv("data/annotated_df.csv")

#Inspecting the Uploaded Data
names(anno_df)

#Extracting the Distribution of the Parts of Speeches
pos_dis <- anno_df %>%
  count(upos)

#Assigning Full Forms
abbr <- c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X")
full <- c("Adjective", "Adposition", "Adverb", "Auxiliary", "Coordinating Conjunction", "Determiner", "Interjection", "Noun", "Numeral", "Particle", "Pronoun", "Proper Noun", "Punctuation", "Subordinating Conjunction", "Symbol", "Verb", "Other")
tib_full <- tibble(abbr, full)

#Joining Full Forms
pos_dis %<>%
  left_join(tib_full, by = c("upos"="abbr")) %>%
  select(upos,full,n)

#Plotting POS Distribution
pos_dis %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(full,n)), fill = "#262626") +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  labs(x = "Frequency", y = "Parts of Speech") +
  theme_classic() +
  theme(
    axis.text.x = element_text(family = "georgia", angle = 0, hjust = 1),
    axis.text.y = element_text(family = "georgia"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "#0D0D0D"),
    legend.position = "none",
    plot.title = element_textbox(face = "bold", family = "georgia", size = 23, hjust = 0.5),
    axis.title = element_textbox(face = "bold", family = "georgia", size = 11),
    axis.text = element_textbox(size = 13),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#F0F0F2"),
    plot.background = element_rect(colour = "#0D0D0D", size = 1),
  ) +
  ggtitle("Distribution of Parts of Speech")


#Extracting 15 most Frequent Nouns
freq_nouns <- anno_df %>%
  filter(upos=="NOUN")%>%
  count(lemma, sort=TRUE)%>%
  slice(1:15)


#Making Table of Most Frequent Nouns
my_flextable <- flextable(freq_nouns)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.1.1.docx")


#Extracting 15 most Frequent Adjectives
freq_adj <- anno_df %>%
  filter(upos=="ADJ")%>%
  count(lemma, sort=TRUE)%>%
  slice(1:15)


#Making Table of Most Frequent Adjectives
my_flextable <- flextable(freq_adj)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.1.2.docx")


#Extracting 15 most Frequent Verbs
freq_verb <- anno_df %>%
  filter(upos=="VERB")%>%
  count(lemma, sort=TRUE)%>%
  slice(1:15)


#Making Table of Most Frequent Verbs
my_flextable <- flextable(freq_verb)
my_flextable <- set_table_properties(my_flextable, width = 1)
save_as_docx(my_flextable, path = "figures_and_tables/table.1.3.docx")

