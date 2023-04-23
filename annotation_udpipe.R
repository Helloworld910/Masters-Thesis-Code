#Importing Libraries
library(tidyverse)
library(udpipe)
library(magrittr)

#Setting Root Directory
setwd("E:/Charles University/Thesis/masters-thesis-code")
rm(list=ls())

#Uploading Data as Tibble
root_df <- read_csv("data/merged_df.csv")

#Inspecting Uploaded Data
names(root_df)

#Loading Udpipe Model
ud_model <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')


#Extracting Corpus from Data
text_corpus <- root_df %>%
  mutate(doc_id = row_number())%>%
  select(doc_id,text)


#Annotating with Udpipe
x <- udpipe_annotate(ud_model, x = text_corpus$text, doc_id = text_corpus$doc_id)
x <- as.data.frame(x)
x <- as_tibble(x)

#Saving Annotated Tibble
write_csv(x, "data/annotated_df.csv", col_names = TRUE, na = "NA")