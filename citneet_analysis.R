#Importing Libraries
library(tidyverse)
library(udpipe)
library(ggtext)
library(magrittr)
library(igraph)
library(ggraph)
library(flextable)

#Setting Root Directory, Adding Font
setwd("E:/Charles University/Thesis/masters-thesis-code")
rm(list=ls())
windowsFonts(georgia = windowsFont("georgia"))

#Upload Data
papers_data <- read_csv("data/merged_df.csv")

#Inspecting Data
names(papers_data)

#DOIs
papers_data %>%
  select(doi)

#

