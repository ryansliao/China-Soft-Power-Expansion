library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)
library(seededlda)

setwd("~/College Assignments/POLI 176 DSC 161/Final Project")
metadata <- read_csv("UNGDspeeches.csv")
metadata

corpus_ungd <- corpus(metadata, text_field = "text")
corpus_ungd

toks <- corpus_ungd %>%tokens()
toks <- tokens(corpus_ungd, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed

usa <- dfm_trimmed[dfm_trimmed$country%in%c("USA") & dfm_trimmed$year> 1991,]
gbr <- dfm_trimmed[dfm_trimmed$country%in%c("GBR") & dfm_trimmed$year> 1991,]
mex <- dfm_trimmed[dfm_trimmed$country%in%c("MEX") & dfm_trimmed$year> 1991,]
chn <- dfm_trimmed[dfm_trimmed$country%in%c("CHN") & dfm_trimmed$year> 1991,]

lda_usa <- textmodel_lda(usa, k = 8)
lda_gbr <- textmodel_lda(gbr, k = 8)
lda_mex <- textmodel_lda(mex, k = 8)
lda_chn <- textmodel_lda(chn, k = 8)

lda_usa.terms <- terms(lda_usa, 10)
lda_gbr.terms <- terms(lda_gbr, 10)
lda_mex.terms <- terms(lda_mex, 10)
lda_chn.terms <- terms(lda_chn, 10)
lda_usa.terms
lda_gbr.terms
lda_mex.terms
lda_chn.terms
