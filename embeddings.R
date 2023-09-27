library(Rcpp)
library(RcppCWB)
library(wordVectors) # https://github.com/bmschmidt/wordVectors
library(readr)
library(polmineR)

sourceCpp(file = "~/Lab/github/nadirasz/data-raw/fastdecode.cpp")

junctures <- c(
  "1990-10-03",
  "1993-05-26",
  "2001-09-11",
  "2006-09-27",
  "2010-08-30",
  "2015-09-01",
  "2022-01-01"
) %>% 
  as.Date()

corpus_id <- "NADIRASZ"

for (i in 1L:(length(junctures) - 1)){
  
  from <- junctures[i]
  to <- junctures[i + 1]
  
  txtfile <- sprintf("~/Lab/tmp/embeddings/%s_%s-%s.txt", tolower(corpus_id), from, to) %>%
    path.expand()
  binfile <- sprintf("~/Lab/tmp/embeddings/%s_%s-%s.bin", tolower(corpus_id), from, to) %>%
    path.expand()
  
  strucs <- corpus(corpus_id) %>% 
    subset(as.Date(article_date) >= from) %>% 
    subset(as.Date(article_date) < to) %>% 
    slot("strucs")
  
  write_token_stream2(
    corpus = corpus_id,
    p_attribute = "word", 
    s_attribute = "article_date",
    strucs = strucs,
    attribute_type = "s",
    registry = Sys.getenv("CORPUS_REGISTRY"),
    filename = txtfile
  )
  
  train_word2vec(
    train_file = txtfile,
    output_file = binfile,
    vectors = 200,
    threads = 6,
    window = 12,
    iter = 5,
    negative_samples = 0
  )
  
  beepr::beep(3)
  unlink(txtfile)
}

