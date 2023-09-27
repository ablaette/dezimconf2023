library(polmineR)
library(data.table)
library(dplyr)

faz <- corpus("NADIRAFAZ")
sz <- corpus("NADIRASZ")
taz <- corpus("NADIRATAZ")

faz_by_years <- lapply(
  1987:2021,
  function(year){
    date_regex <- sprintf("^%s-.*?$", year)
    sc <- faz %>% subset(grepl(!!date_regex, article_date))
    name(sc) <- year
    sc
  }
) %>% 
  as.bundle()

faz <- corpus("GERMAPARL2") %>% 
  subset(speaker_party %in% c("GRUENE"))
faz_by_years <- lapply(
  1987:2021,
  function(year){
    date_regex <- sprintf("^%s-.*?$", year)
    sc <- faz %>%
      subset(grepl(!!date_regex, protocol_date))
    name(sc) <- year
    sc
  }
) %>% 
  as.bundle()


islam_cooc <- cooccurrences(faz_by_years, query = '"[iI]slam.*"', cqp = TRUE, left = 10, right = 10)

m <- islam_cooc %>% 
  subset(ll > 3.84) %>% 
  subset(!word %in% c(punctuation, '"', "'", "*")) %>% 
  lapply(function(x) as.data.table(x)[, "name" := x@name]) %>% 
  rbindlist() %>% 
  select(name, word, rank_ll) %>% 
  dcast(word ~ name, value.var = "rank_ll") %>% 
  as.matrix(rownames = "word")

m[is.na(m)] <- 5000
simil <- cor(m, use = "na.or.complete", method = "spearman")

stability <- data.table(
  year = as.Date(sprintf("%s-01-01", 1988:2021)),
  change = sapply(1990:2021, function(x) simil[as.character(x), as.character(x - 1)])
)

stability %>% as.xts() %>% plot()

change <- data.table(
  year = as.Date(sprintf("%s-01-01", 1988:2021)),
  change = sapply(
    1988:2021,
    function(x)
      proxy::pr_simil2dist(simil)[as.character(x), as.character(x - 1)]
  )
)

change %>% as.xts() %>% plot()

# Dendrogram 
simil %>% 
  proxy::pr_simil2dist() %>% 
  as.dist() %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() %>% 
  plot(horiz = TRUE)
  

# Heatmap
simil %>% 
  reshape2::melt() %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() 