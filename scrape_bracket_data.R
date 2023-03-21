library(rvest)
library(httr)
library(polite)

library(data.table)
library(stringr)

dataList = list()
for(year in c(2002:2019, 2021:2023)){
  url = paste0('https://en.wikipedia.org/wiki/', year, '_NCAA_Division_I_men%27s_basketball_tournament')
  url_bow = polite::bow(url)
  
  caps = polite::scrape(url_bow) %>%  # scrape web page
    rvest::html_nodes("caption") %>%
    rvest::html_text()
  caps = caps[str_detect(caps, "Regional") | str_detect(caps, "Region")]
  
  ind_html <-
    polite::scrape(url_bow) %>%  # scrape web page
    rvest::html_nodes("table.wikitable") %>% # pull out specific table
    rvest::html_table(fill = TRUE)
    
  
  if(year < 2011){
    nteams = 16
  } else{
    nteams = 17
  }
  
  if(length(caps) == 0){
    regNames = lapply(ind_html, names)
    ind_html = lapply(ind_html, function(x){setNames(x, make.unique(unlist(x[1,])))})
    regNames = word(unlist(lapply(regNames[unlist(lapply(lapply(ind_html, names), function(x){any(x=="Seed")}))], function(x){x[1]})), 1, 1)
    ind_html = ind_html[unlist(lapply(lapply(ind_html, names), function(x){any(x=="Seed")}))]
    if(sum(sapply(ind_html, is.null)) > 0){
      ind_html = ind_html[-which(sapply(ind_html, is.null))]
    }
    ind_html = lapply(ind_html, function(x){x%>%slice(-1L)})
    names(ind_html) = regNames
  } else{
    ind_html = ind_html[unlist(lapply(lapply(ind_html, names), function(x){any(x=="Seed") & any(x=="Berth type")}))]
    names(ind_html) = word(caps, 1, 1)
  }
  
  dataList[[as.character(year)]] = rbindlist(ind_html, idcol = "Regional")
}

bracketData = rbindlist(dataList, idcol="Season", fill=T) %>%
  mutate(BerthType = ifelse(is.na(`Berth Type`), `Berth type`, `Berth Type`),
         OverallSeed = ifelse(!is.na(`Overall rank`), `Overall rank`, 
                              ifelse(!is.na(`Overall Seed`), `Overall Seed`, `Overall rank[4]`))) %>%
  select(-c(`Berth type`, `Berth Type`, `Overall rank`, `Overall Seed`, `Overall rank[4]`)) %>%
  mutate(Season = as.numeric(Season),
         Seed = as.numeric(str_replace_all(Seed, '[#\\*]', "")),
         OverallSeed = as.numeric(OverallSeed)) %>%
  mutate(School = ifelse(str_ends(School, "State") & str_starts(School, "NC", negate=T),
                         str_replace(School, "State", "St."),
                         School))
