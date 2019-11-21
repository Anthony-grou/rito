## TODO : filtrer les matchs par type avec des cases à cocher dans l'appli, cela se traduira par des switchs dans les widgets

## NOTE : Chaque fonction envoie une requête à l'API, ne pas oublier de mettre ces appels de fonction en reactive ##
##        ET DE METTRE ENTRE () !!!                                                                               ##

options(stringsAsFactors = FALSE) 

start = Sys.time()

library(httr)
library(jsonlite)
library(lubridate)
library(plyr)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)

api_key = "RGAPI-3df78f29-adf2-4ffe-a288-3d9400d93427"
url = "https://euw1.api.riotgames.com/lol"


### La fonction permet d'extraire le accountId en utilisant le pseudo IG  ###
get_encryp = function(summonerName){
  path = paste("/summoner/v4/summoners/by-name/", summonerName, sep="")
  
  res  = GET(url = paste(url,
                         path,
                         paste("?api_key=", api_key, sep=""),
                         sep=""))
  
  if (res$status_code == 200){
    return((res$content %>% rawToChar() %>% fromJSON())$accountId)
    
  } else {
    print("Erreur, le pseudo ne marche pas")
  }
}


## Permet d'extraire la liste des games en utilisant l'accountId
get_match = function(acc_id){
  path = paste("/match/v4/matchlists/by-account/", acc_id, sep="")
  res = GET(url = paste(url,
                        path,
                        paste("?api_key=", api_key, sep=""),
                        sep=""))
  
  return((res$content %>% rawToChar() %>% fromJSON())$matches[, c("gameId", "champion", "queue", "lane")] %>% head(50))
}


## Permet d'obtenir des infos diverses sur les parties (dans un premier temps juste win/loose)
get_match_info = function(match_id, acc_id){
  path = paste("/match/v4/matches/", match_id, sep="")
  res = GET(url = paste(url,
                        path,
                        paste("?api_key=", api_key, sep=""),
                        sep=""))
  
  temp = res$content %>% rawToChar() %>% fromJSON()
  
  nb = data.frame(temp$participantIdentities$participantId, 
                    temp$participantIdentities$player$accountId)
  colnames(nb) = c("player_id", "account")
  nb = nb[nb$account == acc_id, "player_id"]
  
  info = data.frame(temp$participants$stats$participantId,
                    temp$participants$stats$win)
  colnames(info) = c("player_id", "score")
  info = info[info$player_id == nb, "score"]
  if (info == TRUE){
    return("WIN")
  } else {
    return("LOOSE")
  }
}


## Permet d'obtenir la liste des champions et leur id
get_champ_id = function(){
  champ = GET(url = "http://ddragon.leagueoflegends.com/cdn/9.22.1/data/en_US/champion.json")
  champ = champ$content %>% rawToChar() %>% fromJSON()
  
  champ = do.call(what = "rbind",
                  args = lapply(champ$data, as.data.frame))
  
  rownames(champ) = c()
  champ = champ[, c("name", "key")] %>% unique()
  champ$key = champ$key %>% as.numeric()
  return(champ)
}


## Permet d'obtenir la table de correspondance avec les match et leur type grâce à leur queueId
get_match_type = function(){
  match_type = GET(url = "http://static.developer.riotgames.com/docs/lol/queues.json")
  return((match_type$content %>% rawToChar() %>% fromJSON())[, c("queueId", "map", "description")])
}


## Permet d'obetnir le score final pour chaque game d'un vecteur de gameId (win/loose)
get_score = function(acc_id, match){
  temp = match$gameId %>% as.data.frame()
  i = 1
  for (j in c(20, 40, 50)){
    temp[i:j, 2] = unlist(lapply(temp[i:j, 1], get_match_info, acc_id = acc_id))
    Sys.sleep(time = 1)
    i = j+1
  }
  colnames(temp) = c("gameId", "score")
  return(temp)
}


## Permet de joindre les tables requetees pour simplement l'inserer dans le script Shiny
join_all_df = function(match, champ_id, match_type, score){
  match_df = left_join(match, champ_id, by = c("champion" = "key"))
  match_df = left_join(match_df, match_type, by = c("queue" = "queueId"))
  match_df = left_join(match_df, score, by = "gameId")

  match_df$type = str_remove_all(match_df$description, "games|5v5")
  match_df$player = substring(match_df$description, 1,3)

  match_df = subset(match_df, select = -c(champion, queue, description))
  return(match_df)
}




# Test ----
# start = Sys.time()
# 
# acc_id     = get_encryp("Grodzzilla")
# match      = get_match(acc_id)
# champ_id   = get_champ_id()
# match_type = get_match_type()
# score      = get_score(acc_id)
# df_total    = join_all_df()
# 
# Sys.time() - start
