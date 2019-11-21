# Quand on appelle un objet en reactive il vaut mieux, si on source() les fonctions, de mettre ces objets en paramètre pour les appeller w/ () 

server = function(input, output){
  # Fonction setup ----
  acc_id = reactive({
    if (input$sum_name != ""){
      return(get_encryp(input$sum_name))
    } else {
      return(NULL)
    }
  })
  
  output$crypto = renderPrint({
    if (input$sum_name != ""){
      paste(input$sum_name, acc_id(), sep=" : ")
    } else {
      return(NULL)
    }
  })
  
  match = reactive({
    return(get_match(acc_id()))
  })
  
  champ_id = reactive({
    return(get_champ_id())
  })
  
  match_type = reactive({
    return(get_match_type())})
  
  # score = reactive({
  #   acc_id = acc_id()
  #   match  = match()
  #   temp   = match$gameId %>% as.data.frame()
  #   
  #   i = 1
  #   for(j in c(20,40,50)){
  #     temp[i:j, 2] = unlist(lapply(temp[i:j, 1], get_match_info, acc_id = acc_id))
  #     Sys.sleep(1)
  #     i = j+1
  #   }
  #   colnames(temp) = c("gameId", "score")
  #   return(temp)
  # })
  
  score = reactive({
    return(get_score(acc_id(), match()))
  })
  
  # match_df = reactive({
  #   match_df = left_join(match(), champ_id(), by = c("champion" = "key"))
  #   match_df = left_join(match_df, match_type(), by = c("queue" = "queueId"))
  #   match_df = left_join(match_df, score(), by = "gameId")
  #   
  #   match_df$type = str_remove_all(match_df$description, "games|5v5")
  #   match_df$player = substring(match_df$description, 1,3)
  #   
  #   match_df = subset(match_df, select = -c(champion, queue, description))
  #   return(match_df)
  # })
  
  match_df = reactive({
    return(join_all_df(match(), champ_id(), match_type(), score()))
  })

  
  # Création de resultats pour l'ui ----
  output$table = renderTable({
    if (input$sum_name != ""){
      match_df()
    } else {
      return(NULL)
    }
  })
}