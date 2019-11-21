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
  
  score = reactive({
    return(get_score(acc_id(), match()))
  })
  
  match_df = reactive({
    return(join_all_df(match(), champ_id(), match_type(), score()))
  })
  
  
  # Création de resultats pour l'ui ----
  output$table = DT::renderDataTable(DT::datatable({
    if (input$sum_name != "") return(match_df())
  }))
  
  output$pie = renderPlotly({
    if (input$sum_name != "") return(get_pie(match_df()))
  })

  output$ratio = renderPlotly({
    if (input$sum_name != "") return(get_ratio(match_df()))
  })
}