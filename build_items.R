build_items <- function(){
  
  tabItem(
    tabName = "home",
    tags$div(
      tabName = "home",
      tags$div(
        h1("Sardinia Smart Tourism"),
        tags$div(
          h4("Analisi del settore turistico sardo nel periodo Settembre 2015 - Settembre 2016"),
          br(),
          tags$img(src="home1.png", class="img-circle", size = "1000x800"),
          class="well"
        )
      ),
      class="jumbotron text-center"
    )
  )
  
  
  
}