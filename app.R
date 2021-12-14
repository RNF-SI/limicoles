#Chargement des packages et installation des packages manquants (pour une utilisation en local, pas sur le serveur)
list.of.packages <- c("shiny", "RPostgreSQL", "tidyr", "tidyverse", "lubridate", "ggplot2", "DT", "shinyWidgets", "shinydashboard", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
as.vector(new.packages)
if(length(new.packages)) install.packages(new.packages)
for (pkg in list.of.packages) {
  library(pkg, character.only = TRUE)
}

# récupération des variables dans le fichier config (mdp notamment)
source("config.R")

# chargement du driver PostgreSQL
drv <- dbDriver("PostgreSQL")

# creation de la connection à la base de données
con <- dbConnect(drv, dbname = "geonature2db",
                 host = "212.83.185.127", port = 5432,
                 user = "agallois", password = pw)

# fonction de vérification d'update dans la base de données
# actuellement basé sur les visites, mais à mettre à jour sur les observations dès que possible côté GeoNature(champs meta__date sur t_base_observations)
check_for_update <- function() {
  dbGetQuery(con,"select max(GREATEST(meta_update_date, meta_create_date)) from gn_monitoring.t_base_visits")
}

# fonction d'import des données avec correction de l'encodage
get_data <- function() {
  limicoles <- dbGetQuery(con,"select * from gn_monitoring.v_limicoles")
  cols <- names(limicoles)
  
  for(i in seq_along(cols)){
    
    if(!is.character(limicoles[, cols[[i]]])) next
    
    Encoding(limicoles[, cols[[i]]]) <- "UTF-8"
    
  }
  
  limicoles$site <- factor(limicoles$site)
  limicoles$site_fonctionnel_nom <- factor(limicoles$site_fonctionnel_nom)
  limicoles$cycle <- factor(limicoles$cycle)
  return(limicoles)
}

#appel initial des données (nécessaire pour l'UI)
limicoles <- get_data()

# fonction de fermeture de connexion postgres
close_connection <- function() {
  dbDisconnect(con)
  dbUnloadDriver(drv)
}

# Definition de l'UI (interface utilisateur)
ui <- dashboardPage(
  
  dashboardHeader(title = "Limicoles Côtiers RNF"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse globale", 
               tabName = "dashboard", 
               icon = icon("dashboard"),
               pickerInput(
                 inputId = "selection_SF", 
                 choices = levels(limicoles$site_fonctionnel_nom),
                 selected = levels(limicoles$site_fonctionnel_nom),
                 multiple = TRUE,
                 options = list(
                   `actions-box` = TRUE,
                   `deselect-all-text` = "Aucun site",
                   `select-all-text` = "Tous les sites fonctionnels",
                   `none-selected-text` = "Aucun SF de sélectionné",
                   `selected-text-format` = "count > 1",
                   `count-selected-text` = "{0} site sur {1}"
                 )),
               pickerInput(
                 inputId = "selection_cycles", 
                 choices = levels(limicoles$cycle),
                 selected = levels(limicoles$cycle),
                 multiple = TRUE,
                 options = list(
                   `actions-box` = TRUE,
                   `deselect-all-text` = "Aucun cycle",
                   `select-all-text` = "Tous les cycles",
                   `none-selected-text` = "Aucun cycle de sélectionné",
                   `selected-text-format` = "count > 1",
                   `count-selected-text` = "{0} cycle sur {1}"
                 )),
               menuSubItem("Boxs", tabName = "boxs")
               #menuSubItem("Graphique", tabName = "occurence")
               ),
      menuItem("Analyse d'un site", 
               tabName = "widgets", 
               icon = icon("th"),
               pickerInput(
                 inputId = "selection_sites_fonctionnels", 
                 label = "Site fonctionnel :",
                 choices = levels(limicoles$site_fonctionnel_nom)
                 ),
               menuSubItem("blabla", tabName = "blabla"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "boxs",
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("soussiteBox"),
                valueBoxOutput("visitBox"),
                valueBoxOutput("obsBox")
              )),
      # First tab content
      tabItem(tabName = "occurence",
              fluidRow(
                box(plotlyOutput("plot1", height = 250)),
                
                box(plotOutput("plot2", height = 250))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)



ui2 <- navbarPage("Limicoles côtiers",
                  #Déf du premier onglet du shiny : l'analyse génrale tous SFs confondus
                  
                  ###### Here : insert shinydashboard dependencies ######
                  header = tagList(
                    useShinydashboard()
                  ),
                  tabPanel("Analyse générale",
                           fluidRow(
                             column(6,
                                    pickerInput(
                                      inputId = "selection_SF", 
                                      choices = levels(limicoles$site_fonctionnel_nom),
                                      selected = levels(limicoles$site_fonctionnel_nom),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE,
                                        `deselect-all-text` = "Aucun site",
                                        `select-all-text` = "Tous les sites fonctionnels",
                                        `none-selected-text` = "Aucun SF de sélectionné",
                                        `selected-text-format` = "count > 1",
                                        `count-selected-text` = "{0} site sur {1}"
                                      ))),
                             column(6,
                                    pickerInput(
                                      inputId = "selection_cycles", 
                                      choices = levels(limicoles$cycle),
                                      selected = levels(limicoles$cycle),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE,
                                        `deselect-all-text` = "Aucun cycle",
                                        `select-all-text` = "Tous les cycles",
                                        `none-selected-text` = "Aucun cycle de sélectionné",
                                        `selected-text-format` = "count > 1",
                                        `count-selected-text` = "{0} cycle sur {1}"
                                      )))
                           ),
                           fluidRow(column(4,valueBoxOutput("soussiteBox")),
                                    column(4,valueBoxOutput("visitBox")),
                                    column(4,valueBoxOutput("obsBox"))
                           ),
                           fluidRow(column(12,plotlyOutput("plot1")))
                           
                  ),
                  tabPanel("Analyse par SF")
)


server <- function(input, output, session) {
  
  #fonction de chargement des données toutes les 60s s'il y a eu des mises à jour 
  data <- reactivePoll(60000, session,
                       checkFunc = check_for_update,
                       valueFunc = get_data)
  
  filtered_data <- reactive({
    res <- dplyr::filter(data(), site_fonctionnel_nom %in% input$selection_SF)
    res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    res
  })
  
  output$plot1 <- renderPlotly({
    g <- ggplot(filtered_data()) + aes(x = lubridate::floor_date(date_comptage, "week")) + 
      geom_bar(fill="steelblue") +
      geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white", size=3.5) +
      theme_minimal() +
      labs(title="Nombre d'observations par semaine", 
           x="", y = "Individus")
    ggplotly(g)
  })
  
  output$plot2 <- renderPlot({
    ggplot(filtered_data()) + aes(x = lubridate::floor_date(date_comptage, "week")) + 
      geom_bar(fill="steelblue") +
      geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white", size=3.5) +
      theme_minimal() +
      labs(title="Nombre d'observations par semaine", 
           x="", y = "Individus")
  })
  
  nb_sites <- reactive({
    #res <- dplyr::filter(data(), site %in% input$selection_sites)
    #res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    res <- limicoles %>% filter(site_fonctionnel_nom %in% input$selection_SF) %>%
      filter(cycle %in% input$selection_cycles)
    n_distinct(res$site)
  })
  
  output$soussiteBox <- renderValueBox({
    valueBox(
      nb_sites(), "Sous-sites fonctionnels", icon = icon("map-marked-alt", lib = "font-awesome"),
      color = "green",width = 4
    )
  })
  
  nb_visites <- reactive({
    res <- dplyr::filter(data(), site_fonctionnel_nom %in% input$selection_SF)
    res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    n_distinct(res$id_visite)
  })
  
  output$visitBox <- renderValueBox({
    valueBox(
      nb_visites(), "Visites", icon = icon("binoculars", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  nb_observations <- reactive({
    res <- dplyr::filter(data(), site_fonctionnel_nom %in% input$selection_SF)
    res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    nrow(res)
  })
  
  output$obsBox <- renderValueBox({
    valueBox(
      nb_observations(), "Observations", icon = icon("kiwi-bird", lib = "font-awesome"),
      color = "red"
    )
  })
  
  #coupure des connections à la base de données à la fermeture de shiny
  session$onSessionEnded(close_connection)
}

shinyApp(ui2, server)