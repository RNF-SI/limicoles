#Chargement des packages et installation des packages manquants (pour une utilisation en local, pas sur le serveur)
list.of.packages <- c("shiny", "RPostgreSQL", "tidyr", "tidyverse", "lubridate", "ggplot2", "DT", "shinyWidgets", "shinydashboard", "plotly","ggtext")
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
  limicoles$nom_vern<-factor(limicoles$nom_vern)
  limicoles$annee<-as.numeric(format(limicoles$date_comptage, format = "%Y"))
  limicoles$mois<-as.numeric(format(limicoles$date_comptage, format = "%m"))
  limicoles$jour<-as.numeric(format(limicoles$date_comptage, format = "%d"))
  
  return(limicoles)
}

#appel initial des données (nécessaire pour l'UI)
limicoles <- get_data()

#appel des tableau de données annexes stockées sur le serveur
seuil.inter <- dbGetQuery(con,'select * from ann_limicoles."Seuils_internationaux"')

# fonction de fermeture de connexion postgres
close_connection <- function() {
  dbDisconnect(con)
  dbUnloadDriver(drv)
}

# Definition de l'UI version dashboard (interface utilisateur)
#ui <- dashboardPage(
#  
#  dashboardHeader(title = "Limicoles Côtiers RNF"),
#  ## Sidebar content
#  dashboardSidebar(
#    sidebarMenu(
#      menuItem("Analyse globale", 
#               tabName = "dashboard", 
#               icon = icon("dashboard"),
#               pickerInput(
#                 inputId = "selection_SF", 
#                 choices = levels(limicoles$site_fonctionnel_nom),
#                 selected = levels(limicoles$site_fonctionnel_nom),
#                 multiple = TRUE,
#                 options = list(
#                   `actions-box` = TRUE,
#                   `deselect-all-text` = "Aucun site",
#                   `select-all-text` = "Tous les sites fonctionnels",
#                   `none-selected-text` = "Aucun SF de sélectionné",
#                   `selected-text-format` = "count > 1",
#                   `count-selected-text` = "{0} site sur {1}"
#                 )),
#               pickerInput(
#                 inputId = "selection_cycles", 
#                 choices = levels(limicoles$cycle),
#                 selected = levels(limicoles$cycle),
#                 multiple = TRUE,
#                 options = list(
#                   `actions-box` = TRUE,
#                   `deselect-all-text` = "Aucun cycle",
#                   `select-all-text` = "Tous les cycles",
#                   `none-selected-text` = "Aucun cycle de sélectionné",
#                   `selected-text-format` = "count > 1",
#                   `count-selected-text` = "{0} cycle sur {1}"
#                 )),
#               menuSubItem("Boxs", tabName = "boxs")
#               #menuSubItem("Graphique", tabName = "occurence")
#               ),
#      menuItem("Analyse d'un site", 
#               tabName = "widgets", 
#               icon = icon("th"),
#               pickerInput(
#                 inputId = "selection_sites_fonctionnels", 
#                 label = "Site fonctionnel :",
#                 choices = levels(limicoles$site_fonctionnel_nom)
#                 ),
#               menuSubItem("blabla", tabName = "blabla"))
#    )
#  ),
#  ## Body content
#  dashboardBody(
#    tabItems(
#      tabItem(tabName = "boxs",
#              fluidRow(
#                # Dynamic valueBoxes
#                valueBoxOutput("soussiteBox"),
#                valueBoxOutput("visitBox"),
#                valueBoxOutput("obsBox")
#              )),
#      # First tab content
#      tabItem(tabName = "occurence",
#              fluidRow(
#                box(plotlyOutput("plot1", height = 250)),
#                
#                box(plotOutput("plot2", height = 250))
#              )
#      ),
#      
#      # Second tab content
#      tabItem(tabName = "widgets",
#              h2("Widgets tab content")
#      )
#    )
#  )
#)



ui2 <- navbarPage("Limicoles côtiers",
                  #Déf du premier onglet du shiny : l'analyse génrale tous SFs confondus
                  
                  ###### Here : insert shinydashboard dependencies ######
                  header = tagList(
                    useShinydashboard()
                  ),
                  
                  #### Insertion des tag CSS pour les éléments de l'UI #####
                  tags$head(tags$style(HTML('
                      #textpheno{
                        background-color: #b3b3e6 !important;
                        padding: 10px;
                        margin: 15px;
                        border-radius: 15px 30px 30px;
                        text-align: justify;
                        border: none;
                        }'))),
                  
                  
                  tabPanel("Analyse générale",
                           fluidRow(
                             column(8,
                                    pickerInput(
                                      label = "Sélection des sites fonctionnels contributeurs de l'OPNL",
                                      width = '100%',
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
                             column(4,
                                    pickerInput(
                                      label = "Sélection des cycles annuels d'intérêt",
                                      width = '100%',
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
                           fluidRow(column(4,valueBoxOutput("soussiteBox",width = NULL)),
                                    column(4,valueBoxOutput("visitBox",width = NULL)),
                                    column(4,valueBoxOutput("obsBox",width = NULL))
                           ),
                           fluidRow(column(12,plotlyOutput("plot1")))
                           
                  ),
                  tabPanel("Analyse par SF",
                           fluidPage(sidebarLayout(
                             sidebarPanel(
                               selectInput(label = "Sélectionner votre site fonctionnel d'intérêt",
                                           inputId = "selection_SF2",
                                           choices = levels(limicoles$site_fonctionnel_nom),
                                           selected = NULL,
                                           multiple = FALSE,
                                           width = '100%'),
                               selectInput(label = "Sélectionner une espèce",
                                           inputId = "selection_esp",
                                           choices = levels(limicoles$nom_vern),
                                           selected = NULL,
                                           multiple = FALSE,
                                           width = '100%'),
                               sliderInput(label = "Pas de temps voulu pour la phénologie mensuelle",
                                           inputId = "range",
                                           value = c(2007,max(unique(limicoles$annee))),
                                           min = min(unique(limicoles$annee)),
                                           max = max(unique(limicoles$annee)),
                                           step = 1,
                                           width = '100%',
                                           sep = ""),
                               checkboxGroupInput(label = "Sous-sites inclus dans le site fonctionnel",
                                                  inputId = "sous_sites")
                             ),
                             mainPanel(tabsetPanel(
                               tabPanel("Graphiques",
                                        fluidRow(column(9,plotOutput("pheno_mens")),
                                                 column(3,fluidRow(column(12,htmlOutput("textpheno")),
                                                                   column(12,downloadBttn("DownloadPheno",
                                                                                          label = "Télécharger la phénologie"))
                                                                   )
                                                        )
                                                 )),
                               tabPanel("Données",
                                        fluidRow(column(12,dataTableOutput("table"))),
                                        fluidRow(column(12,dataTableOutput("datapheno")))),
                               tabPanel("Indicateurs")
                             ))
                           )))
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
  
  ################ Fonctions de mise à jour des inputs #######################
  
  #Met à jour les sous-sites séléctionnés chaque fois que le site fonctionnel change
  observe({
    updateCheckboxGroupInput(session,
                             inputId = "sous_sites",
                             choices = unique((limicoles %>% filter(site_fonctionnel_nom == input$selection_SF2))$site),
                             selected = unique((limicoles %>% filter(site_fonctionnel_nom == input$selection_SF2))$site)
                             )
    })
  
  
  
  ############### Panel d'analyse générale #######################
  
  output$plot1 <- renderPlotly({
    g <- ggplot(filtered_data()) + aes(x = lubridate::floor_date(date_comptage, "week")) + 
      geom_bar(fill="steelblue") +
      geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white", size=3.5) +
      theme_minimal() +
      labs(title="Nombre d'observations par semaine", 
           x="", y = "Individus")
    ggplotly(g)
  })
  
  nb_sites <- reactive({
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
  
  
  ############### Panel d'analyse par site fonctionnel #######################
  
  #Filtrage des données brutes pour le site fonctionnel et les sous-sites sélectionnés
  data_pheno <- reactive({
    t1<-limicoles %>% filter(nom_vern == input$selection_esp) %>%
      filter(site_fonctionnel_nom == input$selection_SF2) %>%
      filter(site %in% input$sous_sites) %>%
      filter(annee >= input$range[1] & annee <= input$range[2]) %>%
      select(nom_vern,effectif,site,comptage,date_comptage,cycle)
    print(t1)
  })
  
  #Affichage du tableau lié
  output$table <- renderDataTable({
    data_pheno()
  })
  
  data_graphe_pheno <- reactive({
    #Tri du jeu de données complet selon tous les inputs utilisateur
    t1<-limicoles %>% filter(nom_vern == input$selection_esp) %>%
      filter(site_fonctionnel_nom == input$selection_SF2) %>%
      filter(site %in% input$sous_sites) %>%
      filter(annee >= input$range[1] & annee <= input$range[2])
    
    #Création du dataset pour le graphique, avec les moyennes et les sd par mois
    ordremois<-c("Juil.","Aout","Sept.","Oct.","Nov.","Déc.","Jan.","Fév.","Mar.","Avr.","Mai","Juin") #ordre des mois voulus pour le graphique
    t2<-t1 %>% group_by(annee,mois) %>%
      summarise(Somme_annuelle=sum(effectif,na.rm = T)) %>%
      as.data.frame() %>%
      group_by(mois) %>%
      summarize(Moyenne_effectif = mean(Somme_annuelle,na.rm=T),
                sd_effectif = sd(Somme_annuelle,na.rm = T)) %>%
      mutate(Mois = c("Jan.","Fév.","Mar.","Avr.","Mai","Juin","Juil.","Aout","Sept.","Oct.","Nov.","Déc.")) %>%
      slice(match(ordremois, Mois)) %>%
      mutate(across(where(is.numeric), round, 1)) %>%
      as.data.frame()
    
    t2
  })
  
  output$datapheno <- renderDataTable({
    data_graphe_pheno()
  })
  
  
  plotPhenomens<-reactive({
    ordremois<-c("Juil.","Aout","Sept.","Oct.","Nov.","Déc.","Jan.","Fév.","Mar.","Avr.","Mai","Juin") #ordre des mois voulus pour le graphique
    title<-paste("<span style = 'font-size:12pt'><b>Effectifs moyens agrégés par mois pour les sous-sites séléctionnés - ",input$range[1]," à ",input$range[2],"</b><br>
                 <span style = 'font-size:10pt'>",input$selection_SF2," - <b><em>",input$selection_esp,sep="")
    
    plot<-ggplot(data_graphe_pheno(),aes(x=Mois,y=Moyenne_effectif))+
      geom_col(width = 0.7,
               color="#990000",
               fill="#990000",
               alpha=0.7)+
      scale_x_discrete(name="Mois",
                       limits=ordremois,
                       labels=c("Juil.","Aout","Sept.","Oct.","Nov.","Déc.","Jan.","Fév.","Mar.","Avr.","Mai","Juin"))+
      geom_errorbar(aes(ymin=Moyenne_effectif, ymax=Moyenne_effectif+sd_effectif), 
                    width=.2,
                    position=position_dodge(.9))+
      #geom_hline(aes(yintercept = seuil.inter, #placement de la ligne pour le seuil international
      #               linetype=paste("Internat :",seuil.inter)),  #Titre de cette ligne pour la légende
      #           colour = "darkred",
      #           size=.5)+
      #geom_hline(aes(yintercept = seuil.nat, #placement de la ligne pour le seuil national
      #               linetype=paste("National : ",seuil.nat)), #Titre de cette ligne pour la légende
      #           colour = "blue",
      #           size=.5)+
      #scale_linetype_manual(name = "Seuil 1%", #Nom du titre de la légende
      #                      values = c(2,6),  #Définition du type de ligne pour les deux lignes. 2 c'est dashed, et 6 c'est dotdashed
      #                      guide = guide_legend(override.aes = list(color = c("darkred", "blue"))))+ #On doit redéfinir les couleurs 
    labs(title=title, 
         x="Mois", 
         y = "Effectifs moyens",
         caption="© Observatoire Patrimoine Naturel Littoral - Volet 'Limicoles côtiers'. RNF-OFB, 2022")+
      theme_minimal()+
      theme(plot.title=element_textbox_simple(hjust=0.5,
                                              halign = 0.5,
                                              color="white",
                                              minwidth = unit(1, "in"), # Largeur min et max de la boite autour du texte
                                              maxwidth = unit(8, "in"),
                                              padding = margin(2, 4, 2, 4), # Espaces entre le texte et les bord du cadre
                                              margin = margin(0, 0, 5, 0), # Espaces entre les bords du cadre et les marges
                                              fill="#990000",  # Couleur du fond
                                              box.color = "black",  # Couleur des bords
                                              r = unit(5, "pt"),  # Rayon d'arrondi des coins
                                              linetype = 1),  #Type de ligne (trait plein, pointille, etc)),
            plot.caption = element_text(size=7,face="italic"), #Déf de la forme du copyright
            axis.text.x = element_text(angle=45))#,
    #legend.position=c(0.85,0.85),
    #legend.title=element_text(face="bold",hjust = 0.5),
    #legend.box.background = element_rect(fill="white"),
    #legend.key.width=unit(0.7,"cm")) #permet d'agrandir un peu le petites boites ou sont les figurés dans la légende
    
    plot
    #ggplotly(plot)%>%layout(title = title)
  })
  
  
  output$pheno_mens<-renderPlot({
    plotPhenomens()
    })
  
  output$DownloadPheno <- downloadHandler(
        filename = function(){paste("PhenoMensAgg_",input$selection_SF2,"_",input$selection_esp,"_",input$range[1],".",input$range[2],".png",sep = "")} , # variable du nom
        content = function(file) {
          ggsave(file,
                 plot= plotPhenomens(), 
                 device = "png",
                 dpi = 300,
                 width = 18.5,
                 height = 14,
                 units = "cm")
        })
  
  output$textpheno<-renderText({
    HTML(paste('<div class="shadowbox">',
    "Ce graphique montre l'évolution de la fréquentation du site fonctionnels choisi <b>(",input$selection_SF2,")</b> par l'espèce sélectionnée
    <b>(",input$selection_esp,")</b> au cours des mois de l'année centrés sur la période d'hivernage des limicoles. Les effectifs sont cumulés
    entre pour tous les sous-sites séléctionnés, puis moyennés par mois.
    <br>Le graphique est téléchargeable ci-dessous<br>  </div>",
          sep=""))
    })
  
  
  #coupure des connections à la base de données à la fermeture de shiny
  session$onSessionEnded(close_connection)
}

shinyApp(ui2, server)