#Chargement des packages et installation des packages manquants (pour une utilisation en local, pas sur le serveur)
list.of.packages <- c("shiny", "DataCombine","formattable", "RPostgreSQL", "tidyr", "tidyverse", "lubridate", "ggplot2", "DT", "shinyWidgets", "shinydashboard", "plotly","ggtext")
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
seuil.international <- dbGetQuery(con,'select * from ann_limicoles."Seuils_internationaux"')
seuil.nat <- dbGetQuery(con,'select * from ann_limicoles."Seuils_Nat_WI_2019_Transposee"') %>%
  rename_with(toupper, hpi:tco)
especes<-dbGetQuery(con,'select * from ann_limicoles."sp_latin_verna_abr"')

  

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
                  tags$head(tags$style(HTML("
                                  #textpheno{
                                    background-color: #b3b3e6 !important;
                                    padding: 10px;
                                    margin: 10px 10px 20px;
                                    border-radius: 15px 30px 30px;
                                    text-align: justify;
                                    border: none;
                                    }
                                  #RAMSAR{
                                    border:red;
                                  }"))),
                  
                  
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
                                                 column(3,fluidRow(column(12,prettyCheckboxGroup(label = "Seuils RAMSAR 1%",
                                                                                                inputId = "RAMSAR",
                                                                                                choices = c("National","International"),
                                                                                                shape = "curve",
                                                                                                animation = "pulse",
                                                                                                width = '100%')),
                                                                   column(12,downloadBttn("DownloadPheno",
                                                                                          label = "Télécharger le graphique",
                                                                                          style = "jelly",
                                                                                          size = "sm",
                                                                                          block = T))
                                                                   )
                                                        )
                                                 ),
                                        fluidRow(column(12,htmlOutput("textpheno"))),
                                        fluidRow(column(12,DT::dataTableOutput("datapheno")))
                                        ),
                               tabPanel("Données",
                                        fluidRow(column(12,dataTableOutput("table")))
                                        ),
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
                sd_effectif = sd(Somme_annuelle,na.rm = T))
    
    if (nrow(t2)!=12){  #Test pour résoudre le problème des mois manquants : permet d'insérer des lignes avec des NA quand il n'y pas de comptage pour le mois en question sur période
      if (max(t2$mois)!=12){  #Permet de résoudre le problème qui apparait dans la boucle for juste après, lorsque le mois manquant est le 12 (décembre) : on ajoute d'abord la ligne 12eme mois, avec des NA pour moyenne et sd
        lig<-c(rep(0,3))
        lig[1] <- 12
        lig[2:3] <- NA
        t2 <- InsertRow(t2,NewRow = as.list(lig))
      }
      k<-1
      if (nrow(t2)!=12){for (k in 1:12){  #Boucle pour insérer les mois manquants quand il y en a
        lig = c(rep(0,3))
        if (t2[k,1]!=k){
          lig[1] <- k
          lig[2:3] <- NA
          t2 <- InsertRow(t2,NewRow = as.list(lig), RowNum = k)
        }}
      }}
      t2<- t2 %>% mutate(Mois = c("Jan.","Fév.","Mar.","Avr.","Mai","Juin","Juil.","Aout","Sept.","Oct.","Nov.","Déc.")) %>%
        slice(match(ordremois, Mois)) %>%
        mutate(across(where(is.numeric), round, 1)) %>%
        as.data.frame()
    
    t2
  })
  

  #Récupération du seuil international pour l'espèce
  seuil.inter<-reactive({
    seuil.international[which(seuil.international$vernaculaire==input$selection_esp),"seuil_1pourc_internat"]
  })
  
  #Fonction pour récupération du seuil national moyenné entre deux bornes années 
  seuils.cycle<-function(y1,y2){
    seuils<-seuil.nat
    seuilsMaheo<-seuils[-c(1:20),]
    seuils<-seuils[-21,];seuils$annee<-as.numeric(as.character(seuils$annee))
    
    seuil.cycle<-subset(seuils,seuils$annee>=y1 & seuils$annee<=y2)
    Seuils<-round(colMeans((seuil.cycle[,-1])))
    Seuils<-as.data.frame(Seuils)
    colnames(Seuils)<-c(paste("Seuils 1% moyenne sur cycle",y1,y2))
    
    return(Seuils)
  }
  
  #Récupération du seuil national en réctive par rapport aux changements d'espèces
  seuil.national<-reactive({
    abr_esp<-especes[which(especes$vernaculaire==input$selection_esp),"abr"]
    if (abr_esp %in% colnames(seuil.nat)){
      Seuilnational<-seuils.cycle(y1 = input$range[1], y2 = input$range[2])[abr_esp,]
    } else {
      Seuilnational<-as.numeric(NA)
    }
    
    Seuilnational
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
            axis.text.x = element_text(angle=45))
    
    if(!is.null(input$RAMSAR)){
      if (setequal(input$RAMSAR,"National")){
        plot<-plot + 
          geom_hline(aes(yintercept = seuil.national(), #placement de la ligne pour le seuil national
                         linetype=paste("National : ",seuil.national())), #Titre de cette ligne pour la légende
                     colour = "blue",
                     na.rm = T,
                     size=.5)+
          scale_linetype_manual(name = "Seuil 1%", #Nom du titre de la légende
                                values = 6,  #Définition du type de ligne pour les deux lignes. 2 c'est dashed, et 6 c'est dotdashed
                                guide = guide_legend(override.aes = list(color = c("blue"))))+ #On doit redéfinir les couleurs
          theme(legend.position=c(0.85,0.85),
                legend.title=element_text(face="bold",hjust = 0.5),
                legend.box.background = element_rect(fill="white"),
                legend.key.width=unit(0.7,"cm")) #permet d'agrandir un peu le petites boites ou sont les figurés dans la légende
      
        } else if (setequal(input$RAMSAR,"International")) {
        plot<-plot + 
          geom_hline(aes(yintercept = seuil.inter(), #placement de la ligne pour le seuil international
                         linetype=paste("Internat :",seuil.inter())),  #Titre de cette ligne pour la légende
                     colour = "darkred",
                     na.rm = T, #ignorer le fait de faire une ligne si NA
                     size=.5)+
          scale_linetype_manual(name = "Seuil 1%", #Nom du titre de la légende
                                values = 2,  #Définition du type de ligne pour les deux lignes. 2 c'est dashed, et 6 c'est dotdashed
                                guide = guide_legend(override.aes = list(color = c("darkred"))))+ #On doit redéfinir les couleurs
          theme(legend.position=c(0.85,0.85),
                legend.title=element_text(face="bold",hjust = 0.5),
                legend.box.background = element_rect(fill="white"),
                legend.key.width=unit(0.7,"cm")) #permet d'agrandir un peu le petites boites ou sont les figurés dans la légende
        
      } else if (setequal(input$RAMSAR,c("National","International"))==T){
        plot<-plot +
          geom_hline(aes(yintercept = seuil.inter(), #placement de la ligne pour le seuil international
                         linetype=paste("Internat :",seuil.inter())),  #Titre de cette ligne pour la légende
                     colour = "darkred",
                     na.rm = T, #ignorer le fait de faire une ligne si NA
                     size=.5)+
          geom_hline(aes(yintercept = seuil.national(), #placement de la ligne pour le seuil national
                         linetype=paste("National : ",seuil.national())), #Titre de cette ligne pour la légende
                     colour = "blue",
                     na.rm = T,
                     size=.5)+
          scale_linetype_manual(name = "Seuil 1%", #Nom du titre de la légende
                                values = c(2,6),  #Définition du type de ligne pour les deux lignes. 2 c'est dashed, et 6 c'est dotdashed
                                guide = guide_legend(override.aes = list(color = c("darkred", "blue"))))+ #On doit redéfinir les couleurs
          theme(legend.position=c(0.85,0.85),
                legend.title=element_text(face="bold",hjust = 0.5),
                legend.box.background = element_rect(fill="white"),
                legend.key.width=unit(0.7,"cm")) #permet d'agrandir un peu le petites boites ou sont les figurés dans la légende
      }
    }
    
    plot
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
    HTML(paste("Ce graphique montre l'évolution de la fréquentation du site fonctionnel choisi <b>(",input$selection_SF2,")</b> par l'espèce sélectionnée
    <b>(",input$selection_esp,")</b> au cours des mois de l'année centrés sur la période d'hivernage des limicoles. Les effectifs sont cumulés
    pour tous les sous-sites séléctionnés, puis moyennés par mois. Les écart-types représentés permettent d'apprécier la variabilité inter-annuelle de effectifs.
    <br> Vous pouvez choisir d'afficher ou non les seuils d'importance 1% RAMSAR sur le graphe.
    <br> Le graphique est téléchargeable au format PNG avec le bouton.
    <br> Les données brutes correspondant aux moyennes du graphique visibles ci-dessous :",
          sep=""))
    })
  
  
  data_table_pheno<-reactive({
    initab <- data_graphe_pheno() %>% select(-mois) %>%
      relocate(Mois, Moyenne_effectif, sd_effectif)
    
    ## On formate la fonction true/false qui permettra 
    true_false_formatter <-
      formatter("span",
                style = x ~ style(
                  font.weight = ifelse(x >= seuil.national(), "bold", ""),
                  background = ifelse(x >= seuil.inter(), "rgba(227,53,15,0.8)", ifelse(x >= seuil.national(), "rgba(17,156, 165,0.8)", "")),
                  border.radius = "5px",
                  padding = "3px 7px 3px"
                ))
    
    ## Use formattable
    formattable(
      initab,
      align = c(rep("c",3)),
      list(
        ## use custom formatter for TRUE/FALSE values
        Moyenne_effectif = true_false_formatter
      )
    )
  })
  
  output$datapheno <- renderDT(
    as.datatable(data_table_pheno(),
                 class="hover",
                 options = list(pageLength=12,                                                              #Déf du nombre de ligne par page
                                dom = 'Bfrtip',                                                             #Déf des boutons affichés et de leur ordre, le B c'est pour les boutons de téléchargement
                                buttons = c('copy', 'csv', 'excel'),                                        #Boutons de téléchargement autorisés (copier coller, download csv et excel)
                                columnDefs = list(list(className = 'dt-body-center', targets = "_all"))),   #Forcer le centrer du corps des colonnes, pas des titres de colonnes
                 extensions = c("Buttons"),                                                                 #Permet d'afficher les boutons de téléchargement qui sont une extensions aux boutons normaux
                 rownames = F)
  )
  
  
  #coupure des connections à la base de données à la fermeture de shiny
  session$onSessionEnded(close_connection)
}

shinyApp(ui2, server)