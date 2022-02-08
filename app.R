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
SF_abr<-dbGetQuery(con,'select * from ann_limicoles."Sites.Fonctionnels_Abr"')
  

# fonction de fermeture de connexion postgres
close_connection <- function() {
  dbDisconnect(con)
  dbUnloadDriver(drv)
}


ui2 <- navbarPage("Limicoles côtiers",
                  #Déf du premier onglet du shiny : l'analyse génrale tous SFs confondus
                  
                  # Here : insert shinydashboard dependencies #
                  header = tagList(
                    useShinydashboard()
                  ),
                  
                  tabPanel("Analyse générale",
                           #fluidRow(
                           #  column(8,
                           #         pickerInput(
                           #           label = "Sélection des sites fonctionnels contributeurs de l'OPNL",
                           #           width = '100%',
                           #           inputId = "selection_SF", 
                           #           choices = levels(limicoles$site_fonctionnel_nom),
                           #           selected = levels(limicoles$site_fonctionnel_nom),
                           #           multiple = TRUE,
                           #           options = list(
                           #             `actions-box` = TRUE,
                           #             `deselect-all-text` = "Aucun site",
                           #             `select-all-text` = "Tous les sites fonctionnels",
                           #             `none-selected-text` = "Aucun SF de sélectionné",
                           #             `selected-text-format` = "count > 1",
                           #             `count-selected-text` = "{0} site sur {1}"
                           #           ))),
                           #  column(4,
                           #         pickerInput(
                           #           label = "Sélection des cycles annuels d'intérêt",
                           #           width = '100%',
                           #           inputId = "selection_cycles",
                           #           choices = levels(limicoles$cycle),
                           #           selected = levels(limicoles$cycle),
                           #           multiple = TRUE,
                           #           options = list(
                           #             `actions-box` = TRUE,
                           #             `deselect-all-text` = "Aucun cycle",
                           #             `select-all-text` = "Tous les cycles",
                           #             `none-selected-text` = "Aucun cycle de sélectionné",
                           #             `selected-text-format` = "count > 1",
                           #             `count-selected-text` = "{0} cycle sur {1}"
                           #           )))
                           #),
                           
                           
                           fluidRow(column(4, 
                                           h3(strong("Observatoire du Patrimoine Naturel Littoral - limicoles côtiers"))),
                                           column(8, 
                                                  img(src="Logo RNF vert.png",
                                                      width=144,
                                                      height=78,
                                                      style="margin: 1rem;"),
                                                  img(src="Logo CEFE.png",
                                                      width=175,
                                                      height=78,
                                                      style="margin: 1rem;"),
                                                  img(src="OFB_LogoHoriz_RVB.png",
                                                      width=185,
                                                      height=85,
                                                      style="margin: 1rem;"),
                                                  img(src="Logo-ephe-coul-1.png",
                                                      width=100,
                                                      height=85,
                                                      style="margin: 1rem;"))),
                           tags$hr(),
                           tags$p(id="textintro",
                             "Cet outil de visualisation de données du protocole de comptage mensuel des limicoles côtiers a été créé par l'",
                              tags$a(href="https://www.reserves-naturelles.org/rnf/projets/observatoire-du-patrimoine-naturel-littoral","Observatoire du Patrimoine Naturel Littoral"),
                              "animé par Réserves naturelles de France (RNF) et développé depuis 2009 avec le concours de l’Office Français de la Biodiversité (OFB). 
                             L'OPNL est une démarche à caractère national, ascendante et collective : portée par et pour les gestionnaires d’Aires Marines Protégées (AMP), au service de leurs stratégies de gestion."),
                           
                           tags$p("Applicable à de larges territoires, reproductible et facile à mettre en œuvre, le protocole de surveillance scientifique des oiseaux limicoles côtiers se traduit par des comptages 
                           mensuels aux dates des IWC (International Waderbird Census), soit vers le milieu de chaque mois, avec une différenciation des données 
                           recueillies sur les aires marines protégées de celles observées en dehors, pour évaluer les dispositifs de gestion et de protection de la nature mis en place (mesure de l’effet gestion)."),
                           tags$p("Lancé en 2000, ce dispositif de surveillance est aujourd’hui mis en œuvre sur près de 95 localités littorales. S’inscrivant en complémentarité des comptages nationaux et 
                                  internationaux conduits à la mi-janvier (Wetlands International), cette initiative se traduit par une",
                                  tags$b("standardisation mensuelle des dénombrements"),"étendue à l’ensemble du cycle annuel.",tags$b("Aujourd'hui l'OPNL c'est :")),
                           
                           
                           fluidRow(column(4,valueBoxOutput("soussiteBox",width = NULL)),
                                    column(4,valueBoxOutput("visitBox",width = NULL)),
                                    column(4,valueBoxOutput("obsBox",width = NULL))
                           ),
                           fluidRow(column(6,plotlyOutput("Evo_contributeurs")),
                                    column(6,img(src="Carte_contrib.png",
                                                 width="100%",
                                                 height="auto",
                                                 style="margin: 1rem;"),align="center")
                           )
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
                                        h4(strong("Distribution mensuelle à l'échelle du site fonctionnel")),
                                        fluidRow(column(9,plotOutput("pheno_mens")),
                                                 column(3,fluidRow(column(12,prettyCheckboxGroup(label = "Seuils RAMSAR 1%",
                                                                                                inputId = "RAMSAR",
                                                                                                choices = c("National","International"),
                                                                                                shape = "curve",
                                                                                                animation = "pulse",
                                                                                                width = '100%'),
                                                                          style='padding-top:150px;'),
                                                                   column(12,downloadBttn("DownloadPheno",
                                                                                          label = "Télécharger le graphique",
                                                                                          style = "jelly",
                                                                                          size = "sm",
                                                                                          block = T))
                                                                   )
                                                        )
                                                 ),
                                        fluidRow(column(12,htmlOutput("textpheno"))),
                                        tags$head(tags$style(HTML("#textpheno{background-color: #e6b3b3;
                                                             padding: 15px;
                                                             margin: 10px 10px 20px;
                                                             border-radius: 8px 30px 30px;
                                                             text-align: justify;
                                                             border: none;
                                                                           }"))),
                                        fluidRow(column(9,DT::dataTableOutput("datapheno")),
                                                 column(3,fluidRow(column(12,valueBoxOutput("SeuilNatBox",width = NULL),style='padding-top:50%;'),
                                                                   column(12,valueBoxOutput("SeuilInterBox",width = NULL))
                                                                  )
                                                        )
                                                ),
                                        hr(),
                                        h4(strong("Effort de comptage l'échelle du site fonctionnel")),
                                        p("Le graphique ci-dessous permet de mettre en regard les moyennes par mois obtenues à l'échelle de toute le site fonctionnel
                                          avec l'effort de comptage. Il permet de notamment de visualiser les mois où peu de comptages ont été réalisés entre les deux années séléctionnées.
                                          Il convient donc de prendre les moyennes calculées à partir de peu de comptages (< 4)"),
                                        p(tags$b("Le nombre de site de suivis"),"évolue souvent au fil des années également. Ceci est dans la plupart des cas dû à un augmentation
                                          de la couverture de comptage. Dans certains cas, cela peut être dû à un sous-site dont l'appelation est abandonné au profit d'une autre.",tags$br(),
                                          "En cas de doute, se référer à l'onglet",tags$em("'Données'"),"en classant par date permettra de mieux visualiser l'évolution des sites de comptage"),
                                        p(tags$b("Sur certains sites, lorsque beaucoup de comptages (voire tous) manquent sur une période, il est probable que le site n'était pas encore contributeur de l'OPNL sur la période")),
                                        fluidRow(column(9,plotOutput("Effort_comptage")))
                                        ),
                               tabPanel("Données",
                                        fluidRow(column(12,dataTableOutput("table")))
                                        ),
                               tabPanel("Indicateurs",
                                        h3(strong("Indicateurs d'état des populations de limicoles en janvier")),
                                        p("Dans cet onglet, nous présentons les éléments des",strong("fiches indicateurs"),"établies sur la base des comptages de populations
                                          à la mi-janvier. Ces comptages sont valorisés par l'OPNL au sein d'une fiche synthétique établie pour chaque site fonctionnel et chaque espèce d'importance
                                          dans le site en question."),
                                        p("Chacun des éléments présenté ci-dessous est téléchargeable à l'aide du bouton associé."),
                                        p(strong("La fiche complète est téléchargeable en bas de document pour la décennie séléctionnée")),
                                        hr(),
                                        fluidRow(
                                          column(8,htmlOutput("textindic")),
                                          tags$head(tags$style(HTML("#textindic{background-color: #bdbde5;
                                                             padding: 10px;
                                                             margin: 10px;
                                                             border-radius: 20px 8px 8px 20px;
                                                             text-align: justify;
                                                             border: none;
                                                                           }"))),
                                          column(4,pickerInput(
                                                       label = "Sélection décennie glissante",
                                                       width = '100%',
                                                       inputId = "selection_dec",
                                                       choices = c("2007-2016" = "2007.2016",
                                                                   "2008-2017" = "2008.2017",
                                                                   "2009-2018" = "2009.2018",
                                                                   "2010-2019" = "2010.2019"),
                                                       selected = "2008.2017",
                                                       multiple = FALSE,
                                                       options = list(`actions-box` = TRUE)
                                                       ))
                                        ),
                                        fluidRow(
                                          column(6,offset = 3,downloadBttn("DownloadIndic",
                                                                           label = "Télécharger les indicateurs pour toutes les espèces",
                                                                           style = "jelly",
                                                                           size = "sm",
                                                                           color = "warning",
                                                                           block = T))
                                                ),
                                        hr(),
                                        h4(strong("Informations générales pour le site fonctionnel")),
                                        fluidRow(column(6,imageOutput("tabIndic"),align="center",style="padding-top:25px;padding-bottom:10px;"),
                                                 column(6,imageOutput("Rmultiesp"),align="center",style="padding-top:25px;padding-bottom:10px;")
                                                 ),
                                        fluidRow(column(6,downloadBttn("Download_tabIndic",
                                                                       label = "Télécharger le tableau",
                                                                       style = "jelly",
                                                                       size = "sm",
                                                                       block = T)),
                                                 column(6,downloadBttn("Download_RIndic",
                                                                       label = "Télécharger le graphique général",
                                                                       style = "jelly",
                                                                       size = "sm",
                                                                       block = T))
                                                 ),
                                        hr(),
                                        h4(strong("Indicateurs taxons-centrés")),
                                        fluidRow(column(3,offset = 1,imageOutput("R_local"),style="padding-bottom:10px;"),
                                                 column(3,offset = 1,imageOutput("R_SRM"),style="padding-bottom:10px;"),
                                                 column(3,offset = 1,imageOutput("R_national"),style="padding-bottom:10px;")
                                                 ),
                                        fluidRow(column(3,offset = 1,downloadBttn("Dl_R_local",
                                                                                 label = "Téléch. tendance loc.",
                                                                                 style = "jelly",
                                                                                 size = "xs",
                                                                                 block = T)),
                                                 column(3,offset = 1,downloadBttn("Dl_R_SRM",
                                                                                 label = "Téléch. tendance SRM.",
                                                                                 style = "jelly",
                                                                                 size = "xs",
                                                                                 block = T)),
                                                 column(3,offset = 1,downloadBttn("Dl_R_national",
                                                                                  label = "Téléch. tendance nat.",
                                                                                  style = "jelly",
                                                                                  size = "xs",
                                                                                  block = T))),
                                        fluidRow(column(12,htmlOutput("text_tendances"))),
                                        tags$head(tags$style(HTML("#text_tendances{background-color: #bdbde5;
                                                             padding: 15px;
                                                             margin: 10px 10px 10px;
                                                             border-radius: 8px 8px 30px 30px;
                                                             text-align: justify;
                                                             border: none;
                                                                           }"))),
                                        fluidRow(column(8,imageOutput("Roue")),
                                                 column(4,imageOutput("Map"),style="margin:0 0 100px 0; padding:0;")),
                                        fluidRow(column(4, offset = 2,downloadBttn("Dl_Roue",
                                                                                   label = "Télécharger la roue",
                                                                                   style = "jelly",
                                                                                   size = "sm",
                                                                                   block = T)),
                                                 column(4,offset = 2,downloadBttn("Dl_Map",
                                                                       label = "Télécharger la carte de distrib.",
                                                                       style = "jelly",
                                                                       size = "sm",
                                                                       block = T))
                                                 ),
                                        hr()
                                )
                                        
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
  
 
  
  output$Evo_contributeurs <- renderPlotly({
    tgraph<-limicoles %>% group_by(annee) %>%
      summarise(Nb_sous_sites=length(unique(site))) %>% 
      as.data.frame()
      
    
    g <- ggplot(tgraph, aes(x =annee,y=Nb_sous_sites)) + 
      geom_line(col='#6f6fab',size=.8) + 
      labs(title = "Evolution du nombre de sites contributeurs depuis les début du réseau OPNL",
           x = "Années") + theme_minimal() +
      theme(plot.title=element_text(hjust=0.5))

    ggplotly(g)
  })
  
  nb_sites <- reactive({
    #res <- limicoles %>% filter(site_fonctionnel_nom %in% input$selection_SF) %>%
    #  filter(cycle %in% input$selection_cycles)
    n_distinct(limicoles$site)
  })
  
  output$soussiteBox <- renderValueBox({
    valueBox(
      nb_sites(), "Sous-sites fonctionnels", icon = icon("map-marked-alt", lib = "font-awesome"),
      color = "green",width = 4
    )
  })
  
  nb_visites <- reactive({
    #res <- dplyr::filter(data(), site_fonctionnel_nom %in% input$selection_SF)
    #res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    n_distinct(limicoles$id_visite)
  })
  
  output$visitBox <- renderValueBox({
    valueBox(
      nb_visites(), "Visites", icon = icon("binoculars", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  nb_observations <- reactive({
    #res <- dplyr::filter(data(), site_fonctionnel_nom %in% input$selection_SF)
    #res <- dplyr::filter(res, cycle %in% input$selection_cycles)
    nrow(limicoles)
  })
  
  output$obsBox <- renderValueBox({
    valueBox(
      nb_observations(), "Observations", icon = icon("kiwi-bird", lib = "font-awesome"),
      color = "red"
    )
  })
  
  
  
  #--------------------------------------------------------------------------#
  ############### Panel d'analyse par site fonctionnel #######################
  #--------------------------------------------------------------------------#
  
  
  #On récupère l'abbréviation du site et de l'espèce sélectionnée
  Abr_SF<-reactive({
    SF_abr<- SF_abr %>% 
      mutate(nom_SF=gsub("Site fonc. ","",site.fonctionnel))
    
    SF_abr[which(input$selection_SF2==SF_abr$nom_SF),"abb.site"] ##Récupère l'abbréaviation du site
  })
  
  Abr_esp<-reactive({
    especes[which(especes$vernaculaire==input$selection_esp),"abr"]
  })
  
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
  

  #Récupération du seuil INTERNATIONAL pour l'espèce
  seuil.inter<-reactive({
    seuil.international[which(seuil.international$vernaculaire==input$selection_esp),"seuil_1pourc_internat"]
  })
  
  #Fonction pour récupération du seuil NATIONAL moyenné entre deux bornes années 
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
  
  #Récupération du seuil NATIONAL en réctive par rapport aux changements d'espèces
  seuil.national<-reactive({
    abr_esp<-especes[which(especes$vernaculaire==input$selection_esp),"abr"]
    if (abr_esp %in% colnames(seuil.nat)){
      Seuilnational<-seuils.cycle(y1 = input$range[1], y2 = input$range[2])[abr_esp,]
    } else {
      Seuilnational<-as.numeric(NA)
    }
    
    Seuilnational
  })
  
  
  #Création du ggplot avec différentes options pour ajouter les seuils RAMSAR ou non
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
  
  
  #Fonction de render du graphique ggplot créé, qui permet de l'afficher sur le shiny
  output$pheno_mens<-renderPlot({
    plotPhenomens()
    })
  
  #Création du bouton pour télécharger le graphique
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
  
  #création d'un paragraphe texte explicatif sur les éléments du graphique
  output$textpheno<-renderText({
    paste(
    "Ce graphique montre l'évolution de la fréquentation du site fonctionnel choisi <b>(",input$selection_SF2,")</b> par l'espèce sélectionnée
    <b>(",input$selection_esp,")</b> au cours des mois de l'année centrés sur la période d'hivernage des limicoles. Les effectifs sont cumulés
    pour tous les sous-sites séléctionnés, puis moyennés par mois. Les écart-types représentés permettent d'apprécier la variabilité inter-annuelle de effectifs.
    <br> Vous pouvez choisir d'afficher ou non les seuils d'importance 1% RAMSAR sur le graphe.
    <br> <em>Le graphique est téléchargeable au format PNG avec le bouton à droite de l'image.</em></p>",
          sep="")
    })
  
  
  ##On crée un tableau spécifique réactif avec les éléments présents dans le graphique
  data_table_pheno<-reactive({
    initab <- data_graphe_pheno() %>% select(-mois) %>%
      relocate(Mois, Moyenne_effectif, sd_effectif)
    
    ## On formate la fonction true/false qui permettra la mise en forme conditionnelle de la colonne "Moyenne_effectif" avec le package formattable
    true_false_formatter <-
      formatter("span",
                style = x ~ formattable::style(  #On pointe vers la fonctin style de formattable car plotly possède aussi une fonction style et ça crée une ollision sinon
                  font.weight = ifelse(x >= seuil.national(), "bold", ""),
                  background = ifelse(x >= seuil.inter() & !is.na (seuil.inter()),              #premier test
                                      "rgba(227,53,15,0.8)",                                    #couleur du background si premier test vrai (le 4ieme argument est la transparence)
                                      ifelse(x >= seuil.national() & !is.na (seuil.inter()),    #deuxième test si premier test faux
                                             "rgba(17,156, 165,0.8)",                           #couleur du backround si deuxième test vrai
                                             "")),                                              #couleur du background si deux test faux
                  border.radius = "5px",      #Arrondi les coins du background
                  padding = "3px 7px 3px"     #Défini la taille du rectangle de background. 1ere valeur haut, 2ème valeur droite et gauche, troisième valeur bas
                ))
    
    ## Use formattable
    formattable(
      initab,
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
                                columnDefs = list(list(className = 'dt-center', targets = "_all"))),   #Forcer le centrer du corps des colonnes, pas des titres de colonnes
                 extensions = c("Buttons"),                                                                 #Permet d'afficher les boutons de téléchargement qui sont une extensions aux boutons normaux
                 rownames = F)
  )
  
  
  ##Création des valueBox à droite du tableau
  nb_sup_seuil_nat <- reactive({
    length(which(data_graphe_pheno()$Moyenne_effectif>=seuil.national()))
  })
  
  output$SeuilNatBox <- renderValueBox({
    valueBox(
      nb_sup_seuil_nat(), 
      subtitle = "Nombre de mois où le seuil RAMSAR national est dépassé en moyenne", 
      icon = icon("flag", lib = "font-awesome"),
      color = "teal",
      width = 3
    )
  })
  
  nb_sup_seuil_inter<- reactive({
    length(which(data_graphe_pheno()$Moyenne_effectif>=seuil.inter()))
  })
  
  output$SeuilInterBox <- renderValueBox({
    valueBox(
      nb_sup_seuil_inter(), 
      subtitle = "Nombre de mois où le seuil RAMSAR international est dépassé en moyenne", 
      icon = icon("globe-europe", lib = "font-awesome"),
      color = "red",
      width = 3
    )
  })
  
  
  
  #Création du graphique pour l'effort de comptage :
  
  ploteffort<-reactive({
    
    t<-limicoles %>% filter(site_fonctionnel_nom == input$selection_SF2) %>%
      filter(site %in% input$sous_sites) %>%
      filter(annee >= input$range[1] & annee <= input$range[2]) #on filtre le dataset global selon les inputs utilisateurs
    
    t6<-table(t$annee,t$mois) %>%                                     #Table de contingence entre annee et mois : sort le nb de ligne pour chaque croisement
      as.data.frame() %>%                                             #Remet le tableau en étendu
      rename(Année=Var1,Mois=Var2,nrow=Freq) %>% arrange(Année) %>%   #On renomme les variables
      mutate(Comptage = if_else(nrow>0,"oui","non"),                  #Si le nombre de ligne est sup à 0, c'est qu'il y a eu comptage
             Année =as.numeric(as.character(Année)),
             Mois=as.numeric(as.character(Mois)))
    
    ff<-function(x){length(unique(x))}
    t7<-aggregate(site ~ annee + mois,
                  data = t, 
                  FUN = ff) %>%  #Permet de compter rapidement le nombre de sous-sites comptés pour chaque mois
      arrange(annee) %>%
      rename(Année=annee,Mois=mois)
    
    dat<-left_join(t6,t7)  #On joint les deux tableaux pour ajouter la colonne "site"
    
    
    y_axis_break<-if(max(dat$Année)-min(dat$Année)<=5){     #Si peu d'amplitude entre les années bornes, on passe la séquence de l'axe à un pas de 1
                   seq(min(dat$Année),max(dat$Année),1)
                   }else{seq(min(dat$Année),max(dat$Année),2)}      #Pas de 2 sinon
    
    p<-ggplot(data = dat[which(dat$Comptage=="oui"),],aes(x=factor(Mois),y=Année))+
      geom_point(aes(color=factor(site,ordered = T),size=factor(site,ordered = T)))+
      scale_colour_manual(values=scales::seq_gradient_pal("deepskyblue",
                                                          "dodgerblue4",
                                                          "Lab")(seq(0,1,length.out=n_distinct(dat$site,na.rm = T))))+
      geom_point(data = dat[which(dat$Comptage=="non"),],aes(x=factor(Mois),y=Année),shape=4,color="red",size=2,stroke=2)+
      scale_x_discrete(breaks = seq(1,12,1),
                       limits = factor(c(7,8,9,10,11,12,1,2,3,4,5,6)),
                       labels = c("Jan.","Fév.","Mar.","Avr.","Mai","Juin","Juil.","Aout","Sept.","Oct.","Nov.","Déc."),
                       position="top")+
      scale_y_reverse(breaks = y_axis_break,
                      minor_breaks = seq(min(dat$Année)+0.5,max(dat$Année)-0.5,1))+
      labs(x="Mois",
           color="Nb_sous site_comptés",
           size="Nb_sous site_comptés")+
      guides(color = guide_legend(title.position = "top"),
             size = guide_legend(title.position = "top"))+
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            axis.line = element_line(colour = "gray30", 
                                     size = 1, linetype = "solid"),
            legend.title.align = 0.5,
            legend.direction = "vertical",
            legend.box.just = "center")
    
    p
  })
  
  output$Effort_comptage<-renderPlot({
    ploteffort()
  })
  
  #--------------------------------------------------------------------------#
  ################### Panel indicateurs limicoles ############################
  #--------------------------------------------------------------------------#
  
  output$textindic<-renderText({
    paste(
      "Les indicateurs limicoles sont produits par périodes glissantes de 10 ans.  En sélectionnant une décennie à droite, vous aurez
      accès à toutes les informations calculées pour l'espèce et le site fonctionnel choisi.<br>
      Vous pouvez aussi télécharger la fiche complète pour <b>toutes les espèces</b> du site fonctionnel au format PDF avec le bouton ci dessous.")
  })
  
  #Création du bouton pour télécharger le pdf complet
  output$DownloadIndic <- downloadHandler(
    filename = function(){paste("Indics_LimiCot_TousTaxons_",input$selection_SF2,"_",input$selection_dec,".pdf",sep = "")}, # variable du nom
    content = function(file) {
                   file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/5. FICHES/Fiche complete_",Abr_SF(),"_",input$selection_dec,".pdf",sep=""),
                             file)
    })
  
  output$tabIndic<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/5. FICHES/0. PreFiche/Tabpremierepage_",Abr_SF(),"_",input$selection_dec,".png",sep="")
    list(src = filepath,
         width = "100%",
         height = "auto",
         alt = "Tableau des données")
  },deleteFile = F)
  
  output$Download_tabIndic <- downloadHandler(
    filename = function(){paste("Tab_datasBrutesIndic_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/5. FICHES/0. PreFiche/Tabpremierepage_",Abr_SF(),"_",input$selection_dec,".png",sep=""),
                file)
    })
  
  output$Rmultiesp<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/5. FICHES/0. PreFiche/R_especes_",Abr_SF(),"_",input$selection_dec,".png",sep="")
    list(src = filepath,
         width = "95%",
         height = "auto",
         alt = "Tendance des espèces")
  },deleteFile = F)
  
  output$Download_RIndic <- downloadHandler(
    filename = function(){paste("R_tousTaxons_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/5. FICHES/0. PreFiche/R_especes_",Abr_SF(),"_",input$selection_dec,".png",sep=""),
                file)
    })
  
  output$R_local<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/Local/Taux.loc_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep="")
    list(src = filepath,
         width = "auto",
         height = "100%",
         alt = "Tendance locale")
  },deleteFile = F)
  
  output$R_SRM<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/Sous-region marine/Taux.locSRM_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep="")
    list(src = filepath,
         width = "auto",
         height = "100%",
         alt = "Tendance façade maritime")
  },deleteFile = F)
  
  output$R_national<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/National/Taux.loc.nat_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep="")
    list(src = filepath,
         width = "auto",
         height = "100%",
         alt = "Tendance nationale")
  },deleteFile = F)
  
  output$Dl_R_local<-downloadHandler(
    filename = function(){paste("Tendance locale_",Abr_esp(),"_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/Local/Taux.loc_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep=""),
                file)
    })
  
  output$Dl_R_SRM<-downloadHandler(
    filename = function(){paste("Tendance loc/facade_",Abr_esp(),"_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/Sous-region marine/Taux.locSRM_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep=""),
                file)
    })
  
  output$Dl_R_national<-downloadHandler(
    filename = function(){paste("Tendance loc/nationale_",Abr_esp(),"_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/2. Graphiques/National/Taux.loc.nat_",Abr_esp(),"_",Abr_SF(),gsub("\\.","_",input$selection_dec),".png",sep=""),
                file)
    })
  
  output$text_tendances<-renderText({
    paste(
      "<p><b>r :</b> tendance pour l'espèce et l'entité géographique sur la décennie sélectionnée. <em> Exemple : -0.07 [-0.01 ; -0.12] 
      correspond à une tendance à la décroissance estimée à environ -7%, avec un intervalle de crédibilité de -1% à -12%</em></p>
      <p><b>Proba (r > 0) :</b> correspond à la probabilité calculée que la tendance donnée soit effectivement supérieure à 0 (<em>respectivement inférieure</em>) lorsque que l'intervalle de
      crédibilité de la tendance recouvre 0. Si cette probabilité est de 100%, on considère que la population est statistiquement considérable comme en croissance (<em>resp. en décroissance</em>)</p>
      <p><b>Proba (r < r.SRM <em>ou r.nat</em>) :</b> correspond à la probabilité que la tendance <b>locale</b> soit effectivement inférieure (<em>resp. supérieure</em>)
      à la tendance à l'échelle de la SRM ou à la tendance nationale.</p>
      <p><b>n :</b> nombre de mois de janvier où un comptage a eu lieu pour la décennie glissante considérée (<em>max 10</em>)<br>
      <b>n.SRM :</b> nombre de sites fonctionnels inclus dans le calcul de la tendances à l'échelle SRM <br>
      <b>n.nat :</b> nombre de sites fonctionnels inclus dans le calcul de la tendances à l'échelle nationale")
  })
  
  output$Roue<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/3. ROUES/Roue_",Abr_SF(),"_",Abr_esp(),"_",input$selection_dec,".png",sep="")
    list(src = filepath,
         width = "100%",
         height = "auto",
         alt = "Roue indicateur")
  },deleteFile = F)
  
  output$Map<-renderImage({
    filepath<-paste("Indicateurs/Resultats_",input$selection_dec,"/4. Cartes/Carte_",Abr_SF(),"_",Abr_esp(),"_",input$selection_dec,".png",sep="")
    list(src = filepath,
         width = "100%",
         height = "auto",
         alt = "Carte distribution nationale")
  },deleteFile = F)
  
  output$Dl_Roue<-downloadHandler(
    filename = function(){paste("Roue indicateur_",Abr_esp(),"_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/3. ROUES/Roue_",Abr_SF(),"_",Abr_esp(),"_",input$selection_dec,".png",sep=""),
                file)
    })
  
  output$Dl_Map<-downloadHandler(
    filename = function(){paste("Carte distribution_",Abr_esp(),"_",input$selection_dec,"_",Abr_SF(),".png",sep="")}, # variable du nom
    content = function(file) {
      file.copy(paste("Indicateurs/Resultats_",input$selection_dec,"/4. Cartes/Carte_",Abr_SF(),"_",Abr_esp(),"_",input$selection_dec,".png",sep=""),
                file)
    })
  
  #coupure des connections à la base de données à la fermeture de shiny
  session$onSessionEnded(close_connection)
}

shinyApp(ui2, server)