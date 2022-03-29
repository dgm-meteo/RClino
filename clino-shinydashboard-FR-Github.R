#
#    Load Packages
#
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyr)
library(DT)
library(tidyverse)
library(shinybusy)
options(warn=-1)

source("Own-functions-FR-Github.R")

write.clino.param.calc.files()

startavgperiod='1991'
endavgperiod='2020'
avgperiod=paste0(startavgperiod,'-',endavgperiod)
options(shiny.maxRequestSize = 10*1024^2)
###################################################################################
#     User Interface
###################################################################################

ui <- dashboardPage(
        dashboardHeader(title = "Calcul des Normales Climatologiques OMM", titleWidth = 450),
        dashboardSidebar(width = 160, disable = TRUE,
          sidebarMenu(
            menuItem("Calcul CLINO", tabName = "clino", icon = icon("iclino"))#,
          )
        ),
        dashboardBody(
      #    tags$style(".shiny-file-input-progress {display: none}"),  # This is the new line
          tabItems(
            tabItem("clino", 
                tabsetPanel(id = "inTabset", type = "tabs",
                    #
                    #   Tab : README
                    #
                    tabPanel(strong("A propos"), value = "VReadme",
                             verbatimTextOutput("About1"), 
                             downloadButton('tempmetadatadownload',strong('Modèle de fichier des métadonnées'),
                                             class = 'btn-warning'),
                             downloadButton('tempdailydatadownload', strong('Modèle de fichier des données quotidiennes'),
                                             class = 'btn-warning'),
                             downloadButton('tempmonthlydatadownload', strong('Modèle de fichier des données mensuelles'),
                                            class = 'btn-warning'),
                             downloadButton('howtousedownload', strong("Manuel d'utilisation"),
                                            class = 'btn-warning'),
                              verbatimTextOutput("About"),
                              align="left"),  # end TabPanel ReadMe
                     #
                     #   Tab : Load daily data and metadata files
                     #
                     tabPanel(title = strong("1. Charger les données"),
                              value = "LoadData",
                              column( width = 5, 
                                p("Entrer l'indicateur des valeurs manquantes dans le fichier des données
                                quotidiennes s'il est différent de (NA,-99999,vide)"),
                                textInput("MissID", label = "Indicateur de valeur manquante", 
                                        value = "Entrer Indicateur de valeur manquante..."),
                                h5(strong("Symboles du séparateur et du décimal dans la fichier CSV")),
                                checkboxInput('ENCSV', 'Anglais (Sep = virgule "," & Dec = point ".")', value = TRUE, width = 500),
                                checkboxInput('FRCSV', 'Français (Sep = point-vrigule ";" & Dec = virgule ",")', value = FALSE, width = 500),
                                fileInput("MetaDatafile","Sélectionner le fichier des métadonnées", multiple = FALSE),
                           ##     actionButton("UpldMD", strong("Charger Métadonnées"), class = "btn-primary"),
                                fileInput("StatDatafile","Sélectionner le fichier des données quotidiennes", multiple = FALSE), 
                           ##     actionButton("uploadDD", strong("Charger données quotidiennes"), class = "btn-primary"),
                         #      p("Cliquer sur le boutton pour visualier les données dans l'onglet 'Données'. 
                         #      Ceci peut prendre un moment selon la taille du fichier."),
                                fileInput("MLYDatafile","Sélectionner le fichier des données mensuelles", multiple = FALSE), 
                          ##        actionButton("uploadMD", strong("Charger données mensuelles"), class = "btn-primary"),
                                actionButton("s1to2",strong("Etape suivante"), icon = icon("arrow-circle-right"),
                                class = "btn-warning  btn-lg")
                              ),
                               column(width = 7,
                                     h3(strong("Instructions")),
                                     p(strong(" 1. "),"Avant de télécharger le fichier de données quotidiennes, entrez 
                                     l'indicateur de données manquantes s'il est différent de (NA,-99999,vide)"),
                                     p(strong(" 2. "),"Vérifiez le séparateur et le symbole décimal utilisés dans 
                                       les fichiers csv d'entrée. Il existe deux possibilités de format csv : français 
                                       (Sep = point virgule ';' & Dec = virgule ',') et anglais (Sep = virgule ',' & 
                                       Dec = point '.')."),
                                     p(strong(" 3. "),"Charger le fichier des métadonnées. Ce fichier ", strong("doit"),
                                     " utiliser le format et l'en-tête suivants: "),
                                     p(strong("StCode,WMOid,WIGOSid,Latitude,Longitude,Elevation,StName,Country")),
                                     p('"168",06380,0-20000-0-06380,50.9053,5.7619,114.3,"MAASTRICHT",Nederlands'),
                                     p(" où :",strong(" StCode :"),"Identifiant de la station (WMO/WIGOS/Identifiant national). Il doit être le même dans le fichier 
                                       des données quotidiennes.",strong(" WMOid :"),"Indicatif OMM de la station.",strong(" WIGOSid :"),
                                       " Indicatif WIGOS de la station (s'il existe).",strong(" Latitude :"),"Latitude de la station 
                                       en degrés avec quatre décimals (entre -90 et 90).",strong(" Longitude :"),
                                       "Longitude de la station en degrés avec quatre décimals (entre -180 et 180).",
                                       strong(" Elevation :")," Altitude de la station en mètres.",
                                       strong(" StName :"),"Nom de la station.",
                                       strong(" Country :")," Nom du pays Membre."),
                                     p(strong(" 4. "),"Charger le fichier des données quotidiennes. Ce fichier",
                                      strong("doit"),"utiliser le format et l'en-tête suivants: "),
                                     p(strong("Stcode,date,RR,TX,TN,TM,SLP,WVP,SH,WG,WS,SD,TH")),
                                     p("168,1961-01-01,0,6.8,2.3,4.7,1016.5,7.598521,3.2,19.5,6.7,0,1"),
                                     p(" où :",strong("Stcode :")," Identifiant de la station",
                                     strong("date :")," date en format YYYY-MM-DD.",
                                     strong("RR :")," précipitation quotidienne (mm).",
                                     strong("TX :")," température maximale quotidienne (°C).",
                                     strong("TN :")," température minimale quotidienne (°C).",
                                     strong("TM :")," température moyenne quotidienne (°C).",
                                     strong("SLP :")," pression au niveau de la mer moyenne quotidienne (hPa).",
                                     strong("WVP :")," tension de vapeur moyenne quotidienne (hPa).",
                                     strong("SH :")," insolation quotidienne totale (hours).",
                                     strong("WG :")," rafale de vent quotidienne (m/s).",
                                     strong("WS :")," vitesse maximale quotidienne du vent à 10m (m/s).",
                                     strong("SD :")," Epaisseur de neige (cm).",
                                     strong("TH :")," Journée avec ou sans orage (=1 avec orage and 0 sinon)."
                              )
                               )
                     ), # end tabPanel Load Daily and Metadata files 
                    #
                    #   Tab : Data Quality Control
                    #
                    tabPanel(title = strong("2. Contrôle de Qualité"),
                        value = "QCData",
                        fluidRow(title=" row 1",
                        column( width = 3,
                          fileInput("QCconfig","Charger le fichier de configuration", multiple = FALSE),
                          actionButton("UpQC", strong("Charger configuration du QC"), class = "btn-primary"),
                          p("Ce fichier", strong("doit")," utiliser le format et l'en-tête du modèle  
                                       suivant."),
                          downloadButton('QCficdownload', strong('Télécharger le modèle de fichier de QC'),
                                         class = 'btn-warning'),
                          hr(),
                          actionButton("s1bto2",strong("Etape suivante"), icon = icon("arrow-circle-right"),
                                       class = "btn-warning  btn-lg")
                      ),
                      column( width = 5, 
                          h4(strong("Paramètres du Contrôle de Qualité"), align = "center"),
                          DT::dataTableOutput("QCsettings")
                      ),     
                      column(width = 4,
                         h4(strong("Instructions"), align = "center"),
                         p("Les procédures de Contrôle de qualité contient les tests suivants:"),
                         p(strong("1. Tests des formes de présentation:")," répétitions d'observation; des
                          dates impossibles, etc."),
                         p(strong("2. Tests de cohérence interne:")," Cohérence entre températures maximale et 
                         minimale."),
                         p(strong("3. Tests de cohérence temporelle:")," elle vérifie la variation d'un élément 
                         dans le temps. On vise à détecter ici les sauts anormaux à l'échelle quotidienne."),
                         p(strong("4. Tests de dispersion:")," Ces vérifications établissent des limites
                          supérieures et inférieures pour les valeurs possibles d'un élément climatologique.")
                      )),
         h4(strong("Les sorties du contrôle de qualité"), align = "center"), 
         DT::dataTableOutput("QCoutput")
), # end tabPanel Load Daily and Metadata files  

                     #
                     #  Tab : Settings of Data Completeness Criteria
                     #
                     tabPanel(title = strong("3. Critères d'exhaustivité"),
                              value = "DataGaps",
                            column( width = 6, 
                                    h4(strong("Options d'exhaustivité des données selon les directives de l'OMM :")),
                                    h4(strong("* Pour le calcul des valeurs mensuelles individuelles")),
                                    checkboxInput("IndivMiss", p(strong("Nombre minimal "),"de valeurs quotdiennes manquantes ",
                                    strong("non autorisés par mois")), 
                                                  TRUE, width = 500),
                                    numericInput("IndivMissThres",NULL,11, width = 500),
                                    checkboxInput("ConsMiss", p(strong("Nombre minimal "),"de valeurs quotidiennes manquantes et 
                                    consécutives ",strong("non autorisés par mois")), TRUE, width = 500),
                                    numericInput("ConsMissThres",NULL,5, width = 500),
                                    p("Cochez l'option souhaitée pour effectuer une vérification de les critères d'exhaustivité 
                                      des données selon les directives de l'OMM. Sinon, le jeu de données initial sera utilisé tel quel."),
                                    h4(strong("* Pour le calcul des normales mensuelles ")),
                                    numericInput("YLYMissThres","Pourcentage maximal des années manquantes 
                                    au cours de la période de référence",20, width = 500),
                            actionButton("s2to3",strong("Etape suivante"), icon = icon("arrow-circle-right"),
                                         class = "btn-warning  btn-lg")
                                    ),
                            column(width = 6,
                                   h3(strong("Instructions")),
                                   p("Pour les options des valeurs manquantes selon les directives de l'OMM, l'utilisateur peut 
                                     choisir l'une des deux options ou les deux."),
                                   p('Selon le guide des pratiques climatologiques (WMO, 2011), il est  
                                    recommandé que,',strong(' pour le calcul des valeurs mensuelles individuelles'), 
                                    ", lorsqu'une valeur mensuelle est la moyenne des valeurs quotidiennes de ce mois,", strong('elle ne 
                                    doit pas être calculée'), "si l'un des critères suivants est satisfait:"),
                                  p(strong("– Les données d'observation sont manquantes pour au moins 11 jours du mois en question;")),
                                  p(strong("– Les données d'observation sont manquantes pour une période de 5 jours consécutifs
                                  durant le mois en question.")),
                                  p("Par ailleurs, le Guide des pratiques climatologiques (OMM, 2011) recommande que, 
                                  pour qu'",strong('une normale ou moyenne')," soit calculée pour un mois donné, ", 
                                  strong('les données soient disponibles pour au moins 80 % des années de la période de référence.'), 
                                  "Cela équivaut à avoir des données disponibles pour ce mois dans 24 ou plus des 30 années 
                                  pour une normale climatologique standard ou une normale de référence.")
                            )
                      ), # end tabPanel DataGaps
                     #
                     #  Setting of the Parameters Output
                     #
                     tabPanel(title = strong("4. Paramètres de sortie"),
                              value = "VVarCalc",
                              column( width = 4, 
                                      checkboxInput("all", 'Selectionner Tous/Aucun', value = TRUE),
                                      checkboxInput("allmain", 'Selectionner les principaux paramètres', value = FALSE),
                                      checkboxGroupInput("Precipitation", "Précipitation",
                                                         choices = RRchoices, 
                                                         width = '100%',
                                                         selected = RRchoices),
                                      checkboxGroupInput("MeanTemperature", "Température moyenne",
                                                         choices = TMchoices, 
                                                         width = '100%',
                                                         selected = TMchoices),
                                     align="left",
                                     actionButton("s3to4",strong("Etape suivante"), icon = icon("arrow-circle-right"),
                                                  class = "btn-warning  btn-lg")
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MaximumTemperature", "Température maximale",
                                                         choices = TXchoices, 
                                                         width = '100%',
                                                         selected = TXchoices),
                                      checkboxGroupInput("MinimumTemperature", "Température minimale",
                                                         choices = TNchoices, 
                                                         width = '100%',
                                                         selected = TNchoices),
                                      checkboxGroupInput("MeanSeaLevelPressure", "Pression au niveau de la mer",
                                                         choices = SLPchoices, 
                                                         width = '100%',
                                                         selected = SLPchoices),
                                      
                                      align="left"
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MeanVapourPressure", "Tension de vapeur moyenne",
                                                         choices = WVPchoices, 
                                                         width = '100%',
                                                         selected = WVPchoices),
                                      
                                      checkboxGroupInput("HoursSunshine", "Durée d'insolation",
                                                         choices = SHchoices, 
                                                         width = '100%',
                                                         selected = SHchoices),
                                      checkboxGroupInput("SnowDepth", "Epaisseur de neige",
                                                         choices = SDchoices, 
                                                         width = '100%',
                                                         selected = SDchoices),
                                      checkboxGroupInput("WindSpeed", "Vitesse du vent",
                                                         choices = WSchoices, 
                                                         width = '100%',
                                                         selected = WSchoices),
                                      checkboxGroupInput("WindGusts", "Rafales",
                                                         choices = WGchoices, 
                                                         width = '100%',
                                                         selected = WGchoices),
                                      checkboxGroupInput("DaysWithThunder", "Jours avec orage",
                                                         choices = THchoices, 
                                                         width = '100%',
                                                         selected = THchoices),
                                      align="left"
                              )
                     ), # end tabPanel ParamOutput
                     #
                     #  WMO CLINO Calculation
                     #  
                     tabPanel(title = strong("5. Calcul des CLINO"),
                              value = "VClinooutput",
                              fluidRow(
                                    column(8, offset = 2,
                                      h4(strong("Calcul des CLINO")),
                                      p("Concernant les fichiers de sortie, il est à noter que les fichiers *.csv écrits 
                                        par CLINO() contenant les valeurs normales doivent être révisés pour vérifier 
                                        d'éventuelles erreurs dans les données ou dans le fichier de métadonnées."),
                                      actionButton("do", strong("Calculer les normales OMM"), class = "btn-primary"),
                                      p("Cliquer sur le bouton pour calculer les normales climatiques.")
                                    )
                              ),
                              fluidRow(
                                    column(8, offset = 2,
                                      h3(strong("Sortie de la fonction CLINO:")),
                                      actionButton("clinoview", strong("Afficher les normales OMM"), class = "btn-primary"),
                                      downloadButton('zipdownload', strong('Télécharger les fichiers csv'), class = 'btn-warning'),
                                      verbatimTextOutput("clino"),align="left")
                              )
                     ), # end tabPanel ComputeClino
                    #
                    #  View CLINO outputs
                    #
                    tabPanel(strong("Affiche CLINO"), value = "VCLINOFILES", 
                             downloadButton('zipdownloadbis', strong('Télécharger les fichiers csv'), 
                                            class = 'btn-warning'),
                             DT::dataTableOutput("clinofile")
                    ), # end tabPanel Summary Daily Data
                    #
                    #  View MeataData
                    #
                    tabPanel(title = strong("Métadonnées"), value = "TMetadata", tags$b(h4(textOutput("InfoMsg"))),
                              DT::dataTableOutput("metadatatable"),
                              actionButton("backto1",strong("Vers Etape 1"), icon = icon("arrow-circle-left"),
                              class = "btn-warning  btn-lg")
                     ), # end tabPanel MetaData
                    #
                    #  View Daily Data 
                    #
                    tabPanel(strong("Les Données"), value = "VDatasets" , tags$b(h4(textOutput("InfoMsgDLYdata"))),
                              DT::dataTableOutput("table"), DT::dataTableOutput("mlytable"),
                              actionButton("backto2",strong("Vers Etape 1"), icon = icon("arrow-circle-left"),
                              class = "btn-warning  btn-lg")
                    )#, # end tabPanel Summary Daily Data
               ) # end tabsetpanel  
            )#, # end tabitem clino
            ) # end tabItems
        ) # end dashboardBody
) # end dashboard page

###################################################################################
#     Server
###################################################################################

server <- function(input, output, session)
{
  
  # README file to display for the user of the shiny application
  output$About <- renderPrint({
    cat("Le programme CLINO R pour le calcul des Normales Standard Climatologiques et les fichiers de sortie CSV, a été développé par Jose A. Guijarro (septembre 2021),
sous la licence GPL 3 ou supérieure. Agence météorologique d'État (AEMET), Bureau des îles Baléares, Espagne. Membre de l'équipe d'experts de l'OMM sur les données
requises pour les services climatologiques (ET-DRC). 

L'application Shiny basée sur le programme CLINO a été développée par Driss Bari (septembre 2021), Centre National du Climat, Direction générale de la météorologie (DGM),
Casablanca, Maroc. Courriel : bari.driss@gmail.com \n")
cat("-----------------\n")
cat("Cette application a besoin de deux fichiers d'entrée (au format CSV, avec en-tête prédéfini):\n")
cat("1. Un fichier des métadonnées avec l'en-tête suivant :\n")
cat("                          StCode;WMOid;WIGOSid;Latitude;Longitude;Elevation;StName;Country\n")
cat("2. Un fichier de données quotidiennes avec l'en-tête suivant :\n")
cat("                          Stcode;date;RR;TX;TN;TM;SLP;WVP;SH;WG;WS;SD;TH
          - Stcode : Identifiant de la station ( WMO/WIGOS/Indentifiant national)
          - date : date au format YYYY-MM-DD
          - RR : précipitation quotidienne (mm)
          - TX : température maximale quotidienne (°C)
          - TN : température minimale quotidienne (°C)
          - TM : température moyenne quotidienne (°C)
          - SLP : pression au niveau de la mer moyenne quotidienne (hPa)
          - WVP : tension de vapeur moyenne quotidienne (hPa)
          - SH : durée d'insolation totale quotidienne (hours)
          - WG : rafale du vent quotidienne (m/s)
          - WS : vitesse maximale quotidienne du vent à 10m (m/s)
          - SD : épaisseur de neige (cm)
          - TH : journée avec ou sans orage (=1 avec orage et 0 sans)
          \n")
cat("3. Dans le cas où vous ne disposez pas de données quotidiennes pour un paramétre donné, un fichier de données mensuelles avec l'en-tête suivant :\n")
cat("                          Stcode;MM;YYYY;RR;DRR;TX;TN;TM;SLP;WVP;SH;TH
          - Stcode : Identifiant de la station ( WMO/WIGOS/Indentifiant national)
          - MM : Mois au format MM
          - YYYY : Année au format YYYY
          - RR : précipitation mensuelle (mm)
          - DRR : Nombre de jours avec précipitations >= 1mm
          - TX : température maximale moyenne mensuelle (°C)
          - TN : température minimale moyenne mensuelle(°C)
          - TM : température moyenne mensuelle (°C)
          - SLP : pression au niveau de la mer moyenne mensuelle (hPa)
          - WVP : tension de vapeur moyenne mensuelle (hPa)
          - SH : durée d'insolation totale mensuelle (hours)
          - TH : Nombre de jours avec orage 
          \n")
    cat("N.B.: Il convient de noter que ces fichiers contiennent des colonnes prédéfinies et dans un ordre bien précis à respecter. \n")
    cat("-----------------\n")
    cat("Pour les options des valeurs manquantes selon les directives de l'OMM, l'utilisateur peut choisir l'une des deux options ou les deux.\n")
cat("\n Selon le guide des pratiques climatologiques (WMO, 2011), il est recommandé que, pour le calcul des valeurs mensuelles individuelles, lorsqu'une valeur mensuelle 
est la moyenne des valeurs quotidiennes de ce mois, elle ne doit pas être calculée, si l'un des critères suivants est satisfait:
            – Les données d'observation sont manquantes pour au moins 11 jours du mois en question;
            – Les données d'observation sont manquantes pour une période de 5 jours consécutifs durant le mois en question.\n")
cat(" Par ailleurs, le Guide des pratiques climatologiques (OMM, 2011) recommande que, pour qu'une normale ou moyenne soit calculée pour un mois donné, les données 
soient disponibles pour au moins 80 % des années de la période de référence. Cela équivaut à avoir des données disponibles pour ce mois dans 24 ou plus des 30 années 
pour une normale  climatologique standard ou une normale de référence.\n")
    cat("-----------------\n")
cat("Concernant les fichiers de sortie, il convient de noter que les fichiers *.csv écrits par CLINO() contenant les valeurs normales doivent être révisés pour 
vérifier d'éventuelles erreurs dans les données ou dans le fichier des métadonnées.\n")
  }) # end output$About
  
  output$About1 <- renderPrint({
cat("De nouvelles normales climatologiquesstandard devraient être calculées sur la période de trente ans 1991-2020 répondant à l'appel de l'Organisation Météorologique
Mondiale (OMM). Pour faciliter cette tâche, cette application a été développée sous R dans la plateforme Shiny pour calculer les valeurs des normales climatologiques
et les écrire dans des fichiers CSV au format de livraison spécifié par l'OMM. Cette application nécessite deux fichiers d'entrée (un fichier de métadonnées
et un fichier de données quotidiennes). Un modèle pour ces fichiers peut être téléchargé en cliquant sur les boutons ci-dessous.\n")
  }) # end output$About1
 
  # Les boutons de déplacement d'une étape à une autre
  observeEvent(input$s1to2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "QCData")
  })
  
  observeEvent(input$s1bto2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "DataGaps")
  })
  observeEvent(input$s2to3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VVarCalc")
  })
  
  observeEvent(input$s3to4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VClinooutput")
  })
  
  observeEvent(input$backto1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "LoadData")
  })
  
  observeEvent(input$backto2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "LoadData")
  })  
  
  #
  #  Select the principal climatological surface parameters
  #
  observeEvent(input$allmain, {
    updateCheckboxGroupInput(
      session, 'Precipitation', choices = RRchoices,
      selected = if (input$allmain) RRchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'MaximumTemperature', choices = TXchoices,
      selected = if (input$allmain) TXchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'MinimumTemperature', choices = TNchoices,
      selected = if (input$allmain) TNchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'MeanTemperature', choices = TMchoices,
      selected = if (input$allmain) TMchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'MeanSeaLevelPressure', choices = SLPchoices,
      selected = if (input$allmain) SLPchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'MeanVapourPressure', choices = WVPchoices,
      selected = if (input$allmain) WVPchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'HoursSunshine', choices = SHchoices,
      selected = if (input$allmain) SHchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'SnowDepth', choices = SDchoices,
      selected = if (input$allmain) SDchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'WindSpeed', choices = WSchoices,
      selected = if (input$allmain) WSchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'WindGusts', choices = WGchoices,
      selected = if (input$allmain) WGchoicesm
    ) 
    updateCheckboxGroupInput(
      session, 'DaysWithThunder', choices = THchoices,
      selected = if (input$allmain) THchoicesm
    ) 
    
  })
  
  #
  #  Select All/None
  #
  observeEvent(input$all, {
    updateCheckboxGroupInput(
      session, 'Precipitation', choices = RRchoices,
      selected = if (input$all) RRchoices
    ) 
    updateCheckboxGroupInput(
      session, 'MaximumTemperature', choices = TXchoices,
      selected = if (input$all) TXchoices
    ) 
    updateCheckboxGroupInput(
      session, 'MinimumTemperature', choices = TNchoices,
      selected = if (input$all) TNchoices
    ) 
    updateCheckboxGroupInput(
      session, 'MeanTemperature', choices = TMchoices,
      selected = if (input$all) TMchoices
    ) 
    updateCheckboxGroupInput(
      session, 'MeanSeaLevelPressure', choices = SLPchoices,
      selected = if (input$all) SLPchoices
    ) 
    updateCheckboxGroupInput(
      session, 'MeanVapourPressure', choices = WVPchoices,
      selected = if (input$all) WVPchoices
    ) 
    updateCheckboxGroupInput(
      session, 'HoursSunshine', choices = SHchoices,
      selected = if (input$all) SHchoices
    ) 
    updateCheckboxGroupInput(
      session, 'SnowDepth', choices = SDchoices,
      selected = if (input$all) SDchoices
    ) 
    updateCheckboxGroupInput(
      session, 'WindSpeed', choices = WSchoices,
      selected = if (input$all) WSchoices
    ) 
    updateCheckboxGroupInput(
      session, 'WindGusts', choices = WGchoices,
      selected = if (input$all) WGchoices
    ) 
    updateCheckboxGroupInput(
      session, 'DaysWithThunder', choices = THchoices,
      selected = if (input$all) THchoices
    ) 
  })
  
  # control the checkboxinput of the CSV file format (English /French)
  observe({
    updateCheckboxInput(session, "FRCSV", value = !(input$ENCSV))
  })
  observe({
    updateCheckboxInput(session, "ENCSV", value = !(input$FRCSV))
  })
  
 
  # control the checkboxinput of the surface parameters choice
  observe({
    if (input$allmain)
    { updateCheckboxInput(session, "all", value = !(input$allmain)) }
  })
  
  observe({
    if (input$all)
    { updateCheckboxInput(session, "allmain", value = !(input$all)) }
  })
  

  
  #
  # a reactive fucntion to create the CLINO_variables.csv file
  #
  Create_Clino_Variables_file <- reactive({
    CV <- file("CLINO_variables.csv",'w')
    write("Variable,Daily_data_file,Parameters",CV)
    write(paste0("Precipitation,RR_DLY.csv,",paste(input$Precipitation, collapse = "-")),CV)
    write(paste0("Maximum_temperature,TX_DLY.csv,",paste(input$MaximumTemperature, collapse = "-")),CV)
    write(paste0("Minimum_temperature,TN_DLY.csv,",paste(input$MinimumTemperature, collapse = "-")),CV)
    write(paste0("Mean_temperature,TM_DLY.csv,",paste(input$MeanTemperature, collapse = "-")),CV)
    write(paste0("Sea_level_pressure,PP_DLY.csv,",paste(input$MeanSeaLevelPressure, collapse = "-")),CV)
    write(paste0("Mean_Vapor_Pressure,VP_DLY.csv,",paste(input$MeanVapourPressure, collapse = "-")),CV)
    write(paste0("Hours_of_Sunshine,SS_DLY.csv,",paste(input$HoursSunshine, collapse = "-")),CV)
    write(paste0("Snow_depth,SD_DLY.csv,",paste(input$SnowDepth, collapse = "-")),CV)
    write(paste0("Wind_speed,WS_DLY.csv,",paste(input$WindSpeed, collapse = "-")),CV)
    write(paste0("Wind_gusts,WG_DLY.csv,",paste(input$WindGusts, collapse = "-")),CV)
    write(paste0("Days_with_Thunder,TH_DLY.csv,",paste(input$DaysWithThunder, collapse = "-")),CV)
    close(CV)
  })
  
  
  #
  #  Generate the DLY files per variable
  #
  DLY.Files.Generate <- function(varname)
  {
    if ( !is.null(mydata()))
    {
      mydata1=subset(mydata(), mydata()$Year >= startavgperiod & mydata()$Year <= endavgperiod)
      min.date=min(mydata1$date)
      max.date=max(mydata1$date)
      cpms=unique(mydata1$Stcode)
      final.df=vector()
      dates=seq.Date(as.Date(min.date), as.Date(max.date),by='day')
      dates=as.character(dates)
      final.df=cbind(final.df, dates)
      nbcpm=length(cpms)
      for (i in 1:nbcpm)
      {
        subdcpm=subset(mydata1, mydata1$Stcode == cpms[i])
        df=subdcpm[,c("date",varname)]
        names(df)=c("date",as.character(cpms[i]))
        comp.df = df %>%
          mutate(date = as.Date(date)) %>%
          complete(date = seq.Date(as.Date(min.date), as.Date(max.date), by="day"))
        final.df=cbind(final.df,comp.df[,as.character(cpms[i])])
      }
      return(final.df)
    }
  }
  
  #
  #  a reactive function to write DLY files in the required format by CLINO() program
  #   
  write.dly.files <- reactive({
    write.table(DLY.Files.Generate("TX"),"TX_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("TN"),"TN_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("TM"),"TM_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("RR"),"RR_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("SLP"),"PP_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("WVP"),"VP_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("SH"),"SS_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("WG"),"WG_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("WS"),"WS_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("SD"),"SD_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
    write.table(DLY.Files.Generate("TH"),"TH_DLY.csv", quote = FALSE, sep = ",", dec =".", row.names = FALSE)
  })  


  #############################################################################################################
  # read initial data
  #############################################################################################################
  
    #
    #    **********   MetaData File
    #
  #  metadata <- eventReactive(input$UpldMD ,{
      metadata <- reactive({  
      inFile1 <- input$MetaDatafile
      if (is.null(inFile1)) return(NULL)
      if ( input$FRCSV) 
      {
        fsep=";"
        fdec=","
        metdini <- read.csv(inFile1$datapath, header=TRUE, sep=fsep,  dec = fdec, as.is = TRUE)
        write.csv(metdini, "CLINO_stations.csv", sep = ",", dec = ".",row.names = FALSE)
      } else {
        fsep=","
        fdec="."
        file.rename(inFile1$datapath, "CLINO_stations.csv")
      }
      medata <- read.csv("CLINO_stations.csv", header=TRUE, sep=",",  dec = ".", as.is = TRUE)      
      return(medata)
    })
    
  # By clicking on the button (Upload MetaData), the equivalent Tab is selected
  #observeEvent(input$UpldMD, {
  #  updateTabsetPanel(session, "inTabset",
  #                    selected = "TMetadata")
  #})   
  
  #
  #    **********   Info Message about the MetaData File by checking its format
  #
  output$InfoMsg <- renderText({
    if(is.null(input$MetaDatafile)){return()}
    nb.col=dim(metadata())[[2]]
    if ( nb.col != 8)
    {
      paste0("Le fichier que vous avez sélectionné ne contient pas 8 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
    } else {
      if (identical(colnames(metadata()), c("StCode","WMOid","WIGOSid","Latitude","Longitude",
                                            "Elevation","StName","Country")) )
      {
        paste0("")
      } else {
        paste0("Le fichier que vous avez sélectionné contient 8 colonnes, mais les noms des colonnnes  ou leur ordre  
              ne respectent pas le format requis et exigée par l'application ... Vérifiez et ré-essayez à nouveau !!! 
              Lisez attentivement le manuel d'utilisation ou A PROPOS ")
      }
    }
  })
  
  #
  #    **********   Daily Data File
  #
  #mydata <- eventReactive(input$uploadDD, {
    mydata <- reactive({  
    inFile <- input$StatDatafile
    if (is.null(inFile))
      return(NULL)
    missind=c(NA,"",-99999,input$MissID)
    if ( input$FRCSV) 
    {
      fsep=";"
      fdec=","
    } else {
      fsep=","
      fdec="."
    }
    data0 <- read.csv(inFile$datapath, header=TRUE, sep=fsep,  dec = fdec, 
                      na.string=missind)
    debperiod=min(as.Date('1991-01-01'), min(as.Date(data0$date)))
    finperiod=max(as.Date('2020-12-31'), max(as.Date(data0$date)))
    data <- data.completness(data0,debperiod,finperiod,input$ConsMiss,input$ConsMissThres,
                             input$IndivMiss,input$IndivMissThres)
    if ( dim(data)[[2]] != 13)
    {
      return(NULL)
    } else {
      colnames(data)=c("Stcode","date","RR","TX","TN","TM","SLP","WVP","SH","WG","WS","SD","TH")
      data$Stcode=as.character(data$Stcode)
      data$date=as.character(data$date)
      data$Year=as.numeric(substr(data$date,1,4))
      return(data)
    }
  })
  
  #
  #    **********   Info message about the Daily Data File
  #
  output$InfoMsgDLYdata <- renderText({
    if(is.null(input$StatDatafile)){return()}
    if ( is.null(mydata()))
    {
      paste0("Le fichier que vous avez sélectionné ne contient pas 13 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
    } else {
      nb.col=dim(mydata())[[2]]
      if ( nb.col != 14)
      {
        paste0("Le fichier que vous avez sélectionné ne contient pas 8 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
      } else {
        if (identical(colnames(mydata()), c("Stcode","date","RR","TX","TN",
                                            "TM","SLP","WVP","SH","WG","WS","SD","TH","Year")) )
        {
          paste0("")
        } else {
          paste0("Le fichier que vous avez sélectionné contient 13 colonnes, mais les noms des colonnnes  ou leur ordre  
              ne respectent pas le format requis et exigée par l'application ... Vérifiez et ré-essayez à nouveau !!! 
              Lisez attentivement le manuel d'utilisation ou A PROPOS ")
        }
      }
    }
  })
  
  
  #
  #    **********   Monthly Data File
  #
  #mlydata <- eventReactive(input$uploadMD, {
    mlydata <- reactive({  
    inFile <- input$MLYDatafile
    if (is.null(inFile))
      return(NULL)
    missind=c(NA,"",-99999,input$MissID)
    if ( input$FRCSV) 
    {
      fsep=";"
      fdec=","
    } else {
      fsep=","
      fdec="."
    }
    data <- read.csv(inFile$datapath, header=TRUE, sep=fsep,  dec = fdec, 
                      na.string=missind)
    if ( dim(data)[[2]] != 12)
    {
      return(NULL)
    } else {
      colnames(data)=c("Stcode","MM","YYYY","RR","DRR","TX","TN","TM","SLP","WVP","SH","TH")
      data$Stcode=as.character(data$Stcode)
      return(data)
    }
  })
  
  #
  #    **********   Info message about the Monthly Data File
  #
  output$InfoMsgMLYdata <- renderText({
    if(is.null(input$MLYDatafile)){return()}
    if ( is.null(mlydata()))
    {
      paste0("Le fichier que vous avez sélectionné ne contient pas 12 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
    } else {
      nb.col=dim(mlydata())[[2]]
      if ( nb.col != 12)
      {
        paste0("Le fichier que vous avez sélectionné ne contient pas 12 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
      } else {
        if (identical(colnames(mlydata()), c("Stcode","MM","YYYY","RR","DRR","TX","TN","TM","SLP","WVP","SH","TH")) )
        {
          paste0("")
        } else {
          paste0("Le fichier que vous avez sélectionné contient 12 colonnes, mais les noms des colonnnes  ou leur ordre  
              ne respectent pas le format requis et exigée par l'application ... Vérifiez et ré-essayez à nouveau !!! 
              Lisez attentivement le manuel d'utilisation ou A PROPOS ")
        }
      }
    }
  })
  
  #############################################################################################################
  # Generate the output for the user interface
  #############################################################################################################
  #
  #    >>>>>>>  Plot as Table the MetaData
  #
  options(DT.options = list(pageLength = 4))
  
  QC.config <- eventReactive(input$UpQC ,{
    inFile1 <- input$QCconfig
    if (is.null(inFile1)) return(NULL)
    if ( input$FRCSV) 
    {
      fsep=";"
      fdec=","
      QCsetup <- read.csv(inFile1$datapath, header=TRUE, sep=fsep,  dec = fdec, as.is = TRUE)
    } else {
      fsep=","
      fdec="."
      QCsetup <- read.csv(inFile1$datapath, header=TRUE, sep=fsep,  dec = fdec, as.is = TRUE)
    }
    return(QCsetup)
  })
  
  output$QCoutput <- DT::renderDataTable({   
    if(is.null(input$StatDatafile)){return()}
    data.quality.control(mydata(),QC.config())
  }) # end output$QCoutput
  
  output$QCsettings <- DT::renderDataTable({  
    DT::datatable(QC.config())
  }) # end output$QCsettings
  
  output$metadatatable <- DT::renderDataTable({   
    if(is.null(input$MetaDatafile)){return()}
    nb.col=dim(metadata())[[2]]
    if ( nb.col == 8)
    {
      if (identical(colnames(metadata()), c("StCode","WMOid","WIGOSid","Latitude","Longitude",
                                            "Elevation","StName","Country")) )
      {
        metadata()
      } else {
        return()
      }
    } else {
      return()
    }
  }) # end output$metadatatable
  
  #
  #    >>>>>>>  Action button to perform WMO normals calculations
  #
  observeEvent(input$do, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VClinooutput")
    output$clino <- renderPrint({ 
     show_modal_spinner(
        spin = "double-bounce",
        color = "#112446",
        text = "Patientez un moment, le calcul des normales climatiques est en cours ... "
      ) # show the modal window
      
 #     if(is.null(input$StatDatafile)) {return()}
      if ( !is.null(mydata()))
      {
            Create_Clino_Variables_file()
          write.dly.files()
          CLINO(period=avgperiod, YLYMissThresIn = input$YLYMissThres, mlydata())
      } else {
        if ( !is.null(mlydata()) ) 
        {
          Create_Clino_Variables_file()
          MCLINO(period=avgperiod, YLYMissThresIn = input$YLYMissThres, mlydata())
        } else { return(NULL) }
      }
      remove_modal_spinner() # remove it when done
    }) # end output$clino
  })

  #
  #    >>>>>>>  Action button to view daily data file in the Datasets tab
  #
#  observeEvent(input$uploadDD, {
#    updateTabsetPanel(session, "inTabset",
#                      selected = "VDatasets")
#  })  
  output$table <- DT::renderDataTable({ 
      if(is.null(input$StatDatafile)){return(NULL)}
      if ( !is.null(mydata()))
      {
        nb.col=dim(mydata())[[2]]
        if ( nb.col == 14)
        {
          if (identical(colnames(mydata()), c("Stcode","date","RR","TX","TN",
                                              "TM","SLP","WVP","SH","WG","WS","SD","TH","Year")) )
          {
            mydata()
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      } else {return(NULL) }
    }) # end output$table

  #
  #    >>>>>>>  Action button to view monthly data file in the Datasets tab
  #
  #observeEvent(input$uploadMD, {
  #  updateTabsetPanel(session, "inTabset",
  #                    selected = "VDatasets")
  #})  
  
  output$mlytable <- DT::renderDataTable({ 
    if(is.null(input$MLYDatafile)){return(NULL)}
    if ( !is.null(mlydata()))
    {
      nb.col=dim(mlydata())[[2]]
      if ( nb.col == 12)
      {
        if (identical(colnames(mlydata()), c("Stcode","MM","YYYY","RR","DRR","TX","TN","TM","SLP","WVP","SH","TH")) )
        {
          mlydata()
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else {return(NULL) }
  }) # end output$table
  
  #############################################################################################################
  # This reactive output contains the dataset and display the dataset in table format
  #############################################################################################################
  
  output$zipdownload <- downloadHandler(
    filename = function(){
      paste0("Created-CLINO-Files-",Sys.Date(),"-",Sys.time(),".zip")
    },
    content = function(file){
      files <- NULL;
      namfiles=read.table("Names-Created-Files.txt", header = FALSE)
      nblig=dim(namfiles)[[1]]
      #loop through the created files
      for (i in 1:nblig){
        cat("\n file name ... ",as.character(namfiles[i,1]),"\n")
        files <- c(as.character(namfiles[i,1]),files)
      }
      #create the zip file
      zip(file,files)
    }
  ) 
  
  output$zipdownloadbis <- downloadHandler(
    filename = function(){
      paste0("Created-CLINO-Files-",Sys.Date(),"-",Sys.time(),".zip")
    },
    content = function(file){
      files <- NULL;
      namfiles=read.table("Names-Created-Files.txt", header = FALSE)
      nblig=dim(namfiles)[[1]]
      #loop through the created files
      for (i in 1:nblig){
        cat("\n file name ... ",as.character(namfiles[i,1]),"\n")
        files <- c(as.character(namfiles[i,1]),files)
      }
      #create the zip file
      zip(file,files)
    }
  ) 
  #
  #    >>>>>>>  Action button to view daily data file in the Datasets tab
  #
  observeEvent(input$clinoview, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VCLINOFILES")
    
    
    namfiles=read.table("Names-Created-Files.txt", header = FALSE)
    nblig=dim(namfiles)[[1]]
    if(nblig == 0){
      output$clinofile <- DT::renderDataTable(
        return(NULL)
      )
    } else {
      #loop through the created files
      for (i in 1:nblig){
        clino.data <- read.csv(as.character(namfiles[i,1]), header = FALSE,sep = ",", dec = ".", as.is=TRUE);
        output$clinofile <- DT::renderDataTable(clino.data,
                                                extensions = c('Scroller'
                                                ), 
                                                options = list(
                                                  dom = 'Bfrtip',
                                                  deferRender = TRUE,
                                                  scrollY = 400,
                                                  scroller = TRUE)
        )
      }
    }
  })

  output$QCficdownload <- downloadHandler(
    filename = "QC-settings.csv",
    content = function(file){
      file.copy("QC-settings.csv", file)
    }
  )

  output$tempmetadatadownload <- downloadHandler(
    filename = "Template-CLINO-MetaData-file-EN.csv",
    content = function(file){
      file.copy("Template-CLINO-MetaData-file-EN.csv", file)
    }
  )
  
  output$tempdailydatadownload <- downloadHandler(
    filename = "Template-CLINO-Daily-Data-file-EN.csv",
    content = function(file){
      file.copy("Template-CLINO-Daily-Data-file-EN.csv", file)
    }
  )
  
  output$tempmonthlydatadownload <- downloadHandler(
    filename = "Template-CLINO-Monthly-Data-file-EN.csv",
    content = function(file){
      file.copy("Template-CLINO-Monthly-Data-file-EN.csv", file)
    }
  )
  
  output$howtousedownload <- downloadHandler(
    filename = "CLINO_WebApp_FR.pdf",
    content = function(file){
      file.copy("CLINO_WebApp_FR.pdf", file)
    }
  )
  
}# end server


###################################################################################
#     Shiny Application
###################################################################################

shinyApp(ui, server)