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
options(shiny.maxRequestSize = 30*1024^2)
###################################################################################
#     User Interface
###################################################################################

ui <- dashboardPage(
        dashboardHeader(title = "WMO CLINO Calculation / Calcul des CLINO OMM", titleWidth = 550),
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
                    #   Tab : README / A PROPOS
                    #
                    tabPanel(strong("README / A propos"), value = "VReadme",
                             fluidRow(
                               column( width = 8, 
                                       verbatimTextOutput("About1") 
                               ),    
                               column( width = 4, 
                                       h4(strong("Input files Templates / Modèles de fichiers d'entrée :")),
                                       downloadButton('tempmetadatadownload',strong('Metadata file / fichier des métadonnées'),
                                                      class = 'btn-warning'), hr(),
                                       downloadButton('tempdailydatadownload', strong('Daily Data file / fichier des données quotidiennes'),
                                                      class = 'btn-warning'), hr(),
                                       downloadButton('tempmonthlydatadownload', strong('Monthly Data file / fichier des données mensuelles'),
                                                      class = 'btn-warning'), hr(),
                                       downloadButton('howtousedownload', strong("User guide / Manuel d'utilisation"),
                                                      class = 'btn-warning'))
                             ),
                             fluidRow(
                               column( width = 6, 
                                       verbatimTextOutput("About2") 
                               ),
                               column( width = 6, 
                                       verbatimTextOutput("About") 
                               )
                             ),
                              align="left"),  # end TabPanel ReadMe
                     #
                     #   Tab : Load data files / Charger les fichiers de données
                     #
                     tabPanel(title = strong("1. Load Data / Charger les données"),
                              value = "LoadData",
                              column( width = 4, 
                                textInput("MissID", label = "Missing Data Indicator / Indicateur de valeur manquante", 
                                        value = "Enter missing value indicator..."),
                                h5(strong("CSV File Separtor and Decimal symbols / Symboles du séparateur et du décimal dans la fichier CSV")),
                                checkboxInput('ENCSV', 'English (Sep = comma "," & Dec = point ".") / Anglais (Sep = virgule "," & Dec = point ".")', value = TRUE, width = 500),
                                checkboxInput('FRCSV', 'French (Sep = semicolon ";" & Dec = comma ",") / Français (Sep = point-vrigule ";" & Dec = virgule ",")', value = FALSE, width = 500),
                                h3(strong("Instructions")),
                                p(strong(" 1. "),"Before uploading daily data file, enter missing data indicator if not 
                                      (NA,-99999,empty) / Avant de télécharger le fichier de données quotidiennes, entrez 
                                     l'indicateur de données manquantes s'il est différent de (NA,-99999,vide)"),
                                p(strong(" 2. "),"Check the used separator and decimal symbols in the input csv files. 
                                      There are two possibilities of csv format : French (Sep = semicolon ';' & Dec = comma ',')
                                      and English (Sep = comma ',' & Dec = point '.') / Vérifiez le séparateur et le symbole décimal utilisés dans 
                                       les fichiers csv d'entrée. Il existe deux possibilités de format csv : français 
                                       (Sep = point virgule ';' & Dec = virgule ',') et anglais (Sep = virgule ',' & 
                                       Dec = point '.')")
                              ),
                               column(width = 4,
                                      fileInput("StatDatafile","Upload Daily Data file / Sélectionner le fichier des données quotidiennes", multiple = FALSE), 
                                      fileInput("MLYDatafile","Upload Monthly Data file / Sélectionner le fichier des données mensuelles", multiple = FALSE), 
                                     h3(strong("Instructions")),
                                     p(strong(" 3. "),"Load the daily data file for a single station. This file", strong("must"),"use the following format and 
                                       header / Charger le fichier des données quotidiennes pour un seule station. Ce fichier",
                                      strong("doit"),"utiliser le format et l'en-tête suivants"),
                                     p(strong("Stcode,date,RR,TX,TN,TM,SLP,WVP,SH,WG,WS,SD,TH")),
                              p(strong(" 4. "),"Load the monthly data file if any for a single station. This file", strong("must"),"use the following format and 
                                      header / Charger le fichier des données mensuelles s'il y en a pour une seule station. Ce fichier",
                                strong("doit"),"utiliser le format et l'en-tête suivants :"),
                              p(strong("Stcode,MM,YYYY,RR,DRR,TX,TN,TM,SLP,WVP,SH,TH")),
                              p("Please read the user guide or README Tab for more details."
                               )),
                              column( width = 4, 
                                      fileInput("MetaDatafile","Upload the metadata file / Sélectionner le fichier des métadonnées", multiple = FALSE),
                                      actionButton("s1to2",strong("Next Step"), icon = icon("arrow-circle-right"),
                                                   class = "btn-warning  btn-lg"),
                                      h3(strong("Instructions")),
                                      p(strong(" 3. "),"Upload one metadata file for all target stations. This file", strong("must"),"use the following format and 
                                        header / Charger un seul fichier des métadonnées de toutes les stations cibles. Ce fichier ", strong("doit"),
                                        " utiliser le format et l'en-tête suivants:"),
                                      p(strong("StCode,WMOid,WIGOSid,Latitude,Longitude,Elevation,StName,Country")),
                                      p(" Please read the user guide or README Tab for more details about the fields 
                                      included in the metadata file / Consulter le guide d'utilisateur ou 'A propos' pour plus de détails sur les champs à intégrer dans le 
                                      fichier des métadonnées."
                                      )
                              )
                              
                     ), # end tabPanel Load data files 
                     #
                     #  Setting of the Parameters Output
                     #
                     tabPanel(title = strong("2. Parameters Output / Paramètres de sortie"),
                              value = "VVarCalc",
                              column( width = 4, 
                                      checkboxInput("all", 'Select All/None', value = FALSE),
                                      checkboxInput("allmain", 'Select The principal parameters', value = FALSE),
                                      checkboxGroupInput("Precipitation", "Precipitation / Précipitation",
                                                         choices = RRchoices, 
                                                         width = '100%',
                                                         selected = RRchoicesm),
                                      checkboxGroupInput("MeanTemperature", "Mean Temperature / Température Moyenne",
                                                         choices = TMchoices, 
                                                         width = '100%',
                                                         selected = TMchoicesm),
                                     align="left",
                                     actionButton("s2to3",strong("Next step"), icon = icon("arrow-circle-right"),
                                                  class = "btn-warning  btn-lg")
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MaximumTemperature", "Maximum Temperature / Température Maximale",
                                                         choices = TXchoices, 
                                                         width = '100%',
                                                         selected = TXchoices),
                                      checkboxGroupInput("MinimumTemperature", "Minimum Temperature / Température minimale",
                                                         choices = TNchoices, 
                                                         width = '100%',
                                                         selected = TNchoices),
                                      checkboxGroupInput("MeanSeaLevelPressure", "Mean Sea Level Pressure / Pression au niveau de la mer",
                                                         choices = SLPchoices, 
                                                         width = '100%',
                                                         selected = SLPchoices),
                                      
                                      align="left"
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MeanVapourPressure", "Mean Vapour Pressure / Tension de vapeur moyenne",
                                                         choices = WVPchoices, 
                                                         width = '100%',
                                                         selected = WVPchoices),
                                      
                                      checkboxGroupInput("HoursSunshine", "Hours of Sunshine / Durée d'Insolation",
                                                         choices = SHchoices, 
                                                         width = '100%',
                                                         selected = SHchoices),
                                      checkboxGroupInput("SnowDepth", "Snow Depth / Epaisseur de neige",
                                                         choices = SDchoices, 
                                                         width = '100%',
                                                         selected = SDchoices),
                                      checkboxGroupInput("WindSpeed", "Wind Speed / Vitesse du vent",
                                                         choices = WSchoices, 
                                                         width = '100%',
                                                         selected = WSchoices),
                                      checkboxGroupInput("WindGusts", "Wind Gusts / Rafales du vent",
                                                         choices = WGchoices, 
                                                         width = '100%',
                                                         selected = WGchoices),
                                      checkboxGroupInput("DaysWithThunder", "Days with thunder / Jours avec orage",
                                                         choices = THchoices, 
                                                         width = '100%',
                                                         selected = THchoices),
                                      align="left"
                              )
                     ), # end tabPanel ParamOutput
                     #
                     #  WMO CLINO Calculation and Data Completeness
                     #  
                     tabPanel(title = strong("3. Calcul des CLINO / Compute CLINO"),
                              value = "VClinooutput",
                              fluidRow(
                                column( width = 6, 
                                        h4(strong("Missing data options / Options d'exhaustivité des données:")),
                                        h4(strong("* For individual monthly values calculation / Pour le calcul des valeurs mensuelles individuelles")),
                                        checkboxInput("IndivMiss", p(strong("Minimum "),"number of missing daily data ",
                                                                     strong("not allowed per month")," / ",strong("Nombre minimal "),"de valeurs quotdiennes manquantes ",
                                                                     strong("non autorisés par mois")), 
                                                      TRUE, width = 500),
                                        numericInput("IndivMissThres",NULL,11, width = 500),
                                        checkboxInput("ConsMiss", p(strong("Minimum "),"number of consecutive missing daily 
                                    data",strong("not allowed per month")," / ",strong("Nombre minimal "),"de valeurs quotidiennes manquantes et 
                                    consécutives ",strong("non autorisés par mois")), TRUE, width = 500),
                                        numericInput("ConsMissThres",NULL,5, width = 500),
                                        p("Check the desired option to perform data completeness verification based on the WMO 
                                    standards. Otherwise, the initial dataset will be used as is / Cochez l'option souhaitée pour effectuer une vérification de les critères d'exhaustivité 
                                      des données selon les directives de l'OMM. Sinon, le jeu de données initial sera utilisé tel quel.")
                                ),    
                                column( width = 6, 
                                        h4(strong("Options d'exhaustivité des données / Missing data options :")),
                                        h4(strong("* For monthly normals calculation / Pour le calcul des normales mensuelles")),
                                        numericInput("YLYMissThres","Maximum percentage of missing years in the 
                                    averaging period / Pourcentage maximal des années manquantes 
                                    au cours de la période de référence",20, width = 500),
                                        h4(strong("CLINO Calculation / Calcul des CLINO")),
                                        p("Regarding the output files, it should be noted that the *.csv files written 
                                        by CLINO() containing the normal values should be revised to check for possible
                                        errors in the data or in the metadata file / Concernant les fichiers de sortie, il est à noter que les fichiers *.csv écrits 
                                        par CLINO() contenant les valeurs normales doivent être révisés pour vérifier 
                                        d'éventuelles erreurs dans les données ou dans le fichier de métadonnées."),
                                        actionButton("do", strong("Compute CLINO"), class = "btn-primary"),
                                        p("Click the button to update the WMO normals calculations / Cliquer sur le bouton pour calculer les normales climatiques."),
                                        actionButton("clinoview", strong("View WMO Normals / Afficher les normales OMM"), class = "btn-primary"),
                                        downloadButton('zipdownload', strong('Download zipped csv files / Télécharger les fichiers csv'), class = 'btn-warning'),
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                       h3(strong("Instructions")),
                                       
                                       p("For missing values options with respect to WMO standards, the user can 
                                   choose one or both of the two options."),
                                       p('Following the Guide to Climatological Practices (WMO, 2011), it is  
                                    recommended that,',strong(' for individual monthly values calculation'),'(where 
                                    a monthly value is the mean of that month’s daily values), it should not be calculated if either of 
                                    the following criteria are satisfied:'),
                                       p(strong('– Observations are missing for 11 or more days during the month;')),
                                       p(strong('– Observations are missing for a period of 5 or more consecutive days 
                                    during the month.')),
                                       p('Besides, the Guide to Climatological Practices (WMO, 2011) recommends that, ',
                                         strong('for a normal or average'),' to be calculated for a given month, ',strong('data 
                                    should be available for at least 80% of the years in the averaging period.'),' This equates 
                                    to having data available for that month in 24 or more out of the 30 years 
                                    for a climatological standard normal or a reference normal.'),
                                  #     hr(),
                                       p("----------------------------------------------"),
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
                                                            
                                ),
                                    column(width = 6,
                                      h3(strong("Sortie de la fonction CLINO:")),
                                       verbatimTextOutput("clino"),align="left")
                              )
                     ), # end tabPanel ComputeClino
                    #
                    #  View CLINO outputs
                    #
                    tabPanel(strong("Data View / Affichage"), value = "VCLINOFILES", 
                             downloadButton('zipdownloadbis', strong('Download zipped csv files / Télécharger les fichiers csv'), 
                                            class = 'btn-warning'),
                             h3(strong("WMO CLINO / CLINO OMM :")),
                             DT::dataTableOutput("clinofile"),
                             h3(strong("Metadata / Métadonnées :")),
                             tags$b(h4(textOutput("InfoMsg"))),
                             DT::dataTableOutput("metadatatable"),
                             h3(strong("Daily Data / Données Quotidiennes :")),
                             tags$b(h4(textOutput("InfoMsgDLYdata"))),
                             DT::dataTableOutput("table"), 
                             h3(strong("Monthly Data / Données Mensuelles :")),
                             DT::dataTableOutput("mlytable")
                    ), # end tabPanel Summary Daily Data
                    #
                    #   Tab : Data Quality Control
                    #
                    tabPanel(title = strong("Q.C. / Contrôle de Qualité"),
                             value = "QCData",
                             fluidRow(title=" row 1",
                                      column( width = 3,
                                              fileInput("QCconfig","Load Quality Control settings file / Charger le fichier de configuration", multiple = FALSE),
                                              actionButton("UpQC", strong("Upload QC settings file / Charger configuration du QC"), class = "btn-primary"),
                                              p("This file", strong("must")," use the format and header in the template 
                                       that can can be downloaded here / Ce fichier", strong("doit")," utiliser le format et l'en-tête du modèle  
                                       suivant."),
                                              downloadButton('QCficdownload', strong('Download Template QC settings File'),
                                                             class = 'btn-warning'),
                                      ),
                                      column( width = 5, 
                                              h4(strong("Quality Control Settings / Paramètres du Contrôle de Qualité"), align = "center"),
                                              DT::dataTableOutput("QCsettings")
                                      ),     
                                      column(width = 4,
                                             h4(strong("Instructions"), align = "center"),
                                             p("The QC routines include the following tests:"),
                                             p(strong("1. Format tests:")," Duplicate dates and meteorological elements values."),
                                             p(strong("2. Internal consistency tests:")," Coherence between maximum and minimum temperatures."),
                                             p(strong("3. Temporal consistency tests:")," Consecutive equal values control 
                                   and large jumps within one day."),
                                             p(strong("4. Tolerance tests:")," Out of range values, based on fixed threshold values.")
                                      )),
                             h4(strong("Data Quality Control outputs / Les sorties du contrôle de qualité"), align = "center"), 
                             downloadButton('QCOutficdownload', strong('Download the QC outputs / Télécharger les sorties du QC'),
                                            class = 'btn-warning'), align = "center",
                             DT::dataTableOutput("QCoutput")
                    )#, # end tabPanel Load Daily and Metadata files  
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
cat("Le programme CLINO R pour le calcul des Normales Standard Climatologiques 
et les fichiers de sortie CSV, a été développé par Jose A. Guijarro (septembre 
2021), sous la licence GPL 3 ou supérieure. Agence météorologique d'État 
(AEMET), Bureau des îles Baléares, Espagne. Membre de l'équipe d'experts 
de l'OMM sur les données requises pour les services climatologiques (ET-DRC). 

L'application Shiny basée sur le programme CLINO a été développée par Driss 
Bari (septembre 2021), Centre National du Climat, Direction générale de la 
météorologie (DGM), Casablanca, Maroc. Courriel : bari.driss@gmail.com \n")
cat("-----------------\n")
cat("Cette application a besoin de deux fichiers d'entrée (au format CSV, 
    avec en-tête prédéfini):\n")
cat("1. Un fichier des métadonnées avec l'en-tête suivant :\n")
cat(" StCode,WMOid,WIGOSid,Latitude,Longitude,Elevation,StName,Country
 - StCode    : Identifiant de la station (WMO/WIGOS/Identifiant national). 
               Il doit être le même dans le fichier des données quotidiennes.
 - WMOid     : Indicatif OMM de la station.
 - WIGOSid   : Indicatif WIGOS de la station (s'il existe).
 - Latitude  : Latitude de la station en degrés avec quatre décimals 
               (entre -90 et 90). 
 - Longitude : Longitude de la station en degrés avec quatre décimals 
               (entre -180 et 180).
 - Elevation : Altitude de la station en mètres.
 - StName    : Nom de la station.
 - Country   : Nom du pays Membre.
\n")
cat("2. Un fichier de données quotidiennes avec l'en-tête suivant :\n")
cat("     Stcode,date,RR,TX,TN,TM,SLP,WVP,SH,WG,WS,SD,TH
     - Stcode : Identifiant de la station ( WMO/WIGOS/Indentifiant national)
     - date   : date au format YYYY-MM-DD
     - RR     : précipitation quotidienne (mm)
     - TX     : température maximale quotidienne (°C)
     - TN     : température minimale quotidienne (°C)
     - TM     : température moyenne quotidienne (°C)
     - SLP    : pression au niveau de la mer moyenne quotidienne (hPa)
     - WVP    : tension de vapeur moyenne quotidienne (hPa)
     - SH     : durée d'insolation totale quotidienne (hours)
     - WG     : rafale du vent quotidienne (m/s)
     - WS     : vitesse maximale quotidienne du vent à 10m (m/s)
     - SD     : épaisseur de neige (cm)
     - TH     : journée avec ou sans orage (=1 avec orage et 0 sans)
      \n")
cat("3. Dans le cas où vous ne disposez pas de données quotidiennes pour 
un paramétre donné, un fichier de données mensuelles avec l'en-tête suivant:\n")
cat("                Stcode,MM,YYYY,RR,DRR,TX,TN,TM,SLP,WVP,SH,TH
    - Stcode : Identifiant de la station ( WMO/WIGOS/Indentifiant national)
    - MM     : Mois au format MM
    - YYYY   : Année au format YYYY
    - RR     : précipitation mensuelle (mm)
    - DRR    : Nombre de jours avec précipitations >= 1mm
    - TX     : température maximale moyenne mensuelle (°C)
    - TN     : température minimale moyenne mensuelle(°C)
    - TM     : température moyenne mensuelle (°C)
    - SLP    : pression au niveau de la mer moyenne mensuelle (hPa)
    - WVP    : tension de vapeur moyenne mensuelle (hPa)
    - SH     : durée d'insolation totale mensuelle (hours)
    - TH     : Nombre de jours avec orage 
    \n")
    cat("N.B.: Il convient de noter que ces fichiers contiennent des colonnes 
        prédéfinies et dans un ordre bien précis à respecter. \n")
    cat("-----------------\n")
cat("Le logigramme de l'application web suit les étapes suivantes :
1- Avant de charger les fichiers de données, Spécifier d'abord l'indicateur 
de données manquantes s'il est différent de (NA,-99999,vide) 
2- Spécifier le séparateur et le symbole décimal utilisés dans les fichiers 
csv d'entrée. Il existe deux possibilités de format csv : 
   2.1- français (Sep = point virgule ';' & Dec = virgule ',') 
   2.2- anglais (Sep = virgule ',' & Dec = point '.')
3- Charger les fichiers de données. Il existe trois possibilités :
   3.1- données quotidiennes seulement
   3.2- données mensuelles seulement
   3.3- données quotidiennes et mensuelles
Pour un paramètre donné, l'application cherche d'abord les données quotidiennes 
associées. s'il est manquant sur toute la période dans le fichier d'entrée, 
elle cherche sa disponibilité dans le fichier de données mensuelles. 
4- Si les données quotidiennes pour un paramètre donné existent, l'application 
vérifie les critères d'exhaustivités selon les recommandations de l'OMM. 
Ensuite, elle procède au calcul des normales climatiques selon les standards 
de l'OMM.
5- Une étape optionnelle est rajoutée à l'interface web est le contrôle 
de qualité des données quotidiennes seulement. il suffit de télécharger 
le fichier de configuration des seuils de QC et puis réaliser le QC. 
L'application offre la possibilité de visualiser les anomalies détectées 
ainsi que le téléchargement des sorties du QC.\n")
    cat("-----------------\n")    
    cat("Pour les options des valeurs manquantes selon les directives de l'OMM, 
l'utilisateur peut choisir l'une des deux options ou les deux.\n")
cat("\n Selon le guide des pratiques climatologiques (WMO, 2011), il est 
recommandé que, pour le calcul des valeurs mensuelles individuelles, lorsqu'une 
valeur mensuelle est la moyenne des valeurs quotidiennes de ce mois, elle 
ne doit pas être calculée, si l'un des critères suivants est satisfait:
– Les données d'observation sont manquantes pour au moins 11 jours du mois 
  en question;
– Les données d'observation sont manquantes pour une période de 5 jours 
  consécutifs durant le mois en question.\n")
cat(" Par ailleurs, le Guide des pratiques climatologiques (OMM, 2011) recommande 
que, pour qu'une normale ou moyenne soit calculée pour un mois donné, 
les données soient disponibles pour au moins 80 % des années de la période 
de référence. Cela équivaut à avoir des données disponibles pour ce mois 
dans 24 ou plus des 30 années pour une normale  climatologique standard 
ou une normale de référence.\n")
cat("-----------------\n")
cat("Concernant les fichiers de sortie, il convient de noter que les fichiers 
*.csv écrits par CLINO() contenant les valeurs normales doivent être révisés 
pour vérifier d'éventuelles erreurs dans les données ou dans le fichier 
des métadonnées.\n")
  }) # end output$About
  
  output$About1 <- renderPrint({
cat("New climatological standard normals should be calculated of the thirty-year period 1991-2020 
responding to the call of the World Meteorological Organization (WMO). To facilitate this task, 
this application #as been developed in R under Shiny to calculate the normal values and write 
them into CSV files in the delivering format specified by WMO. It requires two input files 
(a Metadata file and a Daily data file). A template for these files can be downloaded by 
clicking on the buttons alongside.\n\n")
cat("----------------------------------\n\n")
cat("De nouvelles normales climatologiquesstandard devraient être calculées sur la période de trente ans 
(1991-2020) répondant à l'appel de l'Organisation Météorologique Mondiale (OMM). Pour faciliter cette 
tâche, cette application a été développée sous R dans la plateforme Shiny pour calculer les valeurs 
des normales climatologiques et les écrire dans des fichiers CSV au format de livraison spécifié par 
l'OMM. Cette application nécessite deux fichiers d'entrée (un fichier de métadonnées et un fichier 
de données quotidiennes). Un modèle pour ces fichiers peut être téléchargé en cliquant sur les boutons 
à côté.\n\n\n")

  }) # end output$About1
 
  output$About2 <- renderPrint({
cat('The CLINO R-program for Climatological Standard Normals calculation and CSV 
output files. It has been developed by Jose A. Guijarro (September 2021), 
under license GPL 3 or greater. State Meteorological Agency (AEMET), 
Balearic Islands Office, Spain Member of WMO Expert Team on Data 
Requirements for Climate services (ET-DRC). 

The Shiny application based on CLINO program has been developed by Driss Bari 
(September 2021), General Directoriate of Meteorology (DGM), Casablanca, 
Morocco. Email : bari.driss@gmail.com \n')
cat("-----------------\n")
cat("This application needs two input files (in CSV format, with header 
    and semicolon as separator):\n")
cat("1. Stations metadata file with the following header :\n")
cat(" StCode,WMOid,WIGOSid,Latitude,Longitude,Elevation,StName,Country
 - StCode    : Station code (WMO/WIGOS/Domestic Identifier). It must 
               be the same in the daily / monthly data files.
 - WMOid     : WMO code of the station.
 - WIGOSid   : WIGOS code of the station (if defined).
 - Latitude  : Station latitude in degrees with four decimals 
               (between -90 and 90). 
 - Longitude : Station longitude in degrees with four decimals 
               (between -180 and 180).
 - Elevation : Station elevation in meters.
 - StName    : Station name.
 - Country   : Name of the country.
\n")
cat("2. One daily data file per station with the following header :\n")
cat("         Stcode,date,RR,TX,TN,TM,SLP,WVP,SH,WG,WS,SD,TH
- Stcode : station identifier ( WMO/WIGOS/Domestic identifier)
- date   : date in the format YYYY-MM-DD
- RR     : daily precipitation (mm)
- TX     : daily maximum temperature (°C)
- TN     : daily minimum temperature (°C)
- TM     : daily mean temperature (°C)
- SLP    : daily mean sea level pressure (hPa)
- WVP    : daily mean water vapor pressure (hPa)
- SH     : daily total number of sunshine hours (hours)
- WG     : daily wind gusts (m/s)
- WS     : the daily highest 10-minute mean wind speed (m/s)
- SD     : snow depth (cm)
- TH     : day with/without thunder (=1 with thunder and 0 otherwise)
\n")
cat("3. In case you do not have daily data for a given parameter, 
a monthly data file per station with the following header :\n")
cat(" Stcode,MM,YYYY,RR,DRR,TX,TN,TM,SLP,WVP,SH,TH
- Stcode : station identifier ( WMO/WIGOS/Domestic identifier)
- MM     : Month in the format MM
- YYYY   : Year in the format YYYY
- RR     : monthly precipitation (mm)
- DRR    : Number of days with precipitation >= 1mm
- TX     : monthly mean maximum temperature (°C)
- TN     : monthly mean minimum temperature (°C)
- TM     : monthly mean temperature (°C)
- SLP    : monthly mean sea level pressure (hPa)
- WVP    : monthly mean water vapor pressure (hPa)
- SH     : monthly total number of sunshine hours (hours)
- TH     : Number of days with thunder 
\n")
cat("N.B.: It should noted that these files contain the predefined 
columns in the precise shown order. \n")
cat("-----------------\n")
cat("The flowchart of the web application is comprised from the following steps:
1- Before loading the data files, First specify the missing data 
indicator if it is different from (NA,-99999,empty)
2- Specify the separator and the decimal symbol used in the input 
csv files. There are two possibilities of csv format:
  2.1- French (Sep = semicolon ';' & Dec = comma ',')
  2.2- English (Sep=comma ',' & Dec=period '.')
3- Load the data files. There are three possibilities:
  3.1- daily data only
  3.2- monthly data only
  3.3- daily and monthly data
For a given parameter, the application first searches for the associated 
daily data. if it is missing over the entire period in the input file, 
it looks for its availability in the monthly data file.
4- If the daily data for a given parameter exists, the application 
checks the completeness criteria according to WMO recommendations. 
Then, it proceeds to the calculation of the climatic normals 
according to the WMO standards.
5- An optional step is added to the web interface. It is dedicated to
the quality control of the daily data. First, download the QC settings 
file, modify the thresholds if necessary and load it. Then perform the QC. 
The application offers the possibility of visualizing the anomalies 
detected as well as downloading of the outputs of the QC.\n")
cat("-----------------\n")
cat("For missing values options with respect to WMO standards, 
the user can choose one or both of the two options. \n")
cat('\n Following the Guide to Climatological Practices (WMO, 2011), 
it is recommended that, for individual monthly values calculation 
(where a monthly value is the mean of that month’s daily values), 
it should not be calculated if either of the following criteria 
are satisfied:
– Observations are missing for 11 or more days during the month;
– Observations are missing for a period of 5 or more consecutive 
   days during the month.\n')
cat('Besides, the Guide to Climatological Practices (WMO, 2011) 
recommends that, for a normal or average to be calculated for a 
given month, data should be available for at least 80% of the years 
in the averaging period. This equates to having data available for 
that month in 24 or more out of the 30 years for a climatological 
standard normal or a reference normal.\n')
cat("-----------------\n")
cat('Regarding the output files, it should be noted that 
the *.csv files written by CLINO() containing the normal 
values should be revised to check for possible errors 
in the data or in the metadata file.\n')
  }) # end output$About
  
  # Les boutons de déplacement d'une étape à une autre
  observeEvent(input$s1to2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VVarCalc")
  })
  
  observeEvent(input$s2to3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "VClinooutput")
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
    { updateCheckboxInput(session, "allmain", value = !(input$all)) 
    } else {
      updateCheckboxInput(session, "allmain", value = !(input$all)) 
      }
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

  #
  #    **********   Info Message about the MetaData File by checking its format
  #
  output$InfoMsg <- renderText({
    if(is.null(input$MetaDatafile)){return()}
    nb.col=dim(metadata())[[2]]
    if ( nb.col != 8)
    {
      paste0("The input files has not 8 columns, ... Please check and retry !!! 
                 Read carefully the README / Le fichier que vous avez sélectionné ne contient pas 8 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS")
    } else {
      if (identical(colnames(metadata()), c("StCode","WMOid","WIGOSid","Latitude","Longitude",
                                            "Elevation","StName","Country")) )
      {
        paste0("")
      } else {
        paste0("The input files has 8 columns, but the names of columns and/or their order 
              are not respected ... Please check and retry !!! 
              Read carefully the README / Le fichier que vous avez sélectionné contient 8 colonnes, mais les noms des colonnnes  ou leur ordre  
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
      paste0("The input file files has not 13 columns, ... Please check and retry !!! 
                Read carefully the README / Le fichier que vous avez sélectionné ne contient pas 13 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
    } else {
      nb.col=dim(mydata())[[2]]
      if ( nb.col != 14)
      {
        paste0("The input file files has not 13 columns, ... Please check and retry !!! 
                Read carefully the README / Le fichier que vous avez sélectionné ne contient pas 13 colonnes, ... Vérifiez et ré-essayez à nouveau !!! 
                 Lisez attentivement le manuel d'utilisation ou A PROPOS ")
      } else {
        if (identical(colnames(mydata()), c("Stcode","date","RR","TX","TN",
                                            "TM","SLP","WVP","SH","WG","WS","SD","TH","Year")) )
        {
          paste0("")
        } else {
          paste0("The input files has 13 columns, but the names of columns and/or their order 
              are not respected ... Please check and retry !!! 
              Read carefully the README  / Le fichier que vous avez sélectionné contient 13 colonnes, mais les noms des colonnnes  ou leur ordre  
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
    write.table(data.quality.control(mydata(),QC.config()),"QC-Output.csv", quote = FALSE, col.names = TRUE, 
                row.names = FALSE, sep = ",", dec = ".")
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
        text = "Please wait while computing the WMO normals ... / Patientez un moment, le calcul des normales climatiques est en cours ... "
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
    
    
    namfiles=read.table("Names-Created-Files-toshow.txt", header = FALSE)
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

  output$QCOutficdownload <- downloadHandler(
#    filename = function(){
#      paste0("Created-CLINO-Files-",Sys.Date(),"-",Sys.time(),".zip")
#    },
    filename = paste0("QC-Output-",Sys.Date(),"-",Sys.time(),".csv"),
    content = function(file){
          file.copy("QC-Output.csv", file)
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