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

source("Own-functions-EN-Github.R")

write.clino.param.calc.files()

startavgperiod='1991'
endavgperiod='2020'
avgperiod=paste0(startavgperiod,'-',endavgperiod)
options(shiny.maxRequestSize = 10*1024^2)
###################################################################################
#     User Interface
###################################################################################

ui <- dashboardPage(
        dashboardHeader(title = "WMO Climatological Normals Calculation", titleWidth = 450),
        dashboardSidebar(width = 160, disable = TRUE,
          sidebarMenu(
            menuItem("CLINO Calculation", tabName = "clino", icon = icon("iclino"))#,
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
                    tabPanel(strong("README"), value = "VReadme",
                              verbatimTextOutput("About1"), 
                              downloadButton('tempmetadatadownload',strong('Download Template MetaData File'),
                                             class = 'btn-warning'),
                              downloadButton('tempdailydatadownload', strong('Download Template Daily Data File'),
                                             class = 'btn-warning'),
                             downloadButton('tempmonthlydatadownload', strong('Download Template Monthly Data File'),
                                            class = 'btn-warning'),
                             downloadButton('howtousedownload', strong('User Guide'),
                                            class = 'btn-warning'),
                              verbatimTextOutput("About"),
                              align="left"),  # end TabPanel ReadMe
                     #
                     #   Tab : Load daily data and metadata files
                     #
                     tabPanel(title = strong("1. Load data"),
                              value = "LoadData",
                              column( width = 5, 
                                p("Enter the missing data indicator in the daily data file if 
                                  different from (NA,-99999,empty)"),
                                textInput("MissID", label = "Missing Data Indicator", 
                                        value = "Enter missing indicator..."),
                                h5(strong("CSV File Separtor and Decimal symbols")),
                                checkboxInput('ENCSV', 'English (Sep = comma "," & Dec = point ".")', value = TRUE),
                                checkboxInput('FRCSV', 'French (Sep = semicolon ";" & Dec = comma ",")', value = FALSE),
                                fileInput("MetaDatafile","Upload MetaData file", multiple = FALSE),
                    ##            actionButton("UpldMD", strong("Upload MetaData"), class = "btn-primary"),
                                fileInput("StatDatafile","Upload Daily Data file", multiple = FALSE), 
                    ##            actionButton("uploadDD", strong("Upload Daily Data"), class = "btn-primary"),
                    ##            p("Click the button to view the data in the Datasets tab. 
                    ##            it may take a while depending on the size of the file."),
                                fileInput("MLYDatafile","Upload Monthly Data file", multiple = FALSE), 
                    ##        actionButton("uploadMD", strong("Charger données mensuelles"), class = "btn-primary"),
                                actionButton("s1to2",strong("Net step"), icon = icon("arrow-circle-right"),
                                class = "btn-warning  btn-lg")
                              ),
                               column(width = 7,
                                     h3(strong("Instructions")),
                                     p(strong(" 1. "),"Before uploading daily data file, enter missing data indicator if not 
                                      (NA,-99999,empty)"),
                                     p(strong(" 2. "),"Check the used separator and decimal symbols in the input csv files. 
                                      There are
                                      two possibilities of csv format : French (Sep = semicolon ';' & Dec = comma ',')
                                      and English (Sep = comma ',' & Dec = point '.')."),
                                     p(strong(" 3. "),"Upload the metadata file. This file", strong("must"),"use the following format and 
                                        header : "),
                                     p(strong("StCode,WMOid,WIGOSid,Latitude,Longitude,Elevation,StName,Country")),
                                     p('"168",06380,0-20000-0-06380,50.9053,5.7619,114.3,"MAASTRICHT",Nederlands'),
                                     p(" where :",strong(" StCode :"),"Station code (WMO/WIGOS/Domestic Identifier). It must be the same in the daily 
                                       data file.",strong(" WMOid :"),"WMO code of the station.",strong(" WIGOSid :"),
                                       " WIGOS code of the station (if defined).",strong(" Latitude :"),"Station 
                                       latitude in degrees with four decimals (between -90 and 90).",strong(" Longitude :"),
                                       "Station longitude in degrees with four decimals (between -180 and 180).",
                                       strong(" Elevation :")," Station elevation in meters.",
                                       strong(" StName :"),"Station name.",
                                       strong(" Country :")," Name of the country."),
                                     p(strong(" 4. "),"Upload the daily data file. This file", strong("must"),"use the following format and 
                                      header : "),
                                     p(strong("Stcode,date,RR,TX,TN,TM,SLP,WVP,SH,WG,WS,SD,TH")),
                                     p("168,1961-01-01,0,6.8,2.3,4.7,1016.5,7.598521,3.2,19.5,6.7,0,1"),
                                     p(" where :",strong("Stcode :")," station identifier ( WMO/WIGOS/Domestic identifier)",
                                     strong("date :")," date in the format YYYY-MM-DD.",
                                     strong("RR :")," daily precipitation (mm).",
                                     strong("TX :")," daily maximum temperature (°C).",
                                     strong("TN :")," daily minimum temperature (°C).",
                                     strong("TM :")," daily mean temperature (°C).",
                                     strong("SLP :")," daily mean sea level pressure (hPa).",
                                     strong("WVP :")," daily mean water vapor pressure (hPa).",
                                     strong("SH :")," daily total number of sunshine hours (hours).",
                                     strong("WG :")," daily wind gusts (m/s).",
                                     strong("WS :")," the daily highest 10-minute mean wind speed (m/s).",
                                     strong("SD :")," snow depth (cm).",
                                     strong("TH :")," day with/without thunder (=1 with thunder and 0 otherwise)."
                              )
                               )
                     ), # end tabPanel Load Daily and Metadata files  
                    #
                    #   Tab : Data Quality Control
                    #
                    tabPanel(title = strong("2. Data Quality"),
                             value = "QCData",
                             fluidRow(title=" row 1",
                             column( width = 3,
                                     fileInput("QCconfig","Upload Quality Control settings file", multiple = FALSE),
                                     actionButton("UpQC", strong("Upload QC settings file"), class = "btn-primary"),
                                     p("This file", strong("must")," use the format and header in the template 
                                       that can can be downloaded here."),
                                     downloadButton('QCficdownload', strong('Download Template QC settings File'),
                                                    class = 'btn-warning'),
                                    hr(),
                                    actionButton("s1bto2",strong("Net step"), icon = icon("arrow-circle-right"),
                                          class = "btn-warning  btn-lg")
                             ),
                             column( width = 5, 
                                     h4(strong("Quality Control Settings"), align = "center"),
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
                          h4(strong("Data Quality Control outputs"), align = "center"), 
                          DT::dataTableOutput("QCoutput")
                    ), # end tabPanel Load Daily and Metadata files  

                     #
                     #  Tab : Settings of Data Completeness Criteria
                     #
                     tabPanel(title = strong("3. Data Gaps"),
                              value = "DataGaps",
                            column( width = 6, 
                                    h4(strong("Missing data options with respect to WMO standards :")),
                                    h4(strong("* For individual monthly values calculation")),
                                    checkboxInput("IndivMiss", p(strong("Minimum "),"number of missing daily data ",
                                    strong("not allowed per month")), 
                                                  TRUE, width = 500),
                                    numericInput("IndivMissThres",NULL,11, width = 500),
                                    checkboxInput("ConsMiss", p(strong("Minimum "),"number of consecutive missing daily 
                                    data",strong("not allowed per month")), TRUE, width = 500),
                                    numericInput("ConsMissThres",NULL,5, width = 500),
                                    p("Check the desired option to perform data completeness verification based on the WMO 
                                    standards. Otherwise, the initial dataset will be used as is."),
                                    h4(strong("* For monthly normals calculation")),
                                    numericInput("YLYMissThres","Maximum percentage of missing years in the 
                                    averaging period",20, width = 500),
                                    actionButton("s2to3",strong("Net step"), icon = icon("arrow-circle-right"),
                                    class = "btn-warning  btn-lg")
                                    ),
                            column(width = 6,
                                   h3("Instructions"),
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
                                    for a climatological standard normal or a reference normal.')
                            )
                      ), # end tabPanel DataGaps
                     #
                     #  Setting of the Parameters Output
                     #
                     tabPanel(title = strong("4. Parameters Output"),
                              value = "VVarCalc",
                              column( width = 4, 
                                      checkboxInput("all", 'Select All/None', value = TRUE),
                                      checkboxInput("allmain", 'Select The principal parameters', value = FALSE),
                                      checkboxGroupInput("Precipitation", "Precipitation",
                                                         choices = RRchoices, 
                                                         width = '100%',
                                                         selected = RRchoices),
                                      checkboxGroupInput("MeanTemperature", "Mean Temperature",
                                                         choices = TMchoices, 
                                                         width = '100%',
                                                         selected = TMchoices),
                                    
                                     align="left",
                                     actionButton("s3to4",strong("Net step"), icon = icon("arrow-circle-right"),
                                                  class = "btn-warning  btn-lg")
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MaximumTemperature", "Maximum Temperature",
                                                         choices = TXchoices, 
                                                         width = '100%',
                                                         selected = TXchoices),
                                      checkboxGroupInput("MinimumTemperature", "Minimum Temperature",
                                                         choices = TNchoices, 
                                                         width = '100%',
                                                         selected = TNchoices),
                                      checkboxGroupInput("MeanSeaLevelPressure", "Mean Sea Level Pressure",
                                                         choices = SLPchoices, 
                                                         width = '100%',
                                                         selected = SLPchoices),
                                      
                                      align="left"
                              ),
                              column( width = 4, 
                                      checkboxGroupInput("MeanVapourPressure", "Mean Vapour Pressure",
                                                         choices = WVPchoices, 
                                                         width = '100%',
                                                         selected = WVPchoices),
                                      
                                      checkboxGroupInput("HoursSunshine", "Hours of Sunshine",
                                                         choices = SHchoices, 
                                                         width = '100%',
                                                         selected = SHchoices),
                                      checkboxGroupInput("SnowDepth", "Snow Depth",
                                                         choices = SDchoices, 
                                                         width = '100%',
                                                         selected = SDchoices),
                                      checkboxGroupInput("WindSpeed", "Wind Speed",
                                                         choices = WSchoices, 
                                                         width = '100%',
                                                         selected = WSchoices),
                                      checkboxGroupInput("WindGusts", "Wind Gusts",
                                                         choices = WGchoices, 
                                                         width = '100%',
                                                         selected = WGchoices),
                                      checkboxGroupInput("DaysWithThunder", "Days with thunder",
                                                         choices = THchoices, 
                                                         width = '100%',
                                                         selected = THchoices),
                                      align="left"
                              )
                     ), # end tabPanel ParamOutput
                     #
                     #  WMO CLINO Calculation
                     #  
                     tabPanel(title = strong("5. Compute CLINO"),
                              value = "VClinooutput",
                              fluidRow(
                                    column(8, offset = 2,
                                      h4(strong("Compute WMO CLINO")),
                                      p("Regarding the output files, it should be noted that the *.csv files written 
                                        by CLINO() containing the normal values should be revised to check for possible
                                        errors in the data or in the metadata file."),
                                      actionButton("do", strong("Compute WMO Normals"), class = "btn-primary"),
                                      p("Click the button to update the WMO normals calculations.")
                                    )
                              ),
                              fluidRow(
                                    column(8, offset = 2,
                                      h3(strong("CLINO Output :")),
                                      actionButton("clinoview", strong("View WMO Normals"), class = "btn-primary"),
                                      downloadButton('zipdownload', strong('Download zipped csv files'), class = 'btn-warning'),
                                      verbatimTextOutput("clino"),align="left")
                              )
                     ), # end tabPanel ComputeClino
                  #
                  #  View CLINO outputs
                  #
                    tabPanel(strong("View CLINO"), value = "VCLINOFILES", 
                           downloadButton('zipdownloadbis', strong('Download zipped csv files'), 
                                          class = 'btn-warning'),
                           DT::dataTableOutput("clinofile")
                    ), # end tabPanel View CLINO
                    #
                    #  View MeataData
                    #
                    tabPanel(title = strong("Metadata"), value = "TMetadata", tags$b(h4(textOutput("InfoMsg"))),
                               DT::dataTableOutput("metadatatable"),
                                actionButton("backto1",strong("Back to step1"), icon = icon("arrow-circle-left"),
                                class = "btn-warning  btn-lg")
                     ), # end tabPanel MetaData
                    #
                    #  View Daily Data 
                    #
                    tabPanel(strong("Datasets"), value = "VDatasets" , tags$b(h4(textOutput("InfoMsgDLYdata"))),
                              DT::dataTableOutput("table"), DT::dataTableOutput("mlytable"),
                              actionButton("backto2",strong("Back to step1"), icon = icon("arrow-circle-left"),
                              class = "btn-warning  btn-lg")
                    )#, # end tabPanel Summary Daily Data
               ) # end tabsetpanel  
            ) # end tabitem clino
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
    cat('The CLINO R-program for Climatological Standard Normals calculation and CSV output files. It has been developed by Jose A. Guijarro (September 2021), under license 
GPL 3 or greater. State Meteorological Agency (AEMET), Balearic Islands Office, Spain Member of WMO Expert Team on Data Requirements for Climate services (ET-DRC). 

The Shiny application based on CLINO program has been developed by Driss Bari (September 2021), General Directoriate of Meteorology (DGM), Casablanca, Morocco. 
Email : bari.driss@gmail.com \n')
    cat("-----------------\n")
    cat("This application needs two input files (in CSV format, with header and semicolon as separator):\n")
    cat("1. Stations metadata file with the following header :\n")
    cat("                           StCode;WMOid;WIGOSid;Latitude;Longitude;Elevation;StName;Country\n")
    cat("2. One daily data file per station with the following header :\n")
    cat("                           Stcode;date;RR;TX;TN;TM;SLP;WVP;SH;WG;WS;SD;TH
          - Stcode : station identifier ( WMO/WIGOS/Domestic identifier)
          - date : date in the format YYYY-MM-DD
          - RR : daily precipitation (mm)
          - TX : daily maximum temperature (°C)
          - TN : daily minimum temperature (°C)
          - TM : daily mean temperature (°C)
          - SLP : daily mean sea level pressure (hPa)
          - WVP : daily mean water vapor pressure (hPa)
          - SH : daily total number of sunshine hours (hours)
          - WG : daily wind gusts (m/s)
          - WS : the daily highest 10-minute mean wind speed (m/s)
          - SD : snow depth (cm)
          - TH : day with/without thunder (=1 with thunder and 0 otherwise)
          \n")
    cat("3. In case you do not have daily data for a given parameter, a monthly data file per station with the following header :\n")
    cat("                           Stcode;MM;YYYY;RR;DRR;TX;TN;TM;SLP;WVP;SH;TH
          - Stcode : station identifier ( WMO/WIGOS/Domestic identifier)
          - MM : Month in the format MM
          - YYYY : Year in the format YYYY
          - RR : monthly precipitation (mm)
          - DRR : Number of days with precipitation >= 1mm
          - TX : monthly mean maximum temperature (°C)
          - TN : monthly mean minimum temperature (°C)
          - TM : monthly mean temperature (°C)
          - SLP : monthly mean sea level pressure (hPa)
          - WVP : monthly mean water vapor pressure (hPa)
          - SH : monthly total number of sunshine hours (hours)
          - TH : Number of days with thunder 
          \n")
    cat("N.B.: It should noted that these files contain the predefined columns in the precise shown order. \n")
    cat("-----------------\n")
    cat("For missing values options with respect to WMO standards, the user can choose one or both of the two options. \n")
    cat('Following the Guide to Climatological Practices (WMO, 2011), it is recommended that, for individual monthly values calculation (where a monthly value is the mean 
of that month’s daily values), it should not be calculated if either of the following criteria are satisfied:
      –	 Observations are missing for 11 or more days during the month;
      –	 Observations are missing for a period of 5 or more consecutive days during the month.\n')
    cat('Besides, the Guide to Climatological Practices (WMO, 2011) recommends that, for a normal or average to be calculated for a given month, data should be available 
for at least 80% of the years in the averaging period. This equates to having data available for that month in 24 or more out of the 30 years for a climatological 
standard normal or a reference normal.\n')
    cat("-----------------\n")
    cat('Regarding the output files, it should be noted that the *.csv files written by CLINO() containing the normal values should be revised to check for possible errors 
in the data or in the metadata file.\n')
  }) # end output$About
  
  output$About1 <- renderPrint({
#    Create_Clino_Variables_file()
cat("New climatological standard normals should be calculated of the thirty-year period 1991-2020 responding to the call of the World Meteorological Organization (WMO). 
To facilitate this task, this application #as been developed in R under Shiny to calculate the normal values and write them into CSV files in the delivering format 
specified by WMO. It requires two input files (a Metadata file and a Daily data file). A template for these files can be downloaded by clicking on the buttons below.\n")
  }) # end output$About1
  
  
  
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
  
  # control the checkbixinput of the CSV file format (English /French)
  observe({
    updateCheckboxInput(session, "FRCSV", value = !(input$ENCSV))
  })
  observe({
    updateCheckboxInput(session, "ENCSV", value = !(input$FRCSV))
  })
  
  
  # control the checkbixinput of the surface parameters choice
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
      paste0("The input files has not 8 columns, ... Please check and retry !!! 
                 Read carefully the README ")
    } else {
      if (identical(colnames(metadata()), c("StCode","WMOid","WIGOSid","Latitude","Longitude",
                                            "Elevation","StName","Country")) )
      {
        paste0("")
      } else {
        paste0("The input files has 8 columns, but the names of columns and/or their order 
              are not respected ... Please check and retry !!! 
              Read carefully the README ")
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
                Read carefully the README ")
    } else {
      nb.col=dim(mydata())[[2]]
      if ( nb.col != 14)
      {
        paste0("The input files has not 13 columns, ... Please check and retry !!! 
                 Read carefully the README ")
      } else {
        if (identical(colnames(mydata()), c("Stcode","date","RR","TX","TN",
                                            "TM","SLP","WVP","SH","WG","WS","SD","TH","Year")) )
        {
          paste0("")
        } else {
          paste0("The input files has 13 columns, but the names of columns and/or their order 
              are not respected ... Please check and retry !!! 
              Read carefully the README ")
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
        text = "Please wait while computing the WMO normals ... "
      ) # show the modal window
      
#      if(is.null(input$StatDatafile)) {return()}
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
 # })
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
                                               extensions = c('Scroller'#,
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
    filename = "CLINO_WebApp_EN.pdf",
    content = function(file){
      file.copy("CLINO_WebApp_EN.pdf", file)
    }
  )
  
}# end server


###################################################################################
#     Shiny Application
###################################################################################

shinyApp(ui, server)