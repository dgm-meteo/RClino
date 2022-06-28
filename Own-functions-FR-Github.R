#
#  The main recommended parameters
#
RRchoicesm=c("Precipitation total (mm)"="1", 
             "Number of days with precipitation ≥ 1 mm (count) "="2"
)
TXchoicesm= c("Monthly mean values of daily maximum temperature (°C)"="3" 
)
TNchoicesm=c("Monthly mean values of daily minimum temperature (°C)"="4"
)
TMchoicesm=c("Monthly mean values of daily mean temperature (°C)"="5"
)
SLPchoicesm= c("Mean value of sea-level pressure (hPa)"="6")
WVPchoicesm=c("Mean vapour pressure (hPa)"="7")
SHchoicesm= c("Total number of hours of sunshine (hours)"="8")
SDchoicesm=""
WSchoicesm=""
WGchoicesm=""
THchoicesm=""

#
#  All parameters
#
RRchoices=c("Precipitation total (mm)"="1", 
            "Number of days with precipitation ≥ 1 mm (count) "="2", 
            "Mean number of days with daily precipitation ≥ 5 mm (count)"="160",
            "Mean number of days with daily precipitation ≥ 10 mm (count)"="161",
            "Mean number of days with daily precipitation ≥ 50 mm (count)"="162",
            "Mean number of days with daily precipitation ≥ 100  (count)"="163",
            "Mean number of days with daily precipitation ≥ 150 mm (count)"="164",
            "Highest recorded value of daily precipitation (mm)"="24")
TXchoices= c("Monthly mean values of daily maximum temperature (°C)"="3", 
             "Mean number of days with maximum temperature ≥25 °C (count)"="120", 
             "Mean number of days with maximum temperature ≥30 °C (count)"="121",
             "Mean number of days with maximum temperature ≥35 °C (count)"="122",
             "Mean number of days with maximum temperature ≥40 °C (count)"="123",
             "Mean number of days with maximum temperature < 0 °C (count)"="14", 
             "Highest recorded value of daily maximum temperature (°C)"="22")
TNchoices=c("Monthly mean values of daily minimum temperature (°C)"="4",
            "Mean number of days with minimum temperature < 0 °C (count)"="15",
            "Lowest recorded value of daily minimum temperature (°C)"="23")
TMchoices=c("Monthly mean values of daily mean temperature (°C)"="5",
            "Highest recorded values of mean daily temperature (°C)"="20",
            "Lowest recorded values of mean daily temperature (°C)"="21")
SLPchoices= c("Mean value of sea-level pressure (hPa)"="6")
WVPchoices=c("Mean vapour pressure (hPa)"="7")
SHchoices= c("Total number of hours of sunshine (hours)"="8")
SDchoices=c("Mean number of days with snow depth > 0 cm (count)"="170",
            "Mean number of days with snow depth > 1 cm (count)"="171",
            "Mean number of days with snow depth > 10 cm (count)"="172",
            "Mean number of days with snow depth > 50 cm (count)"="173")
WSchoices=c("Mean number of days with wind speed ≥ 10 m/s (count)"="180",
            "Mean number of days with wind speed ≥ 20 m/s (count)"="181",
            "Mean number of days with wind speed ≥ 30 m/s (count)"="182")
WGchoices=c("Highest recorded wind gust (m/s)"="25")
THchoices=c("Mean number of days with thunder (count)"="26")

#
#   Quelques fonctions utiles pour le calcul de la fréquence des phénomènes à seuil 
#

    #- greq.- Count no. of values greater or equal to a threshold
    greq <- function(dat,thr) {
      return(sum(dat >= thr, na.rm=TRUE))
    }
    #- grth.- Count no. of values greater than a threshold
    grth <- function(dat,thr) {
      return(sum(dat > thr, na.rm=TRUE))
    }
    #- loeq.- Count no. of values lower or equal to a threshold
    loeq <- function(dat,thr) {
      return(sum(dat <= thr, na.rm=TRUE))
    }
    #- loth.- Count no. of values lower than a threshold
    loth <- function(dat,thr) {
      return(sum(dat < thr, na.rm=TRUE))
    }

    #- which.max.last - find the last occurrence of the maximum value
    which.max.last <- function(x) {
      return(max(which(x == max(x, na.rm = TRUE), arr.ind = TRUE)))
    }

    #- which.max.last - find the last occurrence of the minimum value 
    which.min.last <- function(x) {
      return(max(which(x == min(x, na.rm = TRUE), arr.ind = TRUE)))
    }

    #- grthPC.- Ratio of no. of values greater than a threshold
    grthPC <- function(dat,thr) {
      if (length(which(!is.na(dat))) > 0)
      {
        ratio=sum(dat > thr, na.rm=TRUE)/length(which(!is.na(dat)))
      } else {
        ratio=NA
      }
      return(ratio)
    }
    
    #- greqPC.- Ratio of no. of values greater or equal to a threshold
    greqPC <- function(dat,thr) {
      if (length(which(!is.na(dat))) > 0)
      {
        ratio=sum(dat >= thr, na.rm=TRUE)/length(which(!is.na(dat)))
      } else {
        ratio=NA
      }
      return(ratio)
    }
    
    #- loeqPC.- Count no. of values lower or equal to a threshold
    loeqPC <- function(dat,thr) {
      if (length(which(!is.na(dat))) > 0)
      {
        ratio=sum(dat <= thr, na.rm=TRUE)/length(which(!is.na(dat)))
      } else {
        ratio=NA
      }
      return(ratio)
    }
    #- lothPC.- Ratio of no. of values lower than a threshold
    lothPC <- function(dat,thr) {
      if (length(which(!is.na(dat))) > 0)
      {
        ratio=sum(dat < thr, na.rm=TRUE)/length(which(!is.na(dat)))
      } else {
        ratio=NA
      }
      return(ratio)
    }
    
#
#  Conversion des coordonnées géographiques sous le format recommandé par l'OMM    
#   
    #- deg2ddmmss.- Convert degrees with decimals to degrees, minutes and seconds.
    deg2ddmmss <- function(deg,coord) {
      #deg: degrees with decimals
      #coord: either 'lat' or 'lon', according with the deg coordinate
      if(deg<0) { 
        deg <- -deg
        if(coord=='lat') L <- 'S' else L <- 'W'
      }
      else { if(coord=='lat') L <- 'N' else L <- 'E' }
      dd <- floor(deg); deg <- (deg - dd) * 60
      mm <- floor(deg); deg <- (deg - mm) * 60
      ss <- round(deg)
      if(ss==60) {
        ss <- 0; mm <- mm+1
        if(mm==60) { mm <- 0; deg <- deg+1 }
      }
      if(coord=='lat') return(sprintf('%02d|%02d|%02d|%s',dd,mm,ss,L))
      else return(sprintf('%03d|%02d|%02d|%s',dd,mm,ss,L))
    }
    
    #- noyr.- Rounded no. of years of original data
    noyr <- function(dat,mv,ny) {
      nas <- aggregate(dat,list(mv),is.na) #missing data (by month)
      nm <- unlist(lapply(nas$x,sum)) #monthly no. of missing data
      nd <- unlist(lapply(nas$x,length)) #monthly no. of days in the period
      return(round(ny*(1-nm/nd),1))
    }

    #
    #  Préparation des fichiers d'entrée à la fonction CLINO :  CLINO_parameters.csv and CLINO_calculations.csv
    #
    
    write.clino.param.calc.files <- function()
    {
      #
      #  Create the CLINO_parameters.csv
      #
      CP <- file("CLINO_parameters.csv",'w')
      write("Parameter_Code,Parameter_Name,Units,Monthly_function,Threshold,Annual_function",CP)
      write("1,Precipitation_Total,mm,sum,,sum",CP)
      write("2,Number_of_Days_with_Precipitation_≥_1_mm,count,greqPC,1,sum",CP)
      write("3,Daily_Maximum_Temperature,Deg_C,mean,,mean",CP)
      write("4,Daily_Minimum_Temperature,Deg_C,mean,,mean",CP)
      write("5,Daily_Mean_Temperature,Deg_C,mean,,mean",CP)
      write("6,Mean_Sea_Level_Pressure,hPa,mean,,mean",CP)
      write("7,Mean_Vapor_Pressure,hPa,mean,,mean",CP)
      write("8,Total_Number_of_Hours_of_Sunshine,hours,sum,,sum",CP)
      write("10,Mean_Station-Level_Pressure,hPa,mean,,mean",CP)
      write("11,Boundaries_of_quintiles_of_monthly_precipitation,mm,,,",CP)
      write("12,Number_of_Days_with_Maximum_Temperature_≥_threshold*_Deg_C,count,,,",CP)
      write("13,Number_of_Days_with_Minimum_Temperature_≤_threshold*_Deg_C,count,,,",CP)
      write("14,Number_of_Days_with_Maximum_Temperature_<_0_Deg_C,count,lothPC,0,sum",CP)
      write("15,Number_of_Days_with_Minimum_Temperature_<_0_Deg_C,count,lothPC,0,sum",CP)
      write("16,Number_of_Days_with_Daily_Precipitation_≥_threshold*_mm,count,,,",CP)
      write("17,Number_of_Days_with_Snow_Depth_>_threshold*_cm,count,,,",CP)
      write("18,Number_of_Days_with_Wind_Speed_≥_threshold*_m/s,count,,,",CP)
      write("19,Number_of_Days_with_Visibility_<_threshold*_m,count,,,",CP)
      write("20,Highest_Value_of_Mean_Daily_Temperature,Deg_C,max,,max",CP)
      write("21,Lowest_Value_of_Mean_Daily_Temperature,Deg_C,min,,min",CP)
      write("22,Highest_Value_of_Daily_Maximum_Temperature,Deg_C,max,,max",CP)
      write("23,Lowest_Value_of_Daily_Minimum_Temperature,Deg_C,min,,min",CP)
      write("24,Highest_Value_of_Daily_Precipitation,mm,max,,max",CP)
      write("25,Highest_Wind_Gust,m/s,max,,max",CP)
      write("26,Mean_Number_of_Days_with_Thunder,count,grthPC,0,sum",CP)
      write("27,Mean_Number_of_Days_with_Hail,count,grthPC,0,sum",CP)
      write("30,Cloud_Amount,okta,mean,,mean",CP)
      write("31,Global_Solar_Radiation,MJ/m2,mean,,mean",CP)
      write("32,Direct_Solar_Radiation,MJ/m2,,,",CP)
      write("33,Diffuse_Solar_Radiation,MJ/m2,,,",CP)
      write("34,Wind_Speed,m/sec,mean,,mean",CP)
      write("35,Wind_Direction,degrees,,,",CP)
      write("36,Soil_Temperature,Deg_C,,,",CP)
      write("37,Snowfall,cm,,,",CP)
      write("38,Relative_Humidity,%,mean,,mean",CP)
      write("39,Dewpoint_Temperature,Deg_C,,,",CP)
      write("40,Rainfall,mm,,,",CP)
      write("41,Bright_Sunshine,hours,,,",CP)
      write("42,Calm_Winds,hours,,,",CP)
      write("43,Number_Days_with_Sandstorm/Thick Dust/Haze,count,grthPC,0,sum",CP)
      write("44,Number_Days_with_Measurable_Bright_Sunshine,count,grthPC,0,sum",CP)
      write("45,Number_Days_with_Lightning,count,grthPC,0,sum",CP)
      write("46,Number_Days_with_Rain_Showers,count,grthPC,0,sum",CP)
      write("47,Number_Days_with_Snowfall,count,grthPC,0,sum",CP)
      write("48,Number_Days_with_Fog/Ice_Fog,count,grthPC,0,sum",CP)
      write("49,Number_Days_with_Fog_Sky_Obscured,count,grthPC,0,sum",CP)
      write("50,Number_Days_with_Fog_Sky_Unobscured,count,grthPC,0,sum",CP)
      write("51,Number_Days_with_Haze/Smoke,count,grthPC,0,sum",CP)
      write("52,Number_Days_with_Dust,count,grthPC,0,sum",CP)
      write("53,Number_Days_with_Blowing_Dust/Sand,count,grthPC,0,sum",CP)
      write("54,Number_Days_with_Visibility_≤_Threshold*_km,count,,,",CP)
      write("55,Number_Days_with_No_Sunshine,count,loeqPC,0,sum",CP)
      write("56,Number_Days_with_Dew,count,grthPC,0,sum",CP)
      write("57,Number_Days_with_Rime/Glaze_Ice,count,grthPC,0,sum",CP)
      write("58,Number_Days_with_Air_Frost,count,grthPC,0,sum",CP)
      write("59,Number_Days_with_Grass_Frost,count,grthPC,0,sum",CP)
      write("60,Number_Days_with_Gale_Force_Winds,count,grthPC,0,sum",CP)
      write("61,Number_Days_Maximum_Temperature_≤_threshold*_Deg_C,count,,,",CP)
      write("62,Number_Days_Minimum_Temperature_≥_threshold*_Deg_C,count,,,",CP)
      write("63,Number_Days_with_Dust/Haze/Mist,count,grthPC,0,sum",CP)
      write("64,Number_Days_Maximum_Temperature_>_threshold*_Deg_C,count,,,",CP)
      write("65,Number_Days_Maximum_Temperature_<_threshold*_Deg_C,count,,,",CP)
      write("66,Number_Days_Minimum_Temperature_>_threshold*_Deg_C,count,,,",CP)
      write("67,Number_Days_Minimum_Temperature_<_threshold*_Deg_C,count,,,",CP)
      write("68,Number_Days_with_Snowfall_≥_threshold*_cm,count,,,",CP)
      write("69,Number_Days_with_Freezing_Rain/Drizzle,count,grthPC,0,sum",CP)
      write("70,Number_Days_with_Blowing_Snow,count,grthPC,0,sum",CP)
      write("71,Number_Days_with_Rain/Drizzle,count,grthPC,0,sum",CP)
      write("72,Number_Days_with_Snow/Hail,count,grthPC,0,sum",CP)
      write("73,Number_Days_with_Fog/Mist,count,grthPC,0,sum",CP)
      write("74,Number_Days_with_Ice_Storm,count,grthPC,0,sum",CP)
      write("75,Number_Days_with_Thick_Haze,count,grthPC,0,sum",CP)
      write("76,Number_Days_with_Rising_Sand,count,grthPC,0,sum",CP)
      write("77,Number_Days_with_Mist,count,grthPC,0,sum",CP)
      write("78,Number_Days_with_Squalls,count,grthPC,0,sum",CP)
      write("79,Number_Days_with_Duststorm/Sandstorm,count,grthPC,0,sum",CP)
      write("80,Number_Days_with_Sleet/Snow,count,grthPC,0,sum",CP)
      write("81,Number_Days_with_Fog,count,grthPC,0,sum",CP)
      write("82,Number_Days_with_Daily_Maximum_Wind_Speed_≥_threshold*_m/s,count,,,",CP)
      write("99,Custom_Element_Specified_by_Contributor,custom,,,",CP)
      write("110,Q0,mm,,,",CP)
      write("111,Q1,mm,,,",CP)
      write("112,Q2,mm,,,",CP)
      write("113,Q3,mm,,,",CP)
      write("114,Q4,mm,,,",CP)
      write("115,Q5,mm,,,",CP)
      write("120,Number_of_Days_with_Maximum_Temperature_≥_25_Deg_C,count,greqPC,25,sum",CP)
      write("121,Number_of_Days_with_Maximum_Temperature_≥_30_Deg_C,count,greqPC,30,sum",CP)
      write("122,Number_of_Days_with_Maximum_Temperature_≥_35_Deg_C,count,greqPC,35,sum",CP)
      write("123,Number_of_Days_with_Maximum_Temperature_≥_40_Deg_C,count,greqPC,40,sum",CP)
      write("160,Number_of_Days_with_Daily_Precipitation_≥_5_mm,count,greqPC,5,sum",CP)
      write("161,Number_of_Days_with_Daily_Precipitation_≥_10_mm,count,greqPC,10,sum",CP)
      write("162,Number_of_Days_with_Daily_Precipitation_≥_50_mm,count,greqPC,50,sum",CP)
      write("163,Number_of_Days_with_Daily_Precipitation_≥_100_mm,count,greqPC,100,sum",CP)
      write("164,Number_of_Days_with_Daily_Precipitation_≥_150_mm,count,greqPC,150,sum",CP)
      write("170,Number_of_Days_with_Snow_Depth_>_0_cm,count,grthPC,0,sum",CP)
      write("171,Number_of_Days_with_Snow_Depth_>_1_cm,count,grthPC,1,sum",CP)
      write("172,Number_of_Days_with_Snow_Depth_>_10_cm,count,grthPC,10,sum",CP)
      write("173,Number_of_Days_with_Snow_Depth_>_50_cm,count,grthPC,50,sum",CP)
      write("180,Number_of_Days_with_Wind_Speed_≥_10_m/s,count,greqPC,10,sum",CP)
      write("181,Number_of_Days_with_Wind_Speed_≥_20_m/s,count,greqPC,20,sum",CP)
      write("182,Number_of_Days_with_Wind_Speed_≥_30_m/s,count,greqPC,30,sum",CP)
      write("190,Number_of_Days_with_Visibility_<_50_m,count,lothPC,50,sum",CP)
      write("191,Number_of_Days_with_Visibility_<_100_m,count,lothPC,100,sum",CP)
      write("192,Number_of_Days_with_Visibility_<_1000_m,count,lothPC,1000,sum",CP)
      write("610,Number_Days_Maximum_Temperature_≤_10_Deg_C,count,loeqPC,10,sum",CP)
      write("990,Highest_Value_of_Minimum_Daily_Temperature,Deg_C,max,,max",CP)
      write("991,Lowest_Value_of_Maximum_Daily_Temperature,Deg_C,min,,min",CP)
      close(CP)
      
      #
      #  Create the CLINO_calculations.csv file
      #
      CC <- file("CLINO_calculations.csv",'w')
      write("Calculation_Name,Calculation_Code,Parameter calculation method descriptions from WMO-No. 1203",CC)
      write("Mean,1,Mean Parameter - mean of daily values during the month",CC)
      write("Max,2,Extreme Parameter Maximum - highest value during month",CC)
      write("Min,3,Extreme Parameter Minimum - lowest value during month",CC)
      write("Sum,4,Sum Parameter - sum of daily values during month",CC)
      write("Count,5,Count Parameter - Number of days expressed as % of available days",CC)
      write("Q0,6,Quintile Parameter 0 - Lower bound of quintile 1 (Extreme Minimum)",CC)
      write("Q1,7,Quintile Parameter 1 - Upper bound of quintile 1",CC)
      write("Q2,8,Quintile Parameter 2 - Upper bound of quintile 2",CC)
      write("Q3,9,Quintile Parameter 3 - Upper bound of quintile 3",CC)
      write("Q4,10,Quintile Parameter 4 - Upper bound of quintile 4",CC)
      write("Q5,11,Quintile Parameter 5 - Upper bound of quintile 5 (Extreme Maximum)",CC)
      write("Median,12,Median Monthly Value",CC)
      write("SDMean,13,Standard Deviation of Mean Monthly Value",CC)
      write("SDMeanD,14,Standard Deviation of Mean Daily Value",CC)
      write("MaxDate,15,Date (Year/Day) of Occurrence of Extreme Maximum Daily Value",CC)
      write("MinDate,16,Date (Year/Day) of Occurrence of Extreme Minimum Daily Value",CC)
      write("MinMon,17,Minimum Monthly Value",CC)
      write("DMinMon,18,Year of Occurrence of Minimum Monthly Value",CC)
      write("MaxMon,19,Maximum Monthly Value",CC)
      write("DMaxMon,20,Year of Occurrence of Maximum Monthly Value",CC)
      write("NOY,98,Number of Years Used to Calculate Normal",CC)
      write("Custom,99,Custom Parameter or Statistic Specified by Contributor",CC)
      close(CC)
      
    }
    
    #
    #  Critères d'exshautivité des données quotidiennes
    #
    data.completness <- function(datclino,debperiod,finperiod,cons.miss.check,cons.miss.thres,
                                 indiv.miss.check,indiv.miss.thres)
    {
      stids <- unique(datclino[["Stcode"]])
      outdatset=as.data.frame(vector())
      for ( stnam in stids)
      {
        substat=subset(datclino, datclino[["Stcode"]] == stnam)
        if ( is.factor(stids)) { substat <- droplevels(substat) }
        substatfull = substat %>%
          mutate(date = as.Date(date)) %>%
          complete(date = seq.Date(as.Date(as.character(debperiod)), as.Date(as.character(finperiod)), by="day"), Stcode)
        substatfull=as.data.frame(substatfull)
        substatfull[["mm"]] = strftime(substatfull[["date"]],'%m')
        substatfull[["yy"]] = strftime(substatfull[["date"]],'%Y')
        um=unique(substatfull[["mm"]])
        uy=unique(substatfull[["yy"]])
        for ( y in uy)
        {
          for (m in um)
          {
            submy=subset(substatfull, substatfull[["mm"]] == m & substatfull[["yy"]] == y)
            for ( param in c("RR","TX","TN","TM","SLP","WVP","SH","WG","WS","SD","TH"))
            {
              # number of missing value
              no.miss=sum(is.na(submy[[param]]))
              # number of consecutive missing values
              r <- rle(is.na(submy[[param]]))
              rmax=max(r$lengths[r$values])
              if ( indiv.miss.check)
              {
                if ( no.miss >= indiv.miss.thres)
                {
                  substatfull[[param]][substatfull[["mm"]] == m & substatfull[["yy"]] == y & substatfull[["Stcode"]] == stnam]=NA
                } # no.miss
              } # indiv.miss.check 
              if ( cons.miss.check)
              {
                if ( rmax >= cons.miss.thres)
                {
                  substatfull[[param]][substatfull[["mm"]] == m & substatfull[["yy"]] == y & substatfull[["Stcode"]] == stnam]=NA
                  } # no. cons. miss
              } # indiv.miss.check 
            } # param
          } # m
        } # y
        outdatset=rbind(outdatset,substatfull)
      }# stat
      outdatset$mm <- NULL
      outdatset$yy <- NULL
      outdatset1=outdatset[,c("Stcode","date","RR","TX","TN","TM","SLP","WVP","SH","WG","WS","SD","TH")]
      return(outdatset1)
    }# data completeness
    
    
    #######################################################################################
    # My Own Internal functions
    #######################################################################################
    #- CLINO.- Climatological Standard Normals calculation and CSV output files.
    #By Jose A. Guijarro (September 2021), under license GPL 3 or greater.
    #State Meteorological Agency (AEMET), Balearic Islands Office, Spain
    #Member of WMO Expert Team on Data Requirements for Climate services (ET-DRC).
    
    CLINO <- function(period='1991-2020', YLYMissThresIn=20, MLYDatasetin) {
      #period: period from which to compute the Climatological Standard Normals
      #  (1991-2020 by default).
      #-----------------
      #This function needs four configuraton files (in CSV format, with header):
      # CLINO_stations.csv : stations metadata
      # CLINO_variables.csv : variables, file names and related parameters
      # CLINO_parameters.csv : list of CLINO parameters (predefined and custom)
      # CLINO_calculations.csv : calculation types and their CLINO codes
      #-----------------  
      #Daily homogenized data are provided in the files listed in CLINO_variables.csv
      #They can be the *.rda results of Climatol daily homogenizations or CSV files
      #  with stations daily data in columns, station codes in the header and a
      #  first column with dates in YYYY-MM-DD format.
      #----------------- Example:
      #An example can be run by uncompressing CLINO_example.tgz into our R working
      #directory and running:   source('CLINO.R'); CLINO()
      #----------------- Initial notice:
      #cat('\nCLINO.- Climatological Standard Normals calculation and CSV output files.\n')
      #cat('By Jose A. Guijarro (September 2021), under license GPL 3 or greater.\n')
      #cat('Directions of use are available at:\n\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-en.pdf (English)\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-fr.pdf (French)\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-es.pdf (Spanish)\n')
      #
      #----------------- Read input files:
      #
      cat('\n ++++++++++>   Reading input files...\n')
      stm <- read.csv('CLINO_stations.csv', sep = ",", dec = ".", as.is=TRUE) #stations metadata
      nst <- nrow(stm) #no. of stations
      cat('\n  The metadata file \n')
      vrb <- read.csv('CLINO_variables.csv',as.is=TRUE) #variables, files and par.
      nv <- nrow(vrb) #no. of variables
      cat('\n  The file listing the variables, daily files and parameters to compute  \n')
      pmt <- read.csv('CLINO_parameters.csv',as.is=TRUE) #parameters list
      cat('\n  The file listing all the recommended parameters by WMO  \n')
      clc <- read.csv('CLINO_calculations.csv',as.is=TRUE) #calculations list
      cat('\n  The file listing the calculation modes for the main meteorological elements \n')
      clcn <- tolower(clc[,1]) #calculation names
      #
      #----------------- Calculate the CLINO values:
      #
      cat('\n ++++++++++>   Calculation of the Climate Normals:\n\n')
      yrs <- unlist(strsplit(period,'-')) #first and last years of the CLINO period
      ny <- as.integer(yrs[2])-as.integer(yrs[1])+1 #no. of years
      dv <- seq(as.Date(sprintf('%s-01-01',yrs[1])),
                as.Date(sprintf('%s-12-31',yrs[2])),by='1 day') #target dates vector
      mv <- strftime(dv,'%m') #months vector
      yv <- strftime(dv,'%Y') #years vector
      dl <- split(dv,mv) #list of dates split by months
      cln <- array(NA,c(150,12,nst)) #initialization of CLINO values
      mnm <- array(NA,c(150,12,nst)) #initialization of monthly minimum values
      mnd <- array(NA,c(150,12,nst)) #initialization of monthly minimum dates
      mxm <- array(NA,c(150,12,nst)) #initialization of monthly maximum values
      mxd <- array(NA,c(150,12,nst)) #initialization of monthly maximum dates
      xnd <- array(NA,c(150,12,nst)) #initialization of daily extreme dates
      noy <- array(NA,c(150,12,nst)) #initialization of no. of years with data
      ##################################
      #        Loop on variables
      ##################################
      for(kv in 1:nv) { #for every variable
        cat(sprintf('Processing %s data from file %s\n',vrb[kv,1],vrb[kv,2]))
        ddf <- vrb[kv,2] #daily data file
        if ( !(is.null(ddf)))
        {
        dah1 <- read.csv(ddf,header = TRUE, sep=",", check.names=FALSE,as.is=TRUE)
        dah=subset(dah1, dah1$dates >= as.Date(sprintf('%s-01-01',yrs[1])) & 
                     dah1$dates <= as.Date(sprintf('%s-12-31',yrs[2])) )
        #
        #  Head of the daily data file "dah"
        #     dates,60033
        #     1991-01-01,22.6
        #     1991-01-02,26.5
        #     1991-01-03,24.3
        #     1991-01-04,23.5
        #     1991-01-05,22
        #
        x <- as.Date(dah[,1]) #vector of dates in the data file
        stid <- names(dah)[2:ncol(dah)] # names of stations
        cat('\n names of stations from data file ',stid,'\n')
        dah <- dah[,2:ncol(dah)] #get rid of the dates column
        dah <- as.matrix(dah) #convert to matrix
        } # fin condition ddf is null
        #      dat <- dah #use homogenized data as the unknown raw data
        prm <- unlist(strsplit(vrb[kv,3],'-')) #parameters to calculate
        ##################################
        #        Loop on stations
        ##################################
        for(k in 1:nst) { #for every station:
          if ( !(is.null(ddf)))
          {          
          ke <- which(stid==stm[k,1])
          if(length(ke)==0) {
            #     cat(sprintf('No %s data for station %s\n',vrb[kv,1],stm[k,1]))
            next
          } 
          } #fin condition ddf is null
          MLYDataset = subset(MLYDatasetin, MLYDatasetin[,"Stcode"] == stm[k,1] & 
                                MLYDatasetin[,"YYYY"] >= yrs[1] & MLYDatasetin[,"YYYY"] <= yrs[2] )
          
          ##################################
          #        Loop on parameters
          ##################################
          for(i in 1:length(prm)) { #for every parameter:
            ComputefromMLY=FALSE
            
            qp <- which(pmt[,1]==prm[i]) #row no. in the parameter list
            
            if(length(qp)==0 ) {
              cat(sprintf('Parameter %s not found in CLINO_parameters.csv\n',
                          prm[i]))
              next
            }
            
            if ( pmt[qp,1] == 1) {pcol = 4}
            if ( pmt[qp,1] == 2) {pcol = 5}
            if ( pmt[qp,1] == 3) {pcol = 6}
            if ( pmt[qp,1] == 4) {pcol = 7}
            if ( pmt[qp,1] == 5) {pcol = 8}
            if ( pmt[qp,1] == 6) {pcol = 9}
            if ( pmt[qp,1] == 7) {pcol = 10}
            if ( pmt[qp,1] == 26) {pcol = 11}
            
            if ( !(is.null(ddf)))
            {           
            if(all(is.na(dah[,ke]))) {
              if (!(pmt[qp,1] %in% c(1,2,3,4,5,6,7,26)) | all(is.na(MLYDataset[,pcol])) )
               {  cat(sprintf('Parameter %s not found in CLINO_parameters.csv\n',
                          prm[i]))
                 next
              } else {
                ComputefromMLY=TRUE
              }
            } 
            } else {
              if (is.null(MLYDataset) | all(is.na(MLYDataset[,pcol])))
              {
                next
              } else {
                ComputefromMLY=TRUE
                }
            }
            
            #
            # calculate monthly data:
            #
            # pmt[qp,5] == Threshold
            # pmt[qp,4] == Monthly_function
            # dah[,ke] == data related to the selected station
            #
            # from daily to individual monthly values
            #    if min/max/mean : compute on all available data after completeness data criteria check (na.rm=TRUE)
            #
            # le format de la matrice md est comme suit 
            #
            #      Group.1 Group.2    RR
            # 1        01    1991  64.5
            # 2        02    1991  28.8
            # 3        03    1991  37.0
            # 4        04    1991  23.6
            #
            #
        if ( ComputefromMLY == FALSE)
        {
            if(is.na(pmt[qp,5])){
              if(pmt[qp,4]=='sum') {
                md <- aggregate(dah[,ke],list(mv,yv),pmt[qp,4])
              }else{
                md <- aggregate(dah[,ke],list(mv,yv),pmt[qp,4],na.rm=TRUE)
              }
            }else{
              md <- aggregate(dah[,ke],list(mv,yv),pmt[qp,4],pmt[qp,5])
            }
        } else {
              md <- MLYDataset[,c(2,3,pcol)]
        }   
            #  Convert daily data file to a maxtrix (year, month)
            #                                           J F M A M J J A S O N D
            #     1991-01-01,22.6                 1991
            #     1991-01-02,26.5    aggregate    1992  
            #     1991-01-03,24.3   ===>          ...     min/max/mean/count
            #     1991-01-04,23.5                 ...
            #     1991-01-05,22                   2000
            #
            md <- t(matrix(md[,3],12,ny))
            #
            #  For count parameter, compute ratio to reconvert it in number of days later
            #
        if ( ComputefromMLY == FALSE)
        {
            if (pmt[qp,3]=='count')
            {
              for ( f in c(1,3,5,7,8,10,12)) { md[,f]=md[,f]*31 }
              for ( f in c(4,6,9,11)) { md[,f]=md[,f]*30 }
              for ( f in c(2)) { md[,f]=md[,f]*28.25 }
            }
        }
            #
            # In some cases, for missing month max / min has infinite or NaN value
            # so, replace with NA
            #
            md[sapply(md, is.infinite)] <- NA
            md[sapply(md, is.nan)] <- NA
            #
            #  Function to compute number of years available to compute monthly normals
            #
            nb.no.missing.year <- function(x){
              return(length(which(!is.na(x))))
            } 
            
            which.min.na <- function(x){
              if (all(is.na(x)))
              {
                return(NA)
              } else {
                return(which.min(x))
              }
            } 
            
            which.max.na <- function(x){
              if (all(is.na(x)))
              {
                return(NA)
              } else {
                return(which.max(x))
              }
            }           
            
            #calculate CLINO values:
            if(pmt[qp,4]=='max'|pmt[qp,4]=='min') {
              cln[qp,,k] <- round(apply(md,2,pmt[qp,4],na.rm=TRUE),1)
            } else {
              cln[qp,,k] <- round(apply(md,2,mean,na.rm=TRUE),1)
            }
            mnm[qp,,k] <- round(apply(md,2,min,na.rm=TRUE),1) #MinMon 
            mnd[qp,,k] <- as.integer(yrs[1])-1+apply(md,2,which.min.na) #years
            mxm[qp,,k] <- round(apply(md,2,max,na.rm=TRUE),1) #MaxMon
            mxd[qp,,k] <- as.integer(yrs[1])-1+apply(md,2,which.max.na) #years 
            noy[qp,,k] <- apply(md,2,nb.no.missing.year) #no. of years with data    
            
            if(pmt[qp,4]=='max'|pmt[qp,4]=='min') {
              z <- tapply(dah[,ke],mv,ifelse(pmt[qp,4]=='max',which.max.na,which.min.na))
              for(j in 1:12) xnd[qp,j,ke] <- dl[[j]][z[j]]
            }
            if(qp==1) { #calculate monthly precipitation quintiles:
              z <- round(apply(md,2,quantile,probs=c(0,.2,.4,.6,.8,1), na.rm=TRUE),1)
              for(zk in 1:6) {
                zq <- match(sprintf('Q%d',zk-1),pmt[,2])
                cln[zq,,k] <- z[zk,]
                noy[zq,,k] <- noy[1,,k] #copy precipitation no. of years  
              }
            } # end loop on monthly quintiles calculation
          } # end loop on parameter
        } # end loop on station
      } #end loop on variable
      ###########################################################
      #----------------- write the CLINO values into CSV files:
      ###########################################################
      cat('\n ++++++++++>   Writing the Climate Normals into CSV and Excel files:\n\n')
      NCF <- file('Names-Created-Files.txt','w')
      sNCF <- file('Names-Created-Files-toshow.txt','w')
      for(k in 1:nst) { #for every station:
        #check if there are at least 80% (24 years) of observations:
        anoy <- apply(noy[,,k],1,min) #minimum annual no. of years
        ThresYLYMiss=ny - (YLYMissThresIn*ny*0.01)
        if(max(anoy[1:8],na.rm=TRUE) < ThresYLYMiss) {
          #     cat(sprintf('Station %d %s has more than %d percent of missing years in the used data (skipped)\n',
          #                  stm[k,2],stm[k,7],YLYMissThresIn))
          next
        } 
        stname <- gsub('[^[:alnum:]]','',stm[k,7])
        fname <- sprintf('%s_%d.csv',stname,stm[k,2])
        sfname <- sprintf('%s_%d_toshow.csv',stname,stm[k,2])        
        write(fname, NCF)
        write(sfname, sNCF)
        cat('Writing file',fname,'\n')
        Fs <- file(fname,'w') #open output file
        sFs <- file(sfname,'w') #open output file
        #------- write file header:------------------------
        write(sprintf('World Meteorological Organization Climate Normals for %s',period),Fs)
        write(sprintf('World Meteorological Organization Climate Normals for %s,,,,,,,,,,,,,,,,',period),sFs)
        # World Meteorological Organization Climate Normals for 1991-2020,,,,,,,,,,,,,,,,
        write('Single Station Data Sheet For All Climatological Surface Parameters',Fs)
        write('Single Station Data Sheet For All Climatological Surface Parameters,,,,,,,,,,,,,,,,',sFs)
        # Single Station Data Sheet For All Climatological Surface Parameters,,,,,,,,,,,,,,,,
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('Station Header Record',Fs)
        write('Station Header Record,,,,,,,,,,,,,,,,',sFs)
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write(sprintf('Country_Name,%s',stm[k,8]),Fs)
        write(sprintf('Country_Name,%s,,,,,,,,,,,,,,,',stm[k,8]),sFs)
        # Country_Name,Morocco,,,,,,,,,,,,,,,
        write(sprintf('Station_Name,%s',stm[k,7]),Fs)
        write(sprintf('Station_Name,%s,,,,,,,,,,,,,,,',stm[k,7]),sFs)
        # Station_Name,LAAYOUNE,,,,,,,,,,,,,,,
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('WMO_Number,Latitude,Longitude,Station_Height',Fs)
        write('WMO_Number,Latitude,Longitude,Station_Height,,,,,,,,,,,,,,',sFs)
        lat <- deg2ddmmss(stm[k,4],'lat')
        lon <- deg2ddmmss(stm[k,5],'lon')
        write(sprintf('%05d,%s,%s,%s',stm[k,2],lat,lon,stm[k,6]),Fs)
        write(sprintf('%05d,%s,%s,%s,,,,,,,,,,,,,',stm[k,2],lat,lon,stm[k,6]),sFs)
        # 60033,27|10|00|N,013|13|00|E,64.000000,,,,,,,,,,,,,
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('WMO Integrated Global Observing System (WIGOS) Station Identifier (if available)',Fs)
        write('WMO Integrated Global Observing System (WIGOS) Station Identifier (if available)',sFs)
        if ( is.na(stm[k,3])) {stm[k,3]=""}
        write(sprintf('%s',stm[k,3]),Fs)
        write(sprintf('%s,,,,,,,,,,,,,,,,',stm[k,3]),sFs)
        # 0-20000-0-60033,,,,,,,,,,,,,,,,
        write('',Fs)
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write(',,,,,,,,,,,,,,,,',sFs)
        #------- write values of calculated parameters:
        write('Principal Climatological Surface Parameters',Fs)
        write('Principal Climatological Surface Parameters,,,,,,,,,,,,,,,,',sFs)
        for(kp in 1:nrow(pmt)) { #for every parameter:
          qp <- pmt[kp,1] #parameter code
          if(qp>99) break #end of predefined parameters
          if(kp==9) {
            write('',Fs)
            write('',Fs)
            write('Secondary and Other Climatological Surface Parameters (add as needed)',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('Secondary and Other Climatological Surface Parameters (add as needed),,,,,,,,,,,,,,,,',sFs)
          }
          if(qp==11) { #precipitation quintiles go together:
            i <- match(11,pmt[,1])
            Qp <- match(110:115,pmt[,1]) # 110: Q0, 111: Q1, 112: Q2, 113: Q3, 114: Q4, 115: Q5
            write('',Fs)
            write('',Fs)
            write('Parameter_Code,Parameter_Name,Units',Fs)
            write(sprintf('%d,%s,%s',pmt[i,1],pmt[i,2],pmt[i,3]),Fs)
            write('',Fs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,',sFs)
            write(sprintf('%d,%s,%s,,,,,,,,,,,,,,',pmt[i,1],pmt[i,2],pmt[i,3]),sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',sFs)
            
            for(iq in Qp) {
              z <- sprintf('%05d,%d,%s,%s',stm[k,2],pmt[i,1],clc[iq-75,1],
                           clc[iq-75,2])
              for(j in 1:12) z <- paste(z,sprintf('%.1f',cln[iq,j,k]),sep=',')
              z <- paste(z,',',sep='')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
          #    write(paste(z,',',sep=''),FCs)              
            }
            z <- sprintf('%05d,11,NOY,98',stm[k,2])
            write(paste(z,paste(noy[1,,k],collapse=','),',',sep=','),Fs)
            write(paste(z,paste(noy[1,,k],collapse=','),',',sep=','),sFs)
    #        write(',,,,,,,,,,,,,,,,',Fs)
    #        write(',,,,,,,,,,,,,,,,',Fs)
            # Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,
            # 11,Boundaries_of_quintiles_of_monthly_precipitation,mm,,,,,,,,,,,,,,
            # ,,,,,,,,,,,,,,,,
            # WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual
            # 60033,11,Q0,6,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,
            # 60033,11,Q1,7,0.0,0.0,0.3,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.2,,
            # 60033,11,Q2,8,1.0,1.8,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.2,1.3,3.1,,
            # 60033,11,Q3,9,3.6,5.9,3.7,0.4,0.2,0.0,0.0,0.0,0.6,1.6,4.4,5.7,,
            # 60033,11,Q4,10,16.4,14.3,6.9,1.0,1.0,0.1,0.0,0.1,1.8,7.6,10.0,11.5,,
            # 60033,11,Q5,11,60.8,59.7,23.4,7.1,4.2,3.5,0.8,6.8,19.1,23.7,79.9,58.2,,
            # 60033,11,NOY,98,30,30,30,30,30,30,30,30,30,30,30,30,,
            next
          }
          #
          # check for multiple instances of the parameter:
          #
          # Variable,Daily_data_file,Parameters
          # Precipitation,RR_DLY.csv,1-2-160-161-162-163-164-24
          # Maximum_temperature,TX_DLY.csv,3-120-121-122-123-14-22
          #
          if(is.na(cln[kp,1,k])) { #check for multiple instances of the parameter:
            z <- qp*10+0:9 #possible codes of the parameter instances
            Qp <- match(z,pmt[,1])
            if(sum(!is.na(cln[Qp,1,k]))==0) next #no instances found
            Qp <- Qp[!is.na(Qp)]
          } else Qp <- qp
          
          # loop on the multiple instance of the studied parameter
          for(i in Qp) {
            if(is.na(anoy[i])) next #no data for this parameter
            if(anoy[i]<ThresYLYMiss) next #parameter with less than 24 years of observation
            write('',Fs)
            write('',Fs) 
            write('Parameter_Code,Parameter_Name,Units',Fs)
            write(sprintf('%d,%s,%s',pmt[i,1],pmt[i,2],pmt[i,3]),Fs)
            write('',Fs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs) 
            write('Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,',sFs)
            write(sprintf('%d,%s,%s,,,,,,,,,,,,,,',pmt[i,1],pmt[i,2],pmt[i,3]),sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',sFs)
            
            qc <- match(pmt[i,4],clcn) #calculation line
            if(is.na(qc)) qc <- match(pmt[i,3],clcn)
            z1 <- sprintf('%05d,%d',stm[k,2],pmt[i,1]) #WMO no. & parameter code
            z <- sprintf('%s,%s,%s',z1,clc[qc,1],clc[qc,2])
            for(j in 1:12) z <- paste(z,sprintf('%.1f',cln[i,j,k]),sep=',')
            if(is.na(pmt[i,6])) z <- paste(z,',',sep='')
            else z <- paste(z,sprintf('%.1f',round(eval(call(pmt[i,6],cln[i,,k]))
                                                   ,1)),sep=',')
            write(z,Fs)
            write(z,sFs)
     #       write(z,FCs)
            #
            #  Extremes Values
            #
            if(!is.na(xnd[i,j,k])) { #dates of extreme values:
              if(pmt[i,4]=='max') {
                z <- sprintf('%s,MaxDate,15',z1)
                jx <- which.max(cln[i,,k]) #month of maximum value
              } else {
                z <- sprintf('%s,MinDate,16',z1)
                jx <- which.min(cln[i,,k]) #month of minimum value
              }
              for(j in 1:12) z <- paste(z,sprintf('%s',
                                                  strftime(as.Date(xnd[i,j,k],origin='1970-01-01'),'%Y/%d')),sep=',')
              paste(z,sprintf('%s',strftime(as.Date(xnd[i,jx,k],
                                                    origin='1970-01-01'),'%Y/%m/%d')),sep=',')
              write(z,Fs)
              write(z,sFs)
              #
              #  Maximum and minimum monthly values
              #  
            } else if(!is.na(mnm[i,j,k])) { #maximum and minimum monthly values:
              z <- sprintf('%s,MinMon,17',z1)
              for(j in 1:12) z <- paste(z,sprintf('%.1f',mnm[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,DMinMon,18',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%d',mnd[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,MaxMon,19',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%.1f',mxm[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,DMaxMon,20',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%d',mxd[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
            }
            z <- sprintf('%05d,%d,NOY,98',stm[k,2],pmt[i,1])
            write(paste(z,paste(noy[i,,k],collapse=','),',',sep=','),Fs)
  #          write(',,,,,,,,,,,,,,,,',Fs)
  #          write(',,,,,,,,,,,,,,,,',Fs)
            }
        }
        close(Fs)
        close(sFs)
        
      } # loop on stations for writing the output files
      cat('\n')
#      close(FCs)
      close(NCF)
    } # end function CLINO
    
    
    #
    #  Contrôle de qualité des données
    #
    data.quality.control <- function(datin, configQC){
      QC <- vector()
      #
      # Tmax >= Tmin
      #
      QCout1=subset(datin,(datin[['TX']]-datin[['TN']])<=0)
      if ( dim(QCout1)[[1]] > 0){
        QCout1$flag <- "TX < TN" 
        QC = rbind(QC, QCout1)
      }
      #
      # Large value
      #
      for ( param in c("TX","TN","TM","RR","SLP","WVP","SH","SD","TH","WG","WS") )
      {
        upperlim=configQC[["upper.lim"]][configQC[["lab.parameter"]]== param]
        lowerlim=configQC[["lower.lim"]][configQC[["lab.parameter"]]== param]
        grande<-subset(datin,(datin[[param]] > upperlim | datin[[param]] < lowerlim))
        if ( dim(grande)[[1]] > 0){
          grande$flag[grande[[param]] > upperlim | grande[[param]] < lowerlim] <- paste0(param," Out of range / Hors fourchette") 
          QC = rbind(QC,grande)
        }
      }
      #
      # Duplicates dates
      #
      datedupli <- subset(datin, duplicated(datin)== TRUE)
      if ( dim(datedupli)[[1]] > 0){
        datedupli$flag = "duplicats"
        QC = rbind(QC,datedupli)
      }  
      #
      # Jumps with lag = 1 day
      #
      for ( param in c("TX","TN","TM","RR","SLP","WVP","SH","SD","TH","WG","WS"))
      {
  #      if ( param == "TX" |  param == "TN" |  param == "TM" ) {diff.thres = 14}
  #      if ( param == "RR" ) {diff.thres = 30}
  #      if ( param == "SLP" ) {diff.thres = 10}
        diff.thres=configQC[["jump.rate"]][configQC[["lab.parameter"]]== param]
        diftx <-abs(round(diff(datin[[param]], lag=1, differences=1),digits=1))
        tx.wdiff <- data.frame(datin,diff.val=c(NA,diftx))
        sub.tx.wdiff <- subset(tx.wdiff, tx.wdiff$diff.val > diff.thres)
        if ( dim(sub.tx.wdiff)[[1]] > 0) {
          sub.tx.wdiff$flag <- paste0(param," Jump of / Saut de ",sub.tx.wdiff$diff.val)
          sub.tx.wdiff$diff.val <- NULL
          QC = rbind(QC,sub.tx.wdiff)
        }
      }
      #
      # flatline 
      #
      for ( param in c("TX","TN","TM","RR","SLP","WVP","SH","SD","TH","WG","WS"))
      {
        # Threshold of consecutive equal values
        #flat.val=5
        flat.val=configQC[["flat.rate"]][configQC[["lab.parameter"]]== param]
        # occurrence of consecutive equal values sequence
        rnums=datin[[param]]
        runs = rle(rnums)
        
        # find indices of the runs with length of at least flat.val
        if ( param == "RR" | param == "SH" | param == "TH" | param == "SD"){
          myruns = which(runs$values > 0 & runs$lengths >= flat.val)
        } else {
          myruns = which(runs$lengths >= flat.val)
        }
        # Next, we can do a cumulative sum of the run lengths and extract the end positions 
        # of the runs with length of at least 5 using the above found indices.
        runs.lengths.cumsum = cumsum(runs$lengths)
        ends = runs.lengths.cumsum[myruns]
        # Next, we find the start positions of these runs.
        newindex = ifelse(myruns>1, myruns-1, 0)
        starts = runs.lengths.cumsum[newindex] + 1
        if (0 %in% newindex) starts = c(1,starts)
        # Lastly, we print out the start and end positions of these runs and use them to extract the runs themselves.
        if ( length(starts) > 0)
        {
          for (n in 1:length(starts))
          {
            sub.flat=datin[starts[n]:ends[n],]
            sub.flat$flag=paste0(param," duplicats / Mêmes valeurs")
            QC = rbind(QC,sub.flat)
          }
        }
      }

      return(QC)
    } 
    
    
    
    #######################################################################################
    # My Own Internal functions
    #######################################################################################
    #- CLINO.- Climatological Standard Normals calculation and CSV output files.
    #By Jose A. Guijarro (September 2021), under license GPL 3 or greater.
    #State Meteorological Agency (AEMET), Balearic Islands Office, Spain
    #Member of WMO Expert Team on Data Requirements for Climate services (ET-DRC).
    
    MCLINO <- function(period='1991-2020', YLYMissThresIn=20, MLYDatasetin) {
      #period: period from which to compute the Climatological Standard Normals
      #  (1991-2020 by default).
      #-----------------
      #This function needs four configuraton files (in CSV format, with header):
      # CLINO_stations.csv : stations metadata
      # CLINO_variables.csv : variables, file names and related parameters
      # CLINO_parameters.csv : list of CLINO parameters (predefined and custom)
      # CLINO_calculations.csv : calculation types and their CLINO codes
      #-----------------  
      #Daily homogenized data are provided in the files listed in CLINO_variables.csv
      #They can be the *.rda results of Climatol daily homogenizations or CSV files
      #  with stations daily data in columns, station codes in the header and a
      #  first column with dates in YYYY-MM-DD format.
      #----------------- Example:
      #An example can be run by uncompressing CLINO_example.tgz into our R working
      #directory and running:   source('CLINO.R'); CLINO()
      #----------------- Initial notice:
      #cat('\nCLINO.- Climatological Standard Normals calculation and CSV output files.\n')
      #cat('By Jose A. Guijarro (September 2021), under license GPL 3 or greater.\n')
      #cat('Directions of use are available at:\n\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-en.pdf (English)\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-fr.pdf (French)\n')
      #cat('   http://www.climatol.eu/CLINO/CLINO-es.pdf (Spanish)\n')
      #
      #----------------- Read input files:
      #
      cat('\n ++++++++++>   Reading input files...\n')
      stm <- read.csv('CLINO_stations.csv', sep = ",", dec = ".", as.is=TRUE) #stations metadata
      nst <- nrow(stm) #no. of stations
      cat('\n  The metadata file \n')
      vrb <- read.csv('CLINO_variables.csv',as.is=TRUE) #variables, files and par.
      nv <- nrow(vrb) #no. of variables
      cat('\n  The file listing the variables, daily files and parameters to compute  \n')
      pmt <- read.csv('CLINO_parameters.csv',as.is=TRUE) #parameters list
      cat('\n  The file listing all the recommended parameters by WMO  \n')
      clc <- read.csv('CLINO_calculations.csv',as.is=TRUE) #calculations list
      cat('\n  The file listing the calculation modes for the main meteorological elements \n')
      clcn <- tolower(clc[,1]) #calculation names
      #
      #----------------- Calculate the CLINO values:
      #
      cat('\n ++++++++++>   Calculation of the Climate Normals:\n\n')
      yrs <- unlist(strsplit(period,'-')) #first and last years of the CLINO period
      ny <- as.integer(yrs[2])-as.integer(yrs[1])+1 #no. of years
      dv <- seq(as.Date(sprintf('%s-01-01',yrs[1])),
                as.Date(sprintf('%s-12-31',yrs[2])),by='1 day') #target dates vector
      mv <- strftime(dv,'%m') #months vector
      yv <- strftime(dv,'%Y') #years vector
      dl <- split(dv,mv) #list of dates split by months
      cln <- array(NA,c(150,12,nst)) #initialization of CLINO values
      mnm <- array(NA,c(150,12,nst)) #initialization of monthly minimum values
      mnd <- array(NA,c(150,12,nst)) #initialization of monthly minimum dates
      mxm <- array(NA,c(150,12,nst)) #initialization of monthly maximum values
      mxd <- array(NA,c(150,12,nst)) #initialization of monthly maximum dates
      xnd <- array(NA,c(150,12,nst)) #initialization of daily extreme dates
      noy <- array(NA,c(150,12,nst)) #initialization of no. of years with data
      ##################################
      #        Loop on variables
      ##################################
      for(kv in 1:nv) { #for every variable
        prm <- unlist(strsplit(vrb[kv,3],'-')) #parameters to calculate
        ##################################
        #        Loop on stations
        ##################################
        for(k in 1:nst) { #for every station:
          MLYDataset = subset(MLYDatasetin, MLYDatasetin[,"Stcode"] == stm[k,1] & 
                                MLYDatasetin[,"YYYY"] >= yrs[1] & MLYDatasetin[,"YYYY"] <= yrs[2] )
          if ( is.null(MLYDataset) )
          {
            next
          }
          ##################################
          #        Loop on parameters
          ##################################
          for(i in 1:length(prm)) { #for every parameter:
            qp <- which(pmt[,1]==prm[i]) #row no. in the parameter list
            
            if(length(qp)==0 ) {
              cat(sprintf('Parameter %s not found in CLINO_parameters.csv\n',
                          prm[i]))
              next
            }
            
            if ( pmt[qp,1] == 1) {pcol = 4}
            if ( pmt[qp,1] == 2) {pcol = 5}
            if ( pmt[qp,1] == 3) {pcol = 6}
            if ( pmt[qp,1] == 4) {pcol = 7}
            if ( pmt[qp,1] == 5) {pcol = 8}
            if ( pmt[qp,1] == 6) {pcol = 9}
            if ( pmt[qp,1] == 7) {pcol = 10}
            if ( pmt[qp,1] == 26) {pcol = 11}
            if ( !(pmt[qp,1] %in% c(1,2,3,4,5,6,7,26)) ){
              next
            }
            #
            # calculate monthly data:
              md <- MLYDataset[,c(2,3,pcol)]

            #  Convert daily data file to a maxtrix (year, month)
            #                                           J F M A M J J A S O N D
            #     1991-01-01,22.6                 1991
            #     1991-01-02,26.5    aggregate    1992  
            #     1991-01-03,24.3   ===>          ...     min/max/mean/count
            #     1991-01-04,23.5                 ...
            #     1991-01-05,22                   2000
            #
            md <- t(matrix(md[,3],12,ny))
            #
            # In some cases, for missing month max / min has infinite or NaN value
            # so, replace with NA
            #
            md[sapply(md, is.infinite)] <- NA
            md[sapply(md, is.nan)] <- NA
            #
            #  Function to compute number of years available to compute monthly normals
            #
            nb.no.missing.year <- function(x){
              return(length(which(!is.na(x))))
            } 
            
            which.min.na <- function(x){
              if (all(is.na(x)))
              {
                return(NA)
              } else {
                return(which.min(x))
              }
            } 
            
            which.max.na <- function(x){
              if (all(is.na(x)))
              {
                return(NA)
              } else {
                return(which.max(x))
              }
            }           
            
            #calculate CLINO values:
            if(pmt[qp,4]=='max'|pmt[qp,4]=='min') {
              cln[qp,,k] <- round(apply(md,2,pmt[qp,4],na.rm=TRUE),1)
            } else {
              cln[qp,,k] <- round(apply(md,2,mean,na.rm=TRUE),1)
            }
            mnm[qp,,k] <- round(apply(md,2,min,na.rm=TRUE),1) #MinMon 
            mnd[qp,,k] <- as.integer(yrs[1])-1+apply(md,2,which.min.na) #years
            mxm[qp,,k] <- round(apply(md,2,max,na.rm=TRUE),1) #MaxMon
            mxd[qp,,k] <- as.integer(yrs[1])-1+apply(md,2,which.max.na) #years 
            noy[qp,,k] <- apply(md,2,nb.no.missing.year) #no. of years with data    
            
            if(qp==1) { #calculate monthly precipitation quintiles:
              if ( !all(is.na(MLYDataset[,4])))
              {
                z <- round(apply(md,2,quantile,probs=c(0,.2,.4,.6,.8,1), na.rm=TRUE),1)
                  for(zk in 1:6) {
                zq <- match(sprintf('Q%d',zk-1),pmt[,2])
                cln[zq,,k] <- z[zk,]
                noy[zq,,k] <- noy[1,,k] #copy precipitation no. of years  
                  }
              } # end condition on existence of RR
            } # end loop on monthly quintiles calculation
          } # end loop on parameter
        } # end loop on station
      } #end loop on variable
      ###########################################################
      #----------------- write the CLINO values into CSV files:
      ###########################################################
      cat('\n ++++++++++>   Writing the Climate Normals into CSV and Excel files:\n\n')
      NCF <- file('Names-Created-Files.txt','w')
      sNCF <- file('Names-Created-Files-toshow.txt','w')
      for(k in 1:nst) { #for every station:
        #check if there are at least 80% (24 years) of observations:
        anoy <- apply(noy[,,k],1,min) #minimum annual no. of years
        ThresYLYMiss=ny - (YLYMissThresIn*ny*0.01)
        if(max(anoy[1:8],na.rm=TRUE) < ThresYLYMiss) {
          next
        } 
        stname <- gsub('[^[:alnum:]]','',stm[k,7])
        fname <- sprintf('%s_%d.csv',stname,stm[k,2])
        sfname <- sprintf('%s_%d_toshow.csv',stname,stm[k,2])        
        write(fname, NCF)
        write(sfname, sNCF)
        cat('Writing file',fname,'\n')
        Fs <- file(fname,'w') #open output file
        sFs <- file(sfname,'w') #open output file
        #------- write file header:------------------------
        write(sprintf('World Meteorological Organization Climate Normals for %s',period),Fs)
        write(sprintf('World Meteorological Organization Climate Normals for %s,,,,,,,,,,,,,,,,',period),sFs)        
        # World Meteorological Organization Climate Normals for 1991-2020,,,,,,,,,,,,,,,,
        write('Single Station Data Sheet For All Climatological Surface Parameters',Fs)
        write('Single Station Data Sheet For All Climatological Surface Parameters,,,,,,,,,,,,,,,,',sFs)        
        # Single Station Data Sheet For All Climatological Surface Parameters,,,,,,,,,,,,,,,,
        write('',Fs)
        write('Station Header Record',Fs)
        write('',Fs)
        write(sprintf('Country_Name,%s',stm[k,8]),Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('Station Header Record,,,,,,,,,,,,,,,,',sFs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write(sprintf('Country_Name,%s,,,,,,,,,,,,,,,',stm[k,8]),sFs)
        # Country_Name,Morocco,,,,,,,,,,,,,,,
        write(sprintf('Station_Name,%s',stm[k,7]),Fs)
        write(sprintf('Station_Name,%s,,,,,,,,,,,,,,,',stm[k,7]),sFs)
        # Station_Name,LAAYOUNE,,,,,,,,,,,,,,,
        write('',Fs)
        write('WMO_Number,Latitude,Longitude,Station_Height',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('WMO_Number,Latitude,Longitude,Station_Height,,,,,,,,,,,,,,',sFs)
        lat <- deg2ddmmss(stm[k,4],'lat')
        lon <- deg2ddmmss(stm[k,5],'lon')
        write(sprintf('%05d,%s,%s,%s',stm[k,2],lat,lon,stm[k,6]),Fs)
        write(sprintf('%05d,%s,%s,%s,,,,,,,,,,,,,',stm[k,2],lat,lon,stm[k,6]),sFs)        
        # 60033,27|10|00|N,013|13|00|E,64.000000,,,,,,,,,,,,,
        write('',Fs)
        write('WMO Integrated Global Observing System (WIGOS) Station Identifier (if available)',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write('WMO Integrated Global Observing System (WIGOS) Station Identifier (if available)',sFs)
        if ( is.na(stm[k,3])) {stm[k,3]=""}
        write(sprintf('%s',stm[k,3]),Fs)
        write(sprintf('%s,,,,,,,,,,,,,,,,',stm[k,3]),sFs)        
        # 0-20000-0-60033,,,,,,,,,,,,,,,,
        write('',Fs)
        write('',Fs)
        write(',,,,,,,,,,,,,,,,',sFs)
        write(',,,,,,,,,,,,,,,,',sFs)
        #------- write values of calculated parameters:
        write('Principal Climatological Surface Parameters',Fs)
        write('Principal Climatological Surface Parameters,,,,,,,,,,,,,,,,',sFs)
        for(kp in 1:nrow(pmt)) { #for every parameter:
          qp <- pmt[kp,1] #parameter code
          if(qp>99) break #end of predefined parameters
          if(kp==9) {
            write('',Fs)
            write('',Fs)
            write('Secondary and Other Climatological Surface Parameters (add as needed)',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('Secondary and Other Climatological Surface Parameters (add as needed),,,,,,,,,,,,,,,,',sFs)
          }
          if(qp==11) { #precipitation quintiles go together:
            if ( !all(is.na(MLYDataset[,4])))
            {          
            i <- match(11,pmt[,1])
            Qp <- match(110:115,pmt[,1]) # 110: Q0, 111: Q1, 112: Q2, 113: Q3, 114: Q4, 115: Q5
            write('',Fs)
            write('',Fs)
            write('Parameter_Code,Parameter_Name,Units',Fs)
            write(sprintf('%d,%s,%s',pmt[i,1],pmt[i,2],pmt[i,3]),Fs)
            write('',Fs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,',sFs)
            write(sprintf('%d,%s,%s,,,,,,,,,,,,,,',pmt[i,1],pmt[i,2],pmt[i,3]),sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',sFs)
            for(iq in Qp) {
              z <- sprintf('%05d,%d,%s,%s',stm[k,2],pmt[i,1],clc[iq-75,1],
                           clc[iq-75,2])
              for(j in 1:12) z <- paste(z,sprintf('%.1f',cln[iq,j,k]),sep=',')
              z <- paste(z,',',sep='')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
            }
            z <- sprintf('%05d,11,NOY,98',stm[k,2])
            write(paste(z,paste(noy[1,,k],collapse=','),',',sep=','),Fs)
            write(paste(z,paste(noy[1,,k],collapse=','),',',sep=','),sFs)
#            write(',,,,,,,,,,,,,,,,',Fs)
#            write(',,,,,,,,,,,,,,,,',Fs)
            # Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,
            # 11,Boundaries_of_quintiles_of_monthly_precipitation,mm,,,,,,,,,,,,,,
            # ,,,,,,,,,,,,,,,,
            # WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual
            # 60033,11,Q0,6,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,
            # 60033,11,Q1,7,0.0,0.0,0.3,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.2,,
            # 60033,11,Q2,8,1.0,1.8,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.2,1.3,3.1,,
            # 60033,11,Q3,9,3.6,5.9,3.7,0.4,0.2,0.0,0.0,0.0,0.6,1.6,4.4,5.7,,
            # 60033,11,Q4,10,16.4,14.3,6.9,1.0,1.0,0.1,0.0,0.1,1.8,7.6,10.0,11.5,,
            # 60033,11,Q5,11,60.8,59.7,23.4,7.1,4.2,3.5,0.8,6.8,19.1,23.7,79.9,58.2,,
            # 60033,11,NOY,98,30,30,30,30,30,30,30,30,30,30,30,30,,
            next
            } # cond on MLYData
          } # end qp=11
          #
          # check for multiple instances of the parameter:
          #
          # Variable,Daily_data_file,Parameters
          # Precipitation,RR_DLY.csv,1-2-160-161-162-163-164-24
          # Maximum_temperature,TX_DLY.csv,3-120-121-122-123-14-22
          #
          if(is.na(cln[kp,1,k])) { #check for multiple instances of the parameter:
            z <- qp*10+0:9 #possible codes of the parameter instances
            Qp <- match(z,pmt[,1])
            if(sum(!is.na(cln[Qp,1,k]))==0) next #no instances found
            Qp <- Qp[!is.na(Qp)]
          } else Qp <- qp
          # loop on the multiple instance of the studied parameter
          for(i in Qp) {
            if(is.na(anoy[i])) next #no data for this parameter
            if(anoy[i]<ThresYLYMiss) next #parameter with less than 24 years of observation
            write('',Fs)
            write('',Fs) 
            write('Parameter_Code,Parameter_Name,Units',Fs)
            write(sprintf('%d,%s,%s',pmt[i,1],pmt[i,2],pmt[i,3]),Fs)
            write('',Fs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',Fs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write(',,,,,,,,,,,,,,,,',sFs) 
            write('Parameter_Code,Parameter_Name,Units,,,,,,,,,,,,,,',sFs)
            write(sprintf('%d,%s,%s,,,,,,,,,,,,,,',pmt[i,1],pmt[i,2],pmt[i,3]),sFs)
            write(',,,,,,,,,,,,,,,,',sFs)
            write('WMO_Number,Parameter_Code,Calculation_Name,Calculation_Code,January,February,March,April,May,June,July,August,September,October,November,December,Annual',sFs)
            
            qc <- match(pmt[i,4],clcn) #calculation line
            if(is.na(qc)) qc <- match(pmt[i,3],clcn)
            z1 <- sprintf('%05d,%d',stm[k,2],pmt[i,1]) #WMO no. & parameter code
            z <- sprintf('%s,%s,%s',z1,clc[qc,1],clc[qc,2])
            for(j in 1:12) z <- paste(z,sprintf('%.1f',cln[i,j,k]),sep=',')
            if(is.na(pmt[i,6])) z <- paste(z,',',sep='')
            else z <- paste(z,sprintf('%.1f',round(eval(call(pmt[i,6],cln[i,,k]))
                                                   ,1)),sep=',')
            write(z,Fs)
            write(z,sFs)
            #
            #  Extremes Values
            #
            if(!is.na(xnd[i,j,k])) { #dates of extreme values:
              if(pmt[i,4]=='max') {
                z <- sprintf('%s,MaxDate,15',z1)
                jx <- which.max(cln[i,,k]) #month of maximum value
              } else {
                z <- sprintf('%s,MinDate,16',z1)
                jx <- which.min(cln[i,,k]) #month of minimum value
              }
             for(j in 1:12) z <- paste(z,sprintf('%s',
                                                  strftime(as.Date(xnd[i,j,k],origin='1970-01-01'),'%Y/%d')),sep=',')
              paste(z,sprintf('%s',strftime(as.Date(xnd[i,jx,k],
                                                    origin='1970-01-01'),'%Y/%m/%d')),sep=',')
              write(z,Fs)
              write(z,sFs)              
              #
              #  Maximum and minimum monthly values
              #  
            } else if(!is.na(mnm[i,j,k])) { #maximum and minimum monthly values:
              z <- sprintf('%s,MinMon,17',z1)
              for(j in 1:12) z <- paste(z,sprintf('%.1f',mnm[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,DMinMon,18',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%d',mnd[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,MaxMon,19',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%.1f',mxm[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
              z <- sprintf('%05d,%d,DMaxMon,20',stm[k,2],pmt[i,1])
              for(j in 1:12) z <- paste(z,sprintf('%d',mxd[i,j,k]),sep=',')
              write(paste(z,',',sep=''),Fs)
              write(paste(z,',',sep=''),sFs)
            }
            z <- sprintf('%05d,%d,NOY,98',stm[k,2],pmt[i,1])
            write(paste(z,paste(noy[i,,k],collapse=','),',',sep=','),Fs)
            write(paste(z,paste(noy[i,,k],collapse=','),',',sep=','),sFs)
#            write(',,,,,,,,,,,,,,,,',Fs)
#            write(',,,,,,,,,,,,,,,,',Fs)
          }
       }
        close(Fs)
        close(sFs)
      } # loop on stations for writing the output files
      cat('\n')
      close(NCF)
    } # end function MCLINO
    