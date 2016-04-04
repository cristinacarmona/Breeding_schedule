#-------------------------------------------------------------------------------
#Helps generate std files for nests and captures
#CODE FROM /G:\Plovers\KP data management\Cleaning data code/care.R  Andras K. sent
#-------------------------------------------------------------------------------
#Mad start cleaning nests ONLY
#03/04/2016 - 1st log start cleaning, up to line 128 fixing floating categories
#04/04/2016 - 2nd log produced standard file, added estimated laying dates to all and hatching dates that Luke had calculated only for 2009-2015


##########attach datasets########################################
#Madagascar
setwd("F:/Plovers/3rd Chapter/input/Madagascar")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles


import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))


#str(import.list)
ls()

working.list <- import.list
#Ceuta: names(working.list) <- c("br1","br2","br","bf","cap1","cap","ne1","ne","re1","re","sex")
#Maio
names(working.list) <- c("bf1","br","br3","bf","cap3","cap","hatch","sex","ne","re")


attach(working.list)
#detach(working.list)

#---------------------------------------------------------------
##########################################################################

library(binom)
library(nlme)

length1 <- function(a) length(a[!is.na(a)])
se <- function(a) sd(a[!is.na(a)])/sqrt(length(a[!is.na(a)]))
rounded <- function(a) floor(a+0.5)

###############################################################################

str(bfa)#2572, 2260 without NAs
str(bre)#872
str(cap)#1699

names(nes)
nes<-ne[,c(1:28)]

str(nes)#608
str(sex)#582
str(surv) #2849

str(hatch)#795

table(nes$year)
nes$year <- as.numeric(nes$year)
nes <- nes[!is.na(nes$year),]

#############################################################################
################################## Nests ####################################
#############################################################################
str(nes)


unique(nes$fate)
table(nes$fate)
length(grep("HATCH",nes$fate,ignore.case=TRUE))#558
length(grep("HATCH",nes$fate))
table(nes$fate)
nes$fate[nes$fate%in%c("Hatch","HATCH")] <- "HATCH"
             x<-import.list[[8]]
             str(x)
             table(x$fate)
sum(nes$fate%in%"HATCH")#500

nes$end_date[nes$fate%in%"HATCH"]  #12 NAs
nes$end_date <- as.numeric(nes$end_date)
str(nes[nes$fate%in%"HATCH" & !is.na(nes$end_date),])#219 cases with known end_date


#CRISTINA 19-09-2014 - calculate unknown laying dates with floating stage------------------------------
  #check how many cases have laying date unknown and float unknown
  
  calc_laydate<-nes[is.na(nes$laying_date),]  #254 cases where laying date is unknown
  str(calc_laydate) #2212 cases where laying date is unknown
  sum(is.na(calc_laydate$float1))#176 are NAs
  str(calc_laydate[is.na(calc_laydate$float1) & is.na(calc_laydate$float2) & is.na(calc_laydate$float3),]) #74 with unknown float1, float2 and 3 in Maio
  str(calc_laydate[!is.na(calc_laydate$float1) | !is.na(calc_laydate$float2) | !is.na(calc_laydate$float3),]) #2037 obs with at least one floating known

#prepare data to calculate days being incubated
  #Floating stage was completed with information in comments when floating was unavailable,
   #e.g. no floating available but in comments it is mentioned that the eggs were pipping, pipping category was added #and pipping stage is considered as nearly hatched and therefore 25 days being incubated.
   
   nes[is.na(nes$float1)&!is.na(nes$comments), c('nest','year','float1','comments','observer')]
   #it was not possible to assign float1 based on comments
   
      
                                           
      
#check nests with NAs in clutch_size
          ind <- which(is.na(nes$clutch_size))
          str(nes[ind,]) #246
          
#nests with info in length and floating of eggs in 3 eggs can show 3 egg clutch sizes
          ind <- which(is.na(nes$clutch_size) & !is.na(nes$length1) & !is.na(nes$length2) & !is.na(nes$length3))
          nes[ind,]
          nes[ind,"clutch_size"] <- 3
          nes[ind,]             
                       
          ind <- which(is.na(nes$clutch_size) & !is.na(nes$length1) & !is.na(nes$length2) & is.na(nes$length3))
          nes[ind,"clutch_size"] <- 2
          nes[ind,]
    
          ind <- which(is.na(nes$clutch_size) & !is.na(nes$length1) & is.na(nes$length2) & is.na(nes$length3))
          nes[ind,"clutch_size"] <- 1
          
          ind <- which(is.na(nes$clutch_size))
          str(nes[ind,]) #27 remain without clutch_size
          nes[ind,]
          
          
    #finish preparing egg data--------------------------------------------------------------
    
  
  
  unique(nes$float1) #check which categories we have:
  table(nes$float1, useNA="always")                  #categories  Mad float1:
          # [1] "B"       NA        "BC"      "G"       "E"       "A"       "AB"      "CD"      "F"       "H"       "C"       "D"       "DE"     
          # [14] "EF"      "GH"      "FG"      "STARRED" "N/A"     "B/C"     "E/F"     "C/D"    
          nes[nes$float1 %in% "G",]
          nes[nes$float1 %in% "FG",]
          nes[nes$float1 %in% "STARRED",]#egg cracked and hatching
          nes[nes$float1 %in% "H",] #almost hatching
          
  unique(nes$float2)
  table(nes$float2, useNA="always")
  #                   #categories Mad float2:
  # [1] "B"       NA        "BC"      "EF"      "D"       "G"       "A"       "C"       "F"       "AB"      "CD"      "E"       "DE"     
  # [14] "FG"      "H"       "GH"      "STARRED" " B"      "N/A"     "B/C"     "E/F"     "D/E"     "C/D"    
  unique(nes$float3)
  table(nes$float3, useNA="always")
                    #categories Mad float3:
  # [1] NA        "A"       "C"       "G"       "3"       "1"       "D"       "F"       "30"      "BC"      "CD"      "E"       "B"      
  # [14] "AB"      "STARRED"  

    #change categories with wrong format
    nes$float1[nes$float1%in%c("B/C", "BC")] <- "C"
    nes$float1[nes$float1%in%c("C/D","CD")]<-"D"   #whenever a category was combined I used the maximum (except AB which does exist)
    nes$float1[nes$float1%in%c("EF", "E/F")]<-"F"
    nes$float1[nes$float1%in%c("DE")] <- "E" #NA needs to be changed to 0, the latest letter on the alphabet needs to be chosen from the three eggs to use floating info of the oldest egg
    nes$float1[nes$float1%in%c("N/A", NA)]<-0
    nes$float1[nes$float1%in%c("FG")]<-"G"
    nes$float1[nes$float1%in%c("GH")]<-"H"
    nes$float1[nes$float1 %in% "STARRED"]<-"H"
    # nes$float[nes$float1%in%c(NA)]<-0
    # nes$float[nes$float1%in%c("CD")]<-"D"

    nes$float2[nes$float2%in%c(" B")]<-"B"
    nes$float2[nes$float2%in%c("B/C", "BC")] <- "C"
    nes$float2[nes$float2%in%c("C/D","CD")]<-"D"
    nes$float2[nes$float2%in%c("DE", "D/E")] <- "E"
    nes$float2[nes$float2%in%c("EF", "E/F")]<-"F"
    nes$float2[nes$float2%in%c("FG")]<-"G"
    nes$float2[nes$float2%in%c(NA, "N/A")] <-"0"
    nes$float2[nes$float2%in%c("GH")]<-"H"
    nes$float2[nes$float2 %in% "STARRED"]<-"H"

    nes$float3[nes$float3%in%c(NA)] <- "0"
    #nes$float3[nes$float3%in%c("JUST F", "just F")] <- "F"
    nes$float3[nes$float3%in%c("CD")]<-"D"
    nes$float3[nes$float3%in%c("BC")]<-"C"
    nes$float3[nes$float3 %in% "STARRED"]<-"H"
    nes$float3[nes$float3 %in% "30"]<-"B"
    nes$float3[nes$float3 %in% "3"]<-0 #ignore....rest of eggs were about to hatch
    nes$float3[nes$float3 %in% "1"]<-"E"
   

    unique(nes$float1) #check if categories are now fine
    unique(nes$float2)
    unique(nes$float3)

    #give the maximum floating stage of all 3 eggs
    float_max<-apply(nes[,c("float1","float2","float3")],1, function(x) max(x,na.rm=T))
    
    str(float_max)
    float_max<-as.character(float_max)
    
    nes.1<-cbind(nes,float_max)
    nes<-nes.1
    
    nes[c("float1","float2","float3","float_max")]
    #####################FUNCTION FOR CALC LAYING DATE FROM FLOATING#####################
    #Calculate number of days since laying according to floating stage
    #Use maximum floating stage of all 3 eggs
         calc_days_stage = function(float)
                      {ifelse (float=="A", 0,
                                            {ifelse (float=="AB", 1,
                                            ifelse(float=="B", 2,
                                            ifelse(float=="C", 5,
                                            ifelse(float =="D", 8,
                                            ifelse(float=="E", 10,
                                            ifelse(float=="F", 11,
                                            ifelse(float=="G", 20,
                                            ifelse(float=="F+2", 21,
                                            ifelse(float=="F+3", 22,
                                            ifelse(float=="H", 24
                                            ,"NA"))))))))))})}
    #############################END OF FUNCTION#############################
    #Apply calc_days_stage function
    nes$days_incubated<-calc_days_stage(nes$float_max) #apply function to know number of days since laying
    nes$days_incubated<-as.numeric(nes$days_incubated)
    str(nes)
    nes[c(1:5),] #check first 5 cases to see if function does calculate correctly
    nes$days_incubated[1:5]

    ###################FUNCTION: convert dates to normal date format#####################
         dateconversion=function(x,y)     #dates in the format x= 101(monthday), y=Year
            {ifelse(nchar(x)<4,
                      { paste(substr(x,2,3),'-0',substr(x,1,1),'-',y, sep="")
                      },
                      { paste(substr(x,3,4),'-',substr(x,1,2),'-',y, sep="")})
                        }
    #####################END OF FUNCTION###################################################

    #convert Found date to normal date
    nes$found_as_date<-dateconversion(nes$found_date, nes$year)
    nes$found_as_date<-as.Date(nes$found_as_date, "%d-%m-%Y")
    #nes$floating_date_asdate<-dateconversion(nes$floating_date, nes$year)
    #nes$floating_date_asdate<-as.Date(nes$floating_date_asdate, "%d-%m-%Y")

    #subtract number of days incubated to the founddate
    str(days_incubated)
    str(nes$floating_date_asdate)
    #nes$estimated_layingdate <- ifelse(!is.na(nes$floating_date), nes$floating_date_asdate-nes$days_incubated, nes$found_as_date-nes$days_incubated)
    nes$estimated_layingdate <- ifelse(!is.na(nes$days_incubated), nes$found_as_date-nes$days_incubated, NA)
    
    nes$estimated_layingdate<-as.Date(nes$estimated_layingdate, origin="1970-01-01")

    names(nes)
          #check if estimations of nests with floating date different from found date are correct
         # nes[nes$year==2013 & !is.na(nes$floating_date_asdate), c("floating_date_asdate","found_as_date","days_incubated","estimated_layingdate")]

#-----------debug-------
head(nes)          
#----------------------------
    nes[c(1:5),]
    nes[nes$year==2013,c("found_date","found_as_date","days_incubated", "laying_date","estimated_layingdate")] #check if calculations were done correctly

    #how many new estimations were calculated:
    str(nes[!is.na(nes$estimated_layingdate)& is.na(nes$laying_date),]) #2037 observations with no laying date but where laying date was able to be estimated
    str(nes[is.na(nes$estimated_layingdate) & is.na(nes$laying_date),]) #still 175 obs without laying date


    #check if estimated_laying date is the same as laying_date of nests where it had been estimated before
    nes$laying_date<-as.numeric(nes$laying_date)
    nes$laying_as_date<-dateconversion(nes$laying_date, nes$year)
    nes$laying_as_date<-as.Date(nes$laying_as_date, "%d-%m-%Y")
    nes[,c("laying_as_date","laying_date")]
    str(nes[!is.na(nes$laying_as_date)& !is.na(nes$estimated_layingdate)&
        nes$laying_as_date==nes$estimated_layingdate, c("laying_as_date","estimated_layingdate")])    #in 207 obs estimations are equal
    str(nes[!is.na(nes$laying_as_date)& !is.na(nes$estimated_layingdate)&
        nes$laying_as_date!=nes$estimated_layingdate, c("laying_as_date","estimated_layingdate")])    #in 365 obs estimations are different

            #to check consistency of new estimated laying dates:
            #estimate difference in days between estimated_layingdate and laying_as_date in obs where it was different
             nes$difference<-nes$laying_as_date-nes$estimated_layingdate

             nes[!is.na(nes$difference)& nes$difference>2 | !is.na(nes$difference) & nes$difference<=(-2),c("laying_as_date","estimated_layingdate","difference","float1","float2","float3")] #obs larger than 2 and smaller than -2

             fivenum(nes$difference)
             table(nes$difference)
             differences.nona<-nes[!is.na(nes$difference),c("difference")]
             differences.nona<-as.numeric(differences.nona)
             stem(differences.nona)
              
             #replace new estimated laying date in cases where no laying date was provided
             nes$estimated_layingdate<-as.Date(nes$estimated_layingdate, "%Y-%m-%d")
             nes$new_laying_as_date<-ifelse(is.na(nes$laying_date), nes$estimated_layingdate, nes$laying_as_date)
             nes$new_laying_as_date<-as.Date(nes$new_laying_as_date, origin="1970-01-01")
             nes[is.na(nes$laying_as_date), c("laying_date","laying_as_date","estimated_layingdate","new_laying_as_date")]
             nes[!is.na(nes$laying_date), c("laying_date","laying_as_date","estimated_layingdate","new_laying_as_date")]

#change back: float1,2,3 == 0 should be returned to NA
nes$float1[nes$float1==0] <- NA
nes$float2[nes$float2==0] <- NA
nes$float3[nes$float3==0] <- NA

names(nes)
nes$new_laying <- as.character(nes$new_laying_as_date)
nes$new_laying_std <- ifelse(!is.na(nes$new_laying), paste(substr(nes$new_laying, 6,7), substr(nes$new_laying, 9,10), sep=""), nes$new_laying)

nes$new_laying_std

#Replace laying date only in cases where laying date was unknown:
nes$laying_date <- ifelse(is.na(nes$laying_date), nes$new_laying_std, nes$laying_date)
nes$laying_date <- as.numeric(nes$laying_date)
nes[1:10,]

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#ADD HATCHING DATE TO FILE USING HATCHING DATES CALCULATED BY LUKE-----------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
str(hatch)
hatch$Hatch_date <- as.Date(hatch$Hatch_date, "%d/%m/%Y" )
hatch_dates <- aggregate(hatch$Hatch_date, by = list(hatch$id.nest), min)
no_chicks<-aggregate(hatch$Ring, by=list(hatch$id.nest), function (x) length(unique(x)))


str(hatch_dates) #529, 202 from nests found, 327 negative broods
str(hatch_dates[grep(hatch_dates$Group.1, pattern="\\-\\-"),])#327 negative broods

nes$hatch_date<-NA

nes$id.nest <- paste(nes$year, nes$species, nes$nest, sep="-")
table(nes$species)
nes$species[nes$species %in% "KIP"] <- "KiP"
nes$species[nes$species %in% "WFP"] <- "WfP"
nes$species[nes$species %in% "WP"] <- "WfP"

nes$hatch_date[nes$site %in% "Andavadoaka"] <- hatch_dates$x[match(nes$id.nest[nes$site%in% "Andavadoaka"],hatch_dates$Group.1)]
nes$hatch_date <- as.Date(nes$hatch_date, origin="1970-01-01")

nes$no_chicks_captured<-NA
nes$no_chicks_captured[nes$site %in% "Andavadoaka"]<-no_chicks$x[match(nes$id.nest[nes$site%in% "Andavadoaka"],no_chicks$Group.1)]

str(nes[!is.na(nes$hatch_date),])#on 187 nests Hatching date was added....15 nests not found??

#see which nests were not found?
ind<-grep(hatch_dates$Group.1, pattern="\\-\\-")
nests.with.hatchdate<-hatch_dates[-ind, "Group.1"]

nests.in.nes<-nes$id.nest[nes$site %in% "Andavadoaka" & nes$year>2008 & nes$fate %in% "HATCH"]

setdiff(nests.with.hatchdate, nests.in.nes)#20 not present in nes file
# [1] "2009-MP-12B"         "2010-KiP-"           "2010-KiP-3"          "2010-KiP-FAMILY"    
# [5] "2010-KiP-KTP"        "2010-KiP-KTP-FAMILY" "2010-MP-4"           "2012-KiP-10a"       
# [9] "2012-KiP-FALSE"      "2012-MP-"            "2013-KiP-402"        "2014-KiP-"          
# [13] "2014-KiP-0"          "2014-KiP-7a"         "2014-KiP-9a"         "2014-TP-"           
# [17] "2014-WfP-"           "2015-KiP-622=507"    "2015-KiP-KEXP1"      "2015-KiP-KEXP2"


#-------------------debug------------------
#nests with hatch date should have fate = HATCH
names(nes)
nes[!is.na(nes$hatch_date), c("id.nest","fate","no_chicks","hatch_date", "no_chicks_captured")]

#------------------update fate and no_chicks-----------------

nes$fate[!is.na(nes$hatch_date)] <- "HATCH"
nes$no_chicks[!is.na(nes$hatch_date)]<- nes$no_chicks_captured[!is.na(nes$hatch_date)]

#--------------------------------------------------------------


#--------------End of modification by Cristina 19-09-2014


# ind <- which(is.na(suppressWarnings(as.numeric(nes$nest))))
# nes[ind,c("year","site","nest","observer","comments")]
# #   Year             Site Nest Obs         Comment
# #70 2009    Lagoa Cimidor   C1 AAT   Lagoa Cimidor
# #71 2009    Lagoa Cimidor   C2 AAT   Lagoa Cimidor
#72 2009    Lagoa Cimidor   C3 AAT   Lagoa Cimidor
#73 2009 Ribeira do Lagoa   B1 AAT Ribera do Lagoa
#74 2009   Ribera do Joao   J1 AAT  Ribera do Joao

# nes[nes$site%in%"Lagoa Cimidor",]
# nes[nes$site%in%"Ribeira do Lagoa",]
# nes[nes$site%in%"Ribera do Joao",]
# #Characters can be removed
# 
# nes$nest[ind] <- sub("^[CBJ]","",nes$nest[ind]) #to supress characters from nest
# nes$nest <- as.integer(nes$nest)
# 
# nes <- nes[with(nes,order(year,site,nest)),]
# 
# ind <- which(nchar(nes$site)>1)         #change Salina to S in Site
# ind<- which(nes$site=="Salina")
# nes[ind,c("year","site","nest","observer","comments")]
# nes$site[ind]<-paste("S")
# nes$site[ind]
# nes$site

#finished running code up to here with Maio Data 01/04/2015
#second log 28/04/2015

# 
# #-------------------------- Incubation length --------------------------- -------
# #----------------- Could be used to fill more hatch dates ----------------------
# 
# #######Cristina's modifications to previous Andras' code:
# #             - instead of using laying_date I used new_laying_date (which includes laying dates calculated with floating stage for those where laying date was unknown)
# 
# fivenum(nes$end_date)#419 1011 1022 1105 1208
# fivenum(nes$laying_date)#324  922 1003 1015 1129
# 
# #Unlikely lay- or enddate
# sum((!is.na(nes$end_date) & nes$end_date<901)|(!is.na(nes$laying_date) & nes$laying_date<901))#12
# 
# names(nes)
# nes[(!is.na(nes$end_date) & nes$end_date<901) |
#     (!is.na(nes$laying_date) & nes$laying_date<901),
#     names(nes)%in%c("year","site","nest","found_date","found_time","laying_date","end_date",
#                     "fate","observer")]
#     nes[nes$year==2013 & nes$site=="S" & nes$nest==103,]
# 
# # year site nest found_date found_time laying_date end_date      fate observer
# # 38  2008   PP    1        429        900         422       NA Abandoned      RJB
# # 39  2008   PP    2        502       1100         501      523 Abandoned      AAT
# # 40  2008   PP    3        506       1600         502      524 Abandoned      AAT
# # 41  2008   PP    4        520        900         510      601      <NA>      AAT
# # 42  2008   PP    5        520        930         512      603      <NA>      AAT
# 43  2008   PP    6        520       1042         426      520     HATCH      AAT
# 44  2008   PP    7        520       1501         519       NA      <NA>      AAT
# 45  2008   PP    8        522        830         513       NA      <NA>      AAT
# 46  2008   PP    9        523       1530         514       NA      <NA>      AAT
# 448 2013    S  103        923       1645         830      924     HATCH       AT
# 529 2014    M    1        404        856         324      419     HATCH       AT
# 530 2014    S    1        618       1111          NA      702   Unknown       AT

#I THINK THESE ARE TYPOS!!!! #----CRISTINA's comment: Araceli was in Maio in the spring, so these laying dates are correct
# # #Laydate after Enddate
# sum((!is.na(nes$end_date) & !is.na(nes$laying_date) & nes$laying_date>nes$end_date))#1
# nes[!is.na(nes$end_date) & !is.na(nes$laying_date) & nes$laying_date>nes$end_date,
#      names(nes)%in%c("year","site","nest","found_date","found_time","new_laying_date","laying_date","end_date",
#                      "fate","observer", "float1", "float2","float3","days_incubated")]
# # 
# #    # year   site nest found_date found_time laying_date end_date      fate  float1 float2 float3 observer days_incubated new_laying_date
# # #394 2012 Salina  102        924        730        1025 (925?)     1012 Abandoned    0      0      0       TS             NA            1025       ***CHECK Tamas notes
# #   #comment - laying date should be unknown given that when the nest was found the eggs were cold and not floated
# # 
# # nes$laying_date[393] <- NA
# # nes$comments[393] <- "Laying date was captured as 1025 but on the found_date the eggs were cold and eggs were not floated, laying date could not have been calculated (CCI changed laying date to NA 28/04/2015)" 
# # nes[393,]
# 
# 
# #Founddate after Enddate
# sum((!is.na(nes$end_date) & !is.na(nes$found_date) & nes$found_date>nes$end_date))#0
# nes[!is.na(nes$end_date) & !is.na(nes$found_date) & nes$found_date>nes$end_date,
#     names(nes)%in%c("year","site","nest","found_date","found_time","laying_date","end_date",
#                     "fate","observer")]
# 
#                     ISOdate(nes$year,nes$end_date%/%100,nes$end_date%%100)
# 
# nes$SeEnddate <- as.integer(difftime(
#                             ISOdate(nes$year,nes$end_date%/%100,nes$end_date%%100),
#                             ISOdate(nes$year,1,1)))
# nes$SeEnddate
# nes$SeLaydate <- as.integer(difftime(
#                             ISOdate(nes$year,nes$laying_date%/%100,nes$laying_date%%100),
#                             ISOdate(nes$year,1,1)))
#  names(nes)
# 
# 
# ind <- which(with(nes,!is.na(SeLaydate) & !is.na(SeEnddate) & fate%in%"HATCH"))
# length(ind)#214 nests that hatched and have layingdate and enddate
# fivenum(nes$SeEnddate[ind]-nes$SeLaydate[ind])
# # 11 22 25 27 48
# 
# 
# nes$hatch_date<-(nes$SeLaydate+25)
# nes[ind,names(nes)%in%c("hatch_date","SeEnddate","SeLaydate")]
# 
# fivenum(nes$hatch_date-nes$SeEnddate)
# #[1] -23  -1   3  10  38
# str(nes[nes$hatch_date-nes$SeEnddate<0 & !is.na(nes$SeEnddate) & !is.na(nes$hatch_date),]) 
#     #113 negative hatchdate minus enddate, could be cases where first chick hatched and the left eggs didn't hatch
# 
# 
# nes[nes$fate %in% "HATCH" & is.na(nes$no_chicks),c("year","nest","site","fate", "clutch_size","no_chicks", "observer")]
#   #nests which hatch but number of chicks is unknown!
# #     year nest site  fate clutch_size no_chicks observer
# # 2   2007    1    S HATCH           3      <NA>       TS
# # 5   2007    4    S HATCH           3      <NA>       TS
# # 6   2007    5    S HATCH           3      <NA>       TS
# # 10  2007    9    S HATCH           3      <NA>       TS
# # 24  2007   23    S HATCH           3      <NA>       AC
# # 49  2008    3    S HATCH           3      <NA>       TS #looked in notes 28/04/2015, no further info found
# # 153 2009  301    S HATCH           3      <NA>     VLPC ####2 chicks were ringed, 3rd chick possibly catched but VLPC ended fieldwork before
# 
# #changed info:
# nes[152,]
# nes$comments[152] <- "2 chicks were ringed, 3rd chick possibly hatched but VLPC ended fieldwork before (comment added by CCI 28/04/2015)"
# nes$no_chicks[152] <- 2
# 
# nes[is.numeric(nes$fate),]
# names(nes)
# 
# #change AG to AT which refers to Alex Gonzales Tavares...same person...
# table(nes$observer)
# nes$observer[nes$observer%in%"AG"]<- "AT"

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----------create new nest standard file with changes implemented--------------
#-------------------------------------------------------------------------------
names(nes)
head(nes)  

getwd()
  
  nes.save <- nes[,c(1:39)]
  head(nes.save)
  setwd("F:/Plovers/3rd Chapter/input/Madagascar/")
  write.csv(nes.save, "Nests_Madagascar_2002-2015_stdfile.csv") 
  

# 
# #-------------------------------------------------------------------------------
# #-------------------------------------------------------------------------------
# #----------correct typos found in nest std file 12/08/2015:
# getwd()
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/input")
# csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
# 
# nes <- read.csv("Nests_Maio_2009-2014_stdfile 21May2015.csv",header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
# #----------------------------------------------------
# # 2010-S-108 ,/ **needs correction #* laying date is wrong, nest was found on 918 with 1 egg, on 921 2 eggs were floated and on 930 3 eggs were found..  
# nes[nes$year=="2010" & nes$nest=="108","comments_stdfile"] <- "laying_date is wrong, nest was found on 918 with 1 egg, on 921 2 eggs were found and on 930 3 eggs were found, LD should be something between 921 and 930"
# 
# # 2012-S-211 ,/ **needs correction # End date of 2012-S-211 should be 1025 which was the date when chicks were ringed. Otherwise time between laying date and hatching would be 48 days
# nes[nes$year=="2012" & nes$nest=="211", "end_date"] <- 1025
# nes[nes$year=="2012" & nes$nest=="211", "comments_stdfile"]<-"typo found in end_date, was 1125 but notes were checked (CCI 2015) "
# 
# # 2013-S-206 ,/ **needs correction # Floating date of 2013-S-206 should be 1017, so laying date is wrong
# nes[nes$year=="2013" & nes$nest =="206" & nes$site=="S", "floating_date"] <- 1017
# nes[nes$year=="2013" & nes$nest =="206" & nes$site=="S",] #floating C = 5 days, 1017-5days = 1012 laying date
# nes[nes$year=="2013" & nes$nest =="206" & nes$site=="S","laying_date"] <- 1012
# 
# #----------------------------------------------------
# #-----------create new nest standard file with changes implemented for v5--------------
# #-------------------------------------------------------------------------------
# 
# getwd()
# names(nes)
# nes.save <- nes
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# write.csv(nes.save, "Nests_Maio_2009-2014_stdfile_12Aug2015.csv") 
# 
# 
# 
# #-------------------------------------------------------------------------------
# 
# ###############################################################################
# ######################## Captures file###################################
# ###############################################################################
# str(cap)#1699 obs
# 
# #change AG to AT
# table(cap$observer)
# cap$site[cap$site %in% "Salina"] <- "S"
# cap$observer[cap$observer %in% "AG"] <- "AT"
# 
# #-------------------------- Non-integer nest in cap ---------------------------
# ind <- which(is.na(suppressWarnings(as.numeric(cap$nest))))
# cap[ind,c("year","site","nest","observer","comments")]
# 
# #N/A can be removed
# cap$nest[cap$nest%in%"N/A"] <- NA
# 
# 
# ind <- which(is.na(suppressWarnings(as.numeric(cap$nest))))
# unique(cap$nest[ind])
# # [1] NA     "2"    "1"    "3"    "-1"   "-3"   "-2"   "-4"   "bs-1" "bs-2" "bs-3" "bs-4" "bs-5" "bs-6" "bs-7"
# #[16] "bs-8"
# 
# cap[cap$site%in%"C",]
# cap[cap$site%in%"B",]
# cap[cap$site%in%"Morro beach",]
# cap[cap$site%in%"Morro",]
# 
# #Characters CBM can be removed but not bs-
# 
# cap$nest[ind] <- sub("^[CBM]","",cap$nest[ind])
# 
# 
# unique(cap$nest)#only bs- characters present
# 
# #removing captures without nest id 
#             #NOTHING CAN BE DONE 28/04/2015
# str(cap[is.na(cap$nest),c("year", "site","nest","sex","date","ring","observer","comments")]) #Maio: 21 captures without nest number
# 
# # year   site nest  sex date   ring  observer
# # 387  2009    RDL   NA    F  415 CA1615        TS
# # 388  2009      S   NA    F  421 CA1616        TS
# # 389  2009      S   NA    F  428 CA1616        TS
# # 391  2009      S   NA    M  428 CA1617        TS
# # 503  2009      S   NA    M  428 CA2656        TS
# # 571  2009      S   NA    J 1030 CA2735      VLPC#
# # 586  2009      S   NA    J 1113 CA2749      VLPC#
# # 652  2009      S   NA    M 1129 CA3050       EAR #not in EAR's notes from 2009 in Bath
# # 679  2010      S   NA    M 1007 CA1677        IF#
# # 777  2010      S   NA <NA> 1019 CA3110       JMH#LOST RING
# # 1089 2012 Salina   NA    F 1111 CA2629        AT#No nest in notes
# # 1473 2014      S   NA    J  215 CA3933        AT
# # 1474 2014      S   NA    J  215 CA2784        AT
# # 1475 2014      S   NA    J  306 CA3880 AT and JR
# # 1476 2014      S   NA    J  306 CA2799 AT and JR
# # 1477 2014      S   NA    J  402 CA2798 AT and JR
# # 1478 2014      S   NA    J  402 CA2797 AT and JR
# # 1489 2014      S   NA    J  605 CA2787 AT and JR
# # 1491 2014      S   NA    M  617 CA2619        AT
# # 1492 2014      S   NA    J  617 CA2786        AT
# # 1493 2014      S   NA    J  618 CA2785 AT and JR
# 
# 
# cap <- cap[with(cap,order(ring,year,date)),]
# str(cap)
# 
# #--------------------------- Sex characters variable in cap -------------------------------
# unique(cap$sex)
# 
# cap[cap$sex%in%"j",]
# cap$sex[cap$sex%in%"j"] <- "J"
# 
# jcap <- cap[cap$sex %in% "J",]
# str(jcap)#921
# 
# #-------Check colour code format: 11 characters, no spaces---------------------Inserted by Cristina 6Nov2014
# cap[nchar(cap$code)!=11, c("year","site","nest","sex","date","time","ring","code","observer","comments")]
# #codes with more or less than 11 characters:
# #      year   site nest  sex date time   ring         code observer                                 comments
# #73   2007      S   35    J 1122  928 CA1118           JJ       AK                                     <NA> #checked AK scanned notes it code should be MX.XX|YX.XX
# #108  2007      S    9    J 1016  724   <NA>   BX.XX|X.XX       PL                                     <NA>
# #374  2009      S   43    F 1121 1210 CA1137 MX.OX.|OX.RX     VLPC field sexed as M, molecularly sexed as F
# #777  2010      S   NA <NA> 1019 1525 CA3110         <NA>      JMH                                LOST RING
# #1038 2010      S  307    J 1207 1731 CA3736   XX.MX|X.WX       IF                                     <NA>
# #1071 2012 Salina   17    F 1015 1246 CA1080   MX.OXLX.GX       LV               ringed from previous years
# #1116 2012 Salina  -31    M 1214  824 CA2773   OX.MX|W.LX       LV                                     <NA>
# #1196 2012 Salina  -19    J 1122  903 CA3683 MX.XX.|YX.XX       LV                                     <NA>
# 
#     #correct errors one by one done 28/04/2015
#     cap$code[cap$code%in%"MX.OXLX.GX"]<-"MX.OX|LX.GX"
#     cap$code[cap$code%in%"MX.OX.|OX.RX"]<-"MX.OX|OX.RX"
#     cap$code[cap$code%in%"OX.MX|W.LX"]<-"OX.MX|WX.LX"
#     cap$code[cap$code%in%"MX.XX.|YX.XX"]<-"MX.XX|YX.XX"
#     cap$code[cap$code%in%"XX.MX|X.WX"]<-"XX.MX|XX.WX"
#     cap$code[cap$code%in%"BX.XX|X.XX"]<-"BX.XX|XX.XX"
#     cap$code[cap$code%in%"JJ"]<-"MX.XX|YX.XX"
#     cap[1071,]
#     cap[170:175,]
# 
#     cap[nchar(cap$code)!=11, c("year","site","nest","sex","date","time","ring","code","observer","comments")]
# 
# 
# #-------Check for consistency in sex and colour code in adults with multiple captures----Inserted by Cristina 5Nov2014
# adultcap<-cap[cap$sex %in%c("M","F") & !is.na(cap$ring),] #include only adults with metal ring
# unique(adultcap$ring)
# 
# captures.per.adult<-as.data.frame(table(adultcap$ring))
# captures.per.adult[captures.per.adult$Freq>1,]
# select.rings<-captures.per.adult[captures.per.adult$Freq>1,]
# rings.to.choose<-as.data.frame(select.rings$Var1)
# names(rings.to.choose)<-"rings"
# cap.more<-adultcap[adultcap$ring %in%rings.to.choose$rings,]#include only cases with more than one capture
# cap.more
# 
#     #Same sex throughout all captures of same individual?
#     cap.more$sex.ring<-paste(cap.more$ring, cap.more$sex, sep="-") #combine sex and ring in one variable
#     cap.more$sex.ring
#     samering<-duplicated(cap.more[,"ring"])
#     samering
#     samesex.ring<-duplicated(cap.more[,"sex.ring"])
#     samesex.ring
#     error.sex.ring<-cap.more[!samesex.ring==samering,]  #If sex is consistent throughout all captures samesex.ring and samering matrix should be equal
#     error.sex.ring$ring
#     error.sex.ring
#     obs.with.sex.error<-cap.more[cap.more$ring %in% c(error.sex.ring$ring), c("ring","sex")] #21 observations with inconsistent sex
#     unique(obs.with.sex.error$ring)#7 rings with sex error
#     cap.more[cap.more$ring %in% c(error.sex.ring$ring), c("year","observer","ring","code","sex","comments")]
# 
#     #compare sex of captures with consistent sex with molecular sex list
#     consistent.sex<-cap.more[!cap.more$ring %in% c(obs.with.sex.error$ring),] #rings that don't appear in obs.with.sex.error list
#     unique.rings.no.sex.error<-unique(consistent.sex$ring)
#     unique.rings.no.sex.error
#     names(sex)
#     consistent.sex<-merge(consistent.sex, sex, by='ring', all.x=TRUE, all.y=FALSE)
#     consistent.sex[,c("ring","sex.x","sex.y")]
#     consistent.sex$sex.equal<-ifelse(consistent.sex$sex.x==consistent.sex$sex.y, "","check")
#     consistent.sex[consistent.sex$sex.equal %in% c("check"),c("ring","sex.x","sex.y","sex.equal")]#all sexes from consistent captures corresponds to molecular sexing
# 
#     #compare inconsistent sex through captures with molecular sex
#     inconsistent.sex<-cap.more[cap.more$ring %in% c(obs.with.sex.error$ring),]#only rings that have inconsistent sexing
#     inconsistent.sex<-merge(inconsistent.sex, sex, by='ring', all.x=TRUE, all.y=FALSE)
#     inconsistent.sex[, c("ring","sex.x","sex.y","comments")] #7 rings w/inconsistent sex, 5 individuals have not been sexed molecularly
#     unique(inconsistent.sex$ring) #7 individuals
#     seven.error.sex <- inconsistent.sex[inconsistent.sex$sex.x != inconsistent.sex$sex.y & !is.na(inconsistent.sex$sexing_year),]    
#   
#     #assign molecular sex to individuals with inconsistent sex
#     cap$nest.id <- paste(cap$year, cap$site, cap$nest, sep="-")    #nest id
#     
# #****fix bug adding nest.id to seven.error.sex
#     seven.error.sex$nest.id <- paste(seven.error.sex$year, seven.error.sex$site, seven.error.sex$nest, sep="-")    #nest id
#     cap$sex.ring <- paste(cap$ring, cap$sex, sep = "-")   
#     
#     
#     
# mol.sex<-merge(cap, sex, by='ring', all.x=TRUE, all.y=FALSE)
# ind <- which(mol.sex$nest.id %in% unique(seven.error.sex$nest.id)&mol.sex$sex.ring %in% unique(seven.error.sex$sex.ring))#choose only cases when ring and sex don't match molecular sex
# ids.to.change <- mol.sex[ind,]
# names(mol.sex)   
# names(ids.to.change)
#   
# for(i in 1:length(mol.sex$ring)){
#   ifelse(mol.sex$nest.id[i] %in% unique(ids.to.change$nest.id) & mol.sex$sex.ring[i] %in% unique(ids.to.change$sex.ring),
#        mol.sex$sex[i] <- mol.sex$sex.y[i], mol.sex$sex[i] <- mol.sex$sex.x[i])
# }
#  
# check.sex<-mol.sex[,c("nest.id","ring","sex.x","sex.y","sex")]
# check.sex$sex.equal<-ifelse(check.sex$sex.x==check.sex$sex.y, "","check")
# check.sex[check.sex$sex.equal %in% c("check")& !check.sex$sex%in%"J",c("nest.id","ring","sex.x","sex.y","sex.equal","sex")]
# 
# #cases where sex was changed: UPDATED 13/05/2015
# # nest.id   ring sex.x sex.y sex.equal sex
# # 138 2011-S-102 CA1099     M     F     check   F
# # 139 2011-S-102 CA1099     M     F     check   F
# # 179  2012-S-20 CA1121     F     M     check   M
# # 202  2013-S--3 CA1136     M     F     check   F
# # 203 2014-S-102 CA1136     M     F     check   F
# # 256  2012-S-20 CA1169     M     F     check   F
# # 670 2013-R-204 CA2746     F     M     check   M
# # 751  2013-S-12 CA3010     F     M     check   F *These two are not part of the seven original errors
# # 866  2010-S-15 CA3082     M     F     check   M *
# #---------------------------FIX COMMENT PART HERE 13/05/2015
# mol.sex[140,]
# mol.sex$sex.comment <- NA
# mol.sex$sex.comment[138] <- "field sexed as M, molecularly sexed as F"
# mol.sex$sex.comment[139] <- "field sexed as M, molecularly sexed as F"
# mol.sex$sex.comment[179] <- "field sexed as M, molecularly sexed as F"
# mol.sex$sex.comment[202] <- "field sexed as F, molecularly sexed as M"
# mol.sex$sex.comment[203] <- "field sexed as M, molecularly sexed as F"
# mol.sex$sex.comment[256] <- "field sexed as M, molecularly sexed as F"
# mol.sex$sex.comment[670] <- "field sexed as M, molecularly sexed as F"
# 
# 
# #two more inconsistent cases flagged:
# # 751      2013-S-12 CA3010     F     M     check   F
# # 866      2010-S-15 CA3082     M     F     check   M
# mol.sex[mol.sex$ring %in% c("CA3010","CA3082"),]
# mol.sex[mol.sex$nest.id %in% c("2013-S-12","2010-S-15"), ]
# mol.sex[751,]
# mol.sex$sex[751]<-"M"
# mol.sex$sex.comment[751] <- "field sexed as F, molecularly sexed as M"
# mol.sex$sex[866]<-"F"
# mol.sex$sex.comment[866] <- "field sexed as M, molecularly sexed as F"
# 
# #two more cases found: nest 2008-S-3 both adults captured for this nest are F
# # 2008  S	3	F	925	1757	37	#N/A	109	#N/A	28.60	15.5	CA1068
# # 2008	S	3	F	1002	1940	39	#N/A	100	#N/A	28.80	14	CA1148
# mol.sex[mol.sex$ring %in% c("CA1068","CA1148"),] #cannot do anything
# 
# names(mol.sex)
# mol.sex$sex.comment[751]
# mol.sex$comments[751]
# 
# mol.sex$note <-ifelse(is.na(mol.sex$comments) & !is.na(mol.sex$sex.comment), mol.sex$sex.comment, 
#                       ifelse(!is.na(mol.sex$comments) & is.na(mol.sex$sex.comment), mol.sex$comments,
#                              ifelse(!is.na(mol.sex$comments) & !is.na(mol.sex$sex.comment), 
#                                     paste(mol.sex$comments, mol.sex$sex.comment, sep=", "), NA)))
# mol.sex$note[256]
# 
# 
# #Same colour combination throughout all captures of same individual?  ----------Inserted by Cristina 14 Nov 2014
# cap.more$code.ring<-paste(cap.more$code, cap.more$ring, sep="-")
# 
# samering.code<-duplicated(cap.more[,"code.ring"])
# error.code.ring<-cap.more[!samering.code==samering,]
# str(cap.more[cap.more$ring %in% c(error.code.ring$ring), c("ring","code")]) #60 observations
# str(unique(cap.more[cap.more$ring %in% c(error.code.ring$ring), c("ring","code")]))#42 rings with inconsistent colour codes
# names(cap.more)
# ring.code.errors <-cap.more[cap.more$ring %in% c(error.code.ring$ring), c("year","observer","ring","code","comments")]
# ring.code.errors[order(ring.code.errors$ring),]
#     #most of these were changed because they were duplicates, errors in field or from juvenile to adult
# 
# #--------------------------------------------End of modification by Cristina 16-Nov-2014--------------
# 
# #-------------------------------------------------------------------------------
# #-------------------------------------------------------------------------------
# #-----------write table with new sex 29/04/2015---------------------------------
# #-------------------------------------------------------------------------------
# 
# 
# names(mol.sex)
# to.write <- mol.sex[, c(1:4,21,6:15,23)]
# names(to.write)
# colnames(to.write)[16] <- "comments"
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# write.csv(to.write, "Captures_Maio_pre_Std.csv") 
# 
# 
# #---------------------------------------------------------------------------------------
# 
# #Use new file generated with corrected sex: Captures_Maio_pre_Std.csv from this point forward
# 
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output/Working files generated")
#  
# cap <- read.csv("Captures_Maio_pre_Std 13May2015.csv", header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
# 
# 
# 
# #--------------------------ignore cases with no metal ring----------------------
# jcap <- cap[cap$sex %in% "J",]
# sum(is.na(jcap$ring))#25 cases with no metal ring
# jcap[is.na(jcap$ring),]
# tmp <- jcap[is.na(jcap$ring),c("year","site","nest","ring")]
# tmp
# aggregate(tmp["ring"],by=list(year=tmp$year,site=tmp$site,nest=tmp$nest),length)
# #   year   site nest ring
# #1  2012 Salina  -29    1
# #2  2012 Salina  -26    3
# #3  2012 Salina  -23    1
# #4  2012 Salina  -22    2
# #5  2012 Salina  -18    1
# #6  2012 Salina  -17    2
# #7  2012 Salina  -15    1
# #8  2012 Salina  -13    1
# #9  2007      S   -2    3
# #10 2007      S   -1    2
# #11 2007      S    1    1
# #12 2007      S    4    1
# #13 2007      S    5    2
# #14 2007      S    9    2
# #15 2012 Salina  110    1
# #16 2012 Salina  111    1
# 
# nes[nes$year%in%2007 & nes$site%in%"S" & nes$nest%in%c(1,4,5,9),]
# cap[cap$year%in%2007 & cap$site%in%"S" & cap$nest%in%c(-1,-2,1,4,5,9),]
# #There is Enddate for broods Nest>0
# #Remove the negative broods (2 broods with 5 captures)
# 
# #Note that there are more than one chicks in several broods,
# #therefore estimated hatching dates would be incorrect for these broods
# #where first captures are selected based on Ring number
# 
# nrow(jcap)#763
# #jcap <- jcap[!(is.na(jcap$ring) & jcap$nest<0),]
# nrow(jcap)#746
# 
# #-------------------------------------------------------------------------------------
# #-------------------------Check measurement's ranges----------------------------------
# #-------------------------------------------------------------------------------------
# ###########Code added by Cristina 27/Jan/2015
# # This code checks that measurements fall within reasonable ranges and are numeric.
# # The ranges should  be defined separately for each population based on boxplots and fivenumbers
# # Measurments that fall out of range and are extreme can be checked in fieldnotes to make sure is not a typo
# 
# 
#    #Check for decimal commas instead of decimal points and replace --> only right_tarsus in Maio
#     names(cap)
# 
#     grep(",", cap$weight)  #none
#     grep(",",cap$left_wing) #none
#     grep(",",cap$right_wing) #none
#     grep(",",cap$left_tarsus)  #none
#     grep(",", cap$bill) #none
#     cap[grep(",",cap$right_tarsus),] #2 cases
#                                     #615  2009     Morro beach   -2          NA         23,5       TS
#                                     #941  2010               S -115          NA         21,7      JMH
#           #change both cases with decimal coma
#           cap$right_tarsus [615] <- 23.5
#           cap$right_tarsus[941] <- 21.7
#     
#     
# 
# #     #replace commas with dots in Righ_tarsus:
# #     cap$right_tarsus2<-gsub(patt=",", replace=".", cap$right_tarsus) #replace commas with dots
# #     cap[c(545,812),] #check whether numbers were kept the same in cases 545 and 812
# #      cap$right_tarsus[545]<-cap$right_tarsus2[545]   #replace new numbers in right_tarsus column
# #      cap$right_tarsus[812]<-cap$right_tarsus2[812]   #replace new numbers in right_tarsus column
# #      cap[c(545,812),]
# 
#   #Tarsus:
#       #Adults (Maio)
# 
#         #check which are non-numeric
#         names(cap)
#         adcap<-cap[cap$sex=="M" | cap$sex=="F",] #only adult captures (all)
#         sum(is.na(adcap$left_tarsus) & is.na(adcap$right_tarsus))#24 NAs
# 
#         ind <- which(is.na(suppressWarnings(as.numeric(adcap$right_tarsus))))
#           adcap[ind,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]
#         ind2 <- which(is.na(suppressWarnings(as.numeric(adcap$left_tarsus))))
#           adcap[ind2,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]
#           adcap[c(1:5),c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]
#           adcap[is.na(adcap$right_tarsus),]
#           adcap$right_tarsus<-as.numeric(adcap$right_tarsus)
#           adcap$left_tarsus <- as.numeric(adcap$left_tarsus)
#         
# #check measures lie within range:
#         fivenum(adcap$left_tarsus)  #28.60 30.00 30.40 30.85 32.40
#         boxplot(adcap$left_tarsus)  #using range from right_tarsus(27,33) there are no outliers
# 
#         adcap[adcap$left_tarsus<27 & !is.na(adcap$right_tarsus), c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those lower than 27 aren't typos
#       
# 
#         adcap[adcap$left_tarsus>32 & !is.na(adcap$right_tarsus), c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those larger than 32 aren't typos
# 
#         fivenum(adcap$right_tarsus) # 18.2 29.2 30.0 30.8 37.8
#         boxplot(adcap$right_tarsus) #>33, <27
#         
#         
#         minor.27 <- adcap[adcap$right_tarsus<27 & !is.na(adcap$right_tarsus), c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those lower than 27 aren't typos
#         minor.27[minor.27$year ==2014,]
#         adcap[adcap$right_tarsus>32 & !is.na(adcap$right_tarsus), c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those larger than 32 aren't typos
#         
#         
#       #Juveniles
#       #check which are non-numeric
#         names(jcap)
#         sum(is.na(jcap$left_tarsus) & is.na(jcap$right_tarsus))#5 NAs
# 
#         ind <- which(is.na(suppressWarnings(as.numeric(jcap$right_tarsus))))
#           jcap[ind,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]
#           str(jcap[ind,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]) #28 obs are NAs
#           str(jcap[!is.na(jcap$right_tarsus),])  #876 obs not NAs
#           jcap$right_tarsus<-as.numeric(jcap$right_tarsus)
# 
#         ind2 <- which(is.na(suppressWarnings(as.numeric(jcap$left_tarsus))))
#           jcap[ind2,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]
#           str(jcap[ind2,c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]) #882 obs are NAs
#           jcap[!is.na(jcap$left_tarsus),]
#           str(jcap[!is.na(jcap$left_tarsus),]) #39 obs not NAs
# 
#         #check measures lie within range: [Juvenile tarsi MAIO: 17.00-23.00]
#         fivenum(jcap$right_tarsus) # 8.6 19.6 20.4 22.0 31.5
#         boxplot(jcap$right_tarsus) #find outliers to check data  <15, >27
# 
#         jcap[jcap$right_tarsus<15 & !is.na(jcap$right_tarsus), c("year","site","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those lower than 27 aren't typos
#         jcap[jcap$right_tarsus>27 & !is.na(jcap$right_tarsus), c("year","site","sex","ring","nest","left_tarsus","right_tarsus","observer","comments")]     #check whether those larger than 33 aren't typos
#                 #tarsus of juveniles from negative nests could be large....check only those from non-negative nests
#                 jcap[jcap$right_tarsus>27 & !is.na(jcap$right_tarsus)& jcap$nest>0, c("year","site","sex","ring","nest","right_tarsus","observer","comments")]
# 
# 
#         fivenum(jcap$left_tarsus)  #18.30 19.90 20.45 21.50 23.00
#         boxplot(jcap$left_tarsus)  #two outliers
#         jcap$left_tarsus<-as.numeric(jcap$left_tarsus)
#         jcap[jcap$left_tarsus>25 & !is.na(jcap$left_tarsus),]
# 
#   #Weight:
#       #Adults
#          #check non-numeric
#          sum(is.na(adcap$weight))#16
#          ind<-which(is.na(suppressWarnings(as.numeric(adcap$weight))))
#          adcap[ind,c("year","site","nest","weight","observer","comments")]
# 
#          #check ranges
#          fivenum(adcap$weight) # 29.00 38.00 39.97 41.50 55.71
#          boxplot(adcap$weight) #<32,>45
# 
#          adcap[adcap$weight>45.20 & !is.na(adcap$weight), c("year","site","sex","ring","nest","weight","observer","comments")]
#          adcap[adcap$weigh<32.0 & !is.na(adcap$weight), c("year","site","sex","ring","nest","weight","observer","comments")]
# 
#       #Juveniles
#         #check non-numeric
#         sum(is.na(jcap$weight))#15
#         ind<-which(is.na(suppressWarnings(as.numeric(jcap$weight))))
#          jcap[ind,c("year","site","nest","weight","observer","comments")]
# 
#         #check ranges
#         fivenum(jcap$weight) #4.38   6.00   6.74   8.00 110.00
#         boxplot(jcap$weight) # clear outlier weight =110
#         ind<-which(jcap$weight==110)#omit outlier of 110
#         jcap[ind,]
#         jcap.nooutlier<-jcap[-ind,]
#         boxplot(jcap.nooutlier$weight)  #<4.30 >12
# 
#         jcap[jcap$weight>12 & !is.na(jcap$weight) & jcap$nest>0, c("year","site","sex","ring","nest","weight","observer","comments")] #checked only non-negative nests since negative nests could have heavy fledglings
#         jcap[jcap$weight<4.30 & !is.na(jcap$weight), c("year","site","sex","ring","nest","weight","observer","comments")] #no outliers
# 
#   #Bill:
#           #check non-numeric bills both adults and juveniles
#           sum(is.na(cap$bill))#29
#           ind<-which(is.na(suppressWarnings(as.numeric(cap$bill))))
#           cap[ind,c("year","site","nest","bill","observer","comments")]
# 
#       #Adults
#          fivenum(adcap$bill) # 6.0 15.2 15.8 16.3 19.0
#          boxplot(adcap$bill) #<14,>18
# 
#          adcap[adcap$bill>18 & !is.na(adcap$bill), c("year","site","sex","ring","nest","bill","observer","comments")]
#          adcap[adcap$bill<14.0 & !is.na(adcap$bill), c("year","site","sex","ring","nest","bill","observer","comments")]
# 
# 
#       #Juveniles
#           fivenum(jcap$bill) #   4.9   7.0   7.5   8.9 103.0
#          ind<-jcap[jcap$bill<20,]
#          boxplot(jcap$bill)
#          hist(ind$bill) #>16
# 
#          jcap[jcap$bill>16 & !is.na(jcap$bill), c("year","site","sex","ring","nest","bill","observer","comments")]
#          #no outliers for lower end (Maio)
# 
# 
#   #Wing:
#      #Adults ONLY
#         #check non-numeric wings
#           names(adcap)
#           sum(is.na(adcap$right_wing))#691
#           ind<-which(is.na(suppressWarnings(as.numeric(adcap$right_wing))))
#           adcap[ind,c("year","site","nest","left_wing","right_wing","observer","comments")]
# 
#         # check ranges
#           fivenum(adcap$right_wing) #  11.2 106.0 108.0 110.0 119.0
#           boxplot(adcap$right_wing) #<100,>113
#           hist(adcap$right_wing)
#           adcap[adcap$right_wing>113 & !is.na(adcap$right_wing), c("year","site","sex","ring","nest","right_wing","observer","comments")]
#           adcap[adcap$right_wing<100 & !is.na(adcap$right_wing), c("year","site","sex","ring","nest","right_wing","observer","comments")]
# 
# 
# 
# ##-------------------------CORRECT TYPOS and add comment-------------------------------------
# ##should be cap$comments_stdfile??
# 
# cap$right_tarsus[1141]<-28.2
# cap$comments[1141] <- "right_tarsus was 18.2, checked notes and it looked more like 28.2 (CCI 29/04/2015)"
# 
# cap$weight[1188] <- 11.0
# cap$comments[1188] <- "weight was recorded as 110 but checked notes and is 11.0 (CCI 29/04/2015)"
# 
# cap$bill[122] <- 9.8
# cap$comments[122] <- "bill was recorded as 98.0, checked notes and should be 9.8 (CCI 29/04/2015)"
# 
# cap$bill[1083] <- 10.3
# cap$comments[1083] <- "bill was recorded as 103.0, checked notes and should be 10.3 (CCI 29/04/2015)"
# 
# cap$right_wing[635] <- 113
# cap$comments[635] <- "ringed from previous years, wing was recorded as 115, checked notes and should be 113 (CCI 29/04/2015)"
# 
# #-----------------------------------------------------------------------------------------------------
# 
# #-----------------------------------------------------------------------------------------
# #-----------------------------Write new STD file for captures-----------------------------
# #-----------------------------------------------------------------------------------------
# getwd()
# names(cap)
# head(cap)
# cap.save <- cap[,2:17]
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# write.csv(cap.save, "Captures_Maio_2009-2014_stdfile 13May2015.csv") 
# 
# 
# 
# #---------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------
# 
# 
# 
# #Change sites: #ADDED TO V2 13/05/2015
# # Morro to M
# # Terras Salgadas to TS
# # Morro beach to M
# 
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# cap <- read.csv("Captures_Maio_2009-2014_stdfile 13May2015.csv", header = TRUE, as.is=TRUE, na.strings=c("NA"," ","")) 
# #------------------------------------------------------------------
# 
# unique(cap$site)
# cap$site[cap$site %in% "Morro"] <- "M"
# cap$site[cap$site %in% "Morro beach"] <- "M"
# cap$site[cap$site %in% "Terras Salgadas"] <- "TS"
# 
# unique(cap$site)
# 
# #Write changes
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# write.csv(cap, "Captures_Maio_2009-2014_stdfile 13May2015.csv") 
# 
# #---------------------------------------------------------------------------------
# #add note to capture of CA1192.   CA1192 appears in this nest as recaptured.however in this nest there was only CA1189, CA1193 and CA1198 as chicks.
# #CA1192 belongs to nest 2008-S-11, could be a typo in ring OR nest ***Check Araceli's notes
# 
# cap$comments[cap$ring %in% "CA1192" & cap$nest ==3] <- "recapture, (this ring or nest number is wrong, original notes need to be checked CCI 14/05/2015)"
# 
# 
# #############################################################
# #############################################################
# #############################################################
# #############################################################
# 
# #New mistakes found in colour combinations 04/08/2015
# #Load stdfile
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output/std v4")
# cap <- read.csv("Captures_Maio_2009-2014_stdfile 21May2015.csv") 
# names(cap)
# 
# #-----------------------
# #-----------------------
# 
# ####-using code from cleaning resightings
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# 
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$" 
#   #regexp2 excludes codes that have X in the first letters
# 
# ind.correct<-grep(pattern = regexp1, cap$code, perl=TRUE)
# ind.correct<-grep(pattern = regexp2, cap$code, perl=TRUE)
# correct.code <- cap[ind.correct,] #CORRECT OBS: 1698 obs with regexp1, 729 obs with regexp2
# str(correct.code)
# cap[-ind.correct,"code"]
# str(cap) #1699
# 
# incorrect.code <- cap[-ind.correct,] 
# str(incorrect.code) #with regexp1: no errors
# head(correct.code)
# 
# 
# ####################################################################
# #Find those two cases were second letters are not X
# regexp2 <- "([RGLBYOWM]{1})([RGLBYOWM]{1})\\.([X]{1})([X]{1})\\|([RGLBYOWM]{1})([RGLBYOWM]{1})\\.([RGLBYOWXM]{1})([X]{1})$" 
# regexp3 <- "([RGLBYOWM]{1})([X]{1})\\.([X]{1})([RGLBYOWM]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([RGLBYOWM]{1})$" 
# 
# ind1 <- grep(pattern = regexp2, cap$code, perl=TRUE)
# ind2 <- grep(pattern = regexp3, cap$code, perl=TRUE)
# wrong.format <- cap[ind1,"code"] # two
# cap[ind2, "code"]# none
# 
# #change codes in wrong format:[1] ML.XX|OO.XX ML.XX|OR.XX
# cap[cap$code %in% wrong.format,]
# 
# cap$code[963] <- "MX.LX|OX.OX"
# cap$code[967] <- "MX.LX|OX.RX"
# 
# ##############################################################################
# #Find duplicated codes
# names(cap)
# 
# adultcap<-cap[cap$sex %in%c("M","F") & !is.na(cap$ring),] #include only adults with metal ring
# unique(adultcap$ring)
# 
# captures.per.adult<-as.data.frame(table(adultcap$ring))
# captures.per.adult[captures.per.adult$Freq>1,]
# select.rings<-captures.per.adult[captures.per.adult$Freq>1,]
# rings.to.choose<-as.data.frame(select.rings$Var1)
# names(rings.to.choose)<-"rings"
# cap.more<-adultcap[adultcap$ring %in%rings.to.choose$rings,]#include only cases with more than one capture
# cap.more
# 
# #---------04/08/2015
# #same ring different codes? Errors? 2 typos and rest CR changes
# 
# cap.more$code.ring<-paste(cap.more$code, cap.more$ring, sep="-")
# 
# samering<-which(duplicated(cap.more$ring) | duplicated(cap.more$ring, fromLast=TRUE))
# cap.more[samering, "ring"] #401
# 
# samering.code<-which(duplicated(cap.more$code.ring) | duplicated(cap.more$code.ring, fromLast=TRUE))
# cap.more[samering.code,"code.ring"] #369
# 
# int.dup <- setdiff(samering,samering.code)
# 
# error.code.ring<-cap.more[int.dup,"code.ring"]
# str(error.code.ring)#32
# 
# # [1] "MX.OX|BX.WX-CA1068" *Duplicated changed CR 
# # [2] "MX.OX|BX.LX-CA1068" *Duplicated changed CR
# # [3] "MX.OX|BX.OX-CA1073" *typo...fix
# # [4] "MX.XX|XX.XX-CA1093" *adult had MX.XX|XX.XX and CR were added
# # [5] "BX.MX|BX.OX-CA1100" *one ring was removed as it was damaging leg
# # [6] "MX.LX|GX.RX-CA1121" *had lost one ring and LV added another colour
# # [7] "MX.LX|OX.WX-CA1136" *Duplicated changed CR
# # [8] "MX.OX|WX.YX-CA1137" *Duplicated changed CR
# # [9] "XX.OX|XX.XX-CA1139" *CR added
# # [10] "MX.LX|LX.GX-CA1673" *CR changed
# # [11] "MX.XX|BX.GX-CA1673" *CR changed
# # [12] "MX.GX|XX.XX-CA2619" *CR changed
# # [13] "MX.OX|XX.GX-CA2656" *CR changed
# # [14] "MX.OX|RX.GX-CA2656" *CR changed
# # [15] "MX.OX|OX.WX-CA2683" *Duplicated changed CR
# # [16] "MX.GX|OX.WX-CA2683" *Duplicated changed CR
# # [17] "MX.LX|YX.RX-CA2740" *Duplicated changed CR
# # [18] "MX.GX|OX.OX-CA2745" *Duplicated changed CR
# # [19] "MX.OX|OX.OX-CA2745" *Duplicated changed CR
# # [20] "MX.LX|LX.WX-CA3011" *Duplicated changed CR
# # [21] "MX.BX|BX.WX-CA3018" *Duplicated changed CR
# # [22] "MX.BX|WX.BX-CA3018" *Duplicated changed CR
# # [23] "MX.BX|YX.GX-CA3019" *typo
# # [24] "MX.BX|WX.GX-CA3019" *typo should be MB|YG
# # [25] "MX.WX|GX.OX-CA3063" *had lost one ring and LV added another colour
# # [26] "MX.LX|GX.OX-CA3063" *had lost one ring and LV added another colour
# # [27] "MX.XX|WX.OX-CA3124" *CR changed
# # [28] "MX.WX|WX.OX-CA3124" *CR changed
# # [29] "WX.MX|LX.BX-CA4088" *Duplicated changed CR
# # [30] "LX.MX|LX.BX-CA4088" *Duplicated changed CR
# # [31] "WX.MX|OX.BX-CA4100" *Duplicated changed CR
# # [32] "GX.MX|OX.YX-CA4100" *Duplicated changed CR
# 
# cap.more[cap.more$ring =="CA4100",]
# #--------------------------
# #--------------------------
# #fix typos:
# # [24] "MX.BX|WX.GX-CA3019" *typo should be MB|YG
# # [3] "MX.OX|BX.OX-CA1073" *typo...fix
# cap[cap$ring =="CA3019",]
# cap[cap$code=="MX.BX|WX.GX",]
# cap$code[771]<-"MX.BX|YX.GX"
# 
# cap$comments_stdfile <- as.character(cap$comments_stdfile)
# cap$comments_stdfile [771]<- "typo found in code, was MX.BX|WX.GX, notes checked and corrected (CCI 2015)" 
# 
# cap[cap$ring =="CA1073",]
# cap$code[98]<-"MX.OX|BX.RX"
# cap$comments_stdfile[98]<- "this code was probably mistaken, captured originally and in notes as MO|BO, presumably Red is fading. Corrected to MO|BR (CCI 2015)"
# 
# #--------------------------
# #--------------------------
# 
# 
# #--------------------------------------------
# #same code different rings? Duplicates?
# adultcap$ring <- as.character(adultcap$ring)
# unique.rings <- unique(adultcap$ring) #516
# 
# adultcap$code.ring <- paste(adultcap$code, adultcap$ring, sep="-")
# unique.code.ring <- unique(adultcap$code.ring) #537
# str(unique.code.ring)
# 
# library(plyr)
# dupl <- ldply(strsplit(unique.code.ring, "-"))
# same.code.diff.ring <- dupl[duplicated(dupl$V1)|duplicated(dupl$V1, fromLast=T), ] #same codes different rings
# same.code.diff.ring[order(same.code.diff.ring$V1),]
# 
# dupl.codes <- adultcap[adultcap$code %in% same.code.diff.ring$V1 |adultcap$ring %in% same.code.diff.ring$V2,]
# str(dupl.codes) #114
# 
# #114 duplicates:
# dupl.codes[order(dupl.codes$code), c("year","code","ring","comments_field","comments_stdfile")]
# 
# # year        code   ring
# # 317  2009 BX.MX|OX.BX CA1618*duplicate
# # 318  2009 BX.MX|OX.BX CA1618*duplicate
# # 1122 2014 BX.MX|OX.BX CA3500*duplicate
# # 1123 2010 BX.MX|OX.BX CA3500*duplicate
# # 667  2013* MX.GX|OX.OX CA2745*duplicated, changed CR
# # 561  2014* MX.GX|OX.WX CA2683*triplicate MO|OW, changed colour combination
# # 754  2014* MX.LX|LX.WX CA3011*duplicate, changed colour combination
# # 203  2014* MX.LX|OX.WX CA1136*duplicated, changed CR
# # 77   2008 MX.OX|BX.BX CA1064*duplicate
# # 78   2010 MX.OX|BX.BX CA1064*duplicate
# # 79   2009 MX.OX|BX.BX CA1064*duplicate
# # 200  2008 MX.OX|BX.BX CA1136*duplicate
# # 201  2009 MX.OX|BX.BX CA1136*duplicate
# # 202  2013 MX.OX|BX.BX CA1136**duplicate
# # 204  2012 MX.OX|BX.BX CA1136*duplicate
# # 87   2008 MX.OX|BX.LX CA1068*duplicate
# # 910  2013 MX.OX|BX.LX CA3119*duplicate
# # 911  2014 MX.OX|BX.LX CA3119*duplicate
# # 912  2013 MX.OX|BX.LX CA3119*duplicate
# # 88   2009 MX.OX|BX.OX CA1069*fixed typo
# # 89   2008 MX.OX|BX.OX CA1069*fixed typo
# # 90   2010 MX.OX|BX.OX CA1069*fixed typo
# # 98   2012 MX.OX|BX.OX CA1073*fixed typo
# # 94   2010 MX.OX|BX.RX CA1073*fixed typo
# # 95   2012 MX.OX|BX.RX CA1073*fixed typo
# # 96   2008 MX.OX|BX.RX CA1073*fixed typo
# # 97   2009 MX.OX|BX.RX CA1073*fixed typo
# 
# # 529  2008 MX.OX|OX.OX CA2657*duplicate
# # 530  2012 MX.OX|OX.OX CA2657*duplicate
# # 668  2012 MX.OX|OX.OX CA2745*duplicate
# # 205  2008 MX.OX|OX.RX CA1137*duplicate
# # 206  2009 MX.OX|OX.RX CA1137*duplicate
# # 207  2009 MX.OX|OX.RX CA1137*duplicate
# # 209  2008 MX.OX|OX.RX CA1137*duplicate
# # 543  2008 MX.OX|OX.RX CA2667*duplicate
# # 544  2009 MX.OX|OX.RX CA2667*duplicate
# # 560  2010 MX.OX|OX.WX CA2683*duplicate
# # 581  2010 MX.OX|OX.WX CA2696*duplicate
# # 582  2009 MX.OX|OX.WX CA2696*duplicate
# # 753  2013 MX.OX|OX.WX CA3011*duplicate
# # 755  2012 MX.OX|OX.WX CA3011*duplicate
# # 208  2013 MX.OX|WX.YX CA1137*duplicated, changed CR
# 
# # 573  2009 BX.MX|OX.WX CA2692*duplicate
# # 1001 2010 BX.MX|OX.WX CA3401*duplicate
# # 324  2009 BX.MX|YX.YX CA1622*duplicate
# # 1002 2010 BX.MX|YX.YX CA3402*duplicate
# # 1548 2014 GX.MX|OX.RX CA4114*duplicate
# # 1640 2014 GX.MX|OX.RX CA4205*duplicate
# # 1438 2013 GX.MX|RX.BX CA3904*duplicate
# # 1443 2013 GX.MX|RX.BX CA3909*duplicate
# 
# # 676  2012 MX.BX|OX.LX CA2749*duplicate
# # 776  2009 MX.BX|OX.LX CA3023*duplicate
# # 777  2010 MX.BX|OX.LX CA3023*duplicate
# # 713  2013 MX.BX|OX.YX CA2777*duplicate
# # 1187 2014 MX.BX|OX.YX CA3650*duplicate
# # 963  2012 MX.LX|OX.OX CA3164*duplicate
# # 1624 2014 MX.LX|OX.OX CA4190*duplicate
# # 593  2014 MX.LX|OX.RX CA2704*duplicate
# # 967  2012 MX.LX|OX.RX CA3168*duplicate
# # 757  2013 MX.LX|WX.OX CA3012*duplicate
# # 1256 2013 MX.LX|WX.OX CA3725*duplicate
# # 660  2014 MX.LX|YX.RX CA2740**duplicated, changed CR
# # 657  2013 MX.LX|YX.YX CA2740*duplicate
# # 661  2010 MX.LX|YX.YX CA2740*duplicate
# # 731  2014 MX.LX|YX.YX CA2796*duplicate
# # 86   2013* MX.OX|BX.WX CA1068*duplicated, changed CR and duplicate was generated again
# # 99   2008 MX.OX|BX.WX CA1074*duplicate
# # 1144 2013 MX.OX|BX.WX CA3613*duplicate
# # 210  2008 MX.OX|OX.BX CA1139*duplicate
# # 211  2008 MX.OX|OX.BX CA1139*duplicate
# # 212  2010 MX.OX|OX.BX CA1139*duplicate
# # 285  2012 MX.OX|OX.BX CA1187*duplicate
# # 574  2009 MX.OX|OX.GX CA2693*duplicate
# # 575  2009 MX.OX|OX.GX CA2693*duplicate
# # 576  2009 MX.OX|OX.GX CA2693*duplicate
# # 679  2012 MX.OX|OX.GX CA2751*duplicate
# # 703  2012 MX.OX|OX.GX CA2768*duplicate
# # 126  2012 MX.OX|WX.OX CA1093*ring changed from MX|XX
# # 128  2012 MX.OX|WX.OX CA1093*ring changed from MX|XX
# # 246  2008 MX.RX|GX.RX CA1161*duplicate
# # 262  2011 MX.RX|GX.RX CA1174*duplicate
# # 309  2008 MX.RX|RX.LX CA1199*duplicate
# # 455  2008 MX.RX|RX.LX CA2604*duplicate
# # 450  2008 MX.RX|YX.LX CA2542*duplicate
# # 509  2009 MX.RX|YX.LX CA2642*duplicate
# # 562  2009 MX.WX|LX.BX CA2685*duplicate
# # 845  2010 MX.WX|LX.BX CA3067*duplicate
# # 383  2012 MX.WX|WX.GX CA1657*duplicate
# # 803  2012 MX.WX|WX.GX CA3038*duplicate
# # 1008 2010 MX.YX|BX.LX CA3407*duplicate
# # 1014 2010 MX.YX|BX.LX CA3411*duplicate
# # 1058 2010 MX.YX|YX.RX CA3444*duplicate
# # 1090 2010 MX.YX|YX.RX CA3468*duplicate
# # 219  2009 OX.MX|WX.LX CA1143*duplicate
# # 708  2012 OX.MX|WX.LX CA2773*duplicate
# # 709  2014 OX.MX|WX.LX CA2773*duplicate
# # 1308 2012 RX.MX|RX.BX CA3771*duplicate
# # 1309 2012 RX.MX|RX.BX CA3772*duplicate
# # 1310 2014 RX.MX|RX.BX CA3772*duplicate
# # 1357 2013 WX.MX|LX.BX CA3818*duplicate
# # 
# # 1520 2013 WX.MX|LX.BX CA4088*duplicate
# # # 1521 2014 LX.MX|LX.BX CA4088*duplicated, changed CR
# # 1355 2013 WX.MX|OX.BX CA3816*duplicate
# # 1533 2013 WX.MX|OX.BX CA4100 *duplicated, changed CR
# # 1534 2014 GX.MX|OX.YX CA4100 *duplicated, changed CR
# 
# 
# 
# all.dupl<-cap.more[cap.more$ring %in% dupl.codes$ring |cap.more$code %in% dupl.codes$code,]
# all.dupl[order(all.dupl$code, all.dupl$ring, all.dupl$year), c("code","ring","year","comments_field","comments_stdfile")]
# 
# #go case by case through previous list:
# cap[cap$code =="BX.MX|OX.BX",]
# cap$comments_stdfile [c(317,318,1122,1123)] <- "duplicate"
# 
# cap[cap$code =="MX.OX|BX.BX",]
# cap$comments_stdfile[c(77,78,79,200,201,204)] <-"duplicate"
# cap$comments_stdfile[202] <- "field sexed as F, molecularly sexed as M (CCI 2015), duplicate"
# 
# cap[cap$code =="MX.OX|BX.LX",]
# cap$comments_stdfile[c(87,910,911,912)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|OX.OX",]
# cap$comments_stdfile[c(529,530,668)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|OX.RX",]
# cap$comments_stdfile[c(205,206,207,209,543,544)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|OX.WX",]
# cap$comments_stdfile[c(506,581,582,753,755)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|WX.YX",]
# 
# cap[cap$code =="BX.MX|OX.WX",]
# cap$comments_stdfile[c(573,1001)]<-"duplicate"
# 
# cap[cap$code =="BX.MX|YX.YX",]
# cap$comments_stdfile[c(324,1002)]<-"duplicate"
# 
# cap[cap$code =="GX.MX|OX.RX",]
# cap$comments_stdfile[c(1548,1640)]<-"duplicate"
# 
# cap[cap$code =="GX.MX|OX.YX",]
# cap$comments_stdfile[c(1533)]<-"duplicate"
# cap[1533,]
# 
# cap[cap$code =="GX.MX|RX.BX",]
# cap$comments_stdfile[c(1438,1443)]<-"duplicate"
# 
# cap[cap$code =="LX.MX|LX.BX",]
# cap$comments_stdfile[1520]<-"duplicate"
# cap[1520,]
# 
# cap[cap$code =="MX.BX|OX.LX",]
# cap$comments_stdfile[c(676,776,777)]<-"duplicate"
# 
# cap[cap$code =="MX.BX|OX.YX",]
# cap$comments_stdfile[c(713,1187)]<-"duplicate"
# 
# cap[cap$code =="MX.LX|OX.OX",]
# cap$comments_stdfile[c(963,1624)]<-"duplicate"
# 
# cap[cap$code =="MX.LX|OX.RX",]
# cap$comments_stdfile[c(593,967)]<-"duplicate"
# 
# cap[cap$code =="MX.LX|WX.OX",]
# cap$comments_stdfile[c(757,1256)]<-"duplicate"
# 
# cap[cap$code =="MX.LX|YX.YX",]
# cap$comments_stdfile[c(657,661,731)]<-"duplicate"
# 
# cap[cap$code =="MX.LX|YX.YX",]
# cap$comments_stdfile[c(657,661,731)]<-"duplicate"
# 
# 
# cap[cap$code =="MX.OX|BX.WX",]
# cap$comments_stdfile[c(86,99,1144)]<-"duplicate"
# cap[86,]
# 
# cap[cap$code =="MX.OX|OX.BX",]
# cap$comments_stdfile[c(210,211,212,285)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|OX.GX",]
# cap$comments_stdfile[c(574,575,576,679,703)]<-"duplicate"
# 
# cap[cap$code =="MX.OX|WX.OX",]
# cap[cap$ring =="CA1093",]
# cap$comments_stdfile[c(574,575,576,679,703)]<-"duplicate"
# 
# cap[cap$code =="MX.RX|GX.RX",]
# cap$comments_stdfile[c(246,262)]<-"duplicate"
# 
# cap[cap$code =="MX.RX|RX.LX",]
# cap$comments_stdfile[c(309,455)]<-"duplicate"
# 
# cap[cap$code =="MX.RX|YX.LX",]
# cap[cap$ring =="CA2642",]
# cap$comments_stdfile[c(450,509)]<-"duplicate"
# 
# cap[cap$code =="MX.WX|LX.BX",]
# cap$comments_stdfile[c(562,845)]<-"duplicate"
# 
# cap[cap$code =="MX.WX|WX.GX",]
# cap$comments_stdfile[c(383,803)]<-"duplicate"
# 
# cap[cap$code =="MX.YX|BX.LX",]
# cap$comments_stdfile[c(1008,1014)]<-"duplicate"
# 
# cap[cap$code =="MX.YX|YX.RX",]
# cap$comments_stdfile[c(1058,1090)]<-"duplicate"
# 
# cap[cap$code =="OX.MX|WX.LX",]
# cap$comments_stdfile[c(219,708,709)]<-"duplicate"
# 
# cap[cap$code =="RX.MX|RX.BX",]
# cap$comments_stdfile[c(1308,1309,1310)]<-"duplicate"
# 
# cap[cap$code =="WX.MX|LX.BX",]
# cap$comments_stdfile[c(1357)]<-"duplicate"
# 
# cap[cap$code =="WX.MX|OX.BX",]
# cap$comments_stdfile[c(1355)]<-"duplicate"
# #-------------end of duplicate signaling
# 
# 
# #look up those comments which mention a duplicate to see if all have been found:
# pat1 <- "plicate"
# ind1 <- grep(pattern = pat1, cap$comments_field, perl=TRUE)
# ind2 <- grep(pattern = pat1, cap$comments_stdfile, perl=T)
# str(cap[ind1,])
# str(cap[ind2,]) #87 check why it's incosistent with the 114 found previously:
# 
# cap$code.ring <- paste(cap$code, cap$ring, sep="-")
# str(setdiff(dupl.codes$code.ring,cap[ind2,"code.ring"]))#checked some, most are ones where ring was changed, or had juveniles rings
# # [1] "MX.OX|BX.OX-CA1069" "MX.OX|BX.RX-CA1073" "MX.OX|BX.OX-CA1073" "MX.OX|WX.OX-CA1093"
# # [5] "MX.XX|XX.XX-CA1093" "XX.XX|MX.XX-CA1099" "MX.LX|OX.WX-CA1136" "MX.OX|WX.YX-CA1137"
# # [9] "XX.OX|XX.XX-CA1139" "MX.XX|XX.XX-CA1617" "MX.OX|OX.WX-CA2683" "MX.GX|OX.WX-CA2683"
# # [13] "MX.LX|YX.RX-CA2740" "MX.GX|OX.OX-CA2745" "MX.LX|LX.WX-CA3011" "MX.XX|XX.XX-CA3050"
# # [17] "XX.XX|MX.XX-CA3126" "LX.MX|LX.BX-CA4088" "GX.MX|OX.YX-CA4100"
# 
# cap[cap$code =="MX.LX|LX.WX",]
# cap[cap$ring =="CA3011",]
# 
# #-----06/08/2015
# cap$code[cap$ring == "CA3018"]<-"MX.BX|BX.WX"
# cap$comments_stdfile[769] <- "typo found in code, was MX.BX|WX.BX, notes checked and corrected (CCI 2015)"
# cap[769,]
# 
# #------07/08/2015
# library(stringr)
# pat <- "bs-"
# ind1 <- grep(pattern = pat, cap$nest, perl=T)
# cap$nest[ind1] <-str_replace(cap$nest, pattern="bs-", "bs--")
#  
# #--------------End of modification by Cristina to correct colour combinations 05/08/2015--------------
# 
# getwd()
# names(cap)
# head(cap)
# cap.save <- cap[,2:18]
# setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2014/output")
# write.csv(cap.save, "Captures_Maio_2009-2014_stdfile 06Aug2015.csv") 
# 
# 
