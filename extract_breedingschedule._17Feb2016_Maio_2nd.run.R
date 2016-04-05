#Start developing code for Breeding schedules 03/08/2015
#Author: Cristina Carmona-Isunza
#Datasets need to have std file column names and have clean colour combinations at least

#MAIO ---1st Run----
#07/08/2015 2nd log: finished up to line 293
#10/08/2015 3rd log: add nest info
#11/08/2015 4th log: add Brood fates info
#12/08/2015 4th log: create final file for Maio
#CHANGES TO ESTIMATE LD OF NEGATIVE BROODS 22/01/2016 and include data from 2015
#completed 27/01/2016
#MAIO ------2nd Run-----
#15/02/2016 1st log: fix files and start running, up to line 757 fixing nest numbers format
#16/02/2016 2nd log: fixed chicks with nor rings
#17/02/2016 3rd log: finished and wrote final file for Maio
#------------------------
#CEUTA
#01/02/2016 1st log: up to line 249
#02/02/2016 2nd log: up to 336 (detected more errors lines 77-93)
#03/02/2016 3rd log: up to line 652
#04/02/2016 4th log: up to 722
#05/02/2016 5th log: up to 919 *problem: cross-fostering broods? 
#08/02/2016 6th log: Use BirdSoc instead of BirdRef, created new files using new *updated files by Clemens from Googledrive
#09/02/2016 7th log: Use new files provided by Luke (nests, captures, resightings)
#10/02/2016 8th log: up to line 951 debugging female desertion times.figuring out how to count consecutive observations in which an adult was not seen
#11/02/2016 9th log: up to line 1264, solved consecutive observations, however chicks section must change given the brood swapping in Ceuta
#12/02/2016 10th log: finished, however matching mol_sex from captures is missing...,/ added
#--------------------------------------------------------------
#TUZLA
#23/02/2016 1st log - Cancelled for the moment


#--------------------------------------------------------------
#MADAGASCAR
#15/03/2016 1st log - Start running and clearing mistakes, produced Resightings std file for MAD using cleaning code.
#16/03/2016 2nd log - Up to line 663 found some errors in BRef....need correction
#17/03/2016 3rd log - Continue from 663, clear errors found in BirdRef, created Cleaning brood fates.R
#18/03/2016 4th log - Modified line 393....
#21/03/2016 5th log - Issue2 - fixed duplicates (also duplicates need to be modified only to Andavadoaka and omit XX ambiguous codes!)
#24/03/2016 6th log - Stopped this code, cleaning bird ref first, finished cleaning bird ref 29/03/2016
                    #Issue 4 OMIT JUVENILES from list of duplicates???? 

#30/03/2016 7th log - Start re-running code again using new BirdRef file produced. 
#31/03/2016 8th log - Continue re-running code up to line 621
#02/04/2016 9th log - up to 641
#03/04/2016 10th log - Ran into error in nests file as laying dates are missing
#04/04/2016 11th log - Produced nest std file, will restart code tomorrow
#05/04/2016 12th log - Re-start running code

#---------------------------------------------------------------
#Maio
#setwd("F:/Plovers/3rd Chapter/input/Maio/Maio 2007-2015")
#Ceuta
#setwd("F:/Plovers/3rd Chapter/input/Ceuta")
#new files path Ceuta:
#setwd("F:/Plovers/3rd Chapter/input/Ceuta/Newnames Clemens2013_CCI")
#Tuzla
# setwd("F:/Plovers/3rd Chapter/input/Tuzla")
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
names(working.list) <- c("bf1","br","br3","bf","cap3","cap","hatch","sex","ne3","ne","re")


attach(working.list)
#detach(working.list)

#---------------------------------------------------------------
#Prepare resightings and surveys MAIO:
# library(gtools)
# re$type <- "resightings"
# surv$type <- "survey"
# 
# names(re)
# names(surv) <- c("year","date","time","code","sex","site","easting",
#                  "northing","distance","degree","habitat" ,"observer","comments","observers","type")
# 
# 
# all.re<-smartbind(re, surv)
# names(all.re)
# head(all.re[all.re$type %in% "survey",])
#---------------------------------------------------------------

se <- function(a) sd(a[!is.na(a)])/sqrt(length(a[!is.na(a)]))
names(br)

str(br)#CEUTA:652 obs, after corrections 653, new BirdSoc file Clemens 2013 - 683 obs
        #MAIO: 1006 obs
        #MAD: 2416, 2nd run 2080
names(bf)
str(bf)#CEUTA:3238 obs, 3310 obs new file 
        #MAIO: 3006 obs
        #MAD:1499, 2nd run 1094
names(cap)
str(cap)#CEUTA:2375, 2460 obs new file (up to 2013); new Luke's file: 2375
          #MAIO: 1935 obs
          #MAD: 5372, 2nd run 5668
#str(cap[!is.na(cap$comments_stdfile) & cap$comments_stdfile =="duplicate",])
      #MAIO: 86 duplicates
names(sex)
names(ne)
str(ne)#CEUTA:659, 690 obs new file (up to 2013); new Luke's file: 659
        #MAIO: 699 obs
        #MAD: 2762, 2nd run 2906
names(re)
str(re)#CEUTA:4148, 4165 obs new file; new Luke's file: 4302
        #MAIO: 5026 obs
        #MAD: 6772, 2nd run 7857

library(stringr)

#--------------------------------------------------------
#Prepare data set:
#0. Omit lost ring and get rid of FALSE for Female in br: #MAIO
# cap <- cap[!is.na(cap$code),]
# str(br)
# br$field_sex_f <- as.character(br$field_sex_f)
# br$field_sex_f <- str_replace(br$field_sex_f, pattern="FALSE", "F")

#1. List of duplicates-----------------------
#a.0) correct codes in with different structure: e.g. WX.BX|BX.YM_802109760
#unique(cap[nchar(cap$code)>11, "code"]) #nchar should be 7 in MAD: W.B|B.M
unique(cap[nchar(cap$code)!=7, "code"]) #119 in Mad, no corrections can be made

        #Check general format in captures and resightings:
#regexp1 <- "([RGLBYOWXMSsP]{2})\\.([RGLBYOWXMSsP]{2})\\|([RGLBYOWXMSsP]{2})\\.([RGLBYOWXMSsP]{2})$"
regexp1 <- "([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})\\|([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})$"

ind<-grep(pattern=regexp1, cap$code)
ind2<-grep(pattern=regexp1, re$code)

str(cap[-ind,c("year","site","nest","code")]) 
# NEW LUkes file, Ceuta:
        #year site nest                  code
# 35   2006    B    5          XX.WB|XX.RWM
# 44   2006    B    1          XX.WX|XX.WMY
# 59   2006    C   11          XX.YR|XX.WWM
# 69   2006    A  104 WX.BX|BX.YM_802109760
# 136  2006    B    5          XX.WB|XX.RWM
# 467  2007    C    4 WX.BX|BX.YM_802109760
# 560  2007    A  202          XX.YR|XX.WWM
# 580  2007    A  202          XX.YR|XX.WWM
# 1106 2008    A    1          XX.YR|XX.WWM


cap[-ind,c("code")] #Mad: 259 with wierd codes...and several NAs which belong to Juveniles?
#Previous file to Luke's (CEUTA):
# [1] "BX.WX|SX."    "XX.WB|XX.RWM" "XX.WX|XX.WMY"
# [4] "XX.YR|XX.WWM" "XX.WB|XX.RWM" "BX.WX|SX."   
# [7] "XX.YR|XX.WWM" "BX.WX|SX."    "XX.YR|XX.WWM"
# [10] "BX.WX|SX."    "XX.YR|XX.WWM" NA            
# [13] NA             "YX.MX|WX." 

#Mad codes in captures with wrong format (excluding NAs):
# "B.O|.G"   "V.X|M.X"   "W.R|M.R?"  "X.X|X.G4"  "X.X|X.L7"  "X.X|X.G2" 
# "X.X|X.L5"  "X.X|X.L2"  "X.X|X.L1"  "X.X|X.G1"  "X.X|X.L3"  "W.K|O.M"   "L.Y|M.K"   "B.K|M.Y"   "Y.K|Y.M"   "G.M|G.K"  
# "L.K|M.L"   "Y.M|Y.K"   "B.K|M.B"   "Y.M|Y.K"   "B.M|B.K"   "B.M|B.K"   "R.M|R.K"   "L.M|L.K"   "O.M|O.K"   "Y.M|Y.K"  
# "W.M|W.K"   "B.M|B.K"   "G.M|G.K"   "X.X|X.L4"  "X.X|X.L4"  "O.W|B.MK"  "R.B|W.MK"  "X.X|X.G3"  "X.X|X.L6"  "YY.X|X.WK"

names(re)
str(re[-ind2,c("year","code","sex")])#Mad: 81 in std file...still some obs have wrong codes
re[-ind2,c("year","code","sex")] #Mad: previous to std file 90 codes not matching format; 33 after cleaning, but some might be alright as K is not included in allowed letters
re.incomplete.codes<-re[-ind2,"code"]

#---------
#corrected following codes (31/03/2016 in std file), added |
# L.G.M.O <NA>
# R.Y.M.B <NA>
# B.BM.B <NA>
#----------
cap[nchar(cap$ring)<6,] #MAIO: those without metal from 2007 and 2012; MAD: >295, only colour rings J or A
cap[nchar(cap$ring)>7, "ring"] #MAIO: 0; MAD 0

#--------------fix codes & data in wrong format in captures/birdref:----------

cap$code<-gsub(" ","",cap$code)
cap$ring<-gsub(" ","",cap$ring)

names(br)
br$parent3
br$parent1<-gsub(" ","",br$parent1)
br$parent2<-gsub(" ","",br$parent2)

# cap$sex<- gsub("f", "F", cap$sex)
# cap$sex<- gsub("m", "M", cap$sex)
table(cap$sex)

#Ceuta and Mad
table(cap$age)
# A    C    J 
# 2646   27 2699 
cap$age <- gsub("C","J", cap$age)
#table(cap$age)

# #other errors found in Ceuta:
# 
# #[6] "CA001" ring changed
# br[which(br$male %in% "CA001"), "male"]
# #br[which(br$male %in% "CA001"), "male"] <- "CA2221" #this ring does not exist in captures, for nest 2006-C-7 in captures the  ring for the male is CA2221
# 
# #[7] g instead of L:
# #2006-D-2 and 2006-D-41
# #br[br$male %in% "BX.WX|gX.XX", "male"] <- "BX.WX|LX.XX"
# 
# #2007-A-131
# #br[br$male%in% "RX.gX|BX.WX", "male"] <- "RX.LX|BX.WX"
# 
# #[8] extra space in Site of nest "2008-A-21 (in birdref):
# #br[br$nest %in% "21" & br$year %in% "2008" & br$site %in% "A", "site"] <- "A"
# br[br$year %in% "2008" & br$nest %in% "21", "site"] <- "A"
# 
# #previous list of codes in wrong format:
# #[1]
# #cap[cap$code %in% "XX.WB|XX.RWM",] #correct
# #cap[cap$ring %in% "802109721",]
# 
# #[2]
# #cap[cap$code %in% "XX.WX|XX.WMY",] #correct
# #cap[cap$ring %in% "802109744",]
# 
# #[3]
# #cap[cap$code %in% "XX.YR|XX.WWM",] #correct
# #cap[cap$ring %in% "802109716",]
# 
# #[4]
# regexp1 <- "([WX]{2})\\.([BX]{2})\\|([BX]{2})\\.([YM]{2})" #add $ at the end to find exact match
# ind<-str_detect(cap$code, pattern=regexp1)
# cap[ind,]
# 
# cap[cap$ring %in% "802109760",]
# #br[br$female %in% "802109760", "female"]#<-"CA1579" #comment in Clemens newest file: CR identical to CA1579 but most likely dead
# cap[cap$ring %in% "802109760", "code"] <- "WX.BX|BX.YM"#
# cap[cap$ring %in% c("CA1579", "802109765"),]
# #cap[cap$ring %in% "CA1579", "code"] <- "WX.BX|BX.YM" #CR changed previously WX.BX|sX.YM, 
#                                       #plus one with wrong code: LM|LB??? from nest 2010-A-105, 
#                                       #it seems that ring was wrong in the captures file I had? 
#                                       #in Luke's file it appears as CA1253 which belongs to LM|LB
# cap[cap$ID %in% "2010_A_105",] #Wrote Medardo 09/02/2016, wait for reply
# br[br$female %in% "CA1253",]
# br[br$female %in% "CA1579",]
# 
# #WX.BX|sX.YM look for this in resightings:
# re[re$code %in% "WX.BX|sX.YM",c("code","year")]
# re[re$code %in% "WX.BX|sX.YM","code"]<- "WX.BX|BX.YM" #change!
# cap[cap$code %in% "WX.BX|sX.YM",]
# cap[cap$code %in% "WX.BX|BX.YM",]
# 
# 
# #[5]
# #cap[cap$code %in% "BX.MX|GX.BX",]
# #cap[cap$code %in% "BX.MX|GX.BX","code"]
# #cap[cap$code %in% "BX.MX|GX.BX ", "code"] <- "BX.MX|GX.BX"
#-----------------------------------------------------------------

  #a) find duplicates in Ceuta:
# rownames(cap)
# cap$code.ring <- paste(cap$code,cap$ring, sep="-")
# cap$sex.code <- paste(cap$code, cap$sex, sep="-")
  
  #a) find duplicates in Mad: OMIT JUVENILES???? 24/03/2016
table(cap$species)
cap$sp.code.ring <- paste(cap$species,"-", cap$code,"-", cap$ring, sep="")
cap$sp.sex.code <- paste(cap$species,"_", cap$code, "-",cap$sex, sep="")

  #a) Duplicate in MAIO only
#dupl <- cap[which(cap$comments_stdfile =="duplicate"),c("code","ring")]
  
#   #b) list duplicates from Ceuta/Mad
# 
# library(stringr)
#unique.code.ring<- unique(cap$code.ring)
unique.code.ring<- unique(cap$sp.code.ring[cap$site %in% "Andavadoaka" & cap$age %in% "A"]) #3017 unique sp.code.ring

lookdup <-strsplit(unique.code.ring, "-")

library(plyr)
ldup <- ldply(lookdup) #turn list into two columns
#colnames(ldup) <- c("code","ring")
colnames(ldup) <- c("sp","code","ring")  
# 
#-------debug-----
head(ldup[order(ldup$code),], n=1300)
#----------------

#Get rid of ambiguous codes to delete duplicates:
pat0<-"X"
#------debug pat0---
# ldup[str_detect(ldup$code, pattern=pat0),]
# #------------------
# #pat<- "XX.XX"#get rid of ambiguous codes from list of duplicates
# pat<- "X\\.X"
# #------debug pat-----
# ldup[str_detect(ldup$code, pattern=pat),]
# #----------------------
# pat2<- "\\.X\\|[A-Z]{1}\\.X"
# #----debug pat2---
# ldup[str_detect(ldup$code, pattern=pat2),]
# #---------------
# pat3<- "X\\.[A-Z]{1}\\|X\\.[A-Z]{1}"
# #----debug pat3---
# ldup[str_detect(ldup$code, pattern=pat3),]
#---------------
# pat<- "X"
# #pat2 <- ".XX"
#  x <- ldup[!str_detect(ldup$code, pattern=pat) #Ceuta
#             & !str_detect(ldup$code, pattern=pat2),] 

#   x <- ldup[!str_detect(ldup$code, pattern=pat) #MAd
#              & !str_detect(ldup$code, pattern=pat2)
#             & !str_detect(ldup$code, pattern=pat3),] 
library(stringr)
x0 <- ldup[!str_detect(ldup$code, pattern=pat0),] #allow no Xs in codes (MAD)

str(x) #CEUTA: 529; 555; 530
      #Mad: 3015

str(x0) #MAD: 1676
x0[order(x0$code),]
# 
library(stringr) 
#x <- ldup[!str_detect(ldup$code, pattern=pat),]
# ind <- grep(ldup$sp_code, pattern=pat)
# with.x <- ldup[ind,]
# head(with.x)
# 
# head(with.x[order(with.x$sp_code),])
# str(x) #Mad: 3502 obs
# head(x)
# ind <- which(duplicated(x$code) | duplicated(x$code, fromLast=TRUE)) 

x0$sp.code <- paste(x0$sp, x0$code, sep="-")

ind <- which(duplicated(x0$sp.code) | duplicated(x0$sp.code, fromLast=TRUE))
str(x0[ind,]) #MAIO 34; 38; 36
#               #MAD: 168
# x1 <- x[ind,]
x1 <- x0[ind,]
x1[order(x1$code),]

# ind <- which(duplicated(ldup$sp_code) | duplicated(ldup$sp_code, fromLast=TRUE))#MAD
# str(ldup[ind,]) #2595 duplicates?? including XX
# str(ldup[-ind,]) #1801 non duplicates

# x1 <- ldup[ind,]
# x1[order(x1$sp_code),]
# 
dupl <- x1 #List of rings with duplicated codes 
str(dupl) #36 Ceuta, 168 in Mad without Juv
# #-------------debug duplicates-------------------
# cap[cap$ring %in% "CA2370",]
# cap[cap$ring %in% "CV007",]
# cap[cap$code %in% "OX.MX|GX.WX",]
# 
# 
# #------------------------------------------------

#2. Omit duplicated codes and rings from birdref
#rings.br <- union(br$male, br$female)
rings.br <- union(br$parent1, br$parent2)

ind <- which(rings.br %in% dupl$sp_code | rings.br %in% dupl$ring)
omit1 <- rings.br[ind] #list of rings to omit MAD: 876 individuals need to be omitted
rings.br[-ind]#Mad: 1794 individuals will be included
  
#---------------MAD:Check state of birdref-------------------
table(br$year)
table(br$site)
table(br$nest)

br1<-br
br<-br1

#names(br)[12]<-"species"
table(cap$species)
#table(br$species) #some need re-labelling
# br$species[br$species %in% "KIP"] <- "KiP"
# br$species[br$species %in% "WFP"] <- "WfP"
# br$species[br$species %in% "WP"] <- "WfP"

#------------------------------------------------
#---------------------------------------------------------------------
#Extract appearances:
library(stringr)
# 1. List all appearances of each individual in a year:
#   a. List all known males and females in BirdRef, 
#   a.1 use omit1 to clear duplicates
# names(br)
# head(br)
# males.with.dupl <- br[!is.na(br$male) &
#                         br$male != "XX.XX|XX.XX",]
# names(males.with.dupl)
# males <- males.with.dupl[!males.with.dupl$male %in% omit1, ] 
#Ceuta: males$sex <- "M"
#Ceuta: males$mate_sex <- "F"

# males$ring
# 
# females.with.dupl <- br[!is.na(br$female) & 
#                           br$female != "XX.XX|XX.XX",] #SITE CHANGE
# females <- females.with.dupl[!females.with.dupl$female %in% omit1,]
#Ceuta: females$sex <- "F"
#Ceuta: females$mate_sex <-"M"
#MAIO:
#colnames(males)[c(4,5,9:14)] <- c("ring","mate_ring","field_sex_focal","mol_sex_focal","captured_focalyear_focal","field_sex_mate","mol_sex_mate","captured_focalyear_mate") #SITE CHANGE

#CEUTA: (change cols to change names)
#colnames(males)[c(10,11,19,20)] <- c("ring","mate_ring","field_sex_focal","field_sex_mate") #SITE CHANGE
#MAIO:
#colnames(females)[c(5,4,9:14)] <- c("ring","mate_ring","field_sex_mate","mol_sex_mate","captured_focalyear_mate","field_sex_focal","mol_sex_focal","captured_focalyear_focal") #SITE CHANGE

#CEUTA: (change cols to change names)
#colnames(females)[c(11,10,19,20)] <- c("ring","mate_ring","field_sex_focal","field_sex_mate") #SITE CHANGE
#tail(females)
#-------------------------------------
#a.1 Madagascar:

head(br)
table(br$parent1)#2080

br[is.na(br$parent1) & !br$parent1 %in% "X.X|X.X",]
p1.with.dupl <- br[!is.na(br$parent1) &
                        !br$parent1 %in% "X.X|X.X",]
p1.with.dupl$parent1 #17671
names(p1.with.dupl)
p1 <-p1.with.dupl[!p1.with.dupl$parent1 %in% omit1, ] 
p1$parent1 #1629

p2.with.dupl <- br[!is.na(br$parent2) & 
                     !br$parent2 %in% c("X.X|X.X","UNK"),] #SITE CHANGE
p2 <- p2.with.dupl[!p2.with.dupl$parent2 %in% omit1,]
p2$parent2 #647

colnames(p1)[c(6,7,8,9,13,14,15,16)] <- c("ring","code","mate_ring", "mate_code", "mol_sex_focal","mol_sex_mate", "captured_focalyear_focal","captured_focalyear_mate") #SITE CHANGE
colnames(p2)[c(8,9,6,7,14,13,16,15)] <- c("ring","code","mate_ring","mate_code", "mol_sex_focal","mol_sex_mate", "captured_focalyear_focal","captured_focalyear_mate") #SITE CHANGE #FOUND ERROR...18/03/2016 column numbers had to be changed

#--------------------------------------

#ids.1 <- as.data.frame(rbind(males,  females))
ids.1 <- as.data.frame(rbind(p1,  p2))
head(ids.1)
tail(ids.1)
ids.1$ring

#pat<- "XX"#get rid of ambiguous codes from list of ids
#pat<- "UNK"
#ids.1[str_detect(ids.1$ring, pattern=pat),] 
#ids2 <- ids.1[!str_detect(ids.1$ring, pattern=pat),] 

#Maio:
#ids.1[str_detect(ids.1$ring, pattern=pat),"ring"]

#CEUTA:
#ids<-ids2[ids2$year != 2013,]#restrict ids from Ceuta to 2006-2012 (omit 2013)
#head(ids)
#ids$ring

#MAD: omit sites not in Andavadoaka
ids2<-ids.1
table(ids2$site)
ids<-ids2[ids2$site %in% "Andavadoaka",]
str(ids) #1990 obs

table(ids$year)


#head(ids)
str(ids.1) #MAD: 2276
str(ids2) #MAD: 2276
str(ids) #MAD:1990
#str(ids) #MAIO: 958 obs (up to 2014), 1129 (up to 2015)
          #Ceuta: 1031 obs (up to 2012)
#table(ids$year)
  #ids$ring
#--------------debug from further down the code...
# #look for nest 2014-WfP-106...which disappears in ids.final:
# head(ids)
# ids[ids$ring %in% "FH69208",] #doesn't appear
# omit1[omit1 %in% "FH69208"] #it's in list of omit1
#--------------------------------------------------
#   a.2 Add colour combinations/rings:
        #use Captures but restrict to adults, only 
        #combinations that are complete and the first
        #appearance of each combination to know ringing year

ids2<-ids #rename
ids<-ids2

table(cap$year)
table(cap$site)
table(ids$year)
table(cap$sex)

#Ceuta & Mad: 
table(cap$age)

#ids$code <- NA #this was added in std_file

#MAIO:
#cap.1 <- cap[cap$sex !="J",]#restrict captures to adults
#CEUTA & MAD:
cap.1 <- cap[cap$age !="J",]

cap.1$code
#pat<-"XX"
pat<- "X\\.X\\|M\\.X"#get rid of ambiguous codes from list of adult captures
pat2<-"X\\.X\\|X\\.M"
cap.1[grep(cap.1$code, pattern=pat),c("year","ring","code")]
cap.2<-cap.1[-grep(cap.1$code, pattern=c(pat)),] 
table(cap.2$code)
cap.1<-cap.2[-grep(cap.2$code, pattern=c(pat2)),]
table(cap.1$code)
#ind<-which(nchar(ids$ring)>9) #6 in Maio (more in 2nd run), 18 in Ceuta

#Unecessary with Std BirdRef file
# ind<-which(nchar(ids$ring)>7) #in Mad only those without any metal
# ids[ind,]
# 
# for(i in 1:length(ids$year)){ #if code is under ring...put it in code
# #  if(nchar(ids$ring[i])>9) #Ceuta and Maio
#   if(nchar(ids$ring[i])>7) #Mad...however some codes have exactly 7 char as metal rings...W.X|M.X so it creates errors
#   ids$code[i] <- ids$ring[i]
# }
# 
# for(i in 1:length(ids$year)){
#   pat <- "\\|"
#   if(length(grep(ids$ring[i], pattern=pat))>0){
#     ids$code[i] <- ids$ring[i]
#     ids$ring[i] <- NA
#   }  
# }


#------------------------
#In Madagascar some codes were assigned to adults when they were juveniles, so explore if the most recent code can be used instead of the code for the code at the first appearance as an adult-----------

ids[is.na(ids$code)| ids$code %in% "inconsistent codes", ]
rings.explore<-ids[is.na(ids$code)| ids$code %in% "inconsistent codes", "ring"]#82
unique(rings.explore)#69 from the list of birds with no code or inconsistent codes these rings will be the ones where new codes need to be assigned
# [1] "FH69234" "FH68731" "FH69181" "FH69116" "FH17810" "FH47901" "FH47160" "FH47260" "FH17846" "FH17803" "FH47983" "FH17804" "FH17280"
# [14] "FH68858" "FH47266" "FH47967" "FH68899" "FH72195" "FH68869" "FH17854" "FH17877" "FH47541" "FH69131" "FH72878" "FH73148" "FH73147"
# [27] "FH69111" "FH72248" "FH69204" "FH80050" "FH73433" "FH17834" "FH72704" "FH73437" "FH17826" "FH73415" "FH68762" "FH73404" "FH47804"
# [40] "FH73440" "FH73576" "FH69209" "FH47370" "FH69254" "FH47981" "FH72432" "FH68802" "FH72824" "FH68721" "FH69067" "FH47040" "FH47993"
# [53] "FH72481" "FH47025" "FH47947" "FH69237" "FH68796" "FH68850" "FH68981" "FH72277" "FH73253" "FH73320" "FH73436" "FH47944" "FH47123"
# [66] "FH47595" "FH17819" "FH73568" "FH69256"
a<-cap[cap$ring %in% rings.explore,c("year","date","ring","code","age")]

a<-a[order(a$ring, a$year,a$date),]
t(a)
b<-table(a$ring)
b

# cap.1[cap.1$ring %in% "FH69116",]
# cap[cap$ring %in% "FH72678",]
# cap[cap$code %in% "L.Y|M.O",]

#send inconsistent code note to comments--------
ids[ids$code %in% "inconsistent codes", "comments_stdfile"]<-"inconsistent codes?"
#-------------------
rownames(a)<-c(1:length(a$ring))
#i<-3782
#a[i,]
a$ring.repeat<-NA
a$ring.repeat[1]<-1
a$ring.repeat<-as.numeric(a$ring.repeat)

a$code.repeat<-NA
a$code.repeat[1]<-1
a$code.repeat<-as.numeric(a$code.repeat)


for(i in 2:length(a$ring)){
  #print(i)
  if(a$ring[i] == a$ring[i-1]){
    a$ring.repeat[i] <- a$ring.repeat[i-1]+1
  }else{
    if(a$ring[i] != a$ring[i-1]){
      a$ring.repeat[i] <- 1
    }
  }
}

for(i in 2:length(a$ring)){
  #print(i)
  #warnings(1)
  if(a$code[i] == a$code[i-1]){
    a$code.repeat[i] <- a$code.repeat[i-1]+1
  }else{
    if(a$code[i] != a$code[i-1]){
      a$code.repeat[i] <- 1
    }
  }
}

#List with rings that have codes that changed
code.change<-a[a$code.repeat != a$ring.repeat,]
unique(code.change$ring)#13 rings, also with ALL captures:

# [1] "FH17803" "FH17804" "FH17819" "FH17846" "FH17877" "FH47123" "FH47804"
# [8] "FH47983" "FH69111" "FH69116" "FH69209" "FH72277" "FH72432"

#try with ALL captures: ,/ same result
# b<-cap[!is.na(cap$ring),c("year","date","ring","code","age")]
# a<-b #and rerun last for loops from line 556

#----------------------------------------------------------------
#debug for loop:
ind<-which(ids$ring %in% "FH69181")
ids[ind,]
i<-427
ids[i,]
#---------------
ids.3<-ids
ids<-ids.3

for(i in 1:length(ids$year)){ #if code is empty, search ring in capt and fill it
  if(is.na(ids$code[i]) | ids$comments_stdfile[i] %in% "inconsistent codes"){
    if(ids$ring[i] %in% code.change$ring){
      ids$code[i] <- "inconsistent codes"
      ids$comments_stdfile[i] <- "inconsistent codes"#cap.1 omits juveniles
    }else{
      if(!ids$ring[i] %in% code.change$ring){
        ind<-which(cap$ring %in% ids$ring[i])
        cc<-cap[ind,]
        ids$code[i] <- cc$code[1] 
      }
    }
  }
}

#rings that appear in code.change should not have any code
ids[ids$ring %in% code.change$ring, "code"] <- "inconsistent codes"
ids[ids$ring %in% code.change$ring, "comments_stdfile"] <- "inconsistent codes"


#--------------------debug
tail(ids)
ids[ids$ring %in% code.change$ring,]
ids[is.na(ids$code),]
#----------------------
# for(i in 1:length(ids$year)){ #if ring has a code, search ring using code and replace#NOT IN MAD
#   if(nchar(ids$ring[i])>9)
#     ids$ring[i] <- cap.1$ring[match(ids$code[i], cap.1$code)]
# }

#exclude duplicates from captures for next for loop:
cap.nodupl<-cap.1[!cap.1$ring %in% omit1,]
# 
# for(i in 1:length(ids$year)){ #modification for MAD, search for rings of individuals with code but no ring
#   if(is.na(ids$ring[i]) & !is.na(ids$code[i]))
#     ids$ring[i] <- cap.nodupl$ring[match(ids$code[i], cap.nodupl$code)]
# }

#----develop/debug last for loop for Mad:
cap.nodupl[cap.nodupl$code %in% "B.W|Y.M",]
ids[ids$code %in% "B.W|Y.M",]
ids[is.na(ids$ring) & !is.na(ids$code),]
#---------------------------------------------

# for(i in 1:length(ids$year)){#if adult has no metal ring, use code instead of ring#NOT IN MAD
#   if(nchar(ids$ring[i])>9 | is.na(ids$ring[i]))
#     if(nchar(cap.1$ring[match(ids$code[i], cap.1$code)])<11)
#       ids$ring[i] <- cap.1$code[match(ids$code[i],cap.1$code)]
# }


#-----debug ----
head(ids)
tail(ids)

#MAIO:
# head(ids[ids$captured_focalyear_focal =="no" ,] )
# 
# cap[cap$ring %in% "CA1148",] #this was captured but as Female??

#MAD:
cap.1[cap.1$code %in% "X.X|X.M",]


#Both:
tail(ids[order(ids$ring),c("code","ring","year")])
ids[order(ids$ring), c("code","ring","year")]
head(ids[order(ids$ring), c("code","ring","year")])

ids[nchar(ids$ring)<12 & nchar(ids$ring)>8,]

# #and changes made to Ceuta-------------------
# cap[cap$ring %in% "BX.WX|SX.",]
# 
# ids[ids$ring %in% "BX.WX|SX.",c("ring","code")]<-"BX.WX|SX.XX"
# 
# #add code to ring where no metal ring was added:
# #ids[is.na(ids$ring), "ring"]<-ids$code[is.na(ids$ring)] #more NAs than it should be appear...
# #ind<-which(is.na(ids$ring) & is.na(ids$year))
# #ids<-ids[-ind,]
# 
# #ids[is.na(ids$ring),"ring"]
# #ids[ids$code %in% c("BX.WX|BX.OX","RX.MX|YX.BX"),]
# #str(ids[ids$code %in% c("BX.WX|BX.OX","RX.MX|YX.BX"),])
# 
# #ids[ids$code %in% c("BX.WX|BX.OX"),"ring"] <- "BX.WX|BX.OX"
# #ids[ids$code %in% c("RX.MX|YX.BX"),"ring"] <- "RX.MX|YX.BX"
#---------continue debug and changes made to Ceuta

# table(ids$code)
# unique(ids$code) #Maio: 472 unique codes, some ambiguous OX.MX|RX.??
# regexp<-"([OX]{2})\\.([MX]{2})\\|([RX]{2})\\.([A-Z]{2})$"
# cap.1[grep(cap.1$code, pat=regexp),]
# ids[grep(ids$code, pat=regexp),]

#Ceuta changes:
# which(br$female %in% "CA127") #corrected
# which(br$male %in% "CA001") #corrected
# which(ids$ring %in% "CA127 ") #corrected
# #ids[which(ids$ring %in% "CA001"), "ring"] <- "CA2221" #this ring does not exist in captures, for nest 2006-C-7 in captures the  ring for the male is CA2221
# #ids[27,]
# #ids[176,]
# 
# ids[ids$year %in% "2006" & ids$site %in% "C" & ids$nest==7,]
# br[br$year %in% "2006" & br$site %in% "C" & br$nest==7,]
# 
# #ids[ids$year %in% "2006" & ids$site %in% "C",]
# #br[br$year %in% "2006" & br$site %in% "C",]
# 
# 
# cap[cap$ring %in% "802109760",]
# cap[cap$ring %in% "CA001",]
# cap[cap$ring %in% "CA127",]
# cap[cap$code %in% "RX.MX|YX.BX",] #this code does not exist in captures, omit from ids
# 
# cap[cap$ring %in% "CA1579",]
# 
# 
# #RM|YB is an inexistent combination, omit from ids:
# cap.1[cap.1$code %in% "RX.MX|YX.BX",]
# cap[cap$code %in% "RX.MX|YX.BX",]
# #is.na(ids[ids$code %in% "RX.MX|YX.BX", "ring"])
# ids[ids$code %in% "RX.MX|YX.BX",]
# ids[ids$ring %in% "RX.MX|YX.BX",]
# 
# cap[cap$year %in% "2007" & cap$site %in% "B" & cap$nest %in% "109",]
# ids[ids$year %in% "2007" & ids$site %in% "B" & ids$nest %in% "109",] #code was RW|YB and has been corrected already from BirdRef
# 
# ind<-which(ids$ring %in% "RX.MX|YX.BX")
# #ids[c(177),c("ring","code")] <- "RX.WX|YX.BX" #Male from this nest was captured, correct ring is RX.WX|YX.BX
# #ids[177,]
# 
# ind<-which(ids$code %in% "RX.MX|YX.BX")
# ids<-ids[-ind,]
# #ids<-ids[-163,] #Get rid of male from nest 2007-B-1 with inexistent colour combination

#-----Changes made in MAIO, omit from Ceuta------ 

# omit2<-ids[is.na(ids$code)|is.na(ids$ring),c("nest","site","code","ring","year")]
# # # nest site        code   ring year
# # # 887  bs--8    S        <NA> CA4094 2014* this was ringed as chick...but it doesn't appear in alex captures
# # # 546     -9    S OX.RX|WX.WX   <NA> 2012* wrong code...in notes appears as that
# # # 7521    32    S OX.MX|RX.??   <NA> 2013* incomplete code, not captured
# # # 8811 bs--2    S        <NA> CA2929 2014* this ring does not exist in captures made note in mistakes of Maio 04/08/2015
# 
# str(omit2) #MAIO: 24 obs with missing ring or code
# cap[cap$ring %in% omit2$ring,] #all have ambiguous codes
# 
# 
# # #omit these for now for data extraction:
# ind <- which(ids$code %in% omit2$code[!is.na(omit2$code)]| ids$ring %in% omit2$ring[!is.na(omit2$ring)])
# ids2 <- ids[-ind,]
# str(ids2) #1102 observations
# 
# # #omit negative broods of 2014 that alex found before september
# pat <- "bs-"
# ind <- which(str_detect(ids2$nest, pattern=pat))
# ids2[ind,]#there is no bs--5, but bs--4 and bs--6! bs--5 had been omitted before as no adult was known
# ids.final <- ids2[-ind,] 
# ids.final[,c("year","site","nest","code","ring")]
#  

#----------------------------01/02/2016----
ids.final<-ids
str(ids.final) #1038 obs Ceuta; 1085 (09/02/2016, using BirdSoc);1031 (10/02/2016 up to 2012 only)
                #1093 obs Maio
                #1990 Mad
table(ids.final$year) 
table(ids.final$site)
#ids.final$nest.id <- paste(ids.final$year, ids.final$site, ids.final$nest, sep="-")
ids.final$nest.id <- paste(ids.final$year, ids.final$species, ids.final$nest, sep="-")
unique(ids.final$nest.id) #CEUTA 602; 601 (10/02/2016, using BirdSoc up to 2012)
                          #MAIO 721
                          #Mad 1472


#       a.3 Add year when metal was added and year when code was added (copied from create colour code field sheet.R)
#extract the year a code first was assigned to each ring and metal ringing year
# cap.1$code.ring <- paste(cap.1$code, cap.1$ring, sep="-")
# cap.1<-cap.1[order(cap.1$code.ring, cap.1$year, cap.1$date),]
names(cap)
cap$code.ring<-paste(cap$code, cap$ring, sep="-")

#year.cr <- aggregate(cap.1$year, by=list(cap.1$code.ring), min)
year.cr<-aggregate(cap$year, by=list(cap$code.ring), min)

#cap.1<-cap.1[order(cap.1$ring, cap.1$year, cap.1$date),]
#year.mr <- aggregate(cap.1$year, by=list(cap.1$ring), min)
year.mr <- aggregate(cap$year, by =list(cap$ring),min) #uses cap instead of cap.1 because cap.1 does not include juveniles, we want to know the actual year when metal ring was added

head(year.mr)
# cap.1$year.cr <- NA
cap$year.cr <- NA
cap$year.mr <- NA

# for(i in 1:length(cap.1$year)){
#   cap.1$year.cr[i] <- year.cr$x[match(cap.1$code.ring[i], year.cr$Group.1)]
# }
for(i in 1:length(cap$year)){
  cap$year.cr[i] <- year.cr$x[match(cap$code.ring[i], year.cr$Group.1)]
}



for(i in 1:length(cap$year)){
  cap$year.mr[i] <- year.mr$x[match(cap$ring[i], year.mr$Group.1)]
}


#ADD years to ids dataset
ids.final$year.cr <- NA
ids.final$year.mr <- NA

ids.final$ring[ids.final$ring == ids.final$code]<-"NA" #TO FIX ERROR IN MADAGASCAR
ids.final$code.ring <- paste(ids.final$code, ids.final$ring, sep="-")

# for(i in 1:length(ids.final$year)){ 
#     ids.final$year.cr[i] <- cap.1$year.cr[match(ids.final$code[i], cap.1$code)]
#     ids.final$year.mr[i] <- cap$year.mr[match(ids.final$ring[i], cap$ring)]
# }
for(i in 1:length(ids.final$year)){ 
    ids.final$year.cr[i] <- cap$year.cr[match(ids.final$code.ring[i], cap$code.ring)]
    ids.final$year.mr[i] <- cap$year.mr[match(ids.final$ring[i], cap$ring)]
}


#-------------debug a.3----------------16/03/2016
head(ids.final)
ids.final[,c("year.cr","year.mr")]
ids.final[ids.final$year.cr!=ids.final$year.mr, c("year.cr", "year.mr","ring")]
ids.final[ids.final$year.cr<ids.final$year.mr,]#CEUTA None ,/, colour rings should have been added after metal ring...unless CR changed (duplicates, omitted from this dataset)
                                              #MAIO None
                                              #MAD none

ids.final[is.na(ids.final$year.cr),] #CEUTA: none, Maio: none, MAD: individuals without metal appear here without year cr was added....change code.ring
ids.final[is.na(ids.final$year.cr),c("ring")]
ids.final[ids.final$code == ids.final$ring,]

ids.final[is.na(ids.final$year.mr),] #Ceuta: 1 (BX.WX|BX.OX); MAIO: none; MAD ones with no metal and those with wrong codes?

ids.final[is.na(ids.final$year.mr) & is.na(ids.final$year.cr),]

#-----------get rid of ub-------------------------
ids.final[ids.final$code %in% c("ub","Luke?"),"code"] <- NA
ids.final[ids.final$mate_code %in% c("ub","Luke?"),]
ids.final[ids.final$mate_ring %in% "Luke?", c("mate_code","mate_ring")]<-NA

#---------------------------
#-------------develop a.3
# cap.1<-cap.1[order(cap.1$code.ring, cap.1$year, cap.1$date),]
# tail(cap.1, n=300)

#-----------------------------------------------------------------------------

#----CEUTA: see if all rings-nests from BirdRef are in Capt-------------------------
# names(ids.final)
# names(cap)
# ids.final[ids.final$field_sex %in% "f","field_sex"] <- "F"
# ids.final[ids.final$field_sex %in% "m", "field_sex"]<- "M"
# 
# chick1<-ids.final[!is.na(ids.final$chick1), c("chick1","nest.id")]
# chick2<-ids.final[!is.na(ids.final$chick2), c("chick2","nest.id")]
# chick3<-ids.final[!is.na(ids.final$chick3), c("chick3","nest.id")]
# 
# ring.nestid.bre1<-paste(ids.final$ring, ids.final$nest.id, ids.final$field_sex, sep="-")
# ring.nestid.bre2<-paste(chick1$chick1,chick1$nest.id, "J", sep="-")
# ring.nestid.bre3<-paste(chick2$chick2, chick2$nest.id,"J", sep="-")
# ring.nestid.bre4<-paste(chick3$chick3, chick3$nest.id,"J",sep="-")
# ring.nestid.bre<-c(ring.nestid.bre1,ring.nestid.bre2,ring.nestid.bre3,ring.nestid.bre4)
# 
# cap$nest.id<-paste(cap$year, cap$site, cap$nest, sep="-")
# ring.nest.id.cap<-paste(cap$ring, cap$nest.id, cap$sex, sep="-")
# 
# 
# setdiff(ring.nestid.bre, ring.nest.id.cap) 
# #most of ids appearing here are males that were not captured
# 
# setdiff(ring.nest.id.cap, ring.nestid.bre)
# #some of this will be duplicates which were omitted from ids.final
# #ids from captures missing from ids.final
# 
# #omit those in  capt which were omitted from ids.final:
# ind <- which(cap$ring %in% dupl$code | cap$ring %in% dupl$ring)
# omit1 <- cap$ring[ind] #list of rings to omit
# cap.nodupl <- cap[!cap$ring %in% omit1,]
# 
# ring.nest.id.cap.nodupl<-paste(cap.nodupl$ring, cap.nodupl$nest.id, cap.nodupl$sex, sep="-")
# 
# setdiff(ring.nest.id.cap.nodupl, ring.nestid.bre)#169
# #includes captures from mistnets (with no nest), omit those:
# cap.nodupl.nest<-cap.nodupl[!is.na(cap.nodupl$nest),]
# ring.nest.id.cap.nodupl.nest<-paste(cap.nodupl.nest$ring, cap.nodupl.nest$nest.id, cap.nodupl.nest$sex, sep="-")
# 
# setdiff(ring.nest.id.cap.nodupl.nest, ring.nestid.bre) #133
# #most ids of juv missing are from adoptions or experimental broods
# 
# #-------------debug--------------
# cap.nodupl[cap.nodupl$nest.id %in% "2012-D-102",]
# cap[cap$nest %in% "210",]
# 
# ids.final[grep(ids.final$nest.id, pattern="2012-G.", perl=T),]
# ids.final[ids.final$nest.id %in% "2012-D-102",]
#------------
#Added missing info to nests directly in BirdRef file 03/02/2016

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#*********************************************************
#   b. Look for colour combinations of each individual in:

#         i. Resightings (first and last seen); 
#             ****Include surveys where available?? YES Jan2016 does not include them yet for MAIO
#             **Feb2016 2n run or Maio includes breeding season surveys
#                 *Ceuta 2012 resightings were missing, Medardo sent some 09/02/2016, include them:

#prepare resightings file:
re$date
min(cap$date); min(re$date)
max(cap$date); max(re$date)
re[is.na(re$date),]
#re[re$date<900,c("observer","year","date")] #Check Maio: there are resightings
re[re$date<300,c("observer","year","date")] # Check Mad
re[re$date>600,c("observer","year","date")]

#-------------minimum laying date and maximum laying dates:
#Madagascar
min(ne$found_date, na.rm=T)
max(ne$found_date, na.rm=T)

#--------------------------------------------------------------------

# #MAIO: in non-breeding seasons of 2009 and 2008, restrict dataset:
# re2<-re[re$date>=800,]
# re<-re2

#MAD: restrict resightings for length of fieldwork season: 300-605
re2<-re[re$date>=300 & re$date<=700,]
min(re2$date)
max(re2$date)  

re<-re2

#--------
re$real.date <- as.Date(ISOdate(re$year,re$date%/%100,re$date%%100), "%Y/%m/%d", tz="GMT")
re[is.na(re$real.date),] 
names(re)
re$real.date <- as.character(re$real.date)

#---------------------------
ids.final$year.code <- paste(ids.final$year, ids.final$code, sep="-")
re$year.code <- paste(re$year, re$code, sep="-")

ids.final$first.res <- NA
ids.final$last.res <- NA


for(i in 1:length(ids.final$year)){ 
  #ind <- grep(ids.final$year.code[i], re$year.code, fixed=TRUE) #corrected bug
  ind <- which(ids.final$year.code[i]==re$year.code)
  group.res<-re[ind,]

  if(length(group.res$year)>0){
    ids.final$first.res[i] <- min(group.res$real.date)
    ids.final$last.res[i] <- max(group.res$real.date)
    }else{
      ids.final$first.res[i] <- "NA"
      ids.final$last.res[i] <- "NA"
    }
}
str(ids.final$first.res)
ids.final$first.res 
ids.final$last.res 

#-----debug------
ids.final[250,]
max(group.res$real.date)

head(ids.final)
ind <- grep(ids.final$year.code[250], re$year.code, fixed=TRUE) #use which instead of grep
group.res<-re[ind,]
#---------------------
#---------------------

#         ii. Captures
#prepare file:
cap$date

cap$real.date <- as.Date(ISOdate(cap$year,cap$date%/%100,cap$date%%100), "%Y/%m/%d", tz="GMT")
head(cap)

names(re)
table(cap$date)
hist(cap$date)

cap.dates<-cap[cap$date >=300 & cap$date <=700,]
cap<-cap.dates
#silenced to see if error comes from this line#cap$real.date <- as.character(cap$real.date)
#---------------------------

ids.final$ring
cap$year.code <- paste(cap$year, cap$code, sep="-")
cap$year.ring <- paste(cap$year, cap$ring, sep="-")
ids.final$year.ring <- paste(ids.final$year, ids.final$ring, sep="-")

ids.final$first.cap <- NA
ids.final$last.cap <- NA


#CHANGE TO year.ring instead of code...26/01/2016
#for(i in 1:length(ids.final$year)){ 
#   ind <- grep(ids.final$year.code[i], cap$year.code, fixed=TRUE)
#   group.cap<-cap[ind,]
#   
#   if(length(group.cap$year)>0){
#     ids.final$first.cap[i] <- min(group.cap$real.date)
#     ids.final$last.cap[i] <- max(group.cap$real.date)
#   }else{
#     ids.final$first.cap[i] <- "NA"
#     ids.final$last.cap[i] <- "NA"
#   }
# }

for(i in 1:length(ids.final$year)){ 
  #ind <- grep(ids.final$year.ring[i], cap$year.ring, fixed=TRUE) #corrected bug
  ind <- which(ids.final$year.ring[i]==cap$year.ring)
  group.cap<-cap[ind,]
  
  if(length(group.cap$year)>0){
    ids.final$first.cap[i] <- as.character(min(group.cap$real.date, na.rm=T))
    ids.final$last.cap[i] <- as.character(max(group.cap$real.date, na.rm=T))
  }else{
    ids.final$first.cap[i] <- "NA"
    ids.final$last.cap[i] <- "NA"
  }
}


#---------------------
#turn first.cap and last.cap to real dates_
ids.final$first.cap.r <- as.Date(ids.final$first.cap, format="%Y-%m-%d")
ids.final$last.cap.r <- as.Date(ids.final$last.cap, format="%Y-%m-%d")

#-----debug------
str(ids.final[ids.final$first.cap!=ids.final$last.cap,])
  #74 adults were captured more than once in a year MAIO
  #397 adults were captured more than once CEUTA
  #135 adults in MAd

head(ids.final[ids.final$first.cap!=ids.final$last.cap,])
#---------------------


#---------------------
#07/08/2015
#         iii. Nests (LD or found_date, End_date, fate, coordinates, chicks, eggs)

names(ne)
names(ids.final)
head(ids.final)
#ne$nest.id <- paste(ne$year, ne$site, ne$nest, sep="-") #Ceuta and Maio

#re-label species in Nests to create nest.id
table(ne$species)
#ne[ne$species %in% "KIP","species"] <- "KiP"
#ne[ne$species %in% "WFP","species"] <- "WfP"

#ne$nest.id <- paste(ne$year, ne$species, ne$nest, sep="-")#Mad

#Check fate categories for nests
table(ne$fate)
#ne[ne$fate %in% "Hatch","fate"] <- "HATCH"

#-------match ld, fd, end_date, etc
for(i in 1:length(ids.final$year)){ 
  ids.final$laying_date[i] <- as.numeric(ne$laying_date[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$found_date[i] <- as.numeric(ne$found_date[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$end_date[i] <- as.numeric(ne$end_date[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$fate[i] <- as.numeric(ne$fate[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$northing[i] <- as.numeric(ne$northing[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$easting[i] <- as.numeric(ne$easting[match(ids.final$nest.id[i], ne$id.nest)])
  #ids.final$latitude[i] <- ne$Latitude[match(ids.final$nest.id[i], ne$nest.id)]
  #ids.final$longitude[i] <- ne$Longitude[match(ids.final$nest.id[i], ne$nest.id)]
  ids.final$no_chicks[i] <- as.numeric(ne$no_chicks[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$clutch_size[i] <- as.numeric(ne$clutch_size[match(ids.final$nest.id[i], ne$id.nest)])
  ids.final$hatch_date[i]<-as.numeric(ne$hatch_date[match(ids.final$nest.id[i], ne$id.nest)])
}

#-----debug------
head(ids.final)
head(ids.final[!is.na(ids.final$clutch_size),])

#---------------------
#convert dates to real dates
ids.final$laying_date<-as.numeric(ids.final$laying_date)
ids.final$found_date <- as.numeric(ids.final$found_date)
ids.final$end_date <- as.numeric(ids.final$end_date)

ids.final$laying_date.r <- as.Date(ISOdate(ids.final$year,ids.final$laying_date%/%100,ids.final$laying_date%%100), "%Y/%m/%d", tz="GMT")
ids.final$found_date.r <- as.Date(ISOdate(ids.final$year,ids.final$found_date%/%100,ids.final$found_date%%100), "%Y/%m/%d", tz="GMT")
ids.final$end_date.r <- as.Date(ISOdate(ids.final$year,ids.final$end_date%/%100,ids.final$end_date%%100), "%Y/%m/%d", tz="GMT")
ids.final$hatching_date.r <- as.Date(ISOdate(ids.final$year,ids.final$hatch_date%/%100,ids.final$hatch_date%%100), "%Y/%m/%d", tz="GMT")#CEUTA
ids.final$laying_date.r <- as.character(ids.final$laying_date.r)
ids.final$found_date.r <- as.character(ids.final$found_date.r)
ids.final$end_date.r <- as.character(ids.final$end_date.r)

#-----debug------
head(ids.final)
head(ids.final[!is.na(ids.final$clutch_size),])
str(ids.final)#1756, 1999 after update

ids.final[is.na(ids.final$laying_date),]
#-----------------------------------------------------MAD ran up to here 21/March/2016 issue3 was detected so stopped

#         iv. Broodfates (earliest and latest dates when male or female was seen)
names(bf)
#bf$nest.id <- paste(bf$year, bf$site, bf$brood, sep="-") #MAIO CEUTA
table(bf$species)
bf$nest.id <- paste(bf$year, bf$species, bf$brood, sep="-") #MAD
#------------------------------
#NEW SECTION ADDED after Maio's 2nd run
bf$date <- as.numeric(bf$date)
bf$real.date <- as.Date(ISOdate(bf$year,bf$date%/%100,bf$date%%100), "%Y/%m/%d", tz="GMT")

              #check nest format
    #table(bf$site)
#     bf[bf$site %in% "Salina","site"]<- "S"
#     bf[bf$site %in% "MORRO","site"]<-"M"
    
    table(bf$brood)
#     bf[grep(bf$brood, pat="Â"),]
#     bf$brood<- gsub("Â", "-", bf$brood)
#     bf[bf$brood %in% "M1",]
#     bf$brood <- gsub("M", "", bf$brood)
#------------------------------              
            #check parents categories (4- both, 3-only male, 2-only female)          
              #table(bf$parents)
#Maio
#               bf[bf$parents %in% "2 OR 4",]
#               bf[c(206,136),]
#               bf[bf$parents=="2 OR 3",]
#               bf[bf$parents=="F",]
#               bf[bf$parents %in% "4?",]
#               bf[bf$parents %in% "?",] 
#               bf[bf$parents %in% "#N/A",] 
#               bf[bf$parents %in% "-1",]
#changes to Maio:             
#               bf$parents[bf$parents=="no parent"] <- 0
#               bf$parents[bf$parents=="?"] <- NA 
#               bf$parents[bf$parents=="#N/A"] <- NA
#               bf$parents[bf$parents =="F"] <- 2
#               bf$parents[bf$parents == "4?"] <- NA 
#               bf$parents[bf$parents =="-1"] <- NA
#               bf$parents[bf$parents == "2 or 3"] <- NA
#               bf$parents[bf$parents == "2 OR 3"] <- NA
#               bf$parents[bf$parents == "2 OR 4"] <- NA
#               bf$parents[bf$parents == "3?"] <- NA
#
#Ceuta
#               bf$parents[bf$parents %in% "?"] <- NA
#               bf$parents[bf$parents=="2+"] <- 2
#               bf$parents[bf$parents=="3+"] <- 3
#               bf$parents[is.na(bf$parents)]
              #table(bf$parents)

#Madagascar
# table(bf$parent1)
# table(bf$parent2)
table(bf$parent)

#--------debug----------
# ids.final$mol_sex <- as.character(ids.final$mol_sex)
# ids.final$field_sex <- as.character(ids.final$field_sex)
# 
# sex.check<-ids.final[ids.final$mol_sex_focal %in% "F" & ids.final$field_sex_focal %in% "M",]
# sex.check2<-ids.final[ids.final$mol_sex_focal %in% "M" & ids.final$field_sex_focal %in% "F",]
# # 
# cap[cap$ring %in% sex.check$ring,]
# cap[cap$ring %in% sex.check2$ring,]
# 
# sex[sex$ring %in% sex.check$ring,] #CA3037 inconsistent mol sex from sex file and mol_sex column!
# # sex[sex$ring %in% sex.check2$ring,]
# # unique(sex.check$ring)
# # 
# br[br$male %in% sex.check$ring,]
# br[br$male %in% sex.check2$ring,]
# 
# ids.final[ids.final$nest.id %in% "2006-D-1", c("northing","easting")]
# ids.final[, c("northing","easting")]
# #10/08/2015 found inconsistent mol_sex in CA3037...bug in BirdRef Cleaning code
# #corrected BirdRef std file ,/
#----------------------------------------
#-----------Add mol_sex to Madagascar...omitted from std file-----------------------------
head(ids.final)
head(sex)
ids.final[is.na(ids.final$ring),]

for (i in 1:length(ids.final$year)){
  if(!is.na(ids.final$ring)){
    ids.final$mol_sex_focal[i] <- sex$sex[match(ids.final$ring[i], sex$ring)]
    if(!is.na(ids.final$mate_ring)){
      ids.final$mol_sex_mate[i] <- sex$sex[match(ids.final$mate_ring[i], sex$ring)]
    }
  }
}


#----------------------------------------
#----------------------------------------
# identify if male, female or both parents were seen and min max dates
#22/01/2016 this part was modified as it was based on mol_sex,
#           but it should be based on field_sex as observers considered the colour rings 
#           originally under male or female as in the original file of BirdRef. Corrected code to use field_sex
names(ids.final)

ids.final$first.bf.adult <- NA
ids.final$last.bf.adult <- NA
ids.final$adult.not.seen.bf <- NA
ids.final$times.adult.not.seen <- NA
ids.final$total.brood.res <- NA
ids.final$last.bf.adult.before.desertion <- NA
ids.final$datenot.seen.after.desertion <- NA
ids.final$bf.quality <- NA

unique(bf$comments)
# ids.final$consec.not.seen.all <- NA
# ids.final$consec.not.seen.any <- NA
# ids.final$consec.times.not.seen <- NA

#----debug for loop----
which(ids.final$nest.id %in% "2014-WfP-108")
i<-813
ids.final[i,]
#-------------------------

for(i in 1:length(ids.final$year)){  #for loop brood fates adults
  #ind <- grep(ids.final$nest.id[i], bf$nest.id, fixed=TRUE) #corrected bug
  #print(i)
  #options(warn=1)
  ind <- which(ids.final$nest.id[i] == bf$nest.id)
  group.bf<-bf[ind,]
  group.bf<-group.bf[order(group.bf$date, group.bf$time) & !is.na(group.bf$parents),]

  ids.final$total.brood.res[i] <- length(group.bf$date)
if(!is.na(ids.final$mol_sex_focal[i]))  
  if(length(group.bf$date)>0 & ids.final$mol_sex_focal[i] == "M"){
    group.bf$counter <- c(1:length(group.bf$date))
    group.bf$date.time<-paste(group.bf$date, group.bf$time, sep="-")  
    group.bf1<-group.bf[group.bf$parents == 3 | group.bf$parents == 4,]
      
      if(length(group.bf1$date)>0){
        ids.final$first.bf.adult[i] <- min(group.bf1$date, na.rm=T)
        ids.final$last.bf.adult[i] <- max(group.bf1$date, na.rm=T)
        
        
        ind <- which(group.bf$date.time == max(group.bf1$date.time, na.rm=T))
        #change for Maio where time is unavailable: ids.final$adult.not.seen.bf[i] <- group.bf$date[ind+1]
        ids.final$adult.not.seen.bf[i] <- ifelse(length(ind)>1, group.bf$date[ind+1][2], group.bf$date[ind+1])
        ids.final$times.adult.not.seen[i] <- length(group.bf$date[group.bf$parents ==2]) #needs to distinguish between consecutive observations...
        #ids.final$times.adult.not.seen[i] <- length(group.bf$date[group.bf$date>max(group.bf1$date)]) #changed because would omit some missing dates
        
        if(length(group.bf$parents[group.bf$parents==2])>0){
          b<-rle(group.bf$parents) 
          c<-cumsum(b$lengths)    
          c.max<-c[max(which(b$lengths >=2 & b$values==2))]
          b.max<-b$lengths[max(which(b$lengths >=2 & b$values==2))]
          
          if(!is.na(c.max) & !is.na(b.max) & c.max-b.max !=0){                 
          ids.final$last.bf.adult.before.desertion[i]<-group.bf$date[c[max(which(b$lengths >=2 & b$values==2))] #female desertions ==3, male ==2
                                                        -b$lengths[max(which(b$lengths >=2 & b$values==2))]]
          ids.final$datenot.seen.after.desertion[i]<-group.bf$date[c[max(which(b$lengths >=2 & b$values==2))] #female desertions ==3, male ==2
                                                      -b$lengths[max(which(b$lengths >=2 & b$values==2))]+1]
          }else{
            ids.final$last.bf.adult.before.desertion[i] <- "NA"
            ids.final$datenot.seen.after.desertion[i]<- "NA"
          }
#           consec<-group.bf[group.bf$parents==2,]
#           ids.final$consec.not.seen.all[i] <- all(diff(consec$counter)==1) #if TRUE means that all obs are consecutive, if TRUE at least two obs are non-consecutive
#           ids.final$consec.not.seen.any[i]<- any(diff(consec$counter)==1)
#           ids.final$consec.times.not.seen[i] <- sum(diff(consec$counter)==1)+1
        }
      }
  }
  
    if(length(group.bf$date)>0 & ids.final$mol_sex_focal[i] == "F"){
      group.bf$date.time<-paste(group.bf$date, group.bf$time, sep="-")  
      group.bf$counter <- c(1:length(group.bf$date))
      group.bf2<-group.bf[group.bf$parents == 2 | group.bf$parents == 4,]
      
      
      if(length(group.bf2$date)>0){
      ids.final$first.bf.adult[i] <- min(group.bf2$date, na.rm=T)
      ids.final$last.bf.adult[i] <- max(group.bf2$date, na.rm=T)
      
      #Added date when adult was not seen to be able to estimate abandoning date or duration of care
      ind <- which(group.bf$date.time == max(group.bf2$date.time, na.rm=T))
      #changed: ids.final$adult.not.seen.bf[i] <- group.bf$date[ind+1]
      ids.final$adult.not.seen.bf[i] <- ifelse(length(ind)>1, group.bf$date[ind+1][2], group.bf$date[ind+1])
      ids.final$times.adult.not.seen[i] <- length(group.bf$date[group.bf$parents == 3])
      #ids.final$times.adult.not.seen[i] <- length(group.bf$date[group.bf$date>max(group.bf2$date)])
      
        if(length(group.bf$parents[group.bf$parents==3])>0){
          b<-rle(group.bf$parents) 
          c<-cumsum(b$lengths)          
          c.max<-c[max(which(b$lengths >=2 & b$values==3))]
          b.max<-b$lengths[max(which(b$lengths >=2 & b$values==3))]
          
          if(!is.na(c.max) & !is.na(b.max) & c.max-b.max !=0){ 
          ids.final$last.bf.adult.before.desertion[i]<-group.bf$date[c[max(which(b$lengths >=2 & b$values==3))] #female desertions b$values==3, male ==2
                                                                  -b$lengths[max(which(b$lengths >=2 & b$values==3))]]
          ids.final$datenot.seen.after.desertion[i]<-group.bf$date[c[max(which(b$lengths >=2 & b$values==3))] #female desertions ==3, male ==2
                                                                -b$lengths[max(which(b$lengths >=2 & b$values==3))]+1]   
          }else{
            ids.final$last.bf.adult.before.desertion[i] <- "NA"
            ids.final$datenot.seen.after.desertion[i]<- "NA"
          }
#           consec<-group.bf[group.bf$parents==3,]
#           ids.final$consec.not.seen.all[i] <- all(diff(consec$counter)==1) #if FALSE means that all obs are consecutive, if TRUE at least two obs are non-consecutive
#           ids.final$consec.not.seen.any[i]<- any(diff(consec$counter)==1)
#           ids.final$consec.times.not.seen[i] <- sum(diff(consec$counter)==1)+1
        }
      }
    }
}
 


#add first date and last date when brood was seen
for(i in 1:length(ids.final$year)){ 
  #print(i)
  options(warn=0)
  ids.final$first.bf[i] <- min(bf$date[which(ids.final$nest.id[i]== bf$nest.id)])
  ids.final$last.bf[i] <- max(bf$date[which(ids.final$nest.id[i]==bf$nest.id)])
}

#-------debug----------03/02/2016
head(ids.final)
rownames(ids.final)<-1:length(ids.final$year)
names(bf)

min(bf$date[which(ids.final$nest.id[1]== bf$nest.id)])
bf[bf$nest.id %in% "2007-S--10",]

table(ids.final$mol_sex)
ids.final$first.bf
ids.final$last.bf
ids.final[,c("first.bf","last.bf","nest.id")]
ids.final[298,]

ids.final$first.bf<-as.numeric(ids.final$first.bf)
ids.final$last.bf<-as.numeric(ids.final$last.bf)
str(ids.final[!ids.final$first.bf %in% "Inf" & ids.final$first.bf > ids.final$last.bf,c("first.bf","last.bf","nest.id")])
bf[bf$nest.id %in% "2009-A-6",]
str(bf$date)
str(ids.final[!is.na(ids.final$first.bf),]) #MAIO: 523 non NAs, 428 NAs (up to 2014);
                                            #MAIO: 608 non NAs, 508 NAs (up to 2015);
                                            #MAIO: second run 1124 non NAs

tail(ids.final)
head(ids.final)
ind1<-sample(rownames(ids.final[ids.final$fate %in% "HATCH",]), 5)
ids.final[ind1,]
# 
# rownames(ids.final)<-1:length(ids.final$year)
# ids.final[ids.final$nest.id %in% "2012-D-4",]
# ids.final[ids.final$nest.id %in% "2009-D-102",]
# ids.final[,]
# ind2 <- which(ids.final$nest.id[337] == bf$nest.id)
# group.bf<-bf[ind2,]
# 
# #---------2nd debug CEUTA 05/02/2016 ----
# ids.final[!is.na(ids.final$first.bf.adult) & ids.final$total.brood.res >0,c("nest.id","times.adult.not.seen", "total.brood.res")]
# ids.final[is.na(ids.final$times.adult.not.seen)& ids.final$total.brood.res>0, c("nest.id","times.adult.not.seen", "total.brood.res")]
# ids.final[1002,]
# bf[bf$nest.id %in% "2011-E-1",]
# ids.final[ids.final$nest.id %in% "2006-C-5",]

#some nests appear with NA in times.adult.not.seen instead of 0 (when total.brood.res >0)
#this is because some brood obs have more than one observation in one day...decide what to do with those.
#These nests are those were the male or female were never seen, code is ok

bf$nest.date<-paste(bf$nest.id, bf$date, sep="-")
b<-as.data.frame(table(bf$nest.date))
str(b)
ind<-which(bf$nest.date %in% b$Var1[b$Freq>1])
tail(bf[ind,])
str(bf[ind,c("parents","chicks","nest.id","date")]) #715 with repeated dates

#---------2nd debug MAIO--------22/01/2016
#ids.final[is.na(ids.final$field_sex), c("ring","field_sex","sex","mol_sex")]
#------------------------

#Latest date when chicks were still alive:


#prepare bf data:

bf1<-bf
table(bf1$chicks)
#bf1$chicks[bf1$chicks %in% "0+"]<- 0
#bf1$chicks[bf1$chicks %in% "+"]<- 0
#bf$chicks[bf$chicks %in% "+1"] <- 1
bf1$chicks[bf1$chicks %in% "1+"] <- 1

bf1$chicks[bf1$chicks %in% "2+"] <- 2
#bf$chicks[bf$chicks %in% "+2"] <- 2

#bf1$chicks[bf1$chicks %in% "3+" | bf1$chicks %in% "3+1"] <- 3
#bf1$chicks[bf1$chicks %in% "4+"] <- 4

head(bf1)

ids.final$chicks.alive.last <- NA
ids.final$chicks.not.seen.bf <- NA

for(i in 1:length(ids.final$year)){ 
  #ind <- grep(ids.final$nest.id[i], bf$nest.id, fixed=TRUE) #corrected bug
  print(i)
  ind <- which(ids.final$nest.id[i] == bf1$nest.id)
  group.bf<-bf1[ind,]
  group.bf<-group.bf[order(group.bf$date),]
  chicks.bf<-group.bf[group.bf$chicks > 0,]
  if(length(group.bf$date)>0 & length(group.bf$chicks[group.bf$chicks>0])>0){
    ids.final$chicks.alive.last[i] <- max(chicks.bf$date, na.rm=T)
    }else{
      ids.final$chicks.alive.last[i]<-NA
    }
  
  if(length(chicks.bf$date)>0 & length(group.bf$date)>length(chicks.bf$date)){
    ind2 <- which(group.bf$date == max(chicks.bf$date, na.rm=T))
    ind3<-ind2+1
    ids.final$chicks.not.seen.bf[i] <- group.bf$date[ind3]
    }else{
      ids.final$chicks.not.seen.bf[i]<-NA
    }
  if(length(chicks.bf$date)==0 & length(group.bf$date)>0){
    ids.final$chicks.not.seen.bf[i] <- min(group.bf$date, na.rm=T)
  }
}

#-------debug----------
str(bf[bf$chicks==0,]) #MAIO: 1995 (2014); 2741 (2015)
str(bf[bf$chicks =="#N/A",])#323 NAs, 
str(bf[bf$chicks !="#N/A",]) #2249 not NAs

head(ids.final)
tail(ids.final)
ind<-sample(rownames(ids.final[ids.final$fate %in% "HATCH",]),5) #Maio: checked 5 at random and they were ok
ids.final[ind,]


ids.final[ids.final$nest.id =="2007-A-122",]
ids.final[6131,]

re[re$year.code %in% "2007-OX.MX|GX.GX",]
cap[cap$year.ring %in% "2007-CA172",]
ne[ne$nest.id =="2007-A-122",]
bf[bf$nest.id =="2010-A-16",]

#------look for a case where female disappeared for more than 2 times and count
ids.final[ids.final$total.brood.res >5 & ids.final$times.adult.not.seen >1 
            #& ids.final$times.adult.not.seen <3
            ,c("times.adult.not.seen", "total.brood.res","nest.id")]

bf[,c("nest.id","parents","date")]
bf1[bf1$nest.id =="2010-S-92",]
ids.final[ids.final$nest.id =="2010-S-92",]

    #debug: for loop brood fates adults
ind <- which(ids.final$nest.id[298] == bf$nest.id)
group.bf<-bf[ind,]
group.bf<-bf[ind,]
group.bf<-group.bf[order(group.bf$date, group.bf$time) & !is.na(group.bf$parents),]
# group.bf$counter <- c(1:length(group.bf$date))
# consec<-group.bf[group.bf$parents==3,]
# all(diff(consec$counter)==1) #if FALSE means that all obs are consecutive, if TRUE at least two obs are non-consecutive
# any(diff(consec$counter)==1)
# ind2<-which(diff(consec$counter)==1)
# sum(diff(consec$counter)==1)+1
# consec[ind2,]
# rle(group.bf$parents)
#ids.final$consec.times.not.seen
#ids.final[!is.na(ids.final$year) & ids.final$consec.times.not.seen ==1,]

ids.final[298,]
ids.final[!is.na(ids.final$year)& !is.na(ids.final$datenot.seen.after.desertion),c("datenot.seen.after.desertion","last.bf.adult.before.desertion", "nest.id")]
str(bf[bf$nest.id %in% "2015-S-2","date"])
ids.final[ids.final$nest.id %in% "2015-S-2",]
#-------------------------------------------------------
#add whether times not seen are consecutive...10/02/2016----code trials:
a<-bf[bf$nest.id %in% "2009-A-6","parents"] #developed with Ceuta population data
a1<-c(a,4,4)
ind <- which(bf$nest.id %in% "2009-A-6")
group.bf<-bf[ind,]
b<-rle(group.bf$parents) #real example

x<-c(4,3,3,4,3,3)
x<-c(4,4,2,3,4,2,3,3)
b<-rle(a1) #made up example

max(which(b$lengths >=2 & b$values==3))

c<-cumsum(b$lengths)

c[max(which(b$lengths >=2 & b$values==3))]-b$lengths[max(which(b$lengths >=2 & b$values==3))]

group.bf$date[c[max(which(b$lengths >=2 & b$values==3))] #female desertions ==3, male ==2
              -b$lengths[max(which(b$lengths >=2 & b$values==3))]]

group.bf$date[c[max(which(b$lengths >=2 & b$values==3))] #female desertions ==3, male ==2
              -b$lengths[max(which(b$lengths >=2 & b$values==3))]+1]

x[c[max(which(b$lengths >=2 & b$values==3))]-b$lengths[max(which(b$lengths >=2 & b$values==3))]]
#-------------------------------------------------------------------------------------------------

#---------------debug last.bf.adult.before.desertion & datenot.seen.after.desertion----------
ind<-which(!is.na(ids.final$last.bf.adult.before.desertion) & !is.na(ids.final$datenot.seen.after.desertion))
b5<-ids.final[ind, c("last.bf.adult.before.desertion","datenot.seen.after.desertion","nest.id")]

ind<-sample(rownames(b5),5)
b5[ind,]

bf1[bf1$nest.id =="2009-D-1",]
ids.final[ids.final$nest.id =="2009-D-1",]


#-------------------------------------------------------------------------------------------------

#------------Rename for undo
ids.final.1<-ids.final
names(ids.final.1)
head(ids.final.1)
#------------------------

#change dates from bf to real dates

#Ceuta:cols<- c(45,46,47,50,51,52,53,54,55) #choose columns to convert to real dates
#Maio:
cols<-c(40,41,42,45,46,47,48,49,50)

ids.final.1[,cols]<-apply(ids.final.1[,cols], 2, function(x) as.numeric(as.character(x)))

names(ids.final.1[,cols])
head(ids.final.1)
head(ids.final)

ids.final.1$first.bf.adult.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$first.bf.adult%/%100,ids.final.1$first.bf.adult%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$last.bf.adult.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$last.bf.adult%/%100,ids.final.1$last.bf.adult%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$adult.not.seen.bf.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$adult.not.seen.bf%/%100,ids.final.1$adult.not.seen.bf%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$last.bf.adult.before.desertion.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$last.bf.adult.before.desertion%/%100,ids.final.1$last.bf.adult.before.desertion%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$datenot.seen.after.desertion.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$datenot.seen.after.desertion%/%100,ids.final.1$datenot.seen.after.desertion%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$first.bf.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$first.bf%/%100,ids.final.1$first.bf%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$last.bf.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$last.bf%/%100,ids.final.1$last.bf%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$chicks.alive.last.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$chicks.alive.last%/%100,ids.final.1$chicks.alive.last%%100), "%Y/%m/%d", tz="GMT")
ids.final.1$chicks.not.seen.bf.r <- as.Date(ISOdate(ids.final.1$year,ids.final.1$chicks.not.seen.bf %/%100,ids.final.1$chicks.not.seen.bf %%100), "%Y/%m/%d", tz="GMT")
# ids.final$first.bf.r <- as.character(ids.final$first.bf.r)
# ids.final$last.bf.r <- as.character(ids.final$last.bf.r)
# ids.final$chicks.alive.last.r <- as.character(ids.final$chicks.alive.last.r)


str(ids.final.1)
names(ids.final.1)
head(ids.final.1)



#----------------------------------------------------------------
#------------Get rid of unused columns----------------
#ids.final<-ids.final.1[,c(1:19, 24,25,29:33,34:36,40:42)] #MAIO
#ids.final<-ids.final.1[,c(1:5,8:17,28:35,39:40,45:51)] #CHANGE
ids.final<-ids.final.1
#----------------------------------------------------------------
#----------------------------------------------------------------

#-------------debug------------
str(ids.final[ids.final$year.cr == ids.final$year,]) #MAIO:530 all of these were colour ringed in the focal year
                                                      #CEUTA: 570
str(ids.final[ids.final$year.cr != ids.final$year,]) #MAIO: 620 not ringed in focal year
                                                      #CEUTA: 477

names(ids.final)
str(ids.final)
table(ids.final$fate)
ids.final$laying_date.r <- as.Date(ids.final$laying_date.r)
ids.final$end_date.r <- as.Date(ids.final$end_date.r)

table(ids.final$laying_date.r + 25 - ids.final$hatching_date.r)
mean(ids.final$laying_date.r + 25 - ids.final$hatching_date.r, na.rm=T)#CEUTA: 0.74
se(ids.final$laying_date.r + 25 - ids.final$hatching_date.r)#CEUTA: 0.19

ind<-which(ids.final$laying_date.r+25-ids.final$hatching_date.r< -10)
ind<-which(ids.final$laying_date.r+25-ids.final$end_date.r< -10)
ind<-which(ids.final$laying_date.r+25-ids.final$hatching_date.r>10) #non of these nests hatched
x<-ids.final[ind,]
x[x$fate %in% "HATCH", "nest.id"] #MAIO: "2010-S-108" "2010-S-108"
                                  #CEUTA: "2012-C-101" "2012-D-1"   "2012-D-4"   "2012-D-5"   "2012-D-6"   "2012-D-101" "2012-G-1"   "2012-G-6"   "2012-G-9"  
#                                         "2012-B-101" "2012-C-101" "2012-D-1"   "2012-D-4"   "2012-D-5"   "2012-G-4"   "2012-G-6"   "2012-G-12" 



#MAIO's potential errors found 12/08/2015:
# 2010-S-108 ,/ **corrected see below (only for this study, comment was added in comments_stdfile)   
# 2012-S-211 ,/ **corrected 
# 2013-S-206 ,/ **corrected
#"2010-S-55" ,/ no mistake 
#"2013-M-1"  ,/ no mistake 
#"2013-R-228" ,/ no mistake but LD should be earlier
#"2014-S-11" ,/ no mistake but LD should be earlier

#****MAIO: change for this study LD of 2010-S-108:
#ne[ne$nest.id=="2010-S-108",]#* laying date is wrong, nest was found on 918 with 1 egg, on 921 2 eggs were floated and on 930 3 eggs were found..
# #use 1016 - 25 as Laying date = 921
# 
 ids.final[ids.final$nest.id=="2010-S-108", "laying_date.r" ] <- "2010-09-21" 
 ids.final[ids.final$nest.id=="2010-S-108",]


#----------------------------------------------------------------

#----------------------------------------------------------------

############################################################################
############################################################################
############################################################################
#2.---------Estimate laying dates of broods with no LD broods ----------
#25/01/2016 ( specially negative broods)---------------
############################################################################
############################################################################
############################################################################

#a. Add dates when chicks were captured, 
#add bill, tarsus and weight of all chicks

#there are chicks without metal rings which have non-individual codes (example 2007-S--2)
#in these cases, nests need to be omitted

names(cap)
names(ids.final)

cap$nest.id <- paste(cap$year, cap$site, cap$nest, sep="-")

#-----------------------
#omit chicks with no metal ring

  #check format of rings from chicks
table(ids.final$chick1)
table(ids.final$chick2)
table(ids.final$chick3)

#Ceuta:
#ids.final$chick1[ids.final$chick1 %in% 
#                   c("unassigned","unknown","DEAD.CHICK.C101.2009","UR1.A1.2009","A109.DEAD.CHICK","DEAD.CHICK.A4.2008")]<-NA
#ids.final$chick2[ids.final$chick2 %in% 
#                   c("unassigned","unknown","DEAD.CHICK.E1.2009","E102.DEAD.CHICK","UR1.C101.2009","UR2.A1.2009")]<-NA
#ids.final$chick3[ids.final$chick3 %in% 
#                   c("unassigned","unknown","unringed","D4.CHICK","E3.CHICK","E8.CHICK","UR2.C101.2009","XX.XX|SX.XX","DEAD.CHICK.C12.2007")]<-NA

#-------re-name-----------

ids.final1<- ids.final

#-------------------------
#Ceuta
ids.final1$chick1
ind<-which(!is.na(ids.final1$chick1) & nchar(ids.final1$chick1) %in% c(6,5,9))
ind<-which(!nchar(ids.final1$chick1) %in% c(6,5,9))

ids.final[-ind,c("chick1","chick2","chick3")]
ids.final[,c("chick1","chick2","chick3")]
nchar(ids.final$chick1[280]) #nchar of NA = 2

#DOES NOT WORK FOR CEUTA to restrict rings to 6 characters as there are rings with different formats
#MAIO:
nests.incomplete.codes<-ids.final[!is.na(ids.final$chick1) & nchar(ids.final$chick1) != 6 | 
                                    !is.na(ids.final$chick2) &nchar(ids.final$chick2) !=6 | 
                                    !is.na(ids.final$chick3) & nchar(ids.final$chick3) !=6, "nest.id"]
                                    #23 nests in Maio
#Ceuta:
# nests.complete.codes<-ids.final[nchar(ids.final$chick1) %in% c(6,5,9) |
#                                     nchar(ids.final$chick2) %in% c(5,6,9) |
#                                     nchar(ids.final$chick3) %in% c(5,6,9), 
#                                   "nest.id"] #667 nests in Ceuta

#FIGURE OUT WHAT TO DO WITH BROODS FROM CROSS-FOSTERING/solved using BirdRefSoc
#nests that hatched but chicks have incomplete rings or no rings:
# ids.final[!ids.final$nest.id %in% 
#             nests.complete.codes & ids.final$fate %in% "HATCH",c("nest.id","fate","chick1","exp")]
# 
# ids.final[!ids.final$nest.id %in% 
#             nests.complete.codes,c("nest.id","fate","chick1","exp")]

#CEUTA: some don't have juv captured and some were from experimental broods
# nest.id  fate chick1     exp (using BirdRef)
# 9    2006-B-1 HATCH   <NA>       U
# 76  2006-D-14 HATCH   <NA>       U
# 279  2008-D-1 HATCH   <NA>       U
# 293  2009-A-1 HATCH   <NA> ParCare
# 333  2009-D-7 HATCH   <NA> ParCare
# 693  2007-B-5 HATCH   <NA> ParCare
#(using BirdSoc)
# 13     2006-B-1 HATCH   <NA>       U
# 92    2006-D-14 HATCH   <NA>       U
# 337    2008-D-1 HATCH   <NA>       U
# 371  2009-A-203 HATCH   <NA> ParCare
# 377    2009-B-2 HATCH   <NA> ParCare
# 423    2009-E-2 HATCH   <NA> ParCare

ids.final[ids.final$nest.id%in%"2007-S--2",]
cap[cap$nest.id%in%"2009-E-2",]
ids.final[ids.final$chick1 %in% "CA2038",]

#Did not omit any nests from Ceuta

#-----
#MAIO 
#omit.from.list<-c("2014-R-305","2014-S-224")

#MAIO check nests: #chicks from these nests do not have metal rings
# [1] "2007-S--2"  "2012-S--13" "2012-S--15" "2012-S--17" "2012-S--23"
# [6] "2012-S--26" "2012-S--29" "2012-S-110" "2007-S--2"  "2012-S--13"
# [11] "2012-S--15" "2012-S--17" "2012-S--22" "2012-S--23" "2012-S--26"
# [16] "2012-S--29" "2012-S-110" "2012-S-111" 

#Maio: Will try to change code to include these, 16/02/2016
# omit.these<-nests.incomplete.codes[!nests.incomplete.codes %in% omit.from.list ]
# ids.final1<-ids.final[!ids.final$nest.id %in% omit.these,]
# ids.final1$nest.id
#----------------------

#----------------------
#MAIO: omit following nests from 2010 with conflicting information:
# 2010-S--116
# 2010-S--21
# 2010-S-307
# 2010-S-326
# 2010-S-39
# 2010-S-9
# 2010-S-335
# 2010-S-305
# 2010-S-306
# 2010-S-84 *didn't add note in file
# 2010-S--114 *didn't add note in file
# 2012-S-221 *nest was deleted from birdref as it does not exist

#list of nests to omit in Maio: 
omit.2 <- c("2010-S-84","2010-S--114","2010-S--116","2010-S--21", "2010-S-307", "2010-S-326","2010-S-39","2010-S-9","2010-S-335", "2010-S-305","2010-S-306" )
ids.final1[ids.final1$nest.id%in% omit.2,]
ids.final1<-ids.final1[!ids.final1$nest.id %in% omit.2,]

#-----re-name------------------
cap1<-cap #rename so undo can be made by using #cap<-cap1
cap<-cap1
#--------------------
#-------not needed: Maio---------
#NOT NEEDED MAIO: omit second capture of CA2734 and CA3461 in captures file (see debbuging) 
#ind<-which(cap$ring %in% c("CA2734","CA3461") & cap$time %in% c(1900,1637))
#cap1[ind,]
#cap<-cap1[-ind,]
#----------------------

 #chicks with no metal ring will need extra step:
cap[is.na(cap$ring) & !is.na(cap$code),]

cap$ring2 <- ifelse(is.na(cap$ring) & !is.na(cap$code), cap$code, NA)
cap$ring <- ifelse(!is.na(cap$ring2), cap$ring2, cap$ring) #replace NA of chicks without metal ring with code
cap$year.ring <- paste(cap$year, cap$ring, sep="-")
ids.final1$year.chick1 <- paste(ids.final1$year, ids.final1$chick1, sep="-")
ids.final1$year.chick2 <- paste(ids.final1$year, ids.final1$chick2, sep="-")
ids.final1$year.chick3 <- paste(ids.final1$year, ids.final1$chick3, sep="-")

#-----------------------
rownames(ids.final1)<-1:nrow(ids.final1)
#assign empty variable names:

ids.final1$chick1.capdate<- NA
ids.final1$chick1.bill<-NA
ids.final1$chick1.tarsus <-NA
ids.final1$chick1.weight<-NA
ids.final1$chick2.capdate<- NA
ids.final1$chick2.bill<-NA
ids.final1$chick2.tarsus <-NA
ids.final1$chick2.weight<-NA
ids.final1$chick3.capdate<- NA
ids.final1$chick3.bill<-NA
ids.final1$chick3.tarsus <-NA
ids.final1$chick3.weight<-NA
#-------------------

str(cap)

for(i in 1:length(ids.final1$year)){ 
  #for(i in 1:95){ 
  population<-"MAIO"
  print(i) #print out iterations to know when it stops because of an error
  options(warn=1) #show in which iteration the error comes out

  #MAIO:
  #Next section only works in Maio, in Ceuta as broods were swapped this will not work
#   ind <- which(ids.final1$nest.id[i]== cap$nest.id) #in Ceuta this needs to change given experimental broods, search needs to be focused on chicks ring and not on nest
#   group.cap<-cap[ind,]
#   chicks.cap<-group.cap[group.cap$sex %in% "J" & !is.na(group.cap$ring),]  
#   chicks.cap<-chicks.cap[order(chicks.cap$date),]
#   chicks.ids<-unique(ids.final1[i, c("chick1","chick2","chick3")])
  #Ceuta:
    chicks.ids<-unique(ids.final1[i, c("chick1","chick2","chick3")])
    chicks.ids.year<-unique(ids.final1[i, c("year.chick1", "year.chick2","year.chick3")])
  
#for chick1 
  #Maio:     
  #if(!is.na(chicks.ids$chick1) & nchar(chicks.ids$chick1)==6){ #this is to avoid including chicks with no metal e.g. XX.XX|BX.XX  
  #ind <- which(ids.final1$nest.id[i]== cap$nest.id) #in Ceuta this needs to change given experimental broods, search needs to be focused on chicks ring and not on nest
  #group.cap<-cap[ind,]
  
  #Ceuta: ###ALSO used for Maio 2nd run 17/02/2016
  if(!is.na(chicks.ids$chick1)){
    ind<-which(chicks.ids.year$year.chick1 == cap$year.ring)
    chicks.cap<-cap[ind,]
    #check whether nest.id from captures is matching nest.id from ids.final
      if(population %in% "MAIO" & any(ids.final1$nest.id[i]!=chicks.cap$nest.id) & length(unique(chicks.cap$nest.id))!=1){
             ind3<-which(ids.final1$nest.id[i]==chicks.cap$nest.id)
             chicks.cap<-chicks.cap[ind3,]
             }else{
               chicks.cap<-cap[ind,]
             }
             
    #Ceuta and Maio:
  ind2<-grep(chicks.ids$chick1, chicks.cap$ring, fixed=T)
  chick1.measures<-chicks.cap[ind2,]
  chick1.measures<-chick1.measures[chick1.measures$date == min(chick1.measures$date),]#if there are recaptures use only first one
    if(length(chick1.measures$year)==1){
      ids.final1$chick1.capdate[i] <- as.character(chick1.measures$real.date)
      ids.final1$chick1.bill[i] <- chick1.measures$bill
      ids.final1$chick1.tarsus [i] <- ifelse(!is.na(chick1.measures$right_tarsus), chick1.measures$right_tarsus, chick1.measures$left_tarsus)
      ids.final1$chick1.weight [i] <- chick1.measures$weight
    }else{
      ids.final1$chick1.capdate[i] <- as.character(chick1.measures$real.date[1])
      ids.final1$chick1.bill[i] <- chick1.measures$bill[1]
      ids.final1$chick1.tarsus [i] <- ifelse(!is.na(chick1.measures$right_tarsus[1]), chick1.measures$right_tarsus[1], chick1.measures$left_tarsus[1])
      ids.final1$chick1.weight [i] <- chick1.measures$weight[1]
    }
  } else {
  ids.final1$chick1.capdate[i] <- NA
  ids.final1$chick1.bill[i] <- NA
  ids.final1$chick1.tarsus [i] <- NA
  ids.final1$chick1.weight [i] <- NA 
  }
  
  #for chick2
  #MAIO:
  #if(!is.na(chicks.ids$chick2) & nchar(chicks.ids$chick2)==6){
  
  #Ceuta:
  if(!is.na(chicks.ids$chick2)){
    ind<-which(chicks.ids.year$year.chick2 == cap$year.ring)
    chicks.cap<-cap[ind,]
    if(population %in% "MAIO" & any(ids.final1$nest.id[i]!=chicks.cap$nest.id) & length(unique(chicks.cap$nest.id))!=1){
      ind3<-which(ids.final1$nest.id[i]==chicks.cap$nest.id)
      chicks.cap<-chicks.cap[ind3,]
    }else{
      chicks.cap<-cap[ind,]
    }

  #Ceuta and Maio
    ind2<-grep(chicks.ids$chick2, chicks.cap$ring, fixed=T)
    chick2.measures<-chicks.cap[ind2,]
    chick2.measures<-chick2.measures[chick2.measures$date == min(chick2.measures$date),]#if there are recaptures use only first one
    
  
      if(length(chick2.measures$year)==1){
        ids.final1$chick2.capdate[i] <- as.character(chick2.measures$real.date)
        ids.final1$chick2.bill[i] <- chick2.measures$bill
        ids.final1$chick2.tarsus [i] <- ifelse(!is.na(chick2.measures$right_tarsus), chick2.measures$right_tarsus, chick2.measures$left_tarsus)
        ids.final1$chick2.weight [i] <- chick2.measures$weight
        }else{
          ids.final1$chick2.capdate[i] <- as.character(chick2.measures$real.date[2])
          ids.final1$chick2.bill[i] <- chick2.measures$bill[2]
          ids.final1$chick2.tarsus [i] <- ifelse(!is.na(chick2.measures$right_tarsus[2]), chick2.measures$right_tarsus[2], chick2.measures$left_tarsus[2])
          ids.final1$chick2.weight [i] <- chick2.measures$weight[2]
        }  
  } else {
  ids.final1$chick2.capdate[i] <- NA
  ids.final1$chick2.bill[i] <- NA
  ids.final1$chick2.tarsus [i] <- NA
  ids.final1$chick2.weight [i] <- NA
  }
  
  #for chick3
  #MAIO:
  #if(!is.na(chicks.ids$chick3) & nchar(chicks.ids$chick3)==6){
  #Ceuta:
  if(!is.na(chicks.ids$chick3)){
    ind<-which(chicks.ids.year$year.chick3 == cap$year.ring)
    chicks.cap<-cap[ind,]
    if(population %in% "MAIO" & any(ids.final1$nest.id[i]!=chicks.cap$nest.id) & length(unique(chicks.cap$nest.id))!=1){
      ind3<-which(ids.final1$nest.id[i]==chicks.cap$nest.id)
      chicks.cap<-chicks.cap[ind3,]
    }else{
      chicks.cap<-cap[ind,]
    }
  
    #Ceuta and Maio:
  ind2 <- grep(chicks.ids$chick3, chicks.cap$ring, fixed=T)
  chick3.measures <- chicks.cap[ind2,]
  chick3.measures <- chick3.measures[chick3.measures$date == min(chick3.measures$date),]
    if(length(chick3.measures$year)==1){
      ids.final1$chick3.capdate[i] <- as.character(chick3.measures$real.date)
      ids.final1$chick3.bill[i] <- chick3.measures$bill
      ids.final1$chick3.tarsus [i] <- ifelse(!is.na(chick3.measures$right_tarsus), chick3.measures$right_tarsus, chick3.measures$left_tarsus)
      ids.final1$chick3.weight [i] <- chick3.measures$weight
    }else{
      ids.final1$chick3.capdate[i] <- as.character(chick3.measures$real.date[3])
      ids.final1$chick3.bill[i] <- chick3.measures$bill[3]
      ids.final1$chick3.tarsus [i] <- ifelse(!is.na(chick3.measures$right_tarsus[3]), chick3.measures$right_tarsus[3], chick3.measures$left_tarsus[3])
      ids.final1$chick3.weight [i] <- chick3.measures$weight[3]
    } 
  
  } else {
  ids.final1$chick3.capdate[i] <- NA
  ids.final1$chick3.bill[i] <- NA
  ids.final1$chick3.tarsus [i] <- NA
  ids.final1$chick3.weight [i] <- NA
  }
}

#------------debug 1- 25/01/2016 & 26/01/2016-------
head(ids.final1)
tail(ids.final1[ids.final1$fate %in% "HATCH",])
tail(ids.final1)

ind<-sample(rownames(ids.final1),5)
ids.final1[ind,] #MAIO: checked complete info of 5 at random and seems ok 26/01/2016
                #Ceuta: checked complete info of 5 at random and seems ok 12/02/2016
ids.final1[!is.na(ids.final1$chick1),]
ids.final1[4,]

ids.final1[ids.final1$nest.id %in% "2012-S--31",]

ids.final1[nchar(ids.final1$year.chick1)>11,] #checked all of these for Maio and they were ok

tail(cap)
#------------------
cap[cap$nest.id %in% "2007-S--2",]
#--MAIO: iterations that run with error: 731,611,198 (on second run),95

#95 "2009-S-201" -> one chick was captured twice on the same day...omit second capture (line 651)
ids.final1[ids.final1$nest.id %in% "2009-S-201",]
cap[cap$nest.id %in% "2009-S-201",]

#198 "2010-S-32" and "2010-S-326": grep is summoning 32 and 326...*code bug
#fixed: use which instead of grep...change rest of coding

#611 "2009-S-201" ,/

#731 "2010-S-32" ,/

#198 second run after debugging: CA3461 has two captures on the same day. Omit second capture (line 651)

#---------------------------------------
#MAIO DEBUG 16/02/2016 for loop chicks (changes made), errors on iterations:
# [1] 245
# [1] "nest.id mis-matches captures nest.id"
# [1] 246
# [1] 418
# Warning in min(chick2.measures$date) :
#   no non-missing arguments to min; returning Inf
# [1] 419
# [1] 558
# [1] "nest.id mis-matches captures nest.id"
# [1] 559
# [1] 776
# [1] "nest.id mis-matches captures nest.id"
# [1] 777
# [1] 797
# [1] "nest.id mis-matches captures nest.id"
# [1] 798
# [1] 939
# Warning in min(chick1.measures$date) :
#   no non-missing arguments to min; returning Inf
# [1] 940
# [1] 989
# Warning in min(chick2.measures$date) :
#   no non-missing arguments to min; returning Inf
# [1] 990
# [1] 45
# [1] "nest.id mis-matches captures nest.id"
# [1] 46

ids.final1[418,] #case 418, chick2 has XX.XX|XX.XX -> Inf is correct

ids.final1[939,] #chick1 is XX.XX|XX.XX...so min(chick1.measures$date) is Inf
ids.final1[989,] #chick2 has XX.XX|XX.XX -> Inf is correct

ids.final1[245,]
cap[cap$year.ring %in% "2012-LX.XX|GX.XX",] #two chicks with same ring in two different nests

ids.final1[558,] #same as 45
cap[cap$year.ring %in% "2008-CA1192",]#std_notes says: recapture, (this ring or nest number is wrong, original notes need to be checked CCI 14/05/2015)
#tarsus length and measurements are from first capture which is fine ,/

ids.final1[776,] #same as 245
cap[cap$year.ring %in% "2012-LX.XX|GX.XX",]

ids.final1[797,] #same as 245
cap[cap$year.ring %in% "2012-LX.XX|GX.XX",]

ids.final1[45,] #same as 558
cap[cap$year.ring %in% "2008-CA1192",]#std_notes says: recapture, (this ring or nest number is wrong, original notes need to be checked CCI 14/05/2015)
#tarsus length and measurements are from first capture which is fine ,/
#fixed code 17/02/2016

#---------27/01/2016
#b. Calculate oldest chick's age with size, oldest chick is not necessarily chick1
#ids.final<- ids.final1
library(fBasics)
ids.final2<-ids.final1 #if undo is needed

names(ids.final2)
str(ids.final1)

#MAIO:
head(rowSums(ids.final2[,c(65,69,73)], na.rm=T))
#Ceuta:head(rowSums(ids.final2[,c(67,71,75)], na.rm=T))

# i) extract largest chick.tarsus and capture date
ids.final2$largest.chick.tarsus <- ifelse(rowSums(ids.final2[,c("chick1.tarsus","chick2.tarsus","chick3.tarsus")], na.rm=T)>0,
                                          apply(ids.final2[,c("chick1.tarsus","chick2.tarsus","chick3.tarsus")],
                                                1, max, na.rm=T), NA)
ids.final2$largest.chick.capture<-NA

for(i in 1:length(ids.final2$year)){
 #ind<-arrayInd(which.max(ids.final2[51,c("chick1.tarsus","chick2.tarsus","chick3.tarsus")]), dim(ids.final2[50,c("chick1.tarsus","chick2.tarsus","chick3.tarsus")])) 
 ind<-which.max(ids.final2[i,c("chick1.tarsus","chick2.tarsus","chick3.tarsus")])
 names(ind)
 print(i)
 if(length(ind) ==0){
   ids.final2$largest.chick.capture[i] <- NA
 } else{
   if(names(ind) == "chick1.tarsus"){
     ids.final2$largest.chick.capture[i] <- ids.final2$chick1.capdate [i]
   } else{
     if(names(ind) == "chick2.tarsus"){
       ids.final2$largest.chick.capture[i] <- ids.final2$chick2.capdate [i]
     } else {
       if(names(ind) == "chick3.tarsus"){
         ids.final2$largest.chick.capture [i] <- ids.final2$chick3.capdate [i]
       }
     }
     }
 }
}




# ii) calculate chicks age using formula from plover fieldwork guide
#this needs to change, use numbers specific to each population from Natalie's paper

 #General formula: ids.final2$largest.chick.age <- (2.52 * ids.final2$largest.chick.tarsus) - 48.341
#see formula and Table 2 in (http://onlinelibrary.wiley.com/doi/10.1111/ibi.12263/full)

#MAIO formula: 
ids.final2$largest.chick.age <- (ids.final2$largest.chick.tarsus-19.16)/0.45

#Ceuta formula:
#ids.final2$largest.chick.age <- (ids.final2$largest.chick.tarsus-17.607)/0.309

#------------debug 2-------------
#(i)
names(ids.final2)
head(ids.final2)
ind<-sample(rownames(ids.final2),5)
ids.final2[ind, c(63,65,67,69,71,73,75,76,77)] #checked Ceuta 27/01/2016
                                              #checked Maio 17/02/2016

ids.final2[which(ids.final2$chick1.capdate != ids.final2$chick2.capdate),  c(63,65,67,69,71,73,75,76,77)]

#(check that when two chicks had same size the capturedates were similar)
ids.final2[which(ids.final2$largest.chick.tarsus == ids.final2$chick1.tarsus & ids.final2$largest.chick.tarsus == ids.final2$chick2.tarsus), c(63,65,67,69,71,73,75,76,77)]
ids.final2[which(ids.final2$largest.chick.tarsus == ids.final2$chick1.tarsus & ids.final2$largest.chick.tarsus == ids.final2$chick3.tarsus),  c(63,65,67,69,71,73,75,76,77,18)]
ids.final2[which(ids.final2$largest.chick.tarsus == ids.final2$chick2.tarsus & ids.final2$largest.chick.tarsus == ids.final2$chick3.tarsus),  c(63,65,67,69,71,73,75,76,77,18)]
    #capture dates are similar in all: +-1day

#(ii)
ids.final2[ind, c(37,41,45,47,48)] 
fivenum(ids.final2$largest.chick.age)
fivenum(ids.final2$chick1.tarsus)

ids.final2[ids.final2$largest.chick.age >18 & !is.na(ids.final2$largest.chick.age),]
ids.final2[ids.final2$largest.chick.age < -1.5 & !is.na(ids.final2$largest.chick.age),]
#chick from 2012-S--4 has tarsus=14.1, it's age is -11, check chicks with similar weight and bill <- changed tarsus to 19.1 based on captures from other chicks with similar weights and bills
#change now in loaded file, but csv has been modified:
#ids.final2[ids.final2$largest.chick.tarsus %in% 14.1,"largest.chick.tarsus"]<-19.1

cap[cap$weight %in% 6,]

ids.final2[ids.final2$chick1.tarsus <18 & !is.na(ids.final2$largest.chick.age),]

ids.final2[ids.final2$nest < 0, c("largest.chick.age","laying_date")]
ids.final2[ids.final2$nest<0 & is.na(ids.final2$largest.chick.age),]

#-----------------------------


#c. Estimate LD

#i) estimate LD
ids.final3<-ids.final2
str(ids.final3)
#ids.final3$chick1.capdate<-as.Date(ids.final3$chick1.capdate, "%Y-%m-%d")
ids.final3$largest.chick.capdate<-as.Date(ids.final3$largest.chick.capture, "%Y-%m-%d")

#ids.final3$estimated.ld <- (ids.final3$chick1.capdate - ids.final3$largest.chick.age)-25 #needs to use largest chick capdate

ids.final3$estimated.ld <- ifelse(ids.final3$largest.chick.age<0, ids.final3$largest.chick.capdate-25, (ids.final3$largest.chick.capdate - ids.final3$largest.chick.age)-25)
ids.final3$estimated.ld <- as.Date(ids.final3$estimated.ld, origin=("1970-01-01"), "%Y-%m-%d")

ids.final3$estimated.hd <- ifelse(ids.final3$largest.chick.age<0, ids.final3$largest.chick.capdate, ids.final3$largest.chick.capdate - ids.final3$largest.chick.age)
ids.final3$estimated.hd <- as.Date(ids.final3$estimated.hd, origin=("1970-01-01"), "%Y-%m-%d")

#ii) merge original laying date of non-neg nests with estimated.ld of neg nests:
ids.final3$layingdate <- ifelse(!is.na(ids.final3$laying_date.r), as.character(ids.final3$laying_date.r), as.character(ids.final3$estimated.ld))


#-----------debug 3----------
ids.final3$laying_date.r<-as.Date(ids.final3$laying_date.r, "%Y-%m-%d")

#check nests with laying date available (non-neg nests)
ids.final3$check.estld<-ids.final3$estimated.ld - ids.final3$laying_date.r

fivenum(ids.final3$check.estld)

#MAIO:
# With chick1 capdate:
#> fivenum(check.estld)
# Time differences in days
# [1] -34.779  -5.795  -3.039   0.165  12.225

#with largest chick capdate:
# > fivenum(ids.final3$check.estld)
# Time differences in days
#[1] -30.3111111  -5.7555556  -2.3111111   0.5222222  10.3555556

#CEUTA:
# > fivenum(ids.final3$check.estld) #with largest chick capdate
# Time differences in days
# [1] -20.9773463  -1.6537217  -0.2718447   1.6407767  37.1715210

ind<-which(ids.final3$check.estld<(-5))
ids.final3[ind,]

head(ids.final3)
#--------------------------------

#---c. Order and extract first, last and LD----------------------[reordered 05/02/2016]
tail(ids.final)
str(ids.final)

ids.final<-ids.final3

#turn to dates so that max and min will work:
str(ids.final)
names(ids.final)
trial<-as.Date(ids.final$last.bf.r)

ids.final$first.res<-as.Date(ids.final$first.res, "%Y-%m-%d")
ids.final$last.res<-as.Date(ids.final$last.res,"%Y-%m-%d")
#ids.final$first.bf.r<-as.Date(ids.final$first.bf.r, "%Y-%m-%d")
#ids.final$last.bf.r<-as.Date(ids.final$last.bf.r, "%Y-%m-%d")
#ids.final$last.bf.r<-as.Date(ids.final$last.bf.r, "%Y-%m-%d")

#earliest date seen (not LD) using: "first.res", "last.res","first.cap","last.cap","first.bf.r", "last.bf.r" 
ids.final$first.d.seen <- apply(ids.final[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) min(x, na.rm=T))
head(ids.final)

ids.final$last.d.seen <- apply(ids.final[,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")],1,function (x) max(x, na.rm=T))


#--------debug c.--------
str(ids.final[55,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")], na.rm=T)

ids.final[200,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.r", "last.bf.r","last.d.seen")]
ids.final[200,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.r", "last.bf.r","first.d.seen")]

max(ids.final[55,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")], na.rm=T)
str(ids.final[55,c("first.res", "last.res","first.cap.r","last.cap.r","first.bf.adult.r", "last.bf.adult.r")])

#------------------------

#----If first is = LD or < LD-10 consider using LD-10?----
str(ids.final) #MAIO: 1083 obs #CEUTA: 1031 obs

ids.final$layingdate <- as.Date(ids.final$layingdate, "%Y-%m-%d" )

ind<- which(ids.final$first.d.seen < ids.final$laying_date.r)
ind<- which(ids.final$first.d.seen < ids.final$layingdate)
str(ids.final[ind,]) #MAIO: 207 or 283 using est.ld cases where individuals were seen before their laying date
                    #CEUTA: 364 cases where individuals were seen before their laying date

ind<- which(ids.final$first.d.seen < (ids.final$laying_date.r-10))
str(ids.final[ind,]) #CEUTA: 326 cases
                      #MAIO: 146 cases

ind2<- which(ids.final$first.d.seen > ids.final$laying_date.r)
str(ids.final[ind2,]) #MAIO: 473 cases where laying date is previous to any other sighting
                      #CEUTA: 617 cases 
x <- ids.final[ind2,]

#MAIO:
#from those where laying date is previous to any other sighting, is found date after laying date?
 ind3<-which(x$found_date.r < x$laying_date.r)  #MAIO: 49 cases where laying date is after found date
 str(x[ind3,])                                  #CEUTA: 107 cases
 x1<-x[ind3,]
#from these 49 cases, how many days before LD is founddate?
a<-as.Date(x1$laying_date.r) - as.Date(x1$found_date.r)
table(a)  #MAIO: 1-6 days before LD
          #CEUTA: 1-7 days before LD
# 
 ind4<-which(x$found_date.r>x$laying_date.r)  #MAIO: 411 cases where laying date is before found date
 str(x[ind4,]) #-1--25 days after LD          #CEUTA: 485 cases

ind3<-which(ids.final$found_date.r< ids.final$laying_date.r)
ids.final[ind3, c("found_date.r","laying_date.r")]

#####It makes no difference to use LD or found date on cases where founddate was earlier than laying date.
#In MAIO, in most cases LD is the first date an individual was seen
#In CEUTA, first.d.seen is earlier than LD-10 in 326 cases.


#--------------------------
###error found: ---------------
#ids.final[ids.final$nest.id %in% "2015-S-11", "chick1"] <- "CA3948" #it had an extra space, corrected in std files as well ,/


#_-------------------------
#ADD mol_sex to Ceuta data (From captures file cleaned by Luke with mol.sex)
#using cap.1 containing only Adults

for (i in 1:length(ids.final$year)){
  ids.final$mol_sex_focal[i] <- cap.1$sex[match(ids.final$ring[i], cap.1$ring)]
  ids.final$mol_sex_mate[i] <- cap.1$sex[match(ids.final$mate_ring[i], cap.1$ring)]
}

#-------------------non matching sexes? field-mol------------

ids.final[ids.final$mol_sex_focal != ids.final$field_sex_focal & !is.na(ids.final$year), c("nest.id","field_sex_focal","field_sex_mate","mol_sex_focal","mol_sex_mate")]
unique(ids.final[ids.final$mol_sex_focal != ids.final$field_sex_focal & !is.na(ids.final$year), "nest.id"])

ids.final[is.na(ids.final$mol_sex_focal), c("nest.id","field_sex_focal","field_sex_mate","mol_sex_focal","mol_sex_mate")]

#---------debug------
ids.final[ids.final$nest.id %in% "2006-C-38",]
ids.final[ids.final$code %in% "BX.WX|BX.OX",]
cap[cap$code %in% "BX.WX|BX.OX",] #adult found with X in age when it should be A....correct

cap[cap$age %in% "X",]
table(cap$age)
#----------------------------------
#choose variables to keep to save csv file:
names(ids.final)
#unique(ids.final3$fate)
#only omit working variables like year.ring, year.chick1, etc.
to.write<-ids.final[,c(1:20,22,23,27:28,32:39,43,44,51:59,63:84)]
#to.write<-ids.final3[,c(1:23,49,25:31)]





# #--------------check potential errors-------------
# #check cases where number of rings does not correspond to number of chicks hatched
# a<-ids.final3[ids.final3$fate =="HATCH", c(15, 5:7,27)]
# a$totalchicks <- apply(a[,2:4],1,function(x) length(which(!is.na(x))))
# 
# a[a$no_chicks != a$totalchicks & !is.na(a$nest.id),]
# 
# #no corrections...probably differences come from chicks that couldn't be trapped

#----------------------------------------------------
#-----------------------------------------------
#-----------------------------------------------
#------------Write file with data (excluding laying dates for negative broods)---------------
setwd("F:/Plovers/3rd Chapter/output/Maio")
write.csv(to.write, "Maio_breeding_schedule_data_2007-2015_17Feb2016.csv")

setwd("F:/Plovers/3rd Chapter/output/Ceuta")
write.csv(to.write, "Ceuta_breeding_schedule_data_2007-2012_full dataset_12Feb2016with_molsex.csv")
#-----------------------------------------------
#-----------------------------------------------
