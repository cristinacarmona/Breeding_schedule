#05/05/2015 #Code to generate std Bird Ref developed using Maio

#1st log 05/05/2015
#2nd log 12/05/2015 up to line 325, finished replacing codes with rings
#3rd log 13/05/2015 added from line 412
#4th log 21/05/2015 rerun first part of code to check and add additional comments (but did not create new pre-std file for Bird_Ref)
                  #continued coding
#5th log 30/05/2015 re-take coding line 423 to change code/ring of uncaptured individuals
#6th log 01/06/2015 re-take line 423
#7th log 02/06/2015 run correcting mistakes code (part III) and check which errors remain
#8th log 03/06/2015 corrected 27 cases of rings missing from BirdRef
#9th log 04/06/2015 unstacked rows and created stdfile
#10th log 04/06/2015 fixed bug that deleted one nest with two females...2011-S-4 fixed bug ,/

#04/08/2015 No changes in code...but changes in file, added nest 2011-S-4 to ring CA3606 which was missing,
#           and changed males captured in the field from yes to no from juveniles captures with unknown nests
#           and added comment to ring CA2929 from bs--2 "CA2929 is not in captures but appears as a female in BirdRef nest 2014-bs--2. Alex didn't record this capture in the captures file nor in the ringing sheet (found 04/08/2015)"

##10/08/2015 found inconsistent mol_sex in CA3037...bug in BirdRef Cleaning code look-up bug

###14/01/2016 add data of 2015
###21/01/2016 finished checking data from 2015 ,/

#-------------------------------------------------
#Apply code to Madagascar
#23/03/2016 1st log start applying code to Mad up to line 103

#25/03/2016 3rd log up to line 571, found missing/wrong rings in birdref

#29/03/2016 Re-made bird ref using captures and ids from adults which were not captured (taken from original Birdref file)
#01/04/2016 5th log: Found error in "Captured in focal year" variable, when re-structuring the data the code is doing something wrong?
#--------------------------------------------------
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
names(working.list) <- c("bf2","brstd","br","bf","cap1","cap","sex","ne","re")
attach(working.list)
#--------------------------------------------------------------------------------
#I Preparation of data consistency and codes

#Check codes which are not rings
bre<-br[br$site %in% "Andavadoaka",]

table(bre$year)
head(bre)
tail(bre)
#bre$year[bre$year %in% "20014"] <- "2014" #correct wrong year 20014


#clean code rings stack males and females so code cleaning is done once
#(males and females will be in one same column and a column sex will be added):

names(bre)
# bre.m <- bre[,c(1:4, 6:9,11,13,15:16)] #choose only males
# colnames(bre.m)[c(4,8,9,10)]<-c("code","field_sex","mol_sex","captured_focalyear")
# bre.m$sex <- "M"
# 
# bre.f <- bre[,c(1:3,5:8,10,12,14, 15:16)] #choose only females
# colnames(bre.f)[c(4,8,9,10)]<-c("code","field_sex","mol_sex","captured_focalyear")
# bre.f$sex <- "F"

bre.1 <- bre[,c(1:6, 8:13)] #choose only males
colnames(bre.1)[c(6,11)] <- c("parent1or2","species")

bre.2 <- bre[,c(1:5,7:13)] #choose only females
colnames(bre.2)[c(6,11)] <- c("parent1or2","species")


#correct cases where field_sex is NA
# bre.stacked[bre.stacked$sex != bre.stacked$field_sex,]
# bre.stacked[c(888:1021), "field_sex"] <- "M"
###############

#bre.stacked <- as.data.frame(rbind(bre.m,  bre.f))

bre.stacked <- as.data.frame(rbind(bre.1,  bre.2))

head(bre.stacked)

tail(bre.stacked)

#check code and rings
#more.11chr <- bre.stacked[nchar(bre.stacked$code)!=6, c("year","site","sex","nest","code")] 

more.11chr <- bre.stacked[nchar(bre.stacked$parent1or2)!=7, c("year","site","nest","parent1or2")] 
str(more.11chr) #762 might not be rings; MAD 1629

#no.ring <- more.11chr[order(more.11chr$code)& !is.na(more.11chr$code),]
no.ring <- more.11chr[order(more.11chr$parent1or2)& !is.na(more.11chr$parent1or2),]
str(no.ring) #190 with code but no metal ring available, the rest are NA

#bre.stacked$code[!bre.stacked$code %in% more.11chr$code] #correct rings = 1189
bre.stacked$parent1or2[!bre.stacked$parent1or2 %in% more.11chr$parent1or2] #correct rings = 2147

#no.ring$code
no.ring$parent1or2
#----------------------change "unringed" to XX.XX|XX.XX and "unknown" to NA
#ind <- grep(pattern = "[Uu]nknown", bre.stacked$code, perl=TRUE)
ind <- grep(pattern = "UNK", bre.stacked$parent1or2, perl=TRUE)
bre.stacked[ind,]
#bre.stacked$code[ind] = NA
bre.stacked$parent1or2[ind] = NA

# ind <- grep(pattern = "[Uu]nringed", bre.stacked$code, perl=TRUE)
ind <- grep(pattern = "UR", bre.stacked$parent1or2, perl=TRUE)
bre.stacked[ind,]
# bre.stacked$code[ind] = "XX.XX|XX.XX"
bre.stacked$parent1or2[ind] = "X.X|X.X"
#-----------------------------------------------------------------------------------------------------


#Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$" #Allows Xs, therefore includes ambiguous codes
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$" #does not allows Xs, only complete codes
regexp1 <- "([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})\\|([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})$"

ind.correct<-grep(pattern = regexp1, no.ring$parent1or2, perl=TRUE)
correct.code <- no.ring[ind.correct,] #CORRECT OBS: 189 obs with correct and ambiguous codes, 92 obs with non-ambiguous codes
str(correct.code) #140
correct.code$parent1or2 #60 Mad

incorrect.code <- no.ring[-ind.correct,] 
str(incorrect.code) #with regexp1: 1 with symbols; with regexp2: 98 ambiguous codes, 3636 MAD in bre.stacked; 127 in no.ring (most UNK and UR)
head(incorrect.code)
unique(incorrect.code$parent1or2)

#Fix codes in wrong format e.g. WRML
fix.code.p1<-incorrect.code[nchar(incorrect.code$parent1or2)==4,"parent1or2"]

for(i in 1:length(bre.stacked$year)){ #for parent 1
  if(bre.stacked$parent1or2[i] %in% fix.code.p1){
    bre.stacked$new.code.p1or2[i] <- paste(substr(bre.stacked$parent1or2[i], 1,1),".", 
                                substr(bre.stacked$parent1or2[i], 2,2), "|",
                                substr(bre.stacked$parent1or2[i], 3,3), ".",
                                substr(bre.stacked$parent1or2[i], 4,4), sep="")
  }else{
    bre.stacked$new.code.p1or2[i] <- bre.stacked$parent1or2[i]
  }
}


# #Regular expressions for codes with g
# regexp <- "gX|g\\.|g$" #finds g in codes
# ind.g<- grep(pattern = regexp, incorrect.code$code, perl=TRUE)
# 
# gs<-incorrect.code[ind.g,]

#change g to L in code--------------------------------------------------------------
#bre.stacked[bre.stacked$code %in% gs$code,]
#bre.stacked$code[bre.stacked$code %in% gs$code] <- gsub("g", "L", bre.stacked$code[bre.stacked$code %in% gs$code])

#bre.stacked[c(53,888,891,925),]

#change g to L in chick1, chick2 and chick3
#regexp <- "gX|g\\.|g$" #finds g in codes
#ind.g<- grep(pattern = regexp, bre.stacked$chick1, perl=TRUE) #only found g's in chick1
#bre.stacked[ind.g,]

#ind.2g<-grep(pattern=regexp, bre$chick1, perl=T)
#bre[ind.2g,]

#bre.stacked$chick1 <- gsub("g","L", bre.stacked$chick1) #only found g's in chick1

#---------------------------------------------------------------------------------------
#Check again after changes:
more.11chr <- bre.stacked[nchar(bre.stacked$new.code.p1or2)!=7, c("year","site","nest","new.code.p1or2")] 
str(more.11chr) #762 might not be rings; MAD 1629; MAD after corrections: 1598

#no.ring <- more.11chr[order(more.11chr$code)& !is.na(more.11chr$code),]
no.ring <- more.11chr[order(more.11chr$new.code.p1or2)& !is.na(more.11chr$new.code.p1or2),]
str(no.ring) #84

#bre.stacked$code[!bre.stacked$code %in% more.11chr$code] #correct rings = 1189
bre.stacked$new.code.p1or2[!bre.stacked$new.code.p1or2 %in% more.11chr$new.code.p1or2] #correct rings = 2147, after MAD corrections: 2178 (includes codes with 7 characters)

#no.ring$code
no.ring$new.code.p1or2
# [1] "YY.X|X.WY" "YY.X|X.WG" "YY.X|X.WO" "YY.X|X.WY" "YY.X|X.WR" "YY.X|X.WW" "YY.X|X.RL"
# [8] "YY.X|X.RW" "YY.X|X.RR" "YY.X|X.RO" "YY.X|X.RG" "YY.X|X.RB" "LL.X|X.OW" "LL.X|X.OR"
# [15] "LL.X|X.OG" "LL.X|X.OB" "GG.X|X.BY" "GG.X|X.BW" "GG.X|X.BR" "GG.X|X.BG" "GG.X|X.BB"
# [22] "GG.X|X.BL" "LL.X|X.GY" "LL.X|X.GW" "LL.X|X.GR" "LL.X|X.GO" "LL.X|X.GL" "GG.X|X.GG"
# [29] "LL.X|X.LY" "LL.X|X.LW" "LL.X|X.LR" "LL.X|X.LO" "LL.X|X.LB" "LL.X|X.LG" "LL.X|X.LL"
# [36] "Luke?"     "GG.X|X.BO" "YY.X|X.RY" "YY.X|X.WK" "YY.X|YY.X" "LL.X|X.LY" "LL.X|X.LG"
# [43] "LL.X|X.LL" "LL.X|X.GL" "YY.X|BG.X" "YY.X|YB.X" "YY.X|X.YR" "YY.X|YY.X" "YY.X|YL.X"
# [50] "YY.X|YL.X" "YY.X|BY.X" "YY.X|BB.X" "?"         "?"         "LL.X|X.LW" "?"        
# [57] "ub"        "Luke?"     "Luke?"     "F73085"    "LL.X|X.OO" "LL.X|X.GG" "LL.X|X.OL"
# [64] "LL.X|X.LW" "LL.X|X.LB" "YY.X|BO.X" "YY.X|YG.X" "YY.X|X.YW" "YY.X|YO.X" "?"        
# [71] "?"         "?"         "?"         "?"         "?"         "?"         "?"        
# [78] "?"         "?"         "?"         "?"         "?"         "?"         "?" 

#Changes----------------------------------
#? to NA
bre.stacked$parent1or2[bre.stacked$parent1or2 %in% "?"] <- NA

#Luke? to info Luke sent in his email for nest: 2015-KiP-104
rownames(bre.stacked)<-1:length(bre.stacked$year)
table(bre.stacked$species)
bre.stacked$species[bre.stacked$species %in% "KIP"] <- "KiP"
bre.stacked$species[bre.stacked$species %in% "WFP"] <- "WfP"
bre.stacked$species[bre.stacked$species %in% "WP"] <- "WfP"
bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$species, bre.stacked$nest, sep="-")

bre.stacked[bre.stacked$parent1or2 %in% "Luke?",]
bre.stacked[bre.stacked$nest.id %in% "2015-KiP-104",]

bre.stacked$comments_stdfile <- NA
bre.stacked[1255,"parent1or2"] <- "FH69063"
bre.stacked[1255, "comments_stdfile"] <- "Added missing parents with info in Luke's email (CCI march 2016)"
bre.stacked[3143,"parent1or2"] <- "FH72227"
bre.stacked[3143, "comments_stdfile"] <- "Added missing parents with info in Luke's email (CCI march 2016)"

#incorrect ring F73085:
bre.stacked[bre.stacked$parent1or2 %in% "F73085",]
bre.stacked[bre.stacked$parent1or2 %in% "F73085","parent1or2"] <- "FH73085"
bre.stacked[3200,]

#incorrect ub: [assumed it means unringed...ur]
bre.stacked[bre.stacked$parent1or2 %in% "ub",]
bre.stacked[bre.stacked$parent1or2 %in% "ub", "parent1or2"]<-"X.X|X.X"

#get rid of ?
bre.stacked[bre.stacked$new.code.p1or2 %in% "?",] 
bre.stacked[bre.stacked$new.code.p1or2 %in% "?", "new.code.p1or2"] <- NA
bre.stacked[bre.stacked$code.p1or2 %in% "?", "code.p1or2"] <- NA

#---------------------------------------------------------------------
#Put rings under ring.p1or2 and codes under code.p1or2
#a) Lookup individuals with no rings and only code in parent1 and parent2------------
letters<-"^[FH0-9]*$"
ind.rings<-grep(pattern=letters, bre.stacked$parent1or2, perl=T)

rings.bre<-bre.stacked[ind.rings,"parent1or2"]
bre.stacked[-ind.rings, "parent1or2"]


#b) Replace code with ring only on those where no ring was present already in parent1 and parent2---------
names(bre.stacked)
bre.stacked2<-bre.stacked
bre.stacked<-bre.stacked2


bre.stacked$ring.p1or2 <- NA
bre.stacked[bre.stacked$parent1or2 %in% rings.bre,"parent1or2"]
bre.stacked[bre.stacked$parent1or2 %in% rings.bre,"ring.p1or2"] <- bre.stacked[bre.stacked$parent1or2 %in% rings.bre,"parent1or2"]

bre.stacked$code.p1or2 <- NA
bre.stacked[!bre.stacked$parent1or2 %in% rings.bre, "parent1or2"]
bre.stacked[!bre.stacked$parent1or2 %in% rings.bre, "code.p1or2"] <- bre.stacked[!bre.stacked$parent1or2 %in% rings.bre, "new.code.p1or2"]

#----------------debug---------------
bre.stacked[!is.na(bre.stacked$parent1or2),c(6,17)]
bre.stacked[nchar(bre.stacked$parent1or2) %in% 4,]
#----------------------------------------------------------

#---------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

#II. Check errors

# table(bre.stacked$year)
# #bre.stacked$year[bre.stacked$year %in% "20014"] <- "2014" #correct wrong year 20014

#--------------------------------------------------------------------------------------------
#Match codes with rings
#a) find duplicates in Mad:
table(cap$age)
cap$age[cap$age %in% "C"] <- "J"
cap$code<-gsub(" ","",cap$code)
cap$ring<-gsub(" ","",cap$ring)

cap.dup<-cap[cap$site %in% "Andavadoaka" & !cap$age %in% "J",]

table(cap$year)
table(cap.dup$species)


cap.dup$sp.code.ring <- paste(cap.dup$species,"-", cap.dup$code,"-", cap.dup$ring, sep="")
cap.dup$sp.sex.code <- paste(cap.dup$species,"_", cap.dup$code, "-",cap.dup$sex, sep="")

unique.code.ring<- unique(cap.dup$sp.code.ring) #3017 unique sp.code.ring; 3233 after update

lookdup <-strsplit(unique.code.ring, "-")

library(plyr)
ldup <- ldply(lookdup) #turn list into two columns
#colnames(ldup) <- c("code","ring")
colnames(ldup) <- c("sp","code","ring")  

#-------debug-----
head(ldup[order(ldup$ring),], n=1300)
#----------------

#Get rid of ambiguous codes to delete duplicates:
pat0<-"X"
library(stringr)
x0 <- ldup[!str_detect(ldup$code, pattern=pat0),] #allow no Xs in codes (MAD)

str(x0) #1787; 1911 after update; 1676 after omitting Juveniles
x0[order(x0$code),]
x0$sp.code <- paste(x0$sp, x0$code, sep="-")

ind <- which(duplicated(x0$sp.code) | duplicated(x0$sp.code, fromLast=TRUE))
str(x0[ind,]) #MAIO 34; 38; 36; MAD: 308 (several with no code); 168 after omitting Juveniles

x1 <- x0[ind,]
x1[order(x1$code),]

dupl <- x1 #List of rings with duplicated codes 
str(dupl) #36 Ceuta, 308 in Mad including Juv; 168 after omitting Juveniles

rings.br <- bre.stacked$ring.p1or2

ind <- which(rings.br %in% dupl$sp_code | rings.br %in% dupl$ring)
omit1 <- rings.br[ind] #list of rings to omit MAD: 199 individuals need to be omitted

rings.br[-ind]#Mad: 2008 individuals will be included; 3576 after update (include NAs)
#replace codes with rings in bird reference ONLY for those unambiguous codes which are NOT duplicates
#to avoid errors of misidentifying individuals we can only assign rings to codes that are unambiguous (complete codes with no Xs) and 
#those that are not duplicated

#--------------------------------------------
#b) Match codes from captures---------------
#MAD:
names(cap.dup)
names(bre.stacked)
unique(cap.dup$code)
#pat<-"XX"
pat<- "X\\.X\\|M\\.X"#get rid of ambiguous codes from list of adult captures
pat2<-"X\\.X\\|X\\.M"

cap.dup[grep(cap.dup$code, pattern=pat),c("year","ring","code")]
cap.2<-cap.dup[-grep(cap.dup$code, pattern=c(pat)),] 
unique(cap.2$code)

cap.2[grep(cap.2$code, pattern=c(pat2)),]
cap.1<-cap.2[-grep(cap.2$code, pattern=c(pat2)),]
cap.1<-cap.1[!cap.1$ring %in% omit1,]
#List codes where ring can be looked up - its code.p1or2 
unique(bre.stacked$code.p1or2)

#------rings.inconsistent.codes--------------------------------------
#some rings-codes appear more than once, if codes changed for some adults the previous for will have errors
#create list of unique ring.codes to know which codes are good to use
names(cap.1)
cap.1$ring.code <- paste(cap.1$ring, cap.1$code,sep="-")
freq.codes <- as.data.frame(table(cap.1$ring.code))
head(freq.codes)

unique.ring.code<-unique(cap.1$ring.code)
lookdup <-strsplit(unique.ring.code, "-")
library(plyr)
ldup <- ldply(lookdup) #turn list into two columns
#colnames(ldup) <- c("code","ring")
colnames(ldup) <- c("ring","code")  
head(ldup)
freq.ring <- as.data.frame(table(ldup$ring))
head(freq.ring)
rings.times<-freq.ring[freq.ring$Freq >1, "Var1"] #rings appearing more than once in captures

multiple.captures<-cap.1[cap.1$ring %in% rings.times,]
multiple.captures[order(multiple.captures$ring),]
multiple.captures[order(multiple.captures$ring) & multiple.captures$year >2012,] 
rings.inconsistent.codes<-multiple.captures[order(multiple.captures$ring),"ring"] #76

#------------------------
names(bre.stacked)
bre.stacked2<-bre.stacked
bre.stacked<-bre.stacked2

table(bre.stacked$code.p1or2, useNA ="always") #3630 NAs before for loop
bre.stacked$code <- NA

for(i in 1:length(bre.stacked$year)){ #if code is empty, search ring in capt and fill it
  print(i)
  options(warn=1)
  if(is.na(bre.stacked$code.p1or2[i]) & !is.na(bre.stacked$ring.p1or2[i]) & !bre.stacked$ring.p1or2[i] %in% rings.inconsistent.codes){
    ind<-which(cap.1$ring %in% bre.stacked$ring.p1or2[i])
    add.cap<-cap.1[ind,]
    if(length(ind) ==1){
      bre.stacked$code[i] <- add.cap$code
      }
  }else{
    if(bre.stacked$ring.p1or2[i] %in% rings.inconsistent.codes){
      bre.stacked$code[i] <- "inconsistent codes"
    }
  }
}

table(bre.stacked$code, useNA="always") #NAs2339 after for loop

#put codes in code.p1or2 in code from the ones that have no ring
ind<-which(!is.na(bre.stacked$code.p1or2) & is.na(bre.stacked$code))
bre.stacked[ind,] 
bre.stacked[ind,"code"] <- bre.stacked[ind,"code.p1or2"]

#----------debug/develop---------
cap[cap$ring %in% "FH72139", ]
cap.1[cap.1$ring %in% "FH72139", ]
 

#trial
ind<-which(cap.1$ring %in% bre.stacked$ring.p1or2[bre.stacked$ring.p1or2%in%"FH47123"])
cap.1[ind,]

bre.stacked[bre.stacked$ring.p1or2 %in% "FH47123",] #1846 idrow
bre.stacked[3682,]
str(bre.stacked[bre.stacked$code %in% "inconsistent codes",]) #77 codes with multiple captures and inconsistent codes, flag the ones where ring changed

#------------------------Add rings to codes with no ring
# for(i in 1:length(ids$year)){ #if ring has a code, search ring using code and replace#NOT IN MAD
#   if(nchar(ids$ring[i])>9)
#     ids$ring[i] <- cap.1$ring[match(ids$code[i], cap.1$code)]
# }

#exclude duplicates from captures for next for loop:
#cap.1 omits duplicates
names(cap.1)
names(bre.stacked)
cap.1$sp.code <- paste(cap.1$species, cap.1$code, sep="-")
bre.stacked$sp.code <- paste(bre.stacked$species, bre.stacked$code, sep="-")

bre.stacked$ring <- NA
bre.stacked[!is.na(bre.stacked$ring.p1or2),"ring"] <- bre.stacked[!is.na(bre.stacked$ring.p1or2),"ring.p1or2"]

for(i in 1:length(bre.stacked$year)){ #modification for MAD, search for rings of individuals with code but no ring
  print(i)
  xx<-"X\\.X"
  xm<-"\\.X\\|M"
  xxm<-"\\|X\\.M"
  if(is.na(bre.stacked$ring[i]) & 
     !is.na(bre.stacked$code[i]) &
     !grepl(xx, bre.stacked$code[i]) &
     !grepl(xm, bre.stacked$code[i]) &
     !grepl(xxm,bre.stacked$code[i])){#its not working 24/03/2016
       ind<-which(cap.1$sp.code %in% bre.stacked$sp.code[i])
       cap.add <- cap.1[ind, ] 
       bre.stacked$ring[i] <- ifelse(length(ind)>0, cap.add$ring, NA)
       }else{
    bre.stacked$ring[i] <- bre.stacked$ring.p1or2[i]
  }
}
#-----------develop previous for-----
# bre.stacked[is.na(bre.stacked$ring.p1or2) & !is.na(bre.stacked$code),]
# bre.stacked[1549,]
# 
# table(bre.stacked$code)
# i<-1223
#---------------------------------------
  #check changes:
head(bre.stacked)
bre.stacked[!bre.stacked$ring %in% bre.stacked$ring.p1or2 & !is.na(bre.stacked$ring), c("nest.id","ring","ring.p1or2","code")]



#-------------------------------------------------------------------------------------------------------

#how many rings remain as codes or unknown?
table(bre.stacked$ring, useNA="always") #1671 NAs
str(bre.stacked[nchar(bre.stacked$ring) !=7,]) #1671 of 3776
ind<-which(nchar(bre.stacked$ring)!=7) #740 cases with "unknown" metal ring (individuals not captured)
str(bre.stacked[-ind,]) #2105 cases with rings
ringsonly<-bre.stacked[-ind,]

#---------------------------------------------------------------------------------------------
#ADD mol sex first, in MAD, copied this section from below
names(bre.stacked)
#add column with mol.sex in bre.stacked
names(sex)

#First create ID of individuals that will be used to assign sex: code for ones with no metal and ring for ones with metal ring:
bre.stacked$id.adult <- NA

bre.stacked[is.na(bre.stacked$ring) & !is.na(bre.stacked$code), "code"]
bre.stacked$id.adult[is.na(bre.stacked$ring) & !is.na(bre.stacked$code)]<-bre.stacked[is.na(bre.stacked$ring) & !is.na(bre.stacked$code), "code"]

bre.stacked[!is.na(bre.stacked$ring), "ring"]
bre.stacked$id.adult[!is.na(bre.stacked$ring)]<-bre.stacked[!is.na(bre.stacked$ring), "ring"]
#-----------------------------------------
bre.stacked$mol_sex <- NA
for (i in 1:length(bre.stacked$year)){
  
  bre.stacked$mol_sex[i] <- sex$sex[match(bre.stacked$id.adult[i], sex$ring)]
}

for (i in 1:length(cap$year)){
  
  cap$mol_sex[i] <- sex$sex[match(cap$ring[i], sex$ring)]
}

table(cap$mol_sex, useNA="always")
# ?    F FAIL    M <NA> 
#   3 1473    1 1446 2745 

cap2<-cap
cap<-cap2

for(i in 1:length(cap$year)){
  if(is.na(cap$mol_sex[i]) &is.na(cap$ring[i]) & !is.na(cap$code[i])){
    cap$mol_sex[i] <- sex$sex[match(cap$code[i], sex$ring)]
  }
}

table(cap$mol_sex, useNA="always")
# ?    F FAIL    M <NA> 
#   3 1538    1 1541 2585 
#cap$sex

tail(bre.stacked[!is.na(bre.stacked$mol_sex),])
table(bre.stacked$mol_sex, useNA="always")
#   ?    F    M <NA> 
# 4  842  826 2104
#-------------------------------------------------------
#Add marker saying if individuals in Birdref were captured that year or not.-----------------------------
#This will allow to see whether id is reliable or not and check whether ids match
bre.stacked$id.nest.sex <- paste(bre.stacked$nest.id, bre.stacked$mol_sex, sep="-") #NEEDS MOL SEX IN BIRDREF

table(cap$species)
cap$id.nest <- paste(cap$year, cap$species, cap$nest, sep="-")
cap$id.nest.sex<-paste(cap$id.nest, cap$mol_sex, sep="-")

bre.stacked$captured_in_focalyear <- ifelse(bre.stacked$id.nest.sex %in% cap$id.nest.sex[!cap$age %in% "J" & cap$site %in% "Andavadoaka"], "yes", "no")
table(bre.stacked$captured_in_focalyear, useNA="always")
# no  yes <NA> 
#   1700 2076    0  

str(bre.stacked[!is.na(bre.stacked$id.adult) & bre.stacked$captured_in_focalyear %in% "no",]) #376 MAD not captured in focal year
str(bre.stacked[!is.na(bre.stacked$id.adult) & bre.stacked$captured_in_focalyear %in% "yes",])#1868 captured in focal year

y<-bre.stacked[!is.na(bre.stacked$id.adult) & bre.stacked$captured_in_focalyear %in% "no",] #see a random sample to see if anything looks suspicious
y[sample(1:nrow(y),30, replace=F),]


# #check that sex of rings2 correspond to sex of captures and molecular sex
ind<-which(nchar(bre.stacked$ring)!=7) #740 cases with "unknown" metal ring (individuals not captured)
str(bre.stacked[-ind,]) #2105 cases with rings
ringsonly<-bre.stacked[-ind,]

ring2.sex1 <- paste(ringsonly$ring, ringsonly$mol_sex, sep="-")
rs1<- as.data.frame(table(ring2.sex1))

table(cap$age)
cap[is.na(cap$ring),]
nojna <- cap[!cap$age %in% "J" & !is.na(cap$ring) & cap$site %in% "Andavadoaka",]
ring.sex2 <- paste(nojna$ring, nojna$mol_sex, sep="-")
rs2 <- as.data.frame(table(ring.sex2))

diff1 <- setdiff(rs1$ring2.sex1,rs2$ring.sex2) #those that are in birdref but not in captures 26, most are ones with conflicting sex
diff2 <- setdiff(rs2$ring.sex2, rs1$ring2.sex1) #those that are in captures but not in birdref 79!!! too many...and no chicks here
      #CA1674 was in this list when code was run in April before ring corrections, in new run this is missing: check

#--------------debug/check ids
bre.stacked[bre.stacked$ring %in% "FH72604",] 
cap[cap$ring %in% "FH72604",]
nojna[nojna$ring %in% "FH47149",]

bre.stacked[bre.stacked$nest.id %in% "2014-KiP--201",]
cap[cap$id.nest %in% "2014-KiP--201",]

#-------------------------------------
#----------------------------------------------------------------------------

#  	Check that rings match rings in captures using nest numbers
unique(bre.stacked$site)
unique(cap$site)
#"S"   "R"   "PP" (Ponta Preta)  "RDL" (ribera da lagoa) "C" (calheta)  "M"   "B" (?)  "TS" (Terras Salgadas) "CV" (Casas Velhas)

# bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")
# cap$nest.id <- paste(cap$year, cap$site, cap$nest, sep="-")
#   
#   #create pasted ids for each capture and bird ref id to compare which are missing and if rings come
  #from the same nest
names(bre.stacked)
#parents.bre.mol <- paste(bre.stacked$nest.id, bre.stacked$id.adult, bre.stacked$mol_sex, sep="-")
parents<-bre.stacked[!is.na(bre.stacked$id.adult),]
parents.bre <- paste(parents$nest.id, parents$id.adult, "A", sep="-")
#parents.bre.field <- paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$field_sex, sep="-") 

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
  chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, "J", sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
  chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, "J", sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
  chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, "J", sep="-")

cap[is.na(cap$ring),]
  ids.cap <- ifelse(is.na(cap$ring),
                    paste(cap$id.nest, cap$code, cap$age, sep="-"),
                    paste(cap$id.nest, cap$ring, cap$age, sep="-"))

#ids.bre.mol <- c(parents.bre.mol, chicks1.bre, chicks2.bre, chicks3.bre)
ids.bre <- c(parents.bre, chicks1.bre, chicks2.bre, chicks3.bre)
  #ids.bre.field <- c(parents.bre.field, chicks1.bre, chicks2.bre, chicks3.bre)

str(ids.cap) #5668 total
unique(ids.cap) #5000 unique

str(parents.bre) #2244
#unique(ids.bre.mol)#2824

#str(ids.bre.field) #4058
str(ids.bre) #4870
#unique(ids.bre.field)#3024
#count <- as.data.frame(table(ids.bre.field))
count <- as.data.frame(table(ids.bre))      
      #CHECK
      count[count$Freq>2,] #each id should be present as duplicate, more than three are mistakes? or ambiguous codes
      # 18    2009-KiP--10-FH47258-J    4
      # 191     2009-MP-40-FH47086-J    4
      # 321     2009-WfP-9-FH47004-J    4
      # 732   2012-KiP-46a-FH68832-J    4
      # 839    2012-WfP-10-FH69272-J    4
      # 1007 2013-KiP--135-FH72284-J    4
      # 1023  2013-KiP--18-FH72196-J    4
      # 1025  2013-KiP--19-FH72293-J    4
      # 1026  2013-KiP--19-FH72294-J    4
      # 1040  2013-KiP--24-FH72312-J    4
      # 1050  2013-KiP--28-FH72341-J    4
      # 1067  2013-KiP--34-FH72362-J    4
      # 1073  2013-KiP--36-FH72377-J    4
      # 1094  2013-KiP--46-FH72388-J    4
      # 1107  2013-KiP--51-FH72422-J    4
      # 1127   2013-KiP--6-FH72133-J    4
      # 1731 2014-KiP--111-FH72560-J    4
      # 1733 2014-KiP--112-FH72562-J    4
      # 1734 2014-KiP--112-FH72563-J    4
      # 2000    2014-MP--1-FH72810-J    4
      # 2591        2015-KiP-1b-UR-J    4
      # 2833  2015-KiP-405-FH73314-J    4
      # 2834  2015-KiP-405-FH73315-J    4
      # 3029      2015-MP--103b-UR-J    4
      # 3084        2015-MP--5b-UR-J    4

      #Debug previous list. Some are chicks that were recaptured and same ring appears in chick1 and chick2, some have multiple appearances in Birdref with different info for parents      
      bre.stacked[bre.stacked$nest.id %in% "2009-MP-40",]
      bre.stacked[bre.stacked$nest.id %in% "2009-MP-40",]  #chick 1 and 2 same ring!! corrected below ,/
      cap[cap$id.nest %in% "2009-MP-40",]
      
      #7 nests in Birdref with no nest number??
      # bre.stacked[bre.stacked$nest.id %in% "2009-S-N/A",c("year","nest","site","code","chick1","chick2","chick3","comments_field")]
      # bre[bre$year %in% "2009" & bre$nest %in% "N/A",]

#----------------------------------------------------------------------------------
      #ERROR FOUND --> same ring for chick1 and chick2 corrections below
      
#----------------------------------------------------------------------------------
#change sexes that read False instead of F 
# #####added 15/01/2016
# names(bre.stacked)
# table(bre.stacked$field_sex)
# bre.stacked[bre.stacked$field_sex %in% "FALSE", "field_sex"] <- "F"

#------------------------------------------------------------

#run previous part with ids pasted again including error corrections
# bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")
# parents.bre.mol <- ifelse(!is.na(bre.stacked$code),
#                           paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$mol_sex, sep="-"),
#                           NA)
#   
# 
# parents.bre.field <- ifelse(!is.na(bre.stacked$code),
#                             paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$sex, sep="-"),
#                             NA)
# 
# chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
# chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, "J", sep="-")
# 
# chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
# chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, "J", sep="-")
# 
# chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
# chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, "J", sep="-")
# 
# cap[is.na(cap$ring),]
# ids.cap <- ifelse(is.na(cap$ring),
#                   paste(cap$nest.id, cap$code, cap$sex, sep="-"),
#                   paste(cap$nest.id, cap$ring, cap$sex, sep="-"))
# 
# ids.bre.mol <- c(parents.bre.mol, chicks1.bre, chicks2.bre, chicks3.bre)
# ids.bre.field <- c(parents.bre.field, chicks1.bre, chicks2.bre, chicks3.bre)
# 
# str(ids.cap) #1935 total
# unique(ids.cap) #1810 unique
# 
# str(ids.bre.mol) #4058
# unique(ids.bre.mol)#2366
# 
# str(ids.bre.field) #4058
# unique(ids.bre.field)#2381, after error correction 2570,after eliminating NAs 1997
# count <- as.data.frame(table(ids.bre.field))
# 
# setdiff(ids.bre.mol, ids.bre.field)#691 with unknown mol sex
# 
# mol.field <- union(ids.bre.mol, ids.bre.field) #add all ids with molsex and fieldsex 

setdiff(unique(ids.cap), unique(ids.bre))#1706 captured individuals which are not present in BirdRef 29/03/2016
  
#----debug--------
bre.stacked[bre.stacked$nest.id %in% "2015-MP-613",] #2015-WfP-621-Y.X|G.X-J
cap[cap$id.nest %in% "2015-MP-613",]
cap[cap$ring %in% "FH73004",]
  #after correction of NA...no changes...see why...changes were not applied to nest.id,
#--------------------------------------
#   #In Maio: after correcting nest.id number is reduced to 53
#   z<-ids.cap[grep(pattern="2009-S-NA-", ids.cap, perl=T)]
#   cap$id.nest.ring.sex <- paste(cap$year, cap$site, cap$nest, cap$ring, cap$sex, sep="-")
#   cap[cap$id.nest.ring.sex %in% z,] #some were caught using mistnet (4), in 3 cases it is unknown if they were caught with mistnet or not...add note in comments_stdfile
# 
#   
# x <- setdiff(ids.cap, mol.field)
# setdiff(mol.field, ids.cap) #1284 individuals in BirdRef not in captures. These are individuals
#                             #that were observed but not captured or were unknown or unringed
#                             
# 
# #check why some are missing from BirdRef or Captures
# bre.stacked[bre.stacked$ring %in% "CA3668",]
# bre.stacked[bre.stacked$nest.id %in% "2013-S-27",]
# cap.std[cap.std$nest.id %in% "2014-S-301",]

#--------------------------------------------------------------------------------
#See how many nests in birdref have rings or codes known but where adults were not captured in focal year
names(bre.stacked)
table(bre.stacked$year)
str(bre.stacked[!is.na(bre.stacked$id.adult) & bre.stacked$captured_in_focalyear %in% "no",c("code","ring")]) #377

adults.not.captured <- bre.stacked[!is.na(bre.stacked$id.adult) & bre.stacked$captured_in_focalyear %in% "no", c(1:3,21,18,11,22,14,24)]
adults.not.captured$age <- "A"
names(adults.not.captured)
colnames(adults.not.captured)<-c("year", "site", "nest", "ring", "code","species","mol_sex", "id.nest","captured_in_focalyear","age")

#------------------check adults not captured
head(adults.not.captured)

#some juveniles were recorded as parents in birdref...this adds errors to this list
adults.not.captured$year.ring <- paste(adults.not.captured$year, adults.not.captured$ring, sep="-")
juvs.caps <- cap[cap$age %in% "J",]
#----------------------------------------

#omit juveniles that were wrongly placed as adults in birdref
juvs.omit<-adults.not.captured[adults.not.captured$year.ring %in% juvs.caps$year.ring,"year.ring"]

adults.not.captured<-adults.not.captured[!adults.not.captured$year.ring %in% juvs.omit,]

#--------------------------------------------------

#-----omit individual with wrong ring FH47178 should be FH47187
ind<-which(adults.not.captured$ring %in% "FH47178")
adults.not.captured[ind,] 
adults.not.captured<-adults.not.captured[-ind,]
#-------------------------
#add these ids to captures
names(cap)
cap$id.ind<-NA
cap[is.na(cap$ring) & !is.na(cap$code), "code"]
cap$id.ind[is.na(cap$ring) & !is.na(cap$code)]<-cap[is.na(cap$ring) & !is.na(cap$code), "code"]

cap[!is.na(cap$ring), "ring"]
cap$id.ind[!is.na(cap$ring)]<-cap[!is.na(cap$ring), "ring"]


inds.captured<-cap[cap$year>2007,c(1:3,31,12,16,17,27,28)]
table(inds.captured$year)
inds.captured$captured_in_focalyear <- "yes"
colnames(inds.captured)[4]<-"ring"
names(inds.captured)

#merge captured and uncaptured
library(gtools)
capt.create.bref<-smartbind(inds.captured, adults.not.captured)
str(capt.create.bref) #4879 obs

#rename
capt<-capt.create.bref

#-------------------------------------------------------------------------------

#---debug------------------

names(capt)
cap$year.ring <- paste(cap$year, cap$ring, sep="-")
names(bre.stacked)
bre.stacked$year.ring <- paste(bre.stacked$year, bre.stacked$ring, sep="-")

ind<-which(bre.stacked$year.ring %in% cap$year.ring & bre.stacked$captured_in_focalyear %in% "no" )
str(bre.stacked[ind, c("nest.id","ring", "mol_sex")]) #625 which appear as not captured but were captured that year

cap[cap$year.ring %in% "2015-FH73404",]
cap[cap$id.nest %in% "2015-MP-224b",]
bre.stacked[bre.stacked$ring %in% "FH73404",]
cap[cap$id.nest %in% "2013-KiP--2",]

#Examples checked were ok, if "captured_in_focalyear" said yes or no it did match the reality ,/
#---------------------------------------


#.------------------------------------------------------------------------------
#Create New BirdRef file from captures and ids of uncaptured birds from original birdref file
#capt<-capt.create.bref
capt$NestID <- paste(capt$year, capt$site, capt$species, capt$nest, sep='-') # Create unique nestIDs

nests <- sort(unique(capt$NestID)) # list of unique nest IDs in capture file
birdref <- array(, c(length(nests), 16)); birdref <- as.data.frame(birdref) # create empty birdref
names(birdref) <- c("year", "site", "nest","species", "parent1","code.p1", "parent2", "code.p2","chick1", "chick2", "chick3", "mol_sex_p1", "mol_sex_p2", "captured_focalyear_p1", "captured_focalyear_p2", "comments_stdfile") # name columns in birdref
birdref$error.p <- NA 
birdref$error.ch <-NA

#library(gtools)
#--------for debug-------
#01/04/2016 Errors found:
#a) in captured_in_focalyear, if one adult was captured twice, then it appears as both adults from the nest were captured...
#b) if a juvenile was misplaced as a parent in birdref, it will consider it as one of the parents...

i<-which(nests %in% "2013-Andavadoaka-KiP-208")
#-------------------------
for (i in 1:nrow(birdref)){
  print(i)
  n <-nests[i]
  cc <- capt[capt$NestID%in%n,]
  birdref[i,c("year","site", "nest", "species")] <- cc[1,c("year","site", "nest", "species")]
  
  # Add p1 and p2 ring numbers to birdref
  p.ring <- unique(cc$ring[cc$age %in% "A"])
  p.code <- unique(cc$code[cc$age %in% "A"])
  capt.focal <- cc[cc$age %in% "A" & !duplicated(cc$ring),c("ring","captured_in_focalyear")]

  if(length(p.ring)==1 | length(p.code)==1){
    birdref[i, "parent1"] <- p.ring[1]
    birdref[i, "code.p1"] <- p.code[1]
    birdref[i, "captured_focalyear_p1"] <- capt.focal[1]
  }
    
  if(length(p.ring)>1 | length(p.code)>1 | length(p.ring)<3 | length(p.code)<3){
    birdref[i, "parent1"] <- p.ring[1]
    birdref[i, "code.p1"] <- p.code[1]
    birdref[i, "captured_focalyear_p1"] <- capt.focal[1]
    birdref[i, "parent2"] <- p.ring[2]
    birdref[i, "code.p2"] <- p.code[2]
    birdref[i, "captured_focalyear_p2"] <- capt.focal[2]
  }
  # Error message if more than 2 adult ring numbers per nestID
  if(length(p.ring)>2 | length(p.code)>2){
    birdref$error.p[i] <-capture.output(cat ("More than 2 parents?", n, " - ", as.character(p.ring, p.code), "\n"))
    }
  
  #Add chick ring number to birdref
  ch <- unique(cc$ring[cc$age%in%'J'])
  birdref[i, c("chick1", "chick2", "chick3")] <- ch[1:3]
  # Error message if more than 3 chick ring numbers per nestID
  if(length(ch)>3){
    birdref$error.ch[i]<-capture.output(cat('CHICK', n, " - ", as.character(ch), "\n"))}
 
}

#---------------debug------------------------
tail(birdref, n=50)
str(birdref[!is.na(birdref$error.ch),]) #12 with error in chicks
str(birdref[!is.na(birdref$error.p),]) #37 with error in parents

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#FINAL CORRECTIONS

#some have mistaken rings...others are nests missing from BirdRef:
#to.write <- bre1#[,c(2:9,14:21)]
#to.write[to.write$year ==2011 & to.write$site =="S" & to.write$nest==4,]
#------------------------------------------------------------------------------------------
#-------------------------------------Write BirdRef_std------------------------------------
#------------------------------------------------------------------------------------------

setwd("F:/Plovers/3rd Chapter/input/Madagascar")

write.csv(birdref, "BirdRef_Mad_stdfile_CCI_29Mar2016.csv")


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
