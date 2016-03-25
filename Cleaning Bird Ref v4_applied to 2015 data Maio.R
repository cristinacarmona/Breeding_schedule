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
names(working.list) <- c("bf2","br","bf","cap","sex","ne","re")
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
bre.stacked[bre.stacked$ring %in% "FH80190",] 
cap[cap$ring %in% "FH80190",]
nojna[nojna$ring %in% "FH47149",]

bre.stacked[bre.stacked$nest.id %in% "2015-KiP--203b",]
cap[cap$id.nest %in% "2012-WfP-102",]

#-------------------------------------
#----------------------------------------------------------------------------

#  	Check that rings match rings in captures using nest numbers
unique(bre.stacked$site)
unique(cap$site)
#"S"   "R"   "PP" (Ponta Preta)  "RDL" (ribera da lagoa) "C" (calheta)  "M"   "B" (?)  "TS" (Terras Salgadas) "CV" (Casas Velhas)

bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")
cap$nest.id <- paste(cap$year, cap$site, cap$nest, sep="-")
  
  #create pasted ids for each capture and bird ref id to compare which are missing and if rings come
  #from the same nest
names(bre.stacked)
parents.bre.mol <- paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$mol_sex, sep="-")
parents.bre.field <- paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$field_sex, sep="-") 

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
  chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, "J", sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
  chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, "J", sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
  chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, "J", sep="-")

cap[is.na(cap$ring),]
  ids.cap <- ifelse(is.na(cap$ring),
                    paste(cap$nest.id, cap$code, cap$sex, sep="-"),
                    paste(cap$nest.id, cap$ring, cap$sex, sep="-"))

ids.bre.mol <- c(parents.bre.mol, chicks1.bre, chicks2.bre, chicks3.bre)
ids.bre.field <- c(parents.bre.field, chicks1.bre, chicks2.bre, chicks3.bre)

str(ids.cap) #1935 total
unique(ids.cap) #1810 unique

str(ids.bre.mol) #4058
unique(ids.bre.mol)#2824

str(ids.bre.field) #4058
unique(ids.bre.field)#3024
count <- as.data.frame(table(ids.bre.field))
      
      #CHECK
      count[count$Freq>2,] #each id should be present as duplicate, more than three are mistakes? or ambiguous codes
      bre.stacked[bre.stacked$nest.id %in% "2007-S--2",]
      bre.stacked[bre.stacked$nest.id %in% "2012-S--18",]  #chick 1 and 2 same ring!! corrected below ,/

      #7 nests in Birdref with no nest number??
      bre.stacked[bre.stacked$nest.id %in% "2009-S-N/A",c("year","nest","site","code","chick1","chick2","chick3","comments_field")]
      bre[bre$year %in% "2009" & bre$nest %in% "N/A",]

#----------------------------------------------------------------------------------
      #ERROR FOUND --> same ring for chick1 and chick2 corrections below
      
#----------------------------------------------------------------------------------
#change sexes that read False instead of F 
#####added 15/01/2016
names(bre.stacked)
table(bre.stacked$field_sex)
bre.stacked[bre.stacked$field_sex %in% "FALSE", "field_sex"] <- "F"

#------------------------------------------------------------

#run previous part with ids pasted again including error corrections
bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")
parents.bre.mol <- ifelse(!is.na(bre.stacked$code),
                          paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$mol_sex, sep="-"),
                          NA)
  

parents.bre.field <- ifelse(!is.na(bre.stacked$code),
                            paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$sex, sep="-"),
                            NA)

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, "J", sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, "J", sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, "J", sep="-")

cap[is.na(cap$ring),]
ids.cap <- ifelse(is.na(cap$ring),
                  paste(cap$nest.id, cap$code, cap$sex, sep="-"),
                  paste(cap$nest.id, cap$ring, cap$sex, sep="-"))

ids.bre.mol <- c(parents.bre.mol, chicks1.bre, chicks2.bre, chicks3.bre)
ids.bre.field <- c(parents.bre.field, chicks1.bre, chicks2.bre, chicks3.bre)

str(ids.cap) #1935 total
unique(ids.cap) #1810 unique

str(ids.bre.mol) #4058
unique(ids.bre.mol)#2366

str(ids.bre.field) #4058
unique(ids.bre.field)#2381, after error correction 2570,after eliminating NAs 1997
count <- as.data.frame(table(ids.bre.field))

setdiff(ids.bre.mol, ids.bre.field)#691 with unknown mol sex

mol.field <- union(ids.bre.mol, ids.bre.field) #add all ids with molsex and fieldsex 
setdiff(ids.cap, mol.field)#43 captured individuals which are not present in BirdRef 15/01/2015
  #14 cases that do not appear in birdref
# [1] "2007-S-5-CA1139-F"   
# [2] "2008-S-3-CA1148-F"   
# [3] "2008-S-3-CA1192-J"   
# [4] "2009-RDL-NA-CA1615-F"
# [5] "2009-S-NA-CA1616-F"  
# [6] "2009-S-NA-CA1617-M"  
# [7] "2010-S-NA-CA1677-M"  
# [8] "2012-S-NA-CA2629-F"  
# [9] "2009-S-NA-CA2656-M"  
# [10] "2013-R-203-CA2756-M" 
# [11] "2009-S-NA-CA3050-M"  
# [12] "2010-S-123-CA3083-F" 
# [13] "2010-S-NA-CA3110-NA" 
# [14] "2010-S-11-CA3483-J"
  bre.stacked[bre.stacked$nest.id %in% "2015-S--10",]
  cap[cap$nest.id %in% "2015-S-11",]
  #after correction of NA...no changes...see why...changes were not applied to nest.id,
  
  #after correcting nest.id number is reduced to 53
  z<-ids.cap[grep(pattern="2009-S-NA-", ids.cap, perl=T)]
  cap$id.nest.ring.sex <- paste(cap$year, cap$site, cap$nest, cap$ring, cap$sex, sep="-")
  cap[cap$id.nest.ring.sex %in% z,] #some were caught using mistnet (4), in 3 cases it is unknown if they were caught with mistnet or not...add note in comments_stdfile

  
x <- setdiff(ids.cap, mol.field)
setdiff(mol.field, ids.cap) #1284 individuals in BirdRef not in captures. These are individuals
                            #that were observed but not captured or were unknown or unringed
                            

#check why some are missing from BirdRef or Captures
bre.stacked[bre.stacked$ring %in% "CA3668",]
bre.stacked[bre.stacked$nest.id %in% "2013-S-27",]
cap.std[cap.std$nest.id %in% "2014-S-301",]

#--------------------------------------------------------------------------------
#do same but excluding sex....this might lead to less differences
str(bre.stacked) #2042
bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")#need re-defining as nest numbers changed from corrections

parents.bre <- ifelse(!is.na(bre.stacked$code),
                          paste(bre.stacked$nest.id, bre.stacked$ring2, sep="-"),
                          NA)

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, sep="-")

cap[is.na(cap$ring),]
ids.cap <- ifelse(is.na(cap$ring),
                  paste(cap$nest.id, cap$code, sep="-"),
                  paste(cap$nest.id, cap$ring, sep="-"))

ids.bre<- c(parents.bre, chicks1.bre, chicks2.bre, chicks3.bre)


str(ids.cap) #19345 total, 2nd run 1576 (after correcting mistakes 02/06/2015)
unique(ids.cap) #1810 unique

str(ids.bre) #4058, 2nd run 3424 (after correcting mistakes 02/06/2015);
            #3rd run 3437 (after adding missing nests 02/06/2015)
unique(ids.bre)#2364 without considering sex, 1986 (02/06/2015); 1996 (after adding missing nests, 02/05/2015)

setdiff(ids.cap, ids.bre)#11 captured individuals which are not present in BirdRef
                        #2nd run n=11 20/01/2016
y<-setdiff(ids.cap, ids.bre)
setdiff(ids.bre, ids.cap) #565 individuals in BirdRef not in captures. These are individuals that were observed but not 
                          #captured or were unknown or unringed
                          #2nd run n=565 20/01/2016

x.nosex<- substr(x, 1,nchar(x)-2) #x comes from line 694, where the set including sex was obtained
setdiff(x.nosex,y)
# [1] "2008-S-3-CA1148"    "2013-R-203-CA2756" 
# [3] "2014-S-bs-5-CA2797" "2009-S-NA-CA3050"  
# [5] "2010-S-NA-CA3110-" *CA3110 is a lost ring 

#2015 run:
#"2008-S-3-CA1192" #chick wrong ring number or nest number...comment added already in captures 
#"2010-S-123-CA3083" **notebook needs to be checked 
#"2010-S-11-CA3483"

#2016 run:
# [1] "2008-S-3-CA1148"  #chick wrong ring number or nest number...comment added already in captures  
# [2] "2013-R-203-CA2756" #sexes are inverted in BirdRef
# [5] "2015-R-101-CA1043" 
# [6] "2015-S-30-CA1119"  
# [7] "2015-R-5-CA1126"   
# [8] "2015-S-311-CA2749" 
# [9] "2015-R--106-CA3130"
# [10] "2015-S--307-CA3140"
# [11] "2015-S-22-CA3145"  
# [12] "2015-R-106-CA3183" 
# [13] "2015-S-32-CA3666"  
# [14] "2015-S-11-CA3757"  
# [15] "2015-R-2-CA3761"   
# [16] "2015-S-17-CA3789"  
# [17] "2015-R-4-CA3815"   
# [18] "2015-S-5-CA3934"   
# [19] "2015-S-4-CA3940"   
# [20] "2015-S--302-CA3949"
# [21] "2015-S--5-CA3956"  
# [22] "2015-S-310-CA3974" 
# [23] "2015-S-304-CA4039" 
# [24] "2015-S--11-CA4043" 
# [25] "2015-S-317-CA4050" 
# [26] "2015-S-306-CA4077" 
# [27] "2015-S-41-CA4222"  
# [28] "2015-R-6-CA4309"   
# [29] "2015-R--102-CA4317"
# [30] "2015-R-7-CA4354"   
# [31] "2015-C--1-CA4373"  
# [32] "2015-R-103-CA4376" 
# [33] "2015-R-12-CA4377" 

#check why nests from 2015 appear in this list
bre.stacked[bre.stacked$code %in% c("CA1139","CA1192","CA3083","CA3483"),]

bre.stacked[bre.stacked$nest.id %in% c("2015-R-101", "2015-S-30", "2015-R-5", "2015-S-311", "2015-R--106", "2015-S--307", "2015-S-22", "2015-R-106", "2015-S-32", "2015-S-11", "2015-R-2", "2015-S-17", "2015-R-4", "2015-S-5", "2015-S-4", "2015-S--302", "2015-S--5", "2015-S-310", "2015-S-304", "2015-S--11", "2015-S-317", "2015-S-306", "2015-S-41", "2015-R-6", "2015-R--102", "2015-R-7", "2015-C--1", "2015-R-103", "2015-R-12"),]
  #field_sex of males is missing...correcting this eliminated nests from list
  bre.stacked[is.na(bre.stacked$field_sex),]
  
#add sex to field_sex where sex is missing:
for (i in 1:length(bre.stacked$code)){
    bre.stacked$field_sex[i] <- bre.stacked$sex[i]
}

cap[cap$nest.id %in% c("2015-R-101"),]
"2013-R-203" , "2015-S-30", "2015-R-5", "2015-S-311", "2015-R--106", "2015-S--307", "2015-S-22", "2015-R-106", "2015-S-32", "2015-S-11", "2015-R-2", "2015-S-17", "2015-R-4", "2015-S-5", "2015-S-4", "2015-S--302", "2015-S--5", "2015-S-310", "2015-S-304", "2015-S--11", "2015-S-317", "2015-S-306", "2015-S-41", "2015-R-6", "2015-R--102", "2015-R-7", "2015-C--1", "2015-R-103", "2015-R-12"),]

# 
# #check why some are missing from BirdRef or Captures
# bre.stacked[bre.stacked$chick1 %in% "CA2797",]
# bre.stacked[bre.stacked$nest.id %in% "2013-R-203",]
# cap.std[cap.std$ring %in% "CA2797",]
# cap[cap$nest.id %in% "2013-R-203",]
# 
# #some have mistaken rings...others are nests missing from BirdRef:
# #Find which are the nests missing from BirdRef
# setdiff(cap.std$nest.id,bre.stacked$nest.id) #14 nests missing from BirdRef
# bre.stacked[1744:length(bre.stacked$year),]
# 

#--------------------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#III Corrections (after applying skip to part IV)

#-----------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#add comments: field comments and stdfile comments:
#colnames(bre.stacked)[8]<- "comments_field"
#bre.stacked$comments_stdfile <- NA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------------------------------------------------------------------------------
#Corrections for file containing years 2009-2014
# #"2014-S-bs-5-CA2797" #it's a juvenile but in birdref it's logged as a female
# bre.stacked[bre.stacked$nest.id %in% "2014-S-bs-5", c("code","ring","ring2")] <- NA
# bre.stacked[bre.stacked$nest.id %in% "2014-S-bs-5", c("chick1")] <- "CA2797"
# 
# #----------------------------------------------------------------------------------
# #ERROR FOUND --> same ring for chick1 and chick2
# # year site nest        code chick1 chick2 chick3 comments field_sex        ring
# # 515  2012    S  -18        <NA> CA3693 CA3693 CA3694     <NA>         M        <NA>
# # 1387 2012    S  -18 MX.XX|WX.XX CA3693 CA3693 CA3694     <NA>         F MX.XX|WX.XX
# cap.std[cap.std$nest.id %in% "2012-S--18",]
# 
# #correct error, 3rd chick had no metal ring
# bre.stacked[bre.stacked$nest.id %in% "2012-S--18","chick1"]
# bre.stacked[bre.stacked$nest.id %in% "2012-S--18","chick2"] <- "CA3694"
# bre.stacked[bre.stacked$nest.id %in% "2012-S--18","chick3"] <- "YX.XX|BX.XX"
# bre.stacked[bre.stacked$nest.id %in% "2012-S--18",]
# 
# #correct unringed for XX.XX|XX.XX in chicks rings
# bre.stacked[bre.stacked$chick1 %in% "unringed","chick1"] #none
# bre.stacked[bre.stacked$chick2 %in% "unringed","chick2"] <- "XX.XX|XX.XX"
# bre.stacked[bre.stacked$chick3 %in% "unringed", "chick3"] #none
# #----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------
#lookup nests in bre.stacked of list that needs corrections
nests.corrections <- setdiff(cap$nest.id,bre.stacked$nest.id) #14 nests missing from BirdRef


bre.stacked[bre.stacked$nests.id %in% nests.corrections,] #none, all these nests are missing from birdRef
cap.std[cap.std$nest.id %in% nests.corrections & !is.na(cap.std$nest),]#nests from captures to add

#------------------------------------------------------------------------------------------------

# #Add missing nests to BirdRef
# names(bre.stacked)
# names(cap.std)
# 
# add.adults <- cap.std[cap.std$nest.id %in% nests.corrections & !is.na(cap.std$nest) & cap.std$sex %in% c("M","F"), c("year","site","nest","ring","sex","nest.id")]
# colnames(add.adults)[5] <- "field_sex"
# colnames(add.adults)[4] <- "code"
# addcols <- c("chick1","chick2","chick3","comments_field","ring","id.nest.sex","ring2","comments_stdfile","captured_in_focalyear","mol_sex","sex.equal","sex")
# add.adults[,addcols] <- NA
# 
# # head(add.adults)
# # paste.rows <- add.adults[unique(add.adults$code),c("year","site","nest","code","chick1","chick2","chick3","comments_field","field_sex","ring","id.nest.sex","ring2","comments_stdfile","captured_in_focalyear","mol_sex","sex.equal","nest.id","sex")]
# # str(add.adults) #7 new rows to add
# # str(paste.rows) #6 as it eliminates the duplicated one
# # str(bre.stacked) #1744
# # #setdiff(names(bre.stacked),names(add.adults))
# # # > unique(add.adults$nest.id) these are the new nests that will be added to BirdRef
# # # [1] "2011-S-6"     "2012-S--31"   "2012-TS--304"
# # # [4] "2011-S-4"     "2012-S-301" 
# # 
# # bre.stacked<-rbind(bre.stacked,add.adults)
# # str(bre.stacked) #1751
# # bre.stacked[1744:length(bre.stacked$year),]
# # #----------------------------------------------
# #add chicks to bre.stacked
# add.j <- cap.std[cap.std$nest.id %in% nests.corrections & !is.na(cap.std$nest) & cap.std$sex %in% c("J"), c("year","site","nest","code","ring","sex","nest.id")]
# colnames(add.j)[6] <- "field_sex"
# colnames(add.j)[5] <- "chick1"
# addcols <- c("code","chick2","chick3","comments_field","ring","id.nest.sex","ring2","comments_stdfile","captured_in_focalyear","mol_sex","sex.equal","sex")
# add.j[,addcols] <- NA
# add.j$field_sex <- NA
# 
# #add  chick2 to nests with more than 1 chick...
# #index <- unlist(sapply(table(add.j$nest.id), seq_len), use.names = FALSE) didn't work
# add.j.index <- transform(add.j, index = ave(rep(NA, nrow(add.j)), add.j$nest.id, FUN = seq_along))
# add.j.index<-add.j.index[order(add.j.index$nest.id),]
# 
# # #------------rerun to clear bug chick2 CA3743 appears in nest s312 where it shouldnt:
# #data was not ordered to run for loop:
# #as it is an isolated error get rid by hand:
# #bre.stacked[bre.stacked$nest.id =="2010-S-312", "chick2"]<-NA
# #------------------------------
# 
# for (i in 1:length(add.j.index$nest.id)){ #nests have to be in order for these bit to work
#   if(add.j.index$index[i]==2)
#     add.j.index$chick2[i-1]=add.j.index$chick1[i]
# }
# add.j.nests <- add.j.index[add.j.index$index==1, -19 ] #omit last column (index)
# 
# setdiff(names(add.j.nests),names(bre.stacked)) #are all columns in both data sets equal?
# #-------sex appears when debugging 03/06/2015
# add.j.nests <- add.j.nests[,-18]
#-----------------------------------------------
#bre.stacked<-rbind(bre.stacked,add.j.nests) #yields wrong dataset, if the nests of these chicks exist already in bre.stacked it will duplicate them

# #conditional rbind, only rbind the nests that do not exist in bre.stacked already
# bre.stacked[bre.stacked$nest.id %in% add.j.nests$nest.id,]
# 
# #add chicks to nests that already exist
# for (i in 1:length(bre.stacked$nest.id)){ 
#   #print(i) #when debugging only
#   if(bre.stacked$nest.id[i] %in% add.j.nests$nest.id){
#     bre.stacked$chick1[i]=add.j.nests$chick1[add.j.nests$nest.id %in% bre.stacked$nest.id[i]];
#     bre.stacked$chick2[i]=add.j.nests$chick2[add.j.nests$nest.id %in% bre.stacked$nest.id[i]]
#   }
#   else{
#     i=i+1
#   }
# }
# 
# #add chicks from nests that are not yet in bre.stacked
# chicks.nonest<-add.j.nests[!add.j.nests$nest.id %in% bre.stacked$nest.id,]
# bre.stacked<-rbind(bre.stacked,chicks.nonest)
# 
# bre.stacked[1744:length(bre.stacked$nest.id),] #check if the for loop worked ,/

# #----------------------------------------------------------------------------------
# #check nest with 4 chicks??? "2008-S-3"
# cap.std[cap.std$nest.id %in% "2008-S-3",] #appears to have 4 chicks, but CA1192 says it was a recapture...however this ring does not exist
# cap.std[cap.std$ring %in% "CA1192",]
# 
# #%%%% check nests added, nest 2011-S-4 was duplicated.
# #was it a duplicate in captures?
# cap.std[cap.std$nest.id %in% "2011-S-4",] #Yes...female was captured twice
# #corrected code...in previous lines (814)
# 
# #%%%% 2012-TS--304 and 2012-S--31 had adults and chicks
# #change conditional rbind to add these ids to the existing nests
# 

#------------------------------------------------------------------------------------------------------------
#corrections 01/06/2015 


#nest 2013-S-12 sexes were inverted, female captured was molecularly a male and male's coder blonged to a female

# bre.stacked[grep(pattern="(2013-S-12-.)", bre.stacked$id.nest.sex, perl=T),]
# cap$nest.id <- paste(cap$year,cap$site,cap$nest, sep="-")
# cap[cap$nest.id %in% "2013-S-12",]
# cap.std[cap.std$code %in% "OX.MX|BX.OX",]
# 
# bre.stacked[1742,]
# bre.stacked[721,]
# #bre.stacked[633, "field_sex"] <- "F"
# #sex[sex$ring %in% "CA3607",]
# #bre.stacked[633, "ring2"] <- "CA3607"
# bre.stacked[1742, "comments_stdfile"] <- "Sexes inverted in field. Male observed in field was OX.MX|BX.OX (CA3607), however the supposed female was captured: CA3010 and molsex indicates that CA3010 is the male (corrected CCI 2015)"
# bre.stacked[721, "field_sex"] <- "M"
# bre.stacked[721, "comments_stdfile"]<- ""
# 
# 
# ##CA1104, CA3083 codes in BirdRef are wrong, added comment in comment_stdfile (mentioning wrong codes: MX.RX|GX.OX for 2012-S-3-F, MX.WX|OX.LX for 2012-S-107-F)
# bre.stacked[1407, "comments_stdfile"] <- "code in Birdref was wrong, however this female was captured (code recorded before: MX.RX|GX.OX) (CCI 2015)"
# bre.stacked[1464, "comments_stdfile"] <- "code in Birdref was wrong, however this female was captured (code recorded before: MX.WX|OX.LX) (CCI 2015)"
# 
# #------------------------------------------------------------------------------------------------------------
# 
# #corrections...have to add note to "2008-S-3" capture of CA1192 there is a typo either in chicks ring or in nest, Araceli's notes need to be checked. Done ,/
# 

#---------------------------------------------------------------------------------------------------------
#Some individuals from captures which are missing in birdref are from nests with no number....but they are as NA instead of N/A...change
#corrections: 01/06/2015

# unique(bre.stacked$nest)
# bre.stacked$nest[bre.stacked$nest %in% "N/A"] <- "NA"
# #------------------------------------------------------------------------------------------
# 
# #after correction of NA in nests...no changes...see why ADD COMMENTS
# z<-ids.cap[grep(pattern="2009-S-NA-", ids.cap, perl=T)]
# cap.std$id.nest.ring.sex <- paste(cap.std$year, cap.std$site, cap.std$nest, cap.std$ring, cap.std$sex, sep="-")
# cap.std[cap.std$id.nest.ring.sex %in% z,] #some were caught using mistnet (4), in 3 cases it is unknown if they were caught with mistnet or not...add note in comments_stdfile
# mist.net<-cap.std[cap.std$id.nest.ring.sex %in% z & cap.std$comments_field %in% "caught using mistnet",]
# 
# head(bre.stacked)
# bre.stacked[bre.stacked$ring2 %in% c("CA1617","CA2656"), "comments_stdfile"] <- "caught with mistnet (CCI 2015) remove from Birdref??"
# bre.stacked[bre.stacked$nest %in% "NA",]
# 
# #rest of nests with no nest number add comment:
# bre.stacked[bre.stacked$nest %in% "NA" & is.na(bre.stacked$comments_stdfile), "comments_stdfile"] <- "unclear if these were caught with mistnet, no comments in captures file (CCI 2015)"

#------------------------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#IV - Checking corrections and pooling remaining errors

#START 02/06/2015


#-copy- part II and modify:

#II. Check errors:
# str(bre.stacked)#1757
# bre.stacked[1744:length(bre.stacked$nest.id),] #check that new nests added are there.
# 
# 
# more.11chr <- bre.stacked[nchar(bre.stacked$code)!=6, c("year","site","sex","nest","code","comments_field","comments_stdfile")] 
# str(more.11chr) #761 might not be rings #768
# 
# no.ring <- more.11chr[order(more.11chr$code)& !is.na(more.11chr$code),]
# str(no.ring) #206 with no ring known, the rest are NA. AFTER MANIP 180 #180
# 
# bre.stacked$code[!bre.stacked$code %in% more.11chr$code] #correct rings = 983 #989
# 
# unique(no.ring$code) #122, most of these are complete codes
# 

#generate list of duplicates in captures
# cap.std$sex.code <- paste(cap.std$code, cap.std$sex, sep="-")
# 
# cap.std$code.ring <- paste(cap.std$code, cap.std$ring, sep="-")
# 
# 
# #code.rings <- Filter(function(x)length(x)>1, split(cap.std$ring,cap.std$code))
# count.codes <- cap.std[!cap.std$sex %in% "J" & !is.na(cap.std$ring),]
# str(count.codes) #759 obs
# 
# count <- as.data.frame(table(count.codes$code.ring)) #count appearance of unique code.ring
# severalapp <- count[count$Freq>1,]
# singleapp <- count[count$Freq<2,]

# library(stringr)
# countls<- unlist(str_split(count$Var1, "-")) #the list of rings-codes gives us unique sets of rings-codes
# m <- matrix(countls, ncol=2, byrow=T)
# count.codes.ls <- as.data.frame(m) #538 obs but CA3110 has no code:
# #CA3110 appears with no code
# cap.std[cap.std$ring %in% "CA3110",] #it's a lost ring!
# 
# count.codes.ls<-count.codes.ls[-423,]
# 
# count.codes.ls$code.ring <- paste(count.codes.ls$V1, count.codes.ls$V2, sep="-")
# 
# #----------------------------------------------------------------
# #List of duplicated codes that need to be omitted
# codedupl <- count.codes.ls[duplicated(count.codes.ls$V1)| duplicated(count.codes.ls$V1, fromLast=T),]
# str(codedupl) #66 duplicated codes but including ambiguous codes
# 
# pat<- "XX.XX"#get rid of ambiguous codes from list of duplicates
# str(codedupl[!str_detect(codedupl$V1, pattern=pat),]) #remaining 61 obs
# omit <- codedupl[!str_detect(codedupl$V1, pattern=pat),]
# 
# #------------------------------------------------------------------------
# pat<- "XX"
# cases.to.change <- no.ring[!no.ring$code %in% omit$V1 & !str_detect(no.ring$code, pattern=pat),]
# #omits duplicates and ambiguous codes
# 
# match.2 <- match(cases.to.change$code, cap.std$code)
# 
# codes.to.replace <- cap.std[match.2, c("code","ring")]


#Replace codes with rings in bre.stacked only when they are not duplicates or ambiguous

# for (i in 1:length(bre.stacked$code)){
#   
#   if(bre.stacked$code[i] %in% cases.to.change$code)
#     bre.stacked$ring[i] <- codes.to.replace$ring[match(bre.stacked$code[i], codes.to.replace$code)]
#   else
#     bre.stacked$ring[i] <- bre.stacked$code[i]
#}
#This was changed below (see line 426 onwards)

# bre.stacked[bre.stacked$code %in% cases.to.change$code,] #seems right
# head(bre.stacked)
# codes.to.replace[order(codes.to.replace$code),]


#--------------------------------------------------------------------------21/05/2015 (lines 385-408)

# #there are comments that read "combination not in file", there might be cases where codes are wrong but
# #coincide with codes of birds ringed later?? check this.
# codes.check<-bre.stacked[bre.stacked$comments %in% "Combination not in file" & order(bre.stacked$code), c("year","nest","code","ring","comments")]
# cap.std[cap.std$code %in% codes.check$code & order(cap.std$code), c("year","date", "code","ring","nest")]
# 
# # codes.check
# # year nest        code        ring                comments
# # 286  2010 -111 OX.MX|GX.BX      CA3616 Combination not in file #this nest does not appear in captures!
# # 313  2010  -19 MX.LX|BX.GX      CA2741 Combination not in file
# # 450  2010  122 MX.OX|WX.YX      CA1137 Combination not in file
# # 470  2010  142 MX.OX|WX.YX      CA1137 Combination not in file
# # 1125 2010 -224 OX.MX|GX.YX      CA3639 Combination not in file
# # 1178 2010  -26 MX.LX|YX.YX MX.LX|YX.YX Combination not in file
# # 1182 2010  -22 MX.YX|RX.BX      CA3448 Combination not in file
# # 1185 2010  -19 OX.MX|BX.RX      CA2712 Combination not in file
# 
# bre.stacked[bre.stacked$nest==-111,] #this nest does not appear in captures file, but in notes the chick appears as MX.XX|OX.XX, however in notebook this nest doesn't appear either
# cap.std[cap.std$nest==-111,]
# 
# #check rest of comments to see if there are more similar cases
# bre.stacked[!is.na(bre.stacked$comments) & bre.stacked$sex %in% "F","comments"]
# 
# 
# unique(codes.to.replace$code) #75 codes in Birdref

#match id.nest.sex form bird.ref and cap.std to see which adults from nests in BirdRef with only code were captured that year
bre.stacked$id.nest.sex <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, bre.stacked$field_sex, sep="-")
cap.std$id.nest.sex <- paste(cap.std$year, cap.std$site, cap.std$nest, cap.std$sex, sep="-")

check.nests <- bre.stacked[bre.stacked$code %in% unique(codes.to.replace$code),]
in.cap<- cap.std[cap.std$id.nest.sex %in% check.nests$id.nest.sex,] # 26 of the 75 with no ring in BirdRef were captured on the year when they bred
str(in.cap)

#head(bre.stacked)
#names(bre.stacked)
#bre.stacked[!is.na(bre.stacked$sex),] #this was created when running corrections....changed ,/
#bre.stacked<-bre.stacked[,-18] #delete sex column

check.nests[check.nests$id.nest.sex %in% "2012-S-105-F",] #check first case

#---------------------------------------------------------------------------
#30/05/2015, 01/06/2015 ONLY in these nests can code be replaced with ring.
#change rings again in these cases
head(bre.stacked)

for (i in 1:length(bre.stacked$code)){
  
  if(bre.stacked$id.nest.sex[i] %in% in.cap$id.nest.sex)
    bre.stacked$ring2[i] <- in.cap$ring[match(bre.stacked$id.nest.sex[i], in.cap$id.nest.sex)]
  else
    bre.stacked$ring2[i] <- bre.stacked$code[i]
}  

#check changes:
bre.stacked[bre.stacked$ring != bre.stacked$ring2 & !is.na(bre.stacked$ring), c("id.nest.sex","ring","ring2")]
x <- bre.stacked[!is.na(bre.stacked$ring2) & bre.stacked$ring != bre.stacked$code | bre.stacked$ring != bre.stacked$ring2, c("id.nest.sex","code","ring","ring2") ]  
x[!is.na(x$ring2),]  

#some have non-matching rings...check few cases
#since some nests have NA some matchings were wrong, add NA to ring2 of these nests:
bre.stacked[bre.stacked$id.nest.sex %in% "2009-S-NA-F","ring2"] <- NA
bre.stacked[bre.stacked$id.nest.sex %in% "2009-S-NA-M","ring2"] <- NA

#in.cap[in.cap$id.nest.sex %in% "2008-S--23-M",]
#in.cap[in.cap$ring %in% "CA3032",]

a<-in.cap$id.nest.sex
b<-x$id.nest.sex[!is.na(x$id.nest.sex)]

c<-setdiff(a,b) #These are in captures but not in Birdref...check one by one
# [1] "2008-S--16-M" "2008-S--18-M" "2012-S-17-M"  "2009-S-32-F"  "2013-R-204-M"
# [6] "2012-S-101-F"

bre.stacked[bre.stacked$id.nest.sex %in% c, c("id.nest.sex","code","ring","ring2")]
#ring2 is correct, these did not have an id in Birdref but matching with captures now they do have:  
#       id.nest.sex code ring  ring2
# 58   2008-S--18-M <NA> <NA> CA1152
# 60   2008-S--16-M <NA> <NA> CA1142
# 549   2012-S-17-M <NA> <NA> CA1638
# 720  2013-R-204-M <NA> <NA> CA2746
# 1027  2009-S-32-F <NA> <NA> CA1674
# 1458 2012-S-101-F <NA> <NA> CA2757
#Considering these six and the 15 cases that were changed add up 21 (22 as one was repeated) nests where code could be changed.


#Check inconsistent rings:
x.1 <- x[grep(pattern="([0-9]+)", x$ring2, perl=T),] #only rings, omit codes
x.2 <- x.1[x.1$ring != x.1$ring2,] #mismatched rings
#     id.nest.sex        code   ring  ring2
# 633   2013-S-12-M OX.MX|BX.OX CA3607 CA3010 # comment_field: male's code belongs to a female, but female was captured so male's CR is wrong #this has been corrected
# 1407   2012-S-3-F MX.RX|GX.OX CA1159 CA1104***checked
# 1464 2012-S-107-F MX.WX|OX.LX CA3072 CA3083***checked
bre.stacked[bre.stacked$id.nest.sex %in% x.2$id.nest.sex,] #both to check *** were captured in focal year....code observed was wrong
# #--------------------------------
# cap.std[cap.std$id.nest.sex %in% x.2$id.nest.sex, c(2:7,15:18,21)] #check those inconsistent nests in captures
# #       ring  year site nest sex date        code observer comments_field                                    comments_stdfile  id.nest.sex
# # 149 CA1104 2012    S    3   F 1015 MX.OX|GX.OX       TS           <NA>                                                <NA>   2012-S-3-F #WRONG CODE in birdref
# # 751 CA3010 2013    S   12   M  920 MX.BX|OX.GX       TS           <NA> field sexed as F, molecularly sexed as M (CCI 2015)  2013-S-12-M #CORRECTED, sexes inverted
# # 868 CA3083 2012    S  107   F 1013 MX.WX|OX.YX       TS           <NA>                                                <NA> 2012-S-107-F #WRONG CODE in birdref

#look if code in birdref which does not match the one in captures belongs to opposite sex:
#unique(x.2$id.nest.sex)#look for opposite sex in birdref
#bre.stacked[bre.stacked$id.nest.sex %in% c("2013-S-12-F", "2012-S-3-M", "2012-S-107-M"),]
# year site nest        code chick1 chick2 chick3                                                                 comments_field sex   ring  id.nest.sex  ring2
# 535  2012    S    3 MX.WX|GX.WX CA3773   <NA>   <NA>                                                                           <NA>   M CA3065   2012-S-3-M CA3065 #ok
# 592  2012    S  107 OX.MX|GX.BX   <NA>   <NA>   <NA>                                                                           <NA>   M CA3616 2012-S-107-M CA3616 #ok
# 1505 2013    S   12      CA3010   <NA>   <NA>   <NA> male's code belongs to a female, but female was captured so male's CR is wrong   M CA3010  2013-S-12-F CA3010 #was CA3607, but sex was inverted, corrected below ,/

#see corrections below, in nest 2013-S-12 sexes were inverted, this was corrected ,/


#CA1104, CA3083 codes in BirdRef are wrong added comment in comment_std, see corrections below [149,868]

#-------------------------------------------------------------------------------------------------------

#how many rings remain as codes or unknown?
str(bre.stacked[nchar(bre.stacked$ring2) !=6,])
ind<-which(nchar(bre.stacked$ring2)!=6) #750 cases with "unknown" metal ring (individuals not captured)
str(bre.stacked[-ind,]) #1007 cases with rings
ringsonly<-bre.stacked[-ind,]


#Add marker saying if individuals in Birdref were captured that year or not.
#This will allow to see whether id is reliable or not
bre.stacked$captured_in_focalyear <- ifelse(bre.stacked$id.nest.sex %in% cap.std$id.nest.sex, "yes", "no")

str(bre.stacked[!is.na(bre.stacked$code) & bre.stacked$captured_in_focalyear %in% "no",]) #451 not captured in focal year/ #02/06/2015 448
str(bre.stacked[!is.na(bre.stacked$code) & bre.stacked$captured_in_focalyear %in% "yes",])#712 captured in focal year / #02/06/2015 721

y<-bre.stacked[!is.na(bre.stacked$code) & bre.stacked$captured_in_focalyear %in% "no",] #see a random sample to see if anything looks suspicious
y[sample(1:nrow(y),30, replace=F),]

str(y[nchar(y$ring2) !=6,]) #144 (145 02/01/2015) of 451 not captured in focal year have codes instead of rings, 
#these could potentially be changed to ring if the code provided belongs to an individual captured before the focal year

str(y[nchar(y$ring2) ==6,]) #307 (303 02/01/2015) of 451 not captured in focal year have rings instead of codes....we don't know how reliable these are
y[nchar(y$ring2) ==6 & !is.na(y$comments_field),c("ring2","comments_field")]



#check that sex of rings2 correspond to sex of captures and molecular sex
str(bre.stacked[nchar(bre.stacked$ring2) !=6,])
ind<-which(nchar(bre.stacked$ring2)!=6) #750 cases with "unknown" metal ring (individuals not captured)
str(bre.stacked[-ind,]) #1007 cases with rings
ringsonly<-bre.stacked[-ind,]
names(ringsonly)
ring2.sex1 <- paste(ringsonly$ring2, ringsonly$field_sex, sep="-")
rs1<- as.data.frame(table(ring2.sex1))

nojna <- cap.std[!cap.std$sex %in% "J" & !is.na(cap.std$ring),]
ring.sex2 <- paste(nojna$ring, nojna$sex, sep="-")
rs2 <- as.data.frame(table(ring.sex2))

diff1 <- setdiff(rs1$ring2.sex1,rs2$ring.sex2) #those that are in birdref but not in captures 31 (30 02/06/2015)
diff2 <- setdiff(rs2$ring.sex2, rs1$ring2.sex1) #those that are in captures but not in birdref 9 (10 02/06/2015)
# 02/06/2015: (only difference from list below)
# [1] "CA1617-M"*nest wit no number

#CA1674 was in this list when code was run in April before ring corrections, in new run this is missing: check
bre.stacked[bre.stacked$ring2 %in% "CA1674",] #it was captured in focal year but was not in birdref! This has been corrected 01/06/2015
bre.stacked[bre.stacked$ring %in% "CA1617",]
cap.std[cap.std$ring %in% "CA1617",]
#diff2
# [1] "CA1674-F"  *from nest 2009-S-32 corrected 01/06/2015
# [2]"CA1677-M" *caught with mistnets
# [3] "CA2756-M" *former captures were sexed as F
# [4]"CA3050-M" *no nest in captures, no notes, caught with mistnet??
# [5] "CA3056-F"  *appears as M in bird ref but as F in captures 
# [6]"CA3066-M" *appears as F in bird ref but as M in captures MOL = M (2010-S-15)
# [7] "CA3072-M" *field and mol sex conflicting
# [8]"CA3110-NA"*lost ring
# [9] "CA3658-F" *field and mol sex conflicting
# [10]"CA3708-M" *field and mol sex conflicting


#add column with mol.sex in bre.stacked
names(sex)

for (i in 1:length(bre.stacked$code)){
  
  bre.stacked$mol_sex[i] <- sex$sex[match(bre.stacked$ring2[i], sex$ring)]
}

bre.stacked[!is.na(bre.stacked$mol_sex),]

#add comment on rings with conflicting sex from list sent to Tess
# names(bre.stacked)
# list.conflictingsex <-c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413","CA3083","CA2692") #added CA3083 and CA2692 (02/06/2015)
#bre.stacked$comments_stdfile[bre.stacked$ring %in% c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413")]<-""
#bre.stacked$comments_stdfile[bre.stacked$ring2 %in% c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413","CA3083","CA2692")]<-"sex needs to be checked (CCI 2015)"
# bre.stacked[bre.stacked$ring2 %in% c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413"),]

# names(cap.std)
# cap.std[cap.std$ring %in% c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413"), c("year","ring","sex","comments_field")]
# sex[sex$ring %in% c("CA1068","CA1148","CA3066","CA3082","CA1136","CA2721","CA3054","CA3056","CA3413"), c("ring","sex")]


#check other conflicting sexes in birdref with mol_sex
bre.stacked$sex.equal<-ifelse(bre.stacked$field_sex==bre.stacked$mol_sex, "","check")

check.sex <- bre.stacked[bre.stacked$sex.equal %in% c("check")& !bre.stacked$sex%in%"J",c("nest","ring","field_sex","mol_sex","sex.equal","comments_field","comments_stdfile")]
str(check.sex) #35 obs
str(check.sex[!check.sex$ring %in% list.conflictingsex,]) #29 obs #27 obs (02/06/2015)

#----------------------------------------------------------------------------

#    Check that rings match rings in captures using nest numbers
unique(bre.stacked$site)
unique(cap.std$site)
#"S"   "R"   "PP" (Ponta Preta)  "RDL" (ribera da lagoa) "C" (calheta)  "M"   "B" (?)  "TS" (Terras Salgadas) "CV" (Casas Velhas)

bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")
cap.std$nest.id <- paste(cap.std$year, cap.std$site, cap.std$nest, sep="-")

#create pasted ids for each capture and bird ref id to compare which are missing and if rings come
#from the same nest
names(bre.stacked)
parents.bre.mol <- paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$mol_sex, sep="-")
parents.bre.field <- paste(bre.stacked$nest.id, bre.stacked$ring2, bre.stacked$field_sex, sep="-") 

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, "J", sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, "J", sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, "J", sep="-")

cap.std[is.na(cap.std$ring),]
ids.cap <- ifelse(is.na(cap.std$ring),
                  paste(cap.std$nest.id, cap.std$code, cap.std$sex, sep="-"),
                  paste(cap.std$nest.id, cap.std$ring, cap.std$sex, sep="-"))

ids.bre.mol <- c(parents.bre.mol, chicks1.bre, chicks2.bre, chicks3.bre)
ids.bre.field <- c(parents.bre.field, chicks1.bre, chicks2.bre, chicks3.bre)

str(ids.cap) #1699 total
unique(ids.cap) #1576 unique

str(ids.bre.mol) #3422 #3451 (02/06/2015)
unique(ids.bre.mol)#2387 #2410 (02/06/2015)

str(ids.bre.field) #3422 #3451 (02/06/2015)
unique(ids.bre.field)#2569 #2593 (02/06/2015)
count <- as.data.frame(table(ids.bre.field))


setdiff(ids.bre.mol, ids.bre.field)#513 with unknown mol sex, 01/06/2015 after ring2 corrections 558
                                    #958 (02/06/2015)
mol.field <- union(ids.bre.mol, ids.bre.field) #add all ids with molsex and fieldsex 
setdiff(ids.cap, mol.field)#59 captured individuals which are not present in BirdRef/ 01/06/2015 57 captured not present in Birdref
                            #33 captured individuals which are not present in BirdRef (02/06/2015)

x <- setdiff(ids.cap, mol.field)
setdiff(mol.field, ids.cap) #993 individuals in BirdRef not in captures. These are individuals
#that were observed but not captured or were unknown or unringed
#01/06/2015 1032 missing after correction of ring2
#02/06/2015 2008

#-------------------------------------------------------------
#--------------------------------------------------------------------------------
#do same but excluding sex....this might lead to less differences
str(bre.stacked) #1751 #1757 (02/06/2015)
bre.stacked$nest.id <- paste(bre.stacked$year, bre.stacked$site, bre.stacked$nest, sep="-")#need re-defining as nest numbers changed from corrections

parents.bre <- ifelse(!is.na(bre.stacked$code),
                      paste(bre.stacked$nest.id, bre.stacked$ring2, sep="-"),
                      NA)

chicks1 <- bre.stacked[!is.na(bre.stacked$chick1),]#only chicks1 that have ring
chicks1.bre <- paste(chicks1$nest.id, chicks1$chick1, sep="-")

chicks2 <- bre.stacked[!is.na(bre.stacked$chick2),]
chicks2.bre <- paste(chicks2$nest.id, chicks2$chick2, sep="-")

chicks3 <- bre.stacked[!is.na(bre.stacked$chick3),]
chicks3.bre <- paste(chicks3$nest.id, chicks3$chick3, sep="-")

cap.std[is.na(cap.std$ring),]
ids.cap <- ifelse(is.na(cap.std$ring),
                  paste(cap.std$nest.id, cap.std$code, sep="-"),
                  
                  paste(cap.std$nest.id, cap.std$ring, sep="-"))

ids.bre<- c(parents.bre, chicks1.bre, chicks2.bre, chicks3.bre)


str(ids.cap) #1699 total, 1699 (02/06/2015)
unique(ids.cap) #1576 unique

str(ids.bre) #3422, 3451 (02/06/2015);

unique(ids.bre)#1985 without considering sex, 2002 (02/05/2015)

setdiff(ids.cap, ids.bre)#56 captured individuals which are not present in BirdRef
#2nd run n=51 01/06/2015
#3rd run after correcting mistakes 02/06/2015 n=34

y<-setdiff(ids.cap, ids.bre)
setdiff(ids.bre, ids.cap) #465 individuals in BirdRef not in captures, if sex is accounted for there are 
#993. These are individuals that were observed but not 
#captured or were unknown or unringed
#2nd run n=460 01/06/2015
#2nd run after correcting mistakes 02/06/2015 n=460

x.nosex<- substr(x, 1,nchar(x)-2) #x comes from line 1295, where the set including sex was obtained
setdiff(x.nosex,y)
#01/06/2015
# [1] "2008-S-3-CA1148"    "2013-R-203-CA2756" 
# [3] "2014-S-bs-5-CA2797" "2009-S-NA-CA3050"  
# [5] "2010-S-NA-CA3110-" *CA3110 is a lost ring 

#02/06/2015
#[1] "2008-S-3-CA1148" *conflicting sex 
#"2013-R-203-CA2756" *conflicting sex in captures is recorded as M
#"2009-S-NA-CA3050" mistnet
#"2010-S-NA-CA3110-" * lost ring

#some of these are still appearing, check cases:
bre.stacked[bre.stacked$code %in% "CA1148",]
bre.stacked[bre.stacked$nest.id %in% "2011-S-6",]
cap.std[cap.std$nest.id %in% "2013-R-203",]


#check why some are missing from BirdRef or Captures
bre.stacked[bre.stacked$chick1 %in% "CA2797",]
bre.stacked[bre.stacked$nest.id %in% "2012-S--18",]
cap.std[cap.std$ring %in% "CA2797",]
cap.std[cap.std$nest.id %in% "2008-S-3",]


#-----------------------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#FINAL CORRECTIONS

#some have mistaken rings...others are nests missing from BirdRef:
#Find which are the nests missing from BirdRef from the new 33 list set
y<-setdiff(ids.cap, ids.bre)
y[order(y)]
y.nests<- substr(y, 1,nchar(y)-7) 
ind<-which(y %in% y[str_detect(y, "NA")]) #omit nests with NA
y<-y[-ind] #new list with no nests with NA: 27 nests to check
y[order(y)]
y.nests<- substr(y, 1,nchar(y)-7) 

y.1<-bre.stacked[bre.stacked$nest.id %in% y.nests,] 
y.1[order(y.1$nest.id),]


y.2<-cap.std[cap.std$nest.id %in% y.nests,]
y.2[order(y.2$nest.id),]


head(bre.stacked)
# #[1]
# bre.stacked[bre.stacked$nest.id %in% "2007-S-5",]
# cap.std[cap.std$nest.id %in% "2007-S-5",]
# cap.std[cap.std$ring %in% "CA1139",]#This female was ringed as XX.OX|XX.XX first in 2007, then in 2008 according to notes the metal ring CA1139 was added. It is unclear if the metal ring present in the file in the capture of 2007 was added in 2008??
# #correction: add note in comments_stdfile
# bre.stacked[892,"comments_stdfile"]<-"This female was ringed as XX.OX|XX.XX first in 2007, then in 2008 according to notes the metal ring CA1139 was added to the bird with that same code. It is unclear if the metal ring present in the file in the capture of this nest was added in 2008??"
# 
# #[2]
# bre.stacked[bre.stacked$nest.id %in% "2008-S--46",]
# cap.std[cap.std$nest.id %in% "2008-S--46",]
# cap.std[cap.std$ring %in% "CA2636",] #juvenile missing in birdref
# #correction: add juvenile to nest
# bre.stacked[bre.stacked$nest.id %in% "2008-S--46","chick2"]<-"CA2636"
# 
# #[3]
# bre.stacked[bre.stacked$nest.id %in% "2008-S-22",]
# cap.std[cap.std$nest.id %in% "2008-S-22",]
# cap.std[cap.std$ring %in% "CA2677",]  #juvenile missing in birdref
# #correction: add juvenile to nest
# bre.stacked[bre.stacked$nest.id %in% "2008-S-22","chick2"]<-"CA2677"
# 
# #[4]skip...this is a recapture that needs verification
# 
# #[5]
# bre.stacked[bre.stacked$nest.id %in% "2009-S-32",]
# cap.std[cap.std$nest.id %in% "2009-S-32",]
# cap.std[cap.std$ring %in% "CA1674",] #not an error....appears in ring2 ,/
# 
# #[6]
# bre.stacked[bre.stacked$nest.id %in% "2010-S--24",]
# cap.std[cap.std$nest.id %in% "2010-S--24",] #two chicks missing in BirdRef
# #correction: add juveniles to nest
# bre.stacked[bre.stacked$nest.id %in% "2010-S--24", "chick1"]<-"CA3730"
# bre.stacked[bre.stacked$nest.id %in% "2010-S--24", "chick2"]<-"CA3731"
# 
# #[7]same as [6]
# 
# #[8]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-11",]
# cap.std[cap.std$nest.id %in% "2010-S-11",] #nest with 4 chicks
# #no correction, this was already in comments_field
# 
# #[9]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-123",]
# cap.std[cap.std$nest.id %in% "2010-S-123",] #ring of male is incorrect in Birdref
# #correction: changed code for male and added note to comments_stdfile
# bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-123-M",c("code","ring2")] <- "CA3037"
# bre.stacked[bre.stacked$nest.id %in% "2010-S-123","comments_stdfile"]<- "This nest's male was captured in BirdRef as CA3083 (that is a female) and is inconsistent with the male captured from this nest, changed ring to CA3037 (which appears in the capture file) (CCI 2015)"
# bre.stacked[bre.stacke$nest.id %in% "2010-S-123-M","mol_sex"]<-"M" #this needs to be changed as mol_sex assignment was done before
# 
# #[10,11]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-205",]
# cap.std[cap.std$nest.id %in% "2010-S-205",] 
# #female captured and in birdref do not coincide....in notes (IF, TS, JMH) I cannot find the ids for this nest
# #correction:nest 2010-S-205 and 2010-S-206 id's were inverted....change
# bre.stacked[bre.stacked$nest.id %in% c("2010-S-205","2010-S-206"),]
# cap.std[cap.std$nest.id %in% c("2010-S-205","2010-S-206"),] 
# bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-205-F",c("code","ring2")]<-"CA3044"
# bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-206-F",c("code","ring2")]<-"CA2740"
# 
# #[12]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-209",]
# cap.std[cap.std$nest.id %in% "2010-S-209",] #juvenile missing
# #correction:add juvenile
# bre.stacked[bre.stacked$nest.id %in% "2010-S-209","chick1"]<-"CA3630"
# 
# #[13]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-306",]
# cap.std[cap.std$nest.id %in% "2010-S-306",] #juvenile missing "CA3719"
# cap.std[cap.std$ring %in% "CA3737",] #this ring was recorded in 2010-S-306, but in captures appears in nest 2010-S-326
# cap.std[cap.std$ring %in% "CA3743",] #this ring was recorded in 2010-S-306, but in captures appears in nest 2010-S-326
# #correction:delete both rings of chicks (CA3737, CA3743) and add CA3719
# bre.stacked[bre.stacked$nest.id %in% "2010-S-306","chick1"]<-"CA3719"
# bre.stacked[bre.stacked$nest.id %in% "2010-S-306","chick2"]<-""
# 
# #[14 and 15]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-307",]
# cap.std[cap.std$nest.id %in% "2010-S-307",] 
# cap.std[cap.std$ring %in% "CA3718",] #this ring was recorded in 2010-S-307, but in captures appears in nest 2010-S-335
# #correction:delete ring CA3718 and add CA3732 and CA3736
# bre.stacked[bre.stacked$nest.id %in% "2010-S-307","chick1"]<-"CA3732"
# bre.stacked[bre.stacked$nest.id %in% "2010-S-307","chick2"]<-"CA3736"
# bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-307-F", "comments_stdfile"]<- "in IF fieldnotes code of female is recorded as MO|OO (CCI 2015)"
# cap.std[cap.std$code %in% "MX.OX|OX.OX",]#in 2010 it was not a duplicate
# 
# #continue......03/06/2015
# #[16]
# bre.stacked[bre.stacked$nest.id %in% "2010-S-326",]
# cap.std[cap.std$nest.id %in% "2010-S-326",] 
# #correction: add missing chick
# bre.stacked[bre.stacked$nest.id %in% "2010-S-326","chick2"] <- "CA3743"
# 
# #[17]
# bre.stacked[bre.stacked$nest.id %in% "2011-S-103",]#female is recorded in BirdRef as CA1161 but in captures as CA1174
# cap.std[cap.std$nest.id %in% "2011-S-103",] 
# cap.std[cap.std$ring %in% "CA1161",]#belongs to duplicate of code...but female was captured for this nest
# cap.std[cap.std$code %in% "MX.RX|GX.RX",]
# #correction: change female's code (rings was confused as colour code is a duplicate)
# bre.stacked[bre.stacked$id.nest.sex %in% "2011-S-103-F",c("ring2","code")] <- "CA1174" #corrected mistake...year in this line was 2010...see below
# 
# #-------------------------------mistake in [17] correction
# bre.stacked[bre.stacked$nest.id %in% "2010-S-103",]
# #bre.stacked[bre.stacked$nest.id %in% "2010-S-103",c("ring2","code")] <- "CA1174" #MISTAKE...correct nest id year should be 2011
# #bre[bre$year ==2010 & bre$site =="S" & bre$nest ==103,] #male = CA2692, female=CA1064
# #bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-103-F",c("ring2","code")] <- "CA1064"
# #bre.stacked[bre.stacked$id.nest.sex %in% "2010-S-103-M",c("ring2","code")] <- "CA2692"
# #----------------------------------------
# 
# #[18 and 19]
# bre.stacked[bre.stacked$nest.id %in% "2012-S--1",]
# cap.std[cap.std$nest.id %in% "2012-S--1",] 
# #correction: chicks missing, add them
# bre.stacked[bre.stacked$nest.id %in% "2012-S--1","chick1"] <- "CA2760"
# bre.stacked[bre.stacked$nest.id %in% "2012-S--1", "chick2"] <- "CA2761"
# 
# #[20]
# bre.stacked[bre.stacked$nest.id %in% "2012-S--101",]
# cap.std[cap.std$nest.id %in% "2012-S--101",] 
# #correction: chicks missing, add them
# bre.stacked[bre.stacked$nest.id %in% "2012-S--101","chick1"] <- "CA2763"
# 
# #[21]
# bre.stacked[bre.stacked$nest.id %in% "2012-S-101",] #correct ring2
# 
# #[22]
# bre.stacked[bre.stacked$nest.id %in% "2012-S-106",]
# cap.std[cap.std$nest.id %in% "2012-S-106",] 
# #correction: sex of both adults trapped is F check if there is mol.sex
# sex[sex$ring %in% c("CA1136","CA2721"),] #both F, they are in list of mol_sex sent to Tess (May2015)
# #bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-M", "code"]<-"MX.OX|BX.BX"
# bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-M", "ring2"] <- "CA1136"
# #bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-F", c("ring2","code")] <- "CA2721"
# bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-M", "mol_sex"] <- "F"
# bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-M", "comments_stdfile"]<-"sex needs to be checked (CCI 2015)"
# bre.stacked[bre.stacked$id.nest.sex %in% "2012-S-106-M", "captured_in_focalyear"]<-"yes"
# 
# #[23]
# bre.stacked[bre.stacked$nest.id %in% "2012-S-17",] #correct ring2
# cap.std[cap.std$nest.id %in% "2012-S-17",] 
# 
# #[24]
# bre.stacked[bre.stacked$nest.id %in% "2013-R-204",]
# cap.std[cap.std$nest.id %in% "2013-R-204",] 
# cap.std[cap.std$ring %in% "CA2746",]
# sex[sex$ring %in% "CA2746",] #it's a male
# #correction: change fieldcomment
# bre.stacked[bre.stacked$nest.id %in% "2013-R-204", "comments_field"]<-"Colour codes read for this nest: MY|WG and MW|YR"
# cap.std[cap.std$code %in% "MX.YX|WX.GX",]
# bre.stacked[bre.stacked$id.nest.sex %in% "2013-R-204-F", "code"]<-"CA3480"
# bre.stacked[bre.stacked$id.nest.sex %in% "2013-R-204-F", "ring"]<-"CA3480"
# bre.stacked[bre.stacked$id.nest.sex %in% "2013-R-204-F", "mol_sex"]<-"F"
# 
# #[25]
# bre.stacked[bre.stacked$nest.id %in% "2013-S--5",]
# cap.std[cap.std$nest.id %in% "2013-S--5",] 
# #correction: add chick
# bre.stacked[bre.stacked$nest.id %in% "2013-S--5","chick1"] <- "CA2800"
# 
# #[26]
# bre.stacked[bre.stacked$nest.id %in% "2013-S--7",]
# cap.std[cap.std$nest.id %in% "2013-S--7",] 
# #correction: add chick
# bre.stacked[bre.stacked$nest.id %in% "2013-S--7","chick2"] <- "CA4097"
# 
# #[27]
# bre.stacked[bre.stacked$nest.id %in% "2013-S-27",]
# cap.std[cap.std$nest.id %in% "2013-S-27",] 
# #correction: add chick
# bre.stacked[bre.stacked$nest.id %in% "2013-S-27","chick1"] <- "CA3857"
# 

#--------------------03/06/2015

#Decide if code/ring or ring2 will be used
head(bre.stacked[!is.na(bre.stacked$code) & bre.stacked$ring != bre.stacked$code | !is.na(bre.stacked$code) & bre.stacked$code != bre.stacked$ring2,])
names(bre.stacked)
code.ring.notmatch<-bre.stacked[!is.na(bre.stacked$code) & bre.stacked$ring != bre.stacked$code, c(1:4,10,12,14)]
code.ring2.notmatch<-bre.stacked[!is.na(bre.stacked$code) & bre.stacked$code != bre.stacked$ring2, c(1:4,10,12,14)]
ring.ring2.notmatch<-bre.stacked[!is.na(bre.stacked$ring) & bre.stacked$ring != bre.stacked$ring2, c(1:4,10,12,13,14)]

code.ring.notmatch[grep(pattern = "CA([0-9]{4})", code.ring.notmatch$code, perl=TRUE),]
#       year site nest   code        ring  ring2 captured_in_focalyear
# 451  2010    S  123 CA3037      CA3083 CA3037                   yes *3083 was wrong, corrected
# 1348 2010    S  205 CA3044      CA2740 CA3044                   yes *correction
# 1349 2010    S  206 CA2740      CA3629 CA2740                   yes *correction
# 1369 2011    S  103 CA1174      CA1161 CA1174                   yes*this was corrected as CA1174 was captured for this nest (confusion is because CA1174 and CA1161 have duplicated codes)

    
code.ring2.notmatch[grep(pattern = "CA([0-9]{4})", code.ring2.notmatch$code, perl=TRUE),]
#none

ring.ring2.notmatch[grep(pattern = "CA([0-9]{4})", ring.ring2.notmatch$code, perl=TRUE),]
#       year site nest   code        ring  ring2
# 451  2010    S  123 CA3037      CA3083 CA3037*correction
# 1348 2010    S  205 CA3044      CA2740 CA3044*correction
# 1349 2010    S  206 CA2740      CA3629 CA2740*correction
# 1369 2011    S  103 CA1174      CA1161 CA1174*correction

  
  bre.stacked[nchar(bre.stacked$code)==11,c("nest.id","code","ring2","captured_focalyear")]
  #all codes in code have a metal ring in ring2 only when they were captured.
  
  #previously this error was found and corrected:
  #719    2013-R-203 XX.XX|XX.XX XX.XX|XX.XX                   yes?? why was this captured and does not have a ring?: in captures is recorded as M
  bre.stacked[bre.stacked$nest.id =="2013-R-203",]
  cap.std[cap.std$nest.id=="2013-R-203",]
  cap.std[cap.std$ring =="CA2756",] #in captures is recorded as Male only for this year no mol sex available
#---------------------------------------------  
# #correction: change captured_in_focalyear to yes in CA2756, add comment about sex
# bre.stacked[bre.stacked$id.nest.sex =="2013-R-203-F","captured_in_focalyear"]<-"yes"
# bre.stacked[bre.stacked$id.nest.sex =="2013-R-203-F","comments_stdfile"]<-"in captures it is sexed as M but in all previous captures it was a F"
# bre.stacked[bre.stacked$id.nest.sex =="2013-R-203-M","captured_in_focalyear"]<-"no"

#---------------------------------------------  

#use of ring or ring2 will depend if individual was captured or not  
# bre.stacked[!is.na(bre.stacked$code) & bre.stacked$captured_in_focalyear == "no", c(1:4,10,12,14)]
# 
#   for (i in 1:length(bre.stacked$year)){
#     if (bre.stacked$captured_in_focalyear[i] =="no"){
#       bre.stacked$ringtouse[i] <- bre.stacked$code [i]}
#     else {
#       bre.stacked$ringtouse[i] <- bre.stacked$ring2[i]}
#   }
# bre.stacked[bre.stacked$ringtouse != bre.stacked$ring2,] #ring2 and ringtouse are equal!
# 
#bre.stacked[!is.na(bre.stacked$ringtouse), c("nest.id","code","ring2","ringtouse","captured_in_focalyear")]
#Use ring2
names(bre.stacked)
bre.st.unst <- bre.stacked[,c(1:3,16,5:12,18)]
head(bre.st.unst)
head(bre.stacked)
#prepare dataset to unstack
freq<-as.data.frame(table(bre.st.unst$nest.id))
freq[freq$Freq>2 | freq$Freq<2,] #all nests should have 2 records (1 for male, 1 for female)
# Var1 Freq
# 80    2008-S--36    4 * make corrections (below) and get rid of extra records
# 229    2009-S-NA   10 * no correction
# 412   2010-S-312    1 *nest added in corrections
# 414   2010-S-326    1*nest added in corrections
# 416   2010-S-335    1*nest added in corrections
# 501     2011-S-6    1*nest added in corrections
# 585   2012-S-221    1*nest added in corrections
# 595   2012-S-301    1*nest added in corrections
# 625 2012-TS--304    1*nest added in corrections
# 760    2014-C--1    1*nest added in corrections
# 773    2014-R--1    1*nest added in corrections

# #---------------error found:
# bre.stacked[bre.stacked$nest.id =="2010-S-312",] #an extra chick appears! CA3743?? / CORRECTED and CORRECTED BUG
# bre[bre$nest =="312",]
# cap.std[cap.std$nest.id =="2010-S-312",] 
# cap.std[cap.std$ring =="CA3743",] #this chick belongs to 2010-S-326 according to captures
# bre.stacked[bre.stacked$nest.id =="2010-S-326",]
# #----------------corrections:
# #correct nest: 2008-S--36
# bre.stacked[bre.stacked$nest.id =="2008-S--36",]
# cap.std[cap.std$nest.id =="2008-S--36",] #in comments: parents BX.MX|BX.GX (already in BirdRef) AND MX.OX|YX.YX (not in BirdRef!) 
# 
# bre.stacked[bre.stacked$id.nest.sex =="2008-S--36-F",c("code","ring2")]<- "MX.OX|YX.YX" 
# #bre.stacked <- bre.stacked[-c(86,958),] #omit repeated cases with no chick
# bre.stacked[bre.stacked$id.nest.sex =="2008-S--36-F",]
# #---------------------------

#Unstack rows
names(bre.stacked)
tail(bre.stacked)
bre.st.unst <- bre.stacked[,c(1:3,16,5:12,18)]
head(bre.st.unst)


set.duplicate <- bre.st.unst[is.na(bre.st.unst$field_sex),]
set.duplicate.f <- set.duplicate
set.duplicate.f$field_sex <- "F"

set.duplicate.m <- set.duplicate
set.duplicate.m$field_sex <- "M"

bre.st.unst[bre.st.unst$nest.id %in% set.duplicate$nest.id,]
bre.st.unst <- bre.st.unst[!bre.st.unst$nest.id %in% set.duplicate$nest.id,]#omit those in set.duplicate to add sex

bre.st.unst.dupl<-rbind(bre.st.unst, set.duplicate.f,set.duplicate.m)

#------------debugging 04/06/2015: change sex of one female in nest 2011-S-4 which appears as duplicate:
# ind<-which(bre.st.unst.dupl$nest.id =="2011-S-4") #1747,1748 (row number)
# bre.st.unst.dupl[1747, "field_sex"]<- "M"
# bre.st.unst.dupl[1747, "mol_sex"]<- NA
# bre.st.unst.dupl[1747, "captured_in_focalyear"]<- "no"
# bre.st.unst.dupl[1747, "ring2"]<- NA
# bre.st.unst.dupl[1747:1748,]
# #------------------------------------------------------

str(bre.st.unst) #2042
str(bre.st.unst.dupl)#1761
#bre.st.unst.dupl$nest.id[bre.st.unst.dupl$nest.id=="2011-S-4"] #1761
unique(bre.st.unst.dupl$nest.id) #878
bre.st.unst.dupl[1750:1761,]

table(bre.st.unst.dupl$field_sex) #F 879 M 882 =1761
males.unst <- bre.st.unst.dupl[bre.st.unst.dupl$field_sex %in%"M",]
str(males.unst) #882


females.unst <- bre.st.unst.dupl[bre.st.unst.dupl$field_sex %in% "F", c(4,8,9,10,11,12,13)]
str(females.unst)#879
females.unst$nest.id
#change colnames of females:
colnames(females.unst)<-c("ring2_fem","field_sex_fem","mol_sex_fem","captured_focalyear_fem","comments_field_fem","comments_stdfile_fem","nest.id")



#MERGE ALL MALES AND FEMALES
unstacked.bre <- merge(females.unst,males.unst, by="nest.id", all.x=T, all.y=T)
head(unstacked.bre)
as.data.frame(table(unstacked.bre$nest.id))
str(unstacked.bre) #1111
unique(unstacked.bre$nest.id) #1112 

#-delete duplicated cases:
unstacked.bre[unstacked.bre$nest.id %in% "2009-S-NA",] #delete the ones repeated and F
delete<-unstacked.bre[229:328,]

unstacked.bre<-unstacked.bre[-c(229:328),]
unique(unstacked.bre$nest.id)#1111 2011-S-4 is there

unstacked.bre[unstacked.bre$nest.id %in% "2009-S-NA",]


#merge columns that are equal:
names(unstacked.bre)
unstacked.bre<-unstacked.bre[,c(1, 8:11,2,12:19,4,7,6,3,5)]

#unstacked.bre2<-unstacked.bre[,c(1, 8:11,2,12:15,3,17,5,16,4,19,7,18,6)]
head(unstacked.bre)
  
#   #-------------debugging 04/06/2015
#   #look up 2011-S-4
#   unstacked.bre[unstacked.bre$nest.id =="2011-S-4",]
#   unstacked.bre2[unstacked.bre$nest.id =="2011-S-4",] #appears but year,site and nest appear as NA!
# 
#       #doesn't appear in this unstacked.bre version
#   #------------------------------------------------------------

colnames(unstacked.bre)<-c("nest.id", "year","site","nest","male","female","chick1","chick2","chick3","field_sex_m","mol_sex_m","captured_focalyear_m","com_field_m","com_stdfile_m","mol_sex_f","com_stdfile_f","com_field_fem","field_Sex_f","captured_focalyear_f")

b<-unstacked.bre[unstacked.bre$com_field_m==unstacked.bre$com_field_f, 9:19]
tail(b)
b[!is.na(b$com_field_m),]

unstacked.bre$comments_field<-ifelse(unstacked.bre$com_field_m == unstacked.bre$com_field_f,
                                     unstacked.bre$com_field_m,
                                     paste(unstacked.bre$com_field_m, unstacked.bre$com_field_f, sep=", "))

b<-unstacked.bre[unstacked.bre$com_field_m!=unstacked.bre$com_field_f, 9:10]
tail(b)
b[!is.na(b$com_field_m),]

str(unstacked.bre)
unstacked.bre$comments_stdfile<- "NA"
unstacked.bre$comments_stdfile<-ifelse(unstacked.bre$com_stdfile_m == unstacked.bre$com_stdfile_f,
                                       unstacked.bre$com_stdfile_m,
                                       paste(unstacked.bre$com_stdfile_m, unstacked.bre$com_stdfile_f, sep=", "))

b<-unstacked.bre[unstacked.bre$com_stdfile_m%in%unstacked.bre$com_stdfile_f, c(9:10,20)]
tail(b)
b[!is.na(b$comments_stdfile),]

# library(stringr)
# unstacked.bre[str_detect(unstacked.bre$comments_stdfile, "this"),]
# 
# #spelling error correction:
# unstacked.bre$comments_stdfile <- str_replace(unstacked.bre$comments_stdfile, "this","these")


#check whether all nests from Nests file appear in BirdRef
names(nes)
nes1<-nes.std
bre1<-unstacked.bre

nes1$nest.id<-paste(nes1$year, nes1$site, nes1$nest, sep="-")
bre1$nest.id<-paste(bre1$year, bre1$site, bre1$nest, sep="-")

nes1$nest.id #607
unique(nes1$nest.id) #607

nests<-unique(nes1$nest.id)
nests.br <- unique(bre1$nest.id)

missingfrombr<-setdiff(nests, nests.br) #35 nests missing from birdref

#1st run
#nests from 2007,2008 and 2009 are in notes from Pedrín and Araceli which are unavailable (04/06/2015)
# [1] "2007-S-7"                "2007-S-10"               "2007-S-11"              
# [4] "2007-S-14"               "2007-S-15"               "2007-S-16"              
# [7] "2007-S-24"               "2008-PP-4"               "2008-PP-5"              
# [10] "2008-PP-6"               "2008-PP-7"               "2008-PP-8"              
# [13] "2008-PP-9"               "2008-S-4"                "2008-S-5"               
# [16] "2008-S-9"                "2008-S-14"               "2008-S-17"              
# [19] "2009-Lagoa Cimidor-1"    "2009-Lagoa Cimidor-2"    "2009-Lagoa Cimidor-3"   
# [22] "2009-Ribeira do Lagoa-1" "2009-Ribera do Joao-1"   "2009-S-35"              
# [25] "2009-S-130"              "2009-S-132"              "2009-S-300"             
# [28] "2009-S-305"              "2009-S-306"              "2009-S-307"             
# [31] "2009-S-309"              "2010-Morro-1" *appears as M-1           "2011-S-4"  *fixed bug...it now appears             
# [34] "2011-S-200"              "2011-St. Antonio-101"   

#after detecting bug:
# [1] "2007-S-7"                "2007-S-10"               "2007-S-11"              
# [4] "2007-S-14"               "2007-S-15"               "2007-S-16"              
# [7] "2007-S-24"               "2008-PP-4"               "2008-PP-5"              
# [10] "2008-PP-6"               "2008-PP-7"               "2008-PP-8"              
# [13] "2008-PP-9"               "2008-S-4"                "2008-S-5"               
# [16] "2008-S-9"                "2008-S-14"               "2008-S-17"              
# [19] "2009-Lagoa Cimidor-1"    "2009-Lagoa Cimidor-2"    "2009-Lagoa Cimidor-3"   
# [22] "2009-Ribeira do Lagoa-1" "2009-Ribera do Joao-1"   "2009-S-35"              
# [25] "2009-S-130"              "2009-S-132"              "2009-S-300"             
# [28] "2009-S-305"              "2009-S-306"              "2009-S-307"             
# [31] "2009-S-309"              "2010-Morro-1"            "2011-S-200"**nest doesn't have parent's id's, no chicks             
# [34] "2011-St. Antonio-101"*nest doesn't have parent's id's, no chicks hatched 
nes.std[nes.std$year ==2011 & nes.std$nest==200,]

#add missing nests of 2011 with no record of ring for adults:
# #add 2011-St. Antonio-101
# str(bre1) #887
# names.list<-as.list(names(bre1))
# str(names.list)
# edit(bre1)
# 
# bre1[887:889,]
# bre1[888, c("nest.id","year","site","nest","comments_stdfile")] <- c("2011-S-200","2011","S","200","no captures for this nest, no ids recorded")
# bre1[889, c("nest.id","year","site","nest","comments_stdfile")] <- c("2011-St. Antonio-101","2011","St. Antonio","101","no captures for this nest, no ids recorded")

#------------check nests from 2011 - check bug
#nest 2011-S-4 should be in file as it was added previously...check in which step it disappears
#cap.std[cap.std$nest.id =="2011-S-4",]
#bre1[bre1$nest.id =="2011-S-4",] #does not appear anymore
#bre.stacked[bre.stacked$nest.id =="2011-S-4",] #does appear and as duplicate
#unstacked.bre[unstacked.bre$year ==2011 & unstacked.bre$site=="S" & unstacked.bre$nest ==4,] #does not appear

#bre.st.unst[bre.st.unst$nest.id =="2011-S-4",] #appears as duplicate
#bre.st.unst.dupl[bre.st.unst.dupl$nest.id =="2011-S-4" & bre.st.unst.dupl$field_sex %in% "F", ]#appears as duplicate
#bre.st.unst.dupl[1747:1761,] #disappears when spliting between males and females
#females.unst[females.unst$nest.id =="2011-S-4",] #it appears...year,site and nest are blank in the merging step as this nest doesn't have a Male

#-----------------------

missingfromn<-setdiff(nests.br,nests) #should be only negative nests...however some non negative nests appear
missingfromn[-grep(pattern="--",missingfromn)]#16 non-negative nests missing from nests file
# [1] "2008-S-100" *no info on this nests in notes
# [2] "2009-B-1" *no info on this nest in notes    
# [3] "2009-C-1" *no info on this nest in notes  
# [4] "2009-C-2" *no info on this nest in notes
# [5] "2009-C-3" *no info on this nest in notes    
# [6] "2010-M-1" *no info on this nest in notes
# [7] "2010-S-305" *errased from notes
# [8] "2010-S-306" *errased from notes
# [9] "2010-S-307" *errased from notes
# [10] "2010-S-312" *errased from notes
# [11] "2010-S-326" *errased from notes
# [12] "2010-S-335" *errased from notes  
# [13] "2012-S-221" *no info on this nest in notes
# [14] "2012-S-301" *no info on this nest in notes
# [15] "2013-S-207" *this nest doesn't exist delete

# 
# bre1[bre1$nest.id %in% c("2012-S-301"),]
# nes1[nes1$nest.id %in% c("2010-Morro-1"),]
# 
# #delete 2013-S-207 since it doesn't exist:
# ind<-which(bre1$nest.id %in% "2013-S-207")
# bre1<-bre1[-ind,]


#choose columns to write on csv file:
names(bre1)


to.write <- bre1#[,c(2:9,14:21)]
#to.write[to.write$year ==2011 & to.write$site =="S" & to.write$nest==4,]
#------------------------------------------------------------------------------------------
#-------------------------------------Write BirdRef_std------------------------------------
#------------------------------------------------------------------------------------------

setwd("F:/Plovers/KP data management/Maintenance/Cleaning data code/Applying code to populations/Maio 2007-2015/output")

write.csv(to.write, "BirdRef_Maio_2009-2015_stdfile21Jan2016.csv")


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
