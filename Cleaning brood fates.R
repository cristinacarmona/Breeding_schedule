#17/03/2016 Clean Madagascar bird reference and produce compatible file to that of Maio and Ceuta
#Author: Cristina Carmona-Isunza

#1st log 17/03/2016 Create code using "Cleaning Resightings" code as base,
                    #found unmatching rings between broodfates and captures....
#2nd log 18/03/2016 Checking list of nests found with non-matching rings in captures or birdRef
#3rd log 21/03/20126 fixed duplicate search, produced list of 52 nests that are problematic 
#                   Decided to flag these nests as poor quality brood care, and still include them in bschedule data

#------------------------------------------------------------------------
#Madagascar
setwd("F:/Plovers/3rd Chapter/input/Madagascar")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles


import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))


#str(import.list)
ls()

working.list <- import.list
names(working.list) <- c("bf","br","cap","sex","sex2","ne","re2","re")

#re-load captures file
# choose.cap<-csvfiles[3]
# import.list2 <- lapply(choose.cap, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))
# work.l<-import.list2
# names(work.l)<-c("cap.raw")

attach(working.list)
attach(work.l)
#-------------------------------------------------------------------------

#Resightings:
#clean code rings
bfa<-bf

names(bfa)
more.11chr <- bfa[nchar(bfa$parent1)!=7 | nchar(bfa$parent2)!=7, c("year","site","date","parent1","parent2","observer","comments")] #15 wrong formatted codes
str(more.11chr[order(more.11chr$parent1),]) #1314 obs

#Fix resightings of codes in wrong format e.g. WRML
#nchar(more.11chr$parent1[41])
fix.code.p1<-more.11chr[nchar(more.11chr$parent1)==4,"parent1"]
fix.code.p2<-more.11chr[nchar(more.11chr$parent2)==4, "parent2"]

table(bfa$year, useNA="ifany")

for(i in 1:length(bfa$year)){ #for parent 1
  if(bfa$parent1[i] %in% fix.code.p1){
    bfa$new.code.p1[i] <- paste(substr(bfa$parent1[i], 1,1),".", 
                             substr(bfa$parent1[i], 2,2), "|",
                             substr(bfa$parent1[i], 3,3), ".",
                             substr(bfa$parent1[i], 4,4), sep="")
  }else{
    bfa$new.code.p1[i] <- bfa$parent1[i]
  }
}


for(i in 1:length(bfa$year)){ #for parent 2
  if(bfa$parent2[i] %in% fix.code.p2){
    bfa$new.code.p2[i] <- paste(substr(bfa$parent2[i], 1,1),".", 
                                substr(bfa$parent2[i], 2,2), "|",
                                substr(bfa$parent2[i], 3,3), ".",
                                substr(bfa$parent2[i], 4,4), sep="")
  }else{
    bfa$new.code.p2[i] <- bfa$parent2[i]
  }
}

#--------------develop
substr(more.11chr$code[41], 2,2)
paste(substr(more.11chr$code[41], 1,1),".", 
      substr(more.11chr$code[41], 2,2), "|",
      substr(more.11chr$code[41], 3,3), ".",
      substr(more.11chr$code[41], 4,4), sep="")

#--------------------------debug
res[nchar(res$new.code)!=7, c("year","site","sex","date","time","new.code","observer","comment")] #15 wrong formatted codes
more.11chr[order(more.11chr$code),]

#--------------substitute new.code-----------------
bfa$parent1 <- bfa$new.code.p1
bfa$parent2 <- bfa$new.code.p2 

      #check other errors:
more.11chr <- bfa[nchar(bfa$parent1)!=7 | nchar(bfa$parent2)!=7, c("year","site","date","parent1","parent2","observer","comments")] #15 wrong formatted codes
str(more.11chr[order(more.11chr$parent1),]) #1314 obs, 1228 after correction

  #subset data to Andavadoaka
table(bfa$site)
bfa <- bfa[bfa$site %in% "Andavadoaka",]

#check again other errors:
more.11chr <- bfa[nchar(bfa$parent1)!=7 | nchar(bfa$parent2)!=7, c("year","site","date","parent1","parent2","observer","comments")] #15 wrong formatted codes
str(more.11chr[order(more.11chr$parent1),]) #1314 obs, 1228 after correction...793 after subsetting only to Andavadoaka
table(more.11chr$parent1)
table(more.11chr$parent2)

bfa[bfa$parent1 %in% c("?","No","UNIDENTIFIED","UB","UNK","UR"), "parent1"] <- NA
bfa[bfa$parent2 %in% c("?","??","No","NO","UB","UNK","UR"), "parent2"] <- NA


####################################################################
#Regular expressions for codes with g or 0 
#regexp <- "[g]X" #finds g before | or after |
regexp<-"[0]"
ind.g<- grep(pattern = regexp, bfa$parent1, perl=TRUE)
ind.g2<- grep(pattern = regexp, bfa$parent2, perl=TRUE)

gs<-bfa[ind.g,]
gs2<-bfa[ind.g2,]
#NOTHING TO CHANGE, no zeroes in codes
#CHANGES TO FILE TO CREATE STD FILE-----------------------------------------------------------

#Replace g with L; or 0 with O
# res[res$code %in% gs$code,]
# res$code[res$code %in% gs$code] <- gsub("0", "O", res$code[res$code %in% gs$code])

# gs<-res[ind.g,] #changes should be seen on same set of codes 
# res[c(4350,4351), "code"] <- gsub("O", "0", res$code[c(4350,4351)])

#############################################################################


#Regular expressions for codes with !
regexp <- "[!]"
ind.exc <- grep(pattern=regexp, bfa$parent1, perl=T)
ind.exc2 <- grep(pattern=regexp, bfa$parent2, perl=T)

exc <- bfa[ind.exc,]
exc2 <- bfa[ind.exc2,]
#NO ! MARKS IN CODES

#CHANGES TO FILE TO CREATE STD FILE -------------------------------------------------------

#replace ! with |
# res$code[res$code %in% exc$code] <- gsub("!", "\\|", res$code[res$code %in% exc$code])
# exc <- res[ind.exc,]

##############################################################################

#Regular expression for spaces
regexp <- "([A-Z]+)([\\s]+)|([\\s]+)([A-Z]+)|([\\s]+)([\\-]+)"
ind.blank <- grep(pattern=regexp, bfa$parent1, perl=T)
ind.blank2<-grep(pattern=regexp, bfa$parent2, perl=T)

bfa[ind.blank,]
bfa[ind.blank2,]

#CHANGES TO FILE------------------------------------------------------
#get rid of blank spaces
bfa$parent1 <- gsub("\\s", "", bfa$parent1)
bfa$parent2 <- gsub("\\s", "", bfa$parent2)

regexp <- "([A-Z]+)([\\s]+)|([\\s]+)([A-Z]+)|([\\s]+)([\\-]+)"
ind.blank <- grep(pattern=regexp, bfa$parent1, perl=T)
ind.blank2<-grep(pattern=regexp, bfa$parent2, perl=T)

bfa[ind.blank,]
bfa[ind.blank2,]
#################################################################################
#Replace commas with dots

#regexp to find commas
regexp <- "([,]+)"
ind.comma <- grep(pattern=regexp, bfa$parent1, perl=T) 
ind.comma2 <- grep(pattern=regexp, bfa$parent2, perl=T)

bfa[ind.comma,]
bfa[ind.comma2,]

#changes to file----------------------------
#get rid of comma
#res$code[res$code %in% commas$code] <- gsub(",", ".", res$code[res$code %in% commas$code])

#-----------------------------------
#check again
more.11chr <- bfa[nchar(bfa$parent1)!=7 | nchar(bfa$parent2)!=7, c("year","site","date","parent1","parent2","observer","comments")] #15 wrong formatted codes
str(more.11chr[order(more.11chr$parent1),]) #1314 obs, 1228 after correction...793 after subsetting only to Andavadoaka
table(more.11chr$parent1)
table(more.11chr$parent2)

table(bfa$parent1)
table(bfa$parent2)
#-----------------------------------
# ##############################################################################
# 
# #Turn codes into rings

#a) find duplicates in Mad so rings can be replaced in brood fates:
#cap<-cap.raw #when re-loading raw captures file
cap<-cap[cap$site %in% "Andavadoaka",] #correct Issue 2....needs to be applied in Extract_breedingschedule code ,/ done 21/03/2016
table(cap$species)
table(cap$species)
cap$sp.code.ring <- paste(cap$species,"-", cap$code,"-", cap$ring, sep="")
cap$sp.sex.code <- paste(cap$species,"_", cap$code, "-",cap$sex, sep="")

#a) Duplicate in MAIO only
#dupl <- cap[which(cap$comments_stdfile =="duplicate"),c("code","ring")]

#   #b) list duplicates from Ceuta/Mad
# 
# library(stringr)
#unique.code.ring<- unique(cap$code.ring)
unique.code.ring<- unique(cap$sp.code.ring[cap$site %in% "Andavadoaka"]) #3017 unique sp.code.ring

lookdup <-strsplit(unique.code.ring, "-")

library(plyr)
ldup <- ldply(lookdup) #turn list into two columns
#colnames(ldup) <- c("code","ring")
colnames(ldup) <- c("sp","code","ring")  
# 
#-------debug-----
head(ldup[order(ldup$ring),], n=1300)
#----------------

#Get rid of ambiguous codes to delete duplicates:
pat0<-"X"

x0 <- ldup[!str_detect(ldup$code, pattern=pat0),] #allow no Xs in codes (MAD)

str(x) #CEUTA: 529; 555; 530
#Mad: 3015

str(x0) #1787
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
#               #MAD: 291
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
str(dupl)

#---------------------------------------------------------------------------
#previous way of dealing with duplicates (includes ambiguous codes)
# cap$sp.code.ring <- paste(cap$species,"_", cap$code,"-", cap$ring, sep="")
# cap$sp.sex.code <- paste(cap$species,"_", cap$code, "-",cap$sex, sep="")
# unique.code.ring<- unique(cap$sp.code.ring) #4396 unique sp.code.ring, 3017 only ]Andavadoaka
# 
# lookdup <-strsplit(unique.code.ring, "-")
# 
# library(plyr)
# ldup <- ldply(lookdup) #turn list into two columns
# #colnames(ldup) <- c("code","ring")
# colnames(ldup) <- c("sp_code","ring")  
# 
# 
# ind <- which(duplicated(ldup$sp_code) | duplicated(ldup$sp_code, fromLast=TRUE))#MAD
# str(ldup[ind,]) #2595 duplicates?? including XX, 1278 only Andavadoaka
# str(ldup[-ind,]) #1801 non duplicates; 1739 only Andavadoaka
# 
# x1 <- ldup[ind,]
# x1[order(x1$sp_code),]
# # 
# dupl <- x1 #List of rings and codes with duplicated codes 
# str(dupl) #36 Ceuta, 2592 in Mad including Juv
# dupl$sp.code.ring <- paste(dupl$sp_code, dupl$ring, sep="-")
#-----------------------------------------------------------------------------

#----------
sp_code.dupl<-unique(dupl$sp.code) #517; 170 only Andavadoaka; 95 omitting ambiguous


#b) omit duplicates from captures-------
names(cap)
str(cap) #5372, 3572 only Andavadoaka

cap.nodupl <- cap[!cap$sp.code.ring %in% dupl$sp.code.ring,]
str(cap.nodupl) #1985 only andavadoaka

cap.nodupl$sp.code <- paste(cap.nodupl$species, cap.nodupl$code, sep="-")

#----------

#c) Lookup individuals with no rings and only code in parent1 and parent2------------
letters<-"^[FH0-9]*$"
ind.rings<-grep(pattern=letters, bfa$parent1, perl=T)
ind.rings2<-grep(pattern=letters, bfa$parent2, perl=T)
bfa[ind.rings,"parent1"]
bfa[-ind.rings, "parent1"]

bfa[ind.rings2, "parent2"]
bfa[-ind.rings2, "parent2"]

#d) Replace code with ring only on those where no ring was present already in parent1 and parent2---------
#bfa2<-bfa
bfa<-bfa2

table(bfa$species)
bfa[bfa$species %in% "KIP","species"] <- "KiP"
bfa[bfa$species %in% "WFP", "species"] <- "WfP"

bfa$sp.code1<-paste(bfa$species, bfa$parent1, sep="-")
bfa$sp.code2<-paste(bfa$species, bfa$parent2, sep="-")


for(i in 1:length(bfa$year)){ #modification for MAD, search for rings of individuals with code but no ring
  if(!is.na(bfa$parent1[i]) & !grepl(pattern=letters, bfa$parent1[i], perl=T)){
    bfa$parent1[i] <- cap.nodupl$ring[match(bfa$sp.code1[i], cap.nodupl$sp.code)]
  }
  if(!is.na(bfa$parent2[i]) & !grepl(pattern=letters, bfa$parent2[i], perl=T)){
    bfa$parent2[i] <- cap.nodupl$ring[match(bfa$sp.code2[i], cap.nodupl$sp.code)]
  }
}

#-------debug
names(bfa)
bfa$nest.id <- paste(bfa$year, bfa$species, bfa$brood, sep="-")
bfa[-ind.rings2, c("nest.id","parent2","sp.code2")]
bfa[-ind.rings, c("nest.id", "parent1","sp.code1")]

bfa[bfa$nest.id %in% "2014-WfP-112",] #this nest has wrong parent2 code...in captures it is B.R|R.M, Issue3
 #in this example both parents are present
#--------------------------------------------
#count where both parents are present

for(i in 1:length(bfa$year)){
  if(!is.na(bfa$parent1[i]) & !is.na(bfa$parent2[i])){
    bfa$parent[i] <- 4
  }else{
    bfa$parent[i] <- NA
  }
}

bfa[bfa$parent %in% "4",]
bfa[!bfa$parent %in% "4",]


#-----------------------------check issue 3------------------------
#omit for this part nests in captures or birdref which are not in brood fates/
#ids cannot be checked from these
cap$nest.id <- paste(cap$year, cap$species, cap$nest, sep="-")
cap.nests <- unique(cap$nest.id)

bf.nests<- unique(bfa$nest.id)

ids.check1<-intersect(cap.nests, bf.nests) #506 nests

br.nests<-unique(ids.final$nest.id)
ids.check2<-intersect(bf.nests, br.nests)#358

ids.check<-bf.nests[bf.nests%in%ids.check1 | bf.nests %in% ids.check2]#510

#check that parents new rings match the nest in captures or BirdRef

bfa$nestid.ring1<-paste(bfa$nest.id, bfa$parent1, sep="_")
bfa$nestid.ring2<-paste(bfa$nest.id, bfa$parent2, sep="_")

nestid.ringall1<-bfa[!is.na(bfa$parent1) & bfa$nest.id %in% ids.check, "nestid.ring1"]
nestid.ringall2<-bfa[!is.na(bfa$parent2) & bfa$nest.id %in% ids.check, "nestid.ring2"]
nestid.ring.unique.bfa<-c(unique(nestid.ringall1), unique(nestid.ringall2)) #471 list of unique nests with rings associated to it in brood fates

#add ids from birdreference as some birds were not captured but id was known
#[used as parallel code from extract_breeding schedule to use ids.final]
ids.final$nest.id.ring <-paste(ids.final$nest.id, ids.final$ring, sep="_")#from extract_breedingschedule code up to line 950
ids.final$nest.id.code <-paste(ids.final$nest.id, ids.final$code, sep="-")
nest.idring.bref<-ids.final$nest.id.ring[!is.na(ids.final$ring)
                                     & ids.final$nest.id %in% ids.check]#428
nest.idcode.bref<-ids.final$nest.id.code[is.na(ids.final$ring) & !is.na(ids.final$code) &
                                           ids.final$nest.id %in% ids.check] #50
unique(nest.idcode.bref) #50 same

#add ids from captures
cap.nodupl$nest.id <- paste(cap.nodupl$year, cap.nodupl$species, cap.nodupl$nest, sep="-")
cap.nodupl<-cap.nodupl[cap.nodupl$age %in% "A" & cap.nodupl$nest.id %in% ids.check,]
cap.nodupl$nest.id.ring<-paste(cap.nodupl$nest.id, cap.nodupl$ring, sep="_")
cap.nodupl$nest.id.code <-paste(cap.nodupl$nest.id, cap.nodupl$code, sep="-")

nestidring.cap<-cap.nodupl$nest.id.ring[!is.na(cap.nodupl$ring) 
                                        & cap.nodupl$nest.id %in% ids.check]#452
nest.id.ring.cap<-unique(nestidring.cap)#440

nest.id.code.cap<-cap.nodupl$nest.id.code[!is.na(cap.nodupl$code) &
                                            is.na(cap.nodupl$ring) &
                                            cap.nodupl$nest.id %in% ids.check]#50

ids.cap_bref1<-c(nest.idcode.bref, nest.idring.bref, nest.id.ring.cap,
                    nest.id.code.cap)
ids.cap_bref<-unique(ids.cap_bref1) #536



setdiff(ids.cap_bref, nestid.ring.unique.bfa) #145 in cap and bref but not in bfa (many NAs), 

setdiff(nestid.ring.unique.bfa, ids.cap_bref) #67 in bfa but not in cap or bref
#61 with new duplicates assignment

# [1] "2013-KiP--108-FH68899"
# [2] "2013-KiP--107-FH69285"
# [3] "2013-KiP--102-FH69278"
# [4] "2013-KiP--62-FH69234" 
# [5] "2013-KiP--35-FH72732" 
# [6] "2013-KiP--28-FH72383" 
# [7] "2013-KiP--19-FH47538" 
# [8] "2013-KiP--9-FH47466"  
# [9] "2013-KiP-25-FH47801"  
# [10] "2013-KiP-36-FH68833"  
# [11] "2013-KiP-124-FH72239" 
# [12] "2013-KiP-131-FH69071" 
# [13] "2013-KiP-157-FH47352" 
# [14] "2013-KiP-219-FH72713" 
# [15] "2013-MP--102-FH17826" 
# [16] "2013-MP--102-FH68762" 
# [17] "2013-MP--102-FH73211" 
# [18] "2013-MP-1-FH17803"    
# [19] "2013-MP-1-FH47521"    
# [20] "2013-WfP--103-FH68802"
# [21] "2013-WfP--102-FH47251"
# [22] "2013-WfP--1-FH69296"  
# [23] "2013-WfP-5-FH69256"   
# [24] "2013-WfP-44-FH72619"  
# [25] "2014-KiP-103-FH69257" 
# [26] "2014-KiP--107-FH72351"
# [27] "2014-KiP--114-FH72151"
# [28] "2014-KiP--108-FH68981"
# [29] "2014-KiP-124-FH73026" 
# [30] "2014-WfP--108-FH47227"
# [31] "2014-WfP--108-FH69253"
# [32] "2014-MP-108-FH48000"  
# [33] "2014-KiP--3-FH72739"  
# [34] "2014-WfP--8-FH69188"  
# [35] "2014-WfP--8-FH69034"  
# [36] "2014-KiP--23-FH72885" 
# [37] "2014-KiP--27-FH73033" 
# [38] "2014-MP--4-FH17875"   
# [39] "2014-KiP-323-FH69204" 
# [40] "2015-KiP-108-FH69255" 
# [41] "2015-KiP-206-FH772320"
# [42] "2015-KiP-302-FH73086" 
# [43] "2015-KiP-104-FH72227" 
# [44] "2015-KiP-509-FH73458" 
# [45] "2015-KiP-622-FH72941" 
# [46] "2015-KiP--7-FH69003"  
# [47] "2015-KiP-603-FH47190" 
# [48] "2015-WfP--2-FH69247"  
# [49] "2013-MP--101-FH72680" 
# [50] "2013-MP-210-FH47521"  
# [51] "2013-WfP--3-FH72396"  
# [52] "2013-WfP--1-FH69298"  
# [53] "2013-WfP-44-FH72691"  
# [54] "2014-MP-2-FH72680"    
# [55] "2014-WfP-104-FH72407" 
# [56] "2014-WfP-104-FH72572" 
# [57] "2014-MP-302-FH72715"  
# [58] "2014-WfP-303-FH72711" 
# [59] "2014-WfP-112-FH47271" 
# [60] "2015-KiP-108-FH72223" 
# [61] "2015-WfP-605-FH47110" 


#-------------debug-------------------------
#check each nest:
bfa[bfa$nest.id %in% "2014-WfP-112",]
cap.nodupl[cap.nodupl$nest.id %in% "2014-WfP-112",]
cap[cap$nest.id %in% "2015-KiP-104",]
ids.final[ids.final$nest.id %in% "2015-KiP-104",]

br$nest.id<-paste(br$year, br$species, br$nest, sep="-")
br[br$nest.id %in% "2014-WfP-106",]

ids.final[!is.na(ids.final$parent3),]
ids.final[ids.final$parent1 == ids.final$parent2,]
head(br[br$parent1 == br$parent2 & !is.na(br$parent1),])


#ignore these 61 cases? See how many nests in total remain in brood fates if these are ignored
a<-setdiff(nestid.ring.unique.bfa, ids.cap_bref)
b<-strsplit(a, "_")
library(plyr)
c <- ldply(b) #turn list into two columns
colnames(c) <- c("nest.id","ring") 

ignore <- c$nest.id
unique(ignore) #52 nests

str(bfa[!bfa$nest.id %in% ignore,]) #857 observations would remain
unique(bfa[!bfa$nest.id %in% ignore,"nest.id"]) #494 nests included

str(bfa[bfa$nest.id %in% ignore & bfa$observer %in% "LEP",]) #129 observations would be ignored
unique(bfa[bfa$nest.id %in% ignore,"nest.id"]) #56 nests ignored

str(ids.final[ids.final$nest.id %in% ignore,]) #44 in ignore for ids.final
str(ids.final[!ids.final$nest.id %in% ignore,]) #1712 nests would remain in ids.final

#--------------------------------------------
#ONLY IGNORE THESE NESTS FROM BROOD FATES AND MARK THEM AS poor quality brood fate data
a<-setdiff(nestid.ring.unique.bfa, ids.cap_bref)
b<-strsplit(a, "_")
library(plyr)
c <- ldply(b) #turn list into two columns
colnames(c) <- c("nest.id","ring") 

ignore <- c$nest.id
unique(ignore) #52 nests

bfa$bf.quality <- NA
bfa[bfa$nest.id %in% ignore, "bf.quality"] <- "poor quality"

#-----------------------------------------------
#################################################
#------------2. Add individual sexes-------------
names(sex)

letters<-"^[FH0-9]*$"
for(i in 1:length(bfa$year)){ #modification for MAD, search for rings of individuals with code but no ring
  if(!is.na(bfa$parent1[i]) & !grepl(pattern=letters, bfa$parent1[i], perl=T)){
    bfa$mol_sex.p1[i] <- sex$sex[match(bfa$parent1[i], sex$ring)]
  }
  if(!is.na(bfa$parent2[i]) & !grepl(pattern=letters, bfa$parent2[i], perl=T)){
    bfa$mol_sex.p2[i] <- sex$sex[match(bfa$parent2[i], sex$ring)]
  }
}
#-----------------------------------------------------------------

#----------------------WRITE STD FILE
getwd()
setwd("F:/Plovers/3rd Chapter/input/Madagascar/")
res.write <- res
write.csv(res.write, "Resightings_Madagascar_stdfile_CCIMarch2016.csv")
