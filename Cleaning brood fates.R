#17/03/2016 Clean Madagascar bird reference and produce compatible file to that of Maio and Ceuta
#Author: Cristina Carmona-Isunza

#1st log 17/03/2016 Create code using "Cleaning Resightings" code as base,
                    #found unmatching rings between broodfates and captures....

#Madagascar
setwd("F:/Plovers/3rd Chapter/input/Madagascar")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles


import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))


#str(import.list)
ls()

working.list <- import.list
names(working.list) <- c("bf","br","cap","sex","sex2","ne","re2","re")


attach(working.list)

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
cap<-cap[cap$site %in% "Andavadoaka",] #correct Issue 2....needs to be applied in Extract_breedingschedule code
table(cap$species)
cap$sp.code.ring <- paste(cap$species,"_", cap$code,"-", cap$ring, sep="")
cap$sp.sex.code <- paste(cap$species,"_", cap$code, "-",cap$sex, sep="")
unique.code.ring<- unique(cap$sp.code.ring) #4396 unique sp.code.ring, 3017 only ]Andavadoaka

lookdup <-strsplit(unique.code.ring, "-")

library(plyr)
ldup <- ldply(lookdup) #turn list into two columns
#colnames(ldup) <- c("code","ring")
colnames(ldup) <- c("sp_code","ring")  


ind <- which(duplicated(ldup$sp_code) | duplicated(ldup$sp_code, fromLast=TRUE))#MAD
str(ldup[ind,]) #2595 duplicates?? including XX, 1278 only Andavadoaka
str(ldup[-ind,]) #1801 non duplicates; 1739 only Andavadoaka

x1 <- ldup[ind,]
x1[order(x1$sp_code),]
# 
dupl <- x1 #List of rings and codes with duplicated codes 
str(dupl) #36 Ceuta, 2592 in Mad including Juv
dupl$sp.code.ring <- paste(dupl$sp_code, dupl$ring, sep="-")

#----------
sp_code.dupl<-unique(dupl$sp_code) #517; 170 only Andavadoaka


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
#check that parents new rings match the nest in captures

nestid.ring.all1<-paste(bfa$nest.id, bfa$parent1, sep="-")
nestid.ring.all2<-paste(bfa$nest.id, bfa$parent2, sep="-")

nestid.ring.unique<-c(unique(nestid.ring.all1), unique(nestid.ring.all2))
#list of unique nests with rings associated to it in brood fates

cap.nodupl$nest.id <- paste(cap.nodupl$year, cap.nodupl$species, cap.nodupl$nest, sep="-")
cap.nodupl<-cap.nodupl[cap.nodupl$age %in% "A",]

nestidcap.ring.all<-paste(cap.nodupl$nest.id, cap.nodupl$ring, sep="-")
nestidcap.unique<-unique(nestidcap.ring.all)

setdiff(nestid.ring.unique, nestidcap.unique) #531 in bfa but not in cap (many NAs), 
intersect(nestid.ring.unique, nestidcap.unique)#433 intersect
setdiff(nestidcap.unique, nestid.ring.unique) #1209 in cap but not in bfa

#-------------debug-------------------------


#--------------------------------------------

#----------------------WRITE STD FILE
getwd()
setwd("F:/Plovers/3rd Chapter/input/Madagascar/")
res.write <- res
write.csv(res.write, "Resightings_Madagascar_stdfile_CCIMarch2016.csv")
