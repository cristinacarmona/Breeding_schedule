#Fix bird reference from Tuzla, add missing nests from captures using previously built Birdref_stdfile
#12/04/2016, Cristina Carmona

#Tuzla
setwd("F:/Plovers/3rd Chapter/input/Tuzla")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles

import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))


#str(import.list)
ls()

working.list <- import.list[1:2]
#Ceuta: names(working.list) <- c("br1","br2","br","bf","cap1","cap","ne1","ne","re1","re","sex")
#Maionames(working.list) <- c("br","bf","cap","ne","re","sex","surv")
#Tuzla
names(working.list) <- c("br","br2")#"bf","cap1","cap","ne2","ne","re","hatch")

attach(working.list)
#detach(working.list)

#--------------------------------------
names(br)
names(br2)

#list nests to look up in br2 (std file)
nests.lookup<-br$nest.id[is.na(br$year)]

write<-br2[br2$NestID %in% nests.lookup,]


setwd("F:/Plovers/3rd Chapter/input/Tuzla")
write.csv(write, "extracted_birdref_nests.csv")
