#23/03/2016
#2nd log 23/03/2016 updated files for Madagascar, run code again

#Cleaning code to generate Std Resigthings

setwd("F:/Plovers/3rd Chapter/input/Madagascar")

csvfiles <- list.files(path = ".", pattern='*\\.csv$', all.files=TRUE)
csvfiles


import.list <- lapply(csvfiles, read.csv, header = TRUE, as.is=TRUE, na.strings=c("NA"," ",""))


#str(import.list)
ls()

working.list <- import.list
#Ceuta: names(working.list) <- c("br1","br2","br","bf","cap1","cap","ne1","ne","re1","re","sex")
#Maio
names(working.list) <- c("bf","br","cap","sex","ne","re")


attach(working.list)



#-------------------------------------------------------------------------

#Resightings:
#clean code rings
res<-re[,c(1:14)]
names(res)
head(res)

more.11chr <- res[nchar(res$code)!=7, c("year","site","sex","date","time","code","observer","comment")] 
more.11chr[order(more.11chr$code),]

#Fix resightings of codes in wrong format e.g. WRML
nchar(more.11chr$code[41])
fix.code<-more.11chr[nchar(more.11chr$code)==4,"code"]

table(res$year, useNA="ifany")

for(i in 1:length(res$year)){
  if(res$code[i] %in% fix.code){
    res$new.code[i] <- paste(substr(res$code[i], 1,1),".", 
                             substr(res$code[i], 2,2), "|",
                             substr(res$code[i], 3,3), ".",
                             substr(res$code[i], 4,4), sep="")
  }else{
    res$new.code[i] <- res$code[i]
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
res$code <- res$new.code
names(res)
res<-res[,-15]

#------------------------------
#Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3958 obs with regexp1, 3958 obs with regexp2
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 73 errors, with regexp2: 73
# head(correct.code)
# 
# str(correct.code[nchar(correct.code$code)!=11, c("year","site","sex","date","time","code","observer","comments")])
# 
# incorrect.code[-1,c(1:2,4:6,12:13)]
# incorrect.code[incorrect.code$observer %in% "AT",c(1:3,4:6,12:13)]


####################################################################
#Regular expressions for codes with g or 0 
#regexp <- "[g]X" #finds g before | or after |
regexp<-"[0]"
ind.g<- grep(pattern = regexp, res$code, perl=TRUE)

gs<-res[ind.g,]

#CHANGES TO FILE TO CREATE STD FILE-----------------------------------------------------------

#Replace g with L; or 0 with O
res[res$code %in% gs$code,]
res$code[res$code %in% gs$code] <- gsub("0", "O", res$code[res$code %in% gs$code])

gs<-res[ind.g,] #changes should be seen on same set of codes 
res[c(4350,4351), "code"] <- gsub("O", "0", res$code[c(4350,4351)]) #change back the ones with numbers

#############################################################################


#Regular expressions for codes with ! or \
regexp <- "[!]"
regexp2 <- "\\\\"
ind.exc <- grep(pattern=regexp, res$code, perl=T)
ind.esc <- grep(pattern=regexp2, res$code, perl=T)

exc <- res[ind.exc,]

esc <- res[ind.esc,]

#CHANGES TO FILE TO CREATE STD FILE -------------------------------------------------------

#replace ! with |
res$code[res$code %in% exc$code] <- gsub("!", "\\|", res$code[res$code %in% exc$code])
exc <- res[ind.exc,]

res$code[res$code %in% esc$code] <- gsub("\\\\", "\\|", res$code[res$code %in% esc$code])
res[ind.esc,]

##############################################################################

#Regular expression for spaces
regexp <- "([A-Z]+)([\\s]+)|([\\s]+)([A-Z]+)|([\\s]+)([\\-]+)"
ind.blank <- grep(pattern=regexp, res$code, perl=T)
res[ind.blank,]

regexp2 <- "([0-9]+)" #only strings that start with numbers
#regexp3 <- "(^[a-zA-Z]+[^{|(\\!,:;???\")}@-]+)" #match only letters and punctuation characters

ind.nonumb <- grep(pattern=regexp2, res$code, perl=T)
res[ind.nonumb, ]
#codes with numbers in MAd....ignore
# year        site date time easting northing species distance degree    code sex ring
# 4336 2014 Andavadoaka  426 1017  320028  7556712     KiP      200     50 FH72534   0 <NA> #adult with no leg...
#   4350 2014 Andavadoaka  426 1146  319889  7556945     KiP      320     70 FH73031   0 <NA>#JUV
#   4351 2014 Andavadoaka  426 1146  319889  7556945     KiP      320     70 FH73036   0 <NA>#JUV
#   4450 2014 Andavadoaka  429 1750  319947  7556687     KiP      280     15 FH72533   0 <NA>#Juv
#   4451 2014 Andavadoaka  429 1750  319947  7556687     KiP      280     15 FH72534   0 #adult with no leg
# 

#ind.onlyletters <- grep(pattern=regexp3, res$code, perl=T)
#res[ind.onlyletters, "code"]
# 
# ind.joint <- setdiff(ind.blank, ind.nonumb)
# res[ind.joint,]
# 
# blanks <- res[ind.joint,]
blanks <- res[ind.blank,]
#CHANGES TO FILE------------------------------------------------------
#get rid of blank spaces
res$code[res$code %in% blanks$code] <- gsub("\\s", "", res$code[res$code %in% blanks$code])
# blanks <- res[ind.joint,]
res[ind.blank,]
#################################################################################
#Replace commas with dots

#regexp to find commas
regexp <- "([,]+)"
ind.comma <- grep(pattern=regexp, res$code, perl=T) 

commas <- res[ind.comma,]

#changes to file----------------------------
#get rid of comma
# res$code[res$code %in% commas$code] <- gsub(",", ".", res$code[res$code %in% commas$code])

#-----------------------------------
#check again:

more.11chr <- res[nchar(res$code)!=7, c("year","site","sex","date","time","code","observer","comment")] #15 wrong formatted codes
more.11chr[order(more.11chr$code),]

regexp1 <- "([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})\\|([RGLBYOWXMSsP]{1,2})\\.([RGLBYOWXMSsP]{1,2})$"
ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
correct.code <- res[ind.correct,]
str(correct.code) #7773
str(res[-ind.correct,]) #84 ones with K, numbers (already checked), or ?/incomplete
res[-ind.correct,"code"]
#nothing more can be fixed in resightings from Madagascar 23/03/2016

#-----------------------------------
# ##############################################################################
# 
# #Change Unringed
# 
# # incorrect.code[-1,c(1:2,4:6,12:13)]
# # incorrect.code[incorrect.code$observer %in% "AT",c(1:3,4:6,12:13)]
# 
# #change---------------------------------------------
# # res$code[c(3008, 3065)]<- "XX.XX|XX.XX"
# 
# ###############################################################################
# #Find codes with no dots
# 
# #Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3979 after changes
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 52 obs after cleanup (before cleaning: 75)
# head(correct.code)
# 
# 
# reg.exp <- "(^[^a-z]+$)" #only codes with upper case
# ind <- grep(pattern = reg.exp, res$code, perl=TRUE)
# trial <- res[ind,] 
# str(trial) #3999
# 
# str(only.code.errors <- setdiff(incorrect.code, trial))
# only.code.errors$code
# #-----------------------------------------stop 30/04/2015
# #----------------------------------------start 01/05/2015
# 
# #choose only incorrect codes that need fixing, omit those with cow countings...etc
# regexp2 <- "^([0-9]+)" #only strings that start with numbers
# regexp3 <- "^([0-9]+)|(^[A-Z]{1}[a-z]+)"
# #regexp3 <- "(^[a-zA-Z]+[^{|(\\!,:;???\")}@-]+)" #match only letters and punctuation characters
# 
# ind.numb <- grep(pattern=regexp3, res$code, perl=T)
# start.numb <- res[ind.numb,]
# #ind.onlyletters <- grep(pattern=regexp3, res$code, perl=T)
# #res[ind.onlyletters, "code"]
# ind.joint <- setdiff(incorrect.code$code, start.numb$code)
# str(ind.joint)
# 
# 
# firstdot <-"([RGLBYOWXM]{2})([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$" 
# lastdot <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})([RGLBYOWXM]{2})$"
# 
# ind.dots<-grep(pattern = lastdot, res$code, perl=TRUE)
# dots.code <- res[ind.dots,]
# str(dots.code)
# 
# #change---------------------------------------------
# res$code[res$code %in% dots.code$code] <- "BX.MX|LX.YX"
# res$code[1290]
# 
# #-------------------------------------------------------------------
# #Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3979 after changes
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 51 obs after last clean
# head(correct.code)
# incorrect.code$code
# 
# #Replace lower case with uppercase
# reg.exp <- "(^[^a-z]+$)" #only codes with upper case
# ind <- grep(pattern = reg.exp, res$code, perl=TRUE)
# upper <- res[ind,]
# upper$code
# 
# ind.joint <- setdiff(incorrect.code$code, upper$code)
# str(ind.joint)
# ind.joint
# 
# dot <-"^[^.]+$"
# ind <- grep(pattern=dot, res$code, perl=TRUE)
# nodot <- res[ind,]
# nodot$code #22 without dot (no codes: goats cows, etc)
# 
# y <- setdiff(ind.joint, nodot$code) #those which have dots and are incorrect
# 
# #------------------changes
# res[3974,]
# res$code[res$code %in% "Two dogs in sand extraction. "] <- "2 dogs in sand extraction"
# 
# #-------------------------------------------------------------------------------------------
# #Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3979 after changes
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 51 obs after last clean
# head(correct.code)
# incorrect.code$code
# 
# #Replace lower case with uppercase
# reg.exp <- "(^[^a-z]+$)" #only codes with upper case
# ind <- grep(pattern = reg.exp, res$code, perl=TRUE)
# upper <- res[ind,]
# upper$code
# 
# ind.joint <- setdiff(incorrect.code$code, upper$code)
# str(ind.joint)
# ind.joint
# 
# dot <-"^[^.]+$"
# ind <- grep(pattern=dot, res$code, perl=TRUE)
# nodot <- res[ind,]
# nodot$code #22 without dot (no codes: goats cows, etc)
# 
# y <- setdiff(ind.joint, nodot$code) #those which have dots and are incorrect
# 
# #----------------------change
# ind<- which(res$code %in% y) #only NA and codes needed...can apply toupper
# res$code[ind] <- toupper(res$code[ind])
# 
# #----------------------------------------------
# #Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3979 after changes
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 50 obs after last clean
# head(correct.code)
# incorrect.code$code
# 
# #do not show those that start with numbers
# regexp2 <- "^([0-9]+)"
# ind.numb <- grep(pattern=regexp2, res$code, perl=T)
# start.numb <- res[ind.numb,]
# 
# change <- setdiff(incorrect.code$code, start.numb$code)
# #--------------------change
# #change by hand incorrect codes that can be corrected:
# 
# ind <- which(res$code %in% "corvo")
# res[ind,]
# res$code[3722]<-"1 corvo"
# 
# res$code[res$code %in% "MX.XX|YX.XX*"] <- "MX.XX|YX.XX"
# res$code[res$code %in% "MX.RX|W.OX"] <- "MX.RX|WX.OX"
# res$code[res$code %in% "MX.OX.OX.LX"] <- "MX.OX|OX.LX"
# res$code[res$code %in% "MX.XX|X.XX"] <- "MX.XX|XX.XX"
# res$code[res$code %in% "MX.RX|YX.B"] <- "MX.RX|YX.BX"
# res$code[res$code %in% "B.XMX|LX.YX"] <- "BX.MX|LX.YX"
# 
# 
# res[res$code %in% "MX.OX|LGX.LX",]
# res$code[res$code %in% "MX.OX|LGX.LX"] <- "MX.OX|GX.LX" #checked notes 05/05/2015
# 
# res[res$code %in% "MX.YX|?X.WX",] #no mistake notes are like that
# 
# #-------------check remaining mistakes
# #Regular expressions for correct codes:
# regexp1 <- "([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})\\|([RGLBYOWXM]{2})\\.([RGLBYOWXM]{2})$"
# regexp2 <- "([RGLBYOWM]{1})([X]{1})\\.([RGLBYOWM]{1})([X]{1})\\|([RGLBYOWM]{1})([X]{1})\\.([A-Z]{1})([X]{1})$"
# 
# ind.correct<-grep(pattern = regexp1, res$code, perl=TRUE)
# correct.code <- res[ind.correct,] #CORRECT OBS: 3979 after changes
# str(correct.code)
# 
# incorrect.code <- res[-ind.correct,] 
# str(incorrect.code) #with regexp1: 43 obs after all cleaning
# head(correct.code)
# incorrect.code$code

#----------------------WRITE STD FILE
getwd()
setwd("F:/Plovers/3rd Chapter/input/Madagascar/")
res.write <- res
write.csv(res.write, "Resightings_Madagascar_stdfile_CCIMarch2016.csv")
