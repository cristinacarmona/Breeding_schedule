#Change xls files to csv
#15/03/2016
#author: Cristina Carmona-Isunza

setwd("F:/Plovers/3rd Chapter/input/Madagascar")

xlsfiles <- list.files(path = ".", pattern='*\\.xls$', all.files=TRUE)
xlsfiles

library(xlsx)

df.list <- lapply(xlsfiles, function(x) read.xlsx(file=x, sheetIndex=1,
                                                   as.data.frame=TRUE, header=T))

working.list <- df.list
names(working.list) <- c("bref", "bfate","cap","mol2","mol","ne","re")

attach(working.list)


lapply(1:length(working.list), 
       function(i) write.csv(working.list[[i]], 
                             file = paste(names(working.list[i]), ".csv"),
                             row.names = FALSE))
