##This script is to read gwl data for each year in each sheet and combine all into one sheet.
remove(list=ls())
library(openxlsx)
library(zoo)


dir.src = "/home/rishu/Projects/1000/wellzzzz/"
setwd(dir.src)

dir.data = "/home/rishu/Projects/1000/wellzzzz/data"
in.fileName = "Gadag_2007-18-June.xlsx"

dir.output <- "/home/rishu/Projects/1000/wellzzzz/out"

years <- 2007:2018;
date.start <- as.Date("2007-01-01")
date.end <- as.Date("2018-12-01")
taluk <- "Shirahatti"
district.code <- "GA"


wb <- openxlsx::loadWorkbook(file = file.path(dir.data,in.fileName))

foo <- list()

for (iyear in years){
  
  data <- read.xlsx(wb, sheet = as.character(iyear), skipEmptyRows = FALSE, colNames = TRUE,startRow = 2, detectDates =T)
  header <- colnames(data)
  
  last.date <- convertToDate(colnames(data)[length(data)])
  
  if(is.na(last.date)){
    col.names.new <- as.Date(sprintf('01-%s',colnames(data)), '%d-%b-%Y')
    indx <- which(!is.na(col.names.new))
    colnames(data)[indx] <- as.character(col.names.new[indx])
    # print(colnames(data))
    
  }
  else{
    col.names.new <- convertToDate(colnames(data))
    indx <- which(!is.na(col.names.new))
    colnames(data)[indx] <- as.character(col.names.new[indx])
  }
  
  data$Taluk <- na.locf(data$Taluk)
  data.gwl <- data[(data$Taluk == taluk),]
  data.well.id <-  unique(data.gwl$Well.Code)
  data.well.id <- data.well.id[!is.na(data.well.id)]
  
  for (id in data.well.id){
    data.well <-  data.gwl[data.gwl$Well.Code == id,]
    colnames(data.well) <- tolower(colnames(data.well))
    data.well <-  data.well[complete.cases(data.well$taluk), ]
    header.info <- data.frame(code = id,taluk = unique(data.well$taluk),village = data.well$village,lat = data.well$latitude,long = data.well$longitude)
    o.fileName <- file.path(dir.output,paste0(taluk,"_",id,".csv"))
    if (!file.exists(o.fileName)){
      out.df <- rbind(t(header.info),t(data.well[,indx[1]:indx[length(indx)]]))
    }else{
      out.df <- t(data.well[,indx[1]:indx[length(indx)]])
    }
    write.table(out.df,o.fileName,sep=",",append = TRUE,col.names = FALSE)
  }
}

files <- list.files(dir.output,pattern = paste0("*",taluk),full.names = TRUE)


## Change this logic to zoo and fill the missing dates with NA or -


date.full.series <- data.frame(date = as.character(seq(date.start,date.end, by = "1 month")))



DF <- list()
head.ls <- list()
count = 1

for (f in files) {
  well.gwl <- read.csv(f,stringsAsFactors = F)
  levl <- well.gwl[5:dim(well.gwl)[1],]
  colnames(levl) = c("date",colnames(levl)[2])
  data.complete <- merge(levl,date.full.series, all = T)
  DF[[count]] <- data.complete
  head.ls[[count]] <- well.gwl[1:4,]
  count <- count + 1
}


DF <- as.data.frame(do.call(cbind,DF))
head.df = as.data.frame(do.call(cbind,head.ls))

colnames(DF) <- gsub("date", "code", colnames(DF))

DF = rbind(head.df,DF)

even.indx <- seq(2,dim(DF)[2],by=2)

DF.data <- DF[,even.indx]

DF.final <- data.frame(code=DF[,1],DF.data)

write.csv(DF.final,file.path(dir.output,paste0(district.code,'_',taluk,'_gwl',".csv")), row.names=FALSE, quote=FALSE)

