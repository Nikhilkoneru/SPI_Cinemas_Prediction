library(data.table)
library(dplyr)
library(omdbapi)
cinema = "SATHYAM"
langs = "HINDI"
elite = 66
Premium = 0
Economy = 0
Budget = 0
Online = 30   
Offline = 30
Period = strptime("5:00PM",format="%I:%M%p")
day = "Tuesday"

morning = strptime("11:00AM",format="%I:%M%p")
evening = strptime("3:00PM",format="%I:%M%p")
night = strptime("8:00PM",format="%I:%M%p")
########################################### MAIN CODE-CODE BLOCK NAME :- SATYAM HEART ###############################################thefinal = data.frame()
file.names <- dir("/home/nikhil/SPI/ticketDatawithscreen/", pattern =".csv")
iterator <- list()
for(i in 1:length(file.names)){
  finalxlsl <- suppressWarnings(try(fread(paste0("/home/nikhil/SPI/ticketDatawithscreen/",file.names[i])),silent = TRUE))
  finalxlsl <- data.frame(finalxlsl)
  finalxlsl   <-subset(finalxlsl,V2=="Sunday")
  finalxlsl$V1 <- as.Date(as.character(finalxlsl$V1),format = "%d/%m/%Y")
  finalxlsl$V3 <- as.Date(as.character(finalxlsl$V3),format = "%d/%m/%Y")
  finalxlsl$V3 <- (finalxlsl$V1-finalxlsl$V3)+1
  finalxlsl <- subset(finalxlsl,V17=="SATHYAM")
  iterator[[i]] <- finalxlsl[,-c(6)]
}
thedata <- do.call("rbind",iterator)
colnames(thedata)<-  c('Date','Day','Rday','movien','time','etime','lang','genric','elite','premium','economy','budget','online','offline','screen','cinema')
thedata[,c(9:14)] <- suppressWarnings(try(lapply(thedata[,c(9:14)],as.numeric),silent = TRUE))
then <- subset(thedata,lang!="ENGLISH"&lang!="TAMIL"&lang!="Tamil"&lang!="MALAYALAM"&lang!="TELUGU"&lang!="TELEGU"&lang!="HINDI"&lang!="Malayalam")
thedata$lang[thedata$lang=="Tamil"] <- "TAMIL"
thedata$lang[thedata$lang=="TELEGU"] <- "TELUGU"
thedata$lang[thedata$lang=="Malayalam"] <- "MALAYALAM"
crmn <- unique(then$movien)
nwmn <- c("ENGLISH","TELUGU","ENGLISH","TAMIL","KANNADA","TAMIL","TAMIL","HINDI","ENGLISH","TAMIL","MALAYALAM","ENGLISH","TAMIL","ENGLISH","ENGLISH","HINDI","TAMIL","TAMIL","ENGLISH","TAMIL","ENGLISH","ENGLISH","MALAYALAM","ENGLISH","MALAYALAM","ENGLISH","HINDI","TELUGU","TELUGU","TAMIL","TAMIL","TAMIL","HINDI","ENGLISH","ENGLISH","HINDI","MALAYALAM","TAMIL","TAMIL","HINDI","ENGLISH","ENGLISH","TAMIL","MALAYALAM","ENGLISH","MALAYALAM","TAMIL","KANNADA","ENGLISH","ENGLISH","ENGLISH","ENGLISH","HINDI","ENGLISH","ENGLISH","TAMIL","MALAYALAM","TAMIL","TAMIL","ENGLISH","ENGLISH","TELUGU","ENGLISH","ENGLISH")
for (i in 1:length(crmn)){
  thedata$lang[thedata$movien==crmn[i]] <- nwmn[i]
}
crmn2 <- unique(thedata$movien)
heros <- vector()
for (i in 1:length(crmn2)){
  heroname <- get_actors(find_by_title(crmn2[i]))
  if(is.null(heroname)){
    heros[i] <- "NULL"
  }else{
    heros[i]<-heroname[1] 
  }
}
thedat <- split(thedata,as.factor(thedata$movien))
rm(thedata)
thedat <- lapply(thedat,function(x){
  for(i in 1:length(crmn2)){
    if(unique(x$movien)==crmn2[i]){
      return(transform(x, heroname=heros[i]))
    } 
  }
})
print("1")
thedat <- do.call("rbind",thedat)
thedat <- split(thedat,as.factor(thedat$Date))
thedat <- lapply(thedat,function(x){
  return(transform(x, noofunim=length(unique(x$movien))))
})
thedat <- do.call("rbind",thedat)
rownames(thedat)<-c(1:nrow(thedat))
rm(i)
df.Base <- fread("/home/nikhil/SPI/sales.csv")
df.Base <- df.Base[,-c(1)]
df.Base$Date <- as.Date(as.character(df.Base$Date),format = "%Y-%m-%d")
cutter <- substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
df.Base$Time <- cutter(df.Base$Time,8)
df.Base$Time <- strptime(df.Base$Time,format="%H:%M:%S")
final <- merge(thedat,df.Base,by = c("Date","Date"),all.x=T, sort=F)
rm(thefinal,thedata)
final <- final[!duplicated(final),]
final <- split(final,list(final$Date,final$time))
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
print("2")
final <- lapply(final,function(x){
  x$time <- strptime(x$time,format="%I:%M%p")
  x$etime<- strptime(x$etime,format="%I:%M%p")
  x <- subset(x,Time>=time&Time<=etime)
  return(x)
})
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
print("3")
final <- lapply(final,function(x){
  itemslist <- unique(x$Item)
  initiater <- vector()
  uni <- unique(x$lobby)
  the_biglist <- list()
  for(i in 1:length(uni)){
    thecal <- x[x$lobby==uni[i],]
    for(k in 1:length(itemslist)){
      themixer  <- suppressWarnings(try(thecal[thecal$Item==itemslist[k],],silent = TRUE))
      initiater[k] <-c(sum(themixer$quantity))
    }
    the_biglist[i] <- list(initiater)
  }
  quant <- unlist(the_biglist)
  lobby <- rep(uni,each=length(itemslist))
  items <- rep(itemslist,length(quant)/length(itemslist))
  movie <- x[1,9:16]
  one <- data.frame(items,lobby,quant)
  return(merge(movie,one))
})
final <- do.call("rbind",final)
print("Phase:2 Done ,Processing predicition block")
print("Phase2-Execution-time:")
timestamp()
row.names(final) <- c(1:nrow(final))
