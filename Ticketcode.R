library(data.table)
ptm <- proc.time()
print("Starting-time:")
timestamp()
#############################INPUT FROM USER
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
################################ Deciding factors

balcony_c = elite
lobby_c = Premium+Economy+Budget



########################################### MAIN CODE-CODE BLOCK NAME :- SATYAM HEART ###############################################
thefinal = data.frame()
file.names <- dir("/home/nikhil/SPI/ticketDatawithscreen/", pattern =".csv")
iterator <- list()
for(i in 1:length(file.names)){
  finalxlsl <- suppressWarnings(try(fread(paste0("/home/nikhil/SPI/ticketDatawithscreen/",file.names[i])),silent = TRUE))
  finalxlsl <- data.frame(finalxlsl)
  if(day=="Sunday"){
    finalxlsl   <-subset(finalxlsl,V17==cinema&V8==langs&V2=="Sunday")
  }else if(day=="Saturday"){
    finalxlsl   <-subset(finalxlsl,V17==cinema&V8==langs&V2=="Sunday")
  }else if(day!="Sunday"&day!="Saturday"){
    finalxlsl   <-subset(finalxlsl,V17==cinema&V8==langs&V2!="Sunday"&V2!="Saturday")
  }
  finalxlsl$V1 <- as.Date(as.character(finalxlsl$V1),format = "%d/%m/%Y")
  finalxlsl$V3 <- as.Date(as.character(finalxlsl$V3),format = "%d/%m/%Y")
  finalxlsl$V3 <- (finalxlsl$V1-finalxlsl$V3)+1
  iterator[[i]] <- finalxlsl[,-c(6)]
}
megadf2 <- do.call("rbind",iterator)
colnames(megadf2)<-  c('Date','Day','Rday','movien','time','etime','lang','genric','elite','premium','economy','budget','online','offline','screen','cinema')
thedata <- subset(megadf2,online!="NULL"&offline!="NULL")
thedata[,c(9:14)] <- suppressWarnings(try(lapply(thedata[,c(9:14)],as.numeric),silent = TRUE))
print("Phase:1 Done ,Processing two more blocks")
print("Phase1-Execution-time:")
timestamp()
#########################################################################################################################

file.names <- dir("/home/nikhil/SPI/ID/", pattern =".csv")
iterator <- list()
for(i in 1:length(file.names)){
  final <- suppressWarnings(try(fread(paste0("/home/nikhil/SPI/ID/",file.names[i])),silent = TRUE))
  iterator[[i]] <-  subset(final,V4!="ES 3D GLASSES"&V4!="3D GLASSES"&V4!="3D GLASSES NEW"&V4!="3D GLASSES ESCAPE"&(V7=="S1 BALCONY"|V7=="S1 LOBBY"|V7=="S1- LOBBY"))
}
thefinal <- data.frame(do.call("rbind",iterator))
thefinal[7][thefinal[7]=="S1- LOBBY"] <- "S1 LOBBY"
thefinal <- thefinal[thefinal$V5=="Sale",]
rm(final,iterator,i,file.names)
thefinal$V1 <- as.Date(as.character(thefinal$V1),format = "%d/%m/%Y")
colnames(thefinal)<- c("Date","Day","Time","Item","Type","quantity","lobby")
final <- merge(thedata,thefinal,by = c("Date","Date"),all.x=T, sort=F)
rm(thefinal,thedata)
final <- final[!duplicated(final),]
final <- split(final,list(final$Date,final$time))
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
thedecider <- function(x){
  finalxlsl = x
  if(Period<=morning){
    finalxlsl  <- subset(finalxlsl,time<=morning)
  }else if(Period>morning&Period<=evening){
    finalxlsl <- subset(finalxlsl,time>morning&time<=evening)
  }else if(Period>evening){
    finalxlsl <- subset(finalxlsl,time>evening) 
  }
  return(finalxlsl)
}
final <- lapply(final,function(x){
  x$time <- strptime(x$time,format="%I:%M%p")
  x$etime<- strptime(x$etime,format="%I:%M%p")
  x$Time <- strptime(x$Time,format="%I:%M%p")
  x <- subset(x,Time>=time&Time<=etime)
  x <- thedecider(x)
  return(x)
})
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
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
thebrain <- function(thelob){
df <- subset(final,lobby==thelob)
df<-df[!(df$quant==0),]
it <- unique(df$items)
thepredictionfunction <- sapply(it,function(x,y,z){
  item <- subset(y,items==x)
  if(z=="S1 BALCONY"){
  newdataframe <- data.frame(quant=item$quant,strength=I(item$elite))
  p <- predict(lm(quant~strength, data=newdataframe),data.frame(strength=balcony_c))
  }else{
  newdataframe <- data.frame(quant=item$quant,strength=I(item$budget+item$premium+item$economy))
  p <- predict(lm(quant~strength, data=newdataframe),data.frame(strength=lobby_c))
  }
  return(p)
},y=df,z=thelob)
return (data.frame(it,thepredictionfunction))
}

S1_lobby <- suppressWarnings(try(thebrain("S1 LOBBY"),silent = TRUE))
S1_Balcony <-suppressWarnings(try(thebrain("S1 BALCONY"),silent = TRUE))
ts2 <- timestamp()
print("Done")
print("Ending Time:")
timestamp()
print(paste("Total Time:",proc.time()-ptm))
