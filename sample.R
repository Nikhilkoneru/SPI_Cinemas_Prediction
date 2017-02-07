final <- merge(thedata,thefinal,by = c("Date","Date"),all.x=T, sort=F)
rm(thefinal,thedata)
final <- final[!duplicated(final),]
final <- split(final,list(final$Date,final$time))
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
final <- lapply(final,function(x){
  x$time <- strptime(x$time,format="%I:%M%p")
  x$etime<- strptime(x$etime,format="%I:%M%p")
  x$Time <- strptime(x$Time,format="%I:%M%p")
  x <- subset(x,Time>=time&Time<=etime)
  x <- x[x$Type=="Sale",]
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
row.names(final) <- c(1:nrow(final)) 
itemsvector <- unique(final$items)
predicit_results <- vector()
for (i in 1:length(itemsvector)){
  r_pop_small <- subset(final,items==itemsvector[i]&lobby=="S1 BALCONY") 
  newdataframe <- data.frame(quant=r_pop_small$quant,strength=I(r_pop_small$premium+r_pop_small$economy))
  predicit_results[i]<- predict(lm(quant~strength, data=newdataframe),data.frame(strength=150))
}
data.frame(itemsvector,predicit_results)

































rm(thefinal,thedata)
final <- final[!duplicated(final),]
final$time <- as.character(final$time)
final <- split(final,list(final$Date,final$time))
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
final <- lapply(final,function(x){
  x$time <- strptime(x$time,format="%I:%M%p")
  x$Time <- strptime(x$Time,format="%I:%M%p")
  x <- subset(x,Time>=time&Time<=etime)
  x <- x[x$Type=="Sale",]
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
row.names(final) <- c(1:nrow(final)) 
itemsvector <- unique(final$items)
predicit_results <- vector()
for (i in 1:length(itemsvector)){
  r_pop_small <- subset(final,items==itemsvector[i]&lobby=="S1 BALCONY") 
  newdataframe <- data.frame(quant=r_pop_small$quant,strength=I(r_pop_small$premium+r_pop_small$economy))
  predicit_results[i]<- predict(lm(quant~strength, data=newdataframe),data.frame(strength=150))
}
data.frame(itemsvector,predicit_results)