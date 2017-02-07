library(e1071)
library(caret)
library(ggplot2)
library(nnet)
library(earth)
thedata <- readRDS("/home/nikhil/SPI/ticketdata.Rds")
thefinal <-readRDS("/home/nikhil/SPI/s.Rds")
thedata <- subset(thedata,Day=="Monday"&lang=="TAMIL")
thefinal <- subset(thefinal,lobby=="S1 BALCONY")
final <- merge(thedata,thefinal,by = c("Date","Date"),all.x=T, sort=F)
final <- final[!duplicated(final),]
final$etime<- strptime(final$etime,format="%I:%M%p")
final$Time<- strptime(final$Time,format="%I:%M%p")
final <- split(final,list(final$Date,final$time))
final <- final[sapply(final, function(x) dim(x)[1]) > 0]
final <- lapply(final,function(x){
  x$time <- strptime(x$time,format="%I:%M%p")
  x <- subset(x,Time>=time&Time<=etime)
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
  movie <- x[1,c(1,3,9:16)]
  one <- data.frame(items,lobby,quant)
  return(merge(movie,one))
})
final <- do.call("rbind",final)
library(ggplot2)
row.names(final) <- c(1:nrow(final))
it <- unique(final$items)
final$online[is.na(final$online)] <- 0
final$total <- I(final$offline+final$online)
item <- subset(final,items=="CREAM DONUTS"&elite<224)
newdataframe <- data.frame(quant=item$quant,strength=I(item$elite))
ggplot(newdataframe,aes(strength,quant))+
  geom_jitter()
theregression <- function(x){
  modelvector <- vector()
  model1 <- predict(lm(quant~strength, data=x),x)
  modelvector[1]<- sqrt(mean(I(x$quant-model1)^2))
  model2 <- predict(svm(quant ~ strength , data=x),x)
  modelvector[2]<- sqrt(mean(I(x$quant-model2)^2))
  model3 <- predict(train(quant ~ strength,x,method = "lm",trControl=trainControl(method="repeatedcv", number=10, repeats=3)),x)
  modelvector[3] <- sqrt(mean(I(x$quant-model3)^2))
  model4 <- predict(rpart(quant~strength, data=x,control=rpart.control(minsplit=5)),x)
  modelvector[4] <- sqrt(mean(I(x$quant-model4)^2))
  model5 <- predict(nnet(quant~strength,x, size=12, maxit=500, linout=T, decay=0.01), x, type="raw")  
  modelvector[5] <- sqrt(mean(I(x$quant-model5)^2))
  model6 <- predict(train(quant ~ strength, data=x, trControl=trainControl(method="repeatedcv", number=10, repeats=3), method="rpart"),x)
  modelvector[6]<-  sqrt(mean(I(x$quant-model6)^2))
  model7 <- predict(earth(quant ~ strength, data=x),x)
  modelvector[7]<- sqrt(mean(I(x$quant-model7)^2))
  df <- data.frame(x,model1,model2,model3,model4,model5,model6,model7)
  df[,-c(1:2)] <- round(df[,-c(1:2)],0)
  return(list(modelvector,df))
}
theregression(newdataframe)
