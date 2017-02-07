library(data.table)
file.names <- dir("/home/nikhil/SPI/ID/", pattern =".csv")
iterator <- list()
for(i in 1:length(file.names)){
  final <- suppressWarnings(try(fread(paste0("/home/nikhil/SPI/ID/",file.names[i])),silent = TRUE))
  iterator[[i]] <-  subset(final,V4!="ES 3D GLASSES"&V4!="3D GLASSES"&V4!="3D GLASSES NEW"&V4!="3D GLASSES ESCAPE"&(V7=="S1 BALCONY"|V7=="S1 LOBBY"|V7=="S1- LOBBY"))
}
thefinal <- data.frame(do.call("rbind",iterator))
thefinal[7][thefinal[7]=="S1- LOBBY"] <- "S1 LOBBY"
thefinal <- thefinal[thefinal$V5=="Sale",]
thefinal$V1 <- as.Date(as.character(thefinal$V1),format = "%d/%m/%Y")
colnames(thefinal)<- c("Date","Day","Time","Item","Type","quantity","lobby")
thefinal$Time <- strptime(thefinal$Time,format="%I:%M%p")
df_list <- split(thefinal, as.factor(thefinal$Date))
#Vijai Balan Sir's query for sales moring 6 to next day 6
for (i in 1:length(df_list)){
  if(i<I(length(df_list)-1)){
    thecluber <- subset(df_list[[I(i+1)]], Time<=strptime("6:00AM",format="%I:%M%p"))
    thecluber$Date <- thecluber$Date-1 
    df_li <- subset(df_list[[i]],Time>=strptime("6:00AM",format="%I:%M%p"))
    df_list[[i]] <- rbind(thecluber,df_li)
  }}
df.Base <- do.call("rbind", df_list)
df.Base$Time <- strftime(df.Base$Time,format="%I:%M%p")
saveRDS(df.Base,"/home/nikhil/SPI/sal.Rds")

