setwd("d:/workspace/R_date")
library(rJava)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(KoNLP)
library(extrafont)
library(plotrix)
useNIADic()
data1 = readLines('hiphop.txt')
data1
data2= Filter(function(x) {nchar(x) >= 3}, data1)
data2= gsub("\\d+","",data2)
data2= gsub(" ","",data2)
data2= gsub("","",data2)
data2= gsub("엿먹","",data2)
data2= gsub("고흐","",data2)
data2= gsub("나","",data2)
data2= gsub("내","",data2)
data2= gsub("것","",data2)
data2= gsub("너","",data2)
data2= gsub("[a-y]","",data2)
data2= gsub("지","",data2)
data2= gsub("한","",data2)
write(unlist(data2),"pop.txt")
data3 = read.table("pop.txt", quote="")
data3
wordcount=table(data3)
nrow(data3)
data3
palete=brewer.pal(10, "Set3")
wordcloud(names(wordcount),freq=wordcount,scale = c(3, 1),rot.per=0.25,min.freq=1,
          random.order=F,random.color=T,colors=palete)

# ------------------------------------------

setwd("d:/workspace/R_date")
library(rJava)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(KoNLP)
library(ggplot2)
useNIADic()
data1 = readLines('hiphop.txt')
data1
data2= Filter(function(x) {nchar(x) >= 3}, data1)
data2= gsub("\\d+","",data2)
data2= gsub("\\'","",data2)
data2= gsub("\\","",data2)
data2= gsub(" ","",data2)
data2= gsub(",","",data2)
data2= gsub("’","",data2)
data2= gsub("z","",data2)
data2= gsub("","",data2)
data2= gsub("[a-y]","",data2)
data2= gsub("[A-Z]","",data2)
write(unlist(data2), "popdata.txt")
data3 = read.table("popdata.txt", quote="")
nrow(data3)
wordcount=table(data3)
wordcount
top10 =head(sort(wordcount, decreasing = T), 10)
pie(top10, main = "dd")


df_top10= as.data.frame(top10)
df_top10
ggplot(df_top10, aes(x='', y=Freq, fill=data3)) +
  geom_bar(width = 1, stat='identity') +
  coord_polar("y", start = 0)


  # --------------------------------------
ba = head(sort(wordcount, decreasing = T), 10)
ba
bp = barplot(ba, main = "노래가사 top10", col = rainbow(10), 
            cex.names=0.7, las = 2, ylim=c(0,25))

            