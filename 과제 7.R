setwd("D:/workspace/R_date/Part2/Stage3_StructuredData")
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(extrafont)

bb = read.csv('야구성적.csv')
bb
bc = select(bb, 선수명, 안타, 득점, 출루율, 타율, 홈런, 도루)
row.names(bc)=bc$선수명
bc = bc[,2:7]
lab=names(bc)
bc=table(bc)
value= table(lab)
stars(bc, filp.label = F, draw.segments = T, frame.plot = T, full = T,
      main =  '선수별 능력치') 

bb %>%
  select( 선수명, 안타, 득점, 출루율, 타율, 홈런, 도루) %>% 
  melt(id=c("선수명"), variable.name = "능력지표", value.name = "능력치") %>%
  ggplot(aes(x=능력지표,y=능력치,fill=능력지표)) + 
  geom_bar(width=1,stat="identity") +
  coord_polar() +
  facet_wrap(~선수명) +
  labs(title = "선수별 능력치") 

# ------------------------

setwd('D:/Workspace/R_Date/Part2/Stage3_StructuredData')

bus = read.csv('버스노선별이용현황합계.csv')
bus_melt = melt(bus, id=c('버스노선번호'))
bus_melt$value = bus_melt$value/1000

ggplot(bus_melt, aes(x=버스노선번호, y=value , fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_text(aes(y=value, label=value), color='black', size=3) +
  labs(x='노선명', y='이용승객수(단위:천명)') +
  theme_bw(base_family='malgun', base_size = 5) +
  theme(axis.text.x  = element_text(angle=90, hjust=1, size = 8)) +
  ggtitle('서울 주요 마을버스 이용승객 현황') +
  theme(plot.title = element_text(family="malgun", face = "bold", hjust = 0.5,
                                  vjust = 2.5, size = 15, color = "darkblue"))
  
# -----------------------------

setwd('D:/Workspace/R_Date/Part2/Stage3_StructuredData')

dt <- read.csv('2000-2013년 연령별실업율_연령별평균_세로.csv')
dt
dt <- melt(unemployment, id=c('연도'),
                          variable.name='연령', value.name='실업률')

dt$연령 <- gsub("X", "", dt$연령)

ggplot(dt, aes(x=연도, y=실업률, fill=연령,
                              color=연령, group= 연령)) +  # group 체크 
  geom_line(linetype=1, size=2) +
  geom_point(shape=19, size=3) +
  geom_hline(yintercept=seq(0,10,0.5), lty='dashed', size=0.1) +
  theme_bw(base_family='malgun',base_size = 10) +
  theme(axis.text.x  = element_text(angle=45, hjust=1)) +
  labs(x = '년도', y = '실업률') +  # 라벨에 이름값
  ggtitle('연령대별 실업률' ) + # 제목
  theme(plot.title = element_text(family="malgun", face = "bold", hjust = 0.5,
                                  vjust = 2.5, size = 15, color = "darkblue")) 
  


  