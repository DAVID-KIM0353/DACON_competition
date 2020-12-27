#데이터 정의
#YM : 기준년월
#SIDO : 지역대분류명
#SIGUNGU : 지역중분류명
#FranClass : 소상공인구분
#Type : 업종명
#Time : 시간대
#TotalSpent : 총사용금액
#DisSpent : 재난지원금 사용금액
#NumOfSpent : 총 이용건수
#NumOfDisSpent : 총 재난지원금 이용건수
#POINT_X, POINT_Y : X,Y 좌표

install.packages('https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.3-9.tar.gz',repos=NULL,type="source")
install.packages('rvest')
install.packages("ggmap")
install.packages('maps')
install.packages('devtools')
install_github('dkahle/ggmap')
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(devtools)
library(rvest)
library(maps)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(gridExtra)

a <- read.csv("KRI-DAC_Jeju_data5.csv")
b <- read.csv("KRI-DAC_Jeju_data6.csv")
c <- read.csv("KRI-DAC_Jeju_data7.csv")
d <- read.csv("KRI-DAC_Jeju_data8.csv")

c <- c[,-c(6,7)]

ab <- rbind(a,b)
cd <- rbind(c,d)
df <- rbind(ab,cd)
sum(is.na(df)) # 결측치 합이 0이므로 결측치가 없는 것으로 파악!

df1 <- filter(df,Time!="x시")
head(df1)


#################################################
#x:달 y: 총액
by_YM <- group_by(df1,YM)
data1 <-summarise(by_YM,YM_total=sum(TotalSpent/100000000))
A1<-ggplot(data=data1,mapping=aes(x=YM,y=YM_total,fill=as.factor(YM)))+
  geom_bar(stat="identity")+

labs(
  title="5월~8월 총 사용금액",
  x="기준년월",
  y="총 사용금액(단위 : 억원)", 
  fill="기준년월"
  )

#x:달 y:지원금
by_YM <- group_by(df1,YM)
data2 <-summarise(by_YM,YM_Dis=sum(DisSpent/100000000))

A2<-ggplot(data=data2 ,mapping=aes(x=YM,y=YM_Dis,fill=as.factor(YM)))+
  geom_bar(stat="identity")+
  labs(
    title="5월~8월 재난지원금 사용금액",
    x="기준년월",
    y="재난지원금 총 사용금액(단위 : 억원)",
    fill="기준년월"
  )
grid.arrange(A1,A2,ncol=2)

##################################################### ----------1차

# 시간대별 
# 전체 
by_Time <- group_by(df1,Time)
data4 <-summarise(by_Time,total_time=sum(TotalSpent))
B1 <- ggplot(data=data4,mapping=aes(x=Time,y=(total_time/1000000000), fill =Time))+
  geom_bar(stat="identity")+
  labs(
    title="시간대별 총 사용금액 분포",
    x="시간",
    y="총 사용금액(단위 : 억원)"
  )+theme(legend.position = "none")

#재난지원금
data5 <-summarise(by_Time,total_time=sum(DisSpent))
B2 <- ggplot(data=data5,mapping=aes(x=Time,y=(total_time/1000000000), fill =Time))+
  geom_bar(stat="identity")+
  labs(
    title="시간대별 총 재난지원금액 분포",
    x="시간",
    y="총 사용금액(단위 : 억원)"
  )+theme(legend.position = "none")

grid.arrange(B1, B2, nrow = 2)


###################################################### ------------2차

# 위경도를 바꾸는 함수
convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}

df_l <- df1[ , c('POINT_X', 'POINT_Y')]
from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
df_l <- cbind(df_l, convertCoordSystem(df_l$POINT_X, df_l$POINT_Y, from.crs, to.crs))
df_s <- df_l[ , c('long', 'lat')]
head(df_s)
head(df_s)
df1[ , c('POINT_X', 'POINT_Y')] <- df_s
################################################################### 위도경도 변환완료

# 지도시각화 재난지원금
df_DisSpent <- df1[, c('DisSpent', 'POINT_X', 'POINT_Y')] # 위치별 재난지원금 사용 보기
head(df_DisSpent)
g1 <- ggmap(get_map(location='Hallasan National Park', zoom=10)) +
  stat_density_2d(data=df_DisSpent, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28)+
  labs(title = '위치별 재난지원금 사용 분포도')
g1<- g1 + scale_fill_gradient(low='yellow', high='red')
g1 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

####################################################### 지도 시각화 완료
# Type별 총사용금액분의 재난지원금 상위 10

df2<- df1[df1$NumofDisSpent>0,]
by_Type<-group_by(df2,Type)
by_typetotal<-summarise(by_Type,total_type=sum(DisSpent/Sum_total))
rank_type10<-arrange(by_typetotal, desc(total_type)) %>% slice(1:10)
unique(rank_type10$Type) # Type목록확인
s1 <- ggplot(data=rank_type10,mapping=aes(x=reorder(Type,-total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  labs(title="Type별 전체 총 사용금액 대비 재난지원금 상위 10개 항목",x='Type',y='총사용금액 대비 재난지원금',
       subtitle="재난지원금/총사용금액",fill="Type")

#Type별 총사용금액분의 재난지원금 하위 10

rank_typeworst10<-arrange(by_typetotal, total_type) %>% slice(1:10)
unique(rank_typeworst10$Type)
s2 <- ggplot(data=rank_typeworst10,mapping=aes(x=reorder(Type,-total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  labs(title="Type별 전체 총 사용금액 대비 재난지원금 하위 10개 항목",x='Type',y='총사용금액 대비 재난지원금',
       subtitle="재난지원금/총사용금액",fill="Type")

grid.arrange(s1, s2, nrow = 2)
###################################################### 전체 비율 상위 10개/ 하위 10개 시각화 완료

df1$Sum_total<-sum(df1$TotalSpent)
by_Fran<-group_by(df1,FranClass)
data3<-summarise(by_Fran,prop_Dis = sum(DisSpent/Sum_total))
ggplot(data=data3,mapping=aes(x="",y=prop_Dis,fill=FranClass))+
  geom_bar(stat="identity")+
  coord_polar("y")+
  geom_text(aes(label= paste0(round(prop_Dis*18.86*100,1), "%")),
            position = position_stack(vjust = 0.5))+
  theme_void()+
  labs(
    title="전체 총 사용금액 대비 재난 지원금 사용비율",
    x="소상공인 구분",
    y="사용비율",
    fill="소상공인 구분"
  )
######################################################총 사용금액 대비 재난 지원금 사용비율 완료
asd <- df1 %>% group_by(FranClass) %>% count()
asd1 <- filter(asd, FranClass %in% c("영세", "일반"))
ggplot(data=asd1,mapping=aes(x='', y = n, fill=FranClass))+
  geom_bar(stat="identity")+
  geom_text(aes(label= paste0(round(n/sum(n) * 100,1), "%")),
            position = position_stack(vjust = 0.5))+
  coord_polar('y')+
  theme_void()+
  labs(
    title="영세와 일반의 거래 횟수 비율",
    x=" n ",
    y="사용비율",
    fill=" 영세 및 일반의 거래 횟수 비율"
  )
################################################################ 영세와 일반의 거래 횟수 비율

#일반 상위10퍼 뽑아 총사용금액
df5<- df1[df1$FranClass =="일반", ]
by_Type5 <- group_by(df5,Type)
by_typetotal5 <-summarise(by_Type5,total5=sum(TotalSpent))

rank_type105<-arrange(by_typetotal5, desc(total5))%>% slice(1:10)
C1 <- ggplot(data=rank_type105,mapping=aes(x=reorder(Type,-total5),y=total5, fill = Type))+
  geom_bar(stat="identity") +
  coord_polar()+labs(
    title="일반에서의 총 사용금액의 합의 상위 10개 항목",
    x="Type",
    y="사용금액",
    fill="Type"
  )
  
#일반 하위10퍼 뽑아 총사용금액

rank_worsttype105<-arrange(by_typetotal5, total5)%>% slice(1:10)
C3 <- ggplot(data=rank_worsttype105,mapping=aes(x=reorder(Type,-total5),y=total5,fill=Type))+
  geom_bar(stat="identity") +
  coord_polar()+labs(
    title="일반에서의 총 사용금액의 합의 하위 10개 항목",
    x="Type",
    y="사용금액",
    fill="Type"
  )

#영세 상위10퍼 뽑아 총사용금액
df6<- df1[df1$FranClass =="영세", ]
by_Type6 <- group_by(df6,Type)
by_typetotal6 <-summarise(by_Type6,total6=sum(TotalSpent))

rank_type106<-arrange(by_typetotal6, desc(total6))%>% slice(1:10)
C2 <- ggplot(data=rank_type106,mapping=aes(x=reorder(Type,-total6),y=total6,fill=Type))+
  geom_bar(stat="identity")+
  coord_polar()+labs(
    title="영세에서의 총 사용금액의 합의 상위 10개 항목",
    x="Type",
    y="사용금액",
    fill="Type"
  )


#영세 하위10퍼 뽑아 총사용금액

rank_worsttype106<-arrange(by_typetotal6, total6)%>% slice(1:10)
C4 <- ggplot(data=rank_worsttype106,mapping=aes(x=reorder(Type,-total6),y=total6,fill=Type))+
  geom_bar(stat="identity")+
  coord_polar()+labs(
    title="영세에서의 총 사용금액의 합의 하위 10개 항목",
    x="Type",
    y="사용금액",
    fill="Type"
  )

grid.arrange(C1, C2, C3, C4, nrow = 2, ncol = 2)
######################################################################### 일반, 영세 총사용금액 별 시각화 완료

#일반 재난지원금 사용 상위 10

by_Type2<-group_by(df2,Type)
by_typetotal2<-summarise(by_Type2,total_type=sum(DisSpent/Sum_total))
rank_type10<-arrange(by_typetotal2, desc(total_type)) %>% slice(1:10)
unique(rank_type10$Type)
D1 <- ggplot(data=rank_type10,mapping=aes(x=reorder(Type,total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  coord_flip()+labs(title="일반에서의 Type 별 재난지원금 매출 상위 10개 항목",x='Type',y='재난지원금 사용 비율',
       subtitle="총 사용금액 합의 재난지원금 비율",fill="Type")

#전체 재난지원금 사용 하위 10개
rank_typeworst10<-arrange(by_typetotal2, total_type) %>% slice(1:10)
unique(rank_typeworst10$Type)
D3 <- ggplot(data=rank_typeworst10,mapping=aes(x=reorder(Type,total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  coord_flip()+labs(title="일반에서의 Type 별 재난지원금 매출 하위 10개 항목",x='Type',y='재난지원금 사용 비율 ',
       subtitle="총 사용금액 합의 재난지원금 비율",fill="Type")


#영세 재난지원금 상위 10개
df2<- df1[df1$NumofDisSpent>0,]
df3<- df2[df2$FranClass == "영세", ]
by_Type3<-group_by(df3,Type)
by_typetotal3<-summarise(by_Type3,total_type=sum(DisSpent/Sum_total))
rank_type103<-arrange(by_typetotal3, desc(total_type)) %>% slice(1:10)
D2 <- ggplot(data=rank_type103,mapping=aes(x=reorder(Type,total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="영세에서의 Type 별 재난지원금 매출 상위 10개 항목",x='Type',y='재난지원금 사용 비율',
       subtitle="총 사용금액 합의 재난지원금 비율",fill="Type")


#영세 재난지원금 하위 10개
df3<- df2[df2$FranClass == "영세", ]
by_Type3<-group_by(df3,Type)
by_typetotal3<-summarise(by_Type3,total_type=sum(DisSpent/Sum_total))
rank_typeworst103<-arrange(by_typetotal3, total_type) %>% slice(1:10)
unique(rank_typeworst103$Type)
D4 <- ggplot(data=rank_typeworst103,mapping=aes(x=reorder(Type,total_type),y=total_type, fill=Type))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="영세에서의 Type 별 재난지원금 매출 하위 10개 항목",x='Type',y='재난지원금 사용 비율',
       subtitle="총 사용금액 합의 재난지원금 비율",fill="Type")

grid.arrange(D1, D2, D3, D4, nrow = 2, ncol = 2)

########################################################################## 총 사용금액의 합의 재난지원금 비율 시각화 완료
# 일반에서 슈퍼마켓 좌표 찍기
dd1 <- filter(df1, FranClass %in% '일반')
dd1 <- filter(dd1, Type %in% '슈퍼마켓')
dd1 <- dd1[ , c('Type', 'POINT_X', 'POINT_Y')]

dd1_market1 <- ggmap(get_map(location='Hallasan National Park', zoom=10)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 슈퍼마켓 분포 지도')
dd1_market1 <- dd1_market1 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market2 <- ggmap(get_map(location='jeju', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 슈퍼마켓 분포 지도 (제주시 집중 분포도)')
dd1_market2 <- dd1_market2 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)


dd1_market3 <- ggmap(get_map(location='seogwipo', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 슈퍼마켓 분포 지도 (서귀포시 집중 분포도)')
dd1_market3 <- dd1_market3 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)


grid.arrange(dd1_market1, dd1_market2, dd1_market3, ncol = 3)
# ------------------------------------------------------------
# 영세에서 슈퍼마켓 좌표 찍기
dd1 <- filter(df1, FranClass %in% '영세')
dd1 <- filter(dd1, Type %in% '슈퍼마켓')
dd1 <- dd1[ , c('Type', 'POINT_X', 'POINT_Y')]

dd1_market1 <- ggmap(get_map(location='Hallasan National Park', zoom=10)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 슈퍼마켓 분포 지도')
dd1_market1 <- dd1_market1 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market2 <- ggmap(get_map(location='jeju', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 슈퍼마켓 분포 지도 (제주시 집중 분포도)')
dd1_market2 <- dd1_market2 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market3 <- ggmap(get_map(location='seogwipo', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 슈퍼마켓 분포 지도 (서귀포시 집중 분포도)')
dd1_market3 <- dd1_market3 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

grid.arrange(dd1_market1, dd1_market2, dd1_market3, ncol = 3)
###################################################################### 슈퍼마켓 지도 완료

# 일반에서 일반한식 좌표 찍기
dd1 <- filter(df1, FranClass %in% '일반')
dd1 <- filter(dd1, Type %in% '일반한식')
dd1 <- dd1[ , c('Type', 'POINT_X', 'POINT_Y')]

dd1_market1 <- ggmap(get_map(location='Hallasan National Park', zoom=10)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 일반한식 분포 지도')
dd1_market1 <- dd1_market1 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market2 <- ggmap(get_map(location='jeju', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 일반한식 분포 지도 (제주시 집중 분포도)')
dd1_market2 <- dd1_market2 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market3 <- ggmap(get_map(location='seogwipo', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '일반에서의 일반한식 분포 지도 (서귀포시 집중 분포도)')
dd1_market3 <- dd1_market3 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

grid.arrange(dd1_market1, dd1_market2, dd1_market3, ncol = 3)
# -------------------------------------------------------------
# 영세에서 일반한식 좌표 찍기
dd1 <- filter(df1, FranClass %in% '영세')
dd1 <- filter(dd1, Type %in% '일반한식')
dd1 <- dd1[ , c('Type', 'POINT_X', 'POINT_Y')]

dd1_market1 <- ggmap(get_map(location='Hallasan National Park', zoom=10)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 일반한식 분포 지도')
dd1_market1 <- dd1_market1 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market2 <- ggmap(get_map(location='jeju', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 일반한식 분포 지도 (제주시 집중 분포도)')
dd1_market2 <- dd1_market2 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

dd1_market3 <- ggmap(get_map(location='seogwipo', zoom=12)) +
  stat_density_2d(data=dd1, aes(x=POINT_X, y=POINT_Y, fill=..level.., alpha=..level..), geom='polygon', size=7, bins=28) +
  labs(title = '영세에서의 일반한식 분포 지도 (서귀포시 집중 분포도)')
dd1_market3 <- dd1_market3 + scale_fill_gradient(low='yellow', high='red', guide=F) + scale_alpha(range=c(0.02, 0.8), guide=F)

grid.arrange(dd1_market1, dd1_market2, dd1_market3, ncol = 3)
