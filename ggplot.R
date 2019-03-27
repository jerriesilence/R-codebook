ggplot(final,aes(x=as.factor(category),group=1))+
  geom_bar(aes(y=apps_count_a),stat='identity',colour='blue')+
  geom_line(aes(y=rank_a),colour='black')+
  ylab("No of apps & Average category rank") +
  xlab("Category")+
  ggtitle('Google play store -- Bar - Unique apps | Line - Average category rank')

ggplot(data = data)+
   geom_bar(aes(x=ym,y=n_cus,fill=factor(customer,levels=c(0,1),labels=c('w/o ID','W/ ID'))),stat='identity')+
   geom_bar(data=data2,aes(x=ym,y=n_cus,fill='New ID'),stat='identity')+  
   labs(x='Month',
        y="Count",
        title="Customers Contribution",
        subtitle="",
        fill = "Customer Type")+
  theme(axis.text.x = element_text(face="bold", angle=45, size=10))+
  scale_x_discrete(limits=c('2016_7','2016_8','2016_9','2016_10','2016_11','2016_12','2017_1','2017_2','2017_3','2017_4','2017_5','2017_6','2017_7','2017_8','2017_9','2017_10','2017_11','2017_12','2018_1','2018_2','2018_3','2018_4','2018_5','2018_6','2018_7','2018_8'))+
  scale_color_manual(breaks = c("8", "6", "4"),
                     values=c("red", "blue", "green")) # colors for specific value

theme1<-theme_bw()+
	theme(text=element_text(family='A'), #字体
           panel.background=element_rect(pptbg), #画布背景颜色
           plot.background=element_rect(pptbg), #图形背景颜色
           plot.title = element_text(hjust=0.5,size=16,vjust=0.5), #标题位置
           panel.border=element_blank(),#图形边界
           panel.grid.major=element_line(colour='lightgrey',linetype="dashed"), #网格线
           panel.grid.minor=element_blank(), #次级网格线
           legend.position = 'top', #图例位置
           legend.title=element_text(size=10,colour='black',family='A',vjust=-0.5), #图例标题
           legend.text=element_text(size=10,colour='black',family='A'), #图例文字
           legend.background =element_rect(pptbg),#图例背景
           axis.text=element_text(size=12,colour="black",family='A'), #坐标轴文字
           strip.text=element_text(size=12,colour="black",family='A'),#分面文字
           strip.background=element_blank(),#分面的背景
           axis.line = element_line(size=0.5, colour = 'black'), #轴颜色大小
           panel.spacing=unit(10,'mm') #画布大小
--------------------- 
作者：Ryan_Yang_ 
来源：CSDN 
原文：https://blog.csdn.net/Yunru_Yang/article/details/77327757 
版权声明：本文为博主原创文章，转载请附上博文链接！ 


ggplot(data = data)+
  geom_line(aes(x=`hour(Time)`,y=num,col=as.factor(weekend)),stat='identity')+
  labs(x='Hour: 7:00 ~ 20:00',
       y="# of transactions",
       title="# of Transactions hour distribution",
       subtitle="Weekends vs. Weekdays",
       col = "")+
  theme(panel.grid.major =element_blank(), 
  	    panel.grid.minor = element_blank(),
  	    panel.background = element_blank(),
  	    axis.line = element_line(colou = "black"),
  	    plot.margin=unit(c(1,5,1,5), 'lines'),
  	    legend.position='bottom')+
  scale_x_continuous(limit = c(7, 19),
  	                 breaks = c(7:19))+
  scale_colour_manual(values=c("#660000", "#000000"))



#Cleaning
# Date and Time Type transform
df$Date<-as.IDate(df$Date,format='%m/%d/%y')
df$Time<-as.ITime(df$Time,format='%H:%M:%S')
df$datetime<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")

# Factor transform
cols<-c(3,4,6,13)
df<-as.data.frame(df)
df[,cols] <- data.frame(apply(df[cols], 2, as.factor))

# Numeric transform
cols<-c(7:10)
df[,cols]<- data.frame(apply(df[cols],2,str_remove, pattern = '\\$'))%>%
  apply(2,str_replace,pattern='\\(',replace='\\-')%>%
  apply(2,str_remove,pattern='\\)')%>%
  apply(2,as.numeric)

#charcter clean
df$Item<-as.factor(str_replace(as.character(df$Item),"ðY<LemonadeðY<","Lemonade"))


#packages
library(arules) # association 
library(arulesViz) # association visualization
library(lubridate) # date time
library(stringr) # string editing
library(purrr) # data munging
library(magrittr) # pipeline

prcomp # PCA

