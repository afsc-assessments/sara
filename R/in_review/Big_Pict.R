### Script for making some Council meeting graphs 12/8/2015, hanselman

library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
library(scales)


 ### Some BSAI overall summary graphs
 cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
 bsum<-read.csv("bsai_sum_20.csv",header=T)
 #bsum<-bsum[-1,]
 bsum2<-bsum
  bsum2[,2:6]<-bsum2[,2:6]/1000000
  bsum2$ClB<-bsum2$Catch/bsum2$Biomass
  bsum2<-bsum2[,-3]
   bsum3<-melt(bsum2,id.vars=1)
 names(bsum3)<-c("Year","Type","mmt")
 png("bigpic.png", width = 6, height = 4, units = 'in', res = 400)
 bsumnames<-c("Catch","ABC","OFL","Biomass","Catch/Bio")
 p<-ggplot(bsum3,aes(x=Year,y=mmt,color=Type)) +   facet_grid(Type ~ ., scales = "free") + theme_bw(base_size=12)+
   ggtitle("Big picture over time")+
   geom_bar(stat = "identity", aes(fill=Type),color="gray30")+labs(y="Million metric tons")+
   scale_fill_manual(values=cbbPalette,labels=bsumnames)
  p
 print(p)
 dev.off() 
 ggplot(bsum3,aes(x=Year,y=mmt,color=Type)) +   theme_bw(base_size=12)+
   ggtitle("Big picture over time")+
   geom_col( aes(fill=Type),color="gray30")+labs(y="Million metric tons")+
   scale_fill_manual(values=cbbPalette,labels=bsumnames)
 
### Slope plot
months<-12
year1<- c(2887361, 260500,260800,356647,59349,87200,92808)
year2<- c(2693588,209500,277500,344457,57958,92000,88354)

group<-c("Pollock", "P. Cod", "YFS","Non-YFS Flats","Rockfish","Atka","Other")
a<-data.frame(year1,year2,group)
b<-a
#a[4,c(1,2)]<-1.2*a[4,c(1,2)]
#a[6,c(1,2)]<-0.95*a[6,c(1,2)]
#a[6,c(1,2)]<-0.95*a[6,c(1,2)]
#a[2,c(1,2)]<-1.2*a[2,c(1,2)]
#a[6,c(1,2)]<-0.95*a[6,c(1,2)]


a[,1]<-log(a[,1])
a[,2]<-log(a[,2])
a[1,1]<-0.93*a[1,1]
a[1,2]<-0.93*a[1,2]
#a[1,2]<-a[1,2]-1200000
b$Percent<-(b[,2]-b[,1])/b[,1]
b$Col<-4
b[b$Percent<0,]$Col<-2
#a$year1<-a$year2-a$year2*b$Percent
a$Mid<-(a[,1]+a[,2])/2
a$Mid[6]<-a$Mid[6]*0.987
a$Mid[1]<-a$Mid[1]*0.995
a$Mid[2]<-a$Mid[2]*0.995
a$Mid[7]<-a$Mid[7]*0.998

#l11<-paste(a$group,comma_format()(round(a$year1/(3600*12*30.5))),sep="\n")
#l13<-paste(a$group,comma_format()(round(a$year2/(3600*12*30.5))),sep="\n")
a
l11<-paste(a$group,comma_format()(b$year1),sep="\n")
l13<-paste(a$group,comma_format()(b$year2),sep="\n")
l11<-paste(a$group,comma_format()(b$year1),sep=": ")
l13<-paste(comma_format()(b$year2),sep=": ")
#a[6,1]<-a[6,1]-0.14

p<-ggplot(a) + geom_segment(aes(x=0,xend=months,y=a$year1,yend=a$year2),size=.75,col=b$Col)+theme_bw(base_size=10)
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())
p<-p + xlab("") + ylab("Change in ABC")
p<-p+theme(axis.title=element_text(size=16,face="bold",vjust=-10))
p
a$year1[6]<-a$year1[6]*0.988
a$year2[6]<-a$year2[6]*0.988
a$year1[2]<-a$year1[2]*0.99
a$year2[2]<-a$year2[2]*0.99
#p<-p + theme(axis.title.y=theme_text(vjust=3))
p<-p + xlim((0-12),(months+12))
p<-p + ylim(min(a$year2,a$year1),(1.01*(max(a$year2,a$year1))))
p<-p + geom_text(label=l13, y=a$year2, x=rep.int(months,length(a[,1])),hjust=-0.2,size=3.5)
p<-p + geom_text(label=l11, y=a$year1, x=rep.int( 0,length(a[,1])),hjust=1.2,size=3.5)
p<-p + geom_text(label="2017", x=0,     y=(1.015*(max(a$year2,a$year1))),hjust= 1.5,size=5)
p<-p + geom_text(label="2018", x=months,y=(1.015*(max(a$year2,a$year1))),hjust=-0.2,size=5)
p<-p + geom_text(label=percent(b$Percent), y=a$Mid, x=rep.int( 6,length(a[,1])),hjust=1.1,vjust=1.5,size=3.2,col=factor(b$Col))

png(filename = "Slope20.png",
    width = 8, height = 4, units = "in", pointsize = 10,
    bg = "transparent",  res = 600)
p
dev.off()

#### You have to monkey with the margins on these
## to make bigger or smaller pies
### Make some pie graphs
dat<-b
group2<-group
by<-"group"
totals<-"year1"

dat$group<-factor(dat$group,levels=rev(group))
#ggpie <- function (dat, by, totals) {
p1<-  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
  geom_bar(stat='identity', color='black') +
  #geom_text(aes(x = prop.table(b$year2), y = pos,label = dat$group)) +
  scale_fill_brewer(palette="Set2") +
  guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
  coord_polar(theta='y') +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y=dat$pos,label = dat$group)) +
  #geom_text(aes(x = , y=1,label = dat$group)) +
  
  theme(axis.ticks=element_blank(),
        plot.margin = unit(c(1,-1 ,0, 1), "cm"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(colour='black',size=9.),
        axis.title=element_blank(),
        legend.position="none")+
  theme(panel.background = element_rect(fill = "white"))+ggtitle("2017")+
  theme(plot.title = element_text(size = rel(1.5),color="blue"))+
  scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=group2) 

totals<-"year2"
#ggpie <- function (dat, by, totals) {

b$pos <- with(dat, ave(year2, group, FUN = function(x) cumsum(x) - 0.5*x))
dat<-b
dat$group<-factor(dat$group,levels=rev(group))

p2<-  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
  geom_bar(stat='identity', color='black') +
  #geom_text(aes(x = prop.table(b$year2), y = pos,label = dat$group)) +
  scale_fill_brewer(palette="Set2") +
  guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
  coord_polar(theta='y') +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y=dat$pos,label = dat$group)) +
  ##geom_text(aes(x = , y=1,label = dat$group)) +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y = zzz, label = group2)) +
  theme(axis.ticks=element_blank(),
        plot.margin = unit(c(1,1 ,0,0), "cm"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(colour='black',size=9),
        axis.title=element_blank(),
        legend.position="none")+
  theme(panel.background = element_rect(fill = "white"))+ggtitle("2018")+
  theme(plot.title = element_text(size = rel(1.5),color="blue"))+
  scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=group2)


library(grid)
library(gridExtra)
png(filename = "2pie17.png",
    width = 8, height = 5, units = "in", pointsize =2 ,
    bg = "white",  res = 600)
grid.arrange(p1, p2, ncol = 2,top = textGrob("Group ABCs for 2018 and 2020",gp=gpar(fontsize=22,font=3)))

dev.off()

### do pies for catch
year1<-c(1354969,243867,135350,89910,36909,54485,36116)
year2<-c(1357937,209047,125620,77136,37991,63657,35909)
group<-c("Pollock", "P. Cod", "YFS","Non-YFS Flats","Rockfish","Atka","Other")
B_group<-c("Pollock", "P. Cod", "YFS","Non-YFS Flats","Rockfish","Atka","Other")
B_ABC_2017<-c(2887361,260500,260800,356647,59349,87200,90633)
B_ABC_2018<-c(2684088,209500,277500,344457,57958,92000,86622) 
B_ABC_2019<-c(2353197,172200,263200,325084,65594,68500,90299)

B_CATCH_2017<-c(1360967,235072,132266,78843,41440,64449,39406)
B_CATCH_2018<-c(1378544,183511,124519,79254,40644,67954,35280)

a<-data.frame(year1,year2,group)
a<-data.frame(B_ABC_2018,B_ABC_2019,B_group)
b<-a

dat<-b
group2<-group
by<-"group"
totals<-"year1"

dat$group<-factor(dat$group,levels=rev(group))
#ggpie <- function (dat, by, totals) {
p1<-  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
  geom_bar(stat='identity', color='black') + scale_fill_brewer(palette="Set2") +
  guides(fill=guide_legend(override.aes=list(colour=NA))) + coord_polar(theta='y') +
  theme(axis.ticks=element_blank(), plot.margin = unit(c(1,-0.5 ,0, 1), "cm"),
        axis.text.y=element_blank(), axis.text.x=element_text(colour='black',size=9.),
        axis.title=element_blank(), legend.position="none")+
  theme(panel.background = element_rect(fill = "white"))+ggtitle("2018")+
  theme(plot.title = element_text(size = rel(1.5),color="blue"))+
  scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=group2)  #+
  # removes black borders from legend
  #geom_text(aes(x = prop.table(b$year2), y = pos,label = dat$group)) +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y=dat$pos,label = dat$group)) +
  #geom_text(aes(x = , y=1,label = dat$group)) +
  p1

totals<-"year2"
#ggpie <- function (dat, by, totals) {

b$pos <- with(dat, ave(year2, group, FUN = function(x) cumsum(x) - 0.5*x))
dat<-b
dat$group<-factor(dat$group,levels=rev(group))

p2<-  ggplot(dat, aes_string(x=factor(1), y=totals, fill=by)) +
  geom_bar(stat='identity', color='black') + scale_fill_brewer(palette="Set2") +
  guides(fill=guide_legend(override.aes=list(colour=NA))) + coord_polar(theta='y') +
  theme(axis.ticks=element_blank(), plot.margin = unit(c(1,0.5 ,0,0), "cm"), axis.text.y=element_blank(),
        axis.text.x=element_text(colour='black',size=9), axis.title=element_blank(), legend.position="none")+
  theme(panel.background = element_rect(fill = "white"))+ggtitle("2019")+
  theme(plot.title = element_text(size = rel(1.5),color="blue"))+
  scale_y_continuous(breaks=cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=group2)
  #geom_text(aes(x = prop.table(b$year2), y = pos,label = dat$group)) +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y=dat$pos,label = dat$group)) +
  ##geom_text(aes(x = , y=1,label = dat$group)) +
  #geom_text(aes(x = c(1,0.8,0.7,0.6,0.5,0.4,0.3), y = zzz, label = group2)) +
p2

png(filename = "2pie18_catch.png", width = 8, height = 5, units = "in", pointsize =2 ,
    bg = "white",  res = 600) grid.arrange(p1, p2, ncol = 2,top = textGrob("ABCs for 2018 and 2019",gp=gpar(fontsize=22,font=3)))

dev.off()

### end for now
dev.off() 
