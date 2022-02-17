### Script for making some Council meeting graphs 12/2/2016, hanselman
## start off by setting to source location
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)

my_inc <- 4

#Get some data tables from AFSC
#library(RODBC)
#database_afsc="afsc"
#username_afsc="hanselmd"
#password_afsc=""
#channel=odbcConnect(database_afsc,uid=username_afsc,pwd=password_afsc,believeNRows=FALSE)

# Get SARA data
########## I don't remember what this did or how I got the .csvs I read in below
#ts<-sqlTables(channel,schema="AGREIG")[,3]
#write(ts,"ts.csv")
#for(i in 1:length(ts)) {
#assign(tolower(ts[i]),sqlFetch(channel,paste("AGREIG.",ts[i],sep=""))) 
#write.csv(get(tolower(ts[i])),file=paste(tolower(ts[i]),".csv",sep=""),row.names=FALSE) }

### Read files back in for offline mode
#ts<-as.character(read.csv("ts.csv",header=F)[,1])
#ts<-ts[-c(4,5,6,7,8,9,10,14,15,20)]
#for(i in 1:length(ts)) {
#assign(tolower(ts[i]),read.csv(file=paste(tolower(ts[i]),".csv",sep=""))) }

sarastocknames<-read.csv("data/sarastocknames.csv",header=T)
#source("R/SARA_R.R")
SARA()
#### make graphs  

sara_stock<-sara_stock[sara_stock$ASSESSYEAR==2021,]
mod_stats<-mod_stats[mod_stats$STOCKJOIN%in%unique(sara_stock$STOCKJOIN),]


m2<-merge(mod_stats,mod_stock,all.x=TRUE)
m2<-merge(m2,sara_stock,all.x=TRUE)
m2<-merge(m2,sarastocknames,all.x=TRUE)
#m2
### Set up some stuff for recruitment graphs
m3<-m2[!is.na(m2$RECRUITMENT),]
m3<-m3[m3$FISHERYYEAR>1976,]
by.type <- group_by(m3, STOCKJOIN)
d <- summarise(by.type, num.types = n(), mu = mean(RECRUITMENT,na.rm=T),sd=sd(RECRUITMENT,na.rm=T))
e<-merge(by.type,d)
f<- group_by(e,STOCKJOIN)%>%mutate(Anomaly=(RECRUITMENT-mu)/sd)
#e
g<-f
g$cola<-"neg"
#g$Anomaly[1]<-0.001
g[g$Anomaly>0,]$cola<-"pos"
g$cola<-factor(g$cola)

stocks<-unique(m3$STOCKJOIN)
## start plots
path<-getwd() 
plotpath<-paste0(path,"/plots")
if (file.exists(plotpath)){
  setwd(plotpath)} else {
    dir.create(plotpath)
    setwd(plotpath)}

### Recruitments 
plotpath<-paste0(path,"/plots/single")
if (file.exists(plotpath)){
  setwd(plotpath)} else {
  dir.create(plotpath)
  setwd(plotpath)}
 ### Plot them all
  ### just call crabs age 0
#  g[is.na(g$RECRUIT_AGE_OR_SIZE),]$RECRUIT_AGE_OR_SIZE<-0

 for (i in 1:length(stocks)) {
   h<-g[g$STOCKJOIN==stocks[i],] 

   stocklab<-paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME,"AGE",h$RECRUIT_AGE_OR_SIZE,"Recruitment"))))
   p<-ggplot(h, aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE, y=RECRUITMENT*RECRUIT_MULTIPLIER/1000000)) +
   geom_bar(stat = "identity", aes(fill=cola),color="gray",width=1.0,size=0.35)+ 
   guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+
     scale_x_continuous(breaks = seq(min(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], max(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], by = my_inc))+
     scale_fill_manual(values=c("red","dark blue"))+ 
   #scale_color_manual(values=c("red","dark blue"))+ 
    labs(y="Recruitment (millions)", x="Year Class")+
   geom_line(aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE,y=mu*RECRUIT_MULTIPLIER/1000000),color="dark green",alpha=0.6,size=1.1,linetype=1) 
      png(paste(stocks[i],"_rec.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
   print(p)
   dev.off() 
 }
 
  
  ### Do total biomass
  for (i in 1:length(stocks)) {
    h<-g[g$STOCKJOIN==stocks[i],] 
    stocklab<-paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME,"Total Biomass"))))
    p<-ggplot(h, aes(x=FISHERYYEAR, y=TOTALBIOMASS/1000)) +geom_line(size=1.8,color="dark blue")+
       guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
 #     geom_line(aes(x=FISHERYYEAR,y=BMSY),color="red",size=1,linetype=2)+ labs(y="Total Biomass (kilotons)")+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = my_inc))+
      geom_line(aes(x=FISHERYYEAR,y=mean(TOTALBIOMASS/1000,na.rm=T)),color="dark green",size=1,linetype=2) +
      annotate("text", label = "Mean", x = 1985, y = 0.9*mean(h$TOTALBIOMASS/1000,na.rm=T), size = 5, colour = "dark green")+
    labs(y="Total Biomass (kilotons)", x="Year")
      png(paste(stocks[i],"_tb.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
    print(p)
    dev.off() 
  }
  
  ## Spawning biomass by stock
  
  for (i in 1:length(stocks)) {
    h<-g[g$STOCKJOIN==stocks[i],] 
    
    stocklab<-paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME,"Spawning Biomass"))))
    p<-ggplot(h, aes(x=FISHERYYEAR, y=SPAWNBIOMASS/1000)) +geom_line(size=1.8,color="dark blue")+
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      geom_line(aes(x=FISHERYYEAR,y=BMSY/1000),color="red",size=1,linetype=2)+ labs(x="Year",y="Spawning Biomass (kilotons)")+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = my_inc))+
      annotate("text", label = "italic(B)[MSY]", x = 1990, y = 0.85*h$BMSY/1000, size = 5, colour = "red",parse=TRUE)
    png(paste(stocks[i],"_SSB.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
    print(p)
       dev.off()
  }
  
  ### Do total catch
  for (i in 1:length(stocks)) {
    h<-g[g$STOCKJOIN==stocks[i],] 
    
    stocklab<-paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME,"Total Catch"))))
    p<-ggplot(h, aes(x=FISHERYYEAR, y=TOTALCATCH/1000))  +
      geom_bar(stat = "identity", fill="dark blue",color="gray",width=1.0,size=0.35,alpha=0.8)+ 
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      #     geom_line(aes(x=FISHERYYEAR,y=BMSY),color="red",size=1,linetype=2)+ labs(y="Total Biomass (kilotons)")+
      geom_line(aes(x=FISHERYYEAR,y=mean(TOTALCATCH/1000,na.rm=T)),color="green",size=1.1,linetype=2) +
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = my_inc))+
      #    annotate("text", label = "Mean", x = 1985, y = 0.9*mean(h$TOTALCATCH/1000,na.rm=T), size = 5, colour = "dark green")+
      labs(y="Total Catch (kilotons)", x="Year")
        png(paste(stocks[i],"_catch.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
    print(p)
    dev.off() 
  }

### recruitment with different colors
for (i in 1:length(stocks)) {
  h<-g[g$STOCKJOIN==stocks[i],] 
  
  stocklab<-paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME,"AGE",h$RECRUIT_AGE_OR_SIZE,"Recruitment"))))
  p<-ggplot(h, aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE, y=RECRUITMENT*RECRUIT_MULTIPLIER/1000000)) +
    geom_bar(stat = "identity", aes(fill=Anomaly),color="gray",width=1.0,size=0.35)+ 
    guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+
    scale_x_continuous(breaks = seq(min(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], max(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], by = my_inc))+
    scale_fill_gradient(low="dark blue",high="green")+ 
    #scale_color_manual(values=c("red","dark blue"))+ 
    labs(y="Recruitment (millions)", x="Year Class")+
    geom_line(aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE,y=mu*RECRUIT_MULTIPLIER/1000000),color="dark green",alpha=0.6,size=1.1,linetype=1)
  png(paste(stocks[i],"_rec_grad.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
  print(p)
  dev.off() 
}


  ### do them as one grid
  panelpath<-paste0(path,"/plots//panel_plots")
  if (file.exists(panelpath)){
    setwd(panelpath)} else {
      dir.create(panelpath)
      setwd(panelpath)}
  ### Do total catch
  for (i in 1:length(stocks)) {
    h<-g[g$STOCKJOIN==stocks[i],] 
    
    stocklab<-paste("Total Catch")
    p1<-ggplot(h, aes(x=FISHERYYEAR, y=TOTALCATCH/1000))  +
      geom_bar(stat = "identity", fill="dark blue",color="gray",width=1.0,size=0.35,alpha=0.8)+ 
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      #     geom_line(aes(x=FISHERYYEAR,y=BMSY),color="red",size=1,linetype=2)+ labs(y="Total Biomass (kilotons)")+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = 6))+
      geom_line(aes(x=FISHERYYEAR,y=mean(TOTALCATCH/1000,na.rm=T)),color="green",size=1.1,linetype=2) +
      #    annotate("text", label = "Mean", x = 1985, y = 0.9*mean(h$TOTALCATCH/1000,na.rm=T), size = 5, colour = "dark green")+
      labs(y="Total Catch (kilotons)", x="Year")
  
    stocklab<-paste("Total Biomass")
    p2<-ggplot(h, aes(x=FISHERYYEAR, y=TOTALBIOMASS/1000)) +geom_line(size=1.8,color="dark blue")+
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      #     geom_line(aes(x=FISHERYYEAR,y=BMSY),color="red",size=1,linetype=2)+ labs(y="Total Biomass (kilotons)")+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = 6))+
      geom_line(aes(x=FISHERYYEAR,y=mean(TOTALBIOMASS/1000,na.rm=T)),color="dark green",size=1.1,linetype=2) +
      annotate("text", label = "Mean", x = 1985, y = 0.9*mean(h$TOTALBIOMASS/1000,na.rm=T), size = 5, colour = "dark green")+
      labs(y="Total Biomass (kilotons)", x="Year")

    stocklab<-paste("Spawning Biomass")
    p3<-ggplot(h, aes(x=FISHERYYEAR, y=SPAWNBIOMASS/1000)) +geom_line(size=1.8,color="dark blue")+
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR), max(h$FISHERYYEAR), by = 6))+
      geom_line(aes(x=FISHERYYEAR,y=BMSY/1000),color="red",size=1.1,linetype=2)+ labs(x="Year",y="Spawning Biomass (kilotons)")+
      annotate("text", label = "italic(B)[MSY]", x = 1990, y = 0.85*h$BMSY/1000, size = 5, colour = "red",parse=TRUE)
    #   png(paste(stocks[i],"_SSB.png",sep=""), width = 6, height = 5, units = 'in', res = 300)
  
    stocklab<-paste("Age",h$RECRUIT_AGE_OR_SIZE,"Recruitment")
    p4<-ggplot(h, aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE, y=RECRUITMENT*RECRUIT_MULTIPLIER/1000000)) +
      geom_bar(stat = "identity", aes(fill=cola),color="gray",width=1.0,size=0.35)+ 
      guides(fill=FALSE)+ theme_bw(base_size=15)+ggtitle(stocklab)+expand_limits(y=0)+
      scale_x_continuous(breaks = seq(min(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], max(h$FISHERYYEAR)-h$RECRUIT_AGE_OR_SIZE[1], by = 6))+
      scale_fill_manual(values=c("red","dark blue"))+ 
      labs(y="Recruitment (millions)", x="Year Class")+
      geom_line(aes(x=FISHERYYEAR-RECRUIT_AGE_OR_SIZE,y=mu*RECRUIT_MULTIPLIER/1000000),color="dark green",alpha=0.6,size=1.1,linetype=1)

    
    
  #  grid.arrange(p1, p2, p3, p4, ncol=2, nrow =2, top=textGrob(paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME)))),gp=gpar(fontsize=20,font=3)))
  
  
 yy<-arrangeGrob(p1, p2, p3, p4, ncol=2, nrow =2, top=textGrob(paste(h$REGION,tools::toTitleCase(tolower(paste(h$STOCK_NAME)))),gp=gpar(fontsize=20,font=3)))
   # use below for no overall titles
 # yy<-arrangeGrob(p1, p2, p3, p4, ncol=2, nrow =2)
  ggsave(paste(stocks[i],"_4_title.png",sep=""),yy, width = 10, height = 7, units = 'in', dpi = 300,bg="transparent")
     
     #   dev.off()
  }
  
 
 # Tier 5 plots
  plotpath<-paste0(path,"/plots/tier5")
  if (file.exists(plotpath)){
    setwd(plotpath)} else {
      dir.create(plotpath)
      setwd(plotpath)}
  y<-merge(sara_series,sara_stock,all.x=TRUE)
  y2<-merge(y,sarastocknames,all.x=TRUE)

## Do trawl biomasses for AI
  z<-y2[grep("AI_trawl",y2$SERIESNAME),]
  z2<-z[grep("2020",z$STOCKJOIN),]
  z3<-z2 # storing variable for possibly plotting trawl and re together
  bs<-15
  stocks2<-unique(z2$STOCKJOIN)
  for(i in 1:length(stocks2)){
  
  zz<-z2[z2$STOCKJOIN==stocks2[i],] 
  zz$SERIESAMT<-zz$SERIESAMT/1000
  stocklab<-paste(zz$REGION,tools::toTitleCase(tolower(paste(zz$STOCK_NAME,"AI Survey Biomass"))))
  p<-ggplot(zz)+geom_line(aes(x=SERIESYEAR,y=SERIESAMT),size=1.5,colour="blue")+geom_point(aes(x=SERIESYEAR,y=SERIESAMT),colour="red",size=5)+expand_limits(y=0)+
#    geom_ribbon(aes(x=year,ymin=lci, ymax=uci),fill="blue",alpha=0.2)+
    theme_bw(base_size=bs)+   labs(y="Survey Biomass (kt)", x="Year")+
    ggtitle(stocklab)
   png(paste(stocks2[i],"_trwlbio_AI.png",sep=""), width = 10, height = 7, units = 'in', res = 300,bg="transparent")
   print(p)
   dev.off()
    }

## Do trawl biomasses for EBS trawl
z<-y2[grep("EBS_trawl",y2$SERIESNAME),]
z2<-z[grep("2020",z$STOCKJOIN),]
z5<-z2 # storing variable for possibly plotting trawl and re together
bs<-20
stocks2<-unique(z2$STOCKJOIN)
for(i in 1:length(stocks2)){
  
  zz<-z2[z2$STOCKJOIN==stocks2[i],] 
  zz$SERIESAMT<-zz$SERIESAMT/1000
  stocklab<-paste(zz$REGION,tools::toTitleCase(tolower(paste(zz$STOCK_NAME,"EBS Survey Biomass"))))
  p<-ggplot(zz)+geom_line(aes(x=SERIESYEAR,y=SERIESAMT),size=1.5,colour="blue")+geom_point(aes(x=SERIESYEAR,y=SERIESAMT),colour="red",size=5)+expand_limits(y=0)+
    #    geom_ribbon(aes(x=year,ymin=lci, ymax=uci),fill="blue",alpha=0.2)+
    theme_bw(base_size=bs)+   labs(y="Survey Biomass (kt)", x="Year")+
    ggtitle(stocklab)
  png(paste(stocks2[i],"_trwlbio_EBS.png",sep=""), width = 10, height = 7, units = 'in', res = 300,bg="transparent")
  print(p)
  dev.off()
}

## Do trawl biomasses for GOA trawl
z<-y2[grep("GOA_trawl",y2$SERIESNAME),]
z2<-z[grep("2020",z$STOCKJOIN),]
z7<-z2 # storing variable for possibly plotting trawl and re together
bs<-20
stocks2<-unique(z2$STOCKJOIN)
for(i in 1:length(stocks2)){
  
  zz<-z2[z2$STOCKJOIN==stocks2[i],] 
  zz$SERIESAMT<-zz$SERIESAMT/1000
  stocklab<-paste(zz$REGION,tools::toTitleCase(tolower(paste(zz$STOCK_NAME,"EBS Survey Biomass"))))
  p<-ggplot(zz)+geom_line(aes(x=SERIESYEAR,y=SERIESAMT),size=1.5,colour="blue")+geom_point(aes(x=SERIESYEAR,y=SERIESAMT),colour="red",size=5)+expand_limits(y=0)+
    #    geom_ribbon(aes(x=year,ymin=lci, ymax=uci),fill="blue",alpha=0.2)+
    theme_bw(base_size=bs)+   labs(y="Survey Biomass (kt)", x="Year")+
    ggtitle(stocklab)
  png(paste(stocks2[i],"_trwlbio_GOA.png",sep=""), width = 10, height = 7, units = 'in', res = 300,bg="transparent")
  print(p)
  dev.off()
}

## Put all trawls together
z<-y2[grep("trawl",y2$SERIESNAME),]
z2<-z[grep("2020",z$STOCKJOIN),]
z3<-z2 # storing variable for possibly plotting trawl and re together
bs<-20

## Do random_effects
z<-y2[grep("random",y2$SERIESNAME),]
z2<-z[grep("2020",z$STOCKJOIN),]
bs<-20
stocks2<-unique(z2$STOCKJOIN)
for(i in 1:length(stocks2)){
  
  zz<-z2[z2$STOCKJOIN==stocks2[i],] 
  zz$SERIESAMT<-zz$SERIESAMT/1000
  stocklab<-paste(zz$REGION,tools::toTitleCase(tolower(paste(zz$STOCK_NAME,"Survey Biomass"))))
  p<-ggplot(zz)+geom_line(aes(x=SERIESYEAR,y=SERIESAMT),size=1.5,colour="blue")+geom_point(aes(x=SERIESYEAR,y=SERIESAMT),colour="red",size=5)+expand_limits(y=0)+
    #    geom_ribbon(aes(x=year,ymin=lci, ymax=uci),fill="blue",alpha=0.2)+
    theme_bw(base_size=bs)+   labs(y="Survey Biomass (kt)", x="Year")+
    ggtitle(stocklab)
  png(paste(stocks2[i],"_re.png",sep=""), width = 10, height = 7, units = 'in', res = 300,bg="transparent")
  print(p)
  dev.off()
}

###

