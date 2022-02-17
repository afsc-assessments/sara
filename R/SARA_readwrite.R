#'
#' SARA report files into dataframes 
#' @export
#' @return  update from last year's files
SARA_readwrite <- function(){
flist=list.files("data-raw",pattern="*.dat");
for (fnam in flist){
  print(fnam)
  fn <- paste0("data-raw/",fnam)
  skipp=0			# skipp indicates the header lines to be skipped
  myfile <- scan(fn, what="character", skip=skipp,flush=T,blank.lines.skip=FALSE, quiet=F)

  # SARASTOCK
  sjoin = paste(myfile[1], myfile[2], myfile[3], sep = "");
  #abm = "age_model";
	#abm = "size_model";
  s1 = data.frame(STOCK= myfile[1], REGION=myfile[2],
	ASSESSYEAR=as.numeric(myfile[3]),
	STOCKJOIN= sjoin,
	TIER=myfile[4],
	TIER2=myfile[5],
	UPDATE_TYPE=myfile[6],
	FLIMIT='',
	BESTB=-999,
	MINB=as.numeric(myfile[7]),
	MAXB=as.numeric(myfile[8]),
	ABUNDMETH=NA,
	STOCKNOTES='');
  crabflag = 0;
  if (s1$STOCK == "TANNER" | s1$STOCK == "SNOWCRAB" | s1$STOCK == "REDKING"){crabflag= 1}

  xlines = as.numeric(myfile[12]);
  if (is.na(xlines)) {xlines = 0;}
  flines = as.numeric(myfile[13]);
  if (is.na(flines)) {flines = 0;}

  # MODSTOCK
  m1 = data.frame(
             STOCKJOIN= sjoin,
             RECRUIT_MULTIPLIER=as.numeric(myfile[14]),             
             RECRUIT_AGE_OR_SIZE=as.numeric(myfile[15]),
             AGE_OR_MMCW_PLUS=as.numeric(myfile[16]),
	     FMORT_TYPE=myfile[17],
	     FMORT_SOURCE=myfile[18],
	     FMORT_RANGE=myfile[19],
             MODEL=myfile[10],
             VERSION=myfile[11],
             BMSY=as.numeric(myfile[9]));
  fisherylist=(scan(fn, skip=skipp+20, nlines=1,quiet=TRUE, what="character"));

  # Get vectors names
  skipp <- 21			# skipp indicates the header lines to be skipped
  ifile <- scan(fn, what="character", skip=skipp,flush=T,blank.lines.skip=FALSE, quiet=F)
  iflex<-which(is.na(ifile))
  idx <- sapply(as.double(ifile), is.na)
  datnum<-which(idx==FALSE)
  vnam <- ifile[idx] #list names
  # remove vnam objects that are simply commented out numbers
  tmp<-rep(0,length(vnam))
  tt<-strsplit(vnam,split="#")
        for(i in 1:length(tmp))
          if(is.na(as.numeric(tt[[i]][2])))
            tmp[i]<-1
  vnam2<-vnam[tmp==1]
  tt   <-strsplit(vnam2,split="#")
  tmp  <-rep(0,length(vnam2))
  for(i in 1:length(tmp))
    if(length(tt[[i]])>1)
      tmp[i]<-1
  vnam2  <-vnam2[tmp==1]
  labnum <-match(vnam2,ifile)
  ifilet <-strsplit(ifile,split="#")
  vnamt  <-strsplit(vnam2,split="#")
  for(i in 1:length(vnamt))
    vnam2[i]<-vnamt[[i]][2]
  for(i in 1:length(ifile)){
    if (length(ifilet[[i]])>0)
      ifile[i]<-ifilet[[i]][length(ifilet[[i]])]
    else 
      ifile[i]<-"";
  }
  vnam2 <-na.omit(vnam2)
  nv    <- length(vnam2) #number of objects
  A     <- list()
  ir    <- 0
  vnam  <- vnam2

  # MODSTATS
  ir  <- match('FISHERYYEAR', ifile) # find the matching name in the ifile set
  dum <- NA
  dum <- as.double(scan(fn, skip=skipp+ir, nlines=1,quiet=TRUE, what="numeric"))
  rlen= length(dum)
  recmatrix=matrix(data=NA,nrow=rlen,ncol=6)
  recmatrix[1:rlen,1]=dum
  cnam=c('FISHERYYEAR','RECRUITMENT','SPAWNBIOMASS','TOTALBIOMASS','TOTFSHRYMORT','TOTALCATCH')
  for (i in 2:6){
    cat(sjoin,cnam[i],"input.\n",file="", sep=" ");
    ir <- match(cnam[i], ifile) # find the matching name in the ifile set
    dum <- NA
    dum <- as.double(scan(fn, skip=skipp+ir, nlines=1,quiet=TRUE, what="numeric"))
    #if (is.element(i,c(3,4,6))) dum=dum*mtmult;
    dlen=length(dum)
    recmatrix[1:dlen,i]=dum
    if (dlen < rlen) cat("error=",sjoin,cnam[i]," fewer numbers than years.\n",file="", sep=" ");
  }
  m2 <- data.frame(STOCKJOIN= sjoin,
                FISHERYYEAR=recmatrix[1:rlen,1],
                RECRUITMENT=recmatrix[1:rlen,2],
                SPAWNBIOMASS=round(recmatrix[1:rlen,3]),
                TOTALBIOMASS=round(recmatrix[1:rlen,4]),
                TOTFSHRYMORT=round(recmatrix[1:rlen,5],6),
                  TOTALCATCH=round(recmatrix[1:rlen,6]));
  #head(m2)
    # create subset of nonzero catch years                       why????????????????n=rlen,
    # since catch for assessment year is incomplete, remove number;
    newlen=length(m2$FISHERYYEAR)
    if (m2[newlen,2] == s1$ASSESSYEAR) m2[newlen,7]=NA;
    # error check
    if (max(m2$TOTALBIOMASS) > 10000000000 && !is.na(max(m2$TOTALBIOMASS))) cat("error=",sjoin,"totbiomass too large.\n",file="", sep=" ");



  # Crab catch
  #   read in crab catch data
  if (crabflag == 1){
    crabmatrix=matrix(data=NA,nrow=rlen,ncol=4)
    crabmatrix[1:rlen,1]=m2$FISHERYYEAR;
    crnam=c('FISHERYYEAR','CRABFISHERY','POTCRABBYCATCH','TWLCRABBYCATCH')
    for (i in 2:4){
         ir <- match(crnam[i], ifile) # find the matching name in the ifile set
         dum <- NA
         dum <- as.double(scan(fn, skip=skipp+ir, nlines=1,n=rlen,quiet=TRUE, what="numeric"))
         #if (is.element(i,c(3,4,6))) dum=dum*mtmult;
         dlen=length(dum)
         crabmatrix[1:dlen,i]=dum
    }	
  # transpose CRABFISHERY, POTCRABBYCATCH, and GFCRABBYCATCH ready for store in SARASERIES
    c1  <-  data.frame(STOCKJOIN= sjoin,       SERIESNAME='CRABFISHERY', SERIESYEAR=crabmatrix[,1],SERIESAMT=crabmatrix[,2], AMT_MULTIPLIER=1,AMT_VARIANCE=NA);
    c2  <-  data.frame(STOCKJOIN= sjoin,       SERIESNAME='POTCRABBYCATCH', SERIESYEAR=crabmatrix[,1],SERIESAMT=crabmatrix[,3], AMT_MULTIPLIER=1,AMT_VARIANCE=NA);
    c3  <-  data.frame(STOCKJOIN= sjoin,       SERIESNAME='TWLCRABBYCATCH', SERIESYEAR=crabmatrix[,1],SERIESAMT=crabmatrix[,4], AMT_MULTIPLIER=1,AMT_VARIANCE=NA);
    # convert NA to zero for catch mortality calculation
    c10 <- c1;  c10[is.na(c10)] =0;
    c20 <- c2;  c20[is.na(c20)] =0;
    c30 <- c3;  c30[is.na(c30)] =0;
    # convert catch of assessment year back to NA
    if (c30[rlen,3]==s1$ASSESSYEAR) c30[rlen,4]=NA;

    if (s1$STOCK == 'REDKING')  {ca = as.numeric(c1$SERIESAMT)+(0.20 * as.numeric(c2$SERIESAMT))+(0.80 * as.numeric(c3$SERIESAMT))};
    if (s1$STOCK == 'TANNER')  {ca = as.numeric(c1$SERIESAMT)+(0.321 * as.numeric(c2$SERIESAMT))+(0.80 * as.numeric(c3$SERIESAMT))};
    m2$TOTALCATCH = ca;
  }
    
  # BESTB
    x=which(m2$FISHERYYEAR==as.numeric(s1$ASSESSYEAR));
    s1$BESTB=m2$SPAWNBIOMASS[x];
    
  # Test whether s1$BESTB is within variance.
  if (!is.na(s1$MAXB) & !is.na(s1$MINB)){
    if (s1$BESTB > s1$MAXB) cat("error=",sjoin,"BestB greater than Bmax.\n",file="", sep=" ");
    if (s1$BESTB < s1$MINB) cat("error=",sjoin,"BestB less than Bmin.\n",file="", sep=" ");
  }
  # STOCKNOTES
  ir <- match('STOCKNOTES', ifile) # find the matching name in the ifile set
  s1$STOCKNOTES=(scan(fn, skip=skipp+ir, nlines=1,quiet=TRUE, what="character")); 

  # MODFISHERY 
  if(flines > 1 &&  crabflag == 0 ){
    cnam=c('FISHERYMORT','FISHERYCATCH')
    clen= length(cnam)
    recray=array(data=NA,dim=c(rlen,clen,flines))
    for  (j in 1:flines){
       for (i in 1:clen){
         ir <- match(cnam[i], ifile) # find the matching name in the ifile set
         mum <- NA
         mum <- as.double(scan(fn, skip=skipp+ir+j-1, nlines=1,n=rlen,quiet=TRUE, what="numeric"))
         dlen=length(mum)
         recray[1:dlen,i,j]=mum
       }
       x <- fisherylist[j];
       m <- data.frame(STOCKJOIN= sjoin,
                FISHERYDESC=x,
                FISHERYYEAR=recmatrix[1:rlen,1],
                FISHERYMORT=recray[1:rlen,1,j],
                FISHERYCATCH=recray[1:rlen,2,j]);
       if (j == 1) m3 = m else m3=rbind(m3,m);
    }
    # create subset of nonzero catch years
    m3 <- m3[which(m3$FISHERYYEAR %in% m2$FISHERYYEAR),];
  }

  if(xlines > 0 &&  crabflag == 0){
  # AGEMATURE
    cnam=c('AGE','MATURITY','SPAWNWT')
    clen= length(cnam)
    ir <- match('AGE', ifile) # find the matching name in the ifile set
    dum <- NA
    dum <- as.double(scan(fn, skip=skipp+ir, nlines=1,quiet=TRUE, what="numeric"))
    rlen= length(dum)
    recmatrix=matrix(data=NA,nrow=rlen,ncol=clen)
    recmatrix[1:rlen,1]=dum;
    for (i in 2:clen){
         ir <- match(cnam[i], ifile) # find the matching name in the ifile set
         dum <- NA
         dum <- as.double(scan(fn, skip=skipp+ir, nlines=1,n=rlen, quiet=TRUE, what="numeric"))
         dlen=length(dum)
         recmatrix[1:dlen,i]=dum
    }
    a1 =  data.frame(STOCKJOIN= sjoin,
                AGE=recmatrix[1:rlen,1],
                MATURITY=recmatrix[1:rlen,2],
                SPAWNWT=recmatrix[1:rlen,3]);

  # AGENAT
    cnam=c('NATMORT','N_AT_AGE')
    clen= length(cnam)
    recray=array(data=NA,dim=c(rlen,clen,xlines))
    for  (j in 1:xlines){
       for (i in 1:clen){
         ir <- match(cnam[i], ifile) # find the matching name in the ifile set
         dum <- NA
         dum <- as.double(scan(fn, skip=skipp+ir+j-1, nlines=1, n=rlen, quiet=TRUE, what="numeric"))
         dlen=length(dum)
         recray[1:dlen,i,j]=dum
       }
       x='FEMALE';
       if (j==2) x='MALE';
       if (xlines==1) x='ALL'
       a =  data.frame(STOCKJOIN= sjoin,
                SEX=x,
                AGE=recmatrix[1:rlen,1],
                NATMORT=recray[1:rlen,1,j],
                N_AT_AGE=recray[1:rlen,2,j]);
       if (j == 1) a2 = a else a2=rbind(a2,a);
    }

  # AGESELECT
  if (sjoin %in% c("PCODEBS2015","PCODGOA2015","NROCKSOLEGOA2015","SROCKSOLEGOA2015")){
    cat("note=",sjoin,"skips AGESELECT.\n",file="", sep=" ");
  } else {
    cnam=c('FSHRY_WT_KG','SELECTIVITY')
    clen= length(cnam)
    recray=array(data=NA,dim=c(rlen,clen,xlines,flines))
    for (i in 1:clen){
       pos=0;
       for  (j in 1:xlines){
          for  (k in 1:flines){      
            ir <- match(cnam[i], ifile) # find the matching name in the ifile set
            dum <- NA
            dum <- as.double(scan(fn, skip=skipp+ir+pos, nlines=1, n=rlen, quiet=TRUE, what="numeric"));
            pos=pos+1;
            dlen=length(dum)
            recray[1:dlen,i,j,k]=dum
            x='FEMALE';
            if (j==2) x='MALE';
            if (xlines==1) x='ALL';
            a =  data.frame(STOCKJOIN= sjoin,
                SEX=x,
                FISHERYDESC=fisherylist[k],
                AGE=recmatrix[1:rlen,1],
                FSHRY_WT_KG=recray[1:rlen,1,j,k],
                SELECTIVITY=recray[1:rlen,2,j,k]);
          if (j == 1 & k == 1) a3 = a else a3=rbind(a3,a);
          }
       }
    }
  }
  }
  } 
    sara_stock <- rbind(sara_stock,s1)
    mod_stock  <- rbind(mod_stock,m1)
    mod_stats  <- rbind(mod_stats,m2)
  # SARASERIES   surveys
  ir         <- match('SURVEYDESC', ifile) # find the matching name in the ifile set
  surveylist <- scan(fn, skip=skipp+ir, nlines=1,quiet=TRUE, what="character");
  slen=length(surveylist);
  irr <- match('SURVEYMULT', ifile) # find the matching name in the ifile set
  surveymult= as.double(scan(fn, skip=skipp+irr, nlines=1,n=slen,quiet=TRUE, what="numeric"));
  surfile= ifile[-(1:irr)];
  i=1
  for (i in 1:slen)
    {
    if (surveylist[i] == 'none')  
      junk=1
    else {            
      ir   <- match(surveylist[i], surfile) # find the matching name in the ifile set
      dum  <- NA
      dum  <- as.double(scan(fn, skip=skipp+irr+ir, nlines=1,quiet=TRUE, what="numeric"));
      dum2 <- NA
      dum2 <- as.double(scan(fn, skip=skipp+irr+ir+1, nlines=1,quiet=TRUE, what="numeric"));
      dum3 <- NA
      v    <- data.frame(STOCKJOIN= sjoin, SERIESNAME=surveylist[i], SERIESYEAR=dum, SERIESAMT=dum2 * surveymult[i],
                AMT_MULTIPLIER=1 ,AMT_VARIANCE=dum3);
        sara_series  <- rbind(sara_series,v)
        print(tail(sara_series[,1:4],3))
    }
  }
}  
