#'
#' Actual function that reads in existing Tier 5 datafiles in data subdirectory, then 
#' appends the results from the rawdata subdirectory
#' 
#' Draft
#'
#' SARA report files into dataframes 
#'
#' @return  update from last year's files
#' @export
SARA_readwrite_form <- function(){
flist=list.files("tier456files",pattern="*.dat");
for (fnam in flist){
   print(fnam)
  fn <- paste0("tier456files/",fnam)
   skipp=0			# skipp indicates the header lines to be skipped
   myfile <- scan(fn, what="character", skip=skipp,flush=T,blank.lines.skip=FALSE, quiet=F);
   b=as.numeric(myfile[8]);
   bn=as.numeric(myfile[9]);
   bx=as.numeric(myfile[10]);
   notes=" ";
   notes=(scan(fn, skip=skipp+12, nlines=1,blank.lines.skip=FALSE,quiet=TRUE,flush=T, what="character"));
   if (length(notes) == 0) notes=" ";

   # SARASTOCK
   tjoin = paste(myfile[1], myfile[2], myfile[3], sep = "");
   t1 = data.frame(STOCK= myfile[1], REGION=myfile[2],
             ASSESSYEAR=as.numeric(myfile[3]),
             STOCKJOIN= tjoin,
             TIER=myfile[4],
             TIER2=NA,
             UPDATE_TYPE=myfile[6],
             FLIMIT=myfile[7],
             BESTB=b,
             MINB=bn,
             MAXB=bx,
             ABUNDMETH=myfile[11],
             STOCKNOTES=notes);
   #sqlSave(channel, t1, tablename="AGREIG.SARASTOCK", append=TRUE, rownames = FALSE, verbose = FALSE);
   sara_stock <- rbind(sara_stock,t1)

# Get vectors names
skipp=13			# skipp indicates the header lines to be skipped
ifile <- scan(fn, what="character", skip=skipp,blank.lines.skip=FALSE, flush=TRUE, quiet=F)
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
        tt<-strsplit(vnam2,split="#")
        tmp<-rep(0,length(vnam2))
        for(i in 1:length(tmp))
                if(length(tt[[i]])>1)
                        tmp[i]<-1
        vnam2<-vnam2[tmp==1]
        labnum<-match(vnam2,ifile)
        ifilet<-strsplit(ifile,split="#")
        vnamt<-strsplit(vnam2,split="#")
        for(i in 1:length(vnamt))
                vnam2[i]<-vnamt[[i]][2]
        for(i in 1:length(ifile)){
           if (length(ifilet[[i]])>0)
                ifile[i]<-ifilet[[i]][length(ifilet[[i]])]
           else ifile[i]<-"";
        }
        vnam2<-na.omit(vnam2)
        nv <- length(vnam2) #number of objects
        A <- list()
        ir <- 0
        vnam<-vnam2
  # Catch series
      ir <- match('TOTALCATCH', ifile) # find the matching name in the ifile set
      catun = as.numeric(ifile[skipp+ir]);
      catun <- NA
      catun <- as.numeric(scan(fn, skip=skipp+ir,blank.lines.skip=FALSE, nlines=1,quiet=FALSE, what=""));
      yr <- NA
      yr <- as.double(scan(fn, skip=skipp+ir+1,blank.lines.skip=FALSE, nlines=1,quiet=FALSE, what=""));
      amt <- NA
      amt <- as.double(scan(fn, skip=skipp+ir+2,blank.lines.skip=FALSE, nlines=1,quiet=TRUE, what=""))
      ylen= length(yr);
      alen= length(amt);
      if (ylen != alen){ cat(tjoin,"Catch years do not match amounts.\n", sep=" ")
      } else if (alen == 0) {cat(tjoin,"Catch missing.\n", sep=" ")
      } else {
        amt = amt * catun[1];            # multiply series by units multiplier
        if (yr[ylen] == t1$ASSESSYEAR){  # delete assessment year catch since it is not complete
          yr=yr[-ylen];
          amt=amt[-ylen];
        }
        t2 =  data.frame(STOCKJOIN= tjoin,SERIESNAME="TOTALCATCH",
                SERIESYEAR=yr,
                SERIESAMT=amt,
                AMT_MULTIPLIER=1,
                AMT_VARIANCE=NA);
       #sqlSave(channel, t2, tablename="AGREIG.SARASERIES", append=TRUE, rownames = FALSE, verbose = FALSE);
        sara_series <- rbind(sara_series,t2)
      }
  # Abundance series
      ir <- match('ABUNDANCE', ifile) # find the matching name in the ifile set
      abuun = as.numeric(ifile[skipp+ir]);
      abuun <- NA
      abuun <- as.numeric(scan(fn, skip=skipp+ir,blank.lines.skip=FALSE, nlines=1,quiet=FALSE, what=""));
      yr <- NA
      yr <- as.double(scan(fn, skip=skipp+ir+1,blank.lines.skip=FALSE, nlines=1,quiet=FALSE, what=""));
      amt <- NA
      amt <- as.double(scan(fn, skip=skipp+ir+2,blank.lines.skip=FALSE, nlines=1,quiet=TRUE, what=""))
      ylen= length(yr);
      alen= length(amt);
      if (ylen != alen){ cat(tjoin,"Abundance years do not match amounts.\n", sep=" ")
      } else if (alen == 0) {cat(tjoin,"Abundance missing.\n", sep=" ")
      } else if (is.na(amt)) {cat(tjoin,"No Abundance.\n", sep=" ")
      } else {
        amt = amt * abuun[1];            # multiply series by units multiplier
        t2 =  data.frame(STOCKJOIN= tjoin,SERIESNAME=t1$ABUNDMETH,
                SERIESYEAR=yr,
                SERIESAMT=amt,
                AMT_MULTIPLIER=1,
                AMT_VARIANCE=NA);
        #sqlSave(channel, t2, tablename="AGREIG.SARASERIES", append=TRUE, rownames = FALSE, verbose = FALSE);
        sara_series <- rbind(sara_series,t2)
      }
   # Survey series
   ir <- match('SURVEYNAMES', ifile) # find the matching name in the ifile set
   surveys=NA
   surveys=(scan(fn, skip=skipp+ir-1, nlines=1,blank.lines.skip=FALSE, what="character"));
   if (length(surveys) > 1){
     surnam = surveys[2:6];
     surun=as.integer(scan(fn, skip=skipp+ir, nlines=1,blank.lines.skip=FALSE,quiet=TRUE, what=""));
     sur=c('SURVEY1','SURVEY2','SURVEY3','SURVEY4','SURVEY5');
     #i=3;
     for (i in 1:5){
       if (surnam[i] != "none"){
         ir <- match(sur[i], ifile) # find the matching name in the ifile set
         yr <- NA
         yr <- as.double(scan(fn, skip=skipp+ir, nlines=1,blank.lines.skip=FALSE,quiet=FALSE, what=""));
         amt <- NA
         amt <- as.double(scan(fn, skip=skipp+ir+1, nlines=1,blank.lines.skip=FALSE,quiet=TRUE, what=""));
         varr <- NA
         varr <- as.double(scan(fn, skip=skipp+ir+2, nlines=1,blank.lines.skip=FALSE,quiet=TRUE, what=""));
         if (is.numeric(amt)){ 
           ylen= length(yr);
           alen= length(amt);
           if (ylen != alen){
              cat(tjoin,"Survey years do not match amounts.\n", sep=" ")
           } else if (alen == 0){ cat(tjoin,"Survey data missing.\n", sep=" ")
           } else {
             amt = amt * as.numeric(surun[i]);
             t3 =  data.frame(STOCKJOIN= tjoin,SERIESNAME=surnam[i],
                   SERIESYEAR=yr,
                   SERIESAMT=amt,
                   AMT_MULTIPLIER=1,
                   AMT_VARIANCE=varr);
             #sqlSave(channel, t3, tablename="AGREIG.SARASERIES", append=TRUE, rownames = FALSE, verbose = FALSE);
             sara_series <- rbind(sara_series,t3)
           }
         }
       }
     }
   }
}
}