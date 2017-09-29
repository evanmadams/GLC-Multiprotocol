#glc testing
require(R2jags)
require(coda)
require(reshape2)
require(car)
require(stringr)
require(chron)

#load the data from Point Blue

#load('Data/alldata_attributed.RData')

df<-read.csv('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/Data/alldata_attributed_v04122017.csv')

#load the segments we sliced each transect into

df2km<-read.csv('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/Data/alldata_attributed_v04122017_5km.csv')
m.df2km<-merge(df,df2km, by.x='X',by.y='Field1', all.x=T)

#attach the segment ID to the original data

df$segID<-m.df2km$FID_2

#load the covariate data
df.allcovs<-read.csv('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/Data/glc_allcovs_up.csv')
df.ic<-read.csv('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/Data/glc_covars_ic_up.csv')

#put the ice data on the scale of the all covs data and incorporate
df.allcovs$ic<-merge(df.allcovs, df.ic, by.x='id', by.y='points.id', all.x=T)$ic

#parse them into dynamic and static covs

df.dycovs2km<-df.allcovs[which(df.allcovs$seg=='2km'),]
df.dycovs2km<-df.dycovs2km[order(df.dycovs2km$proti, df.dycovs2km$newtransi),]
df.dycovs5km<-df.allcovs[which(df.allcovs$seg=='5km'),]
df.dycovs5km<-df.dycovs5km[order(df.dycovs5km$proti, df.dycovs5km$newtransi),]
df.dycovs10km<-df.allcovs[which(df.allcovs$seg=='10km'),]
df.dycovs10km<-df.dycovs10km[order(df.dycovs10km$proti, df.dycovs10km$newtransi),]

############old upload procedure, saving for a bit##############
#load the covariate data
# df.effort<-read.csv('Data/glc_trans_segs.csv')
# df.netcdf<-read.csv('Data/glc_netcdf_covar.csv')
# df.ic<-read.csv('Data/glc_covars_ic.csv')
# df.static<-read.csv('Data/glc3_covars_fromArc.csv')
# 
# #merge the covariate data with the effort data
# 
# m.netcdf<-merge(df.effort, df.netcdf, by='id', all.x=T)
# m.ic<-merge(df.effort, df.ic, by.x='id', by.y='points.id', all.x=T)
# m.static<-merge(df.effort, df.static, by='id', all.x=T)

#add the columns I want to the original effort data for a clean dynamic covariate data set

# df.dycovs<-cbind(df.effort, m.netcdf[,33:41], m.ic[,33], m.static[,32:33])
# 
# df.dycovs2km<-df.dycovs[which(df.dycovs$seg=='2km'),]
# df.dycovs2km<-df.dycovs2km[order(df.dycovs2km$FID, df.dycovs2km$Visit.x),]
# df.dycovs5km<-df.dycovs[which(df.dycovs$seg=='5km'),]
# df.dycovs5km<-df.dycovs5km[order(df.dycovs5km$proti, df.dycovs5km$newtransi),]
# df.dycovs10km<-df.dycovs[which(df.dycovs$seg=='10km'),]
# df.dycovs10km<-df.dycovs10km[order(df.dycovs10km$proti, df.dycovs10km$newtransi),]

#make a condensed version of the static covariates
# df.stcovs<-cbind(df.effort, m.static[,32:33])
# df.stcovs<-unique(df.stcovs[,c(4, 18, 19, 30, 32, 33)])
# df.stcovs<-df.stcovs[order(df.stcovs$proti, df.stcovs$newtransi),]
# 
# df.stcovs2km<-df.stcovs[which(df.stcovs$seg=='2km'),]
# df.stcovs5km<-df.stcovs[which(df.stcovs$seg=='5km'),]
# df.stcovs10km<-df.stcovs[which(df.stcovs$seg=='10km'),]

#change the dynamic cov linking ID so that it is unique to protocol, segment, and visit
df.dycovs5km$uLinkID<-paste(df.dycovs5km$proti, df.dycovs5km$FID, df.dycovs5km$Visit.x, sep='_')

#create a survey date variable

df$Date<-as.Date(paste0(df$SamplingEventYear, '/',df$SamplingEventMonth, '/', df$SamplingEventDay))

df$SamplingEventYear<-as.numeric(df$SamplingEventYear)
df$SamplingEventMonth<-as.numeric(df$SamplingEventMonth)
df$SamplingEventDay<-as.numeric(df$SamplingEventDay)
df$BirdCountnum<-as.numeric(as.character(df$BirdCount))

df$SamplingEventYear[which(df$Date=='2014-12-2')]<-'2013'
df$Date<-as.Date(paste0(df$SamplingEventYear, '/',df$SamplingEventMonth, '/', df$SamplingEventDay))

#replace a couple poorly named transects

df$Transect<-recode(df$Transect, " 'USGSGLWB_T1'='USGSGLWB_T01'; 'USGSGLWB_T2'='USGSGLWB_T02'; 'USGSGLWB_T3'='USGSGLWB_T03'; 'USGSGLWB_T4'='USGSGLWB_T04';
                    'USGSGLWB_T5'='USGSGLWB_T05'; 'USGSGLWB_T6'='USGSGLWB_T06'; 'USGSGLWB_T7'='USGSGLWB_T07'; 'USGSGLWB_T8'='USGSGLWB_T08'; 'USGSGLWB_T9'='USGSGLWB_T09' ")

df$TransectU<-factor(paste0(df$Transect,df$LakeRegion))

df$TransectU<-as.factor(as.character((recode(df$TransectU, " 'USGSGLWB_T28MIDLKPLATEAU'='USGSGLWBZ_T28MIDLKPLATEAU'; 'USGSGLWB_T29MIDLKPLATEAU'='USGSGLWBZ_T29MIDLKPLATEAU';
                                             'USGSGLWB_T30MIDLKPLATEAU'='USGSGLWBZ_T30MIDLKPLATEAU'; 'USGSGLWB_T31MIDLKPLATEAU'='USGSGLWBZ_T31MIDLKPLATEAU';
                                             'USGSGLWB_T32MIDLKPLATEAU'='USGSGLWBZ_T32MIDLKPLATEAU'; 'USGSGLWB_T33MIDLKPLATEAU'='USGSGLWBZ_T33MIDLKPLATEAU' "))))

#remove all data lines that don't have the correct bird count
df<-df[which(is.na(df$BirdCountnum)==F),]

#remove all data lines that don't have a proper segment ID
df<-df[which(is.na(df$segID)==F),]

#summarize count data
spp.sum<-data.frame(table(df$CommonName))
spp.sum<-spp.sum[order(spp.sum$Freq, decreasing = TRUE),]
colnames(spp.sum)<-c('Species','Count')
write.csv(spp.sum, 'GLC_spp_sum.csv')

#name all the surveys properly
df[which(df$ProtocolId=="WGLBBO_AERIALTR2"),'ProtocolId']<-"WGLBBO_AERIALTR"

for(i in 1:nrow(df)){
  if(((df[i,'SamplingEventYear']==2012) & ((df[i, "SamplingEventMonth"]==9)|(df[i, "SamplingEventMonth"]==10)|(df[i, "SamplingEventMonth"]==11)|(df[i, "SamplingEventMonth"]==12)))){
    df$Phase1[i]<-1
  }#if
  else{
    df$Phase1[i]<-0
  }
}#i

for(i in 1:nrow(df)){
  if(((df[i,'SamplingEventYear']==2013) & ((df[i, "SamplingEventMonth"]==1)|(df[i, "SamplingEventMonth"]==2)|(df[i, "SamplingEventMonth"]==3)|(df[i, "SamplingEventMonth"]==4)|(df[i, "SamplingEventMonth"]==5)))){
    df$Phase2[i]<-1
  }#if
  else{
    df$Phase2[i]<-0
  }
}#i

for(i in 1:nrow(df)){
  if((df$Phase1[i]==0)&(df$Phase2[i]==0)){
    df$Phase3[i]<-1
  }#if
  else{
    df$Phase3[i]<-0
  }
}#i

for(i in 1:nrow(df)){
  if(df$Phase1[i]==1|df$Phase2[i]==1){
    df$Phase[i]<-1
  }#if
  else{
    df$Phase[i]<-2
  }
}#i

df$ProtocolID2<-as.factor(paste0(df$ProtocolId,df$Phase))

df$ProtocolID2<-factor(df$ProtocolID2)
df$SamplingEventDt<-as.numeric(df$SamplingEventDt)

#assign each observation to each indexed survey

prots<-data.frame(ProtocolID2=levels(df$ProtocolID2))
prots$prot.idx<-1:nrow(prots)

df<-merge(df,prots,'ProtocolID2',all.x=T)

#dput(df,'glc_update.data')
#save(df, file='glc_data_update.RData')

#now lets use these protocol IDs to correctly assign distance information to each observation (protcol specific)
#levels(df$ProtocolID2)
db.bri<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="BRI_AERIALTR2"),]$DistanceBand)),"BRI_AERIALTR2"))
db.bri<-cbind(db.bri,v=c(NA,1,2,3))
db.clh1<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="CLH_AERIALTR1"),]$DistanceBand)),"CLH_AERIALTR1"))
db.clh1<-cbind(db.clh1,v=c(1,2,3,4))
db.clh2<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="CLH_AERIALTR2"),]$DistanceBand)),"CLH_AERIALTR2"))
db.clh2<-cbind(db.clh2,v=c(1,2,3,4,5,NA))
db.mi1<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="MIGOV_AERIALTR_6.5HRS1"),]$DistanceBand)),"MIGOV_AERIALTR_6.5HRS1"))
db.mi1<-cbind(db.mi1,v=c(1,3,4,5,2,NA))
db.mi2<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="MIGOV_AERIALTR_6.5HRS2"),]$DistanceBand)),"MIGOV_AERIALTR_6.5HRS2"))
db.mi2<-cbind(db.mi2,v=c(1,3,4,5,2,NA))
db.usgs1<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="USGS_GLWBSRV_200m1"),]$DistanceBand)),"USGS_GLWBSRV_200m1"))
db.usgs1<-cbind(db.usgs1,v=c(NA))
db.usgs2<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="USGS_GLWBSRV_200m2"),]$DistanceBand)),"USGS_GLWBSRV_200m2"))
db.usgs2<-cbind(db.usgs2,v=c(1,2)) #lost NA in the update
db.wglbbo1<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="WGLBBO_AERIALTR1"),]$DistanceBand)),"WGLBBO_AERIALTR1"))
db.wglbbo1<-cbind(db.wglbbo1,v=c(NA))
db.wglbbo2<-data.frame(db=paste0(levels(factor(df[which(df$ProtocolID2=="WGLBBO_AERIALTR2"),]$DistanceBand)),"WGLBBO_AERIALTR2"))
db.wglbbo2<-cbind(db.wglbbo2,v=c(NA,NA,NA,NA,1,2,3,NA))

#combine all the assignment keys together
db.key<-rbind(db.bri, db.clh1, db.clh2, db.mi1, db.mi2, db.usgs1, db.usgs2, db.wglbbo1, db.wglbbo2)

df$db_assign<-paste0(df$DistanceBand, df$ProtocolID2)

df.db<-merge(df,db.key, by.x='db_assign', by.y='db', all.x=T)

#create unique names for all transects an assign that name to each observation
trans<-data.frame(TransectU=levels(df.db$TransectU), trans.idx=1:length(levels(df.db$TransectU)))
df.db<-merge(df.db,trans,by='TransectU',all.x=T)

#let's make a transect/protocol matrix that we'll use for indexing in the MCMC
trans.p1<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==1)]))
trans.p2<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==2)]))
trans.p3<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==3)]))
trans.p4<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==4)]))
trans.p5<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==5)]))
trans.p6<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==6)]))
trans.p7<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==7)]))
trans.p8<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==8)]))
trans.p9<-levels(factor(df.db$trans.idx[which(df.db$prot.idx==9)]))

ntrans<-c(27-0,119-27,119-27,171-119,171-119,233-171,233-171,269-233,269-233)
trans.start<-c(0,27,27,119,119,171,171,233,233)

for(i in 1:9){
  for(j in 1:nrow(df.db)){
    df.db$newtransi[j]<-df.db$trans.idx[j]-trans.start[df.db$prot.idx[j]]
  }#i
}#j

#create unique transIDS across all the surveys

df.db$pt.idx<-paste(df.db$prot.idx,'_',df.db$newtransi)

#name each visit to each transect
df.db$prot_visit.idx<-paste(df.db$prot.idx,df.db$Date, sep="_")

#look at survey dates for each of the unique transects

prot.date<-dcast(df.db, prot.idx ~ Date, fun.aggregate= length)

pt.date<-dcast(df.db, pt.idx ~ Date, fun.aggregate= length)

surv.eff<-function(x){
  y<-x[x>0]
  return(length(y))
}

pt.date$visits<-as.numeric(apply(pt.date,1,surv.eff))

pt.date$prot<-as.numeric(str_sub(pt.date$pt.idx,0,1))


#NOTE USGS surveyed some areas twice in short succession. Maybe take weekly ice info to account for this???

write.csv(pt.date, 'glc_effort.csv')

effort<-read.csv('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/Data/glc_survey_effort_date.csv',head=T)
effort$date2<-as.Date(effort$Date, format='%m/%d/%Y')
effort$visit.idx<-paste(effort$Prot,effort$date2,sep='_')
effort$prot_visit.idx<-paste(effort$Prot,effort$Visit,sep='_')

#maximum number of visits per protocol
maxv<-c(5,6,4, 13, 6, 8, 9, 10, 10)

#define the distance observation bands for each survey

maxd<-c(300,568.2,568.2,825,825,200,200,400,400)

#define the number of distance breaks for each survey

db<-c(3,4,5,5,5,1,2,1,3)

#define the midpoints for each distance break for each survey

mdpt1<-c(76,150,250)
mdpt2<-c(108,208,362,518.2)
mdpt3<-c(108,208,307,413,518.2)
mdpt4<-c(25,88,175,325,625)
mdpt5<-c(25,88,175,325,625)
mdpt6<-c(100)
mdpt7<-c(76,150)
mdpt8<-c(200)
mdpt9<-c(150,250,350)

mdpt.ls<-list(mdpt1,mdpt2,mdpt3,mdpt4,mdpt5,mdpt6,mdpt7,mdpt8,mdpt9)

#combine them together into one matrix

mdpts<-matrix(NA,9,max(db))
for(i in 1:9){
  for(j in 1:db[i]){
    if(length(mdpt.ls[[i]]>0)){
      mdpts[i,j]<-mdpt.ls[[i]][j]}#if
  }#j
}#i

#define the difference between distance breaks for each survey (delta)

#we need the first delta gap for survey then we can just calculate the rest based on what we did already

delta.1st<-c(47,100,100,50,50,NA,47,NA,100)

delta<-matrix(NA,9,max(db))
delta[,1]<-delta.1st

for(i in 2:5){
  
  delta[,i]<- 2*(mdpts[,i]-(mdpts[,i-1]+(delta[,i-1]/2)))  
  
}#i

#correct the surveys without distance data and turn them into giant strip surveys essentially

delta[6,1]<-200
delta[8,1]<-400

###############SPECIES SPECIFIC SECTION####################################################################################
#grab data from the species groups of interest

df.db$sppTest<-as.character(df.db$CommonName)
df.db$sppTest[df.db$sppTest=="Bonaparte's Gull"]<-'Bonapartes Gull'

df.db$sppCodeFA<-factor(recode(df.db$sppTest, "'Bonapartes Gull'='All Gulls'; 'Glaucous Gull'='All Gulls'; 'Great Black-backed Gull'='All Gulls'; 'Herring Gull'='All Gulls'; 'Unid. Larus Gull'='All Gulls';
                               'Iceland Gull'='All Gulls'; 'Mew Gull'='All Gulls'; 'Ring-billed Gull'='All Gulls'; 'Unid. Gull'='All Gulls'; 'Unidentified Gull'='All Gulls';
                               'Unidentified small gull'='All Gulls'; 'Common Merganser'='All Mergansers'; 'Hooded Merganser'='All Mergansers'; 'Red-breasted Merganser'='All Mergansers';
                               'Merganser sp.'='All Mergansers'; 'Unid. Scaup'='All Scaup'; 'Greater Scaup'='All Scaup'; 'Unidentified Scaup'='All Scaup'; 'Black Scoter'='All Scoter'; 'Surf Scoter'='All Scoter';
                               'Scoter sp.'='All Scoter'; 'White-winged Scoter'='All Scoter'; 'Surf/White-winged Scoter'='All Scoter'; 'Common Goldeneye'='All Goldeneye'; 'Unidentified Goldeneye'='All Goldeneye';
                               'Horned Grebe'='All Grebe'; 'Red-necked Grebe'='All Grebe'; 'Grebe sp.'='All Grebe'; 'Unid. Grebe'='All Grebe'; 'Common Loon'='All Loons'; 'Red-throated Loon'='All Loons';
                               'Unidentified Loon'='All Loons'; 'Unid. Loon'='All Loons'; 
                               "))

sppgroups<-c('Long-tailed Duck', 'All Gulls', 'All Goldeneye', 'All Grebe', 'All Loons', 'All Mergansers', 'All Scaup', 'All Scoter')
#order all the covariates in the same way that we ordered the count data
df.dycovs5km<-df.dycovs5km[order(df.dycovs5km$X),]
#distance band to count linker
svlink<-df.dycovs5km[,c('X','uLinkID')]

spp.ys<-array(NA,c(9,92,17,length(sppgroups)))
allspp.db<-list()
allspp.gp<-list()
allspp.ys<-list()

for(s in 1:length(sppgroups)){
  df.spp<-df.db[which(df.db$sppCodeFA==sppgroups[s]),]
  
  #get all the distance band information and the associated protocol and survey indexing
  
  spp.db<-data.frame(db=df.spp$v,prot.idx=df.spp$prot.idx,trans.idx=df.spp$trans.idx,surv.idx=df.spp$SamplingEventCounter,prot_date.idx=df.spp$prot_visit.idx,
                     seg.idx=df.spp$segID)
  #spp.db<-spp.db[which(is.na(spp.db$db)==F),]
  
  for(i in 1:9){
    for(j in 1:nrow(spp.db)){
      spp.db$newtransi[j]<-spp.db$trans.idx[j]-trans.start[spp.db$prot.idx[j]]
    }#i
  }#j
  
  #provide the appropriate visit number for each distance band
  spp.db<-merge(spp.db, effort, by.x='prot_date.idx', by.y='visit.idx',all.x=T)
  #make sure that our segments line up with the known segments and remove anything that doesn't (for now)
  spp.db$uLinkID<-paste(spp.db$prot.idx, spp.db$seg.idx, spp.db$Visit, sep='_')
  spp.db<-merge(spp.db, svlink, by='uLinkID', all.x=T)
  spp.db<-spp.db[which(is.na(spp.db$X)==F),]
  spp.db$spp.idx<-s #assign a species index
  allspp.db[[s]]<-spp.db #add to our list of species distance bands
  
  
  #group size is chill too
  
  spp.gp<-data.frame(group_size=df.spp$BirdCountnum,prot.idx=df.spp$prot.idx,trans.idx=df.spp$trans.idx,surv.idx=df.spp$SamplingEventCounter,prot_date.idx=df.spp$prot_visit.idx,
                     seg.idx=df.spp$segID)
  spp.gp<-spp.gp[which(is.na(spp.gp$group_size)==F),]
  
  for(i in 1:9){
    for(j in 1:nrow(spp.gp)){
      spp.gp$newtransi[j]<-spp.gp$trans.idx[j]-trans.start[spp.gp$prot.idx[j]]
    }#i
  }#j
  
  #provide the appropriate visit number for each group size
  spp.gp<-merge(spp.gp, effort, by.x='prot_date.idx', by.y='visit.idx',all.x=T)
  spp.gp$uLinkID<-paste(spp.gp$prot.idx, spp.gp$seg.idx, spp.gp$Visit, sep='_')
  spp.gp$spp.idx<-s
  spp.gp<-merge(spp.gp, svlink, by='uLinkID', all.x=T)
  spp.gp<-spp.gp[which(is.na(spp.gp$X)==F),]
  allspp.gp[[s]]<-spp.gp
  
  
  #now let's grab all the count data and get the transects worked out
  #spp.gp$prot_trans_visit.idx<-paste(spp.gp$prot.idx,spp.gp$trans.idx,spp.gp$Visit,sep="_")
  #ys<-dcast(spp.gp, prot_visit.idx ~ "y")
  #ys<-merge(trans,ys,'trans.idx',all.x=T)
  #ys$y[which(is.na(ys$y)==T)]<-0
  
  y.idx<-dcast(spp.gp, prot.idx + seg.idx + Visit ~ 'y', fun.aggregate = length)
  y.idx$spp.idx<-s
  y.idx$uLinkID<-paste(y.idx$prot.idx, y.idx$seg.idx, y.idx$Visit, sep='_')
  
  allspp.ys[[s]]<-y.idx
}#s
#end of species loops

#summary(allspp.db[[6]])
#allspp.db[[6]][which(is.na(allspp.db[[6]]$Date)),]

allspp.db<-do.call(rbind.data.frame, allspp.db)
allspp.gp<-do.call(rbind.data.frame, allspp.gp)
#allspp.ys<-do.call(rbind.data.frame, allspp.ys)

#merge the count data with the covariate/effort data

m.y<-list()
all.y<-matrix(-99, nrow=nrow(merge(df.dycovs5km, allspp.ys[[1]], by='uLinkID',all.x=T)), ncol=length(sppgroups))
for(i in 1:length(sppgroups)){
  m.y[[i]]<-merge(df.dycovs5km, allspp.ys[[i]], by='uLinkID',all.x=T)
  m.y[[i]]<-m.y[[i]][order(m.y[[i]]$X),]
  m.y[[i]]$y[which(is.na(m.y[[i]]$y)==T)]<-0
  m.y[[i]]$y[which(is.na(m.y[[i]]$assignDate)==T)]<-NA
  all.y[,i]<-m.y[[i]]$y
}
all.ys<-data.frame(all.y)
colnames(all.ys)<-1:length(sppgroups)
all.ys<-stack(all.ys)

#fill in NAs for covariates
#turn ice into a categorical variable because it's highly non-normally distributed
df.dycovs5km$ic[which(df.dycovs5km$ic== -1)]<-NA #assign NAs to the appropriate values
df.dycovs5km$ic[which(is.na(df.dycovs5km$ic)==T)]<-0 #turn all NAs into 0's (essentially the median of the data set)
df.dycovs5km$icef<-cut(df.dycovs5km$ic, c(-1, 20, 80, 100)) #cut into three catgories: 0 or unknown, possible ice, ice

#now turn continuous variables into the appropriate variables
df.dycovs5km$BATH[which(df.dycovs5km$BATH>0)]<-0
df.dycovs5km$BATH<-df.dycovs5km$BATH * -1
df.dycovs5km$BATH[which(is.na(df.dycovs5km$BATH)==T)]<-0

df.dycovs5km$air_u[which(is.na(df.dycovs5km$air_u)==T)]<-mean(df.dycovs5km$air_u, na.rm=T)

df.dycovs5km$air_v[which(is.na(df.dycovs5km$air_v)==T)]<-mean(df.dycovs5km$air_v, na.rm=T)

df.dycovs5km$cl[which(is.na(df.dycovs5km$cl)==T)]<-mean(df.dycovs5km$cl, na.rm=T)

df.dycovs5km$wvp[which(is.na(df.dycovs5km$wvp)==T)]<-mean(df.dycovs5km$wvp, na.rm=T)

#number of segvisits per protocol
segvis<-c(table(df.dycovs5km$proti))

#rewrite all the indeces and covariates for a protocol x segvisit matrix
segvis.mat<-matrix(-99, nrow=max(segvis), ncol=9)
segvis.key<-matrix(-99, nrow=max(segvis), ncol=9)
segvis.prot<-matrix(-99, nrow=max(segvis), ncol=9)
for(i in 1:9){
  for(j in 1:segvis[i]){
    segvis.mat[j, i] <- df.dycovs5km$X[which(df.dycovs5km$proti==i)][j]
    segvis.key[j, i] <- j
    segvis.prot[j, i] <- i
  }#j
}#i

segvis.link<-data.frame(oldLink=c(segvis.mat), newLink=c(segvis.key), prot=c(segvis.prot))

#make a protocol major matrix for the distance band data
nobsp<-c(table(allspp.db$prot.idx))
dc.list<-list()
gpsz.list<-list()
for(i in 1:9){
  dc.list[[i]]<-allspp.db[which(allspp.db$prot.idx==i),]
  gpsz.list[[i]]<-allspp.gp[which(allspp.gp$prot.idx==i),]
}#i

#merge the new link with the distance data
dc.mat<-matrix(NA, nrow=max(nobsp), ncol=9)
gpsz.mat<-matrix(NA, nrow=max(nobsp), ncol=9)
sppi.mat<-matrix(NA, nrow=max(nobsp), ncol=9)
segvisi.mat<-matrix(NA, nrow=max(nobsp), ncol=9)
for(i in 1:9){
  dc.list[[i]] <- merge(dc.list[[i]], segvis.link, by.x='X', by.y='oldLink', all.x=T)
  gpsz.list[[i]] <- merge(gpsz.list[[i]], segvis.link, by.x='X', by.y='oldLink', all.x=T)
  for(j in 1:nobsp[i]){
    dc.mat[j, i] <- dc.list[[i]]$db[j]
    gpsz.mat[j, i] <- gpsz.list[[i]]$group_size[j]
    sppi.mat[j, i] <- dc.list[[i]]$spp.idx[j]
    segvisi.mat[j, i] <- dc.list[[i]]$newLink[j]
  }
}

#rearrange the covariate data to be a newsegvis x protocol matrix
df.dycovs5kmf<-merge(df.dycovs5km, segvis.link, by.x='X', by.y='oldLink', all.x=T)

#scaling the continuous covs
df.dycovs5kmf$air_u<-scale(df.dycovs5kmf$air_u)[,1]
df.dycovs5kmf$air_v<-scale(df.dycovs5kmf$air_v)[,1]
df.dycovs5kmf$cl<-scale(df.dycovs5kmf$cl)[,1]
df.dycovs5kmf$wvp<-scale(df.dycovs5kmf$wvp)[,1]
df.dycovs5kmf$Bathy<-scale(df.dycovs5kmf$BATH)[,1]
df.dycovs5kmf$ic<-scale(df.dycovs5kmf$ic)[,1]
df.dycovs5kmf$POINT_X<-scale(df.dycovs5kmf$POINT_X)[,1]
df.dycovs5kmf$POINT_Y<-scale(df.dycovs5kmf$POINT_Y)[,1]

scale(df.dycovs5kmf$POINT_X)
scale(df.dycovs5kmf$ic)
scale(df.dycovs5kmf$BATH)

#adjusting the lake covariate
df.dycovs5kmf$lakelink<-as.factor(paste(df.dycovs5kmf$lake, df.dycovs5kmf$prot, sep='_'))
df.dycovs5kmf$lake2<-as.factor(recode(df.dycovs5kmf$lakelink, "'e_1'='E Erie'; 'e_4'='W Erie'; 'e_5'='W Erie';
                                      'h_2'='Huron'; 'h_3'='Huron'; 'm_6'='Michigan'; 'm_7'='Michigan';
                                      'm_8'='Michigan'; 'm_9'='Michigan'; 'c_4'='St. Clair'; 'c_5'='St. Clair'"))

#adjust the bottom substrate
df.dycovs5kmf$sub_class2<-as.factor(recode(df.dycovs5kmf$sub_class, " ''='other'; 'clay'='clay'; 'hard'='other'; 'mud'='mudsilt';
                                           'rock'='other'; 'sand'='sand'; 'silt'='mudsilt'; 'unknown'='other'  "))

#grab the months and turn into a categorical cov (months 9-11, 12-2, 3-6)
df.dycovs5kmf$assignDate2<-chron(as.character(df.dycovs5kmf$assignDate), format="y-m-d", out.format='month day year')
df.dycovs5kmf$month<-str_split_fixed(df.dycovs5kmf$assignDate2, pattern=" ", n=3)[,1]


df.dycovs5kmf$season<-as.factor(recode(df.dycovs5kmf$month, " 'April'='Spring';'December'='Winter'; 'February'='Winter';
                                       'January'='Winter'; 'June'='Spring'; 'March'='Spring'; 'May'='Spring';
                                       'November'='Fall'; 'October'='Fall'; 'September'='Fall' "))
#visit and transect coding
visiti_mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'Visit.x', fun.aggregate = mean)
transi_mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'newtransi.x', fun.aggregate = mean)
segi_mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'FID', fun.aggregate = mean)

#continuous predictors
air_u.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'air_u', fun.aggregate = mean)
air_v.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'air_v', fun.aggregate = mean)
cl.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'cl', fun.aggregate = mean)
wvp.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'wvp', fun.aggregate = mean)
bath.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'Bathy', fun.aggregate = mean)
ice.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'ic', fun.aggregate = mean)
lat.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'POINT_Y', fun.aggregate = mean)
lon.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'POINT_X', fun.aggregate = mean)

#categorical predictors
lake.cat<-model.matrix(~df.dycovs5kmf$lake-1)
lake.cat<-cbind(df.dycovs5kmf, lake.cat)

lake2.cat<-model.matrix(~df.dycovs5kmf$lake2-1)
lake2.cat<-cbind(df.dycovs5kmf, lake2.cat)

sub.cat<-model.matrix(~df.dycovs5kmf$sub_class-1)
sub.cat<-cbind(df.dycovs5kmf, sub.cat)

season.cat<-model.matrix(~df.dycovs5kmf$season-1)
season.cat<-cbind(df.dycovs5kmf, season.cat)

lake1.mat<-acast(lake.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lakeh', fun.aggregate = mean)
lake2.mat<-acast(lake.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lakem', fun.aggregate = mean)
lake3.mat<-acast(lake.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lakec', fun.aggregate = mean)

lake1_2.mat<-acast(lake2.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lake2Huron', fun.aggregate = mean)
lake2_2.mat<-acast(lake2.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lake2Michigan', fun.aggregate = mean)
lake3_2.mat<-acast(lake2.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lake2St. Clair', fun.aggregate = mean)
lake4_2.mat<-acast(lake2.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$lake2W Erie', fun.aggregate = mean)

sub1.mat<-acast(sub.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$sub_classclay', fun.aggregate = mean)
sub2.mat<-acast(sub.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$sub_classhard', fun.aggregate = mean)
sub3.mat<-acast(sub.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$sub_classmud', fun.aggregate = mean)
sub4.mat<-acast(sub.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$sub_classsand', fun.aggregate = mean)
sub5.mat<-acast(sub.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$sub_classsilt', fun.aggregate = mean)

sea1.mat<-acast(season.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$seasonFall', fun.aggregate = mean)
sea2.mat<-acast(season.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$seasonWinter', fun.aggregate = mean)
sea3.mat<-acast(season.cat, newLink ~ prot, value.var= 'df.dycovs5kmf$seasonSpring', fun.aggregate = mean)

#rearrange the count data to a newsegvis x protocol x species matrix
df.dycovs5kmf$assignDate2<-as.character(df.dycovs5kmf$assignDate2)
df.allys<-data.frame(df.dycovs5kmf, all.ys)

y.arr<-array(-99, dim=c(max(segvis), 9, length(sppgroups)))
for(i in 1:length(sppgroups)){
  y.arr[,,i]<-acast(df.allys[which(df.allys$ind==i),], newLink ~ prot, value.var='values', fun.aggregate = mean)
}#i

#transect length organized by protocol, species, and segvis

tlength<-df.dycovs5kmf$TransDistN*1.85
tlength.mat<-acast(df.dycovs5kmf, newLink ~ prot, value.var= 'TransDistN', fun.aggregate = mean)
tlength.mat<-tlength.mat*1.85

#transect width organized by protcol, species, and segvis and converted into km2

tarea.mat<-t(tlength.mat)*(maxd/1000)

######GATHER DATA AND RUN THE JAGS MODEL####################################################

#removed lakes
dat<-list(nprot=nrow(mdpts),maxd=maxd,midpts=mdpts, nsegvis=segvis, 
          nbreaks=db, nobs=nobsp, delta=delta, tarea=log(tarea.mat),
          tmat=t(transi_mat), vmat=t(visiti_mat), smat=t(segi_mat), segvisi=t(segvisi.mat), sppi=t(sppi.mat),
          distclass=t(dc.mat), y=aperm(y.arr, c(2,3,1)), groupsize=t(gpsz.mat),
          X1=t(sub1.mat), X2=t(sub2.mat), X3=t(sub3.mat), X4=t(sub4.mat), X5=t(sub5.mat),
          X8=t(sea1.mat), X9=t(sea2.mat),
          X10=t(bath.mat), X11=t(ice.mat), X12=t(lat.mat), X13=t(lon.mat),
          X14=t(ifelse(ice.mat>3.5, 1, 0)),
          V1=t(air_u.mat), V2=t(air_v.mat), V3=t(cl.mat), V4=t(wvp.mat))

dput(dat, '/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/data_spp8_5km_up.glc')

#rm(list=ls())

gc()

dat<-dget('data_spp8_5km_up.glc')

dat$y<-ifelse(is.nan(dat$y), NA, dat$y)

x.inits<-ifelse(dat$y>0, 1, 0)

N.inits<-ifelse(is.nan(dat$y),NA,dat$y)

inits<-function(){list(x=x.inits, N=N.inits)}

params<-c('Tobs','Tobsnew','theta','mu.p','sigma.s0','sigma.p0','sig.p','sig.s','Tob','Tobnew',
          's.b1', 's.b2', 's.b3', 's.b4','N.b0', 'N.b1', 'N.b2','N.b3','N.b4','N.b5',
          'thetaZI','thetaZI.b1','thetaZI.b2','thetaZI.b3', 'Np')

ni<-1000
na<-100
nb<-500
nt<-3
nc<-3

gc()

#pulls in model from 'glc_ms_jags_model.R'
ptm<-proc.time()

mod<-jags.model('glc_ms_full.txt',data=dat, inits=inits, n.chains=nc, n.adapt=na)
gc()
update(mod, nb)
fit<-coda.samples(mod, params, ni, nt)

(proc.time()-ptm)/60^2
save(fit, file='glc_det031317_full.Rdata')

#load('glc_det012117.Rdata')

variable.list<-c(attr(fit[[1]],'dimnames')[2])
View(variable.list)

gc()

plot(fit[,1:3])
plot(fit[,4:6])
plot(fit[,7:8])

plot(fit[,9:11])
plot(fit[,12:14])
plot(fit[,15:16])

plot(fit[,17:19])
plot(fit[,20:22])
plot(fit[,23:24])

plot(fit[,25:27])
plot(fit[,28:30])
plot(fit[,31:32])

plot(fit[,33:35])
plot(fit[,36:38])
plot(fit[,39:40])

plot(fit[,41:43])
plot(fit[,44:46])
plot(fit[,47:48])

plot(fit[,49:51])
plot(fit[,52:54])
plot(fit[,55:56])

plot(fit[,57:58])

plot(fit[,59:61])
plot(fit[,62:64])
plot(fit[,65:66])

plot(fit[,66:68])
plot(fit[,69:71])
plot(fit[,72:74])

plot(fit[,75:77])
plot(fit[,78:80])
plot(fit[,81:83])

plot(fit[,84:86])
plot(fit[,87:89])
plot(fit[,90:92])

plot(fit[,93:95])
plot(fit[,96:98])
plot(fit[,99:101])

plot(fit[,102:104])


#PPC for each survey
plot(fit[,c(1,2)])
t<-summary(fit[,1])$statistics[1]
tnew<-summary(fit[,2])$statistics[1]
t/tnew
mean(unlist(fit[,2])>unlist(fit[,1]))

plot(unlist(fit[,1]), unlist(fit[,2]))
abline(1,1)

#
plot(fit[,c(4,10)])
t<-summary(fit[,4])$statistics[1]
tnew<-summary(fit[,11])$statistics[1]
t/tnew
mean(unlist(fit[,11])>unlist(fit[,4]))

plot(unlist(fit[,4]), unlist(fit[,11]))
abline(1,1)

#pull each of the difference data from the mcmc.list
bpv<-c()
for(i in 1:7){
  bpv[i]<-mean(unlist(fit[,9+i])>unlist(fit[,2+i]))
  plot(unlist(fit[,2+i]), unlist(fit[,9+i]), main=i)
  abline(1,1)
}

#other stuff
plot(fit[,18])

plot(fit[,45514:45516])

#covariates to sigma
summary(fit[,18:27])
summary(fit[,28:37])
summary(fit[,38:47])
summary(fit[,48:57])

#det only parameters
plot(fit[,11:12])
plot(fit[,13:14])
plot(fit[,26:27])

plot(fit[,29:31])
plot(fit[,32:34])
plot(fit[,35:36])
plot(fit[,36:37])

plot(fit[,38:40])
plot(fit[,41:43])
plot(fit[,44:45])
plot(fit[,46:47])

#making det plots
require(unmarked)
s.p0<-summary(fit[,60:68])$statistics[,1]
s.s0<-summary(fit[,69:78])$statistics[,1]
s.b1<-summary(fit[,18:27])$statistics[,1]
s.b2<-summary(fit[,28:37])$statistics[,1]
s.b3<-summary(fit[,38:47])$statistics[,1]
s.b4<-summary(fit[,48:57])$statistics[,1]

sig.hat<-s.p0

theta<-summary(fit[,43:51])$statistics[,1]

#covariate dot model

s.p0<-summary(fit[,24:32])$statistics[,1]
s.s0<-summary(fit[,33:42])$statistics[,1]
summary(fit[,18:21])

#distance bin corrected histograms

#distance data for graphing (minor agument to p5 so it'll graph easily)
dc<-dat$distclass
dc[5,9]<-5

#for loop for all halfnorm protocols
pdet.hn<-c()
for(i in 1:7){
  j<-c(1,2,3,4,5,7,9)
  barplot((table(dc[j[i], ])/dat$delta[j[i],1:dat$nbreaks[j[i]]])/
            max((table(dc[j[i], ])/dat$delta[j[i],1:dat$nbreaks[j[i]]])), width=dat$delta[j[i],1:dat$nbreaks[j[i]]], axes=F, axisnames = F)
  par(new=T)
  plot(function(x) gxhn(x, sigma=exp(sig.hat[j[i]])), 0, dat$maxd[j[i]],
       ylab="Estimated Detection Probability",xlab="Distance (m)",
       col="blue",ylim=c(0,1), main=paste(j[i], bpv[i], sep='_'))
  par(new=F)
  pdet.hn[i]<-integrate(gxhn,0,dat$maxd[j[i]], sigma=exp(sig.hat[j[i]]))$value/dat$maxd[j[i]]
}

#for loop for all hazard protocols
pdet.haz<-c()
for(i in 1:7){
  j<-c(1,2,3,4,5,7,9)
  barplot((table(dc[j[i], ])/dat$delta[j[i],1:dat$nbreaks[j[i]]])/
            max((table(dc[j[i], ])/dat$delta[j[i],1:dat$nbreaks[j[i]]])), width=dat$delta[j[i],1:dat$nbreaks[j[i]]], axes=F, axisnames = F)
  par(new=T)
  plot(function(x) gxhaz(x, shape=exp(sig.hat[j[i]]), scale=theta[j[i]]), 0, dat$maxd[j[i]],
       ylab="Estimated Detection Probability",xlab="Distance (m)",
       col="blue",ylim=c(0,1), main=paste(j[i], bpv[i], sep='_'))
  par(new=F)
  pdet.haz[i]<-integrate(gxhaz,0,dat$maxd[j[i]], shape=exp(sig.hat[j[i]]), scale=theta[j[i]])$value/dat$maxd[j[i]]
}

#looking at pdet for the half-normal
integrate(gxhn,0,dat$maxd[9], sigma=exp(sig.hat[9]))$value/dat$maxd[9]

#some plots to look at#########################################################
#calculate detection probability over the strip using the theoretical detection fxn for the given suvey strip width

integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[1,3])$value/maxd[1]
integrate(gxhn,0,maxd[2], sigma=sigmas.mcmc$quantiles[2,3])$value/maxd[2]
integrate(gxhn,0,maxd[3], sigma=sigmas.mcmc$quantiles[3,3])$value/maxd[3]
integrate(gxhn,0,maxd[4], sigma=sigmas.mcmc$quantiles[4,3])$value/maxd[4]
integrate(gxhn,0,maxd[5], sigma=sigmas.mcmc$quantiles[5,3])$value/maxd[5]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[6,3])$value/maxd[6]
integrate(gxhn,0,maxd[7], sigma=sigmas.mcmc$quantiles[7,3])$value/maxd[7]

#calculate detection probability for a 300 m strip

integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[1,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[2,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[3,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[4,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[5,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[6,3])$value/maxd[1]
integrate(gxhn,0,maxd[1], sigma=sigmas.mcmc$quantiles[7,3])$value/maxd[1]

#calculate detection probability for a 200 m strip

integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[1,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[2,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[3,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[4,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[5,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[6,3])$value/maxd[6]
integrate(gxhn,0,maxd[6], sigma=sigmas.mcmc$quantiles[7,3])$value/maxd[6]


#discuss with Beth 5/2/16
#1 the first distance category has fewer animals than it should, even once you account for strip size differences (BRI, MDNR, MFNI2?, USGS) 
## how do we deal with this? we still need to adjust many of the first categories by limited vision from the plane. How else? combine distance categories?
#2 fixed effect detectability estimation is working fairly well. How do we want to employ a random effects model? I've tried a couple things but haven't gotten anything to work yet. The sigma values are very far apart...
#3 How do we apply our detectability information to surveys without it (entire surveys don't have it, not just some records)?
#4 What do we do about edge of the final distance bar? Just make it really far out? Is that okay?
#5 Some survey bands (the last/furthest ones) have no data. This will likely change, but do we worry?
#6 Can I check out the community distance model code? (It's apparently only in early online access right now)

#talk with Beth 05/13/16
#which surveys looked out both sides of the plane? kevin's flight certainly did
#change first distance bin depending on daily flight height
#assign sigmas from similar surveys (kevin y1 to kevin y2)?
#check to see how species selection is influenced by detection probability?
#combo detections covs (cloud + sea state)
#produce weather covariates for sea state

#questions for Beth 06/10/16
#Why do some of instances of sigma show updating and others don't (e.g., within sigma.s0)
#what should i do with all the NAs. keep or purge? What about sites with no observations?

#to-do list 6/20/16
#1 look at how the surveys handled ice cover
#2 graph count distribution data (flock size)
#3 graph flock size/distance detected
#4 look at bird behavior flags; how often are they used and in what survey
#5 what can we do about the donut effect?
#6 get ice cover covariate data from whomever has it