require(R2jags)

dat<-dget('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/data_spp8_5km_up.glc')


#select a single species or species group
sppid <- 2 #set the species


#distclass and needed indices

newdc<-list()
newsegvisi<-list()
newgpsz<-list()
newdcl<-c()
for(i in 1:9){
  newdc[[i]] <- dat$distclass[i, which(dat$sppi[i,]==sppid)] 
  newsegvisi[[i]] <- dat$segvisi[i, which(dat$sppi[i,]==sppid)]
  newgpsz[[i]] <- dat$groupsize[i, which(dat$sppi[i,]==sppid)]
  newdcl[i] <- length(newdc[[i]])
}

ssdistclass <- matrix(NA, nrow=9, ncol=max(newdcl))
sssegvisi <- matrix(NA, nrow=9, ncol=max(newdcl))
ssgpsz <- matrix(NA, nrow=9, ncol=max(newdcl))

for(i in 1:9){
  for(j in 1:newdcl[i]){
    ssdistclass[i, j] <- newdc[[i]][j]
    sssegvisi[i, j] <- newsegvisi[[i]][j]
    ssgpsz[i, j] <- newgpsz[[i]][j]
  }
}

dat$distclass<-ssdistclass
dat$segvisi<-sssegvisi
dat$nobs<-newdcl
dat$groupsize<-ssgpsz
dat$groupsize[dat$groupsize==0]<-NA
dat$loggpsz <- log(dat$groupsize)

#counts
dat$y<-dat$y[,sppid,] #selecting the species of interest


#define initial values for the needed parameters
dat$y<-ifelse(is.nan(dat$y), NA, dat$y)
dat$yo<-ifelse(dat$y>0, 1, 0)

z.inits<-apply(dat$yo, 1, function(x){max(x, na.rm=T)})

x.inits<-ifelse(dat$y>0, 1, 0)

N.inits<-ifelse(is.nan(dat$y),NA,dat$y)

inits<-function(){list(x=x.inits, N=N.inits)}

params<-c('Tobs','Tobsnew','mu.p','sigma.p0','sig.p','Tob','Tobnew', 'mu.pt', 'sig.pt', 'theta.p0',
          's.b1', 's.b2', 's.b3', 's.b4','N.b0', 'N.b1', 'N.b2','N.b3','N.b4','N.b5', 'N.b6', 'N.b7', 'N.b8',
          'thetaZI','thetaZI.b1','thetaZI.b2','thetaZI.b3','thetaZI.b4','thetaZI.b5',
          'Np','b0.gp','b1.gp','yfit','yfitrep','yfitp','yfitprep', 'sig.NE')

ni<-30000
na<-30000
nb<-100
nt<-3
nc<-3

gc()


#pulls in model from 'glc_ms_jags_model.R'
ptm<-proc.time()

mod<-jags.model('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/glc_spp2_haztest.txt',data=dat, inits=inits, n.chains=nc, n.adapt=na)
gc()
update(mod, nb)
fit<-coda.samples(mod, params, ni, nt)

(proc.time()-ptm)/60^2
save(fit, file='/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/glc_spp2_haz.Rdata')

