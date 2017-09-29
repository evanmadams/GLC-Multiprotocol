require(R2jags)
require(snow)

dat<-dget('/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/data_spp8_5km_up.glc')

#select a single species or species group
sppid <- 5 #set the species

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
dat$y<-dat$y[,sppid,] #selecting species

#define initial values for the needed parameters
dat$y<-ifelse(is.nan(dat$y), NA, dat$y)
dat$yo<-ifelse(dat$y>0, 1, 0)

protcut <- c(-4, -5)

#cut out all the stuff from the protocols without enough information
dat.p7<-list(nprot=7, protsel=c(1,2,3,5,7), maxd=dat$maxd[protcut], midpts=dat$midpts[protcut,], nsegvis=dat$nsegvis[protcut], nbreaks=dat$nbreaks[protcut],
             nobs=dat$nobs[protcut], delta=dat$delta[protcut,], tarea=dat$tarea[protcut,], tmat=dat$tmat[protcut,],vmat=dat$vmat[protcut,], 
             smat=dat$smat[protcut,], segvisi=dat$segvisi[protcut,], sppi=dat$sppi[protcut, ], distclass=dat$distclass[protcut,],
             y=dat$y[protcut,], loggpsz=dat$loggpsz[protcut,], X1=dat$X1[protcut,], X2=dat$X2[protcut,], X3=dat$X3[protcut,], X4=dat$X4[protcut,],
             X5=dat$X5[protcut,], X8=dat$X8[protcut,], X9=dat$X9[protcut,], X10=dat$X10[protcut,], X11=dat$X11[protcut,], X12=dat$X12[protcut,],
             X13=dat$X13[protcut,], X14=dat$X14[protcut,], X15=dat$X15[protcut,], X16=dat$X16[protcut,], X17=dat$X17[protcut,], X18=dat$X18[protcut,],
             V1=dat$V1[protcut,], V2=dat$V2[protcut,], V3=dat$V3[protcut,], V4=dat$V4[protcut,])

dat.p7$midpts[4,] <- dat.p7$midpts[5,]
dat.p7$nbreaks[4] <- dat.p7$nbreaks[5]
dat.p7$delta[4,] <- dat.p7$delta[5,]
dat.p7$midpts[6,] <- dat.p7$midpts[7,]
dat.p7$nbreaks[6] <- dat.p7$nbreaks[7]
dat.p7$delta[6,] <- dat.p7$delta[7,]

x.inits<-ifelse(dat.p7$y>0, 1, 0)

N.inits<-ifelse(is.nan(dat.p7$y),NA,dat.p7$y)

sig.inits <- function(){runif(1,4,5)}

inits<-function(){list(x=x.inits, N=N.inits, mu.p=runif(1, 4, 5), sig.p=runif(1, 1, 2))}

params<-c('Tobs','Tobsnew','mu.p','sigma.p0','sig.p','Tob','Tobnew', 'theta.p0','sig.pt','mu.pt',
          's.b1', 's.b2', 's.b3', 's.b4','N.b0', 'N.b1', 'N.b2','N.b3','N.b4','N.b5', 'N.b6', 'N.b7', 'N.b8',
          'thetaZI','thetaZI.b1','thetaZI.b2','thetaZI.b3','thetaZI.b4','thetaZI.b5',
          'Np','b0.gp','b1.gp','yfit','yfitrep','yfitp','yfitprep', 'sig.NE')

ni<-30000
na<-10000
nb<-100
nt<-3
nc<-3

gc()

coda.samples.wrapper <- function(j)
{ 
  mod = jags.model("/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/glc_p7_spp5_hn.txt", 
                   inits=function(){list(x=x.inits, N=N.inits, mu.p=runif(1,4,5), sig.p=runif(1, 1, 2), .RNG.name="base::Wichmann-Hill",
                                         .RNG.seed=j)}, 
                   data=dat.p7, n.chains=1, n.adapt=na)
  update(mod, nb)
  coda.samples(mod, params, n.iter=ni, thin=nt) 
}

snow.start.time = proc.time()
cl <- makeCluster(nc, "SOCK")

clusterEvalQ(cl, library(R2jags))

clusterExport(cl, list("ni","nt","nb","nc","na","dat.p7","params","x.inits","N.inits",'sig.inits'))
fit = clusterApply(cl, 1:nc, coda.samples.wrapper)

for(i in 1:length(fit)) { fit[[i]] <- fit[[i]][[1]] }
class(fit) <- "mcmc.list"
stopCluster(cl)

snow.end.time = proc.time()
snow.dtime = snow.end.time - snow.start.time
snow.dtime/60^2

save(fit, file='/home/evanadams/Desktop/GLC Phase 3 Analysis/GLC Phase 3 Analysis/Data for UBK/glc_spp5_p7_hn_30k10ksnow2_test.Rdata')

