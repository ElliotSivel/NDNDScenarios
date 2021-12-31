source(file = "C:/Users/a22073/Documents/Model_NDND/NDND/ndnd_code/0_2_NDND_Data.R") # load data

save_dir_out<-paste("C:/Users/a22073/Documents/Model_NDND/NDND/output/",format(Sys.time(),"%Y_%m_%d_%H_%M_%S",sep = ""))
dir.create(path=save_dir_out)

NDNDData_save <- NDNDData

alfa<-0
peche<-c(-0.5,0,0.25,0.5)
temperature <- c(-1,0,1,2)

comb_scen <- expand.grid(temperature=temperature,alfa=alfa,peche=peche)
comb_scen$ID <- paste(comb_scen$temperature,comb_scen$alfa,comb_scen$peche,sep = "_")

s_length<-1000

t1<-Sys.time()

for (c in 1:nrow(comb_scen)) {
  NDNDData <- NDNDData_save
  NDNDData$Mu[1:6] <- NDNDData$Mu[1:6]*1.1^comb_scen$temperature[c]
  NDNDData$Rho[1:6] <- NDNDData$Rho[1:6]*1.1^comb_scen$temperature[c]
  NDNDData$Sgma[1:6] <- NDNDData$Sgma[1:6]*1.1^comb_scen$temperature[c]
  NDNDData$Alpha <- comb_scen$alfa[c]
  NDNDData$Fmp <- NDNDData$Fmp+(NDNDData$Fmp*comb_scen$peche[c])
  NDNDData$Catches <- NDNDData$Catches+(NDNDData$Catches*comb_scen$peche[c])
  out.sim <- list()
  for (d in 1:s_length) {
    capture.output(sim <- SimNDNDpar(NDNDData)) #SIMULATION
    out.sim[[d]] <- sim
  }
  save(out.sim,file=paste(save_dir_out,"/",comb_scen$ID[c],".Rdata",sep=""))
}  
  
t2<-Sys.time()
tdif <- difftime(t2,t1)


names(sim.out) <- comb_scen$ID

sim.reshape <- list()
sim.reshape.f <- list()
sim.reshape.p <- list()

for (j in 1:4) {
  sim.reshape.2 <- list()
  sim.reshape.2.f <- list()
  sim.reshape.2.p <- list()
  for (k in 1:4) {
    sim.reshape.2 [[k]]<- split.scen[[j]][[k]]$Output$BiomassSeries
    sim.reshape.2.f [[k]]<- split.scen[[j]][[k]]$Output$FlowSeries
    sim.reshape.2.p [[k]]<- split.scen[[j]][[k]]$Output$Fish
  }
  sim.reshape[[j]] <- sim.reshape.2
  sim.reshape.f[[j]] <- sim.reshape.2.f
  sim.reshape.p[[j]] <- sim.reshape.2.p
}

sim.bio <- map(sim.reshape,function(x){
  comp.data <- do.call(rbind.data.frame,x)
  comp.data <- cbind(comp.data,"Simulation"=rep(1:4,byrow=F, each=NDNDData$Tmax),"Years"=rep(1:NDNDData$Tmax,times=s_length))
})
sim.flow <- map(sim.reshape.f,function(x){
  comp.data <- do.call(rbind.data.frame,x)
  comp.data <- cbind(comp.data,"Simulation"=rep(1:4,byrow=F, each=(NDNDData$Tmax-1)),"Years"=rep(1:NDNDData$Tmax,times=s_length))
})
sim.fish <- map(sim.reshape.p,function(x){
  comp.data <- do.call(rbind.data.frame,x)
  comp.data <- cbind(comp.data,"Simulation"=rep(1:4,byrow=F, each=NDNDData$Tmax),"Years"=rep(1:NDNDData$Tmax,times=s_length))
})
