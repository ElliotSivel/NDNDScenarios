# Stability
stability<- function(Biomass){
  Biomass1 <- Biomass[,1:7] %>%
    mutate_at(vars(1:7),funs(./sd(.)))
  out.global <- sum(summarise_all(Biomass1,mean))/(sum(cov(Biomass1))^0.5)
  out.global.notsd <- sum(summarise_all(Biomass[1:7],mean))/(sum(cov(Biomass[,1:7]))^0.5)
  out.species <- summarise_all(Biomass1,mean)
  return(list(out.global,out.species,out.global.notsd))
}