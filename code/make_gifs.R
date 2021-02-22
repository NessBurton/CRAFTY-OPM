
rstOutput <- stack(paste0(dirCRAFTYOutput,"/rstRangeshiftR_output_coupled_test2.tif"))

names(rstOutput) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")
clrs.viridis <- colorRampPalette(viridis::viridis(10))

spplot(rstOutput, layout = c(5,2), col.regions=clrs.viridis(14), at = seq(0,70,10))
#dev.off()

for (i in c(1:10)){
  
  #i <- 2
  yr <- names(rstOutput)[i]
  png(paste0(dirFigs,"/rsftr_pops_CRAFTY-coupled_test2_",yr,".png"), width = 600, height = 600)
  print(spplot(rstOutput, col.regions=clrs.viridis(14), at = seq(0,70,10))[i])
  dev.off()

}

rstOutput2 <- stack(paste0(dirCRAFTYOutput,"/rstRangeshiftR_output_coupled_test1.tif"))

names(rstOutput2) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")

for (i in c(1:10)){
  
  #i <- 2
  yr <- names(rstOutput2)[i]
  png(paste0(dirFigs,"/rsftr_pops_CRAFTY-coupled_test1_",yr,".png"), width = 600, height = 600)
  print(spplot(rstOutput2, col.regions=clrs.viridis(14), at = seq(0,70,10))[i])
  dev.off()
  
}

rstOutput3 <- stack(paste0(dirCRAFTYOutput,"/rstRangeshiftR_output_standalone.tif"))
names(rstOutput3) <- c("Yr1","Yr2","Yr3","Yr4","Yr5","Yr6","Yr7","Yr8","Yr9","Yr10")

for (i in c(1:10)){
  
  #i <- 2
  yr <- names(rstOutput3)[i]
  png(paste0(dirFigs,"/rsftr_pops_standalone_",yr,".png"), width = 600, height = 600)
  print(spplot(rstOutput3, col.regions=clrs.viridis(14), at = seq(0,70,10))[i])
  dev.off()
  
}
