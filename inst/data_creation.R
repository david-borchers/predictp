cdfdataframe = data.frame(depth = seq(0,3,0.5),
                           cdf=c(0.124,0.321,0.421,0.462,0.490,0.510,0.527)) # from Tielman et al.
save(cdfdataframe,file="./data/cdfdataframe.rda")

vismodel = readRDS("/Users/dlb/git/porpoisevis/inst/vismodel.RDS")
tagmodel = readRDS("/Users/dlb/git/porpoisetag/tagmodel.RDS")

save(vismodel,file="./data/vismodel.rda")
save(tagmodel,file="./data/tagmodel.rda")

eg_viscov_withpos = data.frame(pos=c("Centre","Centre","Near","Near","Far","Far"),
                                     secchi=c(8,1,8,1,8,1),
                                     SS=c(0,0,1,1,2,2),
                                     cloud=c(0,2,4,4,2,0),
                                     hdglare=c("1","2","3","1","2","3"))
eg_viscov = eg_viscov_withpos[,-1]
eg_tagcov = data.frame(periodfac=c("0","3","2","1","3","0"),
                       diy=c(110,50,20,120,220,330))


save(eg_viscov_withpos,file="./data/eg_viscov_withpos.rda")
save(eg_viscov,file="./data/eg_viscov.rda")
save(eg_tagcov,file="./data/eg_tagcov.rda")

