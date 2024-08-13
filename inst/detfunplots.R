# Model without interactions
# --------------------------
load("/Users/dlb/git/porpoisevis/inst/fit.hd.sm.rda")

cov1 = data.frame(pos="Centre",secchi=7.5,SS=0,cloud=0,hdglare="1")
cov2 = data.frame(pos="Near",secchi=3,SS=1.5,cloud=4,hdglare="2")
cov3 = data.frame(pos="Far",secchi=2,SS=3,cloud=0,hdglare="2")
cov4 = data.frame(pos="Far",secchi=1,SS=3,cloud=8,hdglare="3")

pdf("./inst/detfuns_fit.hd.sm.pdf")
par(mfrow=c(2,2))
plt1 = plotdetfn(fit.hd.sm,cov1,main="No interactions",cex.main=0.75)
plt2 = plotdetfn(fit.hd.sm,cov2,main="No interactions",cex.main=0.75)
plt3 = plotdetfn(fit.hd.sm,cov3,main="No interactions",cex.main=0.75)
plt4 = plotdetfn(fit.hd.sm,cov4,main="No interactions",cex.main=0.75)
dev.off()

# Model with depth:cloud interaction
# -------------------------------------
load("/Users/dlb/git/porpoisevis/inst/fit.hd.sm.dc.rda")

pdf("./inst/detfuns_fit.hd.sm.dc.pdf")
par(mfrow=c(2,2))
plt1 = plotdetfn(fit.hd.sm.dc,cov1,main="Depth:cloud interaction",cex.main=0.75)
plt2 = plotdetfn(fit.hd.sm.dc,cov2,main="Depth:cloud interaction",cex.main=0.75)
plt3 = plotdetfn(fit.hd.sm.dc,cov3,main="Depth:cloud interaction",cex.main=0.75)
plt4 = plotdetfn(fit.hd.sm.dc,cov4,main="Depth:cloud interaction",cex.main=0.75)
dev.off()

# Model with depth:cloud and depth:hdgl interactions
# --------------------------------------------------
load("/Users/dlb/git/porpoisevis/inst/fit.hd.sm.dc.dhdg.rda")

pdf("./inst/detfuns_fit.hd.sm.dc.dhdg.pdf")
par(mfrow=c(2,2))
plt1 = plotdetfn(fit.hd.sm.dc.dhdg,cov1,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt2 = plotdetfn(fit.hd.sm.dc.dhdg,cov2,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt3 = plotdetfn(fit.hd.sm.dc.dhdg,cov3,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt4 = plotdetfn(fit.hd.sm.dc.dhdg,cov4,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
dev.off()



# Repeat but marginalising over pos:
# =================================
cov1npos = cov1[,-1]
cov2npos = cov2[,-1]
cov3npos = cov3[,-1]
cov4npos = cov4[,-1]

# Model without interactions
# --------------------------
pdf("./inst/detfuns_fit.hd.sm_nopos.pdf")
par(mfrow=c(2,2))
plt1a = plotdetfn(fit.hd.sm,cov1npos,main="No interactions",cex.main=0.75)
plt2a = plotdetfn(fit.hd.sm,cov2npos,main="No interactions",cex.main=0.75)
plt3a = plotdetfn(fit.hd.sm,cov3npos,main="No interactions",cex.main=0.75)
plt4a = plotdetfn(fit.hd.sm,cov4npos,main="No interactions",cex.main=0.75)
dev.off()

# Model with depth:cloud interaction
# -------------------------------------
pdf("./inst/detfuns_fit.hd.sm.dc_nopos.pdf")
par(mfrow=c(2,2))
plt1a = plotdetfn(fit.hd.sm.dc,cov1npos,main="Depth:cloud interaction",cex.main=0.75)
plt2a = plotdetfn(fit.hd.sm.dc,cov2npos,main="Depth:cloud interaction",cex.main=0.75)
plt3a = plotdetfn(fit.hd.sm.dc,cov3npos,main="Depth:cloud interaction",cex.main=0.75)
plt4a = plotdetfn(fit.hd.sm.dc,cov4npos,main="Depth:cloud interaction",cex.main=0.75)
dev.off()

# Model with depth:cloud and depth:hdgl interactions
# --------------------------------------------------
pdf("./inst/detfuns_fit.hd.sm.dc.dhdg_nopos.pdf")
par(mfrow=c(2,2))
plt1a = plotdetfn(fit.hd.sm.dc.dhdg,cov1npos,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt2a = plotdetfn(fit.hd.sm.dc.dhdg,cov2npos,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt3a = plotdetfn(fit.hd.sm.dc.dhdg,cov3npos,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
plt4a = plotdetfn(fit.hd.sm.dc.dhdg,cov4npos,main="Depth:cloud & depth:hdgl interaction",cex.main=0.75)
dev.off()
