# ---------------------------
# R version: 3.5.2
# Code: 4/06/2019
# Title: 'Descriptive and Substantive Representation in Congress: Evidence from 80,000 Congressional Inquiries'
# Authors: Kenneth Lowande, Melinda Richtie, and Erinn Lauterbach
# Summary: Replication code for producing all tables and figures. 
# Please report errors to: lowande@umich.edu
# ---------------------------

cat("\014")
rm(list=ls())
# install.packages(c('cem','ggplot2','foreach','doBy','doMC','stargazer','MASS','sandwich','lmtest','doRNG'))
# foreach/cem installation + Mac will require https://www.xquartz.org/
require(MASS)
require(cem)
require(ggplot2)
require(foreach)
require(doMC)
require(doBy)
require(doRNG)
require(stargazer)
source('cluster-se.R')
source('split-del.R')

# ---------------------------
# PRINT ARTICLE
# ---------------------------

# Figure 1: Descriptive Representation in Congress
load('f1-house.Rdata')
load('f1-senate.Rdata')
clrs <- c('#d21a1d','#0000b3','#ABABAB')
f1a=ggplot(house, aes(x=cong,y=no,color=ident)) + geom_line(size=1.2) + geom_point() + labs(x="Congress",y="Representatives") +
    theme_bw() + scale_colour_manual(values=clrs) + theme(legend.title=element_blank())
f1b=ggplot(senate, aes(x=cong,y=no,color=ident)) + geom_line(size=1.2) + geom_point() + labs(x="Congress",y="Representatives") +
    theme_bw() + scale_colour_manual(values=clrs) + theme(legend.title=element_blank())
# ---------------------------

# Table 1: Data Coverage for Legislator Interventions; summary table created manually from FOIA records.
# ---------------------------

# Figure 2: Representatives and the Represented
load('f2-reps.Rdata') # House member and district characteristics in 108-111th Congress
b <- summaryBy(vet.any ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # veteran legislators
c <- summaryBy(vet.perc ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # veteran population
d <- summaryBy(female ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # female legislators
e <- summaryBy(white.pop ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # racial/ethnic minority population
f <- summaryBy(white ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # racial/ethnic minority legislators
g <- summaryBy(tw.ideo.mean ~ GEO_ID,data=reps,FUN=mean,keep.names = T) # ideology
load('f2-usa.Rdata') # US House district map (limited to continguous states)
f2a=ggplot() + geom_map(data = b, aes(map_id = GEO_ID, fill = vet.any), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.2) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2b=ggplot() + geom_map(data = c, aes(map_id = GEO_ID, fill = vet.perc), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.06) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2c=ggplot() + geom_map(data = d, aes(map_id = GEO_ID, fill = female), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=0.2) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2d=ggplot() + geom_map(data = g, aes(map_id = GEO_ID, fill = tw.ideo.mean), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#FFFFFF', mid = '#C0C0C0', high = '#000000', midpoint=-0.25) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2e=ggplot() + geom_map(data = f, aes(map_id = GEO_ID, fill = white), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#000000', mid = '#C0C0C0', high = '#FFFFFF', midpoint=0.6) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
f2f=ggplot() + geom_map(data = e, aes(map_id = GEO_ID, fill = white.pop), map = usa.f) +
  expand_limits(x = usa.f$long, y = usa.f$lat) + scale_fill_gradient2(low = '#000000', mid = '#808080', high = '#FFFFFF', midpoint=0.55) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) 
# ---------------------------

rm(house,senate,contacts,reps,b,c,d,e,f)

load('t2-b10-main.Rdata')
# Table 2: Representation Differences Across Members with Military Service
# note: summary table created manually from the following commands
main.v1=splitdel(main,7)
main.v2=splitdel(main,8)
t.test(main$vet.all.d~main$vet.any) # row 1
t.test(main.v1$vet.all.d~main.v1$vet.any) # row 2
t.test(main$vet.all.d~main$vet.mainbranch) # row 3
t.test(main.v2$vet.all.d~main.v2$vet.mainbranch) # row 4
# ---------------------------

# Table 3: Military Service and Veterans Representation
v1=lm(vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp),data=main)
v1c=cl(main,v1,main$idno)
v2=lm(vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.mainbranch+log(vets.exp),data=main)
v2c=cl(main,v2,main$idno)
vars=c('vet.10.12.d','chamber','cong','vet.any','vets.exp')
main.v0=main[complete.cases(main[,vars]),]
main.v0$idno=as.character(main.v0$idno)
v3=lm(vet.10.12.d~chamber+dwnom1+cong+
        vet.any+log(vets.exp),data=main.v0)
v3c=cl(main.v0,v3,main.v0$idno)
v4=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
        vet.mainbranch+log(vets.exp),data=main.v0)
v4c=cl(main.v0,v4,main.v0$idno)
stargazer(v1c,v2c,v3c,v4c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table 4: Representation Differences Across Gender
# note: summary table created manually from the following commands
main.w=splitdel(main,6)
t.test(main$fem.all.d~main$female) # row 1
t.test(main.w$fem.all.d~main.w$female) # row 2
# ---------------------------

# Table 5: Gender and Women’s Representation
w1=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+
        female,data=main)
w1c=cl(main,w1,main$idno)
w2=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female,data=main)
w2c=cl(main,w2,main$idno)
stargazer(w1c,w2c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table 6: Race/Ethnicity and Minority Representation
r1=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main)
r1c=cl(main,r1,main$idno)
r2=lm(race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r2c=cl(main,r2,main$idno)
vars=c('race.ni.12.d','chamber','cong','tw.ideo.mean','nonwhite','white.pop')
main.r0=main[complete.cases(main[,vars]),]
main.r0$idno=as.character(main.r0$idno)
r3=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main.r0)
r3c=cl(main.r0,r3,main.r0$idno)
r4=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main.r0)
r4c=cl(main.r0,r4,main.r0$idno)
stargazer(r1c,r2c,r3c,r4c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# ---------------------------
# SUPPLEMENTARY INFORMATION
# ---------------------------

# Table B1: Military Service and Veterans Representation (112th Congress)
main.12=main[main$cong==112,]
main.12$idno=as.character(main.12$idno)
v5=lm(vet.12.d~chamber+dwnom1+
        vet.any+log(vets.exp),data=main.12)
v5c=cl(main.12,v5,main.12$idno)
v6=lm(vet.12.d~chamber+dwnom1+
        vet.mainbranch+log(vets.exp),data=main.12)
v6c=cl(main.12,v6,main.12$idno)
v7=glm.nb(vet.12~chamber+dwnom1+
            vet.any+log(vets.exp),data=main.12)
v7c=cl(main.12,v7,main.12$idno)
v8=glm.nb(vet.12~chamber+dwnom1+
            vet.mainbranch+log(vets.exp),data=main.12)
v8c=cl(main.12,v8,main.12$idno)
stargazer(v5c,v6c,v7c,v8c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B2: Representation Differences Across Members with Military Service (Matching)
# note: summary table created manually from the following commands
th <- levels(main$cong)
house <- levels(main$chamber)
main$log.vets.exp=log(main$vets.exp)
dt.na=main[is.na(main$vet.all.d)==F,c(13,14,7,4,5,28)] # row 1
imbalance(group=dt.na$vet.any,data=dt.na[,c(1,4:6)])
mat1 <-  cem('vet.any',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est1 <- att(mat1, vet.all.d ~ vet.any, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,15,7,4,5,28)] # row 2
imbalance(group=dt.na$vet.any,data=dt.na[,c(1,4:6)])
mat2 <-  cem('vet.any',dt.na,cutpoints=list(log.vets.exp=10),
             drop='vet.10.12.d',grouping=list(chamber=house),keep.all=T)
est2 <- att(mat2, vet.10.12.d ~ vet.any, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,14,8,4,5,28)] # row 3
imbalance(group=dt.na$vet.mainbranch,data=dt.na[,c(1,4:6)])
mat3 <-  cem('vet.mainbranch',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est3 <- att(mat3, vet.all.d ~ vet.mainbranch, data = dt.na)
dt.na=main[is.na(main$vet.all.d)==F,c(13,15,8,4,5,28)] # row 4
imbalance(group=dt.na$vet.mainbranch,data=dt.na[,c(1,4:6)])
mat4 <-  cem('vet.mainbranch',dt.na,cutpoints=list(log.vets.exp=10,tw.ideo.mean=10),
             drop='vet.10.12.d',grouping=list(chamber=house),keep.all=T)
est4 <- att(mat4, vet.10.12.d ~ vet.mainbranch, data = dt.na)
# ---------------------------

# Table B3: Military Service and Veterans Representation (Additional Controls)
main$idno=as.character(main$idno)
v9=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp)+poverty+seniority,data=main)
v9c=cl(main,v9,main$idno[is.na(main$vet.10.12.d)==F])
v10=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp)+poverty+seniority,data=main)
v10c=cl(main,v10,main$idno[is.na(main$vet.10.12.d)==F])
v11=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
         vet.any+log(vets.exp)+poverty+seniority,data=main)
v11c=cl(main,v11,main$idno[is.na(main$vet.10.12.d)==F])
v12=lm(vet.10.12.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
         vet.mainbranch+log(vets.exp)+poverty+seniority,data=main)
v12c=cl(main,v12,main$idno[is.na(main$vet.10.12.d)==F])
stargazer(v9c,v10c,v11c,v12c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B4: Military Service and Veterans Representation (Alternative Dependent Variable)
v13=lm(r.vet.all.d~chamber+dwnom1+as.factor(cong)+
        vet.any+log(vets.exp),data=main)
v13c=cl(main,v13,main$idno)
v14=lm(r.vet.all.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp),data=main)
v14c=cl(main,v14,main$idno)
v15=lm(r.vet.10.12.d~chamber+dwnom1+cong+
         vet.any+log(vets.exp),data=main)
v15c=cl(main,v15,main$idno[is.na(main$r.vet.10.12.d)==F])
v16=lm(r.vet.10.12.d~chamber+dwnom1+as.factor(cong)+
         vet.mainbranch+log(vets.exp),data=main)
v16c=cl(main,v16,main$idno[is.na(main$r.vet.10.12.d)==F])
stargazer(v13c,v14c,v15c,v16c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B5: Representation Differences Across Genders (Matching)
# note: summary table created manually from the following commands
dt.wna=main[,c(11,6,4,5,16)] # row 1
imbalance(group=dt.wna$female,data=dt.wna[,c(1,3:4)])
mat5 <-  cem('female',dt.wna,cutpoints=list(dwnom1=10),
             drop='fem.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est5 <- att(mat5, fem.all.d ~ female, data = dt.wna)
dt.wna=main[,c(13,4,5,16,6)] # row 2
imbalance(group=dt.wna$female,data=dt.wna[,c(1:3)])
mat6 <-  cem('female',dt.wna,cutpoints=list(tw.ideo.mean=10),
             drop='fem.all.d',grouping=list(cong=th,chamber=house),keep.all=T)
est6 <- att(mat6, fem.all.d ~ female, data = dt.wna)
# ---------------------------

# Table B6: Gender and Women’s Representation (Additional Controls)
w3=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+
        female+poverty+seniority,data=main)
w3c=cl(main,w3,main$idno)
w4=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female+poverty+seniority,data=main)
w4c=cl(main,w4,main$idno)
w5=lm(fem.all.d~chamber+dwnom1+as.factor(cong)+as.factor(state.abbrev)+
        female+poverty+seniority,data=main)
w5c=cl(main,w5,main$idno)
w6=lm(fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+as.factor(state.abbrev)+
        female+poverty+seniority,data=main)
w6c=cl(main,w6,main$idno)
stargazer(w3c,w4c,w5c,w6c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B7: Gender and Women’s Representation (Alternative Dependent Variable)
w7=lm(r.fem.all.d~chamber+dwnom1+as.factor(cong)+
        female,data=main)
w7c=cl(main,w7,main$idno)
w8=lm(r.fem.all.d~chamber+tw.ideo.mean+as.factor(cong)+
        female,data=main)
w8c=cl(main,w8,main$idno)
stargazer(w7c,w8c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B8: Representation Differences Across Race/Ethnicity (Matching)
# note: summary table created manually from the following commands
dt.rna=main[,c(4,5,10,17,9)] # row 1
imbalance(group=dt.rna$nonwhite,data=dt.rna[,c(1:3)]) 
mat7 <-  cem('nonwhite',dt.rna,cutpoints=list(white.pop=10),
             drop='race.ni.d',grouping=list(cong=th,chamber=house),keep.all=T)
est7 <- att(mat7, race.ni.d ~ nonwhite, data = dt.rna)
dt.rna=main[,c(4,5,13,18,9)]  # row 2
imbalance(group=dt.rna$nonwhite,data=dt.rna[,c(1:3)]) 
mat8 <-  cem('nonwhite',dt.rna,cutpoints=list(tw.ideo.mean=10),
             drop='race.ni.12.d',grouping=list(cong=th,chamber=house),keep.all=T)
est8 <- att(mat8, race.ni.12.d ~ nonwhite, data = dt.rna)
# ---------------------------

# Table B9: Race/Ethnicity and Minority Representation (Additional Controls)
r5=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite+poverty+seniority,data=main)
r5c=cl(main,r5,main$idno)
r6=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite+poverty+seniority,data=main)
r6c=cl(main,r6,main$idno[is.na(main$race.ni.12.d)==F])
r7=lm(race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite+poverty+seniority,data=main)
r7c=cl(main,r7,main$idno)
r8=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite+poverty+seniority,data=main)
r8c=cl(main,r8,main$idno[is.na(main$race.ni.12.d)==F])
r9=lm(race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+as.factor(state.abbrev)+
        nonwhite+poverty+seniority,data=main)
r9c=cl(main,r9,main$idno)
r10=lm(race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r10c=cl(main,r10,main$idno[is.na(main$race.ni.12.d)==F])
r11=lm(race.ni.d~chamber+as.factor(cong)+white.pop+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r11c=cl(main,r11,main$idno)
r12=lm(race.ni.12.d~chamber+as.factor(cong)+white.pop+as.factor(state.abbrev)+
         nonwhite+poverty+seniority,data=main)
r12c=cl(main,r12,main$idno[is.na(main$race.ni.12.d)==F])
stargazer(r5c,r6,r7c,r8c,r9c,r10c,r11c,r12c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Table B10: Race/Ethnicity and Minority Representation (Alternative Dependent Variable)
r13=lm(r.race.ni.d~chamber+as.factor(cong)+tw.ideo.mean+
        nonwhite,data=main)
r13c=cl(main,r13,main$idno)
r14=lm(r.race.ni.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r14c=cl(main,r14,main$idno)
r15=lm(r.race.ni.12.d~chamber+as.factor(cong)+tw.ideo.mean+
         nonwhite,data=main)
r15c=cl(main,r15,main$idno[is.na(main$r.race.ni.12.d)==F])
r16=lm(r.race.ni.12.d~chamber+as.factor(cong)+white.pop+
        nonwhite,data=main)
r16c=cl(main,r16,main$idno[is.na(main$r.race.ni.12.d)==F])
stargazer(r13c,r14c,r15c,r16c,digits=3,out.header=F,star.cutoffs=NA,out='DESIRED FILEPATH')
# ---------------------------

# Figure C1: Detecting Meaningful Effects in Congress
# Note, loads stored simulation results unless otherwise specified.

#rm(list=ls())
#load('fc1-districts.Rdata')
#registerDoMC(cores=detectCores()-1)

# Figure C1(a): Women
#N <- nrow(districts)
#effects <- seq(0.01,0.45,by=0.02)
#iter <- 1:1000
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$female[i] - B2*districts$tw.ideo.mean[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,5,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('female',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ female, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim=rbind(sim,x)
#  }
# return(sim)
#}
load('sim-1.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims=summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1a=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())

# Figure C1(b): Racial/Ethnic Minorities
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$minority[i] - B2*districts$white.pop[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,6,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('minority',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ minority, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim <- rbind(sim,x)
#  }
# return(sim)
#}
load('sim-2.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims <- summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1b=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())

# Figure C1(c): Veterans
#sim <- data.frame()
#x <- data.frame(iter=numeric(1),effects=numeric(1),est=numeric(1),se=numeric(1))
#sim=foreach(t=1:length(effects),.combine=rbind,.options.RNG=1990) %dorng% {
#  for (j in 1:length(iter)) {
#    Y <- 1:N
#    for (i in Y) {
#      B1 <- effects[t]
#      B2 <- 0.1
#      model <- B1*districts$vet.any[i] - B2*districts$vet.perc[i] + rnorm(1,0,1)
#      if (model<0) {model=0} else if (model>1) {model=1} else {model=model}
#      Y[i] <- rbinom(1,1,prob=model)
#    }
#    pp <- cbind(districts[,c(2,4,10,7:9)],Y)
#    th <- levels(pp$cong)
#    house <- levels(pp$chamber)
#    mat <-  cem('vet.any',pp,cutpoints=list(white.pop=10,vet.perc=10,tw.ideo.mean=10),
#                drop='Y',grouping=list(cong=th,chamber=house),keep.all=T)
#    est <- att(mat, Y ~ vet.any, data = pp)
#    x[1,1] <- iter[j]
#    x[1,2] <- effects[t]
#    x[1,3] <- est$att.model[1,2]
#    x[1,4] <- est$att.model[2,2]
#    sim <- rbind(sim,x)
#  }
# return(sim)
#}
load('sim-3.Rdata')
sim$pos <- 0
sim$pos[sim$est>0] <- 1
sim$sig <- 0
sim$sig[which((sim$est-(1.96*sim$se))>0)] <- 1
sims=summaryBy(pos + sig + est ~ effects, data=sim, FUN=mean, keep.names = T)
fc1c=ggplot(sims) + geom_line(size=1.2,aes(x=effects/0.45,y=pos,color='Positive')) + 
  geom_line(size=1.2,aes(x=effects/0.45,y=sig,color='Significant\n(p<0.05)')) + 
  labs(x="Simulated Effect Size (SD)",y="Proportion Accurate") + scale_y_continuous(limits=c(0, 1)) +
  theme_bw() + theme(legend.title=element_blank())
# ---------------------------
