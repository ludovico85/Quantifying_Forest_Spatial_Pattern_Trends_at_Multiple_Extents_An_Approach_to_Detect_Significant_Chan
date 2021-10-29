library(bootES)
library(ggplot2)

large<-read.table("data\\ES_2011_1954L.txt", header = T, sep =";")
NPl<-bootES(large, ci.conf = 0.99, data.col = "NP", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
PDl<-bootES(large, ci.conf = 0.99, data.col = "PD", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
EDl<-bootES(large, ci.conf = 0.99, data.col = "ED", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
TEl<-bootES(large, ci.conf = 0.99, data.col = "TE", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
LSIl<-bootES(large, ci.conf = 0.99, data.col = "LSI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AREA_MNl<-bootES(large, ci.conf = 0.99, data.col = "AREA_MN", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
CLUMPYl<-bootES(large, ci.conf = 0.99, data.col = "CLUMPY", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AIl<-bootES(large, ci.conf = 0.99, data.col = "AI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")

df_large<-data.frame(metric = c("NP", "PD", "ED", "TE", "LSI", "AREA_MN", "CLUMPY", "AI"),
               hedges.g = c(NPl$t0, PDl$t0, EDl$t0, TEl$t0, LSIl$t0, AREA_MNl$t0, CLUMPYl$t0, AIl$t0),
               CI_high = c(NPl$bounds[1], PDl$bounds[1], EDl$bounds[1], TEl$bounds[1], LSIl$bounds[1], AREA_MNl$bounds[1], CLUMPYl$bounds[1], AIl$bounds[1]),
               CI_low = c(NPl$bounds[2], PDl$bounds[2], EDl$bounds[2], TEl$bounds[2], LSIl$bounds[2], AREA_MNl$bounds[2], CLUMPYl$bounds[2], AIl$bounds[2]))

######medium scale######
medium<-read.table("ES_1954_2011M.txt", header = T, sep =";")
head(medium)
NPm<-bootES(medium, ci.conf = 0.99, data.col = "NP", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
PDm<-bootES(medium, ci.conf = 0.99, data.col = "PD", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
EDm<-bootES(medium, ci.conf = 0.99, data.col = "ED", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
TEm<-bootES(medium, ci.conf = 0.99, data.col = "TE", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
LSIm<-bootES(medium, ci.conf = 0.99, data.col = "LSI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AREA_MNm<-bootES(medium, ci.conf = 0.99, data.col = "AREA_MN", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
CLUMPYm<-bootES(medium, ci.conf = 0.99, data.col = "CLUMPY", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AIm<-bootES(medium, ci.conf = 0.99, data.col = "AI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")

df_medium<-data.frame(metric = c("NP", "PD", "ED", "TE", "LSI", "AREA_MN", "CLUMPY", "AI"),
                     hedges.g = c(NPm$t0, PDm$t0, EDm$t0, TEm$t0, LSIm$t0, AREA_MNm$t0, CLUMPYm$t0, AIm$t0),
                     CI_high = c(NPm$bounds[1], PDm$bounds[1], EDm$bounds[1], TEm$bounds[1], LSIm$bounds[1], AREA_MNm$bounds[1], CLUMPYm$bounds[1], AIm$bounds[1]),
                     CI_low = c(NPm$bounds[2], PDm$bounds[2], EDm$bounds[2], TEm$bounds[2], LSIm$bounds[2], AREA_MNm$bounds[2], CLUMPYm$bounds[2], AIm$bounds[2]))

####small scale####
small<-read.table("ES_1954_2011S.txt", header = T, sep =";")
head(small)
NPs<-bootES(small, ci.conf = 0.99, data.col = "NP", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
PDs<-bootES(small, ci.conf = 0.99, data.col = "PD", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
EDs<-bootES(small, ci.conf = 0.99, data.col = "ED", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
TEs<-bootES(small, ci.conf = 0.99, data.col = "TE", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
LSIs<-bootES(small, ci.conf = 0.99, data.col = "LSI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AREA_MNs<-bootES(small, ci.conf = 0.99, data.col = "AREA_MN", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
CLUMPYs<-bootES(small, ci.conf = 0.99, data.col = "CLUMPY", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")
AIs<-bootES(small, ci.conf = 0.99, data.col = "AI", group.col = "TIME", contrast = c("1954", "2011"), effect.type = "hedges.g")

df_small<-data.frame(metric = c("NP", "PD", "ED", "TE", "LSI", "AREA_MN", "CLUMPY", "AI"),
                     hedges.g = c(NPs$t0, PDs$t0, EDs$t0, TEs$t0, LSIs$t0, AREA_MNs$t0, CLUMPYs$t0, AIs$t0),
                     CI_high = c(NPs$bounds[1], PDs$bounds[1], EDs$bounds[1], TEs$bounds[1], LSIs$bounds[1], AREA_MNs$bounds[1], CLUMPYs$bounds[1], AIs$bounds[1]),
                     CI_low = c(NPs$bounds[2], PDs$bounds[2], ED$bounds[2], TEs$bounds[2], LSIs$bounds[2], AREA_MNs$bounds[2], CLUMPYs$bounds[2], AIs$bounds[2]))



############### plot like figure 4 #########################################################
df_large$scale<-rep(512, 8)
df_medium$scale<-rep(256, 8)
df_small$scale<-rep(128, 8)

df<-rbind.data.frame(df_large, df_medium, df_small)
df$scale<-as.factor(df$scale)


figure4<-ggplot(df, aes(y=hedges.g, x=metric))+geom_pointrange(aes(ymin=CI_low, ymax=CI_high))+coord_flip()+
  geom_hline(yintercept = 0, colour="black", linetype = "longdash")+facet_wrap(~scale)+theme_bw()
