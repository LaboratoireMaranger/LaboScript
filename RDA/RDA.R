####################################################################
# Author:
#  Nicolas Fortin St-Gelais, nicolas.fstgelais@gmail.com
# Description:
#  RDA with variable selection (AIC),
#  Lakes are colored by group
#
#modif for test
####################################################################

# clean the memory
rm(list=ls(all=TRUE))

# library to import
library(vegan)

# import species and environmental matrices
data(varespec)
data(varechem)

# Hellinger transformation of species matrix
data.hel<-decostand(varespec,method="hel")

# divide the lakes in two groups
Lgroups<-as.factor(as.matrix(c(rep("G1",12),rep("G2",12))))

# stepwise (both) model selection usign AIC 
modv1<-capscale(data.hel ~. ,data=varechem)
mod0<-capscale(data.hel ~1,data=varechem)
modSel<-ordistep(mod0,scope=formula(modv1),direction="both")

# variables selected
varSel<-names(modSel[[7]]$envcentre)


# rda function, selected environmental variables are scaled
rda<-rda(data.hel,varechem[,varSel],scale=T)

# select the color and pch for each group
grCol<-c("dodgerblue","chartreuse")[Lgroups] 
grPch<-c(1,2)[Lgroups] 

## plot the rda step by step

# select the scaling
sc<-3

# plot the RDA
plot(rda,type="n",scaling=sc)

# plot sites
points(scores(rda,display="sites",scaling=sc)[,1:2],col=grCol,pch=grPch)

# plot species (red cross OR text)
points(scores(rda,c(1,2),display="species",scaling=sc),pch=3,cex=0.8,lwd=2,col="red")
text(scores(rda,c(1,2),display="species",scaling=sc),labels=rownames(scores(rda,c(1,2),display="species")),col="red")

# plot selected variables
arrows(0,0,summary(rda)$biplot[,1],summary(rda)$biplot[,2],length=0.07)
text(summary(rda)$biplot[,1:2],labels=rownames(summary(rda)$biplot[,1:2]),cex=0.8)

# Hull or ellipse on lakes group
ordihull(rda,Lgroups,show.groups="G1",display="sites",draw=c("lines"),col="dodgerblue",lty=2, lwd=2,scal=sc)
ordihull(rda,Lgroups,show.groups="G2",display="sites",draw=c("lines"),col="chartreuse",lty=2,lwd=2,scal=sc)
ordiellipse(rda,Lgroups,show.groups="G1",display="sites",draw=c("lines"),col="dodgerblue",lty=2,lwd=2,scal=sc,kind="se",conf=0.99)
ordiellipse(rda,Lgroups,show.groups="G2",display="sites",draw=c("lines"),col="chartreuse",lty=2,lwd=2,scal=sc,kind="se",conf=0.99)

