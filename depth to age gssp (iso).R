par(mfcol=c(1,1))
######For Composite Age Models#####
#restructuring datafile
order(age.gssp[,'Age'],decreasing=F)->o
rbind(age.gssp[o,])->age.gssp
rm(o)

#constructing a SedRate from age.gssp SHIPBOARD DATA
rm(sed.rate.comp);sed.rate.comp<-0
#gathering sb data
rbind(age.gssp[which(age.gssp[,'Composite.'] == "Use"),])->age.gssp.comp
#calculating sedrate shipboard
for(i in 1:length(age.gssp.comp[,"Datum"]))
{
  sed.rate.comp[i-1]<-(
    age.gssp.comp[i,'Depth..CSF.A.']-age.gssp.comp[i-1,'Depth..CSF.A.']
  )/(
    age.gssp.comp[i,'Age']-age.gssp.comp[i-1,'Age'])
}


#age.gssp[,1:5]->age.gssp #reliced code


#applying the age.gssp SedRates to depth.C
matrix(nrow=length(iso.gssp[,'Depth..CSF.A.']),ncol=2)->depth.age.comp
iso.gssp[,'Depth..CSF.A.']->depth.age.comp[,1]

for(i in 1:length(iso.gssp[,'Depth..CSF.A.'])){
  #finding appropriate sedrate
  max(which(age.gssp.comp[,"Depth..CSF.A."] < iso.gssp[i,'Depth..CSF.A.']))->X
  #age calc
  #difference in depths
  iso.gssp[i,'Depth..CSF.A.']-age.gssp.comp[X,'Depth..CSF.A.']->y
  y*{sed.rate.comp[X]^-1}->Z
  Z+age.gssp.comp[X,'Age']->depth.age.comp[i,2]
  #units sed.rate=m/myr
}
as.data.frame(depth.age.comp)->depth.age.comp
names(depth.age.comp)[1]<-"Depth..CSF.A."
names(depth.age.comp)[2]<-"Age"
head(depth.age.comp)
merge(depth.age.comp,iso.gssp,by="Depth..CSF.A.")->iso.gssp.age


##Plots to check data

##Sample points
plot(depth.age.comp[,2:1],lwd=1,pch=20,
     ylab="Depth [m, CSF.A]",xlab="Age [Myr]"
     ,ylim=c(max(age.gssp.comp[,'Depth..CSF.A.'],na.rm=T),min(age.gssp.comp[,'Depth..CSF.A.'],na.rm=T))
     ,xlim=c(min(age.gssp.comp[,'Age']),max(age.gssp.comp[,'Age']))
)
rug(depth.age.comp[,2],side=1)
rug(depth.age.comp[,1],side=2)
##Foram Age control points ()
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Top.Base'] == 'T'
                           & age.gssp.comp[,'Composite.'] == 'Use'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram' 
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='seagreen')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='seagreen')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='seagreen')

##Foram Age control points (CURRENT)
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'T'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,bg="seagreen",col='seagreen')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Foram'
                           & age.gssp.comp[,'Source'] == 'Current'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')
##Nanno Age control points
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'T'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='blue')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='blue')
points(age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.gssp.comp[which(age.gssp.comp[,'Datum.Type'] == 'Nanno'
                           & age.gssp.comp[,'Composite.'] == 'Use'
                           & age.gssp.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='blue')
#pmag
points(age.gssp[which(age.gssp[,'Datum.Type'] == 'Pmag'),'Age'],
       age.gssp[which(age.gssp[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
       pch=15,col='red')
#Isotope Points
points(age.gssp[which(age.gssp[,'Datum.Type'] == 'Iso'),'Age'],
       age.gssp[which(age.gssp[,'Datum.Type'] == 'Iso'),'Depth..CSF.A.'],
       pch=23,col='darkgoldenrod1',bg="darkgoldenrod1",lwd=3)