par(mfcol=c(1,1))
######For Composite Age Models#####
#restructuring datafile
order(age.1218[,'Age'],decreasing=F)->o
rbind(age.1218[o,])->age.1218
rm(o)

#constructing a SedRate from age.1218 SHIPBOARD DATA
rm(sed.rate.comp);sed.rate.comp<-0
#gathering sb data
rbind(age.1218[which(age.1218[,'Composite.'] == "Use"),])->age.1218.comp
#calculating sedrate shipboard
for(i in 1:length(age.1218.comp[,"Datum"]))
{
  sed.rate.comp[i-1]<-(
    age.1218.comp[i,'Depth..CSF.A.']-age.1218.comp[i-1,'Depth..CSF.A.']
  )/(
    age.1218.comp[i,'Age']-age.1218.comp[i-1,'Age'])
}


#age.1218[,1:5]->age.1218 #reliced code


#applying the age.1218 SedRates to depth.C
matrix(nrow=length(counts.1218[,'Depth..CSF.A.']),ncol=2)->depth.age.comp
counts.1218[,'Depth..CSF.A.']->depth.age.comp[,1]

for(i in 1:length(counts.1218[,'Depth..CSF.A.'])){
  #finding appropriate sedrate
  max(which(age.1218.comp[,"Depth..CSF.A."] < counts.1218[i,'Depth..CSF.A.']))->X
  #age calc
  #difference in depths
  counts.1218[i,'Depth..CSF.A.']-age.1218.comp[X,'Depth..CSF.A.']->y
  y*{sed.rate.comp[X]^-1}->Z
  Z+age.1218.comp[X,'Age']->depth.age.comp[i,2]
  #units sed.rate=m/myr
}
as.data.frame(depth.age.comp)->depth.age.comp
names(depth.age.comp)[1]<-"Depth..CSF.A."
names(depth.age.comp)[2]<-"Age"
head(depth.age.comp)
merge(depth.age.comp,counts.1218,by="Depth..CSF.A.")->counts.1218.age


##Plots to check data

##Sample points
plot(depth.age.comp[,2:1],lwd=1,pch=20,
     ylab="Depth [m, CSF.A]",xlab="Age [Myr]"
     ,ylim=c(max(age.1218.comp[,'Depth..CSF.A.'],na.rm=T),min(age.1218.comp[,'Depth..CSF.A.'],na.rm=T))
     ,xlim=c(min(age.1218.comp[,'Age']),max(age.1218.comp[,'Age']))
)
rug(depth.age.comp[,2],side=1)
rug(depth.age.comp[,1],side=2)
##Foram Age control points ()
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Top.Base'] == 'T'
                          & age.1218.comp[,'Composite.'] == 'Use'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram' 
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='seagreen')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='seagreen')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='seagreen')

##Foram Age control points (CURRENT)
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'T'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,bg="seagreen",col='seagreen')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Foram'
                          & age.1218.comp[,'Source'] == 'Current'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')
##Nanno Age control points
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'T'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='blue')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='blue')
points(age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Age'],
       age.1218.comp[which(age.1218.comp[,'Datum.Type'] == 'Nanno'
                          & age.1218.comp[,'Composite.'] == 'Use'
                          & age.1218.comp[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='blue')
#pmag
points(age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
       pch=15,col='red')
#Isotope Points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Depth..CSF.A.'],
       pch=23,col='darkgoldenrod1',bg="darkgoldenrod1",lwd=3)