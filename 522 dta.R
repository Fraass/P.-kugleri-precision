par(mfcol=c(1,1),xaxt='s')
plot(depth.age[,2:1],lwd=.75,pch=1,
     ylab="Depth [m, CSF.A]",xlab="Age [Myr]"
     #     ,ylim=c(270,265)
     #,ylim=c(360,260)     
     ,ylim=c(max(age.522[,'Depth..CSF.A.'],na.rm=T)
             ,min(age.522[,'Depth..CSF.A.'],na.rm=T))
     #,xlim=c(min(age.522[,'Age'],na.rm=T)
      #       ,max(age.522[,'Age'],na.rm=T))
     ,xlim=c(19,25)
     ,type='n'
     ,main="Hole 522"
)

#Adding polygon for max age, min age
length(age.522[which(age.522[,'Composite.'] == 'Use'),'Age'])->x
polygon(c(age.522[which(age.522[,'Composite.'] == 'Use'),'Age'],
          age.522[which(age.522[,'Composite.'] == 'Use'),'Age'][x:1]),
        c(age.522[which(age.522[,'Composite.'] == 'Use'),'Min.Depth'], 
          age.522[which(age.522[,'Composite.'] == 'Use'),'Max.Depth'][x:1]),
        col='grey90',
        border=NA
)
rug(depth.age.l[,1],side=2)
legend(19.5,360,legend=c("Nanno[T]","Nanno[B]","Plank[T]","Plank[B]","PMag","Max Mi-1 Iso","Bulk Sr","MPlank Sr")
       ,col=c("Blue","Blue","seagreen","seagreen","Red","darkgoldenrod1","Darkorchid4","darkorchid1"),pch=c(25,24,25,24,15,23,16,16)
       ,bg=c("White","White","White","White","White","darkgoldenrod1")
       ,cex=.75,box.col=NA)
#pmag addition
points(age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
       pch=15,col='red')
#adding points for Foram pref Age Model
#points(depth.age.f[,2:1]
#       ,lwd=.75,pch=1,col='seagreen',cex=1
#)
#adding points for Leckie pref Age Model
points(depth.age.l[,2:1]
       ,lwd=.5,pch=1,col='brown',cex=.75
)

##Foram Age control points
points(age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'T'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='seagreen',lwd=3)
points(age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'B'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='seagreen',lwd=3)
points(age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'C'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Foram' & age.522[,'Top.Base'] == 'C'),'Depth..CSF.A.'],
       pch=4,col='seagreen',lwd=3)

##Nanno Age control points
points(age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'T'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='blue',lwd=3)
points(age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'B'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='blue',lwd=3)
points(age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'Tc'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Nanno' & age.522[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='blue',lwd=3)
#CHEMOSTRAT
segments(age.522[which(age.522[,'Source'] == 'Barrera'),'Age']-age.522[which(age.522[,'Source'] == 'Barrera'),'Age.Error'],
         age.522[which(age.522[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         age.522[which(age.522[,'Source'] == 'Barrera'),'Age']+age.522[which(age.522[,'Source'] == 'Barrera'),'Age.Error'],
         age.522[which(age.522[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         col='darkorchid',lwd=2)
#Planktic chemostrat points
points(age.522[which(age.522[,'Source'] == 'Barrera' & age.522[,"Top.Base"] == "P"),'Age'],
       age.522[which(age.522[,'Source'] == 'Barrera' & age.522[,"Top.Base"] == "P"),'Depth..CSF.A.'],
       pch=16,col='darkorchid1',lwd=1)
#Bulk chemostrat points
points(age.522[which(age.522[,'Source'] == 'Barrera' & age.522[,"Top.Base"] == "Bu"),'Age'],
       age.522[which(age.522[,'Source'] == 'Barrera' & age.522[,"Top.Base"] == "Bu"),'Depth..CSF.A.'],
       pch=16,col='darkorchid4',lwd=1)
##Foram Age control points (MY CURRENT BIOSTRAT)
points(age.522[which(age.522[,'Datum.Type'] == 'Foram'
                     & age.522[,'Source'] == 'Current'
                     & age.522[,'Top.Base'] == 'T'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Foram'
                     & age.522[,'Source'] == 'Current'
                     & age.522[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,bg="seagreen",col='seagreen')
points(age.522[which(age.522[,'Datum.Type'] == 'Foram'
                     & age.522[,'Source'] == 'Current'
                     & age.522[,'Top.Base'] == 'B'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Foram'
                     & age.522[,'Source'] == 'Current'
                     & age.522[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')

#Foram Age Control min/max
segments(age.522[which(age.522[,'Datum.Type'] == 'Foram'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Foram'),'Min.Depth'],
         age.522[which(age.522[,'Datum.Type'] == 'Foram'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Foram'),'Max.Depth'],
         col='seagreen',lwd=2)
#Foram Age Control min/max
segments(age.522[which(age.522[,'Datum.Type'] == 'Nanno'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Nanno'),'Min.Depth'],
         age.522[which(age.522[,'Datum.Type'] == 'Nanno'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Nanno'),'Max.Depth'],
         col='blue',lwd=2)
#Isotope Points
points(age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Age'],
       age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Depth..CSF.A.'],
       pch=23,col='darkgoldenrod1',bg="darkgoldenrod1",lwd=3)
#Iso Age Control min/max
segments(age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Min.Depth'],
         age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Iso'),'Max.Depth'],
         col='darkgoldenrod1',lwd=2)
#adding points for the composite (pref pmag+ kugleri)
points(depth.age.comp[,2:1]
       ,pch=16,col='black',cex=.5
)
#pm
segments(age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Min.Depth'],
         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Age'],
         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Max.Depth'],
         col='red',lwd=2)

#PMAG SECTION
#segments(rep(27,times=length(age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'])),
#         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
#         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Age'],
#         age.522[which(age.522[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.']
#)

#segments(22-1/3+.4,285-10.4/3,22+.4,285)
#text(22+.65,282,labels="SB SedRate Est.",cex=.75)




#adjusting Barrera Sr-age curve to be 23myr @ Mi-1
#points(age.522[which(age.522[,'Source'] == 'Barrera'),'Age']-.68,
#       age.522[which(age.522[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
#       pch=1,col='Red',lwd=3)


#RESULTS FROM TUNING 
#JUST SED RATES RIGHT NOW
#sr.34a<-8.193
#sr.34b<-22.18
#segments(21,303.304,
#         21+{312.6-303.304}*{sr.34a^-1},312.6
#  )
#segments(21,303.304,
#         21+{312.6-303.304}*{sr.34b^-1},312.6
#)


#adding astro agemodel
