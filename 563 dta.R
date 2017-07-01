par(mfcol=c(1,1),xaxt='s')
plot(depth.age[,2:1],lwd=.75,pch=1,
     ylab="Depth [m, CSF.A]",xlab="Age [Myr]"
     #     ,ylim=c(270,265)
     #,ylim=c(360,260)     
     ,ylim=c(max(age.563[,'Depth..CSF.A.'],na.rm=T)
             ,min(age.563[,'Depth..CSF.A.'],na.rm=T))
     #,xlim=c(min(age.563[,'Age'],na.rm=T)
     #       ,max(age.563[,'Age'],na.rm=T))
     ,xlim=c(19,25)
     ,type='n'
     ,main="Hole 563"
)

rug(depth.age.l[,1],side=2)
legend(19.5,360,legend=c("Nanno[T]","Nanno[B]","Plank[T]","Plank[B]","PMag","Max Mi-1 Iso","Bulk Sr","MPlank Sr")
       ,col=c("Blue","Blue","seagreen","seagreen","Red","darkgoldenrod1","Darkorchid4","darkorchid1"),pch=c(25,24,25,24,15,23,16,16)
       ,bg=c("White","White","White","White","White","darkgoldenrod1")
       ,cex=.75,box.col=NA)
#pmag addition
points(age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
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
points(age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'T'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='seagreen',lwd=3)
points(age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'B'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='seagreen',lwd=3)
points(age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'C'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Foram' & age.563[,'Top.Base'] == 'C'),'Depth..CSF.A.'],
       pch=4,col='seagreen',lwd=3)

##Nanno Age control points
points(age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'T'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='blue',lwd=3)
points(age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'B'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='blue',lwd=3)
points(age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'Tc'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Nanno' & age.563[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='blue',lwd=3)
#CHEMOSTRAT
segments(age.563[which(age.563[,'Source'] == 'Barrera'),'Age']-age.563[which(age.563[,'Source'] == 'Barrera'),'Age.Error'],
         age.563[which(age.563[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         age.563[which(age.563[,'Source'] == 'Barrera'),'Age']+age.563[which(age.563[,'Source'] == 'Barrera'),'Age.Error'],
         age.563[which(age.563[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         col='darkorchid',lwd=2)
#Planktic chemostrat points
points(age.563[which(age.563[,'Source'] == 'Barrera' & age.563[,"Top.Base"] == "P"),'Age'],
       age.563[which(age.563[,'Source'] == 'Barrera' & age.563[,"Top.Base"] == "P"),'Depth..CSF.A.'],
       pch=16,col='darkorchid1',lwd=1)
#Bulk chemostrat points
points(age.563[which(age.563[,'Source'] == 'Barrera' & age.563[,"Top.Base"] == "Bu"),'Age'],
       age.563[which(age.563[,'Source'] == 'Barrera' & age.563[,"Top.Base"] == "Bu"),'Depth..CSF.A.'],
       pch=16,col='darkorchid4',lwd=1)
##Foram Age control points (MY CURRENT BIOSTRAT)
points(age.563[which(age.563[,'Datum.Type'] == 'Foram'
                     & age.563[,'Source'] == 'Current'
                     & age.563[,'Top.Base'] == 'T'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Foram'
                     & age.563[,'Source'] == 'Current'
                     & age.563[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,bg="seagreen",col='seagreen')
points(age.563[which(age.563[,'Datum.Type'] == 'Foram'
                     & age.563[,'Source'] == 'Current'
                     & age.563[,'Top.Base'] == 'B'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Foram'
                     & age.563[,'Source'] == 'Current'
                     & age.563[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')

#Foram Age Control min/max
segments(age.563[which(age.563[,'Datum.Type'] == 'Foram'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Foram'),'Min.Depth'],
         age.563[which(age.563[,'Datum.Type'] == 'Foram'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Foram'),'Max.Depth'],
         col='seagreen',lwd=2)
#Foram Age Control min/max
segments(age.563[which(age.563[,'Datum.Type'] == 'Nanno'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Nanno'),'Min.Depth'],
         age.563[which(age.563[,'Datum.Type'] == 'Nanno'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Nanno'),'Max.Depth'],
         col='blue',lwd=2)
#Isotope Points
points(age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Age'],
       age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Depth..CSF.A.'],
       pch=23,col='darkgoldenrod1',bg="darkgoldenrod1",lwd=3)
#Iso Age Control min/max
segments(age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Min.Depth'],
         age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Age'],
         age.563[which(age.563[,'Datum.Type'] == 'Iso'),'Max.Depth'],
         col='darkgoldenrod1',lwd=2)
#adding points for the composite (pref pmag+ kugleri)
points(depth.age.comp[,2:1]
       ,pch=16,col='black',cex=.5
)

#PMAG SECTION
#segments(rep(27,times=length(age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'])),
#         age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
#         age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Age'],
#         age.563[which(age.563[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.']
#)

#segments(22-1/3+.4,285-10.4/3,22+.4,285)
#text(22+.65,282,labels="SB SedRate Est.",cex=.75)




#adjusting Barrera Sr-age curve to be 23myr @ Mi-1
#points(age.563[which(age.563[,'Source'] == 'Barrera'),'Age']-.68,
#       age.563[which(age.563[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
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

#finding anchor point
tie.age<-23 #Mi-1
tie.depth<-320.25 #Est. Mi-1 depth


which(abs(time.563[,1]-tie.depth)==min(abs(time.563[,1]-tie.depth)))->a
tie.age-{time.563[a,2]*.01}->tdif
lines(time.563[,2]*.01+tdif
      ,time.563[,1])

