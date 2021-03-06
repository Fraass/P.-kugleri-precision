#1218 DtA
par(mfcol=c(1,1),xaxt='s')
plot(1,1,lwd=.75,pch=1,
     ylab="Depth [m, CSF.A]",xlab="Age [Myr]"
     #     ,ylim=c(270,265)
     ,ylim=c(max(age.1218[,'Depth..CSF.A.'],na.rm=T),min(age.1218[,'Depth..CSF.A.'],na.rm=T))
     ,xlim=c(20,28))
     ,type='n'
     ,main="Hole 1218A"
)
#rug(depth.age.l[,2],side=1)
#rug(depth.age.l[,1],side=2)
legend(15,320,legend=c("Nanno[T]","Nanno[B]","Plank[T]","Plank[B]","PMag","Max Mi-1 Iso","Bulk Sr","MPlank Sr")
       ,col=c("Blue","Blue","seagreen","seagreen","Red","darkgoldenrod1","Darkorchid4","darkorchid1"),pch=c(25,24,25,24,15,23,16,16)
       ,bg=c("White","White","White","White","White","darkgoldenrod1")
       ,cex=.75,box.col=NA)
#pmag addition
points(age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
       pch=15,col='red')
#adding points for Foram pref Age Model
#points(depth.age.f[,2:1]
#       ,lwd=.75,pch=1,col='seagreen',cex=1
#)
#adding points for Leckie pref Age Model
#points(depth.age.l[,2:1]
#       ,lwd=.5,pch=1,col='brown',cex=.75
#)
#Highlight kugleri
points(age.1218[which(age.1218[,'Datum'] == 'P. kugleri'),'Age'],
       age.1218[which(age.1218[,'Datum'] == 'P. kugleri'),'Depth..CSF.A.'],
       pch="+",col='black',cex=2)
##Foram Age control points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'T'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='seagreen',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'B'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='seagreen',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'C'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Foram' & age.1218[,'Top.Base'] == 'C'),'Depth..CSF.A.'],
       pch=4,col='seagreen',lwd=3)

##Nanno Age control points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'T'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='blue',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'B'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='blue',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'Tc'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Nanno' & age.1218[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='blue',lwd=3)
#CHEMOSTRAT NOT WORKING YET
segments(age.1218[which(age.1218[,'Source'] == 'Barrera'),'Age']-age.1218[which(age.1218[,'Source'] == 'Barrera'),'Age.Error'],
         age.1218[which(age.1218[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         age.1218[which(age.1218[,'Source'] == 'Barrera'),'Age']+age.1218[which(age.1218[,'Source'] == 'Barrera'),'Age.Error'],
         age.1218[which(age.1218[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
         col='darkorchid',lwd=2)
#Planktic chemostrat points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Stron' & age.1218[,"Top.Base"] == "P"),'Age']#-1.4,
       age.1218[which(age.1218[,'Datum.Type'] == 'Stron' & age.1218[,"Top.Base"] == "P"),'Depth..CSF.A.'],
       pch=16,col='darkorchid1',lwd=1)


##Rad Age control points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'T'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,col='brown',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'B'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,col='brown',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'Tc'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' & age.1218[,'Top.Base'] == 'Tc'),'Depth..CSF.A.'],
       pch=4,col='brown',lwd=3)
#Primary Rad Markers
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'T'
                     & age.1218[,'Primary'] == "Yes"),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'T'
                     & age.1218[,'Primary'] == "Yes"),'Depth..CSF.A.'],
       pch=25,col='brown',bg='brown',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'B'
                     & age.1218[,'Primary'] == "Yes"),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'B'
                     & age.1218[,'Primary'] == "Yes"),'Depth..CSF.A.'],
       pch=24,col='brown',bg='brown',lwd=3)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'Tc'
                     & age.1218[,'Primary'] == "Yes"),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Rad' 
                     & age.1218[,'Top.Base'] == 'Tc'
                     & age.1218[,'Primary'] == "Yes"),'Depth..CSF.A.'],
       pch=4,col='brown',bg='brown',lwd=3)


#Foram Age control points (MY CURRENT BIOSTRAT)
points(age.1218[which(age.1218[,'Datum.Type'] == 'Foram'
                     & age.1218[,'Source'] == 'Current'
                     & age.1218[,'Top.Base'] == 'T'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Foram'
                     & age.1218[,'Source'] == 'Current'
                     & age.1218[,'Top.Base'] == 'T'),'Depth..CSF.A.'],
       pch=25,bg="seagreen",col='seagreen')
points(age.1218[which(age.1218[,'Datum.Type'] == 'Foram'
                     & age.1218[,'Source'] == 'Current'
                     & age.1218[,'Top.Base'] == 'B'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Foram'
                     & age.1218[,'Source'] == 'Current'
                     & age.1218[,'Top.Base'] == 'B'),'Depth..CSF.A.'],
       pch=24,bg="seagreen",col='seagreen')

#Foram Age Control min/max
segments(age.1218[which(age.1218[,'Datum.Type'] == 'Foram'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Foram'),'Min.Depth'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Foram'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Foram'),'Max.Depth'],
         col='seagreen',lwd=2)
#Foram Age Control min/max
segments(age.1218[which(age.1218[,'Datum.Type'] == 'Nanno'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Nanno'),'Min.Depth'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Nanno'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Nanno'),'Max.Depth'],
         col='blue',lwd=2)
#Isotope Points
points(age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Age'],
       age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Depth..CSF.A.'],
       pch=23,col='darkgoldenrod1',bg="darkgoldenrod1",lwd=3)
#Iso Age Control min/max
segments(age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Min.Depth'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Age'],
         age.1218[which(age.1218[,'Datum.Type'] == 'Iso'),'Max.Depth'],
         col='darkgoldenrod1',lwd=2)


#PMAG SECTION
#segments(rep(27,times=length(age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'])),
#         age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.'],
#         age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Age'],
#         age.1218[which(age.1218[,'Datum.Type'] == 'Pmag'),'Depth..CSF.A.']
#)

#segments(22-1/3+.4,285-10.4/3,22+.4,285)
#text(22+.65,282,labels="SB SedRate Est.",cex=.75)




#adjusting Barrera Sr-age curve to be 23myr @ Mi-1
#points(age.1218[which(age.1218[,'Source'] == 'Barrera'),'Age']-.68,
#       age.1218[which(age.1218[,'Source'] == 'Barrera'),'Depth..CSF.A.'],
#       pch=1,col='Red',lwd=3)



