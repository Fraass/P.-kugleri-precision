#PMAG
par(mfcol=c(1,2))
plot(pmag.904[,'Declination']
     ,pmag.904[,'Depth']
     #     ,ylim=c(270,265)
    ,ylim=c(400,220)
     #     ,ylim=c(max(age.803.l[,'Depth..CSF.A.']),min(age.803.l[,'Depth..CSF.A.']))
     #     ,ylim=c(90,0)
     ,type='p',pch=1,cex=.75
     ,xlab="Declination(deg)",ylab='DEPTH'
     ,main="Site 904"
)
#points(pmag.904[which(pmag.904[,'Demag.Value']==15 & pmag.904[,"Inclination"] > 0),'Declination'],
#       pmag.904[which(pmag.904[,'Demag.Value']==15 & pmag.904[,"Inclination"] > 0),'Depth.mbsf.']
#       ,pch=16,cex=.75
#)
lines(pmag.904[,'Declination'],
      pmag.904[,"Depth"],col='grey',lwd=.5)
points(pmag.904[which(pmag.904[,"Inclination"] > 0),'Declination'],
       pmag.904[which(pmag.904[,"Inclination"] > 0),'Depth']
       ,pch=16,cex=.75
)

####KUGLERI POSITION
#abline(h=c(268.08),col='red')
#text(150,268,labels="kugleri",cex=.5,col='red')
#text(280,155.25,labels="dehiscens",cex=.5,col='red')

plot(pmag.904[,'Inclination']
     ,pmag.904[,'Depth']
     ,ylim=c(400,220)
     
     #     ,ylim=c(270,265)
#     ,ylim=c(max(age.803.l[,'Depth..CSF.A.']),min(age.803.l[,'Depth..CSF.A.']))
     #     ,ylim=c(90,0)
     ,type='p',pch=1,cex=.75
     ,xlab="Inclination(deg)",ylab='MBSF'
     
)
abline(v=0)
#####KUGLERI POSITION
#abline(h=c(268.08),col='red')
#text(0,268,labels="kugleri",cex=.5,col='red')
#points(pmag.904[which(pmag.904[,'Demag.Value']==15 & pmag.904[,"Inclination"] > 0),'Inclination'],
#       pmag.904[which(pmag.904[,'Demag.Value']==15 & pmag.904[,"Inclination"] > 0),'Depth.mbsf.']
#       ,pch=16,cex=.75
#)
lines(pmag.904[,'Inclination'],
      pmag.904[,"Depth"],col='grey',lwd=.5)


points(pmag.904[which(pmag.904[,"Inclination"] > 0),'Inclination'],
       pmag.904[which(pmag.904[,"Inclination"] > 0),'Depth']
       ,pch=16,cex=.75
)
