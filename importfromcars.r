#important and extract data into correct dataframe,author: Skander Mulder
#spiderplot(dataframe=a,numberofcol=6,nlines=5,width=0.04,scale = 0.8,colnames=colnameZ, title= "titlename")
#needs a specfic dataframe for importing data row 1 = max row2 = min row3 =mean, other rows (4-7) = (0.05,0.25,0.75,0.95)
#added 8-9 = CI 05 & 95

#makesubset
airquality_lowwind<-subset(airquality,airquality$Wind <9.7)
airquality_highwind<-subset(airquality,airquality$Wind >9.7)

#run makes spiderplot drawing objects
spiderplotobj_airquality_lowwind<-run(airquality_lowwind)
spiderplotobj_airquality_highwind<-samexyaxis(spiderplotobj_airquality_lowwind,run(airquality_highwind))

#run spiderplot ->
spiderplot(title = substitute(airquality_lowwind),dataframe = spiderplotobj_airquality_lowwind ,colnames = names(airquality_lowwind), plotCI =FALSE, ,plotlines =FALSE )
makelines(spiderplotobj_airquality_lowwind,"blue",2) 
makeCI( spiderplotobj_airquality_lowwind, clr = "blue" ,lwd = 2, lty = "dashed")
makelines(spiderplotobj_airquality_highwind,"red",2) 
makeCI( spiderplotobj_airquality_highwind, clr = "red" ,lwd = 2, lty = "dashed")

plotbars(spiderplotobj_airquality_highwind,0.05,add.alpha("red",0.1))
plotbars(spiderplotobj_airquality_lowwind,0.05,add.alpha("blue",0.1))


