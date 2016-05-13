#important and extract data into correct dataframe,author: Skander Mulder
#spiderplot(dataframe=a,numberofcol=6,nlines=5,width=0.04,scale = 0.8,colnames=colnameZ, title= "titlename")
#needs a specfic dataframe for importing data row 1 = max row2 = min row3 =mean, other rows (4-7) = (0.05,0.25,0.75,0.95)
#added 8-9 = CI 05 & 95
library(foreign)
library(grid)

renaalidnt_gen<-data.frame(read.dta("RENAAL IDNT.dta"))
renaalidnt_alb<-data.frame(read.dta("Albumin data Month 12.dta"))
renaalidnt_alb<-renaalidnt_alb[,c("AN","ALB12")]

ri<-merge(renaalidnt_gen,renaalidnt_alb,by="AN")
names(ri)<-casefold(names(ri))


obj <-list(	
		"a"=ri$acr_3,
		"b"=ri$bp_rd_3,
		"c"=ri$bp_rd_6,
		"d"=ri$chol, 
		"e" = ri$bp_rd_9, 
		"f" = ri$bp_rd_12, 
		"extra" = ri$bp_rd_15 
	    )


#here an matrix is created, first 2 rows are minval and maxval

#makegrid(grid,30,5)#nb uses scale 1

#untested:
#should give obj2 max & min of obj 1; needed for plot in same graph. 
samexyaxis <-function(object1,object2)
{
	for(i in 1:length(object1))
	{
		object2[2,i:length(object1)] <- object1[2,i:length(object1)]
		object2[1,i:length(object1)] <- object1[1,i:length(object1)]
	}
	return(object2)
}
d<-samexyaxis(a,d)

fillvector<-function(varname)
{
max1<-max(varname,na.rm = TRUE)
min1<-min(varname,na.rm = TRUE)
mean1<-mean(varname,na.rm = TRUE)
q005<-quantile(varname,0.05,na.rm = TRUE)
q025<-quantile(varname,0.25,na.rm = TRUE)
q075<-quantile(varname,0.75,na.rm = TRUE)
q095<-quantile(varname,0.95,na.rm = TRUE)
CI005<- mean1 - 1.96*((sd(varname,na.rm = TRUE))/(sqrt(length(varname))))
CI095<- mean1 + 1.96*((sd(varname,na.rm = TRUE))/(sqrt(length(varname))))
return(c(max1,min1,mean1,q005,q025,q075,q095,CI005,CI095))
}
var<-sd(ri$bpsys_3,na.rm = TRUE)
var/sqrt(length(ri$bpsys_3))

run<-function(object)
{
v <- vector()
	for(i in 1:length(object))
	{
		v <-c(v,fillvector(object[[i]]))
	}
#return(v)
return(as.data.frame(matrix(v,nrow=9,ncol=length(object),byrow=FALSE)))
}

a
#example commands:
fillvector(obj2[[2]])
obj2 <-list("a"=ri$acr_15,"b"=ri$bp_rd_15,"c"=ri$bp_rd_18,"d"=ri$chol, "e" = ri$bp_rd_12,"f" = ri$bp_rd_12,"extra" = ri$bp_rd_12,"extra" = ri$bp_rd_12)
a<-run(obj2)
samexyaxis(run(obj1,a)
#makelines(a,7,clr = "green", lwd = 5) 
plotbars(a,7,0.03)

spiderplot(dataframe=a,width=0.03,scale = 0.8,colnames=names(obj), title= "name",plotbars = FALSE, plotlines=TRUE,plotCI=TRUE,plotnumbers =TRUE)
#a<-run(obj) 
a[3,1] <- 99999
#obj2 <-list("a"=ri$acr_15,"b"=ri$bp_rd_15,"c"=ri$bp_rd_18,"d"=ri$chol, "e" = ri$bp_rd_12,"f" = ri$bp_rd_12,"extra" = ri$bp_rd_12)
#a<-run(obj2)

#plotbars(run(obj2),0.03,34)
a
makeCI(a, clr = "red" ,lwd = 2, lty = "dashed")
makelines(a,"black",2) 
makenumber(dataframe=a,nlines =length(a),fsize= 5)
