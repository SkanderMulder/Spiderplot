#author: S.T. Mulder, skandermulder@hotmail.com

spiderplot <- function(dataframe,nlines = 5,width = 0.01, scale = 0.8,colnames = FALSE, title = FALSE,plotbars = FALSE, plotlines=FALSE, plotCI =FALSE, plotnumbers = TRUE)
{
graphics.off()#resetting plot

fornames  <-viewport(x=0.5, y=0.5, width=scale, height=scale, angle = 0)
pushViewport(fornames)


fornames  <-viewport(x=0.5, y=0.5, width=scale, height=scale, angle = 0)
pushViewport(fornames)

if(title != FALSE)
{
	titlefunct(title,scale)
}


degstep<-360/length(dataframe)
align  <-viewport(x=0.5, y=0.5, width=1, height=1, angle = degstep)

if(colnames != FALSE)
{
	colnamesfunct(dataframe,nlines,colnames,scale)
}
makegrid(dataframe,nlines)
if(plotnumbers != FALSE)
{
makenumber(dataframe,nlines)
}
pushViewport(align) #mby should be commented out

if(plotlines != FALSE)
{
	makelines(dataframe,clr = "black" ,lwd = 2)
}
if(plotbars != FALSE)
{
	plotbars(dataframe,width,clr = "orange")
}

if(plotCI != FALSE)
{
	makeCI(dataframe)
}
#popViewport(align)
#upViewport(0)
}

makegrid <- function(dataframe,nlines)
{
	pistep <- 3.14159265359*2/length(dataframe)
	for (i in 0:length(dataframe))  
	{
		grid.lines(c(.5,0.5+sin(pistep*i)*0.5), c(0.5,0.5+cos(pistep*i)*0.5))
		grid.lines(c(0.5+sin(pistep*(1+i))*0.5,0.5+sin(pistep*i)*0.5), c(0.5+cos(pistep*(1+i))*0.5,0.5+cos(pistep*i)*0.5), gp=gpar(lty="dashed"))
		for (f in 1:nlines)  
		{
			factor <-1/nlines
			grid.lines(c(.5,0.5+sin(pistep*i)*0.5), c(0.5,0.5+cos(pistep*i)*0.5))
			grid.lines(c(0.5+sin(pistep*(1+i))*0.5*factor*f,0.5+sin(pistep*i)*0.5*factor*f), c(0.5+cos(pistep*(1+i))*0.5*factor*f,0.5+cos(pistep*i)*0.5*factor*f), gp=gpar(lty="dashed"))
		}
	}
}

makelines <- function(a, clr = "black" ,lwd = 2)
{
pistep <- 3.14159265359*2/length(a)
for (i in 0:length(a))  
	{
		for (i in 1:length(a))  
		{
		f <- returnfactor1(a[3,i],a[1,i],a[2,i])
		if(length(a) > i)
		{
			f2 <-returnfactor1(a[3,i+1],a[1,i+1],a[2,i+1])
		}
		else
		{
			f2 <-returnfactor1(a[3,1],a[1,1],a[2,1])	
		}
		factor <-1
		grid.lines(c(0.5+sin(pistep*(1+i))*0.5*factor*f2,0.5+sin(pistep*i)*0.5*factor*f),
 c(0.5+cos(pistep*(1+i))*0.5*factor*f2,0.5+cos(pistep*i)*0.5*factor*f),gp=gpar(col=clr, lwd=lwd))	}
	}
}



returnfactor1<-function(mean,max,min)

{
		return((mean-min)/(max-min))
}



rectvp0 <-function(pos,up,down,width,clr)
{
avg = (up + down) /2
grid.rect(x = 0.5, y = 0.5 + (avg),
          width = width, height = (up-down),
          gp=gpar(fill=clr))
}
alinesvp0 <-function(pos,up,down,width_,clr)
{
grid.lines(c(.5,.5), c(.5 + down,.5 + up), gp=gpar(fill=clr))
grid.lines(c(.5-width_,.5+width_), c(.5+ up,.5 + up), gp=gpar(fill=clr))
grid.lines(c(.5-width_,.5+width_), c(.5 + down,.5 + down), gp=gpar(fill=clr))
}

plotbars<-function(a,width,clr)
{	
degstep<-360/length(a)
align  <-viewport(x=0.5, y=0.5, width=1, height=1, angle = degstep)
#pushViewport(align)
	for(i in 1:length(a))
	{
	vp  <-viewport(x=0.5, y=0.5, width=1, height=1, angle = -degstep)
	pushViewport(vp)
	q75<-returnfactor1(a[5,i],a[1,i],a[2,i])
	q25<-returnfactor1(a[6,i],a[1,i],a[2,i])	
	q95<-returnfactor1(a[7,i],a[1,i],a[2,i])
	q05<-returnfactor1(a[4,i],a[1,i],a[2,i])	
	rectvp0(1,q75/2,q25/2,width,clr)
	alinesvp0(1,q95/2,q05/2,width,clr)
	}

}
#a[3,1]<-24
#a[6,1]<-34


colnamesfunct <- function(a,nlines,colnames,scale)
{
pistep <- 3.14159265359*2/length(a)
for (i in 0:length(a))  
{
factor <- 0.9

#grid.text(label = colnames[i], x = 0.5+sin(pistep*i-1)*0.5*factor, y= 0.5+cos(pistep*(i-1))*0.5*factor)
#grid.text(label = colnames[i], x = 0.5+sin(pistep*(i))*0.5*factor y= 0.5+cos(pistep*(i))*0.5*factor)
#grid.circle( r= 0.1, x = 0.5+sin(pistep*i-1)*0.5*factor, y= 0.5+cos(pistep*(i-1))*0.5*factor)

f = 1.15
grid.text(label = colnames[i], x = 0.5+sin(pistep*(i-1))*0.5*f, y= 0.5+cos(pistep*(i-1))*0.5*f, gp=gpar(fontsize = 15))

}
}

titlefunct <-function(title,scale)
{
grid.text( label = title, x = 0.5, y = scale + 0.35,  gp=gpar(fontsize=15, col="black"))
}



makeCI<- function(a, clr = "grey" ,lwd = 1, lty = "dashed")
{
pistep <- 3.14159265359*2/length(a)
for (i in 0:length(a))  
	{
		for (i in 1:length(a))  
		{
		f <- returnfactor1(a[8,i],a[1,i],a[2,i])
		if(length(a) > i)
		{
			f2 <-returnfactor1(a[8,i+1],a[1,i+1],a[2,i+1])
		}
		else
		{
			f2 <-returnfactor1(a[8,1],a[1,1],a[2,1])	
		}
		factor <-1
		grid.lines(c(0.5+sin(pistep*(1+i))*0.5*factor*f2,0.5+sin(pistep*i)*0.5*factor*f),
 c(0.5+cos(pistep*(1+i))*0.5*factor*f2,0.5+cos(pistep*i)*0.5*factor*f),gp=gpar(col=clr, lwd=lwd,lty = lty))	}
	}
	
pistep <- 3.14159265359*2/length(a)
for (i in 0:length(a))  
	{
		for (i in 1:length(a))  
		{
		f <- returnfactor1(a[9,i],a[1,i],a[2,i])
		if(length(a) > i)
		{
			f2 <-returnfactor1(a[9,i+1],a[1,i+1],a[2,i+1])
		}
		else
		{
			f2 <-returnfactor1(a[9,1],a[1,1],a[2,1])	
		}
		factor <-1
		grid.lines(c(0.5+sin(pistep*(1+i))*0.5*factor*f2,0.5+sin(pistep*i)*0.5*factor*f),
 c(0.5+cos(pistep*(1+i))*0.5*factor*f2,0.5+cos(pistep*i)*0.5*factor*f),gp=gpar(col=clr, lwd=lwd,lty = lty))	}
	}
}

makenumber <- function(dataframe,nlines,fsize =5)
{
	pistep <- 3.14159265359*2/length(dataframe)
for (i in 0:length(dataframe))  
{
for (f in 1:nlines)  
{
factor <-1/nlines
factor2 = f/nlines
#print(factor2)
number <- (dataframe[1,i+1]- dataframe[2,i+1]) * factor2 + dataframe[2,1+i]
#print(number)
number<-round(number,digits = 1)
ssplus <- 0.5/(f+1)^1.7
grid.text(label = number, x = 0.5+sin(pistep*(i+ssplus))*0.5*factor*f, y= 0.5+cos(pistep*(i+ssplus))*0.5*factor*f, gp=gpar(fontsize = fsize))
}
}
}

samexyaxis <-function(object1,object2)
{
	for(i in 1:length(object1))
	{
		object2[2,i:length(object1)] <- object1[2,i:length(object1)]
		object2[1,i:length(object1)] <- object1[1,i:length(object1)]
	}
	return(object2)
}


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

#as





