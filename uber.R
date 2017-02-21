uber <- read.csv("Uber request data.csv",header = T)
uber$Request.time=as.character(uber$Request.time)
uber$Request.time<-substring(uber$Request.time,1,2)
d=table(uber$Request.time,uber$Pickup.point)
d=as.data.frame.matrix(d)
d <- do.call(rbind, d)
barplot(d, beside = TRUE, ylim=c(0,500), legend.text = rownames(d),args.legend = list(x = "topleft", bty="n"))
uber$ts=c()
addc <- function(uber)
{
    for(i in c(1:6766))
    {  
        if(as.integer(uber$Request.time[i])<4)
        {
            uber$ts[i]="pm"  
        }
        else if(as.integer(uber$Request.time[i])<10)
        {
            uber$ts[i]="mr"  
        }
        else if(as.integer(uber$Request.time[i])<17)
        {
            uber$ts[i]="dt"  
        }
        else if(as.integer(uber$Request.time[i])<22)
        {
            uber$ts[i]="er"  
        }
        else if(as.integer(uber$Request.time[i])<24)
        {
            uber$ts[i]="ln"  
        }
        
    }
    uber
    
}
g=addc(uber)
k=table(g$ts)
barplot(k)
statusc=table(uber$Request.time,uber$Status)
statusc=as.data.frame.matrix(statusc)
statusc <- do.call(rbind, statusc)
barplot(statusc, beside = TRUE, ylim=c(0,500), legend.text = rownames(statusc),args.legend = list(x = "topleft", bty="n"))
ubert=subset(g,g$ts=="mr")
uberts=table(ubert$Pickup.point,ubert$Status)
barplot(uberts, beside = TRUE, ylim=c(0,1000), legend.text = rownames(uberts),args.legend = list(x = "topleft", bty="n"))
ubert2=subset(g,g$ts=="er")
uberts2=table(ubert2$Pickup.point,ubert2$Status)
barplot(uberts2, beside = TRUE, ylim=c(0,1400), legend.text = rownames(uberts2),args.legend = list(x = "topleft", bty="n"))

