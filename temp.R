addn <- function(d,e)
{
   for(i in c(1:24))
   {  
       e=c(e,d[i,1])
       
       e=c(e,d[i,2])
       e=c(e,d[i,3])
       
       
   }
    e

}
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



