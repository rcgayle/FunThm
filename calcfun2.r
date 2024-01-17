selFun<-function(){a<-runif(1, -2, -1)
                   b<-runif(1, a+0.75, 0.5)
                   c<-runif(1, b+0.75, 2)
                   S<-sample(c(-1,1), 1)
                   Z<-c(a,b,c)
                   g<-function(x){S*(x-a)*(x-b)*(x-c)}
                   return(list(g,S,Z))
                   }
                   
plotFun<-function(G){
      tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-0.5
      yM<-max(G[[1]](t))+0.5
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tM-0.05, -sign(tM)*0.5, 't')
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
       paste('Shown is the graph y=f(t).'), col='blue', cex=1.25)
                }
                
selA<-function(G){
        A1<-seq(G[[3]][1]-0.5, G[[3]][1]-0.2, 0.001)
        A2<-seq(G[[3]][1]+0.2, G[[3]][2]-0.2, 0.001)  
        A3<-seq(G[[3]][2]+0.2, G[[3]][3]-0.2, 0.001)
        A4<-seq(G[[3]][3]+0.2, G[[3]][3]+0.5, 0.001)
        A<-c(A1,A2,A3,A4)
        return(sample(A,1))
                 }
                 
GetPlotE1<-function(){
    x<-c()
    y<-c()
    plot(x,y, xlim=c(-10,10), ylim=c(-10,10), xlab='', ylab='',axes=F)
    text(0,0, "Generate Graph/Question", cex=1.5, col='blue', font=2)
                     }
                     
GetPlotE2<-function(){
    x<-c()
    y<-c()
    plot(x,y, xlim=c(-10,10), ylim=c(-10,10), xlab='', ylab='',axes=F)
    text(0,0, "Generate Graph", cex=1.5, col='blue', font=2)
                     }
                     
quesPlot1<-function(G,a,x, count){
     tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25, font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      if (a<=x){
         t1<-seq(a, x, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(x, a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(x, 0, pch=19, col='blue', lwd=0.1) 
     text(c,par('usr')[3]*0.8,
        paste(
         "What are the signs of F(x), F'(x), and F''(x)?"), col='blue',
         font=2,cex=1.25)
          text(c,par('usr')[3]*0.95,
       paste0('You have three tries, ', 
           3-count, ' remaining.'),col='blue',cex=1.25,font=2)
     text(a,-sign(G[[1]](a))*0.5, 'a', cex=1.25)
     text(x,-sign(G[[1]](x))*0.5, 'x', col='blue',cex=1.25)
                         }                     

                
quesPlot2<-function(G,a,P,Q,count){
     tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2, cex=1.25)
      if (a<=P[[1]]){
         t1<-seq(a, P[[1]], 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(P[[1]], a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(P[[1]], 0, pch=19, col='blue', lwd=0.1) 
     text(c,par('usr')[3]*0.8,
          paste0('Click on an x so that ',Q[[2]]), col='blue',
          font=2, cex=1.25)
     text(c,par('usr')[3]*0.95,
       paste0('You have three tries, ', 
           3-count, ' remaining.'),col='blue',cex=1.25,font=2)
     text(a,-sign(G[[1]](a))*0.5, 'a', cex=1.25)
     text(P[[1]],-sign(G[[1]](P[[1]]))*0.5, 'x', col='blue', cex=1.25)
                         }
                         
quesPlot3<-function(G,a,P){
     tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      if (a<=P[[1]]){
         t1<-seq(a, P[[1]], 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(P[[1]], a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     F<-round(TrapInt(G[[1]], a, P[[1]]),2)
     dF<-round(G[[1]](P[[1]]),2)
     d2F<-round(Der(G)(P[[1]]),2)
     points(P[[1]], 0, pch=19, col='blue', lwd=0.1) 
     text(c,par('usr')[3]*0.65,
          paste('Click on the t-axis to change x.'), 
          font=2,col='blue',cex=1.25)
     text(c,par('usr')[3]*0.85,
          paste0("F(x)= ", F,
                 ",   F'(x)= ", dF,
                 ",  and F''(x)= ", d2F)
                 , col='red',cex=1.5, font=2)
     text(a,-sign(G[[1]](a))*0.5, 'a', cex=1.25)
     text(P[[1]],-sign(G[[1]](P[[1]]))*0.5, 'x', col='blue', cex=1.25)
                         }
                         
corPlot1<-function(G,a,x,V,count){
     tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      if (a<=x){
         t1<-seq(a, x, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(x, a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(x, 0, pch=19, col='blue', lwd=0.1) 
     if (checkVal1(G, a,x, V)==TRUE)
        {
         text(c,par('usr')[3]*0.8,
           paste(posResp()," At the x shown"), col='blue',
           font=2,cex=1.25)
         text(c, par('usr')[3]*0.95,
           paste(QuesVals(V)), col='blue',cex=1.25,font=2)
         }
     else {
        text(c,par('usr')[3]*0.85,
         paste0(negResp(),
           " those aren't the right signs. Try again."), col='blue',
           font=2,cex=1.25)                   
           }
     text(a,-sign(G[[1]](a))*0.5, 'a',cex=1.25)
     text(x,-sign(G[[1]](x))*0.5, 'x', col='blue',cex=1.25)
                         }
                         
corPlot2<-function(G,a,P,Q,count){
     tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      if (a<=P[[1]]){
         t1<-seq(a, P[[1]], 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(P[[1]], a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(P[[1]], 0, pch=19, col='blue', lwd=0.1) 
     if (checkVal2(G, a,P[[1]],Q[[1]])==TRUE)
        {
         text(c,par('usr')[3]*0.85,
           paste0(posResp()," Your value makes ",
           Q[[2]]), col='blue',cex=1.25,font=2)
         }
     else {
         text(c,par('usr')[3]*0.85,
           paste0(negResp()," Your value doesn't make ",
                 Q[[2]]), col='blue',cex=1.25,font=2)                    
           }
     
     text(a,-sign(G[[1]](a))*0.5, 'a',cex=1.25)
     text(P[[1]],-sign(G[[1]](P[[1]]))*0.5, 'x', col='blue',cex=1.25)
                         }
                         
revPlot1<-function(G,a,x){
      tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      if (a<=x){
         t1<-seq(a, x, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(x, a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(x, 0, pch=19, col='red', lwd=0.1)
     text(c,par('usr')[3]*0.8,
           paste("You've used all three tries."), col='red',cex=1.25,font=2)
     text(c,par('usr')[3]*0.95,
           paste0(DerVals(G,a,x)), col='red',cex=1.25,font=2)
     text(a,-sign(G[[1]](a))*0.5, 'a', cex=1.25)
     text(x,-sign(G[[1]](x))*0.5, 'x', col='red', cex=1.25)
                         }
                         
revPlot2<-function(G,a,Q){
      tm<-G[[3]][1]-0.5
      tM<-G[[3]][3]+0.5
      tax<-0
      if (a>=G[[3]][3])
           {tax<-tm+0.05}
      else {tax<-tM-0.05}
      t<-seq(tm, tM, 0.001)
      ym<-min(G[[1]](t))-1
      yM<-max(G[[1]](t))+1
      y<-G[[1]](t)
      plot(t, y, type='l', lwd=2.5, axes=F, xlim=c(tm,tM), ylim=c(ym, yM),
      xlab='', ylab='')
      abline(h=0)
      text(tax, -0.2, 't',cex=1.25)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,0.95*par('usr')[4],
        paste('Shown is the graph y=f(t).'), col='blue', cex=1.25,font=2)
      c<-mean(c(par('usr')[1], par('usr')[2]))
      text(c,par('usr')[4]*0.75, 
        expression(F(x)==integral(f(t)*dt, a, x)), col='blue', 
        font=2,cex=1.25)
      P<-corPoint(G,a, Q)
      if (a<=P[[1]]){
         t1<-seq(a, P[[1]], 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
             }
      else {
         t1<-seq(P[[1]], a, 0.001)
         y1<-rep(0,length(t1))
         y2<-G[[1]](rev(t1))
         z<-c(t1, rev(t1))
         w<-c(y1, y2)
         polygon(z, w, col='skyblue')
            }
     points(P[[1]], 0, pch=19, col='red', lwd=0.1) 
     text(c,par('usr')[3]*0.75,
           paste("You've used all three tries."), col='red',cex=1.25,font=2)
     text(c,par('usr')[3]*0.9,
           paste0("For the point in red ", Q[[2]]), col='red',cex=1.25,font=2)
     text(a,-sign(G[[1]](a))*0.5, 'a',cex=1.25)
     text(P[[1]],-sign(G[[1]](P[[1]]))*0.5, 'x', col='blue',cex=1.25)
                         }
                         



          
                    
              

                             
