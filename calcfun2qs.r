Z2<-function(a){
           if (a-floor(a/2)*2==1)
               {return(1)}
           else {return(-1)}
                   }
                   
quesPt1<-function(G, a){
          A<-seq(G[[3]][1]-0.5, G[[3]][1]+0.5, 0.1)
          A<-A[abs(A-a)>0.5]
          A<-A[abs(A-G[[3]][1])>0.3]
          A<-A[abs(A-G[[3]][2])>0.3]
          A<-A[abs(A-G[[3]][3])>0.3]
          return(sample(A,1))
                       }
                       
DerVals<-function(G,a,x){
           i<-sign(TrapInt(G[[1]],a,x))
              if (i==-1) {i<-2}
              else {i<-i}
           j<-sign(G[[1]](x))
              if (j==-1) {j<-2}
              else {j<-j} 
           k<-sign(Der(G)(x))
              if (k==-1) {k<-2}
              else {k<-k} 
          return(QUES[[i]][[j]][k])
                       } 
                       
QuesVals<-function(V){
         i<-0
         if (V[1]==1)
             {i<-1}
         else {i<-2}
         j<-0
         if (V[2]==1)
             {j<-1}
         else {j<-2}
         k<-0
         if (V[3]==1)
             {k<-1}
         else {k<-2}
         return(QUES[[i]][[j]][k])
                       }

quesFun<-function(G, a){
     V<-c(0,0,0)
     Q<-NULL
     if (a<G[[3]][1] & G[[1]](a)>0)
           {i<-sample(1:2,1)
            j<-sample(1:2,1)
            V<-c(Z2(i),Z2(j),-1)
            Q<-QUES[[i]][[j]][2]
            return(list(V,Q))
            }
     else if (a<G[[3]][1] & G[[1]](a)<0)  
           {i<-sample(1:2,1)
            j<-sample(1:2,1)
            V<-c(Z2(i),Z2(j), -1)
            Q<-QUES[[i]][[j]][1]
            return(list(V,Q))
            }
     else if (a>G[[3]][1] & a<G[[3]][3] & G[[1]](a)>0)  
            {i<-sample(1:2,1)
             k<-sample(1:2,1)
             V<-c(Z2(i), 1, Z2(k))
             Q<-QUES[[i]][[1]][k]
             return(list(V,Q))
            }
     else if (a>G[[3]][1] & a<G[[3]][3] & G[[1]](a)<0)  
           {i<-sample(1:2,1)
             k<-sample(1:2,1)
             V<-c(Z2(i), -1, Z2(k))
             Q<-QUES[[i]][[2]][k]
             return(list(V,Q))
            }
     else if (a>G[[3]][3] & G[[1]](a)>0)
           {i<-sample(1:2,1)
            j<-sample(1:2,1)
            V<-c(Z2(i),Z2(j),1)
            Q<-QUES[[i]][[j]][1]
            return(list(V,Q))
            }
     else if (a>G[[3]][1] & G[[1]](a)<0)  
           {i<-sample(1:2,1)
            j<-sample(1:2,1)
            V<-c(Z2(i),Z2(j),-1)
            Q<-QUES[[i]][[j]][2]
            return(list(V,Q))
            }
           
           }
           


QUES<-list(
         list(c("F(x)>0, F'(x)>0, and F''(x)>0.",
                  "F(x)>0, F'(x)>0, and F''(x)<0."),
             c("F(x)>0, F'(x)<0, and F''(x)>0.",
                   "F(x)>0, F'(x)<0, and F''(x)<0.")),
        list(c("F(x)<0, F'(x)>0, and F''(x)>0.",
                   "F(x)<0, F'(x)>0, and F''(x)<0."),
             c("F(x)<0, F'(x)<0, and F''(x)>0.",
                  "F(x)<0, F'(x)<0, and F''(x)<0."))
       )
       
checkVal1<-function(G, a, x, V){
    if (sign(TrapInt(G[[1]], a, x))==sign(V[1]) &
        sign(G[[1]](x))==sign(V[2])  &
        (sign(Der(G)(x))==sign(V[3])| abs(Der(G)(x))<=0.05))
           {return(TRUE)}
    else {return(FALSE)}
                             }
     
checkVal2<-function(G,a,x, V){
     if (V[1]==1 & V[2]==1 & V[3]==1 
                & TrapInt(G[[1]], a, x)>0 & G[[1]](x)>0 & Der(G)(x)>0)
           {return(TRUE)}
     else if (V[1]==1 & V[2]==1 & V[3]==-1  
               & TrapInt(G[[1]], a, x)>0 & G[[1]](x)>0 & Der(G)(x)<0)
           {return(TRUE)}
     else if (V[1]==1 & V[2]==-1 & V[3]==1 
               & TrapInt(G[[1]], a, x)>0 & G[[1]](x)<0 & Der(G)(x)>0)
           {return(TRUE)}
     else if (V[1]==1 & V[2]==-1 & V[3]==-1 
               & TrapInt(G[[1]], a, x)>0 & G[[1]](x)<0 & Der(G)(x)<0)
           {return(TRUE)}
     else if (V[1]==-1 & V[2]==1 & V[3]==1 
               & TrapInt(G[[1]], a, x)<0 & G[[1]](x)>0 & Der(G)(x)>0)
           {return(TRUE)}
     else if (V[1]==-1 & V[2]==1 & V[3]==-1 
               & TrapInt(G[[1]], a, x)<0 & G[[1]](x)>0 & Der(G)(x)<0)
           {return(TRUE)}
     else if (V[1]==-1 & V[2]==-1 & V[3]==1 
               & TrapInt(G[[1]], a, x)<0 & G[[1]](x)<0 & Der(G)(x)>0)
           {return(TRUE)}
     else if (V[1]==-1 & V[2]==-1 & V[3]==-1 
               & TrapInt(G[[1]], a, x)<0 & G[[1]](x)<0 & Der(G)(x)<0)
           {return(TRUE)}
     else {return(FALSE)}
                    }
                    
negResp<-function(){
         Resp<-c('Oops!', 'Doggone!', 'Shoot!', '#!&$*@!!')
         return(sample(Resp,1))
                   }
                   
                   
posResp<-function(){
         Resp<-c('Awesome!', 'Swweeet!', '¡Muy Bueno!', 'Toll Mensch!',
                   'Fantastique!', 'Mooi!', 'Molto Bene!')
         return(sample(Resp,1))
                   }
                   
corPoint<-function(G, a,Q){
       z<-seq(G[[3]][1]-0.5, G[[3]][3]+0.5, 0.05)
       w<-rep(0, length(z))
       for (i in 1:length(z)){
          if (checkVal2(G, a, z[i], Q[[1]])==TRUE 
               & abs(G[[1]](z[i]))>0.5)
                {w[i]<-z[i]}
                             }
       w<-w[w!=0]
       if (length(w)!=0)
             {return(list(w[1],0))}
       else {return(NULL)}
                           }
                           
TrapInt<-function(f, a, x){
        if (a<=x){
            z<-seq(a, x, 0.001)
            y<-f(z)
            N<-length(z)
            return(0.001*(0.5*(y[1]+y[N])+sum(y[2:N-1])))
                  }
        else {
            z<-seq(x, a, 0.001)
            y<-f(z)
            N<-length(z)
            return(-0.001*(0.5*(y[1]+y[N])+sum(y[2:N-1])))
              }
                            }
                            
Der<-function(G){
         a<-G[[3]][1]
         b<-G[[3]][2]
         c<-G[[3]][3]
         S<-G[[2]]
         return(
            function(t){S*((t-b)*(t-c)+(t-a)*(t-c)+(t-a)*(t-b))}
               )
                 }
                