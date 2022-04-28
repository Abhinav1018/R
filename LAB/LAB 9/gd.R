rm(list=ls())
gd<-function(x,y,m,c,alpha,conv_thr,iter){
  iterations=0
  Lf=0
  while(iterations<=iter){
    y_pred=m*x+c
    Lf_new=0.5*(sum(y_pred-y)^2)
    m=m-alpha*sum((y_pred-y)*x)
    c=c-alpha*sum(y_pred-y)
    if(abs(Lf-Lf_new)<conv_thr){
      break;
    }
    Lf=Lf_new
    iterations=iterations+1
  }
  return(paste('Optimum Slope',m,"Optimum Intercept",c,"Number of iterations",iterations,"Loss function",Lf))
}
data<-mtcars
gd(data$wt,data$mpg,32,-0.2,0.005,0.0001,10000)
reg<-lm(data$mpg~data$wt)
reg

