#Momentum based Gradient Descent
mgd=function(x1,x2,y,m1,m2,c,alpha,gamma,iter){
  iterations=0
  #Lf=0
  nu_m1=0
  nu_m2=0
  nu_c=0
  while(iterations<=iter){
    y_pred=m1*x1+m2*x2+c
    Lf_new=0.5*sum((y_pred-y)^2)
    nu_m1=gamma*nu_m1+alpha*sum((y_pred-y)*x1)
    nu_m2=gamma*nu_m2+alpha*sum((y_pred-y)*x2)
    nu_c=gamma*nu_c+alpha*sum(y_pred-y)
    m1=m1-nu_m1
    m2=m2-nu_m2
    
    c=c-nu_c
    Lf=Lf_new
    iterations=iterations+1
  }
  paste("Optimal intercept:",c,"Optimal slope:",m1,m2,"Loss function",Lf)
}
data=mtcars
mgd(data$wt,data$hp,data$mpg,-0.2,-0.2,32,0.000002,0.98,50000)
model=lm(data$mpg~data$hp+data$wt)
summary(model)
