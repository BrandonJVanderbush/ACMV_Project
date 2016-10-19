k = 0

dk = 100

kmax = 2500000

y = 0

ck=c(k)
cy=c(y)
while(k<kmax){
  k = k + dk
  y = 0.2* k*(1-(k/2500000))
  ck=c(ck,k)
  cy=c(cy,y)
}
Population=data.frame(ck,cy)
plot(Population$ck, Population$cy, type="l",xlab="Population",ylab="Change in pop", ylim=c(0,200000),xlim=c(0,2500000),lwd=1,col="blue")
abline(h=100000)

