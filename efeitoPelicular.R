likelihood=function(n,y,theta){
  return(theta^y*(1-theta)^(n-y))
}

theta=seq(from=0.01,to=0.99,by=0.01)

plot(theta,likelihood(400,72,theta))

abline(v=.18)

loglike=function(n,y,theta){
  return(y*log(theta)+(n-y)*log(1-theta))
}

plot(theta,loglike(400,72,theta),type = "l")

#probabilidade normal com media 0 e desvio padrao 1

tempo = seq(from=-3,to=3,by=0.1)
distNormalPDF = dnorm(tempo,0,1)
distNormalCDF = pnorm(tempo,0,1)
distNormalQuatile = qnorm(tempo,0,1)
distNormalQuatile = rnorm(tempo,0,1)
plot(distNormal,type = "l")

?dnorm

qnorm(p=0.975,mean=0,sd=1)

#propagação de onda harmonica variando em (z,t)

propagacaoOnda=function(I,cAlpha,cBeta,f,z){
  corrente = I*exp(-cAlpha*z)*cos(2*pi*f-cBeta*z)
  return(corrente)
}

plot(propagacaoOnda(10,0.1,1,60,seq(from=0,to=50,by=0.1)),
     main="Propagação de um 'bom' condutor", type="l"
     xlab="comprimentoda LT" ylab="corrente elétrica")