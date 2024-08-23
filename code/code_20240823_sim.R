## Understanding the Effect of Market Risks on New Pension System and Government Responsibility

## Author: Sourish Das , Bikramaditya Datta , Shiv Ratan Tiwari  


### 1000 simulations for corpus
n <- 30                 # Service period
m <- 20                 # Retirement period
b <- 100                # Initial Basic Pay
inc <- 0.03             # Annual increment of Basic Pay
c <- 0.24               # Contribution towards NPS (includes employee and employer contribution)
annuity_rate <- 0.05    # Annuity rate at retirement 
discout_rate <- 0.07    # Discount rate used for responsible financing calculation
mean_infl <- 4          # mean of normal distribution from which inflation values are drawn
sd_infl <- 1.           # sd of normal distribution from which inflation values are drawn

## simulate NPS performance from GBM
sim_prices<-function(Time_len,S1,mu,vol){
  
  S = rep(NA,Time_len)
  S[1] = S1
  
  drift = mu - (vol^2/2)
  for(t in 2:Time_len){
    z <- rnorm(1,mean=0,sd=1)
    rt_mkt = drift+vol*z
    S[t]=S[t-1]*exp(rt_mkt)
  }
  return(S)
}



NPS_mu <- 9
NPS_sigma <- 5

#S <-sim_prices(31,100,NPS_mu/100,NPS_sigma/100)
#rt<-diff(log(S))
#plot(ts(rt),ylim=c(-1,1))
#sd(rt)

sim_size <- 1000
final_corpus <- pdv_Gov_surt <-no_of_yrs_wo_suprt <- no_of_yrs_inad_pension <-rep(NA,sim_size)
salary_20 <- rep(NA,sim_size)


plot(NULL,xlim=c(0,n),ylim=c(0,10000),xlab="Service Period"
     ,ylab="Corpus (Rs)")
set.seed(20240816)
for(s in 1:sim_size){
  infl <- rnorm(n+m,mean=mean_infl,sd=sd_infl)
  basic_pay <- da <- salary <- rep(NA,n)
  basic_pay[1] <- b
  for (i in 2:n) {
    basic_pay[i] <- basic_pay[i-1]*(1+inc)
  }
  
  da[1] <- 0
  for (i in 2:n) {
    da[i] <- basic_pay[i-1]*infl[i-1]/100
  }
  
  salary <- basic_pay + da
  salary_20[s] <- salary[20]
  
  emplye_contrib <- salary*0.1
  emplyr_contrib <- salary*0.14
  
  total_contrib <- emplye_contrib + emplyr_contrib
  dispo_incm <- salary - emplye_contrib
  
  ### NPS performance
  ### 
  
  S <- sim_prices(Time_len=n,S1=100,mu = NPS_mu/100, vol=NPS_sigma/100)
  return = diff(log(S))
  return = c(0, return)
  
  corpus <- rep(NA,n)
  corpus[1] <-total_contrib[1]
  for( t in 2:n){
    corpus[t] <- (corpus[t-1]*(1+return[t]))+total_contrib[t]
  }
  
  final_corpus[s] <- corpus[n]
  lines(corpus,col="pink",lwd=2)
  pension <- corpus[n]*annuity_rate
  inlation_adjusted_salry <- rep(NA,m)
  
  inlation_adjusted_salry[1] <- salary[n]*0.5*(1+infl[n]/100)
  for(t in 2:m){
    inlation_adjusted_salry[t] <- inlation_adjusted_salry[t-1]*(1+infl[n+t-1]/100)
  }
  diff <- inlation_adjusted_salry - pension
  Gov_support<-diff
  Gov_support[diff<0] <- 0
  pdv_Gov_surt[s] <- sum(Gov_support*exp(-discout_rate*(n+1):(n+m)))
  
  
  no_of_yrs_wo_suprt[s] <- sum(Gov_support==0)
  no_of_yrs_inad_pension[s] <- m - no_of_yrs_wo_suprt[s]
  
}
png(filename = "fig6_final_corpus_mu11_sigma5.png")
hist(final_corpus,xlab="Final Corpus (Rs)",main=""
     ,probability = F,col="pink",xlim=c(1000,12000))
abline(v=mean(final_corpus),lwd=3,col="3")
text(x = (mean(final_corpus)+200)
     , y = 130, srt = 90, paste('mean of the corpus Rs.'
                               ,round(mean(final_corpus),2))
     , col = 'black', cex = 0.8)
dev.off()

png(filename = "fig11_pdv_Gov_sut_annuity7.png")
hist(pdv_Gov_surt, xlab="Present Discounted Value of Government Support"
     , main="", col="green")
dev.off()
mean(pdv_Gov_surt)
(mean(pdv_Gov_surt)*exp(discout_rate*20)/mean(salary_20))*100

barplot(table(no_of_yrs_wo_suprt)
        ,xlab="Number of years when pension was more than 50% of last adjusted salary"
        ,col="pink")

png(filename = "fig9_numbr_of_yrs_inad_annuity5.png")
barplot(table(no_of_yrs_inad_pension)
        ,xlab="Number of years when pension was less than 50% of last adjusted salary"
        ,col="red")
dev.off()
one_sim = cbind.data.frame(Year=1:n,Inflation=infl[1:n],Basic_Pay=basic_pay
                 ,DA=da,Salary=salary,Contribtion = total_contrib
                 ,Return =return, Corpus = corpus)

one_sim = round(one_sim,2)
min(final_corpus)
max(final_corpus)
mean(final_corpus)
var(final_corpus)

one_sim_retirement = cbind.data.frame(Year=(n+1):(n+m)
                                      ,Inflation=infl[(n+1):(n+m)]
                                      ,Adjusted_Salary=inlation_adjusted_salry
                                      ,Pension=pension)
