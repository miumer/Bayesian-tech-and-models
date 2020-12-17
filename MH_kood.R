#Loome funktsiooni, mis teeb g(...) funktsioonist logaritmilise funktsiooni. Funktsiooni lgaritm on tema avaldise logaritm. 
lg = function(mu, n, ybar) {
  mu2 = mu^2
  n * (ybar * mu - mu2 / 2.0) - log(1 + mu2)
}

## Random-Walk Metropolis-Hastings algorithm, mis võtab sisendina n = valimi suurus, ybar = ehk kogutud andmete keskmine, n_iter = iteratsioonide arv, mu_init = algne mu väärtus, cand_sd = kandidaate tootva q(...) jaotuse standardhälve 

mh = function(n, ybar, n_iter, mu_init, cand_sd) {
  
  ## step 1, initialize
  mu_out = numeric(n_iter) #väljundiks saame n_iter koguse mu-sid
  accpt = 0 #selleks, et näha kui palju mu proposale vastu võetakse loome selle tunnuse, mis on alguses väärtusega 0
  mu_now = mu_init #see on "praegune" (pärast updatei) mu väärtus (esialgu on see mu_init algne väärtus)
  lg_now = lg(mu=mu_now, n=n, ybar=ybar) #samuti update'ime pidevalt g() (siinkohal log of g()) väärtust vastavalt käesolevale mu väärtusele (ja valimi suurusele ning andmetele ybar)
  
  ## step 2, iterate
  for (i in 1:n_iter) {
    ## step 2a
    mu_cand = rnorm(n=1, mean=mu_now, sd=cand_sd) # draw a candidate
    
    ## step 2b. Kuna meie proposal distribution on normaaljaotus, mis on sümmeetriline, siis on q(...) funktsiooniga jaotuses kõigi tõmmete tõenäosus sama (vt ülevalt proposal distribution ja Random-Walk Metropolis-Hastings) ja see tähendab, et valemis taanduvad joone peal ja joone all q(...) tingimuslikud jaotused ära ja alpha väärtus ehk acceptance ratio on ainult kandidaadi ja praeguse oleku punktides g() funktsiooni hinnangute suhe. Siinkohal küll g() funktsiooni logaritmi arvutusliku stabiilsuse huvides
    lg_cand = lg(mu=mu_cand, n=n, ybar=ybar) # evaluate log of g with the candidate
    lalpha = lg_cand - lg_now # log of acceptance ratio
    alpha = exp(lalpha) #kuna oleme kasutanud logaritmilist skaalat, siis võtame alphast eksponendi.
    
    ## step 2c
    u = runif(1) # draw a uniform variable which will be less than alpha with probability min(1, alpha). Sest kui meil on uniform jaotus 0st-1ni siis on kõigi väärtuste tõmbamine ühe tõenäoline. Mis on tõenäosus saada väikesemat väärtust kui 0,5? 50%, väiksemat kui 0.4? 40%. Seega väiksemat kui alpha alpha*100%. Sellega viimegi läbi sammu, kus tingimuslikult võtame kandidaadi vastu tõenäosusega alpha, sest u-d, mis on väiksem kui alpha saada tõenäosus on alpha. Kui alpha on suurem kui 1, siis on u alati väiksem, mis tähendab, et võtame alati vastu
    if (u < alpha) { # then accept the candidate
      mu_now = mu_cand #praegune mu väärtus muudetakse kandidaadiks
      accpt = accpt + 1 # to keep track of acceptance
      lg_now = lg_cand #muudame ka praeguse lg ära.
    }
    
    ## collect results
    mu_out[i] = mu_now # save this iteration's value of mu. Kui väärtus jääb samaks, siis saab i-ndaks väärtuseks sama mu väärtus, kui muutub, siis uus väärtus.
  }
  
  ## return a list of output
  list(mu=mu_out, accpt=accpt/n_iter)
}

y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
ybar = mean(y)
n = length(y)
hist(y, freq=FALSE, xlim=c(-1.0, 3.0)) # histogram of the data
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu (x=x on argumendi puudumine pmtslt, annab generic jaotuse)
points(y, rep(0,n), pch=1) # individual data points
points(ybar, 0, pch=19) # sample mean

set.seed(43) # set the random seed for reproducibility
post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=3.0) #anname üleval loodud funktsioonile argumendid
str(post) #annab objekti (siinkohal list) sees olevad andmed

library("coda")
traceplot(as.mcmc(post$mu)) #peame post muutuja tegema mcmc objektiks

post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.05)
post$accpt

traceplot(as.mcmc(post$mu))

post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.9)
post$accpt

traceplot(as.mcmc(post$mu))

post = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=30.0, cand_sd=0.9)
post$accpt

traceplot(as.mcmc(post$mu))

post$mu_keep = post$mu[-c(1:100)] # discard the first 100 samples ja lisama oma post listi uue objekti mu_keep
plot(density(post$mu_keep, adjust=2.0), main="", xlim=c(-1.0, 3.0), xlab=expression(mu)) # plot density estimate of the posterior
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu
points(ybar, 0, pch=19) # sample mean

curve(0.017*exp(lg(mu=x, n=n, ybar=ybar)), from=-1.0, to=3.0, add=TRUE, col="blue") # approximation to the true posterior in blue

#----------------------------------------QUIZZZI ÜLESANNNE-------------------------------------
  
y2 = c (-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)

ybar2 = mean(y2)
n2 = length(y2)

set.seed(45) # set the random seed for reproducibility
post11 = mh(n=n2, ybar=ybar2, n_iter=1e3, mu_init=0.0, cand_sd=0.5) #anname üleval loodud funktsioonile argumendid

post12 = mh(n=n2, ybar=ybar2, n_iter=1e3, mu_init=0.0, cand_sd=1.5) #anname üleval loodud funktsioonile argumendid

post13 = mh(n=n2, ybar=ybar2, n_iter=1e3, mu_init=0.0, cand_sd=3.0) #anname üleval loodud funktsioonile argumendid

post14 = mh(n=n2, ybar=ybar2, n_iter=1e3, mu_init=0.0, cand_sd=4.0) #anname üleval loodud funktsioonile argumendid


library("coda")
traceplot(as.mcmc(post11$mu)) #peame post muutuja tegema mcmc objektiks
post11$accpt
traceplot(as.mcmc(post12$mu)) #peame post muutuja tegema mcmc objektiks
post12$accpt
traceplot(as.mcmc(post13$mu)) #peame post muutuja tegema mcmc objektiks
post13$accpt
traceplot(as.mcmc(post14$mu)) #peame post muutuja tegema mcmc objektiks
post14$accpt

pmc2 = mcmc.list(as.mcmc(post11$mu), as.mcmc(post12$mu), 
                as.mcmc(post13$mu), as.mcmc(post14$mu))

coda::traceplot(pmc2)

coda::effectiveSize(as.mcmc(post11$mu)) 
coda::effectiveSize(as.mcmc(post12$mu)) 
coda::effectiveSize(as.mcmc(post13$mu)) 
coda::effectiveSize(as.mcmc(post14$mu)) 

nburn = 1000 # remember to discard early iterations
post11$mu_keep = post11$mu[-c(1:nburn)]
summary(as.mcmc(post11$mu_keep)) #kuna oleme kindlad, et tegemist statsionaarse jaotusega, siis saame hinnata jaotust, nagu see oleks õige mu jaotus, mida otsimegi, ja öelda mis on erinevate mu väärtuste tõenäosus jne. 

