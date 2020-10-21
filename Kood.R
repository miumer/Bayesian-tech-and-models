



set.seed(32)


####n = 100

n = 100
a = 2.0
b = 1.0/3.0

theta = rgamma(n,a,b)

head(theta)

tail(theta)

hist(theta, freq=FALSE)
curve(dgamma(x,a,b), col = "blue", add = TRUE)

sum(theta)/n
mean(theta) #simuleeritud keskmine
a/b #see on antud gamma jaotuse analüütiline keskmine. Meie simuleeritud keskmine pole väga hea


###n = 10000

  n = 10000
  a = 2.0
  b = 1.0/3.0
  
  theta = rgamma(n,a,b)
  
  head(theta)
  
  tail(theta)
  
  hist(theta, freq=FALSE)
  curve(dgamma(x,a,b), col = "blue", add = TRUE)

mean(theta)


var(theta) #simuleeritud variatsioon
a/b^2 #analüütiline variatsioon - suht lähedal

ind <- theta < 5.0 #millised väärtused on simulatsioonis alla 5
head(ind)
mean(ind) # simulatsiooni hinnang, et väärtus on alla 5.0
pgamma(5,2,1/3) #analüütiline hinnang, et väärtus on alla 5.0       

quantile(theta,.95) #paneb theta vektori numbrid järjekorda ja võtab 95th quantile'i
qgamma(.95,2,1/3) #analüütiline meie gamma quantile

se <- sd(theta)/sqrt(n) #monte carlo hinnangu standardviga
se

2.0*se #usalduspiiride ulatus keskmisele. Ütleb, et oleme 95% kindlad, et tõeline theta väärtus pole
#kaugemal kui see väärtus keskmisest:

mean(theta) - (2.0*se)
mean(theta) + (2.0*se)

se <- sd(ind)/sqrt(n) 
se #usalduspiiride ulatus tõenäosusele, et väärtus on alla 5.0
#ehk proportsioonile väärtustele, mis on simulatsioonis alla 5.0. 

2*se #Peaks ütlema meile sisuliselt, et 95% tõenäosus, et kui teeme simulatsiooni, 
#siis alla 5.0 jäävate väärtuste proportsioon jääb meie mean(ind) väärtusest nii palju
#mõlemale poole (+ ja -)

####HIERHARHILINE MUDEL
##2 sammu:

m = 1e5 #Montecarlo valimi suurus
y <- numeric(m) #tühi numbriline vektor m liikmega
phi <- numeric(m) #---"---

#järgmises funktsioonis me sisuliselt tõmbame ükshaaval phi vektorisse ühe väärtuse 
#beta jaotusest ja paneme selle väärtuse binomiaalse jaotuse parameetriks, et tõmmata
#y vektorisse üks väärtus. Teeme seda 1e5 ehk 100k korda.
for (i in 1:m){
  phi[i] <- rbeta(1, 2, 2)
  y[i] <- rbinom(1,10,phi[i])
}

#Siin teeme sama asja, mis üleval ainult vektoritega, mis on vääääga palju kiirem
#sisuliselt teeme kohe 100k tõmmet phi vektorisse ja seejärel kasutame igat phi
#vektori liiget ükshaaval, et teha igale phi vektori väärtusele vastav tõmmet
#binomiaalsest jaotusest y vektorisse ja nii 100k korda.
phi <- rbeta(m,2,2)
y <- rbinom(m,10,phi)

#Nüüd vaatame y  marginaalse jaotuse omadusi. Sisuliselt lihtalt ignoreerime
#Phi-d ja vaatame y-d Aga pea meeles, et y marginaalne jaotus pole binomiaalne
#vaid beta-bionmiaalne, sest parameeter phi ei ole püsiv.

table(y) #Vaatame kui palju me erinevaid väärtuseid saime y vektorisse
plot(table(y)/m) #plotime tõenäosused saada mingit y väärtust
mean(y) # samuti saame leida marginaalne y beta-binomiaalse jaotuse keskmise



m <- 1e5
theta <- numeric(m)

theta <- rbeta(m,5,3)
theta <- theta/(1-theta)

0.625/(1-0.625)

head(theta)

mean(theta>1)
          
qnorm(0.3,0,1)

monorm <- rnorm(m,0,1)
quantile(monorm, 0.3)

sqrt(5.2/5000)


Q = matrix(c(0.0, 1, 
             0.3, 0.7), 
             nrow=2, byrow=TRUE)

n = 100000
x = numeric(n)
x[1] = 1 # fix the state as 1 for time 1
for (i in 2:n) {
  x[i] = sample.int(2, size=1, prob=Q[x[i-1],])}

table(x)/n

vec <- c(0, 1)

vec %*% Q %*% Q
