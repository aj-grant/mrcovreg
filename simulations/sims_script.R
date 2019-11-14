install.packages("MendelianRandomization")
library(MendelianRandomization)
install.packages("glmnet")
library(glmnet)
install.packages("parallel")
library(parallel)
install.packages("MASS")
library(MASS)

cl = makeCluster(4)
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(MendelianRandomization))
clusterEvalQ(cl, library(glmnet))
clusterExport(cl, c('data_sim', 'data_summarise', 'mr_covreg', 'cv.mr_covreg', 'thest', 'post_reg_est', 'dbl_lasso'))

M = 1000
n = 20000
m = 0.3
bux = 1
buy = 1

#theta = 0.2
th0 = 0.2

#S1 simulations with p = 10, k = 8
p = 10
k = 8
buw = rep(1/k, k)

z = 4
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190501)
Dsum1_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 6
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190502)
Dsum1_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 7
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190503)
Dsum1_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum1_50', 'Dsum1_30', 'Dsum1_10'))
clusterSetRNGStream(cl, 20190901)
pinf1_50 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum1_50[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf1_30 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum1_30[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf1_10 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum1_10[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

#S2 simulations with p = 10, k = 12
p = 10
k = 12
buw = rep(1/k, k)

z = 8
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190504)
Dsum2_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 10
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190505)
Dsum2_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 11
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190506)
Dsum2_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum2_50', 'Dsum2_30', 'Dsum2_10'))
clusterSetRNGStream(cl, 20190902)
pinf2_50 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum2_50[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf2_30 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum2_30[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf2_10 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum2_10[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

#S3 simulations with p = 80, k = 70
p = 80
k = 70
buw = rep(1/k, k)

z = 35
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190507)
Dsum3_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 49
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190508)
Dsum3_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 63
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190509)
Dsum3_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum3_50', 'Dsum3_30', 'Dsum3_10'))
clusterSetRNGStream(cl, 20190903)
pinf3_50 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum3_50[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf3_30 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum3_30[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf3_10 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum3_10[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

#S4 simulations with p = 80, k = 90
p = 80
k = 90
buw = rep(1/k, k)

z = 55
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190510)
Dsum4_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 69
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190511)
Dsum4_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 83
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190512)
Dsum4_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum4_50', 'Dsum4_30', 'Dsum4_10'))
clusterSetRNGStream(cl, 20190904)
pinf4_50 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum4_50[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf4_30 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum4_30[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

pinf4_10 = parSapply(cl, 1:M, function(j){
  Dsum = Dsum4_10[[j]]
  thest(Dsum$bxhat1, Dsum$bwhat1, Dsum$byhat1, Dsum$seby1, Dsum$bxhat2, Dsum$bwhat2, Dsum$byhat2, Dsum$seby2, Dsum$bxhat3, Dsum$bwhat3, Dsum$byhat3, Dsum$seby3, Dsum$v)
})

##################################################################################
#Sims with theta = 0
th0 = 0

#S1 simulations with p = 10, k = 8
p = 10
k = 8
buw = rep(1/k, k)

z = 4
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190601)
Dsum_null1_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 6
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190602)
Dsum_null1_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 7
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190603)
Dsum_null1_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null1_50', 'Dsum_null1_30', 'Dsum_null1_10'))
clusterSetRNGStream(cl, 20191001)
pinf_null1_50 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null1_50[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null1_30 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null1_30[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null1_10 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null1_10[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

#S2 simulations with p = 10, k = 12
p = 10
k = 12
buw = rep(1/k, k)

z = 8
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190604)
Dsum_null2_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 10
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190605)
Dsum_null2_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 11
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190606)
Dsum_null2_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null2_50', 'Dsum_null2_30', 'Dsum_null2_10'))
clusterSetRNGStream(cl, 20191002)
pinf_null2_50 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null2_50[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null2_30 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null2_30[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null2_10 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null2_10[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

#S3 simulations with p = 80, k = 70
p = 80
k = 70
buw = rep(1/k, k)

z = 35
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190607)
Dsum_null3_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 49
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190608)
Dsum_null3_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 63
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190609)
Dsum_null3_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null3_50', 'Dsum_null3_30', 'Dsum_null3_10'))
clusterSetRNGStream(cl, 20191003)
pinf_null3_50 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null3_50[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null3_30 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null3_30[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null3_10 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null3_10[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

#S4 simulations with p = 80, k = 90
p = 80
k = 90
buw = rep(1/k, k)

z = 55
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190610)
Dsum_null4_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 69
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190611)
Dsum_null4_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 83
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190612)
Dsum_null4_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  g[v] = 0
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null4_50', 'Dsum_null4_30', 'Dsum_null4_10'))
clusterSetRNGStream(cl, 20191004)
pinf_null4_50 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null4_50[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null4_30 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null4_30[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

pinf_null4_10 = parSapply(cl, 1:M, function(j){
  Dsum_null = Dsum_null4_10[[j]]
  thest(Dsum_null$bxhat1, Dsum_null$bwhat1, Dsum_null$byhat1, Dsum_null$seby1, Dsum_null$bxhat2, Dsum_null$bwhat2, Dsum_null$byhat2, Dsum_null$seby2, Dsum_null$bxhat3, Dsum_null$bwhat3, Dsum_null$byhat3, Dsum_null$seby3, Dsum_null$v)
})

##################################################################################
#Sims with sparsity in bw0 and with theta = 0.2
th0 = 0.2

#S1 simulations with p = 10, k = 8
p = 10
k = 8
buw = rep(1/k, k)

z = 4
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190501)
Dsum_bw01_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 6
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190502)
Dsum_bw01_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0 
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 7
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190503)
Dsum_bw01_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_bw01_50', 'Dsum_bw01_30', 'Dsum_bw01_10'))
clusterSetRNGStream(cl, 20190901)
pinf_bw01_50 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw01_50[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw01_30 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw01_30[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw01_10 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw01_10[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

#S2 simulations with p = 10, k = 12
p = 10
k = 12
buw = rep(1/k, k)

z = 8
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190504)
Dsum_bw02_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 10
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190505)
Dsum_bw02_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 11
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190506)
Dsum_bw02_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_bw02_50', 'Dsum_bw02_30', 'Dsum_bw02_10'))
clusterSetRNGStream(cl, 20190902)
pinf_bw02_50 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw02_50[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw02_30 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw02_30[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw02_10 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw02_10[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

#S3 simulations with p = 80, k = 70
p = 80
k = 70
buw = rep(1/k, k)

z = 35
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190507)
Dsum_bw03_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 49
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190508)
Dsum_bw03_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 63
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190509)
Dsum_bw03_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_bw03_50', 'Dsum_bw03_30', 'Dsum_bw03_10'))
clusterSetRNGStream(cl, 20190903)
pinf_bw03_50 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw03_50[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw03_30 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw03_30[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw03_10 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw03_10[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

#S4 simulations with p = 80, k = 90
p = 80
k = 90
buw = rep(1/k, k)

z = 55
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190510)
Dsum_bw04_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 69
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190511)
Dsum_bw04_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 83
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190512)
Dsum_bw04_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_bw04_50', 'Dsum_bw04_30', 'Dsum_bw04_10'))
clusterSetRNGStream(cl, 20190904)
pinf_bw04_50 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw04_50[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw04_30 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw04_30[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

pinf_bw04_10 = parSapply(cl, 1:M, function(j){
  Dsum_bw0 = Dsum_bw04_10[[j]]
  thest(Dsum_bw0$bxhat1, Dsum_bw0$bwhat1, Dsum_bw0$byhat1, Dsum_bw0$seby1, Dsum_bw0$bxhat2, Dsum_bw0$bwhat2, Dsum_bw0$byhat2, Dsum_bw0$seby2, Dsum_bw0$bxhat3, Dsum_bw0$bwhat3, Dsum_bw0$byhat3, Dsum_bw0$seby3, Dsum_bw0$v)
})

##################################################################################
#Sims with sparsity in bw0 and with theta = 0
th0 = 0

#S1 simulations with p = 10, k = 8
p = 10
k = 8
buw = rep(1/k, k)

z = 4
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190501)
Dsum_null_bw01_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 6
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190502)
Dsum_null_bw01_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 7
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190503)
Dsum_null_bw01_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null_bw01_50', 'Dsum_null_bw01_30', 'Dsum_null_bw01_10'))
clusterSetRNGStream(cl, 20190901)
pinf_null_bw01_50 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw01_50[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw01_30 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw01_30[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw01_10 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw01_10[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

#S2 simulations with p = 10, k = 12
p = 10
k = 12
buw = rep(1/k, k)

z = 8
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190504)
Dsum_null_bw02_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 10
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190505)
Dsum_null_bw02_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 11
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190506)
Dsum_null_bw02_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.15,0.3)
  bw = sapply(1:k, function(x){runif(p,-0.2,0.4)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null_bw02_50', 'Dsum_null_bw02_30', 'Dsum_null_bw02_10'))
clusterSetRNGStream(cl, 20190902)
pinf_null_bw02_50 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw02_50[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw02_30 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw02_30[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw02_10 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw02_10[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

#S3 simulations with p = 80, k = 70
p = 80
k = 70
buw = rep(1/k, k)

z = 35
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190507)
Dsum_null_bw03_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 49
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190508)
Dsum_null_bw03_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 63
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190509)
Dsum_null_bw03_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null_bw03_50', 'Dsum_null_bw03_30', 'Dsum_null_bw03_10'))
clusterSetRNGStream(cl, 20190903)
pinf_null_bw03_50 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw03_50[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw03_30 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw03_30[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw03_10 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw03_10[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

#S4 simulations with p = 80, k = 90
p = 80
k = 90
buw = rep(1/k, k)

z = 55
clusterExport(cl, c('n', 'z', 'p', 'k', 'm', 'bux', 'buy', 'buw', 'th0'))
clusterSetRNGStream(cl, 20190510)
Dsum_null_bw04_50 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 69
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190511)
Dsum_null_bw04_30 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

z = 83
clusterExport(cl, c('z'))
clusterSetRNGStream(cl, 20190512)
Dsum_null_bw04_10 = parLapply(cl, 1:M, function(y){
  g = runif(k,-0.2,0.3)
  v = sample(k, z)
  bx = runif(p,0.05,0.12)
  bw = sapply(1:k, function(x){runif(p,-0.1,0.15)})
  bw[,v] = 0
  D1 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m), r2 = TRUE)
  D2 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  D3 = data_summarise(data_sim(n, p, k, th0, g, bx, bw, bux, buw, buy, m))
  return(list("bxhat1" = D1$bxhat, "bwhat1" = D1$bwhat, "byhat1" = D1$byhat, "seby1" = D1$seby, "bxhat2" = D2$bxhat, "bwhat2" = D2$bwhat, "byhat2" = D2$byhat, "seby2" = D2$seby, "bxhat3" = D3$bxhat, "bwhat3" = D3$bwhat, "byhat3" = D3$byhat, "seby3" = D3$seby, "xgr2" = D1$xgr2, "g" = g, "v" = v))
})

clusterExport(cl, c('Dsum_null_bw04_50', 'Dsum_null_bw04_30', 'Dsum_null_bw04_10'))
clusterSetRNGStream(cl, 20190904)
pinf_null_bw04_50 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw04_50[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw04_30 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw04_30[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

pinf_null_bw04_10 = parSapply(cl, 1:M, function(j){
  Dsum_null_bw0 = Dsum_null_bw04_10[[j]]
  thest(Dsum_null_bw0$bxhat1, Dsum_null_bw0$bwhat1, Dsum_null_bw0$byhat1, Dsum_null_bw0$seby1, Dsum_null_bw0$bxhat2, Dsum_null_bw0$bwhat2, Dsum_null_bw0$byhat2, Dsum_null_bw0$seby2, Dsum_null_bw0$bxhat3, Dsum_null_bw0$bwhat3, Dsum_null_bw0$byhat3, Dsum_null_bw0$seby3, Dsum_null_bw0$v)
})

stopCluster(cl)
