mr_covreg = function(bx, bw, by, S, klessp = TRUE, lambda = numeric(0), nlam = 100, K = 10, cv_mt = 2){
  p = length(bx)
  k = dim(bw)[2]
  b = S^(1/2) %*% bx
  Pb = b %*% t(b) / c(t(b) %*% b)
  xlas = (diag(p) - Pb) %*% S^(1/2) %*% bw
  ylas = (diag(p) - Pb) %*% S^(1/2) %*% by
  if (cv_mt == 1){
    if (length(lambda) == 0) {
      cv.alas = cv.glmnet(xlas, ylas, intercept = FALSE, nfolds = K)
    } else{
      cv.alas = cv.glmnet(xlas, ylas, intercept = FALSE, lambda = lambda, nfolds = K)
    }
  } else {
    if (length(lambda) == 0){
      cv.alas = cv.mr_covreg(bx, bw, by, S, nlambda = nlam, nfolds = K)
    } else {
      cv.alas = cv.mr_covreg(bx, bw, by, S, lambda = lambda, nlambda = nlam, nfolds = K)
    }
  }
  lam_pos = which(cv.alas$lambda == cv.alas$lambda.min)
  if (klessp == TRUE & cv.alas$glmnet.fit$df[lam_pos] > (p - 2)){
    lam_pos = length(cv.alas$glmnet.fit$df) - min(which(rev(cv.alas$glmnet.fit$df) <= (p - 2))) + 1
  }
  th1 = as.vector(cv.alas$glmnet.fit$beta[,lam_pos])
  lam_1se_pos = which(cv.alas$lambda == cv.alas$lambda.1se)
  if (klessp == TRUE & cv.alas$glmnet.fit$df[lam_1se_pos] > (p - 2)){
    lam_1se_pos = length(cv.alas$glmnet.fit$df) - min(which(rev(cv.alas$glmnet.fit$df) <= (p - 2))) + 1
  }
  th1_1se = as.vector(cv.alas$glmnet.fit$beta[,lam_1se_pos])
  e = by - bw %*% th1
  thest = t(bx) %*% S %*% e / (t(bx) %*% S %*% bx)
  e1 = by - bw %*% th1_1se
  thest_1se = t(bx) %*% S %*% e1 / (t(bx) %*% S %*% bx)
  return(list(thest = thest, thest_1se = thest_1se, a = th1, a_1se = th1_1se, lambda = cv.alas$lambda[lam_pos], lambda_1se = cv.alas$lambda[lam_1se_pos], lamseq = cv.alas$lambda))
}

mr_covreg_lam = function(bx, bw, by, S, lambda){
  p = length(bx)
  k = dim(bw)[2]
  b = S^(1/2) %*% bx
  Pb = b %*% t(b) / c(t(b) %*% b)
  xlas = (diag(p) - Pb) %*% S^(1/2) %*% bw
  ylas = (diag(p) - Pb) %*% S^(1/2) %*% by
  alas = glmnet(xlas, ylas, intercept = FALSE, lambda = lambda)
  th1 = as.vector(alas$beta)
  e = by - bw %*% th1
  thest = t(bx) %*% S %*% e / (t(bx) %*% S %*% bx)
  return(list(thest = thest, a = th1))
}

cv.mr_covreg = function(bx, bw, by, S, lambda = numeric(0), nlambda = 100, nfolds = 10){
  p = length(bx)
  b = S^(1/2) %*% bx
  Pb = b %*% t(b) / c(t(b) %*% b)
  xlas = (diag(p) - Pb) %*% S^(1/2) %*% bw
  ylas = (diag(p) - Pb) %*% S^(1/2) %*% by
  if (length(lambda) == 0){
    alas = glmnet(xlas, ylas, intercept = FALSE, nlambda = nlambda)
  } else {
    alas = glmnet(xlas, ylas, intercept = FALSE, lambda = lambda, nlambda = nlambda)
  }
  lamseq = alas$lambda
  holdout = split(sample(1:p), rep(1:nfolds, length = p))
  h = sapply(1:nfolds, function(y){
    test = holdout[[y]]
    ptrain = length(bx[-test])
    bcv = S[-test, -test]^(1/2) %*% bx[-test]
    Pbcv = bcv %*% t(bcv) / c(t(bcv) %*% bcv)
    xlascv = (diag(ptrain) - Pbcv) %*% S[-test, -test]^(1/2) %*% bw[-test,]
    ylascv = (diag(ptrain) - Pbcv) %*% S[-test, -test]^(1/2) %*% by[-test]
    alascv = glmnet(xlascv, ylascv, lambda = lamseq, intercept = FALSE, thresh = 1E-6)
    bym = by %*% t(rep(1, length(alascv$lambda)))
    e = bym[-test,] - bw[-test,] %*% alascv$beta
    thest = t(bx[-test]) %*% S[-test, -test] %*% e / c(t(bx[-test]) %*% S[-test, -test] %*% bx[-test])
    hy = as.vector(diag(t(as.matrix(bx[test] %*% thest + bw[test,] %*% alascv$beta - bym[test,])) %*% S[test, test] %*% as.matrix(bx[test] %*% thest + bw[test,] %*% alascv$beta - bym[test,]) / length(bx[test])))
    if (length(hy)==length(lamseq)) {
      hy
    } else {
      c(hy, rep(0, (length(lamseq)-length(hy))))
    }
  })
  f = rbind(rowMeans(h), apply(h, 1, sd) / sqrt(nfolds))
  s = which.min(f[1,])
  s1 = min(which(f[1,] <= f[1,s] + f[2,s]))
  fit = list(beta = alas$beta, df = alas$df)
  return(list(glmnet.fit = fit, lambda = lamseq, lambda.min = lamseq[s], lambda.1se = lamseq[s1]))
}
