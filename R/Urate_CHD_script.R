install.packages("devtools")
library(devtools)
install_github("phenoscanner/phenoscanner")
library(phenoscanner)
install.packages("tidyverse")
library(tidyverse)
install.packages("MendelianRandomization")
library(MendelianRandomization)
install.packages("glmnet")
library(glmnet)
install.packages("RColorBrewer")
library(RColorBrewer)

#Load genetic variant-risk factor associations
x.data = read.csv('bx_associations.csv', header = TRUE)

#Load GWAS summary data from phenoscanner
pheno_res = phenoscanner(snpquery = x.data$snp, pvalue = 1)
all.data = pheno_res$results
all.data$snp = (as.factor(all.data$snp))
all.data$beta = suppressWarnings(as.double(all.data$beta))
all.data$se = suppressWarnings(as.double(all.data$se))

#Read outcome and covariate data from GWAS summary data and align alleles with genetic variant-risk factor associations
CHD.data = (all.data %>% filter(study == "CARDIoGRAMplusC4D" & trait == "Coronary artery disease" & dataset == "CARDIoGRAMplusC4D_CHD_Mixed_2015"))[, c("snp","beta","se","a1")]
CHD.data[,"beta"] = CHD.data[,"beta"] * case_when(as.vector(CHD.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(CHD.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
glu.data = (all.data %>% filter(study == "MAGIC" & trait == "Fasting glucose" & dataset == "MAGIC_FG_EUR_2010"))[, c("snp","beta","se","a1")]
glu.data[,"beta"] = glu.data[,"beta"] * case_when(as.vector(glu.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(glu.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
BMI.data = (all.data %>% filter(study == "GIANT" & trait == "Body mass index" & dataset == "GIANT_BMI_EUR_2015"))[, c("snp","beta","se","a1")]
BMI.data[,"beta"] = BMI.data[,"beta"] * case_when(as.vector(BMI.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(BMI.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
T2D.data = (all.data %>% filter(study == "DIAGRAM" & trait == "Type II diabetes" & dataset == "DIAGRAM_T2D_EUR_2017"))[, c("snp","beta","se","a1")]
T2D.data[,"beta"] = T2D.data[,"beta"] * case_when(as.vector(T2D.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(T2D.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
HDL.data = (all.data %>% filter(study == "GLGC" & trait == "High density lipoprotein" & dataset == "GLGC_HDL_EUR_2013"))[, c("snp","beta","se","a1")]
HDL.data[,"beta"] = HDL.data[,"beta"] * case_when(as.vector(HDL.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(HDL.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
LDL.data = (all.data %>% filter(study == "GLGC" & trait == "Low density lipoprotein" & dataset == "GLGC_LDL_EUR_2013"))[, c("snp","beta","se","a1")]
LDL.data[,"beta"] = LDL.data[,"beta"] * case_when(as.vector(LDL.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(LDL.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
Tri.data = (all.data %>% filter(study == "GLGC" & trait == "Triglycerides" & dataset == "GLGC_TG_EUR_2013"))[, c("snp","beta","se","a1")]
Tri.data[,"beta"] = Tri.data[,"beta"] * case_when(as.vector(Tri.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(Tri.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
SBP.data = (all.data %>% filter(study == "Neale B" & trait == "Systolic blood pressure" & dataset == "Neale-B_UKBB_EUR_2017"))[, c("snp","beta","se","a1")]
SBP.data[,"beta"] = SBP.data[,"beta"] * case_when(as.vector(SBP.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(SBP.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)
DBP.data = (all.data %>% filter(study == "Neale B" & trait == "Diastolic blood pressure" & dataset == "Neale-B_UKBB_EUR_2017"))[, c("snp","beta","se","a1")]
DBP.data[,"beta"] = DBP.data[,"beta"] * case_when(as.vector(DBP.data[,"a1"])==as.vector(x.data[,"effect_allele"]) ~ 1, as.vector(DBP.data[,"a1"])!=as.vector(x.data[,"effect_allele"]) ~ -1)

#Create genetic variant association matrices
bw = reduce(list(glu.data[, c("snp", "beta")], BMI.data[, c("snp", "beta")], T2D.data[, c("snp", "beta")], HDL.data[, c("snp", "beta")], LDL.data[, c("snp", "beta")], Tri.data[, c("snp", "beta")], SBP.data[, c("snp", "beta")], DBP.data[, c("snp", "beta")]), inner_join, by = "snp")
colnames(bw) = c("snp", "beta.w1", "beta.w2", "beta.w3", "beta.w4", "beta.w5", "beta.w6", "beta.w7", "beta.w8")
sebw = reduce(list(glu.data[, c("snp", "se")], BMI.data[, c("snp", "se")], T2D.data[, c("snp", "se")], HDL.data[, c("snp", "se")], LDL.data[, c("snp", "se")], Tri.data[, c("snp", "se")], SBP.data[, c("snp", "se")], DBP.data[, c("snp", "se")]), inner_join, by = "snp")
colnames(sebw) = c("snp", "se.w1", "se.w2", "se.w3", "se.w4", "se.w5", "se.w6", "se.w7", "se.w8")
bx = x.data[, c("snp", "beta")]
sebx = x.data[, c("snp", "se")]
by = CHD.data[, c("snp", "beta")]
seby = CHD.data[, c("snp", "se")]
allbeta = reduce(list(bx, bw, by), inner_join, by = "snp")
allse = reduce(list(sebx, sebw, seby), inner_join, by = "snp")

#Perform cross-validation 100 times and take the average lambda
set.seed(20190625)
z = vector(length = 100)
for (j in 1:100){
  z[j] = mr_covreg(allbeta[,2], as.matrix(allbeta[,3:10]), allbeta[,11], diag(allse[,11]^-2), klessp = TRUE, nlam = 100, K = 10, cv_mt = 2)$lambda
}
lambda = mean(z)

#Run the regularization procedure for the chosen lambda
t = mr_covreg_lam(allbeta[,2], as.matrix(allbeta[,3:10]), allbeta[,11], diag(allse[,11]^-2), lambda)
lamseq = mr_covreg(allbeta[,2], as.matrix(allbeta[,3:10]), allbeta[,11], diag(allse[,11]^-2), klessp = TRUE, nlam = 100, K = 10, cv_mt = 2)$lamseq

#Plot coefficient values for varying lambda
f = matrix(nrow = length(lamseq), ncol = 9)
for (j in 1:length(lamseq)){
  mod = mr_covreg_lam(allbeta[,2], as.matrix(allbeta[,3:10]), allbeta[,11], diag(allse[,11]^-2), lamseq[j])
  f[j,] = c(mod$thest, mod$a)
}
p = dim(bx)[1]
F.df = as.data.frame(cbind(log(p*lamseq), f))
names(F.df)=c("log_lam", "Urate", "Glu", "BMI", "T2D", "HDL", "LDL", "Tri", "SBP", "DBP")
F.df = gather(F.df, key = "Covariate", value = "Beta", -log_lam) %>% mutate(Covariate = fct_relevel(Covariate, c("Urate", "Glu", "BMI", "T2D", "HDL", "LDL", "Tri", "SBP", "DBP")))
F.plot = ggplot(F.df, aes(x = log_lam, y = Beta, group = Covariate)) +
  geom_line(aes(color = Covariate, linetype = Covariate)) +
  geom_vline(xintercept = log(p*lambda), linetype = "dashed") +
  geom_hline(yintercept = 0, col = "gray") +
  scale_color_manual(values = c("black", rev(brewer.pal(8, "Set2")))) +
  xlab(expression(paste("log(", lambda, ")"))) +
  ylab(expression(hat(delta[j]))) +
  theme_bw() +
  theme(title = element_text(size = 8), axis.title = element_text(size = 9), axis.text = element_text(size = 8), legend.title = element_text(size = 9), legend.text = element_text(size = 8), plot.margin = margin(0, 0.55, 0, 0, "cm"))
F.plot

#IVW estimate
mrob = mr_input(bx = allbeta$beta.x, bxse = allse$se.x, by = allbeta$beta.y, byse = allse$se.y)
mod_ivw = mr_ivw(mrob)
th_ivw = mod_ivw$Estimate
se_ivw = mod_ivw$StdError
bal_ivw = cor(allbeta$beta.y, as.matrix(allbeta[,2:10]))

#MV all
mrmvob_all = mr_mvinput(bx = as.matrix(allbeta[,2:10]), bxse = as.matrix(allse[,2:10]), by = allbeta$beta.y, byse = allse$se.y)
mod_mv_all = mr_mvivw(mrmvob_all)
th_mv_all = mod_mv_all$Estimate
se_mv_all = mod_mv_all$StdError
e = allbeta$beta.y - as.matrix(allbeta[,3:10]) %*% th_mv_all[2:9]
bal_mv_all = cor(e, as.matrix(allbeta[,2:10]))

#MV using covariates identified by White
mrmvob_white = mr_mvinput(bx = as.matrix(allbeta[,c(2, 6, 8, 9, 10)]), bxse = as.matrix(allse[,c(2, 6, 8, 9, 10)]), by = allbeta$beta.y, byse = allse$se.y)
mod_white = mr_mvivw(mrmvob_white)
th_white = mod_white$Estimate
se_white = mod_white$StdError
e = allbeta$beta.y - as.matrix(allbeta[,c(6, 8, 9, 10)]) %*% th_white[2:5]
bal_mv_white = cor(e, allbeta[,2:10])

#MV using DBP as the only covariate
mrmvob_reg1 = mr_mvinput(bx = as.matrix(allbeta[,c(2,10)]), bxse = as.matrix(allse[,c(2,10)]), by = allbeta$beta.y, byse = allse$se.y)
mod_reg1 = mr_mvivw(mrmvob_reg1)
th_mvreg1 = mod_reg1$Estimate
se_mvreg1 = mod_reg1$StdError
e = allbeta$beta.y - as.matrix(allbeta[,10]) %*% as.vector(th_mvreg1[2])
bal_mvreg1 = cor(e, allbeta[,2:10])

#MV using DBP and BMI as covariates
mrmvob_reg2 = mr_mvinput(bx = as.matrix(allbeta[,c(2,4,10)]), bxse = as.matrix(allse[,c(2,4,10)]), by = allbeta$beta.y, byse = allse$se.y)
mod_reg2 = mr_mvivw(mrmvob_reg2)
th_mvreg2 = mod_reg2$Estimate
se_mvreg2 = mod_reg2$StdError
e = allbeta$beta.y - as.matrix(allbeta[,c(4,10)]) %*% th_mvreg2[2:3]
bal_mvreg2 = cor(e, allbeta[,2:10])

#Plot covariate balance
B.df = cbind(c("Urate", "Glu", "BMI", "T2D", "HDL", "LDL", "Tri", "SBP", "DBP"),as.data.frame(cbind(as.vector(bal_ivw), as.vector(bal_mv_all), as.vector(bal_mv_white), as.vector(bal_mvreg1), as.vector(bal_mvreg2))))
names(B.df) = c("X", "None", "All", "White", "DBP", "DBP & BMI")
B.df = gather(B.df, key = "Covariates", value = "Balance", -X) %>% mutate(Covariates = fct_relevel(Covariates, c("None", "All", "White", "DBP", "DBP & BMI"))) %>% mutate(X = fct_relevel(X, c("DBP", "SBP", "Tri", "LDL", "HDL", "T2D", "BMI", "Glu", "Urate")))
Bjitter.plot = ggplot(data = B.df, aes(x = Balance, y = X, Group = Covariates)) +
  geom_point(aes(color = Covariates, shape = Covariates), size = 2, position = position_jitter(height = 0.15, seed = 20190627)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab(expression(paste("cor(", hat(epsilon)[j], ", ", "[", hat(beta)[X], " ", hat(beta)[W], "])"))) +
  theme_bw() +
  theme(title = element_text(size = 8), axis.title.y = element_blank(), axis.title.x = element_text(size = 8), axis.text = element_text(size = 8), legend.title = element_text(size = 9), legend.text = element_text(size = 8), plot.margin = margin(0, 1.5, 0, 0.5, "cm")) +
  scale_color_manual(values = rev(brewer.pal(5, "Set2")))
Bjitter.plot
