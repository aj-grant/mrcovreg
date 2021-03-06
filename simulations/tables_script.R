q = qnorm(0.975, 0, 1)
th0 = 0.2
p = 10
k = 8
z = 1
A = pinf_bw01_10
rbind(
  c(mean(A[1,]), sd(A[1,]), mean(A[2,]), mean(A[1,]-q*A[2,]<th0 & A[1,]+q*A[2,]>th0), mean(A[1,]-qt(0.975,(p-1))*A[2,]<th0 & A[1,]+qt(0.975,(p-1))*A[2,]>th0), mean(A[1,]-q*A[2,]>0 | A[1,]+q*A[2,]<0), mean(A[1,]-qt(0.975,(p-1))*A[2,]>0 | A[1,]+qt(0.975,(p-1))*A[2,]<0), log(mean((A[1,]-th0)^2))),
  c(mean(A[3,]), sd(A[3,]), 0, 0, 0, 0, 0, log(mean((A[3,]-th0)^2))),
  c(mean(A[5,]), sd(A[5,]), mean(A[6,]), mean(A[5,]-q*A[6,]<th0 & A[5,]+q*A[6,]>th0), mean(A[5,]-qt(0.975,(p-A[4,]-1))*A[6,]<th0 & A[5,]+qt(0.975,(p-A[4,]-1))*A[6,]>th0), mean(A[5,]-q*A[6,]>0 | A[5,]+q*A[6,]<0), mean(A[5,]-qt(0.975,(p-A[4,]-1))*A[6,]>0 | A[5,]+qt(0.975,(p-A[4,]-1))*A[6,]<0), log(mean((A[5,]-th0)^2))),
  c(mean(A[7,]), sd(A[7,]), 0, 0, 0, 0, 0, log(mean((A[7,]-th0)^2))),
  c(mean(A[9,]), sd(A[9,]), mean(A[10,]), mean(A[9,]-q*A[10,]<th0 & A[9,]+q*A[10,]>th0), mean(A[9,]-qt(0.975,(p-A[8,]-1))*A[10,]<th0 & A[9,]+qt(0.975,(p-A[8,]-1))*A[10,]>th0), mean(A[9,]-q*A[10,]>0 | A[9,]+q*A[10,]<0), mean(A[9,]-qt(0.975,(p-A[8,]-1))*A[10,]>0 | A[9,]+qt(0.975,(p-A[8,]-1))*A[10,]<0), log(mean((A[9,]-th0)^2))),
  c(mean(A[11,]), sd(A[11,]), 0, 0, 0, 0, 0, log(mean((A[11,]-th0)^2))),
  c(mean(A[13,]), sd(A[13,]), mean(A[14,]), mean(A[13,]-q*A[14,]<th0 & A[13,]+q*A[14,]>th0), mean(A[13,]-qt(0.975,(p-A[12,]-1))*A[14,]<th0 & A[13,]+qt(0.975,(p-A[12,]-1))*A[14,]>th0), mean(A[13,]-q*A[14,]>0 | A[13,]+q*A[14,]<0), mean(A[13,]-qt(0.975,(p-A[12,]-1))*A[14,]>0 | A[13,]+qt(0.975,(p-A[12,]-1))*A[14,]<0), log(mean((A[13,]-th0)^2))),
  c(mean(A[15,]), sd(A[15,]), 0, 0, 0, 0, 0, log(mean((A[15,]-th0)^2))),
  c(mean(A[17,]), sd(A[17,]), mean(A[18,]), mean(A[17,]-q*A[18,]<th0 & A[17,]+q*A[18,]>th0), mean(A[17,]-qt(0.975,(p-A[16,]-1))*A[18,]<th0 & A[17,]+qt(0.975,(p-A[16,]-1))*A[18,]>th0), mean(A[17,]-q*A[18,]>0 | A[17,]+q*A[18,]<0), mean(A[17,]-qt(0.975,(p-A[16,]-1))*A[18,]>0 | A[17,]+qt(0.975,(p-A[16,]-1))*A[18,]<0), log(mean((A[17,]-th0)^2))),
  c(mean(A[19,]), sd(A[19,]), 0, 0, 0, 0, 0, log(mean((A[19,]-th0)^2))),
  c(mean(A[21,]), sd(A[21,]), mean(A[22,]), mean(A[21,]-q*A[22,]<th0 & A[21,]+q*A[22,]>th0), mean(A[21,]-qt(0.975,(p-A[20,]-1))*A[22,]<th0 & A[21,]+qt(0.975,(p-A[20,]-1))*A[22,]>th0), mean(A[21,]-q*A[22,]>0 | A[21,]+q*A[22,]<0), mean(A[21,]-qt(0.975,(p-A[20,]-1))*A[22,]>0 | A[21,]+qt(0.975,(p-A[20,]-1))*A[22,]<0), log(mean((A[21,]-th0)^2))),
  c(mean(A[23,]), sd(A[23,]), 0, 0, 0, 0, 0, log(mean((A[23,]-th0)^2))),
  c(mean(A[25,]), sd(A[25,]), mean(A[26,]), mean(A[25,]-q*A[26,]<th0 & A[25,]+q*A[26,]>th0), mean(A[25,]-qt(0.975,(p-A[24,]-1))*A[26,]<th0 & A[25,]+qt(0.975,(p-A[24,]-1))*A[26,]>th0), mean(A[25,]-q*A[26,]>0 | A[25,]+q*A[26,]<0), mean(A[25,]-qt(0.975,(p-A[24,]-1))*A[26,]>0 | A[25,]+qt(0.975,(p-A[24,]-1))*A[26,]<0), log(mean((A[25,]-th0)^2))),
  c(mean(A[27,A[27,]!=0]), sd(A[27,A[27,]!=0]), mean(A[28,A[27,]!=0]), mean(A[27,A[27,]!=0]-q*A[28,A[27,]!=0]<th0 & A[27,A[27,]!=0]+q*A[28,A[27,]!=0]>th0), mean(A[27,A[27,]!=0]-qt(0.975,(p-A[29,A[27,]!=0]-1))*A[28,A[27,]!=0]<th0 & A[27,A[27,]!=0]+qt(0.975,(p-A[29,A[27,]!=0]-1))*A[28,A[27,]!=0]>th0), mean(A[27,A[27,]!=0]-q*A[28,A[27,]!=0]>0 | A[27,A[27,]!=0]+q*A[28,A[27,]!=0]<0), mean(A[27,A[27,]!=0]-qt(0.975,(p-A[29,A[27,]!=0]-1))*A[28,A[27,]!=0]>0 | A[27,A[27,]!=0]+qt(0.975,(p-A[29,A[27,]!=0]-1))*A[28,A[27,]!=0]<0), log(mean((A[27,A[27,]!=0]-th0)^2))),
  c(mean(A[30,A[30,]!=0]), sd(A[30,A[30,]!=0]), mean(A[31,A[30,]!=0]), mean(A[30,A[30,]!=0]-q*A[31,A[30,]!=0]<th0 & A[30,A[30,]!=0]+q*A[31,A[30,]!=0]>th0), mean(A[30,A[30,]!=0]-qt(0.975,(p-k-1))*A[31,A[30,]!=0]<th0 & A[30,A[30,]!=0]+qt(0.975,(p-k-1))*A[31,A[30,]!=0]>th0), mean(A[30,A[30,]!=0]-q*A[31,A[30,]!=0]>0 | A[30,A[30,]!=0]+q*A[31,A[30,]!=0]<0), mean(A[30,A[30,]!=0]-qt(0.975,(p-k-1))*A[31,A[30,]!=0]>0 | A[30,A[30,]!=0]+qt(0.975,(p-k-1))*A[31,A[30,]!=0]<0), log(mean((A[30,A[30,]!=0]-th0)^2))),
  c(mean(A[32,]), sd(A[32,]), mean(A[33,]), mean(A[32,]-q*A[33,]<th0 & A[32,]+q*A[33,]>th0), mean(A[32,]-qt(0.975,(p-z-1))*A[33,]<th0 & A[32,]+qt(0.975,(p-z-1))*A[33,]>th0), mean(A[32,]-q*A[33,]>0 | A[32,]+q*A[33,]<0), mean(A[32,]-qt(0.975,(p-z-1))*A[33,]>0 | A[32,]+qt(0.975,(p-z-1))*A[33,]<0), log(mean((A[32,]-th0)^2)))
)