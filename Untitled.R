dat <- simulate(N=100, p=p, v_e2=v_e2, v_e3=v_e3, a2=a2, 
                 b2=b2, a3=a3, b3=b3, gam=gam, kap=1)

#linear structural equation model, parametric estimation
fit_m <- lm(M~T, data=data)
beta2 <- fit_m$coefficients[[2]]
# if(i==1){
#   beta2 <- b_2
# } else {
#   beta2 <- c(beta2, b_2)
# }
assign(paste0("beta2_", size), beta2)
fit_y <- lm(Y~T+M, data=data)
gamma <- fit_y$coefficients[[3]]
# if(i==1){
#   gamma <- g
# } else {
#   gamma <- c(gamma, g)
# }
ACME <- beta2*gamma
assign(paste0("beta2_", size), beta2)
assign(paste0("gamma_", size), gamma)
assign(paste0("ACME_", size), mean(ACME))

#nonparametric estimation
J <- 50
n0 <- table(data$T_i)[[1]]
n1 <- table(data$T_i)[[2]]
M_breaks <- quantile(data$M_i, probs = seq(0, 1, length.out = J + 1), na.rm = TRUE)
M_factor <- cut(data$M_i, M_breaks, include.lowest = TRUE, labels = FALSE)
M_counts <- table(M_factor)
###
sum_Y0 <- tapply(data$Y_i[which(data$T_i==0)], M_factor[which(data$T_i==0)], sum)
sum_Y1 <- tapply(data$Y_i[which(data$T_i==1)], M_factor[which(data$T_i==1)], sum)
###
sum_Y0 <- tapply(data$M_i, M_factor, function(x) sum((data$Y_i[data$M_i %in% x])[data$T_i[data$M_i %in% x] == 0]))
sum_Y1 <- tapply(data$M_i, M_factor, function(x) sum((data$Y_i[data$M_i %in% x])[data$T_i[data$M_i %in% x] == 1]))

counts_T0 <- tapply(data$M_i, M_factor, function(x) sum(data$T_i[data$M_i %in% x] == 0))
counts_T0_n0 <- counts_T0[counts_T0 != 0]
counts_T1 <- tapply(data$M_i, M_factor, function(x) sum(data$T_i[data$M_i %in% x] == 1))
counts_T1_n0 <- counts_T1[counts_T1 != 0]
ACME_non_T0 <- sum(sum_Y0/counts_T0_n0 *(counts_T1/n1 - counts_T0/n0)[counts_T0 != 0])
ACME_T0 <- sum(sum_Y0/counts_T0 *(counts_T1/n1 - counts_T0/n0), na.rm=TRUE)
ACME_T1 <- sum(sum_Y1/counts_T1 *(counts_T1/n1 - counts_T0/n0), na.rm=TRUE)

ACME_non_T1 <- sum(sum_Y1/counts_T1_n0 *(counts_T1/n1 - counts_T0/n0)[counts_T1 != 0])
ACME_non <- mean(c(ACME_non_T0, ACME_non_T1))
if(i==1){
  ACME_c <- ACME_non 
} else {
  ACME_c <- c(ACME_c, ACME_non)
}
assign(paste0("ACME_non_", size), mean(ACME_c))