set.seed(0)
tab.tmp <- data.frame(matrix(NA, nrow=100, ncol=4, dimnames=list(paste0("r", 1:100), c("stat1", "p1", "stat2", "p2"))))
tab.tmp[, c(1, 3)] <- rnorm(n=200)
tab.tmp[, 2] <- 2*pnorm(-abs(tab.tmp[, 1]))
tab.tmp[, 4] <- 2*pnorm(-abs(tab.tmp[, 3]))

mtrx.tmp <- matrix(NA, nrow=100, ncol=4)
mtrx.tmp[] <- as.matrix(tab.tmp)

# mediation
set.seed(0)
n = 100; beta_xe = 0.5; beta_em = 0.7; beta_my = 0.6; beta_xy = 0.3; error_sd = 1
E <- rnorm(n)
M1 <- beta_em * E + rnorm(n, sd = error_sd)
M2 <- beta_em * E + rnorm(n, sd = error_sd) + 0.2*rnorm(n)
M <- rbind(M1,M2)
Y <- beta_my * M1 + beta_xy * E + rnorm(n, sd = error_sd)
names(E) <- names(Y) <- colnames(M) <- paste0("S", 1:ncol(M))
rownames(M) <- paste0("G", 1:nrow(M))

set.seed(0)
cvrts.df <- data.frame(x1 = rnorm(n=n), lvl=c("low", "med", "high")[sample(1:3, size=n, replace = TRUE)])
cvrts.mod <- model.matrix(~., data = cvrts.df)[, -1]

