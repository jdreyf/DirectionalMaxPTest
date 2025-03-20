set.seed(0)
tab.tmp <- data.frame(matrix(NA, nrow=100, ncol=4, dimnames=list(paste0("r", 1:100), c("stat1", "p1", "stat2", "p2"))))
tab.tmp[, c(1, 3)] <- rnorm(n=200)
tab.tmp[, 2] <- 2*pnorm(-abs(tab.tmp[, 1]))
tab.tmp[, 4] <- 2*pnorm(-abs(tab.tmp[, 3]))

mtrx.tmp <- matrix(NA, nrow=100, ncol=4)
mtrx.tmp[] <- as.matrix(tab.tmp)

n <- 10
E <- rnorm(n)
Y <- rnorm(n)
M <- matrix(rnorm(100*n), nrow=100, ncol=n)
covariates <- matrix(rnorm(n*3), nrow=n, ncol=3)
names(E) <- names(Y) <- colnames(M) <- rownames(covariates) <- paste0("s", 1:n)
rownames(M) <- paste0("g", 1:nrow(M))
colnames(covariates) <- paste0("cvr", 1:ncol(covariates))