

#install.packages("pglm")
library(pglm)
library(matrixStats)

result <- pglm(m2, dictatorDF, effect="individual", model="random", family=binomial("logit"),
     index=c("country","year"), method="bfgs")
summary(result)

pe <- result$estimate
vc <- solve(-result$hessian)
se <- sqrt(diag(vc))

simbetas <- mvrnorm(10000, pe, vc)
colnames(simbetas)

xhyp <- seq(5.5, 11, 0.5)
nscen <- length(xhyp)

alphas <- rep(0, 10000)
for (i in 1:10000) {
  alphas[i] <- rnorm(n=1, mean=0, sd=sqrt(simbetas[i,8]))  
}

# fixed intercepts- apply to all scenarios
intercept <- cbind(simbetas[,1],simbetas[,1],simbetas[,1],simbetas[,1],
                   simbetas[,1],simbetas[,1],simbetas[,1],simbetas[,1],
                   simbetas[,1],simbetas[,1],simbetas[,1],simbetas[,1]) # 10000, 12


# alphas -> unit-effects, drawn from N(0, sigma)
alphas <- rep(0, 10000)
for (i in 1:10000) {
  alphas[i] <- rnorm(n=1, mean=0, sd=sqrt(simbetas[i,8]))  
}
alphas_mat <- cbind(alphas,alphas,alphas,alphas,alphas,alphas,
                    alphas,alphas,alphas,alphas,alphas,alphas) # 10000, 12

function(input) {
  
}
xb <- as.matrix(nopartylegScen$x[,-1]) %*% t(simbetas[,2:7]) # varies by scenario , 10000, 12
lc <- intercept + t(xb) + alphas_mat # linear combination
pi <- 1 / (1 + exp(-lc)) # simulated pis

pe <- colMeans(pi)
lower <- colQuantiles(pi, probs=0.025)
upper <- colQuantiles(pi, probs=0.975)

# AIC comparison
k.m2 <- 7
k.pglm <- 8

aic.m2 <- 2*k.m2 - 2*ll.m2
aic.pglm <- 2*k.pglm - 2*(-1622.7)
aic.test <- aic.m2 - aic.pglm
aic.test











