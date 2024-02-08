imv<-function (resp, pv1, pv2, eps = 1e-06) 
{
    pv1 <- ifelse(pv1 < eps, eps, pv1)
    pv2 <- ifelse(pv2 < eps, eps, pv2)
    pv1 <- ifelse(pv1 > 1 - eps, 1 - eps, pv1)
    pv2 <- ifelse(pv2 > 1 - eps, 1 - eps, pv2)
    ll <- function(x, p) {
        z <- log(p) * resp + log(1 - p) * (1 - resp)
        z <- sum(z)/length(x)
        exp(z)
    }
    loglik1 <- ll(resp, pv1)
    loglik2 <- ll(resp, pv2)
    getcoins <- function(a) {
        f <- function(p, a) abs(p * log(p) +
                                (1 - p) * log(1 - p) - log(a))
        nlminb(0.5, f, lower = 0.001, upper = 0.999, a = a)$par
    }
    c1 <- getcoins(loglik1)
    c2 <- getcoins(loglik2)
    ew <- function(p1, p0) (p1 - p0)/p0
    imv <- ew(c2, c1)
    imv
}
