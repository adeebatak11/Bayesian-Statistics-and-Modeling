library(rethinking)
library(tidyverse)

### 11.1.1
data("chimpanzees")
d <- chimpanzees

# create an index variable
d$treatment <- 1+d$prosoc_left+2*d$condition

# verify index var is correct using crosstabs
xtabs(~treatment + prosoc_left + condition, d)

# use HMC
dat_list <- list(pulled_left = d$pulled_left,
                 actor = d$actor,
                 treatment = as.integer(d$treatment))

m11.4 <- ulam(
  alist(
    pulled_left~dbinom(1,p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0,0.5)
  ),
  data = dat_list, chains = 4, cores = 4, log_lik = TRUE
)

precis(m11.4, depth = 2)
post <- extract.samples(m11.4)

# examining each actors behavior (only considering each chimpanzee)
p_left <- inv_logit(post$a)
plot(precis(as.data.frame(p_left)), xlim = c(0,1))

# examining the effect of treatment
plot(precis(m11.4, depth = 2, pars = "b"))
