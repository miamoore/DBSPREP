get.control.list = function(control){
  control.default = list(maxit = 1000000, quantiles.to.calc = c(0.025, 0.5, 0.975), precision = .001, N.new = 1000, rm.na = T)
  control.default[names(control)] = control
  control.default
}
Truvada.Eff.DBS.Grant = function(DBS){
  curve0 = log(4.07) - 0.00454*DBS
  curvehi = log(4.81) - 0.00389*DBS
  curvelo = log(3.44) - 0.00511*DBS

  zscore1 = stats::rnorm(1)
  zscore2 = stats::rnorm(1)

  se1 = curvehi - curve0

  se2 = log(3.45/2.6)

  1 - mean(exp(curve0 + se1 * zscore1 + se2*zscore2 - log(2.6)))
}

Truvada.Eff.DBS.Anderson = function(DBS){
  p = stats::runif(1)
  HR1 = 1 - expit(stats::qnorm(p, mean = logit(.76), sd = (logit(.76) - logit(.56))/stats::qnorm(.975)))
  HR2 = 1 - expit(stats::qnorm(p, mean = logit(.96), sd = (logit(.96) - logit(.90))/stats::qnorm(.975)))
  HR3 = 1 - expit(stats::qnorm(p, mean = logit(.99), sd = (logit(.99) - logit(.96))/stats::qnorm(.975)))

  HR = ifelse(DBS>=350,
              ifelse(DBS>=700,
                     ifelse(DBS>=1250,
                            HR3,
                            HR2),
                     HR1),
              1)

  mean(1 - HR)

}

Truvada.Eff.DBS.Moore = function(DBS){
  k.sample = sample(k.dbs.vals, 1)
  mean(1 - exp(-DBS/k.sample))
}
Truvada.Efficacy.Update = function(DBS, N, func){
  Efficacy.Estimate = numeric(0)
  for(i in seq(N)){
    Efficacy.Estimate[i] = func(DBS)
  }
  Efficacy.Estimate
}

Truvada.Efficacy.Converge = function(DBS, control){
  with(control,{
    if(rm.na){
      DBS = DBS[!is.na(DBS)]
    }
    efficacy = Truvada.Efficacy.Update(DBS, N.new, func)

    q = quantile(efficacy, prob = quantiles.to.calc)

    total.iterations = 0
    while(total.iterations<maxit){
      total.iterations = total.iterations + N.new
      efficacy = c(efficacy, Truvada.Efficacy.Update(DBS, N.new, func))
      qnew = quantile(efficacy, prob = quantiles.to.calc)
      if(max(abs(q - qnew))<precision){
        return(qnew)
      }
      q = qnew
    }

    q})
}

logit = function(x){log(x/(1 - x))}
expit = function(x){exp(x)/ (1 + exp(x))}
