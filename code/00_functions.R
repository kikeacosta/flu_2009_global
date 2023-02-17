library(readxl)
library(tidyverse)
library(countrycode)
library(lubridate)
library(ISOweek)
library(HMDHFDplus)
library(mgcv)

write_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}

interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t)
  db2 <- 
    db %>% 
    mutate(pop2 = spline(xs, ys, xout = ts)$y)
  return(db2)
}
# ====





rescale_age <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Age == "TOT") %>% dplyr::pull(Deaths)
  chunk %>% 
    dplyr::filter(Age != "TOT") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT)
}

# rescale deaths by sex to total sexes
rescale_sex <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Sex == "t") %>% dplyr::pull(Deaths)
  temp1 <- 
    chunk %>% 
    dplyr::filter(Sex != "t") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT) %>% 
    bind_rows(chunk %>% 
                dplyr::filter(Sex == "t"))
  
}

std_db <- function(db){
  db2 <- db %>% 
    select(Country, Year = YearOccurrence, Sex, Age = AgeStart, Deaths)
}

# chunk <- 
#   all_in2 %>% 
#   filter(Source == "unpd",
#          Country == "Belgium", 
#          Year == 2018, 
#          Sex == "f")

# chunk <- temp5

# functions for baseline mortality estimation
# ===========================================

# db <- 
#   dt3 %>% 
#   filter(country == "Austria",
#          age == 60)

est_linear <- 
  function(db){
    test_model <- 
      gam(dts ~ s(t, bs = 'ps', m = c(2,2)) + offset(log(exposure)), 
          data = db, 
          family = "quasipoisson")
    
    resp <- predict(test_model, newdata = db, type = "response", se.fit = TRUE)
    
    db %>% 
      mutate(bsn = resp$fit,
             ll = bsn - 2*resp$se,
             ul = bsn + 2*resp$se)
    
  }

# fitting the model
# ~~~~~~~~~~~~~~~~~
est_baseline <- 
  function(db, knots = NA){
    
    
    if(!is.na(knots)){
      gam_model <- 
        gam(dts ~ t + 
              s(week, bs = 'cp', k = knots) +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }else{
      gam_model <- 
        gam(dts ~ t + 
              s(week, bs = 'cp') +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }
    
    resp <- predict(gam_model, newdata = db, type = "response")
    
    db %>% 
      mutate(bsn = resp,
             p_score = dts / bsn,
             dts_r = dts / exposure,
             bsn_r = bsn / exposure) %>% 
      left_join(simul_intvals(gam_model, db, 1000),
                by = "date") %>% 
      mutate(ll_r = ll / exposure,
             ul_r = ul / exposure)
  }

# bootstrapping using Jonas' method 
simul_intvals <- function(model, db, nsim){
  # matrix model
  X_prd <- predict(model, newdata = db, type = 'lpmatrix')
  # estimated coefficients
  beta <- coef(model)
  # offsets
  offset_prd <- matrix(log(db$exposure))
  
  # applying Huber-White adjustment for robust estimators 
  # beta_sim <- MASS::mvrnorm(nsim, beta, sandwich::vcovHAC(model))
  beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
  Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd%*%b + offset_prd))
  
  y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
    y <- mu <- Ey
    # NA's can't be passed to the simulation functions, so keep them out
    idx_na <- is.na(mu) 
    mu_ <- mu[!idx_na] 
    N <- length(mu_)
    phi <- summary(model)$dispersion
    # in case of under-dispersion, sample from Poisson
    if (phi < 1) { phi = 1 }
    y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
    return(y)
  })
  
  ints_simul <- 
    db %>% 
    select(date)
  
  colnames_y_sim <- paste0('deaths_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  ints_simul <-
    ints_simul %>%
    pivot_longer(cols = starts_with('deaths_sim'),
                 names_to = 'sim_id', values_to = 'deaths_sim') %>%
    group_by(date) %>%
    summarise(
      ll = quantile(deaths_sim, 0.05, na.rm = TRUE),
      ul = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}



