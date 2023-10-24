# Function to extract infestation pressure and sea temperature for a farm
# based on its ID (locid)
# Assumes datafolder is set
extract_IP = function(lokid){
  IP_all = read.table(paste0(datafolder, "LarverTotaltFra2012.txt"), sep = "\t", dec = ",")
  ST_all = read.table(paste0(datafolder, "Sjoetemp.txt"), sep = "\t", dec = ",")
  which_nxt <- which(colnames(IP_all)=="XNextWeek1")
  this_week <- colnames(IP_all)[which_nxt - 1]
  if(is.element(lokid, rownames(IP_all))){
    IP_1wk = IP_all[lokid, this_week]
    IP_2wk = IP_all[lokid, which_nxt]
    ST = ST_all[lokid, this_week]
    week_no = substr(this_week, 6, 7)
    return(list(IP_1wk=IP_1wk, IP_2wk=IP_2wk, ST=ST, week_no=week_no))
  } else return(NA)
}
# Example:
# datafolder = "//vetinst.no\\dfs-felles/StasjonK/FAG/Akva/FoU-Prosjekter/34055_NewTechAqua/WP2/licecalc/data/"
# extract_IP("12115")

# Function to extract lice data for a farm
# based on its ID (locid)
# Assumes datafolder is set

# Note: there is an error in the "Fastsittendelus.txt" example file, so this does not work 100 % yet.

extract_lice = function(lokid){
  AF_all = read.table(paste0(datafolder, "VoksneHunnlus.txt"), sep = "\t", dec = ",")
  OM_all = read.table(paste0(datafolder, "BevegligeLus.txt"), sep = "\t", dec = ",")
  # FX_all = read.table(paste0(datafolder, "Fastsittendelus.txt"), sep = "\t", dec = ",")
  this_week <- colnames(AF_all)[ncol(AF_all)]
  if(is.element(lokid, rownames(AF_all))){
    AF = AF_all[lokid, this_week]
    OM = OM_all[lokid, this_week]
    # FX = FX_all[lokid, this_week]
    week_no = substr(this_week, 6, 7)
    return(list(AF=AF, OM=OM, #FX=FX, 
             week_no=week_no))
  } else return(NA)
}
# Example:
# datafolder = "//vetinst.no\\dfs-felles/StasjonK/FAG/Akva/FoU-Prosjekter/34055_NewTechAqua/WP2/licecalc/data/"
# extract_lice("12115")


# Function to predict lice per fish in different stages and cages one to two weeks ahead (from NewTechAqua project)
predict_lice = function(AF, # vector with number of adult female lice per fish in each cage in the farm
                         OM, # vector with number of other motile lice per fish in each cage in the farm
                         FX, # vector with number of fixed lice per fish in each cage in the farm
                         ST = 9, # sea temperature (degrees C)
                         W_SAL = 2.2, # mean salmon weight (kg) in each cage
                         N_SAL = 130, # salmon abundance (thousands) in each cage
                         CLF = F, # presence or absence of sea wrasses as cleaner fish in each cage
                         IP_1wk = 94000, # external lice infestation pressure for FX one week ahead
                         IP_2wk = 94000, # external lice infestation pressure for FX two weeks ahead
                         q1 = 0.05, # lower limit of prediction interval
                         q2 = 0.95, # upper limit of prediction interval
                         ncount = 20 # number of salmon counted in each cage to be predicted
                     ){

  # Parameter values
  param = list(
    a0_fx = 0.009259406,
    a0_om = 0.02446042,
    a0_af = 0.006548497,
    a_smp = 1.145382,
    s1 = 0.4914239,
    s2 = 2.274599,
    s3 = 0.8800338,
    s4 = 0.6109081,
    d10 = -2.376319,
    d11 = 0.3655094,
    d20 = -2.104319,
    d21 = 0.03749721,
    a_nfish_fx = -0.002420001,
    a_nfish_om = -0.0008172557,
    a_wgt_om = 0.07834939,
    a_wgt_af = 0.1127151,
    a_wrasse_af = -0.1984205,
    sigma_farm_day = 0.5281977,
    sigma_farm_day_fx = 1.23087,
    sigma_farm_day_om = 0.4223059,
    sigma_farm_day_af = 0.349999,
    phi_fx = 2.456719,
    phi_om = 5.804841,
    phi_af = 5.150588,
    w_farm_fx = 0.8777189,
    w_farm_om = 0.6324091,
    w_farm_af = 0.5355347,
    sigma_farm_day_2wk = 0.6788851,
    sigma_farm_day_fx_2wk = 1.376319,
    sigma_farm_day_om_2wk = 0.4843275,
    sigma_farm_day_af_2wk = 0.4212641,
    phi_fx_2wk = 2.472188,
    phi_om_2wk = 5.611457,
    phi_af_2wk = 4.960478
  )
  
    
  # Functions
  smptransform = function(x){x * 10^-9} # Note! If from maps, use (exp(x) - 1) * 10^-9
  
  inv_logit = function(x) exp(x) / (1 + exp(x)) 

  count_ci <- function(logmu, logsd, phi, B = 10000, ...){ # prediction intervals for counts
    RE = rnorm(n = B, mean = 0, sd = logsd)
    PRED = array(dim = c(length(logmu), B))
    for(i in 1:length(logmu)) PRED[i, ] = 
      rnbinom(n = B, 
              size = phi, 
              mu = exp(logmu[i] + RE) * ncount) / ncount
    ci1 = apply(PRED, 1, function(x) quantile(x, q1))
    ci2 = apply(PRED, 1, function(x) quantile(x, q2))
    return(list(ci1 = ci1, ci2 = ci2))
  }

  # centred variables
  tempc = ST - 9
  wgt = W_SAL - 2.2
  nsal = N_SAL - 130
  SMP_FX_1wk = smptransform(IP_1wk)  
  SMP_FX_2wk = smptransform(IP_2wk)  
  wrasse = 1 * CLF
  
  # State variables
  
  # - temperature-dependent development
  d1 = inv_logit(param$d10 + param$d11 * tempc)
  d2 = inv_logit(param$d20 + param$d21 * tempc)
  
  # Farm average of lice abundance
  AF_farm = mean(AF)
  OM_farm = mean(OM)
  FX_farm = mean(FX)
  
  # - weighted averages of cage and farm counts  
  FX_hat = 
    (1-param$w_farm_fx) * FX + 
    param$w_farm_fx * FX_farm 
  
  OM_hat = 
    (1-param$w_farm_om) * OM + 
    param$w_farm_om * OM_farm 
  
  AF_hat = 
    (1-param$w_farm_af) * AF + 
    param$w_farm_af * AF_farm   
  
  # - log-expected 1 wk 
  logmu_FX_1wk = log(
    param$a0_fx + param$s1 * (1 - d1) * FX_hat + 
    param$a_smp * SMP_FX_1wk) + 
    param$a_nfish_fx * nsal
  
  logmu_OM_1wk = log(
    param$a0_om + param$s2 * d1 * FX_hat + 
    param$s3 * (1 - d2) * OM_hat) + 
    param$a_nfish_om * nsal +
    param$a_wgt_om * wgt
  
  logmu_AF_1wk = log(
    param$a0_af + (.5*param$s3 + .5*param$s4) * d2 * OM_hat + 
    param$s4 * AF_hat) + 
    param$a_wgt_af * wgt +
    param$a_wrasse_af * wrasse

  # - log-expected 2 wk 
  logmu_FX_2wk = log(
    param$a0_fx + param$s1 * (1 - d1) * exp(logmu_FX_1wk) + 
      param$a_smp * SMP_FX_2wk) + 
    param$a_nfish_fx * nsal
  
  logmu_OM_2wk = log(
    param$a0_om + param$s2 * d1 * exp(logmu_FX_1wk) + 
      param$s3 * (1 - d2) * exp(logmu_OM_1wk)) + 
    param$a_nfish_om * nsal +
    param$a_wgt_om * wgt
  
  logmu_AF_2wk = log(
    param$a0_af + (.5*param$s3 + .5*param$s4) * d2 * exp(logmu_OM_1wk) + 
      param$s4 * exp(logmu_AF_1wk)) + 
    param$a_wgt_af * wgt +
    param$a_wrasse_af * wrasse
  
  # - standard errors for 1-wk predictions
  logsd_FX_1wk = sqrt(param$sigma_farm_day^2 + param$sigma_farm_day_fx^2)
  logsd_OM_1wk = sqrt(param$sigma_farm_day^2 + param$sigma_farm_day_om^2)
  logsd_AF_1wk = sqrt(param$sigma_farm_day^2 + param$sigma_farm_day_af^2)
  
  # - standard errors for 1-wk predictions
  logsd_FX_2wk = sqrt(param$sigma_farm_day_2wk^2 + param$sigma_farm_day_fx_2wk^2)
  logsd_OM_2wk = sqrt(param$sigma_farm_day_2wk^2 + param$sigma_farm_day_om_2wk^2)
  logsd_AF_2wk = sqrt(param$sigma_farm_day_2wk^2 + param$sigma_farm_day_af_2wk^2)

  # - bias correction for log(mean(x)) > mean(log(x)) for 1-wk predictions  
  biascorrection_FX_1wk = 0.5 * logsd_FX_1wk^2 
  biascorrection_OM_1wk = 0.5 * logsd_OM_1wk^2  
  biascorrection_AF_1wk = 0.5 * logsd_AF_1wk^2  
  
  # - bias correction for log(mean(x)) > mean(log(x)) for 2-wk predictions  
  biascorrection_FX_2wk = 0.5 * logsd_FX_2wk^2 
  biascorrection_OM_2wk = 0.5 * logsd_OM_2wk^2  
  biascorrection_AF_2wk = 0.5 * logsd_AF_2wk^2  
  
  # Best 1-wk predictor of the arithmetic mean:
  mu_FX_1wk = exp(logmu_FX_1wk + biascorrection_FX_1wk)
  mu_OM_1wk = exp(logmu_OM_1wk + biascorrection_OM_1wk)
  mu_AF_1wk = exp(logmu_AF_1wk + biascorrection_AF_1wk)
  
  # Best 2-wk predictor of the arithmetic mean:
  mu_FX_2wk = exp(logmu_FX_2wk + biascorrection_FX_2wk)
  mu_OM_2wk = exp(logmu_OM_2wk + biascorrection_OM_2wk)
  mu_AF_2wk = exp(logmu_AF_2wk + biascorrection_AF_2wk)
  
  # 1-wk prediction intervals for "true" values
  ci1_true_FX_1wk = exp(logmu_FX_1wk) * exp(qnorm(q1) * logsd_FX_1wk)
  ci2_true_FX_1wk = exp(logmu_FX_1wk) * exp(qnorm(q2) * logsd_FX_1wk)
  
  ci1_true_OM_1wk = exp(logmu_OM_1wk) * exp(qnorm(q1) * logsd_OM_1wk)
  ci2_true_OM_1wk = exp(logmu_OM_1wk) * exp(qnorm(q2) * logsd_OM_1wk)
  
  ci1_true_AF_1wk = exp(logmu_AF_1wk) * exp(qnorm(q1) * logsd_AF_1wk)
  ci2_true_AF_1wk = exp(logmu_AF_1wk) * exp(qnorm(q2) * logsd_AF_1wk)
  
  # 2-wk prediction intervals for "true" values
  ci1_true_FX_2wk = exp(logmu_FX_2wk) * exp(qnorm(q1) * logsd_FX_2wk)
  ci2_true_FX_2wk = exp(logmu_FX_2wk) * exp(qnorm(q2) * logsd_FX_2wk)
  
  ci1_true_OM_2wk = exp(logmu_OM_2wk) * exp(qnorm(q1) * logsd_OM_2wk)
  ci2_true_OM_2wk = exp(logmu_OM_2wk) * exp(qnorm(q2) * logsd_OM_2wk)
  
  ci1_true_AF_2wk = exp(logmu_AF_2wk) * exp(qnorm(q1) * logsd_AF_2wk)
  ci2_true_AF_2wk = exp(logmu_AF_2wk) * exp(qnorm(q2) * logsd_AF_2wk)
  
  # Prediction intervals for counts
  ci_count_FX_1wk = count_ci(logmu = logmu_FX_1wk, 
                             logsd = logsd_FX_1wk, 
                             phi = param$phi_fx)
  ci_count_OM_1wk = count_ci(logmu = logmu_OM_1wk, 
                             logsd = logsd_OM_1wk, 
                             phi = param$phi_om)
  ci_count_AF_1wk = count_ci(logmu = logmu_AF_1wk, 
                             logsd = logsd_AF_1wk, 
                             phi = param$phi_af)
  ci_count_FX_2wk = count_ci(logmu = logmu_FX_2wk, 
                             logsd = logsd_FX_2wk, 
                             phi = param$phi_fx_2wk)
  ci_count_OM_2wk = count_ci(logmu = logmu_OM_2wk, 
                             logsd = logsd_OM_2wk, 
                             phi = param$phi_om_2wk)
  ci_count_AF_2wk = count_ci(logmu = logmu_AF_2wk, 
                             logsd = logsd_AF_2wk, 
                             phi = param$phi_af_2wk)
  
  # return(list(
  #   FX_1wk = list(mu = mu_FX_1wk, 
  #                 ci1_true = ci1_true_FX_1wk, 
  #                 ci2_true = ci2_true_FX_1wk,
  #                 ci1_count = ci_count_FX_1wk$ci1, 
  #                 ci2_count = ci_count_FX_1wk$ci2),
  #   OM_1wk = list(mu = mu_FX_1wk, 
  #                 ci1_true = ci1_true_OM_1wk, 
  #                 ci2_true = ci2_true_OM_1wk,
  #                 ci1_count = ci_count_OM_1wk$ci1, 
  #                 ci2_count = ci_count_OM_1wk$ci2),
  #   AF_1wk = list(mu = mu_AF_1wk, 
  #                 ci1_true = ci1_true_AF_1wk, 
  #                 ci2_true = ci2_true_AF_1wk,
  #                 ci1_count = ci_count_AF_1wk$ci1, 
  #                 ci2_count = ci_count_AF_1wk$ci2),
  #   FX_2wk = list(mu = mu_FX_2wk, 
  #                 ci1_true = ci1_true_FX_2wk, 
  #                 ci2_true = ci2_true_FX_2wk,
  #                 ci1_count = ci_count_FX_2wk$ci1, 
  #                 ci2_count = ci_count_FX_2wk$ci2),
  #   OM_2wk = list(mu = mu_OM_2wk, 
  #                 ci1_true = ci1_true_OM_2wk, 
  #                 ci2_true = ci2_true_OM_2wk,
  #                 ci1_count = ci_count_OM_2wk$ci1, 
  #                 ci2_count = ci_count_OM_2wk$ci2),
  #   AF_2wk = list(mu = mu_AF_2wk, 
  #                 ci1_true = ci1_true_AF_2wk, 
  #                 ci2_true = ci1_true_AF_2wk,
  #                 ci1_count = ci_count_AF_2wk$ci1, 
  #                 ci2_count = ci_count_AF_2wk$ci2)
  # ))


  prediction <- tibble::tibble(
    week = c("FX_1wk", "OM_1wk", "AF_1wk", "FX_2wk", "OM_2wk", "AF_2wk"),
    mu = c(mu_FX_1wk, mu_OM_1wk,  mu_AF_1wk, mu_FX_2wk, mu_OM_2wk, mu_AF_2wk),
    c1_true = c(ci1_true_FX_1wk, ci1_true_OM_1wk, ci1_true_AF_1wk,
      ci1_true_FX_2wk, ci1_true_OM_2wk, ci1_true_AF_2wk),
    c2_true = c(ci2_true_FX_1wk, ci2_true_OM_1wk, ci2_true_AF_1wk,
      ci2_true_FX_2wk, ci2_true_OM_2wk, ci2_true_AF_2wk),
    ci1_count = c(ci_count_FX_1wk$ci1, ci_count_OM_1wk$ci1, ci_count_AF_1wk$ci1,
    ci_count_FX_2wk$ci1, ci_count_OM_2wk$ci1, ci_count_AF_2wk$ci1),
    ci2_count = c(ci_count_FX_1wk$ci2, ci_count_OM_1wk$ci2, ci_count_AF_1wk$ci2,
    ci_count_FX_2wk$ci2, ci_count_OM_2wk$ci2, ci_count_AF_2wk$ci2)
  )

  prediction


}

# Example:
# AF = 1:3
# OM = 3:1
# FX = c(0,0,.5)
# predict_lice(AF=AF, OM=OM, FX=FX, ST=14)$AF_1wk$mu
# predict_lice(AF=AF, OM=OM, FX=FX, ST=14)$AF_2wk$mu
# predict_lice(AF=AF, OM=OM, FX=FX, ST=14)$AF_2wk$ci1_count
# predict_lice(AF=AF, OM=OM, FX=FX, ST=14)$AF_2wk$ci2_count

# Function to plot predictions for one cage:
plot_lice = function(prediction.object, cage=1, lan="no"){

  yat <- c(0,.1,.2,.5,1,2,4,8)
  ymax <- 5

  xlab1 <- ifelse(lan=="no", "Uke", "Week")
  if(lan=="no") xlab2 <- c("Fastsittende", "Mobile", "Hunnlus") else xlab2 <- c("Sessile", "Motile", "Female")
  xlab3 <- ifelse(lan=="no", paste0("Bur ", cage), paste0("Cage ", cage))
  
  ylab <- ifelse(lan=="no", "Lus per fisk", "Lice per fish")

  ytrans <- function(x) log(x + .1)

  prediction.cage <- lapply(prediction.object, function(x) x[[cage]])
  
  extractpoint <- function(stage, wk, metric, cage){
    return(ytrans(prediction.object[[paste0(stage,"_",wk,"wk")]][[metric]][cage]))
  }

  extractinterval <- function(stage, wk, cage){
    out <- c(extractpoint(stage, wk, "ci1_count", cage),
             extractpoint(stage, wk, "ci1_true", cage),
             extractpoint(stage, wk, "mu", cage),
             extractpoint(stage, wk, "ci2_true", cage),
             extractpoint(stage, wk, "ci2_count", cage))
    return(out)
  }
  
  plotinterval <- function(stage, wk, cage, xat, col){
    interval <- extractinterval(stage, wk, cage)
    segments(x0 = xat, x1 = xat, y0 = interval[1], y1 = interval[5], col=col)
    segments(x0 = xat, x1 = xat, y0 = interval[2], y1 = interval[4], col=col, lwd=5)
    points(x = xat, y = interval[3], col = col, pch = 16, cex = 2)
  }

  plot(0,0, type="n", xlim = c(0,3), ylim = c(ytrans(0), ytrans(ymax)), axes=F, xlab="", ylab="")
  
  plotinterval("FX", 1, cage, .33, "grey")
  plotinterval("FX", 2, cage, .66, "grey")
  
  plotinterval("OM", 1, cage, 1.33, "blue")
  plotinterval("OM", 2, cage, 1.66, "blue")

  plotinterval("AF", 1, cage, 2.33, "red")
  plotinterval("AF", 2, cage, 2.66, "red")

  axis(side=2, at=ytrans(yat), lab=yat)  

  mtext(c(1,2,1,2,1,2), side=1, at=c(.33,.66,1.33,1.66,2.33,2.66))
  mtext(xlab1, side = 1, line = 2)
  mtext(ylab, side = 2, line = 2.5)
  mtext(xlab2, side = 3, at = c(.5, 1.5, 2.5))
  mtext(xlab3, side = 3, line = 2, font = 2)
}

# Example:
# AF = 1:3
# OM = 3:1
# FX = c(0,0,.5)
# prediction.object = predict_lice(AF=AF, OM=OM, FX=FX)
# par(mai = c(.7,.7,.6,.01))
# plot_lice(prediction.object, cage = 2, lan="en")
