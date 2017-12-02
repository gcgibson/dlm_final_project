library(tmvtnorm)
library(ggplot2)
SJdat<-read.csv("http://dengueforecasting.noaa.gov/Training/San_Juan_Training_Data.csv")
dat <- SJdat[c("season_week","total_cases")]

cases <- dat$total_cases[1:200]
require(rbiips)
library(MCMCpack)
dMN_dim <- function(s,i) {
  # Check dimensions of the input and return dimension of the output of
  # distribution dMN
  3
}
dMN_sample <- function(s,i) {
  # Draw a sample of distribution dMN
  
  rsamp <- rtmvnorm(1, mean=c(s,i),sigma=matrix(c(1,0,0,1),nrow=2,byrow = FALSE))
                  
  c(rsamp[1],rsamp[2])
}
biips_add_distribution('ddirch', 2, dMN_dim, dMN_sample)



model_file = '/home/gcgibson/dlm_final_project/blob.bug' # BUGS model filename
cat(readLines(model_file), sep = "\n")

par(bty='l')
light_blue = rgb(.7, .7, 1)
light_red = rgb(1, .7, .7)

t_max = length(cases)
mean_x_init = 1
prec_x_init = 1/5
prec_x = 1/10
log_prec_y_true = log(1) # True value used to sample the data
data = list(t_max=t_max, y = cases +1,prec_x_init=prec_x_init,
            prec_x=prec_x, 
            mean_x_init=c(.9,.05))

sample_data = FALSE # Boolean
model = biips_model(model_file, data, sample_data=sample_data) # Create Biips model and sample data

data = model$data()

### PMMH
if (FALSE){
  n_burn = 2000 # nb of burn-in/adaptation iterations
  n_iter = 2000 # nb of iterations after burn-in
  thin = 1 # thinning of MCMC outputs
  n_part = 50 # nb of particles for the SMC
  param_names = c('beta','gamma') # name of the variables updated with MCMC (others are updated with SMC)
  latent_names = c('x') # name of the variables updated with SMC and that need to be monitored
  
  
  obj_pmmh = biips_pmmh_init(model, param_names, inits=list(beta=.9,gamma=.1),
                             latent_names=latent_names) # creates a pmmh object
  biips_pmmh_update(obj_pmmh, n_burn, n_part) # adaptation and burn-in iterations
  
  out_pmmh = biips_pmmh_samples(obj_pmmh, n_iter, n_part, thin=thin) # samples
  
  summ_pmmh = biips_summary(out_pmmh, probs=c(.025, .975))
}



### PARTICLE FILTER
n_part = 1000 # Number of particles
variables = c('x','y') # Variables to be monitored
mn_type = 'fs'; rs_type = 'stratified'; rs_thres = 0.9 # Optional parameters



out_smc = biips_smc_samples(model, variables, n_part,
                            type=mn_type, rs_type=rs_type, rs_thres=rs_thres)
diag_smc = biips_diagnosis(out_smc)
summ_smc = biips_summary(out_smc, probs=c(.025, .975))


x_f_mean = summ_smc$x$f$mean
x_f_quant = summ_smc$x$f$quant

plot(exp(x_f_mean[2,]),col='red')
points(cases,col=1)


