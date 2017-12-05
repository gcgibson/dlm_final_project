SJdat<-read.csv("http://dengueforecasting.noaa.gov/Training/San_Juan_Training_Data.csv")
dat <- SJdat[c("season_week","total_cases")]

cases <- dat$total_cases[1:100]
require(rbiips)
library(MCMCpack)
model_file = '/Users/gcgibson/DLM_final_project/log_alpha.bug' # BUGS model filename
cat(readLines(model_file), sep = "\n")

par(bty='l')
light_blue = rgb(.7, .7, 1)
light_red = rgb(1, .7, .7)


t_max = length(cases)
n_burn = 500 # nb of burn-in/adaptation iterations
n_iter = 1000 # nb of iterations after burn-in
thin = 5 # thinning of MCMC outputs
n_part = 50 # nb of particles for the SMC
param_names = c('alpha') # names of the variables updated with MCMC (others are updated with SMC)
latent_names = c('x') # names of the variables updated with SMC and that need to be monitored

G = matrix(c(cos(2*pi),sin(2*pi),-sin(2*pi),cos(2*pi)), nrow=2, byrow=TRUE)

#setting the mean value of the initial count to 1400
data = list(t_max=t_max, y = cases,  G = G, mean_sigma_init = c(0,0), cov_sigma_init=diag(2) ,mean_x_init=c(0,0))
model = biips_model(model_file, data=data,sample_data = FALSE)

##fixing variance for now, will extend model to handle inference over variance later
n_burn = 5000 # nb of burn-in/adaptation iterations
n_iter = 10000 # nb of iterations after burn-in
thin = 5 # thinning of MCMC outputs
n_part = 50 # nb of particles for the SMC
param_names = c('alpha') # names of the variables updated with MCMC (others are updated with SMC)
latent_names = c('x') # names of the variables updated with SMC and that need to be monitored
inits = list(10)

obj_pmmh = biips_pmmh_init(model, param_names, inits=list(1),
                           latent_names=latent_names) # creates a pmmh object
biips_pmmh_update(obj_pmmh, n_burn, n_part) # adaptation and burn-in iterations
out_pmmh = biips_pmmh_samples(obj_pmmh, n_iter, n_part, thin=thin) # samples

summ_pmmh = biips_summary(out_pmmh, probs=c(.025, .975))

