model_file = '/Users/gcgibson/DLM_Final_Project/switching.bug' # BUGS model filename
cat(readLines(model_file), sep = "\n")

require(rbiips)
par(bty='l')
light_blue = rgb(.7, .7, 1)
light_red = rgb(1, .7, .7)
hot_colors = colorRampPalette(c('black', 'red', 'yellow', 'white'))

set.seed(0)
library(ggplot2)
SJdat<-read.csv("http://dengueforecasting.noaa.gov/Training/San_Juan_Training_Data.csv")
dat <- SJdat[c("season_week","total_cases")]

cases <- dat$total_cases[seq(1,10)]

sigma = .4; alpha = c(-2.5, -1); phi = .5; c0 = 1; x0 = 0; t_max = length(cases)
pi = matrix(c(.9, .1, .1, .9), nrow=2, byrow=TRUE)
data = list(t_max=t_max, sigma=sigma,y=cases,
            alpha=alpha, phi=phi, pi=pi, c0=c0, x0=x0)

model = biips_model(model_file, data, sample_data=FALSE)

n_burn = 2000
n_iter = 10000
thin = 1
n_part = 50


obj_pimh = biips_pimh_init(model, variables)


biips_pimh_update(obj_pimh, n_burn, n_part) # Burn-in iterations

out_pimh = biips_pimh_samples(obj_pimh, n_iter, n_part, thin=thin) # Return samples
summ_pimh = biips_summary(out_pimh, probs=c(.025, .975))
