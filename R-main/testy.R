library(GSImulator)
library(rubias)
library(dplyr)

# just configuring some migration rates.
Nmix <- 1000
Dpar <- 1.5
ruDpar <- 1.5
Npop <- 17
rs <- rep(144, Npop)  # refsizes
ru <- c(2, 3, 12)

ms <- {
  rut <- rgamma(length(ru), shape = ruDpar, scale = 10)
  rut <- rut / sum(rut)
  pvec <- lapply(1:length(rut), function(x) {
    tmp <- rgamma(ru[x], shape = Dpar, scale = 10)
    tmp <- tmp / sum(tmp)
    tmp <- tmp * rut[x]
  }) %>%
    unlist

  rmultinom(1, size = Nmix, prob = pvec)[,1]

}
TruePi <- pvec


GSImulate(refsizes = rs,
          mixsizes = ms,
          num_loci = 20,
          num_sets = 1,
          variability_string = " -t 0.3 ",
          marker_pars = " -u .15 3 ",
          M_matrix = sh_island_mig_mat(ru = c(2, 3, 12), Min = 40, Mbt = 3.0))


#lapply(1:2, function(x) as.data.frame(basefile2fst(x)))

reppy_frame <- data_frame(repunit = paste("repu", rep(1:3, ru), sep = "_"),
                          collection = paste("Pop", 1:Npop, sep = "_"))
indat <- gsim2bhru(1, reppy_frame)


oup <- ref_and_mix_pipeline(reference = indat %>% filter(sample_type == "reference"),
                     mixture = indat %>% filter(sample_type == "mixture"),
                     gen_start_col = 5,
                     method = "MCMC")


ref <- indat %>% filter(sample_type == "reference") %>%
  mutate(collection = factor(collection, levels = unique(collection)),
         repunit = factor(repunit, levels = unique(repunit)))

Hasselman_simulation_pipeline(reference = ref, gen_start_col = 5, seed = 345)

bias_comparison(reference = ref, gen_start_col = 5, seed = 345)
