# PREAMBLE ----------------------------------------------------------------
# TITLE: run_model.R
# DESCRIPTION: predict receipt of EIP components based on resources within DES model

# Clear the workspace
rm(list=ls())

# Load packages
library(simmer)
library(simmer.plot)



# PARAMETERS --------------------------------------------------------------

# EIP duration
fun_EIPduration <- function() {
  
  t_EIPduration <- rnorm(n=1, mean = 3*365.25, sd = 50)
  
  return(t_EIPduration)
}


# TRAJECTORIES ------------------------------------------------------------

traj <- trajectory(name = "FEP") %>%
  # Starting point: referral for suspected FEP
  
  # Enter queue for EIP
  seize(resource = "coordinator", amount = 1) %>%
  
  # Calculate time in EIP
  set_attribute(keys = "t_EIPduration", values = function() fun_EIPduration()) %>%
  timeout_from_attribute(key = "t_EIPduration") %>%
  
  # Discharge
  release(resource = "coordinator", amount = 1)

plot(traj, verbose=TRUE)

# SIMULATION --------------------------------------------------------------

# Set simulation parameters
n_serviceusers <- 500
t_serviceusergap <- 2

# Create main simulation environment
sim <- simmer(name = "sim") %>%
  add_resource(name = "coordinator", capacity = 112, mon = 2) %>%
  add_generator(name_prefix = "serviceuser",
                trajectory = traj,
                distribution = at(seq(from = 1, to = n_serviceusers * t_serviceusergap,
                                      by = t_serviceusergap)),
                mon = 2)


# Run simulations
random_seed <- 123
set.seed(random_seed)
sim %>% reset() %>% run(until = n_serviceusers * t_serviceusergap + (365.25 * 3) )

df_sim <- get_mon_attributes(.env = sim)
df_resources <- get_mon_resources(.env = sim)
df_arrivals <- get_mon_arrivals(.env = sim, per_resource = TRUE, ongoing = TRUE)

# VALIDATION --------------------------------------------------------------


