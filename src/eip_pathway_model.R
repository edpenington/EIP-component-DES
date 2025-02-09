# PREAMBLE ----------------------------------------------------------------
# TITLE: eip_pathway.R
# DESCRIPTION: specify the trajectory EIP referral through to discharge

rm(list=ls())

# Load packages
library(simmer)
library(simmer.plot)
library(dplyr)

create_eip_trajectories <- function(
    env, 
    params = list(
      # Default sample parameters - real functions to be devised and passed as arguments
      
      # Care coordination parameters
      T_EIP_DURATION = function() max(1, rnorm(1, 365*3, 90)),
      T_CBT_DURATION = function() max(1, rnorm(1, 7*16, 7)),
    
    )
) {
  
  # Set up a global counter for unique IDs
  env %>% add_global("id_counter", 0)
  
  # Trapping trajectory for recording receipt of components
  trap_traj <- trajectory("trap") %>%
    log_(function() paste0("Starting trap trajectory for user ", get_attribute(env, "id"))) %>%
    set_attribute("clone_branch", 1) %>%
    # Care coordinator discharge
    set_attribute("cc_discharge", 0) %>%
    trap(function() paste0("cc_discharge_", get_attribute(env, "id")),
         trajectory() %>%
           log_(function() paste0("Received signal cc_receipt for user ", get_attribute(env, "id"))) %>%
           set_attribute("cc_discharge", 1)
    ) %>%
    # CBT receipt
    set_attribute("cbt_receipt", 0) %>%
    trap(function() paste0("cbt_receipt_", get_attribute(env, "id")),
         trajectory() %>%
           log_(function() paste0("Received signal cbt_receipt for user ", get_attribute(env, "id"))) %>%
           set_attribute("cbt_receipt", 1)
    ) %>%
    # Waits
    wait() %>% wait() %>%
    timeout(0.01)  
  
  # Care coordination trajectory
  carecoord_traj <- trajectory("eip_carecoord") %>%
    log_(function() paste("Joining queue for care coordination for user", get_attribute(env, "id"))) %>%
    set_attribute("clone_branch", 2) %>%
    seize("care_coordinator", 1) %>%
    timeout(params$T_EIP_DURATION) %>%
    release("care_coordinator", 1) %>%
    log_(function() paste("Ending care coordination for user", get_attribute(env, "id"))) %>%
    log_(function() paste("Sending signal that care coordination has ended for user ", get_attribute(env, "id"))) %>%
    send(function() paste0("cc_discharge_", get_attribute(env, "id")))
    
  
  # CBT trajectory
  cbt_traj <- trajectory("cbt") %>%
    log_(function() paste0("Starting CBTp for user ", get_attribute(env, "id"))) %>%
    set_attribute("clone_branch", 3) %>%
    seize("cbt_therapist", 1) %>%
    timeout(params$T_CBT_DURATION) %>%
    release("cbt_therapist", 1) %>%
    log_(function() paste("CBT complete for user", get_attribute(env, "id"))) %>% 
    log_(function() paste("Sending signal that CBT has been received for user ", get_attribute(env, "id"))) %>%
    send(function() paste0("cbt_receipt_", get_attribute(env, "id"))) %>%
    set_attribute("cbt_end", now(env))

  
  # Main EIP trajectory
  eip_pathway_traj <- trajectory("main") %>%
    log_("Starting EIP pathway") %>%
    # Add unique ID at start
    set_global("id_counter", 1, mod="+") %>%
    set_attribute("id", function() get_global(env, "id_counter")) %>% 
    set_attribute("clone_branch", 0) %>%
    
    
    # Clone and distribute to EIP components
    clone(
      n = 3,
      carecoord_traj,
      cbt_traj,
      trap_traj
    ) %>%
    log_(function() paste0("Trajectory cloned for user ", get_attribute(env, "id"))) %>%
    synchronize(wait = TRUE, mon_all = TRUE) %>%
    log_(function() paste0("Trajectories synchronized for user ", get_attribute(env, "id"))) %>%
    log_(function() paste0("Attributes for user ", get_attribute(env, "id"), 
                           ": cc_discharge - ", get_attribute(env, "cc_discharge"),
                           ", cbt_receipt - ", get_attribute(env, "cbt_receipt"),
                           ", clone_branch - ", get_attribute(env, "clone_branch")))
  
  return(eip_pathway_traj)
}


# Example usage
if (FALSE) {
  env <- simmer("EIP_pathway")
  
  # Add resources
  env %>%
    add_resource("care_coordinator", 1) %>%
    add_resource("cbt_therapist", 1) 
  
  # Get trajectory
  eip_traj <- create_eip_trajectories(env)
  plot(eip_traj, verbose = TRUE)
  
  # Add generator
  env %>%
    add_generator("service_user", eip_traj, at(0), mon = 2) %>%  # Generate one entity at time 0
    run(until = 10000)
  
  # Get monitoring data
  attributes <- get_mon_attributes(env)
  arrivals <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  resources <- get_mon_resources(env)
  print(attributes)
}
