# PREAMBLE ----------------------------------------------------------------
# TITLE: eip_pathway.R
# DESCRIPTION: specify the trajectory EIP referral through to discharge

rm(list=ls())

# Load packages
library(simmer)
library(simmer.plot)
library(dplyr)

# MODEL PARAMETERS -------------------------------------------------------

# Duration parameters (in days)
EIP_DURATION <- 365 * 3    # 3 years of EIP care
CBT_DURATION <- 7 * 16     # 16 weekly sessions
FI_DURATION <- 7 * 10      # 10 weekly sessions
CARER_DURATION <- 7 * 8    # 8 weekly sessions
IPS_DURATION <- 365        # 1 year of support
PHYSICAL_DURATION <- 7     # 1 week for assessment and intervention

# Time-to-event functions
get_eip_duration <- function() EIP_DURATION
get_cbt_duration <- function() CBT_DURATION  
get_fi_duration <- function() FI_DURATION
get_carer_duration <- function() CARER_DURATION
get_ips_duration <- function() IPS_DURATION
get_physical_duration <- function() PHYSICAL_DURATION

# Acceptance probabilities (to be replaced with more sophisticated logic)
CBT_ACCEPT_PROB <- 0.7     # 70% accept CBT when offered
FI_ACCEPT_PROB <- 0.6      # 60% accept FI when offered
CARER_ACCEPT_PROB <- 0.8   # 80% accept carer intervention
IPS_ACCEPT_PROB <- 0.5     # 50% accept IPS
PHYSICAL_ACCEPT_PROB <- 0.9 # 90% accept physical health intervention

# HELPER FUNCTIONS ------------------------------------------------------

# Function to create signal names for component outcomes
create_signal_name <- function(component, id, outcome) {
  paste0(component, "_", outcome, "_", id)
}

# DEFINE EIP TRAJECTORY ------------------------------------------------

create_eip_trajectories <- function(env) {
  
  # Set up a global counter for unique IDs
  env %>% add_global("id_counter", 0)
  
  # Monitoring trajectory
  monitor_traj <- trajectory("monitor") %>%
    log_(function() paste0("Starting monitoring trajectory for user ", get_attribute(env, "id"))) %>%
    set_attribute("clone_branch", 1) %>%
    # Initialize receipt tracking attributes
    set_attribute("cc_discharge", 0) %>%
    set_attribute("cbt_receipt", 0) %>%
    set_attribute("fi_receipt", 0) %>%
    set_attribute("carer_receipt", 0) %>%
    set_attribute("ips_receipt", 0) %>%
    set_attribute("physical_receipt", 0) %>%
    # Set up traps for acceptances
    trap(function() create_signal_name("cc", get_attribute(env, "id"), "discharge"),
         trajectory() %>% set_attribute("cc_discharge", 1)) %>%
    trap(function() create_signal_name("cbt", get_attribute(env, "id"), "accept"),
         trajectory() %>% set_attribute("cbt_receipt", 1)) %>%
    trap(function() create_signal_name("fi", get_attribute(env, "id"), "accept"),
         trajectory() %>% set_attribute("fi_receipt", 1)) %>%
    trap(function() create_signal_name("carer", get_attribute(env, "id"), "accept"),
         trajectory() %>% set_attribute("carer_receipt", 1)) %>%
    trap(function() create_signal_name("ips", get_attribute(env, "id"), "accept"),
         trajectory() %>% set_attribute("ips_receipt", 1)) %>%
    trap(function() create_signal_name("physical", get_attribute(env, "id"), "accept"),
         trajectory() %>% set_attribute("physical_receipt", 1)) %>%
    
    # Set up traps for refusals
    trap(function() create_signal_name("cbt", get_attribute(env, "id"), "refuse"),
         trajectory() %>% set_attribute("cbt_receipt", 99)) %>%
    trap(function() create_signal_name("fi", get_attribute(env, "id"), "refuse"),
         trajectory() %>% set_attribute("fi_receipt", 99)) %>%
    trap(function() create_signal_name("carer", get_attribute(env, "id"), "refuse"),
         trajectory() %>% set_attribute("carer_receipt", 99)) %>%
    trap(function() create_signal_name("ips", get_attribute(env, "id"), "refuse"),
         trajectory() %>% set_attribute("ips_receipt", 99)) %>%
    trap(function() create_signal_name("physical", get_attribute(env, "id"), "refuse"),
         trajectory() %>% set_attribute("physical_receipt", 99)) %>%
    
    # Wait for all signals
    # Wait for signals
    wait() %>% wait() %>% wait() %>% wait() %>% wait() %>% wait() %>%
    timeout(0.01)
  
  # Care coordination trajectory
  carecoord_traj <- trajectory("eip_carecoord") %>%
    set_attribute("clone_branch", 2) %>%
    seize("care_coordinator", 1) %>%
    timeout(get_eip_duration) %>%
    release("care_coordinator", 1) %>%
    send(function() create_signal_name("cc", get_attribute(env, "id"), "discharge"))
  
  # CBT trajectory with acceptance branch
  cbt_traj <- trajectory("cbt") %>%
    set_attribute("clone_branch", 3) %>%
    branch(
      function() runif(1) < CBT_ACCEPT_PROB,
      continue = TRUE,
      trajectory() %>%
        seize("cbt_therapist", 1) %>%
        timeout(get_cbt_duration) %>%
        release("cbt_therapist", 1) %>%
        send(function() create_signal_name("cbt", get_attribute(env, "id"), "accept")),
      trajectory() %>%
        send(function() create_signal_name("cbt", get_attribute(env, "id"), "refuse"))
    )
  
  # Family Intervention trajectory with acceptance branch
  fi_traj <- trajectory("fi") %>%
    set_attribute("clone_branch", 4) %>%
    branch(
      function() runif(1) < FI_ACCEPT_PROB,
      continue = TRUE,
      trajectory() %>%
        seize("fi_therapist", 1) %>%
        timeout(get_fi_duration) %>%
        release("fi_therapist", 1) %>%
        send(function() create_signal_name("fi", get_attribute(env, "id"), "accept")),
      trajectory() %>%
        send(function() create_signal_name("fi", get_attribute(env, "id"), "refuse"))
    )
  
  # Carer intervention trajectory with acceptance branch
  carer_traj <- trajectory("carer") %>%
    set_attribute("clone_branch", 5) %>%
    branch(
      function() runif(1) < CARER_ACCEPT_PROB,
      continue = TRUE,
      trajectory() %>%
        seize("carer_worker", 1) %>%
        timeout(get_carer_duration) %>%
        release("carer_worker", 1) %>%
        send(function() create_signal_name("carer", get_attribute(env, "id"), "accept")),
      trajectory() %>%
        send(function() create_signal_name("carer", get_attribute(env, "id"), "refuse"))
    )
  
  # Employment support trajectory with acceptance branch
  ips_traj <- trajectory("ips") %>%
    set_attribute("clone_branch", 6) %>%
    branch(
      function() runif(1) < IPS_ACCEPT_PROB,
      continue = TRUE,
      trajectory() %>%
        seize("employment_specialist", 1) %>%
        timeout(get_ips_duration) %>%
        release("employment_specialist", 1) %>%
        send(function() create_signal_name("ips", get_attribute(env, "id"), "accept")),
      trajectory() %>%
        send(function() create_signal_name("ips", get_attribute(env, "id"), "refuse"))
    )
  
  # Physical health trajectory with acceptance branch
  physical_traj <- trajectory("physical") %>%
    set_attribute("clone_branch", 7) %>%
    branch(
      function() runif(1) < PHYSICAL_ACCEPT_PROB,
      continue = TRUE,
      trajectory() %>%
        seize("physical_health_specialist", 1) %>%
        timeout(get_physical_duration) %>%
        release("physical_health_specialist", 1) %>%
        send(function() create_signal_name("physical", get_attribute(env, "id"), "accept")),
      trajectory() %>%
        send(function() create_signal_name("physical", get_attribute(env, "id"), "refuse"))
    )
  
  # Main EIP trajectory
  main_traj <- trajectory("main") %>%
    # Add unique ID and initialize attributes
    set_global("id_counter", 1, mod="+") %>%
    set_attribute("id", function() get_global(env, "id_counter")) %>%
    set_attribute("clone_branch", 0) %>%
    set_attribute("entry_time", function() now(env)) %>%
    
    # Clone and distribute to EIP components
    clone(
      n = 7,
      carecoord_traj,
      cbt_traj,
      fi_traj,
      carer_traj,
      ips_traj,
      physical_traj,
      monitor_traj
    )   %>%
    synchronize(wait = TRUE, mon_all = TRUE) %>%
    log_(function() paste0(
      "Post-synchronization for user ", get_attribute(env, "id"),
      " branch ", get_attribute(env, "clone_branch"), 
      " at time ", now(env),
      "\n  cc_discharge: ", get_attribute(env, "cc_discharge"),
      "\n  cbt_receipt: ", get_attribute(env, "cbt_receipt"),
      "\n  fi_receipt: ", get_attribute(env, "fi_receipt"),
      "\n  carer_receipt: ", get_attribute(env, "carer_receipt"),
      "\n  ips_receipt: ", get_attribute(env, "ips_receipt"),
      "\n  physical_receipt: ", get_attribute(env, "physical_receipt")
    ))
  return(main_traj)
}

env <- simmer("env")

eip_traj <- create_eip_trajectories(env)
plot(eip_traj)

# RUN SIMULATION -------------------------------------------------------

run_eip_simulation <- function(
    yearly_caseload,   # Expected number of new referrals per year
    resources = list(  # Resource levels (FTE staff)
      care_coordinator = 15,
      cbt_therapist = 5,
      fi_therapist = 4,
      carer_worker = 5,
      employment_specialist = 5,
      physical_health_specialist = 1
    ),
    sim_duration = 365*10  # Simulation duration in days
) {
  
  # Calculate arrival rate (in days) from yearly caseload
  arrival_rate <- 365/yearly_caseload
  
  # Create simulation environment
  env <- simmer("EIP_pathway")
  
  # Add resources
  for (resource_name in names(resources)) {
    env %>% add_resource(resource_name, resources[[resource_name]])
  }
  
  # Create trajectory
  eip_traj <- create_eip_trajectories(env)
  
  # Generate arrival times
  arrival_times <- cumsum(rexp(ceiling(sim_duration/arrival_rate * 1.5), 1/arrival_rate))
  arrival_times <- arrival_times[arrival_times <= sim_duration]
  
  # Add generator and run simulation
  env %>% 
    add_generator("service_user", eip_traj, at(arrival_times), mon = 2) %>%
    run(until = sim_duration)
  
  # Return results
  list(
    attributes = get_mon_attributes(env),
    arrivals = get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE),
    resources = get_mon_resources(env)
  )
}

# EXAMPLE USAGE -------------------------------------------------------

if (TRUE) {
  # Run simulation with 50 service users per year
  results <- run_eip_simulation(
    yearly_caseload = 50,
    resources = list(
      care_coordinator = 3,
      cbt_therapist = 2,
      fi_therapist = 1,
      carer_worker = 1,
      employment_specialist = 1, 
      physical_health_specialist = 1
    )
  )
  
  # Analyze receipt rates at end of simulation
  receipt_rates <- results$attributes %>%
    filter(name != "") %>%
    mutate(discharged = if_else(key=="cc_discharge" & value == 1, 1, 0)) %>%
    group_by(name) %>%
    filter(max(discharged) == 1) %>% ungroup() %>%
    group_by(name, key) %>%
    filter(time == max(time)) %>% ungroup() %>%
    select(name, key, value) %>%
    filter(grepl("_receipt$", key))   %>%
    group_by(key) %>%
    summarize(rate = mean(value))
  
  print(receipt_rates)
}
