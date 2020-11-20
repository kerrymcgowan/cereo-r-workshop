#setwd("~/cereo-r-workshop/drake_template/")

# Load packages
source("R/packages.R")

# Load functions
source("R/functions.R")

# Load plan
source("R/plan.R")

# Look at status of work flow
vis_drake_graph(plan)
#outdated(plan)

make(
  
  # Run the plan created with plan.R
  plan,
  # Show target-by-target messages (status updates)
  verbose = 1,
  recover = TRUE
  
)

# Check status of workflow again
vis_drake_graph(plan)

#drake_history()

# Restart R to run (Session > Restart R), clears environment, before running for the first time after troubleshooting

# Tells you which targets are invalidated if you clean your drake memory
which_clean()

vis_drake_graph()

clean()

make(
  
  # Run the plan created with plan.R
  plan,
  # Show target-by-target messages (status updates)
  verbose = 1
  
)

vis_drake_graph(plan)















