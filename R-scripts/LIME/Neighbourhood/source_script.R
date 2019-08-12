### These two have to be executed at first.
source("R-scripts/LIME/Neighbourhood/packages.R")
source("R-scripts/LIME/Neighbourhood/utils.R")

### These scripts can be exectued in arbitrary order.
### They compute the results and will take VERY long.
source("R-scripts/LIME/Neighbourhood/intro.R")
source("R-scripts/LIME/Neighbourhood/1d_setting.R")
source("R-scripts/LIME/Neighbourhood/global_linear.R")
source("R-scripts/LIME/Neighbourhood/local_linear.R")
source("R-scripts/LIME/Neighbourhood/global_nonlinear.R")
source("R-scripts/LIME/Neighbourhood/fi.R")
source("R-scripts/LIME/Neighbourhood/real.R")

### Last script to execute: Create plots from results.
source("R-scripts/LIME/Neighbourhood/plot_script.R")
