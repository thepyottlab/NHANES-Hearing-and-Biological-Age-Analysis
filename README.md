# Install
1. Install git # if not already installed
2. Install Rtools # if not already installed
3. Open terminal in a folder and run `git clone https://github.com/TomNaber/NHANES_Analysis.git`
4. Open `NHANES Analysis.Rproj`
5. In R terminal, run `install.packages("renv")`  # if not already installed
6. In R terminal, run `renv::install("RcppArmadillo@15.2.3-1")` or `install.packages("RcppArmadillo")` if the previous fails
7. In R terminal, run `renv::restore(exclude = RcppArmadillo)`
