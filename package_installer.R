# setup the installation repository
options(repos=structure(c(CRAN="https://cloud.r-project.org/")))

# List out the packages in a vector
cran.packages <- c("e1071",
                   "ggplot2",
                   "glmnet",
                   "Hmisc",
                   "igraph",
                   "lme4",
                   "lubridate",
                   "plyr",
                   "RCurl",
                   "reshape",
                   "RJSONIO",
                   "scales",
                   "tm",
                   "XML")

cat("This script will now attempt to install all of the R packages used in 'Machine Learning for Hackers'")

for(p in cran.packages) {
# if package is not available, install it
    if(!suppressWarnings(require(p, character.only = TRUE, quietly = TRUE))) {
        cat(paste(p, "missing, will attempt to install\n"))
        install.packages(p, dependencies = TRUE, type = "source")
    }
    else {
        cat(paste(p, "installed OK\n"))
    }
}

print("### All required packages installed ###")
