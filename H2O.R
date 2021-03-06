# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("statmod","RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/2/R")

library(h2o)
h2o.cluster <- h2o.init()

h2o.clusterInfo()

#Demo h2o.glm
demo(h2o.glm)

#Download Loan Data and Import thru H2O
warnings()

class(loan.data)
h2o.summary(loan.data)

mtcars.data <- as.h2o(mtcars)

h2o.dim(loan.data)

h2o.colnames(loan.data)

#Histogram of loan amount
h2o.hist(loan.data$loan_amnt)

h2o.group_by(loan.data,by = "home_ownership",mean("loan_amount"))

head(loan.data)

