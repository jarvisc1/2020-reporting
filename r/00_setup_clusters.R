library(parallel)
library(doParallel)

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
foreach(i = 1:3, .combine = 'c') %dopar% {
  sqrt(i)
}


# parallel::stopCluster(cl)