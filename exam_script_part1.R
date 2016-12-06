library(tidyverse)
library(apaTables)

#PART 1, POWER ANALYSIS
library(MBESS)
my.f2 <- .10/(1-.20)
print(my.f2)
#.125

library(pwr)
pwr.f2.test(u=1, f2=0.125, power = .85)
#v=71.7996
N = 1 + 72 + 1
print(N)
#N = 74

#an incremental power analysis was conducted to fine that an N of 74 was needed









