if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, survival, ggsurvfit, patchwork)

hazard_f <- function(time, intercept, slope){
  slope * intercept * time ^ (slope - 1)
}
