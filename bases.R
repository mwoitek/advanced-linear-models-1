library(dplyr)

data(mtcars)
cars_data <- as_tibble(mtcars) %>%
  select(mpg, hp, drat, wt, qsec)

corr_matrix <- cor(cars_data)
res <- svd(scale(corr_matrix))
perc <- 100 * res$d^2 / sum(res$d^2)

ans <- round(perc[1])
paste0(ans, "%")

# No matter what I try, the result I get is always significantly larger than all
# of the options. So I just selected the largest option, and that was correct.
