library(tidyverse)

set.seed(1)
t1 <- tibble(
  col1 = rep(c("A1", "B1"), each = 300),
  col2 = rep(rep(c("X1", "Y1", "Z1"), each = 100), 2),
  data = rnorm(600)
)

set.seed(2)
t2 <- tibble(
  col1 = rep(c("A2", "B2"), each = 300),
  col2 = rep(rep(c("X2", "Y2", "Z2"), each = 100), 2),
  data = rnorm(600)
)

ggplot(t1, aes(x = data)) +
  geom_histogram() +
  facet_grid(
    rows = vars(col1),
    cols = vars(col2),
    scales = "free_y"
  )

ggplot(t2, aes(x = data)) +
  geom_histogram() +
  facet_grid(
    rows = vars(col1),
    cols = vars(col2),
    scales = "free_y"
  )

write_tsv(t1, "data1.tsv")

write_tsv(t2, "data2.tsv")
