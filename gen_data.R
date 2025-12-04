library(tidyverse)

set.seed(1)
t1 <- tibble(
  t = 0,
  test_name = "test_name",
  test_id = "id_n",
  group = "group_n",
  index = rep(1:10, each = 5),
  sweep_val = rep(1:5, 10),
  sweep_par = "x",
  unit = "v",
  data = rep(1:5 + 2, 10),
) |> mutate(data = data + rnorm(50, sd = 0.05))

set.seed(2)
t2 <- tibble(
  t = 33,
  test_name = "test_name",
  test_id = "id_n",
  group = "group_n",
  index = rep(1:10, each = 5),
  sweep_val = rep(1:5, 10),
  sweep_par = "x",
  unit = "v",
  data = rep(1:5 + 2, 10),
) |> mutate(data = data + rnorm(50, sd = 0.05))

t_data_test1 <- rbind(t1, t2) |>
  mutate(ref = rep(t1$data, 2)) |>
  mutate(delta = data - ref) |>
  mutate(`%` = delta / ref * 100)

t_data_test2 <- t_data_test1 |>
  filter(sweep_val == 1) |>
  mutate(sweep_val = NA, sweep_par = NA)

hist(t_data_test1$`%`)

write_tsv(t_data_test1, "data_test1.tsv")
write_tsv(t_data_test2, "data_test2.tsv")


