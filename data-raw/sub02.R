library(tidyverse)

sub02 <- fst::read_fst(here::here("data-raw", "sub-02_task-con_area-V1_space-T1w_beta.fst")) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    sub = factor(sub),
    voxel = factor(voxel),
    run = factor(run),
    ses = factor(ses),
    orientation = deg(orientation)) %>%
  select(sub, voxel, contrast, orientation, y)

usethis::use_data(sub02, overwrite = TRUE)
