install.packages("usethis")
library(usethis)
use_git()
use_github(protocol = "https", private = FALSE)
browse_github_token()
edit_r_environ()
## install if needed (do this exactly once):
## install.packages("usethis")

library(usethis)
use_git_config(user.name = "vibhuk10", user.email = "vibhuk10@gmail.com")


mtcars
library(tidyverse)
mtcars %>% rownames_to_column() %>% as_tibble()
