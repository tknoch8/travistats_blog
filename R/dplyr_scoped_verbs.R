require(tidyverse)
data(mtcars)
glimpse(mtcars)

test <- mtcars %>% 
  mutate_all(.funs = list(~as.character(.)))

glimpse(test)

test <- mtcars %>% 
  rename_all(.funs = list(~paste0("my_variable_", .)))

glimpse(mtcars)

test <- mtcars %>% 
  mutate_at(vars(starts_with("c")), list(~ ./ 100))

data("mpg")

test <- mpg %>% 
  mutate_if(is.double, list(~ as.integer(.)))

test <- nycflights13::flights %>% 
  select_if(function(col_class) is.double(col_class) || is.integer(col_class))

