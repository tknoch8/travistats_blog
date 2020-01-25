mod <- mtcars %>% 
  lm(mpg ~., data = .)

fit_vals <- mtcars %>% 
  lm(mpg~., data = .) %>% 
  pluck(fitted.values)

stud_res <- mtcars %>% 
  lm(mpg ~., data = .) %>% 
  studres()

mtcars %>% 
  ggplot(aes(fit_vals, stud_res)) +
  geom_point() +
  geom_smooth(method = "loess")
