# The diagnostic call is deliberately prohibited in submitted code
values <- 1:5
learncrimemapping::check_code()

# Preflight should prevent all execution, including this assignment
later_value <- 100
