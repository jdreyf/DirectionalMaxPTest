testthat::test_that("check_tab", {
  expect_true(check_tab(tab.tmp, num.cols = 1:4))
  
  df <- data.frame(a=1:3, c=4:6)
  df$b <- list(1:1, 1:2, 1:3)
  expect_error(check_tab(df, num.cols = 1:3))
  
  tab.lst <- data.frame(
    column1 = c(1, 2, 3),
    column2 = c("a", "b", "c"),
    column3 = list(c(10, 20, 60)),
    column4 = list(100, 200, 300)
  )
  
  expect_error(check_tab(tab.lst, num.cols = 1:4))
  # unfortunately, it doesn't catch a list column after data.frame()
  expect_true(check_tab(tab.lst, num.cols = c(1, 3:4)))

  # works, but don't want tibble dependency  
  # my_tibble <- tibble(
  #   Name = c("Alice", "Bob", "Charlie", "David"),
  #   Age = c(25, 30, 22, 27),
  #   City = c("New York", "San Francisco", "Los Angeles", "Chicago"),
  #   Salary = c(50000, 60000, 45000, 55000)
  # )
  # check_tab(my_tibble, num.cols = c(2, 4))
})
