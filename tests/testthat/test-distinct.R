context("distinct")

df <- tibble(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 2, 1, 2)
)
dfs <- test_load(df)

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  dfs %>%
    map(. %>% distinct()) %>%
    expect_equal_tbls(unique(df))
})

test_that("distinct for single column equivalent to local unique (#1937)", {
  dfs %>%
    map(. %>% distinct(x, .keep_all = FALSE)) %>%
    expect_equal_tbls(unique(df["x"]))

  dfs %>%
    map(. %>% distinct(y, .keep_all = FALSE)) %>%
    expect_equal_tbls(unique(df["y"]))
})

test_that("distinct throws error if column is specified and .keep_all is TRUE", {
  mf <- memdb_frame(x = 1:10)
  expect_error(
    mf %>% distinct(x, .keep_all = TRUE) %>% collect(),
    "specified columns.*[.]keep_all"
  )
})

test_that("supports .data pronoun (dbplyr#132)",{
  dfs %>%
    map(. %>% distinct( .data$x, .data$y ) ) %>%
    expect_equal_tbls( df %>% distinct( .data$x, .data$y ) )
})

test_that("selects by RHS of named arguments (dbplyr#132)",{
  dfs %>%
    map(. %>% distinct( v = y ) ) %>%
    expect_equal_tbls( df %>% distinct( v = y ) )
})
