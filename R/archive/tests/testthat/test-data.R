test_that("numeric_data", {
  expect_equal(class(numeric_data(iris, out='data.frame')), 'data.frame')
  expect_equal(class(numeric_data(iris, out='matrix')), c('matrix', 'array'))
  expect_equal(class(numeric_data(iris, out='vector')), 'numeric')  
  expect_equal(class(numeric_data(as.matrix(iris), out='data.frame')), 'data.frame')
  expect_equal(class(numeric_data(as.matrix(iris), out='matrix')), c('matrix', 'array'))
  expect_equal(class(numeric_data(as.matrix(iris), out='vector')), 'numeric')  
  expect_equal(class(numeric_data(iris$Species, out='data.frame')), 'data.frame')
  expect_equal(class(numeric_data(iris$Species, out='matrix')), c('matrix', 'array'))
  expect_equal(class(numeric_data(iris$Species, out='vector')), 'numeric')  
})

test_that("character_data", {
  expect_equal(class(character_data(iris, out='data.frame')), 'data.frame')
  expect_equal(class(character_data(iris, out='matrix')), c('matrix', 'array'))
  expect_equal(class(character_data(iris, out='vector')), 'character')  
  expect_equal(class(character_data(as.matrix(iris), out='data.frame')), 'data.frame')
  expect_equal(class(character_data(as.matrix(iris), out='matrix')), c('matrix', 'array'))
  expect_equal(class(character_data(as.matrix(iris), out='vector')), 'character')  
  expect_equal(class(character_data(iris$Species, out='data.frame')), 'data.frame')
  expect_equal(class(character_data(iris$Species, out='matrix')), c('matrix', 'array'))
  expect_equal(class(character_data(iris$Species, out='vector')), 'character')  
})

test_that("group_data", {
  expect_equal(class(group_data(iris, out='data.frame')), 'data.frame')
  expect_equal(class(group_data(iris, out='matrix')), c('matrix', 'array'))
  expect_equal(class(group_data(iris, out='vector')), 'character')  
  expect_equal(class(group_data(as.matrix(iris), out='data.frame')), 'data.frame')
  expect_equal(class(group_data(as.matrix(iris), out='matrix')), c('matrix', 'array'))
  expect_equal(class(group_data(as.matrix(iris), out='vector')), 'character')  
  expect_equal(class(group_data(iris$Species, out='data.frame')), 'data.frame')
  expect_equal(class(group_data(iris$Species, out='matrix')), c('matrix', 'array'))
  expect_equal(class(group_data(iris$Species, out='vector')), 'character')  
})

test_that("color_data", {
  expect_equal(class(color_data(iris)), 'character')  
  expect_equal(class(color_data(as.matrix(iris))), 'character')  
  expect_equal(class(color_data(iris$Species)), 'character')  
})
