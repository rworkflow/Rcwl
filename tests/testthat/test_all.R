
## test echo
input1 <- InputParam(id = "sth")
echo <- cwlParam(baseCommand = "echo", inputs = InputParamList(input1))
echo$sth <- "Hello World!"
r1 <- runCWL(echo)
test_that("simple echo", {
    expect_equal(tail(r1$logs, 1), "Final process status is success")})

out1 <- readLines(r1$output)
test_that("simple echo", {
    expect_equal(out1, "Hello World!")})
