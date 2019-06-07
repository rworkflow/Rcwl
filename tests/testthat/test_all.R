
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

## no inputBinding
p1 <- InputParam(id = "infiles", type = "File[]")
p2 <- InputParam(id = "outfile", type = "string",
                 default = "catout.txt", position = -1)
Cat <- cwlParam(baseCommand = "cat",
                inputs = InputParamList(p1, p2),
                stdout = "$(inputs.outfile)")
writeCWL(Cat, file.path(tempdir(), "cat"))
Cat1 <- readCWL(file.path(tempdir(), "cat.cwl"))
test_that("negative position", {
    expect_equal(inputs(Cat1)$outfile@inputBinding$position, -1)})

## inputParam
test_that("InputParam class", {
    expect_error(InputParam())
})

test_that("InputParamList class and element type", {
    expect_true(is(echo@inputs, "SimpleList"))
    expect_true(is(echo@inputs, "InputParamList"))
    expect_true(validObject(InputParamList()))
    expect_error(InputParamList(p1, p2, "test"))
    expect_error(InputParamList(p1, p2, OuputParam(id = "test")))
})
