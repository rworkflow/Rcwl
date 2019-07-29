
## test echo
input1 <- InputParam(id = "sth")
echo <- cwlParam(baseCommand = "echo", inputs = InputParamList(input1))
echo$sth <- "Hello World!"
r1 <- runCWL(echo)
test_that("simple echo", {
    expect_match(tail(r1$logs, 1), "success")})

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

## InputParam
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

## OutputParam
o1 <- OutputParam(id = "file", type = "File", glob = "*.txt")
test_that("InputParam class", {
    expect_true(validObject(OutputParam()))
})

test_that("OutputParamList class and element type", {
    expect_true(is(echo@outputs, "SimpleList"))
    expect_true(is(echo@outputs, "OutputParamList"))
    expect_true(validObject(OutputParamList()))
    expect_error(OutputParamList(o1, "test"))
    expect_error(OutputParamList(o1, InputParam(id = "test")))
})

## test R function
test_that("R function as command", {
    fun1 <- function(x)x*2
    testFun <- function(a, b){
        cat(fun1(a) + b^2, sep="\n")
    }
    assign("fun1", fun1, envir = .GlobalEnv)
    assign("testFun", testFun, envir = .GlobalEnv)
    p1 <- InputParam(id = "a", type = "int", prefix = "a=", separate = F)
    p2 <- InputParam(id = "b", type = "int", prefix = "b=", separate = F)
    o1 <- OutputParam(id = "o", type = "File", glob = "rout.txt")
    TestFun <- cwlParam(baseCommand = testFun,
                        inputs = InputParamList(p1, p2),
                        outputs = OutputParamList(o1),
                        stdout = "rout.txt")
    TestFun$a <- 1
    TestFun$b <- 2
    r1 <- runCWL(TestFun, Args = "--preserve-entire-environment")
    expect_equal(readLines(r1$output), "6")
})
