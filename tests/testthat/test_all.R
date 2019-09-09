
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

## ## ExpressionTool, not pass check in bioc server
## p1 <- InputParam(id = "file1", type = "File")
## p2 <- InputParam(id = "file2", type = "File")
## o1 <- OutputParam(id = "out", type = "Directory")
## expression <- '${
##     return {"out": {
##         "class": "Directory",
##         "basename": "group",
##         "listing": [inputs.file1, inputs.file2]
##     }};
## }'
## req <- list(class = "InlineJavascriptRequirement")
## groupFiles <- cwlParam(cwlClass = "ExpressionTool",
##                        requirements = list(req),
##                        inputs = InputParamList(p1, p2),
##                        outputs = OutputParamList(o1),
##                        expression = expression)
## f1 <- tempfile()
## f2 <- tempfile()
## file.create(f1, f2)
## groupFiles$file1 <- f1
## groupFiles$file2 <- f2
## r1 <- runCWL(groupFiles, outdir = tempdir())
## test_that("ExpressionTool", {
##     expect_true(all(basename(r1$output) %in%
##                     c("group", basename(f1), basename(f2))))
## })

## extensions and metadata
test_that("extensions", {
    ext <- list("$namespaces" = list(
                    s = "https://schema.org/"),
                "s:author" = list(
                    class = "s:Person",
                    "s:name" = "Qiang H"
                ))
    extensions(echo) <- ext
    r1 <- runCWL(echo)
    cwlfile <- unlist(strsplit(r1$logs[2], split="'"))[2]

    expect_match(tail(r1$logs, 1), "success")
    expect_true(all(ext %in% read_yaml(cwlfile)))
})
