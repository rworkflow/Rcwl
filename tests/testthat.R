library(testthat)
library(RPipe)
library(batchtools)

test_check("RPipe")

a <- InputParam(id = "a", prefix = "-i", value = 1)
b <- InputParam(id = "b", value = "b")
c1 <- cwlParam(inputs = InputParamList(a, b))

a <- InputParam(id = "a", type = "File", prefix = "-i=", label = "test", separate = FALSE)
b <- InputParam(id = "b", type = "int", prefix = "-b")
c2 <- cwlParam(baseCommand = "echo", inputs = InputParamList(a, b))
c2$a <- "../cwl/test"
c2$b <- 1

writeCWL(c2, "../cwl/test")
testrun <- runOSCommand("/home/qhu/.local/bin/cwl-runner", "../cwl/test.cwl ../cwl/test.yml")
testrun <- system2("/home/qhu/.local/bin/cwl-runner", "../cwl/test.cwl ../cwl/test.yml")

r1 <- runCWL(c2)
