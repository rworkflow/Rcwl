# Bioinformatics tools and pipelines using R and CWL

# Instructors

* Qiang Hu (Qiang.Hu@roswellpark.org)

# Workshop Description

"The Common Workflow Language (CWL) is a specification for describing
analysis workflows and tools in a way that makes them portable and
scalable across a variety of software and hardware environments." The
`Rcwl` and `RcwlPipelines` package is built on top of the Common
Workflow Language (CWL), and it provides a simple and easy way to wrap
command line tools and build data analysis pipelines in R using
CWL. In this workshop we will introduce how to wrap Bioinformatics
tools and build a pipeline for real data analysis using R and CWL.

## Pre-requisites

* Basic knowledge of R syntax
* Basic knowledge of CWL
* Familiarity with DNA/RNA sequencing data
* Familiarity with basic tools for NGS data

Useful background reading

* <https://www.commonwl.org/user_guide/>
* <https://hubentu.github.io/others/Rcwl>

## Workshop Participation

Students will be able to build their own pipelines to analyze NGS
data.

## _R_ / _Bioconductor_ packages used

* Rcwl
* RcwlPipelines
* BiocParallel
* yaml
* shiny

## Time outline

| Activity                     | Time |
|------------------------------|------|
| Introduction to CWL and Rcwl | 30m  |
| Build tools and pipeline     | 45m  |
| Run case study               | 45m  |

# Workshop goals and objectives

## Learning goals

* Understand basic syntax of CWL
* Understand how to run tools with `Rcwl`
* Understand how to wrap tools with `Rcwl`
* Practice to build tools with existing resources

## Learning objectives

* Create a basic `echo` tool
* Run Rcwl tools in different environments
* Build a simple DNASeq pipeline
* Analyze RNASeq data with `rnaseq_Sf` pipeline
