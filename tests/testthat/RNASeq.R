setwd("../data/")
## fastqc
f1 <- InputParam(id = "fastqs", type = "File")
o1 <- OutputParam(id = "zfile", type = "File", glob = "*.zip")

fastqc <- cwlParam(baseCommand = "/home/qhu/software/FastQC/fastqc",
                   arguments = list("--outdir", "./"),
                   inputs = InputParamList(f1),
                   outputs = OutputParamList(o1))
fastqc$fastqs <- "../data/SRR1919599_1.fastq.gz"
res1 <- runCWL(fastqc, prefix = "fastqc", stderr = "")

##
f1 <- InputParam(id = "fastqs", type = "File")
f2 <- InputParam(id = "odir", type = "Directory", prefix = "--outdir")
o1 <- OutputParam(id = "zfile", type = "File", glob = "SRR1919599_1_fastqc.zip")

req <- list(
    list(class = "InlineJavascriptRequirement"),
    list(
        class = "InitialWorkDirRequirement",
        listing = list("$(inputs.fastqs)")
    ))
fastqc <- cwlParam(baseCommand = "/home/qhu/software/FastQC/fastqc",
                   requirements = req,
                   inputs = InputParamList(f1, f2),
                   outputs = OutputParamList(o1))
fastqc$fastqs <- "../data/SRR1919599_1.fastq.gz"
fastqc$odir <- "./"
res1 <- runCWL(fastqc, prefix = "fastqc")


## STAR
p1 <- InputParam(id = "genomeDir", type = "Directory", prefix = "--genomeDir")
p2 <- InputParam(id = "sjdbGTFfile", type = "File", prefix = "--sjdbGTFfile")
p3 <- InputParam(id = "outFilterMultimapNmax", type = "int", prefix = "--outFilterMultimapNmax", default = 3)
p4 <- InputParam(id = "outSAMunmapped", type = "string", prefix = "--outSAMunmapped", default = "Within")
p5 <- InputParam(id = "outFilterMismatchNmax", type = "int", prefix = "--outFilterMismatchNmax", default = 2)
p6 <- InputParam(id = "runThreadN", type = "int", prefix = "--runThreadN", default = 6)
p7 <- InputParam(id = "outSAMstrandField", type = "string", prefix = "--outSAMstrandField", default = "intronMotif")
p8 <- InputParam(id = "readFilesCommand", type = "string", prefix = "--readFilesCommand", default = "zcat")
p9 <- InputParam(id = "outSAMtype", type = "string[]", prefix = "--outSAMtype", default = list("BAM","SortedByCoordinate"))
p10 <- InputParam(id = "twopassMode", type = "string", prefix = "--twopassMode", default = "Basic")
p11 <- InputParam(id = "quantMode", type = "string", prefix = "--quantMode", default = "GeneCounts")
##p12 <- InputParam(id = "outFileNamePrefix", type = "Directory", prefix = "--outFileNamePrefix")
p13 <- InputParam(id = "readFilesIn", type = "string[]", prefix = "--readFilesIn")

o1 <- OutputParam(id = "outBAM", type = "File", glob = "*.bam")

star <- cwlParam(baseCommand = "STAR",
                 arguments = list("--outFileNamePrefix", "./"),
                 inputs = InputParamList(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p13),
                 outputs = OutputParamList(o1))

star$genomeDir <- "/rpcc/bioinformatics/reference/STAR/GRCh38_75"
star$sjdbGTFfile <- "/rpcc/bioinformatics/annotation/GENECODE/gencode.v25.annotation.gtf"
star$readFilesIn <- c("/home/qhu/workspace/projects/RPipe/data/SRR1919599_1.fastq.gz",
                      "/home/qhu/workspace/projects/RPipe/data/SRR1919599_2.fastq.gz")
##star$outFileNamePrefix <- "/home/qhu/workspace/projects/RPipe/data/SRR1919599"
res2 <- runCWL(star)
##res2 <- runCWL(star, prefix = "star")

## STAR with default arguments
STAR <- cwlParam(baseCommand = "STAR",
                 arguments = list("--outFileNamePrefix", "./",
                                  "--outFilterMultimapNmax", "3",
                                  "--outSAMunmapped", "Within",
                                  "--outFilterMismatchNmax", "2",
                                  "--runThreadN", "6",
                                  "--outSAMstrandField", "intronMotif",
                                  "--readFilesCommand", "zcat",
                                  "--outSAMtype", "BAM", "SortedByCoordinate",
                                  "--twopassMode", "Basic",
                                  "--quantMode", "GeneCounts"),
                 inputs = InputParamList(p1, p2, p13),
                 outputs = OutputParamList(o1))
STAR$genomeDir <- "/rpcc/bioinformatics/reference/STAR/GRCh38_75"
STAR$sjdbGTFfile <- "/rpcc/bioinformatics/annotation/GENECODE/gencode.v25.annotation.gtf"
STAR$readFilesIn <- c("/home/qhu/workspace/projects/RPipe/data/SRR1919599_1.fastq.gz",
                      "/home/qhu/workspace/projects/RPipe/data/SRR1919599_2.fastq.gz")
Res2 <- runCWL(STAR)


## Index bam
p1 <- InputParam(id = "bam", type = "File")
o1 <- OutputParam(id = "idx", type = "File", glob = "$(inputs.bam.basename)", secondaryFiles = ".bai")
reqs <- list(class = "InitialWorkDirRequirement",
             listing = list("$(inputs.bam)"))
Idx <- cwlParam(baseCommand = c("samtools", "index"),
                requirements = list(reqs),
                inputs = InputParamList(p1),
                outputs = OutputParamList(o1))
Idx$bam <- res2$output
res3 <- runCWL(Idx)
res3 <- runCWL(Idx, prefix = "Idx")

## featureCounts
f1 <- InputParam(id = "gtf", type = "File", prefix = "-a")
f2 <- InputParam(id = "out", type = "string", prefix = "-o")
f3 <- InputParam(id = "bam", type = "File")
o1 <- OutputParam(id = "count", type = "File", glob = "*.txt")
fcount <- cwlParam(baseCommand = "/mnt/lustre/users/qhu/software/subread-1.6.2-source/bin/featureCounts",
                   arguments = list("-o", "count.txt"),
                   inputs = InputParamList(f1, f3),
                   outputs = OutputParamList(o1))
fcount$gtf <- "/rpcc/bioinformatics/annotation/GENECODE/gencode.v25.annotation.gtf"
fcount$bam <- "../data/Aligned.sortedByCoord.out.bam"
res4 <- runCWL(fcount)

## Pipeline
setwd("../data")
I1 <- InputParam(id = "genomeDir", type = "Directory")
I2 <- InputParam(id = "sjdbGTFfile", type = "File")
I3 <- InputParam(id = "readFilesIn", type = "string[]")
O1 <- OutputParam(id = "outBAM", type = "File", outputSource = "star/outBAM")
O2 <- OutputParam(id = "count", type = "File", outputSource = "fcount/count")

rnaseq <- cwlStepParam(inputs = InputParamList(I1, I2, I3),
                       outputs = OutputParamList(O1, O2))
s1 <- Step(id = "star", run = STAR,
           In = list(genomeDir = "genomeDir",
                     sjdbGTFfile = "sjdbGTFfile",
                     readFilesIn = "readFilesIn"))
s2 <- Step(id = "fcount", run = fcount,
           In = list(gtf = "sjdbGTFfile",
                     bam = "star/outBAM"))
rnaseq <- rnaseq + s1 + s2
rnaseq$genomeDir <- "/rpcc/bioinformatics/reference/STAR/GRCh38_75"
rnaseq$sjdbGTFfile <- "/rpcc/bioinformatics/annotation/GENECODE/gencode.v25.annotation.gtf"
rnaseq$readFilesIn <- c("/home/qhu/workspace/projects/RPipe/data/SRR1919599_1.fastq.gz",
                      "/home/qhu/workspace/projects/RPipe/data/SRR1919599_2.fastq.gz")

res5 <- runCWL(rnaseq, prefix = "rnaseq", stderr = "")
