# Argopecten irradiains lcWGS Bioinformatics

## <span style="color:blue">**Table of Contents**</span>
  - [Upon upload to HPC...](#Initial-diagnostics-upon-sequence-upload-to-HPC)
	  - [Upon upload to HPC...](#Initial-diagnostics-upon-sequence-upload-to-HPC)
      - [Digital fingerprint md5sums](#run-checksum) (HPC script; <span style="color:green">**md5_checksum.sh**<span>)
  - [1. MultiQC: Initial QC](#Quality-check-of-raw-reads) (HPC script; <span style="color:green">**mutliqc.sh**<span>)
  - [2. Trimming and QC of 'clean' reads](#Trimming-and-post-trim-quality-check-of-'clean'-reads)
	- [fastp - about/commands](#What-this-script-will-do...)
	- [fastp and MulitiQC: Trim and QC](#shell-script-fastp_multiqc.sh) (HPC script; <span style="color:green">**fastp_mutliqc.sh**<span>)
  - [3. Alignment of cleaned reads to reference](#HISAT2-Alignment-of-cleaned-reads-to-reference)
	- [Upload reference genome](#Reference-genome-upload-to-HPC)
	- [HISAT2 - about/commands](#HISAT2-alignment)
	- [samtools - about/commands](#samtools)
	- [HISAT2: Index reference and alignment](#HPC-Job-HISAT2-Index-Reference-and-Alignment) (HPC script; <span style="color:green">**HISAT2.sh**<span>)

# Initial diagnostics upon sequence upload to HPC
--------------------------------------------

# Assemble cumulative md5 reference
 
	* Genewiz provides a one txt line .md5 file for each of the .gz sequence files 
	
	* first objective to assemble a cumulative file with these text lines in one .txt 

```

```
	


# run checksum

# shell script: <span style="color:green">**md5checksum.sh**<span>

```
#!/bin/bash
#SBATCH --job-name="md5checksum"
#SBATCH -t 002:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/F1/output/md5checksum/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/F1/output/md5checksum/"%x_err.%j"

# before running..
# make directory named output/md5checksum/

cd  # nav to home dir from whereever the script is run

echo "start time stamp" $(date)

# generate md5
md5sum ../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.gz > Airradians_lcWGS/F1/output/md5checksum/Oct2022_check.md5

echo "md5checksum complete!" $(date)

# count the number of sequences per file
zcat ../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.gz  | echo $((`wc -l`/4)) > Airradians_lcWGS/F1/output/md5checksum/Oct2022_rawread_count.txt

echo "zcat raw read counts complete!" $(date)
```

```
sbatch md5checksum.sh
```

- check the digital fingerprint of the files with md5sum
- compare md5sum of our output URI.md5 file to the UT Library Information pdf; okay the upload - move forward


# Quality check of raw reads
-------------------------------------------


# shell script: <span style="color:green">**fastqc_raw.sh**<span>
```
**insert bash script here**
```

**Run the sbatch**

```
sbatch fastqc_raw.sh
```

**Export multiqc report**

*exit bluewaves and run from terminal*
- save to gitrepo as multiqc_clean.html
```
scp samuel_gurr@bluewaves.uri.edu:/data/putnamlab/sgurr/Geoduck_TagSeq/output/fastp_mutliQC/multiqc_report.html  C:/Users/samjg/Documents/My_Projects/Pgenerosa_TagSeq_Metabolomics/TagSeq/HPC_work/Output
```

### IMPORTANT! Quality check multiqc.html before you proceed!

- view the multiqc.html report and note observations to guide trimming!
  - **Per Sequence GC Content**: double peak of ~0-10% GC and 30-40% GC
**To do:** *initial peak due to polyA tail?*

  - **Mean Quality Scores*&: >22-2; **To do:** *trim base calls < 30*

  - **Per Base Sequence Content**: 15-25% C and G, 25-28% T, 35-40+% A

  - **Adapter Content**: high adapters present, 1-6% of sequences; **To do:** *trim adapter sequence*


# Trimming and post-trim quality check of 'clean' reads
-------------------------------------------

### Trimming-polyA-tail
- Remember that TagSeq involves priming from the polyA tail of mRNA sequences! Thus, we will need to
trim mononnucleotide sequence of As using fastp (in addition to threshold quality score and adapter(s)!)


### What this script will do...
- ``` --adapter_sequence ``` =
	- trim adapter sequence ```AGATCGGAAGAGCACACGTCTGAACTCCAGTCA```
	- common single-end adapter in Illumina. You can run a test on a fastq.gz to count
- ``` --adapter_fasta``` = polyA_tail.fasta
	- **create 'polyA_tail.fasta' to call here**
	- important! ``` --adapter_sequence ``` is called by fastp before ``` --adapter_fasta``` and will call each adapter in the .fasta one-by-one
	- the sequence distribution of trimmed adapters can be found in the HTML/JSON report
- ```multiqc ./``` = outputs mutliqc report of the 'clean' reads in the current directory


# shell script: <span style="color:green">**fastp_mutliqc.sh**<span>
```
**insert bash script here**
```

### EXPORT MUTLIQC REPORT
*exit SEDNA and run from terminal*
- save to gitrepo as multiqc_clean.html
```
scp **write call here**
```

# Alignment of cleaned reads to reference
-------------------------------------------

## Reference genome upload to HPC

*exit SEDNA and run from terminal*

```
scp **upload to HPC**
```

-  file name: **file name here**
	- **reference paper**
-  reference genome file size: **size here**
-  uploaded to **location of the reference in the HPC**

## HISAT2 alignment

**About:** HISAT2 is a sensitive alignment program for mapping sequencing reads.
In our case here, we will use HISAT2 to **(1)** index our *P. generosa* reference genome **(2)** align our clean TagSeq reads to the indexed reference genome.

More information on HISAT2 can be read [here](http://daehwankimlab.github.io/hisat2/manual/)!

*Main arguments used below*:

**(1)** *Index the reference genome*

``` hisat2-build ``` =
builds a HISAT2 index from a set of DNA sequences. Outputs 6 files that together consitute the index.
ALL output files are needed to slign reads to the reference genome and the original sequence FASTA file(s)
are no longer used for th HISAT2 alignment

``` -f <reads.fasta> ``` =
the reads (i.e <m1>, <m2>, <m100>)
FASTA files usually have extension .fa, .fasta, .mfa, .fna or similar.
FASTA files do not have a way of specifying quality values, so when -f is set,
the result is as if --ignore-quals is also set

Note: other options for your reads are ```-q ``` for FASTQ files,
 ```--qseq ``` for QSEQ  files, etc. - check [here](http://daehwankimlab.github.io/hisat2/manual/) for more file types

**(2)** *align reads to reference*

``` -x <hisat2-indx> ``` =
base name for the index reference genome, first looks in the current directory then in the directory specified in HISAT_INDEXES environment variable

``` --dta ``` =
 **important!** reports the alignments tailored for *StringTie* assembler.
 With this option, HISAT2 requires longer anchor lengths for de novo discovery of splice sites.
 This leads to fewer alignments with short-anchors, which helps transcript assemblers improve
 significantly in computation and memory usage.

 This is important relative to Cufflinks ```--dta-cufflinks```
 in which HISAT2 looks for novel splice sites with three signals (GT/AG, GC/AG, AT/AC)

``` -U <r> ``` =
Comma-separated list of files contained unparied reads to be aligned.
*In other words...*, our array of post-trimmed 'clean' TagSeq reads

``` -p NTHREADS``` =
Runs on separate processors, increasing -p increases HISAT2's memory footprint, increasing -p from 1 to 8
increased the footprint by a few hundred megabytes

Note: Erin Chile from Putnam Lab ran -p 8 for HISAT2 alignment of *Montipora* [here](https://github.com/echille/Montipora_OA_Development_Timeseries/blob/master/Amil/amil_RNAseq-analysis.sh)

``` -S <hit> ``` =
file to write SAM alignments to. By default, alignments are written to the “standard out” or “stdout” filehandle (i.e. the console).

**Note:** HISAT2 also has several criteria to trim such as the phred score and base count 5' (left) and 3' (right)
Since we already assessed quality and trimmed, we will not use these commands


## samtools
**About:** used to manipuate alignments to a binary BAM format - files contain the spliced reads alignemnts sorted by the reference position
with a tag to indicate the genomic strand that produced the RNA from which the read was sequenced.
samtools quickly extracts alignments overlapping particular genomic regions - outputs allow viewers to quickly display alignments in each genomic region
note: the SAM format output by HISAT2 must be sorted and converted to BAM format using the samtools program

``` sort ``` =
sorts the alignments by the leftmost coordinates

```-o <out.bam``` =
outputs as a specified .bam file

```-@ threads``` =
Set number of sorting and compression threads. By default, operation is single-threaded

more information on samtools commands [here](http://www.htslib.org/doc/1.1/samtools.html)

## HPC Job: HISAT2 Index Reference and Alignment
-----------------------------------------------------------------

- create directory output\hisat2

``` mkdir HISAT2 ```

- index reference and alignment

**input**
- Panopea-generosa-genes.fna *= reference genome*
- clean/*.fastq.gz *= all clean TagSeq reads*

**ouput**
- Pgenerosa_ref *= indexed reference by hisat2-build; stored in the output/hisat2 folder as 1.hy2, 2.ht2... 8.ht2*
- <clean.fasta>.sam *=hisat2 output, readable text file; removed at the end of the script*
- <clean.fasta>.bam *=converted binary file complementary to the hisat sam files*

# shell script: <span style="color:green">**HISAT2.sh**<span>

```
**insert bash script here**
```
- HISAT2 complete with format prepared for StringTie assembler!
