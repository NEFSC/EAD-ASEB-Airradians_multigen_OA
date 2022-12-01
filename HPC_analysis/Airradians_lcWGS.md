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

# count the number of sequences per file  - for fastq file the accession ID of each read starts with @, grep does this well + fast!
grep -c '@' ../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.fastq.gz > Airradians_lcWGS/F1/output/md5checksum/Oct2022$

echo " grep '@' raw fastq read counts complete!" $(date)

```

run the script 
```
sbatch md5checksum.sh
```

- print the digital fingerprint of the md5sum script output *Oct2022_check.md5*

cat Oct2022_check.md5 | head -2
```
9b222e2ad82096e20efdae837af55591  ../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/101_R1_001.fastq.gz
21accf228ec279d62b1417e810402110  ../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/101_R2_001.fastq.gz
```
use *awk* to cut a mutliple character delimiter and echo to print 

cat Oct2022_check.md5 | awk -F[../] '{print $1 $11}'  > checksummary.txt
```
9b222e2ad82096e20efdae837af55591  101_R1_001
21accf228ec279d62b1417e810402110  101_R2_001
8165b1cbf30c6e0d473dd92d476f02b1  103_R1_001
42691c8282ef8cdda6c1d0142ecb3488  103_R2_001
f2a1da51d2b5040ffac65e18e01c876c  104_R1_001
a90aaf9a879451b576025a4df64c18fe  104_R2_001
cbf7085e3e08f82e90f6a2049c312dd9  153_R1_001
1082f16ab44f20cf136ca478377ced27  153_R2_001
f4ef92166b83346fed65e6f418df1f7c  154_R1_001
63758272cbed1b1aaff5115f23666aa4  154_R2_001
4aaf29326e8ee3827c60a37a325186aa  155_R1_001
b04f866ca6600a08d116940dc18f9fb3  155_R2_001
89550daafe9e4c0856b0ff895a13ef7b  201_R1_001
2d5fb0ddf5a67394bcdb6f1dc48e55d5  201_R2_001
a7c1d45273b1b059effe85934655fbd3  203_R1_001
62df2ec4f6806d4758102f3426f8b371  203_R2_001
9bdc0fa2524c15bdea2978cadac0031e  204_R1_001
aefebf16610c1889886c7299cdb3b1ad  204_R2_001
7b3cd7b104f0cb63b1be5e933d471047  251_R1_001
105a4c1ec9e7d9c98cecb40ea90ed45b  251_R2_001
e8336d0070269ce885eed7be428961d6  253_R1_001
864bb789dbc214d1130539bbe68ece40  253_R2_001
7c2dc7ac72002138f2870e16d35b4f01  254_R1_001
81dfd6447ea06d6edefa5d264e4f5af5  254_R2_001
42e1b7cd5e21e508968cdbd2bc59bff0  301_R1_001
e97f0aae79c05709628757e8b75ca5e2  301_R2_001
a356e5f0b7dc458adfca95f5587c7d8c  303_R1_001
e3b7a9509d820ec3e092c4d3830ad022  303_R2_001
547fce4deb5c811955bf4ff49ea9620f  304_R1_001
dcb9614ec86df811ab647069c42a984c  304_R2_001
4f9886022ad393cb0724058a39b58cc5  351_R1_001
bb2c71fe8def1af27ed1f8deef98dc3f  351_R2_001
386084822e4b259917306fef1d41d397  352_R1_001
35ffc9f615522d1327b2adca13391881  352_R2_001
d732a8ca7f7c4ad55c2c5dc77f6306ef  353dup_R1_001
bbdaae2dc1e2ef43238877dd3eb03462  353dup_R2_001
c2a6c77ef79ee4c2d9632c109fb2d179  353_R1_001
5b9a7df7543f7019f08fcaaf60629b20  353_R2_001
a886be2b8ecce4a313798513b3c0f9a1  3_R1_001
d94352887177a3da9149a2506f0bacb7  3_R2_001
3628bc6e8ee42dc6602f29b3e879810e  4_R1_001
7a89f26826eebd2bf6b3b0f04162e381  4_R2_001
6033884aea9abc4eeb28b87197bd18b8  53_R1_001
da703a8752bca0e5965308e5d53057af  53_R2_001
c43bfd95cdd7501e2218d12d9cb100d9  54_R1_001
2fd418c554c3df68155bb0f8401ab7aa  54_R2_001
729119cc0a8c716d118cf4a21f80de83  55_R1_001
40cb63a52f6fd382a86280a49d9abbe0  55_R2_001
9d2bef38a95a59c4b17eb9d504731143  5_R1_001
d3ab7acfb6c23955471ab47ece590d56  5_R2_001

```

How do these md5 checks compare to the reference? (Genewiz output) *check Oct2022_reference.md5*

cat Oct2022_reference.md5 | awk -F[./] '{print $1 $3}' > refsummary.txt

```
9b222e2ad82096e20efdae837af55591  101_R1_001
21accf228ec279d62b1417e810402110  101_R2_001
8165b1cbf30c6e0d473dd92d476f02b1  103_R1_001
42691c8282ef8cdda6c1d0142ecb3488  103_R2_001
f2a1da51d2b5040ffac65e18e01c876c  104_R1_001
a90aaf9a879451b576025a4df64c18fe  104_R2_001
cbf7085e3e08f82e90f6a2049c312dd9  153_R1_001
1082f16ab44f20cf136ca478377ced27  153_R2_001
f4ef92166b83346fed65e6f418df1f7c  154_R1_001
63758272cbed1b1aaff5115f23666aa4  154_R2_001
4aaf29326e8ee3827c60a37a325186aa  155_R1_001
b04f866ca6600a08d116940dc18f9fb3  155_R2_001
89550daafe9e4c0856b0ff895a13ef7b  201_R1_001
2d5fb0ddf5a67394bcdb6f1dc48e55d5  201_R2_001
a7c1d45273b1b059effe85934655fbd3  203_R1_001
62df2ec4f6806d4758102f3426f8b371  203_R2_001
9bdc0fa2524c15bdea2978cadac0031e  204_R1_001
aefebf16610c1889886c7299cdb3b1ad  204_R2_001
7b3cd7b104f0cb63b1be5e933d471047  251_R1_001
105a4c1ec9e7d9c98cecb40ea90ed45b  251_R2_001
e8336d0070269ce885eed7be428961d6  253_R1_001
864bb789dbc214d1130539bbe68ece40  253_R2_001
7c2dc7ac72002138f2870e16d35b4f01  254_R1_001
81dfd6447ea06d6edefa5d264e4f5af5  254_R2_001
42e1b7cd5e21e508968cdbd2bc59bff0  301_R1_001
e97f0aae79c05709628757e8b75ca5e2  301_R2_001
a356e5f0b7dc458adfca95f5587c7d8c  303_R1_001
e3b7a9509d820ec3e092c4d3830ad022  303_R2_001
547fce4deb5c811955bf4ff49ea9620f  304_R1_001
dcb9614ec86df811ab647069c42a984c  304_R2_001
4f9886022ad393cb0724058a39b58cc5  351_R1_001
bb2c71fe8def1af27ed1f8deef98dc3f  351_R2_001
386084822e4b259917306fef1d41d397  352_R1_001
35ffc9f615522d1327b2adca13391881  352_R2_001
d732a8ca7f7c4ad55c2c5dc77f6306ef  353dup_R1_001
bbdaae2dc1e2ef43238877dd3eb03462  353dup_R2_001
c2a6c77ef79ee4c2d9632c109fb2d179  353_R1_001
5b9a7df7543f7019f08fcaaf60629b20  353_R2_001
a886be2b8ecce4a313798513b3c0f9a1  3_R1_001
d94352887177a3da9149a2506f0bacb7  3_R2_001
3628bc6e8ee42dc6602f29b3e879810e  4_R1_001
7a89f26826eebd2bf6b3b0f04162e381  4_R2_001
6033884aea9abc4eeb28b87197bd18b8  53_R1_001
da703a8752bca0e5965308e5d53057af  53_R2_001
c43bfd95cdd7501e2218d12d9cb100d9  54_R1_001
2fd418c554c3df68155bb0f8401ab7aa  54_R2_001
729119cc0a8c716d118cf4a21f80de83  55_R1_001
40cb63a52f6fd382a86280a49d9abbe0  55_R2_001
9d2bef38a95a59c4b17eb9d504731143  5_R1_001
d3ab7acfb6c23955471ab47ece590d56  5_R2_001


```

- check the read counts 
ano  Oct2022_rawread_count.txt | head -2

```
../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/101_R1_001.fastq.gz:5194197
../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/101_R2_001.fastq.gz:6077857

```

use *awk* to cut a mutliple character delimiter and echo to print 

cat Oct2022_rawread_count.txt | awk -F'Oct2022/' '{print $2}'

```
101_R1_001.fastq.gz:5194197
101_R2_001.fastq.gz:6077857
103_R1_001.fastq.gz:4774413
103_R2_001.fastq.gz:5528382
104_R1_001.fastq.gz:4722932
104_R2_001.fastq.gz:5315996
153_R1_001.fastq.gz:4486204
153_R2_001.fastq.gz:4854951
154_R1_001.fastq.gz:4348240
154_R2_001.fastq.gz:4844429
155_R1_001.fastq.gz:5368641
155_R2_001.fastq.gz:6056000
201_R1_001.fastq.gz:6455581
201_R2_001.fastq.gz:7467221
203_R1_001.fastq.gz:4561695
203_R2_001.fastq.gz:4940046
204_R1_001.fastq.gz:6838830
204_R2_001.fastq.gz:7578940
251_R1_001.fastq.gz:4134299
251_R2_001.fastq.gz:4820032
253_R1_001.fastq.gz:3520110
253_R2_001.fastq.gz:3953096
254_R1_001.fastq.gz:2457466
254_R2_001.fastq.gz:2680292
301_R1_001.fastq.gz:2531469
301_R2_001.fastq.gz:2931756
303_R1_001.fastq.gz:2159676
303_R2_001.fastq.gz:2338376
304_R1_001.fastq.gz:2146083
304_R2_001.fastq.gz:2373869
351_R1_001.fastq.gz:4487960
351_R2_001.fastq.gz:4953422
352_R1_001.fastq.gz:3644657
352_R2_001.fastq.gz:4160578
353dup_R1_001.fastq.gz:3992322
353dup_R2_001.fastq.gz:4546429
353_R1_001.fastq.gz:4001316
353_R2_001.fastq.gz:4572255
3_R1_001.fastq.gz:3433517
3_R2_001.fastq.gz:3705182
4_R1_001.fastq.gz:3740345
4_R2_001.fastq.gz:4222517
53_R1_001.fastq.gz:4015646
53_R2_001.fastq.gz:4385423
54_R1_001.fastq.gz:4575551
54_R2_001.fastq.gz:5036292
55_R1_001.fastq.gz:3781524
55_R2_001.fastq.gz:4210142
5_R1_001.fastq.gz:4643576
5_R2_001.fastq.gz:5090338
```
now use 'comm' -23 to check or any discrepancies 

comm -23 <(sort checksummary.txt | uniq) <(sort refsummary.txt | uniq) # returns uniqe lines in the file checksummary.txt that do not exits in refsummary.txt

* no output == GOOD!
* any output here, the file partially uploaded or is corrupted - reupload the .gez file


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
