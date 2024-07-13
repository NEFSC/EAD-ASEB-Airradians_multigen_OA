# Argopecten irradiains lcWGS Bioinformatics!

**NOTE** the following script chunks were run in tandem using 
Snakemake, though here the input syntax and outputs are written in general terms for clarity 
You can alternatively cater these tasks to a job-byjob workflow using SLURM, whatever floats your boat!

## <span style="color:blue">**Table of Contents**</span>
  - [Upon upload to HPC...](#Initial-diagnostics-upon-sequence-upload-to-HPC)
      - [Digital fingerprint md5sums](#run-checksum)
      - script: <span style="color:green">**md5_checksum.sh**<span>
  - [1. MultiQC: Initial QC](#Quality-check-raw-reads)
      - script: <span style="color:green">**mutliqc.sh**<span>
  - [2. Trim and QC clean reads](#Trim-and-quality-check-clean-reads)
	    - [about](#What-this-script-does)
      - script: <span style="color:green">**fastp_mutliqc.sh**<span>
  - [3. Alignment of cleaned reads to reference](#Alignment-of-cleaned-reads-to-reference)
      - [about](#HISAT2-alignment)
      - script: <span style="color:green">**HISAT2.sh**<span>
	- [Upload reference genome](#Reference-genome-upload-to-HPC)
	- [HISAT2 - about/commands](#HISAT2-alignment)
	- [samtools - about/commands](#samtools)
	- [HISAT2: Index reference and alignment](#HPC-Job-HISAT2-Index-Reference-and-Alignment) (HPC script; <span style="color:green">**HISAT2.sh**<span>)

# Initial diagnostics upon sequence upload to HPC
--------------------------------------------

## Assemble cumulative md5 reference

* The sequencing vedor (GEnewiz, Azenta, etc.) provides one .md5 for each of the .gz raw sequence files

* Q: What is an md5? 

	- A: This is the digital barcode

* Q: Why do we care? 

	- A: A digital barcode will differ if corrupted, incompletely exported/downloade, etc. Since the data 
	is now crossinghands from the sequencing vendor - to me - then to the high-performance computing cluster, this is 
	a standard sanity check prior to working with these data. 


*Note:* You can concatenate all md5 files to one using 'cat *.md5 > new_file.md5'

```
cat *.md5 > new_file.md5
```

### First, gather the digital fingerprint of all the files you pushed to the server! 

**Objective** output a cumulative .txt file with the digital fingerprints of the uploaded .gz files

### run checksum

### shell script: <span style="color:green">**md5checksum.sh**<span>

* general description of the task: 

```
 md5sum <input directory>/*.gz > <output folder>/check.md5 
```

* how to mark this in a for loop (using double brackets {{}} for snakemake) 

```
cat ./md5checksum/${{m}}/${{m}}_check.md5
```

* general description of the task: 

```
 md5sum <input directory>/*.gz > <output folder>/check.md5 
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


cat April2023_check.md5| head -2
```
d00b958fe8ff3f0a7308a1cd455790c7  ../../../share/nefsc/mcfarland_sequecenes/lcWGS_scallops_April2023/F0-B10_R1_001.fastq.gz
58f4d2c3af4ba150cb024181bcaaf1ec  ../../../share/nefsc/mcfarland_sequecenes/lcWGS_scallops_April2023/F0-B10_R2_001.fastq.gz
```

cat April2023_check.md5 | awk -F "../" '{print $1 $8}' > April2023_check_summary.txt # use "" in -F to have mutliple delimiters
cat April2023_check_summary.txt| head -2
```
d00b958fe8ff3f0a7308a1cd455790c7  F0-B10_R1_001.fastq.gz
58f4d2c3af4ba150cb024181bcaaf1ec  F0-B10_R2_001.fastq.
```

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

cat Oct2022_rawread_count.txt | head -2


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


# Quality check raw reads
-------------------------------------------


# shell script: <span style="color:green">**multiQC_raw.sh**<span>


**Run the sbatch**

```
sbatch multiQC_raw.sh
```

```
#!/bin/bash
#SBATCH --job-name="multiQC_raw_reads"
#SBATCH -t 002:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/F1/output/fastp_multiQC/raw/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/F1/output/fastp_multiQC/raw/"%x_err.%j"

# load modules needed
module load bio/fastp/0.23.2
module load bio/fastqc/0.11.9

# NOTE! before running this job...
#  mkdir a directory/directories output/fastp_multiQC/raw in the lcWGS/F1  dir

cd # can run from anywhere - initially navigaes to home dir
cd ./Airradians_lcWGS/F1/output/fastp_multiQC/ # then navs to the output dir to simplify output files

# symbolically link clean reads to fastp_multiQC dir
ln -s ../../../../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.gz  ./ # call backward from the directory to the share folder, input symbolic links to all .gz to folder
# hashed out if the symbolic links are already present

echo "symbolic links in output/fastp_multiQC folder SUCCESS!"

# Make an array of sequences to trim
array1=($(ls *.fastq.gz))  # call all symbolically linked .fastq.gz files now linkined symbolically in the  output/fastp_multiQC folder

# fastqc loop of raw reads - output fastqc files to raw folder
for i in ${array1[@]}; do
        fastqc ${i} --outdir ./raw # output in the raw dir
done

echo "QC of raw reads complete." $(date)

# Quality Assessment of Raw reads

source ../../../../python_venv/bin/activate # from the current directory, activates the bin of installed python packages, including multiqc

multiqc ./raw #Compile MultiQC report from FastQC files - output .html in raw directory ( multiQC html report)

echo "Raw MultiQC report generated." $(date)
```




**Export multiqc report**

*exit sedna and run from terminal*

- save to gitrepo as multiqc_raw.html

- here is our run from Oct 2022

```
scp sgurr@sedna.nwfsc2.noaa.gov:/Airradians_lcWGS/F1/output/fastp_multiQC/raw/multiqc_report.html C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/HPC_analysis/output/Oct2022_lcWGS_QCreport/multiqc_raw.html

```

- here is out run for April 2023
```
scp sgurr@sedna.nwfsc2.noaa.gov:/Airradians_lcWGS/F0_F2_April2023/output/fastp_multiQC/multiqc_report.html C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/HPC_analysis/output/April2023_lcWGS_QCreport/multiqc_raw.html

```

### IMPORTANT! Quality check multiqc.html before you proceed!


- view the multiqc.html report and note observations to guide trimming!

  - **Per Sequence GC Content**: double peak of ~0-10% GC and 30-40% GC

  - **Mean Quality Scores*&: >22-2; **To do:** *trim base calls < 30*

  - **Per Base Sequence Content**: 15-25% C and G, 25-28% T, 35-40+% A

  - **Adapter Content**: high adapters present, 1-6% of sequences; **To do:** *trim adapter sequence*


# Trim and quality check clean reads
-------------------------------------------

### What this script does
- ``` --adapter_sequence ``` =
	- trim adapter sequence ```AGATCGGAAGAGCACACGTCTGAACTCCAGTCA```
	- common single-end adapter in Illumina. You can run a test on a fastq.gz to count
- ```multiqc ./``` = outputs mutliqc report of the 'clean' reads in the current directory


# shell script: <span style="color:green">**fastp_multiQC_adapters_only.sh**<span>

```
#!/bin/bash
#SBATCH --job-name="fastp_multiQC_adapters"
#SBATCH -t 048:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/F1/output/fastp_multiQC/adapter_trim/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/F1/output/fastp_multiQC/adapter_trim/"%x_err.%j"

# load modules needed
module load bio/fastp/0.23.2
module load bio/fastqc/0.11.9

# NOTE! before running this job...
#  mkdir a directory/directories output/fastp_multiQC/adapter_trim in the lcWGS/F1  dir

cd # can run from anywhere - initially navigaes to home dir
cd ./Airradians_lcWGS/F1/output/fastp_multiQC/ # then navs to the output dir to simplify output files

# symbolically link clean reads to fastp_multiQC dir
# ln -s ../../../../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.gz  ./ # call backward from the directory to the share folder, input symbolic links to all .gz to folder
# hashed out if the symbolic links are already present (i.e. links here if you already ran the multiQC_raw.sh job)

# echo "symbolic links in output/fastp_multiQC folder SUCCESS!"

# Make an array of sequences to trim
array1=($(ls *.fastq.gz))  # call all symbolically linked .fastq.gz files now linkined symbolically in the  output/fastp_multiQC folder

# fastp loop; trim the Read 1 TruSeq adapter sequence; trim poly x default 10 (to trim polyA)
for i in ${array1[@]}; do
        fastp --in1 ${i} --out1 ./adapter_trim/adapter_trim.${i} --adapter_sequence=CTGTCTCTTATACACATCT --adapter_fasta adapter.fasta # --detect_adapter_for_pe  # CTGTCTCTTATACACATCT # check JUST ad$
        fastqc  ./adapter_trim/adapter_trim.${i} --outdir ./adapter_trim # call the output files from fastp in previous line and output fastqc in the same folder with adapter_trim filename head
done

# https://support.illumina.com/bulletins/2016/12/what-sequences-do-i-use-for-adapter-trimming.html - check out the adapters to choose for trimming (Nextera XT used above)

echo "Read trimming of adapters complete." $(date)

# Quality Assessment of Trimmed Reads

source ../../../../python_venv/bin/activate # from the current directory, activates the bin of installed python packages, including multiqc

multiqc ./adapter_trim  -o ./adapter_trim #Compile MultiQC report from FastQC files - output .html in adpater_trim directory ( fast_muiltiQC folder)

echo "Cleaned MultiQC report generated." $(date)


```

### EXPORT MUTLIQC REPORT

*exit SEDNA and run from terminal*

- save to gitrepo as multiqc_adapter_trim.html

```
scp sgurr@sedna.nwfsc2.noaa.gov:Airradians_lcWGS/F1/output/fastp_multiQC/adapter_trim/multiqc_report.html C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/HPC_analysis/output/F1_lcWGS_25samples/multiqc_adapter_trim.html

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
In our case here, we will use HISAT2 to **(1)** index our *A. irradians* reference genome **(2)** align our clean lcWGS reads to the indexed reference genome.

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
- Argopecten_irradians_irradians_genome.fasta *= reference genome*
- adapter_trim/*.fastq.gz *= all trimmed lcWGS reads*

**ouput**
- Airradians_ref *= indexed reference by hisat2-build; stored in the output/hisat2 folder as 1.hy2, 2.ht2... 8.ht2*
- <adapter_trim.fasta>.sam *=hisat2 output, readable text file; removed at the end of the script*
- <adapter_trim.fasta>.bam *=converted binary file complementary to the hisat sam files*

# shell script: <span style="color:green">**HISAT2.sh**<span>

```
#!/bin/bash
#SBATCH --job-name="hisat2_align_Airradians"
#SBATCH -t 072:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/F1/output/alignment/hisat2/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/F1/output/alignment/hisat2/"%x_err.%j"

# before running..
# mkdir(s) as Airradians_lcWGS/F1/output/alignment/hisat2/

BASEDIR=~/ # base directory
REFDIR=~/refs # the reference folder
PYTHONENV=~/python_venv/bin # python enrnvrionment
DATDIR=~/Airradians_lcWGS/F1/output/fastp_multiQC/adapter_trim/ # directory of trimmed and filtered fastq.gz files
OUTDIR=~/Airradians_lcWGS/F1/output/alignment/hisat2/


# nav to hd
cd ~ #nav back to home directory (allows job to be run from anywhere)

# load modules, requires hisat2 and samtools
module load bio/hisat2/2.2.1
module load bio/samtools/1.11


# symbolically link clean reads to hisat2 dir
ln -s $DATDIR/*.fastq.gz $OUTDIR/ # call the .fastq.gz output from fastp trim - make symb link to output/hisat2
echo "Symbolic directories successfully linked"

# activate python for hisat2-build
source $PYTHONENV/activate  # activate python virtual envriomment to call python and run hisat2-build
#echo "Python virtual env activated"

# index the reference genome for Panopea generosa output index to working directory
hisat2-build -f $REFDIR/Argopecten_irradians_irradians_genome.fasta $OUTDIR/Airradians_ref
echo "Referece genome indexed. Starting alingment" $(date)

# exit python virtual envrionment
deactivate

# This script exports alignments as bam files
# sorts the bam file because Stringtie takes a sorted file for input (--dta)
# removes the sam file because it is no longer needed
array=($(ls $OUTDIR/*.fastq.gz)) # call the symbolically linked sequences - make an array to align
for i in ${array[@]}; do
        hisat2 -p 8 --dta -x $OUTDIR/Airradians_ref -U ${i} -S ${i}.sam
        samtools sort -@ 8 -o ${i}.bam ${i}.sam
                echo "${i} bam-ified!"
        rm ${i}.sam
done
```

- HISAT2 complete!


# ANGSD: Genotype Likelihood
-------------------------------------------

ANGSD wikipedia-like site for all call  definitions and tutorial items here: http://www.popgen.dk/angsd/index.php/Main_Page

## About some core ANGSD calls...

```-GL``` =	1: SAMtools model;	2: GATK model

**quality filtering**

```-minQ``` = minimum phred quality score

```-minMapQ``` = minimum mapping quality score

* note: studies use a minQ of 30-32 depending on how consersative you want your base calls

```-minInd``` = the minimum representation of the variant in your data.

* note: contemporary studies use a threshold so >80% of individuals are represented

* thoughts... if you have 100 from population A and 100 from population B, should you:
	- (a) GLs as -minInd 160 for all 200 samples
	- (b) two GLs separately for each population with -minQ 80 for 100 samples

**pvalues**

```-snp_pval``` = SNP p value

* angsd tutorials set this quite low to 1X10-6 (0.000001), however studies do set this to simply 0.05 also

```-sb_pvalue``` = strand bias

```-hetbias_pval``` = hetbias p value, based on reads of genotypes that are called to be heterozygotes - requires the -doGeno option

```-anc``` = ancetral state, if you do not have this you can use the assembly that you have maped against but remember to add -fold 1 n readSFS and real SFS sf2theta step

```-doMaf``` =

*	1: Known major, and Known minor.
		   Here both the major and minor allele is assumed to be known (inferred or given by user).
		   The allele frequency is the obtained using based on the genotype likelihoods.
		   The allele frequency estimator from genotype likelihoods are from this publication but using the EM algorithm and is briefly described here.

* 2: Known major, Unknown minor.Here the major allele is assumed to be known (inferred or given by user) however the minor allele is not determined. Instead we sum over the 3 possible minor alleles weighted by their probabilities.

*	4: frequency based on genotype posterior probabilities.If genotype probabilities are used as input to ANGSD the allele frequency is estimated directly on these by summing over the probabitlies.

*	8: frequency based on base counts.This method does not rely on genotype likelihood or probabilities but instead infers the allele frequency directly on the base counts.


  * example: doMaf 1 outputs 'knownEM' as an allele frequency value based on geneotype liklihoods of the minor and manjor allele assumed as known

```-doMajorMinor``` =

*	1: Infer major and minor from GL

*	2: Infer major and minor from allele counts

*	3: use major and minor from a file (requires -sites file.txt)

*	4: Use reference allele as major (requires -ref)

*	5: Use ancestral allele as major (requires -anc)


```-P``` =

	*	number of threads that you are running your GL

```-ref``` =

*	if emplying -doMajorMinor 4, if forces the major allele according to the reference, you will need to define this by a fasta (___.fa) file


Before getting started..

we first need a file containg the root to all bam files from home dir - below as 'bam_filelist.txt'

```
ls Airradians_lcWGS/F1/output/alignment/hisat2/*gz.bam > Airradians_lcWGS/F1/output/alignment/hisat2/bam_filelist.txt
```

Now we can run a test script for the angsd call for bam file input


# shell script: <span style="color:green">**angsd_GL.sh**<span>

```
#!/bin/bash
#SBATCH --job-name="angsd_GL"
#SBATCH -t 500:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=64GB
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/F1/output/angsd/GL/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/F1/output/angsd/GL/"%x_err.%j"

# before running..
# mkdir(s) as Airradians_lcWGS/F1/output/alignment/angsd/GL

BASEDIR=~/ # base directory
DATDIR=~/Airradians_lcWGS/F1/output/alignment/hisat2 # directory of trimmed and filtered fastq.gz files
OUTDIR=~/Airradians_lcWGS/F1/output/angsd/GL/

# nav to hd
BASEDIR #nav back to home directory (allows job to be run from anywhere)

# load modules, requires hisat2 and samtools
module load bio/angsd/0.933

# run angsd GL
angsd -out $OUTDIR/angsd_mafs_SAMtools.gz -bam $DATDIR/bam_filelist.txt -GL 1 -doMaf 2 -doMajorMinor 1 -P 5

```

### Let's look at the output file while the job runs...

zcat angsd_mafs_SAMtools.gz.mafs.gz | head -20

```
chromo  position        major   minor   unknownEM       nInd
Contig0 1       C       A       0.000001        38
Contig0 2       A       C       0.000000        38
Contig0 3       T       A       0.000000        38
Contig0 4       G       A       0.000000        39
Contig0 5       A       C       0.000000        39
Contig0 6       G       A       0.000000        41
Contig0 7       A       C       0.000000        41
Contig0 8       A       G       0.076930        41
Contig0 9       T       C       0.000000        42
Contig0 10      A       C       0.000000        41
Contig0 11      G       A       0.000000        45
Contig0 12      C       A       0.000000        45
Contig0 13      T       A       0.000000        45
Contig0 14      G       T       0.000001        45
Contig0 15      T       A       0.000000        45
Contig0 16      A       G       0.000001        45
Contig0 17      T       A       0.000000        45
Contig0 18      G       A       0.261971        45
Contig0 19      G       A       0.000000        45
```

### How to read this output?

* we have 6 total columns as..

	*  'chromo' == the chromosome number

	* 'position' == bp number/position within each chromosome

	* 'major' == major allele at the noted position

	* 'minor' == minor allele at the noted position

	* 'unknownEM' == the minor allele frequency

	* 'nInd' == the number of individuals for which there is coverage at each SNP

* at Contig0 position 18 we have a high frequency of the minor allele 'A' exhibited by 45 individuals, but wait.... how is this possible if we only have 25 ini

zcat angsd_mafs_SAMtools.gz.mafs.gz | tail -20

```
Contig52        675542  T       A       0.000003        21
Contig52        675543  A       G       0.037912        21
Contig52        675544  A       C       0.000003        21
Contig52        675545  C       A       0.000003        22
Contig52        675546  T       A       0.000005        22
Contig52        675547  T       A       0.000005        19
Contig52        675548  T       A       0.000005        20
Contig52        675549  A       C       0.000003        14
Contig52        675550  G       A       0.000004        15
Contig52        675551  A       C       0.000004        13
Contig52        675552  A       G       0.000006        12
Contig52        675553  G       A       0.000003        9
Contig52        675554  G       A       0.000005        7
Contig52        675555  C       A       0.000006        8
Contig52        675556  A       C       0.000006        8
Contig52        675557  C       A       0.000006        8
Contig52        675558  A       C       0.000006        8
Contig52        675559  A       C       0.000006        8
```

* **NOTE:** are the regions with SNP positions of high or poor mapping quality?
are the regions with SNP position of excessive coverage? You may need to supplement a csv(s) with categorical justification of regions with varied quality or putatively high-repeated regions (excessive coverage), filtering these cumulative SNP hits downstream in R


# Appears we need to merge our paired-end reads!!



* Let's merge bams files (hisat2 outputs)

### <span style="color:red">IMPORTANT:<span> merge lanes here (.bam stage)


* nav to target dir

```
<navigate the the hisat2 directory with you paired-end .bam outputs>
```

* example file 'adapter_trim.352_R2_001.fastq.gz.bam'

* mkdir for merged bam files

```
mkdir merged_bam_files

cd merged_bam_files
```

* enter interactive mode

```
interactive
```

* load samtools

```
module load bio/samtools/1.15.1
```

*  ID file lists the sample ID characters (i.e. adapter_trim.101) - sample .bam file adapter_trim.101_R1_001.fastq.gz.bam

```
ls *R1_001*.bam | awk -F '[_]' '{print $1"_"$2}' | sort | uniq > ID
```

* output looks like this:

```
../adapter_trim.101
../adapter_trim.103
../adapter_trim.104
../adapter_trim.153
../adapter_trim.154
../adapter_trim.155
../adapter_trim.201
../adapter_trim.203
../adapter_trim.251
../adapter_trim.253
../adapter_trim.254
../adapter_trim.3
../adapter_trim.301
../adapter_trim.303
../adapter_trim.304
../adapter_trim.351
../adapter_trim.352
../adapter_trim.353
../adapter_trim.353dup
../adapter_trim.4
../adapter_trim.5
../adapter_trim.53
../adapter_trim.54
../adapter_trim.55
```

- for loop using ```samtools``` to merge bam files together by sample ID ('ID' created above)
- example of files to merge: adapter_trim.101_R1_001.fastq.gz.bam and adapter_trim.101_R2_001.fastq.gz.bam - where 'R1' and 'R2' are paired-end reads
and the ID called adapter_trim.101 as $i below... (note: all .gz file names end with '001.fastq.gz.bam' no matter the lane)

```
for i in `cat ./ID`;
	do samtools merge $i\.bam $i\_R1_001.fastq.gz.bam $i\_R2_001.fastq.gz.bam;
	done
```

- when run in ```interactive``` mode, this loop takes up to ~1-2 hours

## Call root directory of the merged bam files

* when running **angsd**, we will need to call these merged bam files as a txt list file

* below is an example calling bam file lists for the Low pCO2 and moderately elevated pCO2 F1 cohort - an example with the first 25 samples form October 2022

	- Moderately-elevated pCO2 cohort, 12 animals with the IDs in the 300s and 200s

ls -d "$PWD"/*.35*.bam "$PWD"/*30*.bam "$PWD"/*.2*.bam > Mod_pCO2_bamlist.txt

	- Low pCO2 cohort, 12 animals with the IDs 3,4,5 and in the 100s

ls -d "$PWD"/*.3.bam "$PWD"/*.4.bam "$PWD"/*.5*.bam "$PWD"/*.1*.bam > Low_pCO2_bamlist.txt


## Lets run ANGSD!

* **Objective**: obtain SNP calls. chromosome/contig positions to infer genotype liklihoods and putativelly adaptive molecular candidates (we also have differential expression work!)
