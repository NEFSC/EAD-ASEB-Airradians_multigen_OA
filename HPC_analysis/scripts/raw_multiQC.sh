#!/bin/bash
#SBATCH --job-name="multiQC_raw_reads"
#SBATCH -t 002:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output="%x_out.%j"
#SBATCH --error="%x_err.%j"

# load modules needed
module load bio/fastp/0.23.2
module load bio/fastqc/0.11.9 

# run sbatch from the sgurr home directory..

cd Airradians_F1s_TagSeq/output/fastp_multiQC # run from the sgurr directory, navigate to the fast_multiQDC folder
mkdir raw # make directory for trimmed fastq files and multiqc report 

# symbolically link clean reads to fastp_multiQC dir
ln -s ../../../../../share/nefsc/mcfarland_sequecenes/TagSeq_F1scallops_2022/*.gz  ./ # call backward from the directory to the share folder, input symbolic link to folder 
# hashed out if the symbolic links are already present

# Make an array of sequences to trim
array1=($(ls *.fastq.gz))  # call the folder will all symbolically linked .fastq.gz files (without the SA* folder included)

# fastqc loop of raw reads - output fastqc files to raw folder 
for i in ${array1[@]}; do
        fastqc ${i} --outdir ./raw
done 

echo "QC of raw reads complete." $(date)

# Quality Assessment of Raw reads

source ../../../python_venv/bin/activate # from the current directory, activates the bin of installed python packages, including multiqc

multiqc ./raw #Compile MultiQC report from FastQC files - output .html in current directory ( multiQC html report)

echo "Raw MultiQC report generated." $(date)

