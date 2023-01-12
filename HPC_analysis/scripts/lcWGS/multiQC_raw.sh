#!/bin/bash
#SBATCH --job-name="multiQC_raw_reads"
#SBATCH -t 048:00:00
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

multiqc ./raw  -o ./raw #Compile MultiQC report from FastQC files - output .html in raw directory ( multiQC html report)

echo "Raw MultiQC report generated." $(date)

