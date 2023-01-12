#!/bin/bash
#SBATCH --job-name="fastp_multiQC_clean"
#SBATCH -t 002:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_F1s_TagSeq/output/fastp_multiQC/clean/"%x_out.%j"
#SBATCH --error=./Airradians_F1s_TagSeq/output/fastp_multiQC/clean/"%x_err.%j"

# before running..
# make directory named clean in the folder output/fastp_multiQC/
# run sbatch from the host home directory

# load modules needed
module load bio/fastp/0.23.2
module load bio/fastqc/0.11.9

cd Airradians_F1s_TagSeq/output/fastp_multiQC # run from the sgurr directory, navigate to the fast_multiQDC folder

# symbolically link clean reads to fastp_multiQC dir
# commented out if the symbolic links are already created
# ln -s ../../../../../share/nefsc/mcfarland_sequecenes/<change to your desired directory for seq data>  ./ # call backward from the directory to the share folder, input symbolic link to folder

# Make an array of sequences to trim
array1=($(ls *.fastq.gz))  # call the folder will all symbolically linked .fastq.gz files (without the SA* folder included)

# fastp loop; trim the Read 1 TruSeq adapter sequence; trim poly x default 10 (to trim polyA)
for i in ${array1[@]}; do
	fastp --in1 ${i} --out1 ./clean/clean.${i} --adapter_sequence=AGATCGGAAGAGCACACGTCTGAACTCCAGTCA --trim_poly_x 6 -q 30 -y -Y 50 # trim poly to remove poly A tail, -q to trim phred score, -y -Y 50 for seq complexity (default 30)
        fastqc  ./clean/clean.${i} --outdir ./clean # calls the  files output by fastp in previous line and output into the same folder
done

echo "Read trimming of adapters complete." $(date)

# Quality Assessment of Trimmed Reads

source ../../../python_venv/bin/activate # from the current directory, activates the bin of installed python packages, including multiqc

multiqc ./clean -o ./clean #Compile MultiQC report from FastQC files - output .html in current directory ( fast_muiltiQC folder)

echo "Cleaned MultiQC report generated." $(date)

