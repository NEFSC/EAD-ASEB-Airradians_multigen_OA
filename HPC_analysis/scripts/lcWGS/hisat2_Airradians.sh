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
