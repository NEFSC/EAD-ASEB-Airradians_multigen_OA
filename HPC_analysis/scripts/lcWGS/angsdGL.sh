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
## about calls
### -GL 1 == SAMtools model; -GL 2 == GATK model 
### -P number of threads - lets try 5
### -doMajorMinor 1 == major and minor allele inferred based on the GL
### -doMaf 2 == infer the major and minor allele by picking the two most frequenctly observed bases across indivs

# -doMajorMinor 4 == force the major allele according to the reference states if you have defined in -ref [fasta.fa]
