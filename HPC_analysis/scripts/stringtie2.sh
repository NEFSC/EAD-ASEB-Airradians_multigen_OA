#!/bin/bash
#SBATCH --job-name="stringtie2"
#SBATCH -t 048:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output="%x_out.%j"
#SBATCH --error="%x_err.%j"

# before running..
# make directory named stringtie2 as output/stringtie2/Pmaximus


# call command names 
BASEDIR=~/ # base directory
REFDIR=~/refs
DATDIR=~/Airradians_F1s_TagSeq/output/hisat2/Pmaximus_map # directory of mapped bam files from hisat2
OUTDIR=~/Airradians_F1s_TagSeq/output/stringtie2/Pmaximus

# load modules, requires stringtie2
module load bio/stringtie/2.2.0

cd ~ # nav back to home directory (allows job to be run from anywhere)

array=($(ls ${DATDIR}/*.bam)) #Make an array of sequences to assemble

# awk example: file name clean.C9-larva-22_S77.bam - use awk to get sample name w/o .bam using . as delimiter
# run strnigtie on the array and output to the stringtie2 directory
for i in ${array[@]}; do #Running with the -e option to compare output to exclude novel genes. Also output a file with the gene abundances
        sample_name=`echo ${i}| awk -F [.] '{print $2}'`
	stringtie -p 8 -e -B -G ${REFDIR}/GCF_902652985.1_xPecMax1.1_genomic.gff -A ${OUTDIR}/${sample_name}.gene_abund.tab -o ${OUTDIR}/${sample_name}.gtf ${i}
        echo "StringTie assembly for seq file ${i}" $(date)
done
echo "StringTie assembly COMPLETE, starting assembly analysis" $(date)

