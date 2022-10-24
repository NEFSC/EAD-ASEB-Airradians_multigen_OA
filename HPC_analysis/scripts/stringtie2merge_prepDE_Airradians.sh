#!/bin/bash
#SBATCH --job-name="merge_countmatrix"
#SBATCH -t 048:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output="%x_out.%j"
#SBATCH --error="%x_err.%j"

# load packages
module load bio/stringtie/2.2.0
module load bio/gffcompare/0.12.6

# before running..
# make directory named stringtie2 as output/stringtie2/Arradians
# make directory names count_matrix/Airradians

# call command names
BASEDIR=~/ # base directory
REFDIR=~/refs # direcrory with ref genome data
DATDIR1=~/Airradians_F1s_TagSeq/output/hisat2/Airradians_map # directory of mapped bam files from hisat2
DATDIR2=~/Airradians_F1s_TagSeq/output/hisat2/Airradians_map # directory of .gtf files from first stringtie2 run
OUTDIR1=~/Airradians_F1s_TagSeq/output/stringtie2/Airradians # outdir of the gtf stringtie2 files in stringtie2.sh
OUTDIR2=~/Airradians_F1s_TagSeq/output/stringtie2/Airradians/merged # outdir for the outputs in here in a new merged directory within stringtie2 outputs
OUTDIR3=~/Airradians_F1s_TagSeq/output/count_matrix/Airradians # output of prepde.py
SCRIPTS=~/Airradians_F1s_TagSeq/scripts

cd ${BASEDIR} # nav to home dir

# stringtie2 merge ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


cd ${OUTDIR1}

# gtf_list.txt and listGTF.txt
ls *.gtf > gtf_list.txt # list the .gtf files in output/stringtie2

# run stringtie merge
stringtie --merge -p 8 ../../../../refs/Argopecten_irradians_irradians.gff -o Airradians_merged.gtf gtf_list.txt  #Merge GTFs to form $
echo "Stringtie merge complete" $(date)

cd 




# gff compare :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




# run gff compaure to report accuracy of alignment to reference
gffcompare -r ${REFDIR}/Argopecten_irradians_irradians.gff  -G -o ${OUTDIR1}/merged ${OUTDIR1}/Airradians_merged.gtf #Compute the accuracy and pre$
echo "GFFcompare complete, Starting gene count matrix assembly..." $(date)


# prepDE.py  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



for filename in ${OUTDIR1}/Ai*.gtf; # call the merged files output in stringtie above
        do echo $(basename -- $filename) $filename;
        done > ${OUTDIR1}/listGTF.txt # full root to each .gtf file in output/stringtie2 - call in prepDE.py as -i

# prepDE.py to assemble count matrix for R
# python is an alias for python_venv/bin/python (requires virtual envrionment installation on sedna)
python2 ${SCRIPTS}/prepDE.py3 -g  ${OUTDIR3}/Airradians_gene_count_matrix.csv -t  ${OUTDIR3}/Airradians_transcript_count_matrix.csv -i  ${OUTDIR1}/listGTF.txt #Compile the gene count matrix
echo "Gene count matrix compiled." $(date)
