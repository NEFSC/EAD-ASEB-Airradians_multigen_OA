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
        fastp --in1 ${i} --out1 ./adapter_trim/adapter_trim.${i} --adapter_sequence=CTGTCTCTTATACACATCT --adapter_fasta adapter.fasta # --detect_adapter_for_pe  # CTGTCTCTTATACACATCT # check JUST adapters trimmed without other parameters 
        fastqc  ./adapter_trim/adapter_trim.${i} --outdir ./adapter_trim # call the output files from fastp in previous line and output fastqc in the same folder with adapter_trim filename head
done

# https://support.illumina.com/bulletins/2016/12/what-sequences-do-i-use-for-adapter-trimming.html - check out the adapters to choose for trimming (Nextera XT used above)

echo "Read trimming of adapters complete." $(date)

# Quality Assessment of Trimmed Reads

source ../../../../python_venv/bin/activate # from the current directory, activates the bin of installed python packages, including multiqc

multiqc ./adapter_trim  -o ./adapter_trim #Compile MultiQC report from FastQC files - output .html in adpater_trim directory ( fast_muiltiQC folder)

echo "Cleaned MultiQC report generated." $(date)

