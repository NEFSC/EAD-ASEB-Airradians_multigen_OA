#!/bin/bash
#SBATCH --job-name="diamond"
#SBATCH --mem=100GB
#SBATCH -t 100:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output="%x_out.%j"
#SBATCH --error="%x_err.%j"


date

# Note: diamond is an alternative and more efficient module for blastx
# module load BLAST+/2.7.1-foss-2018a
module load bio/diamond/2.0.14.152

# Pass the database you want as the first argument
# Pass the query you want as the second argument

database=$1 # first call contains the protein database you want to blast against (i.e maybe contains more annotation that your reference file)
query=$2 #  second is your nucelotide fast - what you are trying to learn more about through this blast (i.e. accessoin IDs do nothavve annotation and want to ID your genes)

mkdir ./Cgigasdb/

# Make the database - note build a protein database and follow with blastx (nucl query w/ protein database)
diamond makedb --in $1 -d ./Cgigasdb/Cgigas_db


# runs blast with the unannotated Airradians gene fasta against the annotated Cvirginiva protein database we created above
# --very sensitive finds hits with best sensitivity <40% identity 

diamond blastx -d ./Cgigasdb/Cgigas_db.dmnd -q $2 -o ./cgigas_diamond_out --outfmt 6 

# qseqid sseqid pident evalue length qlen slen qstart qend sstart send sseq

echo "Done"
date
