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

cd ./ # nav to home dir from whereever the script is run

echo "start time stamp" $(date)

# generate md5
md5sum ../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.gz > Airradians_lcWGS/F1/output/md5checksum/Oct2022_check.md5

echo "md5checksum complete!" $(date)

# count the number of sequences per file  - for fastq file the accession ID of each read starts with @, grep does this well + fast!
grep -c '@' ../../../share/nefsc/mcfarland_sequecenes/lcWGS_F1scallops_Oct2022/*.fastq.gz > Airradians_lcWGS/F1/output/md5checksum/Oct2022_rawread_count.txt

echo " grep '@' raw fastq read counts complete!" $(date)
