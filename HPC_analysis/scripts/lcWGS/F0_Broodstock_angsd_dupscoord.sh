#!/bin/bash
#SBATCH --job-name="F0B_angsd_run_dupscoord"
#SBATCH -t 500:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=160GB
#SBATCH -p medmem
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/snakemake_pipeline/angsd/output/F0_Broodstock/coord_dups/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/snakemake_pipeline/angsd/output/F0_Broodstock/coord_dups/"%x_err.%j"

# Set-up


# output dir
mkdir -p angsd/output


## load module
module load bio/angsd/0.933
module load bio/samtools/1.15.1

#IDS=('F1_Broodstock' 'F1_Juveniles' 'F2_Juveniles' 'F2_Broodstock' 'F3_Juveniles') # loop strings

## loop vars by treatment (for loop #2)
#TREATMENT_ALL=('LOWpCO2' 'MODpCO2' 'HIGHpCO2') # a list of the common str on file names and outputs in the for loop below


## dir shortcuts
DATDIR=~/Airradians_lcWGS/snakemake_pipeline/angsd/data # Path to generation and life stage bam files
OUTDIR=~/Airradians_lcWGS/snakemake_pipeline/angsd/output # Path to the base directory where output files will be written
REFDIR=~/refs # Path to reference genome

# angsd parameters
DOMAJORMINOR=4 # 1 is for either seq data like bam files or genotpe liklihood data where the maj minor allele is inferred from likli$
# 4 is forcing the major allele according to the reference states defined in ref - the minor allele will be inferred based on the ge$
DOMAF=1 # 1 assumed known major and minor allele 2 assumed known major and unknown minor allele
DOGENO=5 # 1 write major and minor allele 2 print the called genotype and count of minor 4 print the called genotype
DOPOST=1 # 1 estimate the posterior genotype probability based on the allele frequency as a prior 2 estimat eth posterier genotpe pr$
GENOLIKE=1 # SAMtools
DOGLF=2 # beagle genotype liklihood format
DOCOUNTS=1 # provide freq of bases - output as pos.gz
DODEPTH=1 # default 0 output distribution of seq depths sites aove maxDepth will be binnes out files in depthSample and depthGlobal
MAXDEPTH=100 # default 100
SETMINDEPTHIND=5 # min depth, below which the individual is omitted from analysis
SETMAXDEPTHIND=150 # max depth - above which the indivisual is omitted from analysis
DUMPCOUNTS=2 # 1 is the total seq depth for site as .pos.gz, 2 is the seq deth per sample as .pos.gz and .counts.gz; look up allele $
MINQ=30 # Minimum quality filter
MINMAPQ=30 # Minmm mapping quality
MINMAF=0.05 # Minimum minor allele frequency filter
# C -50 is flag to adjust for excessive mismatches as recomennded when BWA was used for alignment
# -uniqueOnly; removed reads that have multiple best hits 0 default, 1 remove
# -remove_bads = reads flagged as bad
# -only_proper_pairs = read that did not have a mate pair

# ANGSD for data within generation (F1 - F2 - F3, broodstock and juveniles separately)
# for loop - run ansd for the total bam list within time/generation (i.e. all F1 Broodstock, F2_Juveniles, etc.)
# objective to input a strata / metadta to identify treatments associated with ids downstream..


mkdir -p $OUTDIR/F0_Broodstock # make director for ansd output files
mkdir -p $OUTDIR/FO_Broodstock/coord_dups

# nav to dir
cd $DATDIR/F0_Broodstock/merged_bams/coord_dups_removed # NAV TO THE REMOVED DUPLICATES BY QUERYNAME!
ls ./*.bam | sort | uniq > ./merged_bamlist.txt # create a list of all bam files in merged_bams

# cal number of indiv
nIND=$(wc -l $DATDIR/F0_Broodstock/merged_bams/coord_dups_removed/merged_bamlist.txt | awk '{print $1}') # call and count lines of the bamlist with delimiter id
minIND=$(echo "(${nIND} * 0.95)" | bc) # min individual is 90% of the total count
minINDRd=$(printf "%.0f" ${minIND}) # round that

# set min and max depth for minuum of 5X and max of 20X coerage per loci to acoid potential bias arising from seq errors and recommendations in O'Leary (etal 2018)
MINDP=$(echo "(${minINDRd} * 5)" | bc) # min coverage of 5X accoutning for the minimum num of individuals for genotype calls as 90% and assuming per indiv per loci
MAXDP=$(echo "(${minINDRd} * 20)" | bc) # max coverage of 20X accounting for the minimum num of individual for genotype calls as 90% and assuming per indiv per loci

# call unique outbase
OUTBASE='F0Broodstock_doMaf'$DOMAF'_minMaf'$MINMAF'_majorminor'$DOMAJORMINOR'_minind'$minINDRd'_minD5x'$MINDP'_maxD20x'$MAXDP'minDind'$SETMINDEPTHIND'_maxDind'$SETMAXDEPTHIND'_minq'$MINQ'_minmapQ' # Build base name of output files

# run angsd
angsd -b $DATDIR/F0_Broodstock/merged_bams/coord_dups_removed/merged_bamlist.txt \
    -ref $REFDIR/Argopecten_irradians_irradians_genome.fasta \
    -GL $GENOLIKE \
    -doGlf $DOGLF \
    -doMaf $DOMAF \
    -doMajorMinor $DOMAJORMINOR \
    -dogeno $DOGENO \
    -doPOST $DOPOST \
    -doCounts $DOCOUNTS \
    -dumpCounts $DUMPCOUNTS \
    -doDepth $DODEPTH \
    -maxDepth $MAXDEPTH \
    -setMinDepth $MINDP \
    -setMaxDepth $MAXDP \
    -setMaxDepthInd $SETMAXDEPTHIND \
    -setMinDepthInd $SETMINDEPTHIND \
    -doIBS 1 \
    -makematrix 1 \
    -doCov 1 \
    -minInd $minINDRd \
    -dobcf 1 \
    --ignore-RG 0 \
    -minQ $MINQ \
    -minMapQ $MINMAPQ \
    -minMaf $MINMAF \
    -SNP_pval 1e-6 \
    -P -20 \
    -remove_bads 1 \
    -only_proper_pairs 1 \
    -uniqueOnly 1 \
    -C 50 \
    -out $OUTDIR/F0_Broodstock/coord_dups/$OUTBASE \
    >& $OUTDIR/F0_Broodstock/coord_dups/$OUTBASE'.log';
#done
