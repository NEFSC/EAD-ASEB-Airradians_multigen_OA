#!/bin/bash
#SBATCH --job-name="F1B_angsd_run_dupsquery"
#SBATCH -t 500:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=160GB
#SBATCH -p medmem
#SBATCH --mail-user=samuel.gurr@noaa.gov
#SBATCH --output=./Airradians_lcWGS/snakemake_pipeline/angsd/output/F1_Broodstock/query_dups/"%x_out.%j"
#SBATCH --error=./Airradians_lcWGS/snakemake_pipeline/angsd/output/F1_Broodstock/query_dups/"%x_err.%j"

# Set-up


# output dir
mkdir -p angsd/output


## load module
module load bio/angsd/0.933
module load bio/samtools/1.15.1

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


mkdir -p $OUTDIR/F1_Broodstock # make director for ansd output files
mkdir -p $OUTDIR/F1_Broodstock/query_dups
mkdir -p $OUTDIR/F1_Broodstock/coord_dups


# nav to dir
cd $DATDIR/F1_Broodstock/merged_bams/query_dups_removed # NAV TO THE REMOVED DUPLICATES BY QUERYNAME!
# ls ./*.bam | sort | uniq > ./merged_bamlist.txt # create a list of all bam files in merged_bams


# Query dups removed LOW pCO2
# cal number of indiv
nIND_LOW=$(wc -l $DATDIR/F1_Broodstock/merged_bams/query_dups_removed/LOWpCO2_bamlist.txt | awk '{print $1}') # call and count lines of the bamlist with delimiter id
minIND_LOW=$(echo "(${nIND_LOW} * 0.95)" | bc) # min individual is 90% of the total count
minINDRd_LOW=$(printf "%.0f" ${minIND_LOW}) # round that

# set min and max depth for minuum of 5X and max of 20X coerage per loci to acoid potential bias arising from seq errors and recommendations in O'Leary (etal 2018)
MINDP_LOW=$(echo "(${minINDRd_LOW} * 5)" | bc) # min coverage of 5X accoutning for the minimum num of individuals for genotype calls as 90% and assuming per indiv per loci
MAXDP_LOW=$(echo "(${minINDRd_LOW} * 20)" | bc) # max coverage of 20X accounting for the minimum num of individual for genotype calls as 90% and assuming per indiv per loci

# call unique outbase
OUTBASE_LOW='F1Broodstock_pH8_doMaf'$DOMAF'_minMaf'$MINMAF'_majorminor'$DOMAJORMINOR'_minind'$minINDRd_LOW'_minD5x'$MINDP_LOW'_maxD20x'$MAXDP_LOW'minDind'$SETMINDEPTHIND'_maxDind'$SETMAXDEPTHIND'_minq'$MINQ'_minmapQ' # Build base name of output files

# run angsd
angsd -b $DATDIR/F1_Broodstock/merged_bams/query_dups_removed/LOWpCO2_bamlist.txt \
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
    -setMinDepth $MINDP_LOW \
    -setMaxDepth $MAXDP_LOW \
    -setMaxDepthInd $SETMAXDEPTHIND \
    -setMinDepthInd $SETMINDEPTHIND \
    -doIBS 1 \
    -makematrix 1 \
    -doCov 1 \
    -minInd $minINDRd_LOW \
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
    -out $OUTDIR/F1_Broodstock/query_dups/$OUTBASE_LOW \
    >& $OUTDIR/F1_Broodstock/query_dups/$OUTBASE_LOW'.log';




# Query dups removed MODERATE pCO2
# cal number of indiv
nIND_MOD=$(wc -l $DATDIR/F1_Broodstock/merged_bams/query_dups_removed/MODpCO2_bamlist.txt | awk '{print $1}') # call and count lines of the bamlist with delimiter id
minIND_MOD=$(echo "(${nIND_MOD} * 0.95)" | bc) # min individual is 90% of the total count
minINDRd_MOD=$(printf "%.0f" ${minIND_MOD}) # round that

# set min and max depth for minuum of 5X and max of 20X coerage per loci to acoid potential bias arising from seq errors and recommendations in O'Leary (etal 2018)
MINDP_MOD=$(echo "(${minINDRd_MOD} * 5)" | bc) # min coverage of 5X accoutning for the minimum num of individuals for genotype calls as 90% and assuming per indiv per loci
MAXDP_MOD=$(echo "(${minINDRd_MOD} * 20)" | bc) # max coverage of 20X accounting for the minimum num of individual for genotype calls as 90% and assuming per indiv per loci

# call unique outbase
OUTBASE_MOD='F1Broodstock_pH75_doMaf'$DOMAF'_minMaf'$MINMAF'_majorminor'$DOMAJORMINOR'_minind'$minINDRd_MOD'_minD5x'$MINDP_MOD'_maxD20x'$MAXDP_MOD'minDind'$SETMINDEPTHIND'_maxDind'$SETMAXDEPTHIND'_minq'$MINQ'_minmapQ' # Build base name of output fil$

# run angsd
angsd -b $DATDIR/F1_Broodstock/merged_bams/query_dups_removed/MODpCO2_bamlist.txt \
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
    -setMinDepth $MINDP_MOD \
    -setMaxDepth $MAXDP_MOD \
    -setMaxDepthInd $SETMAXDEPTHIND \
    -setMinDepthInd $SETMINDEPTHIND \
    -doIBS 1 \
    -makematrix 1 \
    -doCov 1 \
    -minInd $minINDRd_MOD \
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
    -out $OUTDIR/F1_Broodstock/query_dups/$OUTBASE_MOD \
    >& $OUTDIR/F1_Broodstock/query_dups/$OUTBASE_MOD'.log';
