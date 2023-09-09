# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(dplyr)
library(kableExtra)
library(pander)
library(data.table)
library(stringr)
library(devtools)





# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/") # personal computer







# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# LOAD DATA (with edits) ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# count matrix from prepDE.py script 
# NOTE: aligned to the Airradians draft and unannotated genome!
raw.countmatrix  <- read.csv(file="HPC_Analysis/output/F1_TagSeq/Airradians_transcript_count_matrix.csv", header=T)
raw.countmatrix[is.na(raw.countmatrix)] <- 0 # replace all occurances of NA with 0 in the cell NOT THE WHOLE ROW!
nrow(raw.countmatrix) # 26595 total transcripts

# due to the lack of annotation in the Airraians draft genome..
# call the Cvirginica database of protein names and transcript ID calls
Cvirg_seqID      <-  as.data.table(read.delim2(file = "RAnalysis/Data/Transcriptomics/seq_id.txt", header =F)) %>% 
                              `colnames<-`("fullID")
nrow(Cvirg_seqID) # 66625
Cvirg_GOterms    <-  read.csv(file = "RAnalysis/Data/Transcriptomics/Cviginiva_GOterms.csv", header =T) %>% 
                              dplyr::select(c('GeneID','Annotation_GO_ID', 'Length')) %>% 
                              dplyr::group_by(GeneID) %>% # tif you add GO column here we get duplicates, some of the same gene ID calls (of diff length) have GO term or do not, weird!
                              dplyr::summarise(
                                meanLength = mean(Length)) %>% 
                              unique() # there are many redundant rows here
subset(Cvirg_GOterms,duplicated(GeneID)) # no duplicates, BUT need to filter in the GO terms here 
Cvirg_GOterms2 <- merge(Cvirg_GOterms,
                       ( unique(read.csv(file = "RAnalysis/Data/Transcriptomics/Cviginiva_GOterms.csv", header =T) %>% 
                          dplyr::select(c('GeneID','Annotation_GO_ID')) %>% 
                          dplyr::filter(!Annotation_GO_ID == "")) ), 
                       by = 'GeneID')
nrow(Cvirg_GOterms2) #19667
# diamond result to obtain accession IDs of annotated genes Cvirg and Cgigas for gene ID, GO, and KEGG ID information 
#(1) Airradians protein database (...pep.fna file) with Cvirginica nucleotide query
blastx_Airr_Cvirg <- as.data.table(read.delim2(file="HPC_Analysis/output/F1_TagSeq/blastx/AirrProDB_CvirgNQuery/airradians_diamond_out", header=F)) %>% 
                              `colnames<-`(c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"))

#(2) Cgigas protein database with Airradians nucleotide query
blastx_Airr_Cgig  <- as.data.table(read.delim2(file="HPC_Analysis/output/F1_TagSeq/blastx/CgigProDB_AirrNQuery/cgigas_diamond_out", header=F)) %>% 
                              `colnames<-`(c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"))

# where do we go from here? We have to estimate which diamond (blast) was best for obtaining 
# gene annotation for the Airradians transcripts - diagnostics below to reveal which obtained most gene relatedness

  




# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  WHICH BLASTX IS BETTER SUITED? :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

nrow(raw.countmatrix) # 26595 total unique transcrips calls in A irradians count matrix

# how many unique trnascript IDs of Airradians were covered by oyster blastx(s)?
# Cvirginica
length(unique(blastx_Airr_Cvirg$sseqid)) # 19042 - Airradians transcripts - in blast x Airradiads Prot database  to Cvriginica nucleotide query
(length(unique(blastx_Airr_Cvirg$sseqid)) / nrow(raw.countmatrix))* 100 # 71.6% of genes!
# C gigas
length(unique(blastx_Airr_Cgig$qseqid)) # 7046 - Airradians transcripts - in Cgigas protein database to Airradians nucleotide query
(length(unique(blastx_Airr_Cgig$sseqid)) / nrow(raw.countmatrix))* 100 # 32.3% of genes!

# winner. winner. chicken dinner. C virginica got em!
# Proceed with the Cvirg blastx values - more hits to unique genes (~ 3x more!)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PROCEED WTH C VIRGINICA ANNOTATION      :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# call the best hits with highest bitscore/evalue 

# by bitscore (highest is the best hit) use 'which.max'
bybitscore  <- blastx_Airr_Cvirg[,.SD[which.max(bitscore)],by=sseqid] # max bitscore
length(unique(bybitscore$sseqid)) # 19042
length(unique(bybitscore$sseqid))  == length(unique(blastx_Airr_Cvirg$sseqid))# TRUE
nrow(bybitscore %>% dplyr::filter(sseqid %in% raw.countmatrix$transcript_id)) # 18874
# by evalue (lowest is the best hit) - use 'which.min'
byevalue    <- blastx_Airr_Cvirg[,.SD[which.min(evalue)],by=sseqid] # min evalue
length(unique(byevalue$sseqid)) # 19042
length(unique(byevalue$sseqid))  == length(unique(blastx_Airr_Cvirg$sseqid))# TRUE
nrow(byevalue %>% dplyr::filter(sseqid %in% raw.countmatrix$transcript_id)) # 18874

# calla dataframe for the two sseqids of blatx dataframes by e value and bitscore
# what does this do? if only one column output than the two are the exact same,
#  if two than bitscore (highest) and evalue (lowest) call different transcript IDs
head(as.data.table(c(byevalue$sseqid, bybitscore$sseqid)), header=F) # one column  meaning they are the exact same!

# lets go with evalue as the 'gold stnadard' 
# 'byevalue' gives us the Airradians trnascript ID (i.e. evm.model.Contig....' alonside 
# for each of the corresponding C virginica IDs (i.e. XM_....') to obtaitn KEGG and GO annotation based
# on sequence relatedness
head(byevalue)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# MASTER SEQ ID FOR CVRIGNICA             :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



# Now lets call the C virginica transcriptome and edit to fit our needs 
# seq ID reference fr C virginica data 
Cvirg_seqID_editted <- as.data.frame(Cvirg_seqID[Cvirg_seqID$fullID %like% "XM_", ]  %>% # call all mRNA samples - accession always starts with XM
  dplyr::mutate(TranscriptID = (str_match(fullID, ">\\s*(.*?)\\s* PREDICTED:")[,2])) %>% # remove excess ID information
  dplyr::mutate(ProteinID = sub('.*Crassostrea virginica ', '',(gsub("\\s\\(LOC.*|\\sLOC111.*", "", perl=TRUE, fullID))) ) %>% # parse out the protein ID
  dplyr::mutate(GeneID = paste('L', (gsub('),.*', '',(gsub(".*\\s\\(L", "", fullID)))), sep = '')) %>%  # parse out the gene ID
  dplyr::select(-fullID)) # remove the full ID

nrow(Cvirg_seqID_editted) # 60201
nrow(Cvirg_GOterms2) # 19667 - only rows with a GO term present, meanLength of all unique gene IDs
Cvirg_seqIDMASTER <- unique(merge(Cvirg_seqID_editted,Cvirg_GOterms2, by = 'GeneID'))
nrow(Cvirg_seqIDMASTER) # 36573

# write csv
write.csv(Cvirg_seqIDMASTER, file = "RAnalysis/Data/Transcriptomics/seq_id_Cvirginica_master.csv", row.names = FALSE)

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# MERGE THE BEST BLAST HITS WITH ANNOTATION :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# objective - merge the data for a master metadata spreadsheet omitted of all annotation (for Cvrginica) that is orrelevant 
# to trnascripts that did not hit to the Airradians transcripts - this will be used for gene function analysis of DEGs!

#  read 'Cvirg_seqIDMASTER' output above 
# file contains the Cvirgnica transcript ID, protein ID, gene ID and GO term annotation
Cvirg_seqID      <-  read.csv(file = "RAnalysis/Data/Transcriptomics/seq_id_Cvirginica_master.csv", header =T) %>% 
                              dplyr::rename(Cvirginica_TranscriptID = TranscriptID)
# # lern how many unique A irradians transcript IDs we have in the raw count matrix 
Airr.ID         <- as.data.frame(raw.countmatrix$transcript_id) %>% 
                         `colnames<-`("Airradians_TranscriptID")
nrow(unique(Airr.ID)) == nrow(Airr.ID) # TRUE 
nrow(Airr.ID) # 26595 - the number of transcripts TOTAL in the raw count matrix1
# merge the Cvirginica seIDs (all cvirginica IDs) with the blastx table we made contianing Airradians hits!
Cvirg_ID.evalue <- merge(Cvirg_seqID, 
                         (byevalue   %>% 
                            dplyr::select(sseqid, qseqid) %>% 
                            `colnames<-`(c("Airradians_TranscriptID", "Cvirginica_TranscriptID"))), by="Cvirginica_TranscriptID",  all=T) %>% 
                  `colnames<-`(c("blastxEval_CvirgTranscriptID", 
                                 "blastxEval_CvirgProteinID",
                                 "blastxEval_CvirgGeneID", 
                                 "meanLength",
                                 "blastxEval_CvirgGOterms",
                                 "Airradians_TranscriptID"))


# we can now do a final merge
# here was have all Airradians Transcript IDs that had the highest 
# evalue hit to the Cvirginica protein database
# merged are the protein names, geneID, GOterms from the Cvirginica database
# to facilitate functional analsiss of DEGs in the Airradians data 
Airr_Cvirg_master_seq_ID  <- merge(Airr.ID,Cvirg_ID.evalue,by="Airradians_TranscriptID") 
# merge2  <- merge(merge1, Cvirg_ID.bitsc,by="Airradians_TranscriptID", all=T)
nrow(Airr_Cvirg_master_seq_ID) # 19014
(nrow(Airr_Cvirg_master_seq_ID) / nrow(raw.countmatrix))*100 # 71.49464 % of genes in our count matrix are represented

# write csv
write.csv(Airr_Cvirg_master_seq_ID, file = "RAnalysis/Data/Transcriptomics/seq_id_AirrCvirg_MERGED_master.csv", row.names = FALSE)
# 'Airr_Cvirg_master_seq_ID' IS OUT MAIN TAKEAWAY FROM THIS CHUNK

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# MERGE ANNOTATION WITH THE COUNT MATRIX   :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# note: in the following script we will upload the 3 cpm read matrix 
# and merge bytranscript ID to the master data frame with the gene annotation 
# however!!!! the 'byevalue' table of Cvirginica database to Airradians blast query results DOES NOT have 
# all the the Airradians transcripts needed for 

# load the filtered count matrix (decided upon 3 CPM in 50% of samples) review the Counts_Filtered.Rmd script 
read_matrix_raw  <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Transcriptomics/raw_count_matrix_editted.csv", head = TRUE) %>% 
  dplyr::rename(Airradians_TranscriptID = 'X') # make sure you name the transcript ID column the SAME NAME as the master 'Airr_Cvirg_master_seq_ID'

# load the filtered count matrix (decided upon 3 CPM in 50% of samples) review the Counts_Filtered.Rmd script 
read_matrix_3CPM <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Transcriptomics/Filtered_counts_matrix/filtered_3cpm50perc.csv", head = TRUE) %>% 
  dplyr::rename(Airradians_TranscriptID = 'X') # make sure you name the transcript ID column the SAME NAME as the master 'Airr_Cvirg_master_seq_ID'



# FILTERED COUNT MATRIX - read_matrix_3CPM 
# merge the functional annotation (from above) to the filtered matrix
MERGE_filtered_reads  <- full_join(read_matrix_3CPM, Airr_Cvirg_master_seq_ID, by = "Airradians_TranscriptID")
MASTER_filtered_reads <- (subset(MERGE_filtered_reads, !is.na(Ai13)))
nrow(MASTER_filtered_reads) # 2553 - there are some duplicates - based on the amount oF GO terms present in the merge
# call the duplicates and get a single to add back into the matrix 
n_occur <- data.frame(table(MASTER_filtered_reads$Airradians_TranscriptID))
n_occur[n_occur$Freq > 1,]
n_occur_dups <- as.data.frame(n_occur %>% dplyr::filter(Freq > 1))
select_dups_to_add <- MASTER_filtered_reads[MASTER_filtered_reads$Airradians_TranscriptID %in% n_occur$Var1[n_occur$Freq > 1],] %>% 
                        group_by(Airradians_TranscriptID) %>%
                        slice(which.max(is.na(blastxEval_CvirgGOterms)))
# call and ommit hte duplicates
MASTER_filtered_reads_omdups <- MASTER_filtered_reads %>% dplyr::filter(!Airradians_TranscriptID %in% n_occur_dups$Var1)
nrow(MASTER_filtered_reads_omdups) # 2491 with duplicates omitted
# add the, now single form duplicates, back into the matrix
MASTER_filtered_reads_2.0 <- rbind(MASTER_filtered_reads_omdups, select_dups_to_add)
nrow(MASTER_filtered_reads_2.0) # 2522
# diagnostics - must be TRUE
nrow(MASTER_filtered_reads_2.0) == nrow(read_matrix_3CPM)
unique(sort(MASTER_filtered_reads_2.0$Airradians_TranscriptID) == sort(read_matrix_3CPM$Airradians_TranscriptID)) # nust be TRUE!!
# write csv
MASTER_filtered_reads <- write.csv(MASTER_filtered_reads_2.0, file = "RAnalysis/Output/Transcriptomics/Filtered_counts_matrix/filter_3cmpm50perc_WITH_ANNOTATION.csv", row.names = FALSE)



# RAW COUNT MATRIX - read_matrix_raw 
# merge the functional annotation (from above) to the filtered matrix
MERGE_raw_reads  <- full_join(read_matrix_raw, Airr_Cvirg_master_seq_ID, by = "Airradians_TranscriptID")
MASTER_raw_reads <- (subset(MERGE_filtered_reads, !is.na(Ai13)))
nrow(MASTER_filtered_reads) # 2553 - there are some duplicates - based on the amount oF GO terms present in the merge
# call the duplicates and get a single to add back into the matrix 
n_occur <- data.frame(table(MASTER_raw_reads$Airradians_TranscriptID))
n_occur[n_occur$Freq > 1,]
n_occur_dups <- as.data.frame(n_occur %>% dplyr::filter(Freq > 1))
select_dups_to_add <- MASTER_raw_reads[MASTER_raw_reads$Airradians_TranscriptID %in% n_occur$Var1[n_occur$Freq > 1],] %>% 
  group_by(Airradians_TranscriptID) %>%
  slice(which.max(is.na(blastxEval_CvirgGOterms)))
# call and ommit hte duplicates
MASTER_raw_reads_omdups <- MASTER_raw_reads %>% dplyr::filter(!Airradians_TranscriptID %in% n_occur_dups$Var1)
nrow(MASTER_raw_reads_omdups) # 26306 with duplicates omitted
# add the, now single form duplicates, back into the matrix
MASTER_raw_reads_2.0 <- rbind(MASTER_raw_reads_omdups, select_dups_to_add)
nrow(MASTER_raw_reads_2.0) # 26595
# diagnostics - must be TRUE
nrow(MASTER_raw_reads_2.0) == nrow(read_matrix_raw)
unique(sort(MASTER_raw_reads_2.0$Airradians_TranscriptID) == sort(read_matrix_raw$Airradians_TranscriptID)) # nust be TRUE!!
# write csv
MASTER_raw_reads <- write.csv(MASTER_raw_reads_2.0, file = "RAnalysis/Output/Transcriptomics/Raw_counts_matrix/raw_count_matrix_WITH_ANNOTATION.csv", row.names = FALSE)
