# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(dplyr)
library(ggplot2)
library(forcats)
library(lme4)
library(lmerTest)
library(see)
library(performance)
library(car)
library(kableExtra)
library(pander)
library(data.table)
library(stringr)
library(latex2exp)
library(Rmisc)
library(devtools)
library(ggpubr)
library(hrbrthemes)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/") # personal computer




# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# LOAD DATA (with edits) ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# count matrix from prepDE.py script (Airradians raft and unannotated genome!)
raw.countmatrix  <- read.csv(file="HPC_Analysis/output/Airradians_transcript_count_matrix.csv", header=T)
raw.countmatrix[is.na(raw.countmatrix)] <- 0 # replace all occurances of NA with 0
# View(raw.countmatrix)
Cvirg_seqID      <-  as.data.table(read.delim2(file = "RAnalysis/Data/TagSeq/seq_id.txt", header =F)) %>% 
                              `colnames<-`("fullID")
Cvirg_GOterms    <-  read.csv(file = "RAnalysis/Data/TagSeq/Cviginiva_GOterms.csv", header =T) %>% 
                              dplyr::select(c('GeneID','Annotation_GO_ID'))
# diamond result to otain accession IDs of annotated genomes Cvirg and Cgigas for gene ID, GO, and KEGG ID information 
#(1) Airradians protein database (...pep.fna file) with Cvirginica nucleotide query
blastx_Airr_Cvirg <- as.data.table(read.delim2(file="HPC_Analysis/output/blastx/AirrProDB_CvirgNQuery/airradians_diamond_out", header=F)) %>% 
                              `colnames<-`(c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"))
#(2) Cgigas protein database with Airradians nucleotide query
blastx_Airr_Cgig  <- as.data.table(read.delim2(file="HPC_Analysis/output/blastx/CgigProDB_AirrNQuery/cgigas_diamond_out", header=F)) %>% 
                              `colnames<-`(c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore"))



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INITIAL DIAGNOSTICS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

nrow(raw.countmatrix) # 26595 total unique transcrips calls in A irradians count matrix
length(unique(blastx_Airr_Cvirg$sseqid)) # 19042 - Airradians transcripts - in blast x Airradiads Prot database  to Cvriginica nucleotide query
length(unique(blastx_Airr_Cgig$qseqid)) # 7046 - Airradians transcripts - in Cgigas protein database to Airradians nucleotide query

# go with the Cvirg blastx values - more hits to unique genes (~ 3x more!)
bybitscore  <- blastx_Airr_Cvirg[,.SD[which.max(bitscore)],by=sseqid] # max bitscore
length(unique(bybitscore$sseqid)) # 19042
length(unique(bybitscore$sseqid))  == length(unique(blastx_Airr_Cvirg$sseqid))# TRUE
nrow(bybitscore %>% dplyr::filter(sseqid %in% raw.countmatrix$transcript_id)) # 18874

byevalue    <- blastx_Airr_Cvirg[,.SD[which.min(evalue)],by=sseqid] # min evalue
length(unique(byevalue$sseqid)) # 19042
length(unique(byevalue$sseqid))  == length(unique(blastx_Airr_Cvirg$sseqid))# TRUE
nrow(byevalue %>% dplyr::filter(sseqid %in% raw.countmatrix$transcript_id)) # 18874


head(as.data.table(c(byevalue$sseqid, byevalue$qseqid)), header=F)



# 



# seq ID reference fr C virginica data 
Cvirg_seqID <- Cvirg_seqID[Cvirg_seqID$fullID %like% "XM_", ]  %>% # call all mRNA samples - accession always starts with XM
  dplyr::mutate(TranscriptID = (str_match(fullID, ">\\s*(.*?)\\s* PREDICTED:")[,2])) %>% 
  dplyr::mutate(ProteinID = sub('.*Crassostrea virginica ', '',(gsub("\\s\\(LOC.*|\\sLOC111.*", "", perl=TRUE, fullID))) ) %>% 
  dplyr::mutate(GeneID = paste('L', (gsub('),.*', '',(gsub(".*\\s\\(L", "", fullID)))), sep = '')) %>% 
  dplyr::select(-fullID)

write.csv(Cvirg_seqID, file = "RAnalysis/Data/TagSeq/seq_id_Cvirginica_master.csv", row.names = FALSE)



#  
Cvirg_seqID      <-  read.csv(file = "RAnalysis/Data/TagSeq/seq_id_Cvirginica_master.csv", header =T) %>% 
                              dplyr::rename(Cvirginica_TranscriptID = TranscriptID)

#
Airr.ID         <- as.data.frame(raw.countmatrix$transcript_id) %>% `colnames<-`("Airradians_TranscriptID")
Cvirg_ID.evalue <- merge(Cvirg_seqID, 
                         (byevalue   %>% dplyr::select(sseqid, qseqid) %>% `colnames<-`(c("Airradians_TranscriptID", "Cvirginica_TranscriptID"))),
                         by="Cvirginica_TranscriptID", 
                         all=T) %>% `colnames<-`(c("blastxEval_CvirgTranscriptID", "blastxEval_CvirgProteinID", "blastxEval_CvirgGeneID", "Airradians_TranscriptID"))
Cvirg_ID.bitsc  <- merge(Cvirg_seqID,
                         (bybitscore %>% dplyr::select(sseqid, qseqid) %>% `colnames<-`(c("Airradians_TranscriptID", "Cvirginica_TranscriptID"))),
                         by="Cvirginica_TranscriptID", 
                         all=T) %>% `colnames<-`(c("blastxBit_CvirgTranscriptID", "blastxBit_CvirgProteinID", "blastxBit_CvirgGeneID", "Airradians_TranscriptID"))

merge1  <- merge(Airr.ID,Cvirg_ID.evalue,by="Airradians_TranscriptID")  %>% dplyr::rename(GeneID = blastxEval_CvirgGeneID)
merge2  <- merge(merge1, Cvirg_ID.bitsc,by="Airradians_TranscriptID", all=T)
seq.IDs <- merge(merge1, Cvirg_GOterms, by = "GeneID") %>% 
  dplyr::rename(blastxEval_CvirgGeneID = GeneID) %>% 
  dplyr::rename(blastxEval_Cvirg_GOterms = Annotation_GO_ID)
View(seq.IDs)






