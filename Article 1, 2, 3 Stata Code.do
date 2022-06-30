*article 1

use File  "C:\Users\Maggie\Downloads\maggie_data_v7_collapsed domains.xlsx", sheet("Sheet1") 
firstrow clear 

summarize grade pvalue hasPic word_count, detail

oneway word_count grade, bonferroni tabulate
oneway word_count domain, bonferroni tabulate
oneway word_count state, bonferroni tabulate

*article 2

use File  "C:\Users\Maggie\Downloads\maggie_data_v7_collapsed domains.xlsx", sheet("Sheet1") 
firstrow clear 

summarize CC-BG, detail

oneway CC grade, bonferroni tabulate
oneway CD grade, bonferroni tabulate
oneway DT grade, bonferroni tabulate
oneway EX grade, bonferroni tabulate
oneway FW grade, bonferroni tabulate
oneway IN grade, bonferroni tabulate
oneway JJ grade, bonferroni tabulate
oneway JJR grade, bonferroni tabulate
oneway JJS grade, bonferroni tabulate
oneway LS grade, bonferroni tabulate
oneway MD grade, bonferroni tabulate
oneway NN grade, bonferroni tabulate
oneway NNS grade, bonferroni tabulate
oneway NNP grade, bonferroni tabulate
oneway NNPS grade, bonferroni tabulate
oneway PDT grade, bonferroni tabulate
oneway POS grade, bonferroni tabulate
oneway PRP grade, bonferroni tabulate
oneway AH grade, bonferroni tabulate
oneway RB grade, bonferroni tabulate
oneway RBR grade, bonferroni tabulate
oneway RBS grade, bonferroni tabulate
oneway RP grade, bonferroni tabulate
oneway SYM grade, bonferroni tabulate
oneway TO grade, bonferroni tabulate
oneway UH grade, bonferroni tabulate
oneway VB grade, bonferroni tabulate
oneway VBD grade, bonferroni tabulate
oneway VBG grade, bonferroni tabulate
oneway VBN grade, bonferroni tabulate
oneway VBP grade, bonferroni tabulate
oneway VBZ grade, bonferroni tabulate
oneway WDT grade, bonferroni tabulate
oneway WP grade, bonferroni tabulate
oneway BC grade, bonferroni tabulate
oneway WRB grade, bonferroni tabulate
oneway BE grade, bonferroni tabulate
oneway BF grade, bonferroni tabulate
oneway BG grade, bonferroni tabulate


regress pvalue grade hasPic CC-BG readability_flesch_kincaid acedemic_polysemous_count, beta

estat vif

sktest res

rfvplot

lvr2plot

lvr2plot, mlabel(A) mlabp(0) m(none) mlabsize(small) jitter(5)



* article 3

use File: "C:\Users\Maggie\Documents\Python Scripts for NLP Work\readability5 domain chg.xlsx", sheet("Sheet1") firstrow clear


oneway word_count grade, bonferroni tabulate
oneway word_count state, bonferroni tabulate
oneway word_count domain, bonferroni tabulate

oneway pvalue grade, bonferroni tabulate
oneway pvalue state, bonferroni tabulate
oneway pvalue domain, bonferroni tabulate





