# RWeka-Naive-Bayes-10-Fold-Cross-Validation
This is a collection of scripts I used to manipulate and perform 10-fold cross validation on a huge data set using R and RWeka.

#####Script Descriptions

10-bayes-bc-50
- This script takes the processed data and performs NB 10x validation on the binary classifications.

10-bayes-originals-50
- This script takes the processed data and performs NB 10x validation on the normal classifications.

AddingBinaryClassifications-50
- This script strips the preset data fields and get's it's binary classifications.

Cleaner_Merger-50
- This script cleans the data and merges some of it together (you probably could have gathered that from just the name.)

MergingFrames-50
- This script was used to take data from a txt file and strip it into readable portions so that it could be placed into a subsequent CSV file.

MinimumRemoval-50
- This script took the repetitive (un-classifiable) minimums from the data and replaces them with '?' so it could be easily understood as an empty value in RWeka.

summaryGeneratorTing-50
- This script was only used to garner a short summary of some of the data.

tableGeneratorTing-50
- This script was used to create xTables dealing with preset aspects for the data. The tables were later used in LaTeX documents for viewing.
