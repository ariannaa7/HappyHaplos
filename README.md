# Welcome to HappyHaplos
HappyHaplos is an Rshiny app that allows you to determine haplogroup lineage, visualize haplogroup frequence per country via heatmap and table, and determine year of emergence for a haplogroup in a country!

## Files Needed
There are three input files needed to go through the following steps
1. Then Allen Ancient DNA Resource (AADR) dataset which contains a large amount of information, notably haplogroups and associated contries (AADR Annotation.xlsx)
2. The ISOGG Y-chromosome haplogroup treefile to trace paternal lineage (chrY_hGrpTree_isogg2016.txt)
3. The PhyloTree mitochondial DNA haplogroup treefile to trace maternal lineage (mt_phyloTree_b17_Tree2.txt)

More information on how to access these files in the next steps!

## Preparatory Steps

### Set-up a Parent Directory
```bash
cd ~ # Move to your home directory

mkdir PopulationGeneticsProject # Create a parent directory

cd PopulationGeneticsProject

mkdir Raw_Data # Create a directory to store data that will be pulled from Eran's onedrive

mkdir 01_CleanData # Create a directory to store parsed files
```

### Get the Data
Download these 3 data directories from Eran Elhaik's OneDrive: AADR_54.1, AncientYDNA, AncientMtDNA

Then move the directories to the Raw_Data directory

### Parse the AADR dataset
Parse the AADR datset to output two separate datasts, one containing Y-Chromosome haplogroups and one containing Mitochondrial haplogrouops. 

```bash
# Run this from the PopulationGeneticsProject directory
python3 AADR_parser.py Raw_Data/AADR_54.1/AADR\ Annotation.xlsx # to run on mac, adjust how Annotation file name is written if necessary
```

The parser will:
1. Pull relevant columns (genetic ID, mean date (years before 1950), country, and haplogroup in ISOGG format)
2. Convert the mean dates by subtracting the values from 1950
2. Only pull rows that contain a haplogroup (e.g. for AADR_mt it won't pull rows that are missing mitochondrial haplogroups in ISOGG format)
3. Clean up the haplogroup formatting. Some of the haplogroups include special characters which are not present in the associated tree file. The parser accounts for that by removing the special character and all characters following it from the haplogroup
4. Output the files into the 01_CleanData Directory

**Outputs:** (1) AADR_y.txt (2) AADR_mt.txt

### Change format of Mitochondrial haplogroup tree
The format of the mitochondrial haplogroup tree file will lead to significant data loss if read into R.

To account for this, the .txt file should be saved as an excel file. 
1. Open up the Mitochondrial haplogroup tree (~/PopulationGeneticsProject/Raw_Data/AncientMtDNA/mt_phyloTree_b17_Tree2.txt) with excel
2. Save the file as mt_phyloTree_b17_Tree2.xlsx within the 01_CleanData directory!

## Run R-shiny via R studio
Open HappyHaplosApp.R with RStudio (v2023.12.1+402) and run!

HappyHaplosApp.R will:
1. Pull the lineage for a haplogroup that the user selected based on the associated tree file
2. Create relevant subsets of the AADR dataset and shapefile (geographic file to create heatmap) to determine population, haplogroup frequency, and year of emergence
3. Output this information to the user interface in a user-friendly manner

