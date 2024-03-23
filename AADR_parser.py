#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar  5 09:41:55 2024

@author: ariannaalamshahi
"""
'''
AADR_parser.py

Description: This program create two dataframes which are subsets of the AADR dataset.
One dataframe will be contain entries which feature Y-haplogroup information and the other
will contain entries which feature mitochondrial DNA haplogroup information.

User-defined functions: none

Non-standard modules: pandas, re
    
Procedure:
    1. Read the input file in as a dataframe
    2. Remove irrelevant columns from the dataframe
    4. Convert years so they are no longer based on year befores 1950
    5. Create two separate dataframes (one for Y one for mitochondrial)
    6. Remove entries which don't include haplogroup information from each dataframe
    7. Fix haplogroup formatting issues
    8. Output the files

Input: AADR Annotation.xlsx
Output: dataframes will be outputted to AADR_y.txt and AADR_mt.txt in the 01_CleanData directory

Usage: python3 AADR_parser.py Raw_Data/AADR_54.1/AADR\ Annotation.xlsx

Version: 1.00
Date: 2023-03-23
Name: Arianna Alamshahi

'''
#%% Installs & Imports
import sys
from pathlib import Path

import pandas as pd # version 2.2.0

import re # version 2.2.1 


#%% Error handling hub - for custom exceptions! 

class WrongNumberArguments(Exception):
    pass

class WrongFormat(Exception):
    pass

#%%% Run from the command like: Python3 AADR_Parser.py AADR Annotation.xlsx

# will only run if 2 arguments are provided and if input (AADR) is a file
if len(sys.argv) == 2 and Path(sys.argv[1]).is_file():
    
    aadr_parser = sys.argv[0] # AADR_parser.py
    
    input_aadr = sys.argv[1] # AADR Annotation.xlsx, input
    
    # no output, in the command line! It will automatically output AADR_y.txt and AADR_mt_txt. User cannot rename, safeguard for clarity
    
# Error handling! If greater than or less than 3 arguments are provided
try:
    if len(sys.argv) > 2 or len(sys.argv) < 2:
        raise WrongNumberArguments
except WrongNumberArguments:
    print("Error! You provided more/less than 2 arguments. Goodbye!")
    sys.exit()
    

#%% Load the data
AADR_df = pd.read_excel(input_aadr)

# Error handling! If a different number of columns in the file
try:
    if len(AADR_df.columns) != 36:
        raise WrongFormat
except WrongFormat:
    print("Error! This doesn't look like an AADR file that I can parse. Goodbye!")
    sys.exit()
    

#%% Clean up df for mtDNA

# Remove unecessary columns
AADR_df = AADR_df.drop(['#', 'Master ID','Skeletal code', 'Skeletal element', 
                        "Year data from this individual was first published [for a present-day individuals we give the data of the data reported here; missing GreenScience 2010 (Vi33.15, Vi33.26), Olalde2018 (I2657), RasmussenNature2010 (Australian)]",
                        'Publication',
                        'Method for Determining Date; unless otherwise specified, calibrations use 95.4% intervals from OxCal v4.4.2 Bronk Ramsey (2009); r5; Atmospheric data from Reimer et al (2020)',
                        'Date standard deviation in BP [OxCal sigma for a direct radiocarbon date, and standard deviation of the uniform distribution between the two bounds for a contextual date]',
                        'Full Date One of two formats. (Format 1) 95.4% CI calibrated radiocarbon age (Conventional Radiocarbon Age BP, Lab number) e.g. 2624-2350 calBCE (3990±40 BP, Ua-35016). (Format 2) Archaeological context range, e.g. 2500-1700 BCE',
                        'Age at Death from physical anthropology', 'Group ID', 'Lat.', 'Long.', 'Pulldown Strategy',
                        'Data source', 'No. Libraries', '1240k coverage (taken from original pulldown where possible)',
                        'SNPs hit on autosomal targets (Computed using easystats on 1240k snpset)', 'SNPs hit on autosomal targets (Computed using easystats on HO snpset)',
                        'Molecular Sex', 'Family ID and position within family', 'Y haplogroup (manual curation in terminal mutation format)',
                        'mtDNA coverage (merged data)','mtDNA match to consensus if >2x (merged data)',
                        'Damage rate in first nucleotide on sequences overlapping 1240k targets (merged data)',
                        'Sex ratio [Y/(Y+X) counts] (merged data)', 'Library type (minus=no.damage.correction, half=damage.retained.at.last.position, plus=damage.fully.corrected, ds=double.stranded.library.preparation, ss=single.stranded.library.preparation)',
                        'Libraries', 'ASSESSMENT', 'ASSESSMENT WARNINGS (Xcontam interval is listed if lower bound is >0.005, "QUESTIONABLE" if lower bound is 0.01-0.02, "QUESTIONABLE_CRITICAL" or "FAIL" if lower bound is >0.02) (mtcontam confidence interval is listed if coverage >2 and upper bound is <0.'],
                       axis=1)

# Rename columns
AADR_df = AADR_df.rename(columns = {"Genetic ID":"Gen_ID", "Date mean in BP in years before 1950 CE [OxCal mu for a direct radiocarbon date, and average of range for a contextual date]":"DateMean", "Political Entity":"Pol_En", "Y haplogroup (manual curation in ISOGG format)" : "Y_hapgrp", "mtDNA haplogroup if >2x or published" : "Mt_hapgrp"}) 

#%%% Adjust date column entries

# need to disable pandas warning "A value is trying to be set on a copy of a slice from a DataFrame"
pd.options.mode.chained_assignment = None  # default='warn'

for ind in AADR_df.index: # look through the dataframe by index
    
    # replace each date entry with 1950-meandate! Positives indicate CE, negatives indicate BCE
    AADR_df['DateMean'][ind] = (1950 - AADR_df['DateMean'][ind])
    
    
#%%% Create two seperate dataframes, one for y chromosome & one for mtDNA     

AADR_y_df = AADR_df.drop(["Mt_hapgrp"], axis = 1)

AADR_mt_df = AADR_df.drop(["Y_hapgrp"], axis = 1)

#%%% Remove entries without haplgroup information

for ind in AADR_y_df.index: # look through the dataframe by index

    # if the haplogroup field is blank or not applicable
    if AADR_y_df.loc[ind, "Y_hapgrp"] == ".." or "n/a" in str(AADR_y_df.loc[ind, "Y_hapgrp"]) or "na" in str(AADR_y_df.loc[ind, "Y_hapgrp"]) or "not published" in str(AADR_y_df.loc[ind, "Y_hapgrp"]) or AADR_y_df.loc[ind, "Y_hapgrp"] == "":
        
        # Remove the whole row from the dataframe (via index)
        AADR_y_df = AADR_y_df.drop([ind])


for ind in AADR_mt_df.index: # look through the dataframe by index

    # if the haplogroup field is blank or not applicable
    if AADR_mt_df.loc[ind, "Mt_hapgrp"] == ".." or "n/a" in str(AADR_mt_df.loc[ind, "Mt_hapgrp"]) or "na" in str(AADR_mt_df.loc[ind, "Mt_hapgrp"]) or "or" in str(AADR_mt_df.loc[ind, "Mt_hapgrp"]):
        
        # Remove the whole row from the dataframe (via index)
        AADR_mt_df = AADR_mt_df.drop([ind])
        
        
#%%% Fix the format of remaining haplogroups remove characters that don't exist in the y_tree

for ind in AADR_y_df.index: # look through the dataframe by index
        
    # Remove straggler spaces from the ends of Y haplogroups
    AADR_y_df.loc[ind, "Y_hapgrp"] = AADR_y_df.loc[ind, "Y_hapgrp"].strip()
    
    
    # If the haplogroup doesn't match the typical format
    if AADR_y_df.loc[ind, "Y_hapgrp"] != re.search('^[\w\d]*',AADR_y_df.loc[ind, "Y_hapgrp"]).group(0):
        
        
        # if the haplogroup includes any of these characters
        if "-" or ";" or "(" or "~" or "*" or "?" or "'" in AADR_y_df.loc[ind, "Y_hapgrp"]: 
            
            
            # if there is a space before the parantheses
            if re.match('[\S]*[\s][(]', AADR_y_df.loc[ind, "Y_hapgrp"]):
                
                # then replace the space + dash/parantheses and everything after with a blank
                AADR_y_df.loc[ind, "Y_hapgrp"] = re.sub('[\s][()][\s\S]*',"", AADR_y_df.loc[ind, "Y_hapgrp"], count=1)
                
                
            # if there is no space before the parantheses, tilde, asterisk, question mark, or apostrophe
            if re.match("[\S]*[(~*?']", AADR_y_df.loc[ind, "Y_hapgrp"]):
                
                # then replace the symbol and everything after with a blank
                AADR_y_df.loc[ind, "Y_hapgrp"] = re.sub("[()~*?'][\S\s]*","", AADR_y_df.loc[ind, "Y_hapgrp"], count=1)
            
            
            # if there is a semicolon (e.g E2(xE2b); E-M75)
            if re.match('[\S]*[;]', AADR_y_df.loc[ind, "Y_hapgrp"]):
                
                # then replace the semi-colon and everything after it with a blank
                AADR_y_df.loc[ind, "Y_hapgrp"] = re.sub('[;][\s][\S\s]*', "", AADR_y_df.loc[ind, "Y_hapgrp"], count=1)
        
        
            # if there is still a space
            if " " in AADR_y_df.loc[ind, "Y_hapgrp"]: 
                
                # and if it matches the following format (e.g E1b1b1b1a1 E-M183)
                if re.match('[\S]*[ ]', AADR_y_df.loc[ind, "Y_hapgrp"]):
                    
                    # then replace the space and everything after with a blank
                    AADR_y_df.loc[ind, "Y_hapgrp"] = re.sub('[ ][\s\S]*',"", AADR_y_df.loc[ind, "Y_hapgrp"], count=1)
                    
              
                    
#%%% Fix the format of remaining haplogroups, remove characters that don't exist in the mt_tree

for ind in AADR_mt_df.index: # look through the dataframe by index
        
    # Remove straggler spaces from the ends of Mt haplogroups
    AADR_mt_df.loc[ind, "Mt_hapgrp"] = AADR_df.loc[ind, "Mt_hapgrp"].strip()
    
    #print(AADR_df.loc[ind, "Mt_hapgrp"])

    # If the haplogroup doesn't match the typical format
    if AADR_mt_df.loc[ind, "Mt_hapgrp"] != re.search('^[\w\d]*',AADR_mt_df.loc[ind, "Mt_hapgrp"]).group(0):
        
        
        # if the haplogroup includes any of these characters
        if "+" or ";" or "�" or "’" or "/" or "(" or "*" in AADR_mt_df.loc[ind, "Mt_hapgrp"]:
            
            
            # if there is a space before the parantheses, slash, or addition sign
            if re.match('[\S]*[\s][(/+]', AADR_mt_df.loc[ind, "Mt_hapgrp"]):
                
                # then replace the space + parantheses/slash/addition sign and everything after with a blank
                AADR_mt_df.loc[ind, "Mt_hapgrp"] = re.sub('[\s][()+/][\s\S]*',"", AADR_mt_df.loc[ind, "Mt_hapgrp"], count=1)

                
            # if there is no space before the character
            if re.match("[\S]*[(*+;’�]", AADR_mt_df.loc[ind, "Mt_hapgrp"]):
                
                # then replace the symbol and everything after with a blank
                AADR_mt_df.loc[ind, "Mt_hapgrp"] = re.sub("[()*+;’�][\S\s]*","", AADR_mt_df.loc[ind, "Mt_hapgrp"])

        
        
            # if there is still a space
            if " " in AADR_mt_df.loc[ind, "Mt_hapgrp"]: 
                
                # and if it matches the following format (e.g E1b1b1b1a1 E-M183)
                if re.match('[\S]*[ ]', AADR_mt_df.loc[ind, "Mt_hapgrp"]):
                    
                    # then replace the space and everything after with a blank
                    AADR_mt_df.loc[ind, "Mt_hapgrp"] = re.sub('[ ][\s\S]*',"", AADR_mt_df.loc[ind, "Mt_hapgrp"], count=1)
              
                    
#%% Output to txt file

AADR_y_df.to_csv('01_CleanData/AADR_y.txt', sep='\t', index=False)

AADR_mt_df.to_csv('01_CleanData/AADR_mt.txt', sep='\t', index=False)

        
