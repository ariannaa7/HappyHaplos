# Load packages! ####

library(readxl) # version 1.4.3, read in aadr and mt tree
library(shiny) # version 1.8.0, create a user interface
library(tidyverse) # version 2.0.0, df handling (e.g. pivot longer)
library(dplyr) # version 1.1.4, df handling (e.g. filters)
library(giscoR) # version 0.4.0, pull shapefile to make map
library(sf) # version 1.0-15, used to read shapefiles!
library(leaflet) # version 2.2.1, heatmap
library(gt) # version 0.10.1, used to display a table with country flags


# Set the working directory ####

setwd("~/PopulationGeneticsProject/")

# Load the data in! ####

# complete aadr file before python parser, use to pull country populations!
aadr_complete = read_excel("Raw_Data/AADR_54.1/AADR Annotation.xlsx")

# parsed AADR files
aadr_y = read.csv("01_CleanData/AADR_y.txt", header=T, sep="\t")
aadr_mt = read.csv("01_CleanData/AADR_mt.txt", header=T, sep="\t")

# tree files
y_tree = read.csv("Raw_Data/AncientYDNA/chrY_hGrpTree_isogg2016.txt", header=T, sep="\t")
mt_tree = as.data.frame(read_excel("01_CleanData/mt_phyloTree_b17_Tree2.xlsx"))

# pull a shapefile for the world, includes countries/codes & geometry 
countries_shp = gisco_get_countries()

# Data prep! ####

# rename the columns in the tree files
colnames(y_tree)=c("Branch","Parent")
colnames(mt_tree)=c("Branch","Parent")

# find haplogroups that appear in both the aadr and tree files
commonHP_y = aadr_y$Y_hapgrp[aadr_y$Y_hapgrp %in% y_tree$Parent & aadr_y$Y_hapgrp %in% y_tree$Branch]
commonHP_mt = aadr_mt$Mt_hapgrp[aadr_mt$Mt_hapgrp %in% mt_tree$Parent & aadr_mt$Mt_hapgrp %in% mt_tree$Branch]

# Filter the aadr files only for entries where the haplogroup is also present in the associated tree file
filtered_aadr_y = filter(aadr_y, Y_hapgrp %in% commonHP_y)
filtered_aadr_mt = filter(aadr_mt, Mt_hapgrp %in% commonHP_mt)

# merge the aadr_y and aadr_mt dataframes into one
aadr = bind_rows(filtered_aadr_y, filtered_aadr_mt)

# Fix discrepancies between country names in aadr and countries_shp

# changes to aadr
for (row in 1:nrow(aadr)) {
  # space after Azerbaijan
  if (aadr[row,"Pol_En"] == "Azerbaijan ") {
    aadr[row,"Pol_En"] = "Azerbaijan"
  }
  # space after China
  if (aadr[row,"Pol_En"] == "China ") {
    aadr[row,"Pol_En"] = "China"
  }
  # typo in Curaçao
  if (aadr[row,"Pol_En"] == "Curacao") {
    aadr[row,"Pol_En"] = "Curaçao"
  }
  # space after Czechia
  if (aadr[row,"Pol_En"] == "Czechia ") {
    aadr[row,"Pol_En"] = "Czechia"
  }
  # rename Czech Republic to match shapefile
  if (aadr[row,"Pol_En"] == "Czech Republic") {
    aadr[row,"Pol_En"] = "Czechia"
  }
  # space after Turkey
  if (aadr[row,"Pol_En"] == "Turkey ") {
    aadr[row,"Pol_En"] = "Turkey"
  }
  # typo in Germany
  if (aadr[row,"Pol_En"] == "Gernamy") {
    aadr[row,"Pol_En"] = "Germany"
  }
  # rename Federated States of Micronesia to match shapefile
  if (aadr[row,"Pol_En"] == "Federated States of Micronesia") {
    aadr[row,"Pol_En"] = "Micronesia"
  }
  # rename St. Lucia to match shapefile
  if (aadr[row,"Pol_En"] == "St. Lucia") {
    aadr[row,"Pol_En"] = "Saint Lucia"
  }
  # rename USA to match shapefile
  if (aadr[row,"Pol_En"] == "USA") {
    aadr[row,"Pol_En"] = "United States"
  }
}

# changes to complete AADR (will pull populations from this, need names to match regular aadr)
for (row in 1:nrow(aadr)) {
  # space after Azerbaijan
  if (aadr_complete[row,"Political Entity"] == "Azerbaijan ") {
    aadr_complete[row,"Political Entity"] = "Azerbaijan"
  }
  # space after China
  if (aadr_complete[row,"Political Entity"] == "China ") {
    aadr_complete[row,"Political Entity"] = "China"
  }
  # typo in Curaçao
  if (aadr_complete[row,"Political Entity"] == "Curacao") {
    aadr_complete[row,"Political Entity"] = "Curaçao"
  }
  # space after Czechia
  if (aadr[row,"Pol_En"] == "Czechia ") {
    aadr[row,"Pol_En"] = "Czechia"
  }
  # rename Czech Republic to match shapefile
  if (aadr_complete[row,"Political Entity"] == "Czech Republic") {
    aadr_complete[row,"Political Entity"] = "Czechia"
  }
  # space after Turkey
  if (aadr_complete[row,"Political Entity"] == "Turkey ") {
    aadr_complete[row,"Political Entity"] = "Turkey"
  }
  # typo in Germany
  if (aadr_complete[row,"Political Entity"] == "Gernamy") {
    aadr_complete[row,"Political Entity"] = "Germany"
  }
  # rename Federated States of Micronesia to match shapefile
  if (aadr_complete[row,"Political Entity"] == "Federated States of Micronesia") {
    aadr_complete[row,"Political Entity"] = "Micronesia"
  }
  # rename St. Lucia to match shapefile
  if (aadr_complete[row,"Political Entity"] == "St. Lucia") {
    aadr_complete[row,"Political Entity"] = "Saint Lucia"
  }
  # rename USA to match shapefile
  if (aadr_complete[row,"Political Entity"] == "USA") {
    aadr_complete[row,"Political Entity"] = "United States"
  }
}

# changes to shapefile
for (row in 1:nrow(countries_shp)) {
  
  # rename Democratic Republic of The Congo to match aadr
  if (countries_shp$NAME_ENGL[row] == "Democratic Republic of The Congo") {
    countries_shp$NAME_ENGL[row] = "DR Congo"
  }
  # rename Russian Federation to match aadr
  if (countries_shp$NAME_ENGL[row] == "Russian Federation") {
    countries_shp$NAME_ENGL[row] = "Russia"
  }
  # rename United Republic of Tanzania to match aadr
  if (countries_shp$NAME_ENGL[row] == "United Republic of Tanzania") {
    countries_shp$NAME_ENGL[row] = "Tanzania"
  }
  # update IS02 code so country flag will be pulled for tableView
  if (countries_shp$CNTR_ID[row] == "UK") {
    countries_shp$CNTR_ID[row] = "GB"
  }
}

# Country populations ####

# create a dataframe of just one column with all the country entries in complete aadr
countries_df = data.frame(aadr_complete$`Political Entity`)
names(countries_df) = "Political Entity" # change the column header to "Political Entity"

# add a column to the dataframe with 1 in each cell, signifying 1 haplogroup occurrence
countries_df = cbind(countries_df, "n" = rep(1, times = nrow(countries_df)))

# count occurrences of each country based off of values in n column
population_df = countries_df %>% count(`Political Entity`, name = "pop")

# remove the row symbolizing blank entry for country
population_df = population_df[!(population_df$`Political Entity` %in% ".."),]

# Haplogroup selection prep ####

# put all the haplogroups from aadr into lists to eventually have user select from
hpDropdown_y = sort(unique(commonHP_y))
hpDropdown_mt = sort(unique(commonHP_mt))

# Define User Interface (UI)----
ui = fluidPage(
  
  # App title ----
  titlePanel("HappyHaplos: track haplogroup lineage and frequency per country"),
  
  # Establish sidebar----
  sidebarLayout(
    
    # Sidebar panel (inputs) ----
    sidebarPanel(

      # Input: type of haplogroup (y or mtDNA) ----
      radioButtons(
        inputId = "DNA_type", # use this input ID to refer to user's selection
        label = "Do you want to trace lineage through the Y-chromosome DNA (paternal) or Mitochondrial DNA (maternal)? ", # instructions to the user
        choices = c("Y-chromosome DNA", "Mitochondrial DNA"), # options for user, default is Y-chromosome
        inline = TRUE # will show choices horizontally
      ),
  
      # Dynamically generated - will show haplogroup options based on DNA type (y or mtDNA) selection
      uiOutput("hpgrpOptions"),
      
      # Dynamically generated - will list haplogroups in the lineage, based on ancestry_lineage output
      uiOutput("hpgrpSelector")
    ),
    
    # Main panel (outputs) ----
    mainPanel(
      tabsetPanel(
        tabPanel("Lineage", # tab name is "Lineage"
                 htmlOutput("lineage") # outputs the lineage
        ),
        tabPanel("Frequency", # tab name is "Frequency"
                 htmlOutput("frequencyExplanation"), # preface message
                 leafletOutput("heatmap", width = "120vh", height = "60vh"), # the heatmap
                 gt_output("tableView") # the table view (with flags!)
        )
      )
    )
  )
)

# Define server ----
server = function(input, output) { 
  
  # Create a dropdown of haplogroup values for user to select based on DNA type choice ----
  
  output$hpgrpOptions = renderUI({
  
    if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
      selectizeInput( # dropdown
        inputId = "usrHPGRP", # how to call user's selection
        label = "Select the haplogroup you would like to determine the lineage for: ", # instructions to the user
        choices = hpDropdown_y # options provided to the user
        )
    }
    
    else if (input$DNA_type == "Mitochondrial DNA") { # if the user selected mitochondrial DNA 
      selectizeInput( # dropdown
        inputId = "usrHPGRP", # how to call user's selection
        label = "Select the haplogroup you would like to determine the lineage for: ",  # instructions to the user
        choices = hpDropdown_mt # options provided to the user
        )
    }

  })

  # Find the parental haplogroups! ----
  
  ancestry_lineage = reactive({
      
      # create a vector to store parent haplogroups, begin with user haplogroup
      lineage = c(input$usrHPGRP)
      
      # store the user's haplogroup here, it will be overwritten
      tempHPGRP = input$usrHPGRP
      
      if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
        
        # loop through the tree until you reach root
        while (tempHPGRP!="Root"){ # as long as the temporary haplogroup isn't root
          
          # then the temporary haplogroup should be:
          tempHPGRP = y_tree$Parent[y_tree$Branch==tempHPGRP] # where the tempHPGRP matches Branch, pull the parent
          
          # append that parent to the parent vector
          lineage = append(lineage, tempHPGRP)
        }
      }
      else if (input$DNA_type == "Mitochondrial DNA") { # if the user selected mtDNA
        
        # loop through the tree until you reach mt-MRCA
        while (tempHPGRP!="mt-MRCA"){ # as long as the temporary haplogroup isn't root
          
          # then the temporary haplogroup should be:
          tempHPGRP = mt_tree$Parent[mt_tree$Branch==tempHPGRP] # where the tempHPGRP matches Branch, pull the parent
          
          # append that parent to the parent vector
          lineage = append(lineage, tempHPGRP)
        }
      }

    return(lineage)
  })
  
  # Print lineage to screen ----
  
  output$lineage = renderText({
    
    # make a vector to store the lineage starting from root
    lineage = rev(ancestry_lineage())
    
    # <br> is newline character equivalent in HTML
    intro_text = "<br>Here is the lineage for the haplogroup you selected:&emsp; "
    
    # create an empty string to store tabs (\t) before the haplogroup
    tabs_hp = ""
    
    # go through lineage vector by index
    for (i in 1:length(lineage)) {
      
      # Adjust the number of tabs for each iteration
      tabs = paste(rep("&emsp;", times = i - 1), collapse = "")  
      
      # append the tabs to the haplogroup by index!
      tabs_hp[i] = paste0(tabs, lineage[i])
    }
    
    # put it all together so each haplogroup prints on a new line with one more tab before it than the prior haplogroup
    lineage_waterfall = paste0(intro_text, "<br><br>", paste("<span><strong>", tabs_hp, collapse = "<br>"))
    return(lineage_waterfall)
  })
  
  # Create a list of haplogroups in the lineage that also exist in AADR, for the next step (radio buttons) ----
  
  lineageAADR = reactive({
    
    lineage = rev(ancestry_lineage()) # pull the lineage (in reverse so most ancient at top) instead of altering original
    
    if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
      commonHP = lineage[lineage %in% commonHP_y] # then create a vector that stores y-haplogroups from lineage in common with haps found in aadr
    }
    
    else if (input$DNA_type == "Mitochondrial DNA") { # if the user selected mitochondrial DNA 
      commonHP = lineage[lineage %in% commonHP_mt] # then create a vector that stores mt-haplogroups from lineage in common with haps found in aadr
    }
    
    return(commonHP)
  })
  
  # Create radio buttons of ancestry_lineage values for user to select haplogroup they want to see map/table for ----
  
  output$hpgrpSelector = renderUI({
    # Create a selectInput for haplogroup selection
    radioButtons(
      inputId = "selectedHPGRP", # use this input ID to refer to user's selection
      label = "Select a haplogroup in the lineage to see its frequency:", # instructions to the user
      choices = lineageAADR(), # options provided to the user
      inline = FALSE # will show choices vertically
    )
    
  })
  
  # Find each country linked to the haplogroup lineage ----
  
  # Create an aadr subset that only includes haplogroup matches 
  HpHits_AADRsubset = reactive({
    if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
      HP_filtered_aadr = filter(aadr, Y_hapgrp %in% ancestry_lineage()) # then create a subset that only pulls aadr haplogroup matches
    }
    else if(input$DNA_type == "Mitochondrial DNA") { # if the user selected mtDNA
      HP_filtered_aadr = filter(aadr, Mt_hapgrp %in% ancestry_lineage()) # then create a subset that only pulls aadr haplogroup matches
    }
    return(HP_filtered_aadr)
  })
  
  # Create a dataframe that will store each country, # of haplogroups in country, # of specific haplogroup in country ----
  
  country_pop_df = reactive ({
  
    countries = unique(HpHits_AADRsubset()$Pol_En) # look through aadr subset and pull one instance of each country name
    
    pop = rep(0,length(countries)) # create a vector that has as many zeros as countries
    
    country_pop = data.frame("country" = countries) # dataframe with one column called "countries", lists countries associated with lineage
    
    # join the countries and their populations in a dataframe
    country_pop = left_join(country_pop, population_df, # recall that population_df comes from the COMPLETE aadr
                           by = join_by(x$country == y$`Political Entity`))
    
    country_pop[ancestry_lineage()] = 0 # Create a column for each haplogroup of interest
    
    return(country_pop)
  })
  
  
  # Filter aadr for our countries of interest to decreasing computing power needed for the loops ----
  
  CountryHit_AADRsubset = reactive({
    if (input$DNA_type == "Y-chromosome DNA") {  # if the user selected y chromosome
      Country_filtered_aadr = filter(aadr, Pol_En %in% country_pop_df()$country & !is.na(Y_hapgrp)) #if Y_hapgrp isn't NA, then it has an entry
    }
    else if (input$DNA_type == "Mitochondrial DNA") { # if the user selected mtDNA
      Country_filtered_aadr = filter(aadr, Pol_En %in% country_pop_df()$country & !is.na(Mt_hapgrp)) #if Mt_hapgrp isn't NA, then it has an entry
    }
    return(Country_filtered_aadr)
  })
  
  
  # Add number of haplogroups of interest which originate from each country to the data frame----
  
  hapsOfInterest_per_country = reactive ({
    
    # Copy country_pop_df instead of modifying the original
    country_pop_dfFREQS = country_pop_df()
    
    # loop through the haplogroups in our lineage
    for (hp in ancestry_lineage()) {
      
      # loop through our country_pop data frame
      for (crow in 1:nrow(country_pop_dfFREQS)) {
        
        # create a temporary variable to store haplogroup frequency for each haplogroup per country
        temp_hapgrpfrq = 0
        
        # loop through aadr subset
        for (row in 1:nrow(CountryHit_AADRsubset())) {
          
          # if you find a country match 
          if (country_pop_dfFREQS[crow, "country"] == CountryHit_AADRsubset()[row, "Pol_En"]) {
            
            if (input$DNA_type == "Y-chromosome DNA") {  # if the user selected y chromosome
              
              # if you find a haplogroup match
              if (hp == CountryHit_AADRsubset()[row, "Y_hapgrp"]) {
                
                # then we know there is an occurrence of that haplogroup in that country
                temp_hapgrpfrq = temp_hapgrpfrq + 1 # so up the count by 1
              }
              
            } else if (input$DNA_type == "Mitochondrial DNA") {  # if the user selected mt DNA
              
              # if you find a haplogroup match
              if (hp == CountryHit_AADRsubset()[row, "Mt_hapgrp"]) {
                
                # then we know there is an occurrence of that haplogroup in that country
                temp_hapgrpfrq = temp_hapgrpfrq + 1 # so up the count by 1
              }
            }
          }
        }
        # add each haplogroup count to the corresponding country/haplogroup
        country_pop_dfFREQS[crow, hp] = temp_hapgrpfrq
        
        #reset
        temp_hapgrpfrq = 0
      }
    }
    return(country_pop_dfFREQS)
  })
  
  
  # Calculate haplogroup frequency per country ----
  
  hap_freq_df = reactive ({
    
    # Create a dataframe to store haplogroup frequency per country
    freq_df = data.frame(matrix(nrow = 0, ncol = length(country_pop_df()$country))) # empty dataframe with as many columns as countries with the haplogroups
    
    colnames(freq_df) = (hapsOfInterest_per_country()$country) # the columns should be named after countries
    
    # Loop through the haplogroups
    for (hp in ancestry_lineage()) {
      
      # Loop through the dataframe with haplogroup counts
      for (crow in 1:nrow(hapsOfInterest_per_country())) {
        
        # Calculate the frequency of the haplogroup per country
        freq = as.numeric((hapsOfInterest_per_country()[crow,hp] / hapsOfInterest_per_country()$pop[crow]) * 100)
        
        # Round to two decimals
        freq = round(freq, digits = 2)
        
        # For the associated country name column, add the frequency
        freq_df[hp,hapsOfInterest_per_country()$country[crow]] = freq
      }
    }
    
    # Remove the haplogroups as row names
    rownames(freq_df) = NULL
    
    # Create a column for haplogroups at the start of the dataframe
    freq_df = cbind("Haplogroup" = ancestry_lineage(),freq_df)
    
    # Turn the haplogroup frequency table into long 
    freq_df_long = pivot_longer(data = freq_df, cols = 2:ncol(freq_df), names_to = "Country", values_to = "HpFrequency")
    
    return(freq_df_long)
  })
  

  # Create a message to display on frequency page! ----
  
  output$frequencyExplanation = renderText({
    "<br>Use the interactive heatmap and the table to explore the haplogroup frequency per country for the selected haplogroup!<br><br>
    <span><strong>See a haplogroup in the lineage but not as an option to explore frequency?</strong></span><br>Unfortunately, while this haplogroup does exist in the lineage, 
    we don't have any data for it in the Allen Ancient DNA Resource (AADR).<br><br>"
  })
  
  
  # Heatmap shapefile prep ----
 
  # pull haplogroup frequencies for the haplogroup that user selected to see frequency of
   selected_hap_freqs = reactive({
    filltered_hapfreq = filter(hap_freq_df(), Haplogroup == input$selectedHPGRP) # only pull rows of hap_freq_df where the haplogroup matches what the user picked
    return(filltered_hapfreq)
  })
  
  # Add two columns to the shapefile to store haplogroup frequency per country for the haplogroup that the user chooses
  map_selectedHPGRP = reactive({
    map_subset = left_join(countries_shp, selected_hap_freqs(),
                           by = join_by(x$NAME_ENGL == y$Country)) %>% # join selected haps & freqs to the shapefile based off of country name
      mutate(HpFrequency = if_else(is.na(HpFrequency), 0, HpFrequency)) # if frequency is NA, then change value to 0! Important for heatmap legend for close to beginning haplos (e.g. a1b)
    
    return(map_subset)
  })
  
  # Pull the earliest emergence of each haplogroup for each country
  pullAncientYear = reactive({
    
    if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
      HpHits_AADRsubset_earliest = HpHits_AADRsubset() %>% # use the subset that only shows entries with haplogroup hits
        group_by(Y_hapgrp, Pol_En) %>% # pull smallest value in DateMean for each haplogroup in each country
        summarise(earliestYear = min(DateMean)) # then determine the smallest value in the DateMean column for each group
    }
    else if(input$DNA_type == "Mitochondrial DNA") { # if the user selected mtDNA
      HpHits_AADRsubset_earliest = HpHits_AADRsubset() %>% # use the subset that only shows entries with haplogroup hits
        group_by(Mt_hapgrp, Pol_En) %>% # group by haplogroup AND country
        summarise(earliestYear = min(DateMean)) # pull smallest value in DateMean for each haplogroup in each country
    }
    
    return(HpHits_AADRsubset_earliest)
  })
  
  # Convert years to BCE and CE
  
  ancientYearConvert = reactive({
    
    # save into new data frame so we don't alter original
    ancientYearsDF = pullAncientYear() %>% 
      mutate(earliestYear = ifelse(earliestYear < 0, paste(abs(earliestYear), "BCE", sep = " "), # if negative, take abs value and add BCE
                                   ifelse(earliestYear > 0, paste(earliestYear, "CE", sep = " "), as.character(earliestYear)) # if positive, add CE
                                   )
             ) 
                                   
    return(ancientYearsDF)
  })
  
  # Add a column to the shapefile to store earliest emergence (year) of that haplogroup in that country!
  map_selectedHPGRP_yearEmerged = reactive({
    
    if (input$DNA_type == "Y-chromosome DNA") { # if the user selected y chromosome
      map_subset = left_join(map_selectedHPGRP(), ancientYearConvert(),
                             by = join_by(x$Haplogroup == y$Y_hapgrp, x$NAME_ENGL == y$Pol_En)) # join ancientYearConvert to the shapfile based on haplogroup & country
    }
    else if(input$DNA_type == "Mitochondrial DNA") { # if the user selected mtDNA
      map_subset = left_join(map_selectedHPGRP(), ancientYearConvert(),
                             by = join_by(x$Haplogroup == y$Mt_hapgrp, x$NAME_ENGL == y$Pol_En)) # join ancientYearConvert to the shapfile based on haplogroup & country
    }
    return(map_subset)
  })
  
  
  # Heatmap! ----
  
  # Heat map colors will depend on haplogroup frequency
  pal_hap_frq = reactive({
    colorNumeric(rev(heat.colors(25)), map_selectedHPGRP_yearEmerged()$HpFrequency) # reverse the palette so that red indicates higher frequency and white indicates lower
  })
  
  # Create a label that will pop up when user clicks on a country on the map
  label_country = function(HP, HP_Freq, country, yearEmerged) {
    str_glue("The ", "<span><strong>{HP}</strong></span>", " haplogroup has a frequency of " ,
             "<span><strong>{HP_Freq}%</strong></span>",
             " in <span><strong>{country}</strong></span>", " and emerged in <span><strong>{yearEmerged}</strong></span>")
  }
  
  # Alternate label for if haplogroup = NA
  label_country_NA = function(HP, HP_Freq, country) {
    
    # if the haplogroup is NA, replace it with the selected haplogroup name, if not then leave it as HP_Freq
    HP_NA_adj = ifelse(is.na(HP), input$selectedHPGRP, HP)
    str_glue("The ", "<span><strong>{HP_NA_adj}</strong></span>", " haplogroup has a frequency of " ,
             "<span><strong>{HP_Freq}%</strong></span>",
             " in <span><strong>{country}</strong></span>")
  }
  
  # Create a heatmap using leaflet!
  output$heatmap = renderLeaflet({
    map_selectedHPGRP_yearEmerged() %>% # base it off this shapefile
      leaflet() %>% 
      setView(lng = 40, lat = 30, zoom = 0.5) %>% # slight zoom on map when it opens, default is very zoomed out
      addPolygons(weight = 1,
                  color = "black", #outline country borders in black
                  
                  # if haplogroup frequency is 0, country should colored gray, otherwise, use the heatmap color gradient
                  fillColor = ~ifelse(HpFrequency == 0, "lightgrey", pal_hap_frq()(HpFrequency)),
                  fillOpacity = 0.9, # opacity of country color
                  
                  #display popup specific to if NA present or not!
                  popup = ~ifelse(is.na(Haplogroup), label_country_NA(Haplogroup, HpFrequency, NAME_ENGL),
                                  label_country(Haplogroup, HpFrequency, NAME_ENGL, earliestYear))) %>%
      
      addLegend(pal = pal_hap_frq(),
                values = ~HpFrequency, # if kept NA values in shapefile then: values = ~HpFrequency[!is.na(HpFrequency)]
                opacity = 0.9,
                title = paste(input$selectedHPGRP, "haplogroup<br>frequency per country", sep = " ") # legend title
      )
  })
  
  
  
  # Create a table view of haplogroup frequency! ----
  
  # Create a long table of country, haplogroup, and occurrences
  hapLong = reactive({
    
    noPopCol = select(hapsOfInterest_per_country(), -pop) # pull all columns except for pop
    
    long =  pivot_longer(noPopCol, cols = 2:ncol(noPopCol), names_to = "haplogroup", values_to = "hapOccurrences") # change to long format
    
    return(long)
  })
  
  
  # Create subset of map_selectedHPGRP that only shows rows that have haplogroup present
  map_hapOccurrences = reactive({
    
    # filter for entries that aren't NA
    map_subset = filter(map_selectedHPGRP(), !is.na(Haplogroup))
    
    # add total population for country
    map_subset = left_join(map_subset, population_df,
                           by = join_by(x$NAME_ENGL == y$`Political Entity`))
    
    # add occurrence of haplogroup
    map_subset = left_join(map_subset, hapLong(),
                           by = join_by(x$NAME_ENGL == y$country, x$Haplogroup == y$haplogroup))
    
    return(map_subset)
  })
  
  
  # Table View
  tableView = reactive({
    tableView.df = as.data.frame(map_hapOccurrences()) # convert to dataframe so we can drop the geometry field
    
    tableView.df = tableView.df %>% select(CNTR_ID, NAME_ENGL, pop, hapOccurrences, HpFrequency) # only include these columns in the df
    
    tableView.df = arrange(tableView.df, desc(HpFrequency)) # Sorts table by highest to lowest frequency
    
    names(tableView.df) = c(' ','Country', 'Population', 'Haplogroup occurrences', 'Haplogroup frequency') # rename the columns
    
    tableView.df = tableView.df %>% relocate('Haplogroup occurrences', .before = 'Population') # relocate the columns
    
    tableView.df = filter(tableView.df, `Haplogroup occurrences` > 0 ) # only show row if there is an occurrence of that haplogroup
    
    # add a percentage for each haplogroup frequency cell 
    for (row in 1:nrow(tableView.df)) {
      tableView.df[row, "Haplogroup frequency"] = paste(tableView.df[row, "Haplogroup frequency"], "%", sep = "")
    }
    
    # convert to gt object, need this type of object to add the country flags
    tableView_gt = gt(tableView.df)
    
    tableView_gt = tab_style(data = tableView_gt,
                              style = cell_text(align = "center"), # center align the text in specified cells
                              locations = list(cells_column_labels(columns = -c(" ", "Country")), cells_body(columns = -c(" ", "Country"))) # in all the cells except flags & country (both body & header)
    )
    
    # add country flags!
    tableView_gt_flag = fmt_flag(
      tableView_gt,
      columns = " ", #this is ISO2, just don't want a column title
    )
    
    return(tableView_gt_flag)
  })
  
  # Create output for table view
  output$tableView = render_gt({
    tableView()
  })
  
}


shinyApp(ui = ui, server = server)

