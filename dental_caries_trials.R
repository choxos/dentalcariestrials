library(europepmc)
library(dplyr)
library(metareadr)
library(xml2)
library(stringr)


# Load the datasets
## Load International Standard Serial Numbers (ISSNs)
journals = read.csv("data/dental_journals.csv")

## Some modifications are needed...
ISSNs = paste0("ISSN:", journals$issn)

ISSNsQuery = paste(ISSNs, 
                    collapse = " OR ")

ISSNsQuery = paste(ISSNsQuery, 'OR (KW:"dental caries")')

## Load all open access dentistry and oral health papers indexed in PubMed 
## till the end of 2021:

db = epmc_search(
        query = paste0(
                "'(",
                ISSNsQuery,
                ") ",
                'AND (FIRST_PDATE:[1900-01-01 TO 2024-12-31])
    AND (OPEN_ACCESS:y)
    AND (PUB_TYPE:"Randomized Controlled Trial" OR PUB_TYPE:"Clinical Trial" OR PUB_TYPE:"Clinical Trial, Phase II" OR PUB_TYPE:"Clinical Trial, Phase III" OR PUB_TYPE:"Clinical Trial, Phase IV")',
                "'"
        ),
        limit = 100000 , output = "parsed", verbose = F
)


### Removing the duplicates:
db = db %>% distinct(pmid, .keep_all = TRUE)

### Removing "PMC" from the cells
db$pmcid_ =
        gsub("PMC", "", as.character(db$pmcid))

### Now, we make a folder for xml format articles and switch to that folder:
setwd("pmc")

### Next, we download xmls in format accessible with rtransparent. To skip 
### errors (i.e., The metadata format 'pmc' is not supported by the item or by 
### the repository.), first define a new function:
skipping_errors = function(x) tryCatch(mt_read_pmcoa(x), error = function(e) e)

### And then run the function:
sapply(db$pmcid_, skipping_errors)


filepath = dir(pattern=glob2rx("PMC*.xml"))

trial_databases <- c("LBCTR", "OMON", "DRKS", "CTRI", "ICTRP", "EUCTR", "REBEC", "AMCTR", "ISRCTN", "ANZCTR", "NCT", "ACTRN", "ChiCTR", "IRCT", "JPRN", "NTR", "TCTR", "UMIN")

trial_identifier = data.frame()

for (i in filepath){
        doc = read_xml(i)
        trial_numbers = character()
        
        for (db in trial_databases){
                matching_nodes = xml_find_all(doc, paste0("//text()[contains(., '", db, "')]"))
                matching_text = xml_text(matching_nodes)
                # Extract trial numbers from matching text using regex
                trial_numbers = c(trial_numbers, regmatches(matching_text, regexpr(paste0(db, "(\\d+)"), matching_text)))
        }
        if(length(trial_numbers) == 0){
                trial_numbers = NA
        } else {
                trial_numbers = trial_numbers[1]
        }
        trial_identifier = rbind(trial_identifier, data.frame(pmcid = paste0("PMC", gsub("[^0-9]", "", i)), trial_numbers = trial_numbers))
}
