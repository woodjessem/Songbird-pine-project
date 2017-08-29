# Songbird-pine-project
There is a CSV file including point count data for 3-4 visits, and vegetation data for that site, for each species of interest.
  ^ These are labeled "spcode_abund" for now, to indicate I am using them for pcount of abundance.
      It should also be noted that these are from PC data that only includes independent detections (flock numbers not broken down,
      so doesn't exactly reflect every single bird on site)
Then, there is an R script file containing the model selection analysis for each of those species.
While these R files will have the same basic structure...
      (first model selection based on which detection covariates are significant (fm1)
       then site variable model selection with null models for detection covariates (fm2), followed by
       site variable model selection with the detection covariates based on the top or 2 top models from fm1 (fm3+)
       anything beyond that may reflect additional tweaks added later, like scaling the detection covariates prior to fm1, etc.)
   ... they will not be identical but will be edited to be species-specific (different covariates revealed).
   
   PC_ind_detections (only independent detections - as recorded in field. flock of 4 birds only 1 line, considered 1 bird)
      ** ^ abund CSV files are currently only based on THIS ^ data.
   PC_all_detections (every bird - flocks are broken down and counted as multiple birds, separate detections)
      ^ This data may be used later if true abundance (#s) are important.
