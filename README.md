# A tutorial on how to use record linkage to remove duplicate from a Registration list

Record linkage, also known as data matching or deduplication or Unique Entity Estimation (UEE), is the process of identifying and linking records within or between datasets that refer to the same entity or individual. The goal of record linkage is to reconcile and merge information from different non-matching sources to create a unified and accurate view of the underlying entities.

In UNHCR context, this can be the case when merging registration list from different field partners, for instance when creating a sampling universe to organise a survey. Registration records form each list may vary in terms of data quality, format, and completeness. Record linkage helps to overcome these challenges by identifying and connecting related records, even when they do not have a common unique identifier.

The process of record linkage typically involves several steps:

 * __Data Cleaning__: Before linking records, it is essential to clean and standardize the data to ensure consistency. This may involve tasks such as correcting typos, standardizing formats, and handling missing or incomplete information.

 * __Blocking__: To reduce the computational complexity, the dataset is often divided into blocks or subsets based on certain criteria (e.g., the first letter of a name, the first few digits of a postal code). Blocking helps limit the number of candidate record pairs that need to be compared.

 * __Comparison__: For each pair of records within a block, a set of comparison rules is applied to determine the similarity between the records. This may involve comparing attributes such as names, addresses, dates of birth, or other relevant information.

 * __Matching__: Based on the results of the comparison, a decision is made about whether two records are a match or not. Matching may be exact or probabilistic, depending on the chosen algorithms and criteria. Probabilistic matching is often used when dealing with variations or errors in the data.

 * __Linking and Merging__: After determining which records are matches, the linked records are merged or consolidated to create a single, comprehensive record that combines information from the original sources.
 
 
 ## {RecordLinkage} & {fastLink}
 
 There are numerous packages for Record Linkage.
 
 We show here 2 vignettes, one for each package.
 
 
 
 ## Reference
 
Adapted from https://github.com/cleanzr/record-linkage-tutorial/

