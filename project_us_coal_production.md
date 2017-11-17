Project: Mapping and identifying trends in US Coal Production
-------------------------------------------------------------

#### *Bram Stone*

 

Coal production in the United States is a contentious issue and has
received a resurgence of support in the current administration. As with
any political issue, it is important to have a strong objective
understanding of the situation in order to make informed decisions. This
project demonstrates trends in coal over several decades, identifying
patterns in production and employment. Raw data were obtained in `txt`
files from the *Mine Safety and Health Administration (MSHA)*, which is
under the US Department of Labor. **The key questions addressed by this
exploration are:**

-   In which states, counties, and towns is coal most embedded? Where is
    production and employment highest?
    -   This question is important because it may show where support for
        coal is strongest, and where the support of alternative energy
        is weakest. Since this is a political topic, understanding the
        proportion of counties within a state affected by this issue may
        be relevent.
-   What are the overall trends in coal production, and does this
    pattern vary by region or locality?
    -   This question is important because it addresses which areas will
        be most affected by any loss in coal-based employment.
        Understanding regional variation will also help identify where
        coal production is robust in the face of national declines.

 

### 1. Data Import

First, the data were downloaded from two related sources both found at
<https://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp>:

1.  `Employment/Production Data Set (Quarterly)`: CSV of quarterly mine
    production and employment beginning in 2000 (171.8 Mb)
2.  `Mines Data Set`: CSV of information about the location and status
    of all mines in the US (34.4 Mb)

The following code creates a temporary file to store the compressed
contents of each data set which are then imported using `read.table`.
The temporary file is then deleted after the R session is over. If the
data are already downloaded and uncompressed, a faster way to import is
to use the `data.table` package.

    temp <- tempfile()

    download.file('https://arlweb.msha.gov/OpenGovernmentData/DataSets/ContractorProdQuarterly.zip', 
                  temp, mode='wb')
    coal <- read.table(unz(temp, 'MinesProdQuarterly.txt'), sep='|', header=T)
    download.file('https://arlweb.msha.gov/OpenGovernmentData/DataSets/Mines.zip', 
                  temp, mode='wb')
    mines <- read.table(unz(temp, 'Mines.txt'), sep='|', header=T)

    unlink(temp)
