
Project: Mapping and identifying trends in US Coal Production
-------------------------------------------------------------

#### *Bram Stone*

 

Coal production in the United States is a contentious issue and has received a resurgence of support in the current administration. As with any political issue, it is important to have a strong objective understanding of the situation in order to make informed decisions. This project demonstrates trends in coal over several decades, identifying patterns in production and employment. Raw data were obtained in `txt` files from the *Mine Safety and Health Administration (MSHA)*, which is under the US Department of Labor. **The key questions addressed by this exploration are:**

-   In which states, counties, and towns is coal most embedded? Where is production and employment highest?
    -   This question is important because it may show where support for coal is strongest, and where the support of alternative energy is weakest. Since this is a political topic, understanding the proportion of counties within a state affected by this issue may be relevent.
-   What are the overall trends in coal production, and does this pattern vary by region or locality?
    -   This question is important because it addresses which areas will be most affected by any loss in coal-based employment. Understanding regional variation will also help identify where coal production is robust in the face of national declines.

 

### 1. Data Import

First, the data were downloaded from two related sources both found at <https://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp>:

1.  `Employment/Production Data Set (Quarterly)`: Textfile of quarterly mine production and employment beginning in 2000 (171.8 Mb)
2.  `Mines Data Set`: Textfile of information about the location and status of all mines in the US (34.4 Mb)

The following code creates a temporary file to store the compressed contents of each data set which are then imported using `read.table`. The temporary file is then deleted after the R session is over. If the data are already downloaded and uncompressed, a faster way to import is to use the `data.table` package. There are nearly 2 million rows in the mining productivity dataset and 86 thousand in the mine information dataset.

``` r
temp <- tempfile()

#employment/production data
download.file('https://arlweb.msha.gov/OpenGovernmentData/DataSets/MinesProdQuarterly.zip', 
              temp, mode='wb')
coal <- read.table(unz(temp, 'MinesProdQuarterly.txt'), sep='|', header=T)

#mine information
download.file('https://arlweb.msha.gov/OpenGovernmentData/DataSets/Mines.zip', 
              temp, mode='wb')
mines <- read.table(unz(temp, 'Mines.txt'), sep='|', header=T)
dim(coal); dim(mines)

unlink(temp)
```

    ## [1] 1902038      13

    ## [1] 86880    59

 

### 2. Data Exploration

The next step is to clean and combine these data for downstream analyses and visualizations. First, column names are converted to lowercase to conform to best database practices, then non-coal mines are excluded from the data. Finally, coal productivity is merged with mine information using a left-join. Lastly, a function is applied across the dataset counting the number of NA values, which may be relevant for future steps.

``` r
names(coal) <- tolower(names(coal))
names(mines) <- tolower(names(mines))

coal <- coal[coal$coal_metal_ind=='C',]
mine_vars_to_keep <- c('mine_id', 
                       'current_mine_status', 
                       'fips_cnty_cd', 
                       'fips_cnty_nm', 
                       'bom_state_cd')

coal_mines <- merge(coal, mines[mine_vars_to_keep], by='mine_id', all.x=T)
dim(coal_mines); coal_mines[1:6,1:6]
```

    ## [1] 271217     17

    ##   mine_id     curr_mine_nm state subunit_cd
    ## 1 0100163 Arkadelphia 5761    AL         30
    ## 2 0100163 Arkadelphia 5761    AL         03
    ## 3 0100163 Arkadelphia 5761    AL         99
    ## 4 0100163 Arkadelphia 5761    AL         03
    ## 5 0100163 Arkadelphia 5761    AL         99
    ## 6 0100163 Arkadelphia 5761    AL         30
    ##                            subunit cal_yr
    ## 1 MILL OPERATION/PREPARATION PLANT   2005
    ## 2           STRIP, QUARY, OPEN PIT   2005
    ## 3      OFFICE WORKERS AT MINE SITE   2004
    ## 4           STRIP, QUARY, OPEN PIT   2005
    ## 5      OFFICE WORKERS AT MINE SITE   2005
    ## 6 MILL OPERATION/PREPARATION PLANT   2005

``` r
sapply(coal_mines, function(x) sum(is.na(x)))
```

    ##             mine_id        curr_mine_nm               state 
    ##                   0                   0                   0 
    ##          subunit_cd             subunit              cal_yr 
    ##                   0                   0                   0 
    ##             cal_qtr           fiscal_yr          fiscal_qtr 
    ##                   0                   0                   0 
    ##    avg_employee_cnt        hours_worked     coal_production 
    ##                   0                   0               22577 
    ##      coal_metal_ind current_mine_status        fips_cnty_cd 
    ##                   0                   0                   0 
    ##        fips_cnty_nm        bom_state_cd 
    ##                   0                   0

Right off the bat there are 22577 `NA` values in the `coal_production` variable, 8% of the dataset. It is worth exploring if there's any bias in these missing values. Here, the `aggregate` function is used to count the number observations that that are `NA` (*i.e.* have no production values reported). However this needs to be standardized, since states with more mines may presumably have more `NA` values (unless there's a bias). By dividing this value by the number of observations in the whole dataset, the *proportion* of missing production numbers can be seen. Earlier, the `current_mine_status` variable was noticed, and missing production values may be from mines that are no longer active. It is also worth looking to see if missing production values occur more often from certain states.

``` r
na.prop <- function(x) sum(is.na(x) / length(x))

#run separate summaries for each factor
(na_by_status <- aggregate(coal_production ~ current_mine_status, 
                           coal_mines, 
                           na.prop, 
                           na.action=NULL))
```

    ##    current_mine_status coal_production
    ## 1            Abandoned     0.101833428
    ## 2 Abandoned and Sealed     0.133319346
    ## 3               Active     0.065128933
    ## 4         Intermittent     0.067615658
    ## 5             New Mine     0.008086253
    ## 6         NonProducing     0.073162713
    ## 7    Temporarily Idled     0.041813196

``` r
na_by_state <- aggregate(coal_production ~ state, 
                         coal_mines, 
                         na.prop, 
                         na.action=NULL)

#are there more NA values in high or low production states?
prod_by_state <- aggregate(coal_production ~ state, coal_mines, function(x) sum(as.numeric(x)))
na_by_state <- merge(na_by_state, prod_by_state, by='state', all.x=T)

with(na_by_state, 
     plot(coal_production.y ~ coal_production.x, pch=21, cex=1.5, bg=hsv(0,.7,.8),
          xlab='Number of Missing Production Values', ylab='Total Coal Production',
          main='NA Values Against Total Coal Production')
)
```

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

![](missing_values.png)

It looks like `NA` values are more likely to occur from inactive and abandoned mines than those that are active, as expected, and there is some difference in the number of `NA` values per state. However, the amount of coal produced seems to have little bearing on the quality of the data. Removing these cases shouldn't bias our results against any highly productive or unproductive states.

#### Coal Production and Employment in 2016

It is worth looking at what coal production in the US currently looks like. Which are the top coal producing states? The code below looks at active and intermittently active mines during the year 2016. Production and employment are totaled by each mine in 2016, which are then aggregated to the state level for comparison Note: I try to utilze base code as much as possible given the size of the dataset (`data.table` and `dplyr` often outperform base functions and so are necessary in some cases) because I don't like too many dependencies in my code. `ggplot` is, however, used to construct visualizations, as it certainly produces very elegant code for exploratory data analyses. Both county and state data are manipulated into long format so that they can be faceted using the `reshape` function which isn't largely utilized due to several better, less obtuse, alternatives.

``` r
library(ggplot2)

#select mines active to some degree in 2016
active <- c('Active', 'Intermittent', 'New Mine')
coal_16 <- coal_mines[coal_mines$fiscal_yr==2016 & 
                        coal_mines$current_mine_status %in% active,]

#sum productivity and employees by mine and state (include state code as well)
by_mine <- aggregate(cbind(avg_employee_cnt, coal_production) ~ mine_id + state + bom_state_cd, 
                     data=coal_16, sum)

#which states have highest employment and productivity?
by_state <- aggregate(cbind(avg_employee_cnt, coal_production) ~ state, data=by_mine, sum)
by_state <- by_state[order(by_state$avg_employee_cnt, decreasing=T),]
by_state <- reshape(by_state, direction='long',
        idvar='state', varying=list(2:3), #keep state, condense columns 2 and 3
        v.names='value', #name condensed column 'value'
        times=names(by_state)[2:3], #numbers in 'value' column attributed to these names
        new.row.names=1:(nrow(by_state) * 2)) #override default rownames generated

#plot
state_compare <- ggplot(by_state, aes(reorder(state, sort(value, decreasing=T)), value)) +
  geom_bar(stat='identity',fill='navy') +
  facet_wrap(~ time, scales='free') + 
  coord_flip() +
  ylab('Number of workers employed') +
  xlab('') +
  labs(title='Employment by State in 2016') +
  theme_bw()

state_compare
```

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

![](state_comparison.png)

Now that we can see who the top producing states are, let's look at a county-by-county comparison. In order to create a chloropleth map, where production and employment are quantified by color intensity, the data need to be related to a spatial database. Here, production and employment data is summarized by county, and merged with more county-based information from `chloroplethrMaps` that will allow the dataset to be mapped using `ggplot`. More specifically, data were joined on county *FIPS* (Federal Information Processing) codes. After joining, `NA` values were zeroed, redundante columns were removed, and county and state columns were renamed to `subregion` and `region`, respectively. This last part was to make data comply with `ggplot`'s expected column names.

``` r
library(choroplethrMaps)

#county info from choroplethrMaps
data(county.regions) 
county.regions <- within(county.regions, {
  fips_cnty_cd <- substr(county.fips.character,3,5)
  state <- state.abb
  fips_cnty_nm <- county.name
  county.name <- NULL
  state.abb <- NULL
  county.fips.character <- NULL
})

#summarize employment and productivity by county
by_mine <- merge(by_mine, mines, all.x=T)
by_county <- aggregate(cbind(avg_employee_cnt, coal_production) ~ fips_cnty_nm + fips_cnty_cd + state, 
                       data=by_mine, sum)

#make county names in dataset lowercase to match county.regions from chloroplethrMaps
by_county$fips_cnty_nm <- tolower(by_county$fips_cnty_nm)

#merge by FIPS code
by_county <- merge(by_county, county.regions)

#clean: replace NAs with 0 for plotting, change county info to 'subregion' and state info to 'region'
by_county <- within(by_county, {
  avg_employee_cnt[is.na(avg_employee_cnt)] <- 0
  coal_production[is.na(coal_production)] <- 0
  subregion <- fips_cnty_nm
  region <- state.name
  fips_cnty_nm <- NULL; state.name <- NULL
  state.fips.character <- NULL; state <- NULL
  fips_cnty_cd <- NULL
})
head(by_county)
```

    ##   avg_employee_cnt coal_production        region subregion
    ## 1              587          864643      maryland  allegany
    ## 2               26            9220  pennsylvania allegheny
    ## 3              287          285490  pennsylvania armstrong
    ## 4             1003         2706248         texas  atascosa
    ## 5             1016          909735 west virginia   barbour
    ## 6               70          189972      missouri     bates

``` r
#create map data
coal_map <- map_data('county')
coal_map <- merge(coal_map, by_county, all.x=T, sort=F)
coal_map <- coal_map[order(coal_map$order),]
```

``` r
#If we wanted to change the scale of the log-tranformation, it would be done here and 'log5' could 
#be called in place of trans='log10' in the scale_fill_continuous function()
library(scales)
log5_trans <- function() {
  scales::trans_new('log5', function(x) log(x, base=5), function(x) 5^(x), breaks=log_breaks(10, 10))
}

#Map of employment
employ_map <- ggplot(coal_map, aes(long, lat, group=group, fill=avg_employee_cnt)) +
  geom_polygon() +
  coord_map('lambert', parameters=c(50,30)) +
  borders('state', size=.25) +
  scale_fill_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                        trans='log10', low=gray(.9), high='darkblue', na.value='white',
                        breaks=log_breaks() (c(10,10000))) +
  labs(fill='Employees',
    title='U.S. Workers Employed in Coal Production in 2016',
    subtitle=paste0('Total 2016 employment: ', format(sum(by_county$avg_employee_cnt), big.mark=','))) +
  theme_void() +
  theme(plot.title = element_text(size=rel(1.5)))

employ_map
```

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

    ## Warning: Transformation introduced infinite values in discrete y-axis

![](coal_employment_2016.png)

``` r
#for labeling
data_source <- paste0('Quarterly mine production and employment,', 
                      '\nDepartment of Labor, Mine Safety and Health Admin (MSHA)',
                      '\nsource: https://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp')

#Map of coal production
product_map <- ggplot(coal_map, aes(long, lat, group=group, fill=coal_production)) +
  geom_polygon() +
  coord_map('lambert', parameters=c(50,30)) +
  borders('state', size=.25) +
  scale_fill_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                        trans='log10', low=gray(.9), high='darkred', na.value='white') +
  labs(fill='Tons of coal',
       title='U.S. Coal Production in 2016',
       subtitle=paste0('Total 2016 production: ', format(sum(by_county$coal_production), big.mark=',')),
       caption=data_source) +
  theme_void() +
  theme(plot.title = element_text(size=rel(1.5)))

product_map
```

    ## Warning: Transformation introduced infinite values in discrete y-axis

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

    ## Warning: Transformation introduced infinite values in discrete y-axis

![](coal_production_2016.png)

Next, let's look at the change in coal production and employment across time (2000--2007) by state. For simplicity's sake, only the 10 most productive states are shown on the graph. The following code aggregates production and employment by quater and state. Because year and quarter are separate, numeric, columns, they are combined in the `aggregate` function using a mathematical expression in the formula notation (which is the purpose of the `I` function that wraps that particular expression).

Looking at the output, there appears to be some difference between production and employment. While many states seem to be fairly steady in both categories since 2000, West Virginia and Kentucky, the two states with the most people employed in coal production, have seen a fairly significant drop off in employment since 2012 with production steadily dropping since 2000. Another obvious pattern is the dominance of Wyoming coal production as compared to it's mediocre employment numbers. Coal production in Wyoming is by far the highest and most variable of any state, but employs roughly the same number of workers as Pennslyvania. This means that less people in Wyoming are as directly dependent on coal production and perhaps less vulnerable to losses of these jobs. Interestingly, employment (and to a slightly lesser degree production) in West Virginia and Kentucky follow nearly identical patterns over time, rather than taking on different trajectories, suggesting that the same forces are acting on these across both states.

    ## png 
    ##   2

![](coal_by_time.png)

#### Employment in coal in West Virginia and Kentucky

To explore employment patterns further, we will look at mining operations at each state individually. The code below aggregates employment by `mine_id`, and joins the data to the `mines` dataset, choosing only **Kentucky** and **West Virginia**. Conveniently, both `longitude` and `latitude` are included in the `mines` dataset, which can be used to map coal employment. In order to map using `ggplot`, state and FIPS county code are changed to `region` and `subregion`, respectively. Longitude and latitude were erroneously encoded as factors when they were originally uploaded, so they are converted to numbers. Lastly, before plotting, `NA` values, some extreme outliers were removed. The plot below isn't perfect; there are still some points beyond the spatial extent of the states, but the spatial clustering of coal production can be seen clearly.

``` r
#Next to map is coal employment in West Virginia, and Kentucky by town.

#Get mapmaking info
wv_ky <- map_data('county', c('west virginia', 'kentucky'))
wv_ky_state <- map_data('state', c('west virginia', 'kentucky'))
head(wv_ky)
```

    ##        long      lat group order   region subregion
    ## 1 -85.24466 36.92713     1     1 kentucky     adair
    ## 2 -85.46812 36.94432     1     2 kentucky     adair
    ## 3 -85.46812 36.94432     1     3 kentucky     adair
    ## 4 -85.45666 36.97869     1     4 kentucky     adair
    ## 5 -85.51395 37.01308     1     5 kentucky     adair
    ## 6 -85.52541 37.02453     1     6 kentucky     adair

``` r
#get employment numbers for WV and KY mines in 2016, aggregate by mine_id
coal_16_wv_ky <- coal_16[coal_16$state %in% c('WV', 'KY'),]
coal_16_wv_ky <- aggregate(cbind(avg_employee_cnt, coal_production) ~ mine_id, data=coal_16_wv_ky, sum)

#get geographic information for each mine from 'mines' info, has latitude and longitude
mine_info <- merge(coal_16_wv_ky, mines, by='mine_id', all.x=T)
mine_info <- mine_info[names(mine_info) %in% c('mine_id', 'avg_employee_cnt', 'coal_production',
                                               'current_mine_type', 'state', 'fips_cnty_nm',
                                               'longitude', 'latitude')]
#change names to match with those expected for plotting
names(mine_info)[names(mine_info) %in% c('state', 'fips_cnty_nm', 
                                         'longitude', 'latitude')] <- c('region', 'subregion', 'long', 'lat')
#convert latitude and longitude to numbers and invert longitude which is in decimal degrees West
mine_info <- within(mine_info, {
  long <- as.numeric(as.character(long)) * -1
  lat <- as.numeric(as.character(lat))
})
head(mine_info)
```

    ##   mine_id avg_employee_cnt coal_production current_mine_type region
    ## 1 1202416               72          193386           Surface     KY
    ## 2 1502132             1492         3763705       Underground     KY
    ## 3 1502263              401          431031       Underground     KY
    ## 4 1503178              215               0          Facility     KY
    ## 5 1503627               90          276378           Surface     KY
    ## 6 1505215                0               0          Facility     KY
    ##   subregion      long      lat
    ## 1      Ohio -87.09750 37.35444
    ## 2   Webster -87.77028 37.45250
    ## 3    Harlan -83.01083 36.87472
    ## 4     Union -87.94722 37.76000
    ## 5     Perry -83.29667 37.33889
    ## 6    Harlan -83.25833 36.81500

``` r
#find any NA values
sapply(mine_info, function(x) which(is.na(x)))
```

    ## $mine_id
    ## integer(0)
    ## 
    ## $avg_employee_cnt
    ## integer(0)
    ## 
    ## $coal_production
    ## integer(0)
    ## 
    ## $current_mine_type
    ## integer(0)
    ## 
    ## $region
    ## integer(0)
    ## 
    ## $subregion
    ## integer(0)
    ## 
    ## $long
    ## [1] 160 332
    ## 
    ## $lat
    ## [1] 160 332

``` r
mine_info <- mine_info[!is.na(mine_info$long),]
#find outlier values
par(mfrow=c(1,2))
long_outlier <- boxplot(mine_info$long)$out
lat_outlier <- boxplot(mine_info$lat)$out
```

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

``` r
#return any data points that match the extreme outliers
outliers <- which(mine_info$long==max(long_outlier) | mine_info$lat==max(lat_outlier))
mine_info[outliers,]
```

    ##     mine_id avg_employee_cnt coal_production current_mine_type region
    ## 351 4609474                0               0       Underground     WV
    ##     subregion      long      lat
    ## 351   Wyoming -37.54611 81.50528

``` r
mine_info <- mine_info[-outliers,]
#add in group, a required column for adding to ggplot map
mine_info$group <- rep(1L, nrow(mine_info))

#plot
wv_ky_map <- ggplot(wv_ky, aes(long, lat, group=group)) +
  geom_polygon(aes(long, lat), color=gray(.6), fill=NA) +
  coord_map('lambert', parameters=c(30,80)) +
  theme_void() +
    labs(color='Mine Type',
         size='Employee Count',
         title='Coal Operations in West Virginia and Kentucky',
         subtitle='Employment in 2016',
         caption=data_source) +
  guides(color = guide_legend(override.aes = list(size=6))) +
  theme(plot.title = element_text(size=rel(1.5)))

wv_ky_map +
  geom_polygon(data=wv_ky_state, aes(long, lat), color='black', fill=NA, size=.75) +
  geom_point(data=mine_info, aes(long, lat, size=avg_employee_cnt, color=current_mine_type), alpha=.5)
```

![](project_us_coal_production_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-2.png)

    ## png 
    ##   2

![](coal_employment_wv_ky.png)
