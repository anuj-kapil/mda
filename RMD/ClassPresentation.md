Multivariate Data Analysis - Presentation
================

## Step 1: Download the dataset from Kaggle

To simplify the process, I am using my github account to host the
dataset but the original dataset is available here: [Kaggle: House Sales
in King County](https://www.kaggle.com/harlfoxem/housesalesprediction)

``` r
wd <- getwd()
filep <- "https://raw.githubusercontent.com/anuj-kapil/mda/master/Data/kc_house_data.csv"
filename <- "kc_house_data.csv"
download.file(url=filep, destfile=filename)
```

## Step 2: Read the downloaded file to memory

We are using the data.table package from R to read the file in to memory

``` r
kc_houses<-fread('kc_house_data.csv')
```

## Step 3: Exploratory Analysis

Let’s have a look at the number of features and observations in the
dataset

``` r
# Rows and columns
dim(kc_houses)
```

    ## [1] 21597    21

Quick look at the data types and ranges of data. Below summary shows
that there are no null or missing values

``` r
# No missing values
summary(kc_houses)
```

    ##        id                 date               price        
    ##  Min.   :   1000102   Length:21597       Min.   :  78000  
    ##  1st Qu.:2123049175   Class :character   1st Qu.: 322000  
    ##  Median :3904930410   Mode  :character   Median : 450000  
    ##  Mean   :4580474287                      Mean   : 540297  
    ##  3rd Qu.:7308900490                      3rd Qu.: 645000  
    ##  Max.   :9900000190                      Max.   :7700000  
    ##     bedrooms        bathrooms      sqft_living       sqft_lot      
    ##  Min.   : 1.000   Min.   :0.500   Min.   :  370   Min.   :    520  
    ##  1st Qu.: 3.000   1st Qu.:1.750   1st Qu.: 1430   1st Qu.:   5040  
    ##  Median : 3.000   Median :2.250   Median : 1910   Median :   7618  
    ##  Mean   : 3.373   Mean   :2.116   Mean   : 2080   Mean   :  15099  
    ##  3rd Qu.: 4.000   3rd Qu.:2.500   3rd Qu.: 2550   3rd Qu.:  10685  
    ##  Max.   :33.000   Max.   :8.000   Max.   :13540   Max.   :1651359  
    ##      floors        waterfront            view          condition   
    ##  Min.   :1.000   Min.   :0.000000   Min.   :0.0000   Min.   :1.00  
    ##  1st Qu.:1.000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:3.00  
    ##  Median :1.500   Median :0.000000   Median :0.0000   Median :3.00  
    ##  Mean   :1.494   Mean   :0.007547   Mean   :0.2343   Mean   :3.41  
    ##  3rd Qu.:2.000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:4.00  
    ##  Max.   :3.500   Max.   :1.000000   Max.   :4.0000   Max.   :5.00  
    ##      grade          sqft_above   sqft_basement       yr_built   
    ##  Min.   : 3.000   Min.   : 370   Min.   :   0.0   Min.   :1900  
    ##  1st Qu.: 7.000   1st Qu.:1190   1st Qu.:   0.0   1st Qu.:1951  
    ##  Median : 7.000   Median :1560   Median :   0.0   Median :1975  
    ##  Mean   : 7.658   Mean   :1789   Mean   : 291.7   Mean   :1971  
    ##  3rd Qu.: 8.000   3rd Qu.:2210   3rd Qu.: 560.0   3rd Qu.:1997  
    ##  Max.   :13.000   Max.   :9410   Max.   :4820.0   Max.   :2015  
    ##   yr_renovated        zipcode           lat             long       
    ##  Min.   :   0.00   Min.   :98001   Min.   :47.16   Min.   :-122.5  
    ##  1st Qu.:   0.00   1st Qu.:98033   1st Qu.:47.47   1st Qu.:-122.3  
    ##  Median :   0.00   Median :98065   Median :47.57   Median :-122.2  
    ##  Mean   :  84.46   Mean   :98078   Mean   :47.56   Mean   :-122.2  
    ##  3rd Qu.:   0.00   3rd Qu.:98118   3rd Qu.:47.68   3rd Qu.:-122.1  
    ##  Max.   :2015.00   Max.   :98199   Max.   :47.78   Max.   :-121.3  
    ##  sqft_living15    sqft_lot15    
    ##  Min.   : 399   Min.   :   651  
    ##  1st Qu.:1490   1st Qu.:  5100  
    ##  Median :1840   Median :  7620  
    ##  Mean   :1987   Mean   : 12758  
    ##  3rd Qu.:2360   3rd Qu.: 10083  
    ##  Max.   :6210   Max.   :871200

The dates are loaded as string of characters. Let’s convert them to look
at the range of values. The summary now shows that the dates are in the
range of May 2014 - May 2015

``` r
# Convert Dates
kc_houses$date<-as.IDate(kc_houses$date, format = "%m/%d/%Y")
summary(kc_houses)
```

    ##        id                  date                price        
    ##  Min.   :   1000102   Min.   :2014-05-02   Min.   :  78000  
    ##  1st Qu.:2123049175   1st Qu.:2014-07-22   1st Qu.: 322000  
    ##  Median :3904930410   Median :2014-10-16   Median : 450000  
    ##  Mean   :4580474287   Mean   :2014-10-29   Mean   : 540297  
    ##  3rd Qu.:7308900490   3rd Qu.:2015-02-17   3rd Qu.: 645000  
    ##  Max.   :9900000190   Max.   :2015-05-27   Max.   :7700000  
    ##     bedrooms        bathrooms      sqft_living       sqft_lot      
    ##  Min.   : 1.000   Min.   :0.500   Min.   :  370   Min.   :    520  
    ##  1st Qu.: 3.000   1st Qu.:1.750   1st Qu.: 1430   1st Qu.:   5040  
    ##  Median : 3.000   Median :2.250   Median : 1910   Median :   7618  
    ##  Mean   : 3.373   Mean   :2.116   Mean   : 2080   Mean   :  15099  
    ##  3rd Qu.: 4.000   3rd Qu.:2.500   3rd Qu.: 2550   3rd Qu.:  10685  
    ##  Max.   :33.000   Max.   :8.000   Max.   :13540   Max.   :1651359  
    ##      floors        waterfront            view          condition   
    ##  Min.   :1.000   Min.   :0.000000   Min.   :0.0000   Min.   :1.00  
    ##  1st Qu.:1.000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:3.00  
    ##  Median :1.500   Median :0.000000   Median :0.0000   Median :3.00  
    ##  Mean   :1.494   Mean   :0.007547   Mean   :0.2343   Mean   :3.41  
    ##  3rd Qu.:2.000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:4.00  
    ##  Max.   :3.500   Max.   :1.000000   Max.   :4.0000   Max.   :5.00  
    ##      grade          sqft_above   sqft_basement       yr_built   
    ##  Min.   : 3.000   Min.   : 370   Min.   :   0.0   Min.   :1900  
    ##  1st Qu.: 7.000   1st Qu.:1190   1st Qu.:   0.0   1st Qu.:1951  
    ##  Median : 7.000   Median :1560   Median :   0.0   Median :1975  
    ##  Mean   : 7.658   Mean   :1789   Mean   : 291.7   Mean   :1971  
    ##  3rd Qu.: 8.000   3rd Qu.:2210   3rd Qu.: 560.0   3rd Qu.:1997  
    ##  Max.   :13.000   Max.   :9410   Max.   :4820.0   Max.   :2015  
    ##   yr_renovated        zipcode           lat             long       
    ##  Min.   :   0.00   Min.   :98001   Min.   :47.16   Min.   :-122.5  
    ##  1st Qu.:   0.00   1st Qu.:98033   1st Qu.:47.47   1st Qu.:-122.3  
    ##  Median :   0.00   Median :98065   Median :47.57   Median :-122.2  
    ##  Mean   :  84.46   Mean   :98078   Mean   :47.56   Mean   :-122.2  
    ##  3rd Qu.:   0.00   3rd Qu.:98118   3rd Qu.:47.68   3rd Qu.:-122.1  
    ##  Max.   :2015.00   Max.   :98199   Max.   :47.78   Max.   :-121.3  
    ##  sqft_living15    sqft_lot15    
    ##  Min.   : 399   Min.   :   651  
    ##  1st Qu.:1490   1st Qu.:  5100  
    ##  Median :1840   Median :  7620  
    ##  Mean   :1987   Mean   : 12758  
    ##  3rd Qu.:2360   3rd Qu.: 10083  
    ##  Max.   :6210   Max.   :871200
