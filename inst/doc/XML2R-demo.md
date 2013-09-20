<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{A Markdown Vignette with knitr}
-->




Introduction to XML2R package
====================================

Extracting entire documents
--------------------------

The  **XML2R** package is meant to simplify scraping XML data from the web. All you need is a collection of urls that point to XML files. By default, `XML2R` will pool all information into a list of matrices -- each corresponding to a common XML node. Here is a simple example:


```r
library(XML2R)
urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml", 
          "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
dat <- XML2R(urls)
```



```r
names(dat)
```

```
##  [1] "highlights//media"                    "highlights//media//duration"         
##  [3] "highlights//media//headline"          "highlights//media//keywords//keyword"
##  [5] "highlights//media//player"            "highlights//media//team"             
##  [7] "highlights//media//thumb"             "highlights//media//thumbnails//thumb"
##  [9] "highlights//media//url"               "url_map"
```


Within these two files (which have similar and fairly simple structure), there are nine different "levels" of information. For example, the first element of `dat` corresponds to all information occuring on the "highlights//media level". Lets take a look at the first 10 occurences of this XML node:


```r
head(dat[[1]], 10)
```

```
##       id         date                       type    top-play URL_source condensed
##  [1,] "25915543" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [2,] "25915749" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [3,] "25915619" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [4,] "25915913" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [5,] "25916039" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [6,] "25918919" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [7,] "25919053" "2013-03-30T13:05:00-0400" "video" "true"   "url1"     NA       
##  [8,] "25924103" "2013-03-30T13:05:00-0400" "video" NA       "url1"     "true"   
##  [9,] "25943751" "2013-04-01T19:10:00-0400" "video" "true"   "url2"     NA       
## [10,] "25944519" "2013-04-01T19:10:00-0400" "video" "true"   "url2"     NA
```


This XML node has five different attributes: "id", "date", "type", "top-play" and "condensed". The condensed attribute is missing in most cases, but in the eighth row "condensed" appears while "top-play" is now missing. For tracking purposes, `XML2R` will also append a column names `URL_source` which can be used to track observations back to their file. Since some urls can be quite long, we store the mapping from values in this column to the actual file name via the `dat[["url_map"]]`


```r
dat[["url_map"]]
```

```
##                                                                 url1 
## "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml" 
##                                                                 url2 
## "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml"
```


Another nice aspect of `XML2R` is the automatic inclusion of both XML values and attributes. For example, information on the highlights//media//player level contains both values and attributes. In this case, there is one unique attribute -- "player_id". 


```r
dat[[5]]
```

```
##       player_id XML_value         URL_source
##  [1,] "430832"  "Jose Bautista"   "url1"    
##  [2,] "408314"  "Jose Reyes"      "url1"    
##  [3,] "276519"  "Jimmy Rollins"   "url1"    
##  [4,] "460055"  "John Mayberry"   "url1"    
##  [5,] "276545"  "Michael Young"   "url1"    
##  [6,] "435081"  "Mike Nickeas"    "url1"    
##  [7,] "429400"  "Adam Loewen"     "url1"    
##  [8,] "518692"  "Freddie Freeman" "url2"    
##  [9,] "462564"  "Dan Uggla"       "url2"    
## [10,] "518692"  "Freddie Freeman" "url2"    
## [11,] "400284"  "Chase Utley"     "url2"    
## [12,] "462564"  "Dan Uggla"       "url2"    
## [13,] "400284"  "Chase Utley"     "url2"    
## [14,] "457708"  "Justin Upton"    "url2"    
## [15,] "518886"  "Craig Kimbrel"   "url2"
```


Using XPath
--------------------------

If one is interested in just a subset of the XML document, the `xpath` option of `XML2R` can be used to specify [XPath syntax](http://www.w3schools.com/xpath/xpath_syntax.asp). Suppose I'm just interested in information that occurs on the `highlights//media//player` level. 


```r
dat2 <- XML2R(urls, xpath = "/highlights/media/player")
```



```r
dat2
```

```
## $attrs
##       player_id XML_value         URL_source
##  [1,] "430832"  "Jose Bautista"   "url1"    
##  [2,] "408314"  "Jose Reyes"      "url1"    
##  [3,] "276519"  "Jimmy Rollins"   "url1"    
##  [4,] "460055"  "John Mayberry"   "url1"    
##  [5,] "276545"  "Michael Young"   "url1"    
##  [6,] "435081"  "Mike Nickeas"    "url1"    
##  [7,] "429400"  "Adam Loewen"     "url1"    
##  [8,] "518692"  "Freddie Freeman" "url2"    
##  [9,] "462564"  "Dan Uggla"       "url2"    
## [10,] "518692"  "Freddie Freeman" "url2"    
## [11,] "400284"  "Chase Utley"     "url2"    
## [12,] "462564"  "Dan Uggla"       "url2"    
## [13,] "400284"  "Chase Utley"     "url2"    
## [14,] "457708"  "Justin Upton"    "url2"    
## [15,] "518886"  "Craig Kimbrel"   "url2"    
## 
## $url_map
##                                                                 url1 
## "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml" 
##                                                                 url2 
## "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml"
```

