    CHANGES IN XML2R VERSION 0.0.6

MINOR FIX

  - In version 0.5, the url column does not contain the file name (instead, it contains integers). This has consequences for pitchRx, but is now fixed.

    CHANGES IN XML2R VERSION 0.0.5

MAJOR CHANGES/IMPROVEMENTS

  - A dependency to RCurl was added so asynchronous downloads can be done via getURL. This dependency also has the benefit of more robust HTTP downloads.
  
    CHANGES IN XML2R VERSION 0.0.4

MAJOR CHANGES/IMPROVEMENTS

  - The collapse() function was changed to collapse_obs() to avoid namespace clashing with dplyr's collapse().

  - The child argument was removed from add_key(). The recycle argument was added to add_key() which allows one to use an existing value in the parent node as a key to connect the parent observation to its descendents (thanks Carlos Scheidegger) 
  
  - Better garbage collection
  
    CHANGES IN XML2R VERSION 0.0.3

IMPROVEMENTS

  - url.map option was added to XML2Obs.
  
    CHANGES IN XML2R VERSION 0.0.2

MAJOR CHANGES

  - Function rename() was changed to re_name() to avoid NAMESPACE issue conflicts with plyr.

MINOR FIXES

  - quiet option added to several functions

    CHANGES IN XML2R VERSION 0.0.1

NEW FEATURES

  - First version of XML2R. See the package [tutorial](http://cpsievert.github.io/XML2R/)

  - This package was born out of an effort to abstract the functionality of [pitchRx::urlsToDataFrame](https://github.com/cpsievert/pitchRx/blob/master/R/urlsToDataFrame.R) so that other projects can be built on top of this framework.

MISC

  - in this NEWS file, #n means the issue number on GitHub, e.g. #1 is
  https://github.com/cpsievert/XML2R/issues/1