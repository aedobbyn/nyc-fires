🔥 `drake` for Workflow Happiness 🔥
==================================

This is an intro to the [`drake`](https://github.com/ropensci/drake) R
package for data pipeling. It might be useful for you if you’re
interested in reproducibility after you make changes to your code.

![ilovechanges](https://media.giphy.com/media/JFawGLFMCJNDi/giphy.gif)

The talk is split into two parts:
[slides](https://github.com/aedobbyn/nyc-fires/blob/master/pres.Rmd),
and a [live coding
walkthrough](https://github.com/aedobbyn/nyc-fires/blob/master/live_code.Rmd).
The latter is meant to be stepped through in an R session rather than
knit.

#### The conceit

This talk is motivated by the **\[NYCFireWire Twitter
account\]**(<a href="https://twitter.com/NYCFireWire" class="uri">https://twitter.com/NYCFireWire</a>)
with an assist from [Gritty](https://youtu.be/FNt0anp7WK8?t=8) at a
[burner account](https://twitter.com/didntstartit).

It relies on the
**\[rtweet\]**(<a href="https://github.com/mkearney/rtweet" class="uri">https://github.com/mkearney/rtweet</a>)
and
**\[ggmap\]**(<a href="https://github.com/dkahle/ggmap" class="uri">https://github.com/dkahle/ggmap</a>)
packages, so to be able to run it in full you’ll need a [Twitter API
access token](https://rtweet.info/articles/auth.html) and [Google Maps
Geocoding API
key](https://developers.google.com/maps/documentation/geocoding/intro#Geocoding).

### Other things you might want to know

All functions used in both parts live in
[`didnt_start_it.R`](https://github.com/aedobbyn/nyc-fires/blob/master/didnt_start_it.R).

Feel free to use any and all of the data, including the
[raw](https://github.com/aedobbyn/nyc-fires/blob/master/data/raw/lots_o_fires.csv)
and
[geocoded](https://github.com/aedobbyn/nyc-fires/blob/master/data/derived/dat.csv)
tweet motherlode.

The best resources on `drake` remain the [`drake`
manual](https://ropenscilabs.github.io/drake-manual/) and
[Will](https://twitter.com/wmlandau)
[Landau](https://github.com/wlandau) 😄.
