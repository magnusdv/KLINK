# KLINK 0.7.3

* Update **pedprobr** version to 0.9.3, giving much faster likelihood calculations in many cases.

* Show wait cursor during LR calculations.

* Use scientific format also for small LR totals in-app.

* Unlinked report: Prioritise D5S2500 over D5S2800 if both are present, and similarly D10S2325 over D10S1435, and D19S253 over D19S433 (reverses previous behaviour).

* Fix typos in built-in linkage map.

* Fix outdated mutation model in dataset `paternity`.


# KLINK 0.7.2

* Internal reorganisation, moving the Shiny code to `inst/shiny/app.R`.

* `launchApp()` is now the main launching function, with `runKLINK()` as an alias.

* Fixes a minor regression error in the previous version.


# KLINK 0.7.1

* New button "Mask" for downloading a masked version of the dataset.

* Reading/writing .fam files is now handled by the new package `pedFamilias()`, which has been split out from `forrel`.

* Added R option `KLINK.debug` which can be set to TRUE for debugging (only when running KLINK from R). 

* Use (experimental) `autoScale` option in pedigree plots.

* Minor internal tweaks and bug fixes.


# KLINK 0.7.0

* Although KLINK is primarily a Shiny app, the package now documents and exports the main functions, enabling analysis in R as well.

* Added new dataset `paternity`, with simulated data for a paternity case (including a mutation).

* Improved formatting in output Excel document.

* Simplified code in sync with recent pedsuite updates. The plots may appear slightly modified.


# KLINK 0.6.1

* First CRAN release.


# KLINK 0.5.0

* Add karyogram showing marker positions.

* Add sheets "Unlinked" and "Linked only" in excel output.

* Tweaked button placements.


# KLINK 0.4.1

* New button letting the user choose fallback mutation model (applied when a model specified in the input file fails for whatever reason).

* Fixed a couple of minor bugs


# KLINK 0.4.0

* Added a `NEWS.md` file to track changes to the package.

* **pedtools** version 2.2.0 is now required.

* If the pedigrees prohibit arbitrary lumping, all complex mutation models are replaced with a simpler model. A notification to this effect is added when loading the file. This behaviour is currently triggered if either pedigree has an untyped nonfounder.   
