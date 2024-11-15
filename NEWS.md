# dev version

## New features

* Added new datasets `sibship` and `halfsib`. In the app, these are available as `Example1` and `Example2`, respectively.

* The **LR table** shown in the app has a new look, now using the same layout and marker order as the other tables. 
The "LR table" sheet of the Excel download also uses this marker order. 
*Note*: The *Report* sheets has not changed.

* New app controls `Empty markers` (hide/show) and `Likelihoods` (hide/show/loglik) 
for the LR table in the app and the Excel download.

* The Excel download includes a new sheet, `Plots`, containing the two pedigree plots.

* Added tooltips explaining app controls and table columns.

* Busy indicator: A pulsating effect is now shown when the app is busy.

* Only on shinyapp.io: Show banner warning about uploading sensitive data.

## Minor changes and fixes

* Empty the `.xml` field when a new `.fam` file is loaded.

* RStudio now stops the app when the user closes the browser.


# KLINK 1.0.2

* Fixed bug affecting download when XML initials contain hyphens.


# KLINK 1.0.1

* Modified the selection of markers for the `Unlinked report`. Within each pair, the marker with highest PIC value is chosen. For consistency, the PIC values used in this calculation are now always based on the same database (NorskDB_2024).

* Minor tweaks of app appearance.


# KLINK 1.0.0

This version is a major update of both the KLINK package and the shiny app.

* Expanded built-in genetic map covering 50 common STR markers, up from 18. Unlike the previous version, where linkage pairs were hard coded, the markers are now dynamically paired up after data is loaded, ensuring more optimal use of the data.

* The user may set a maximum linkage distance (cM), with the effect that markers farther than this are considered unlinked.

* Improved karyogram plot for clearer visualisation of markers and linkage.

* Improved readability of tables, using colours to indicate linked markers.

* New button "Reset" resetting all fields of the shiny app.

* New XML input field allowing the user to integrate data from other sources (particularly: Amelogenin genotypes and ID initials).


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

* Reading/writing `.fam` files is now handled by the new package `pedFamilias()`, which has been split out from `forrel`.

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
