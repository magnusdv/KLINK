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
