# KLINK 0.4.0

* Added a `NEWS.md` file to track changes to the package.

* **pedtools** version 2.2.0 is now required.

* If the pedigrees prohibit arbitrary lumping, all complex mutation models are replaced with simpler "proportional" models. A notification to this effect is added when loading the file. This behaviour is currently triggered if either pedigree has an untyped nonfounder.   