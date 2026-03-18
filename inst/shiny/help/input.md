<style>
h1 {margin-top: 0;}
</style>

# Input

### Familias .fam file

The is the main input for KLINK, containing the pedigree structure, genotypes, allele frequencies and parameters for mutation models.
It is typically exported from the Familias program itself (https://www.familias.name/), but can also be created from R using the `pedFamilias` package.

### Optional .xml file

This file can be used to provide two things that are sometimes not included in the .fam file:

* Initials of the individuals, to be used in plots and tables instead of the (often long) sample labels.
* AMEL marker data, for providing or checking the sex of the individuals.

