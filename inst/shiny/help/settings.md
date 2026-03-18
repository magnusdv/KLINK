<style>
h1 {margin-top: 0;}
</style>

# Settings

### Marker map
The marker map specifies the genetic positions of markers, measured in centiMorgans (cM).

- **Built-in**: A map with updated positions for 50 markers from several common forensic STR kits. (Default.)
- **Custom**: Lets you upload your own map file.

A custom map file must be tab-delimited and contain columns matching **Marker**, **Chr**, and **cM**. Column order does not matter, and matching is case-insensitive. Longer names are also accepted, provided they start with matching terms; for example **Marker name** or **Chromosome**. If no column matches **cM**, KLINK will look for **POS** instead and use that column as the marker position.

Chromosome values must be positive integers, optionally written with the prefix `chr`.

### Map function
Controls how recombination fractions are derived from genetic distances.

- **Kosambi**: Allows for crossover interference. (Default.)
- **Haldane**: Assumes no crossover interference.

In most cases, the choice has only a modest effect. If in doubt, use **Kosambi**.

### Mutation model
Controls how mutations are modelled in the likelihood calculations.

- **Original**: Uses the mutation model specified in the input file. (Default.)
- **Simple**: Applies an 'equal' model with rate 0.001, to all markers.
- **Off**: Disables all mutation models.

The main purpose of the **Simple** option is to provide a lightweight fallback when the input file lacks mutation models. 
**Off** is intended mainly for testing. Note that the LR table always includes a 'No mut' column, regardless of this setting.

### Empty markers
Controls whether markers without genotype data are shown in the tables.

- **Hide**: Omit empty markers. (Default.)
- **Show**: Include them in the tables.

Affects both app display and downloaded tables.

### Likelihoods
Controls whether (log-)likelihood columns are included in the LR table.

- **Hide**: Show LR columns only. (Default.)
- **Show**: Show likelihood columns.
- **Loglik**: Show log-likelihood columns.

Affects app display only; both likelihoods and log-likelihoods are always included in the 'LR table' sheet of the Excel download.

### Decimals
Number of decimals shown in tables. Affects app display only; downloaded results use full precision.

### Unlinked > cM
Markers farther apart than this threshold are treated as unlinked.
Leave this empty to indicate no limit (this is the default), meaning that markers on the same chromosome are always linked.
