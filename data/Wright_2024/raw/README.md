# Plant functional traits of the 100 most abundant species in the Pasoh forest reserve 50-ha forest dynamics plot, Malaysia

[https://doi.org/10.5061/dryad.q2bvq83v7](https://doi.org/10.5061/dryad.q2bvq83v7)

## Description of the data and file structure

 The functional traits data are in four tab-delimited txt files. The four files hold crown traits, leaf morphological traits, leaf nitrogen and phosphorus concentrations, and wood densities. The structure of each file is described below.

### Files and variables

#### File: Pasoh\_crown\_by\_ind.txt

**Description:** *Pasoh_crown_by_ind.txt* holds heights and mean crown diameters for 436 trees of 100 species. The 100 species were the 100 most abundant species in the 2005 census of the Pasoh 50-ha plot. The 436 trees were the largest individuals by DBH for each species in the 2005 census. We believe the heights and crown diameters approach species-specific maximum values because the 100th most abundant species was represented by 894 individuals in the 2005 census. There is one row of data for each tree. The only missing values are for the variable *variety*.

##### Variables

* sp: Six-letter code used to identify species in Pasoh 50-ha plot censuses.
* family: plant family
* genus: genus from Latin binomial
* species: species from Latin binomial
* variety: The category in the botanical nomenclatural hierarchy between species and form appropriate for distinctive taxa seen throughout the species range. Usually missing.
* growth.form: Tree growth forms developed for the Pasoh 50-ha plot. Values are E for species whose adults emerge above the surrounding forest canopy, C for species whose adults form part of the continuous forest canopy, U for species whose adults are typically 20 to 30 m tall, T for species whose adults are typically 10 to 20 m tall, and S for species whose adults are usually less than 10 m tall. The measured heights in the data file *Pasoh_crown_by_ind.txt* include many exceptions.
* tag: Unique integer on a permanent tag attached to each tree during 50-ha plot censuses. This variable links trees in *Pasoh_crown_by_ind.txt *and *Pasoh_leaf_traits_by_leaf.txt.*
* dbh: Diameter at breast height during the 2005 census of the 50-ha plot (units are cm).
* exposure: A five-point scale to quantify crown exposure to sunlight. Values are 1, 2, 3, 4, or 5. Methods are described in the subsection entitled *Leaf and crown traits*.
* diameter: Mean crown diameter (units are m). Methods are described in the subsection entitled *Leaf and crown traits*.
* height: Tree height (units are m). Methods are described in the subsection entitled *Leaf and crown traits*.

#### File: Pasoh\_leaf\_traits\_by\_leaf.txt

**Description:** *Pasoh_leaf_traits_by_leaf.txt* holds leaf traits for 862 leaves from 433 trees of the 100 most abundant species in the 2005 census of the Pasoh 50-ha plot. The 433 trees were the largest individuals by DBH for each species. There is one row of data for each leaf. Missing values are represented by NA.

##### Variables

* sp: Six-letter code used to identify species in Pasoh 50-ha plot censuses.
* family: plant family
* genus: genus from Latin binomial
* species: species from Latin binomial
* variety: The category in the botanical nomenclatural hierarchy between species and form appropriate for distinctive taxa seen throughout the species range. Usually missing.
* growth.form: Tree growth forms developed for the Pasoh 50-ha plot. Values are E for species whose adults emerge above the surrounding forest canopy, C for species whose adults form part of the continuous forest canopy, U for species whose adults are typically 20 to 30 m tall, T for species whose adults are typically 10 to 20 m tall, and S for species whose adults are usually less than 10 m tall. The measured heights in the data file *Pasoh_crown_by_ind.txt* include many exceptions.
* tag: Unique integer on a permanent tag attached to each tree during 50-ha plot censuses. This variable links trees in *Pasoh_crown_by_ind.txt *and *Pasoh_leaf_traits_by_leaf.txt.*
* dbh: Diameter at breast height during the 2005 census of the 50-ha plot (units are cm).
* exposure: A five-point scale to quantify crown exposure to sunlight. Values are 1, 2, 3, 4, or 5. Methods are described in the subsection entitled *Leaf and crown traits*.
* leaflets: Integer number of leaflets for compounds leaves, and 1 for simple leaves.
* leaf.area: Area of simple leaves (cm2) and missing for compound leaves. Excludes petiole.
* leaflet.area: The area of one leaflet for compound leaves (cm2) and missing for simple leaves. Excludes petiolule.
* mass.fresh: Fresh mass of the leaf for simple leaves and of one leaflet for compound leaves (units are g). Excludes petiole and petiolule.
* mass.dry: Oven-dry (60 °C) mass of the leaf for simple leaves and of one leaflet for compound leaves (units are g). Excludes petiole and petiolule.
* LDMC: Leaf dry matter content (a proportion). Calculated as mass.dry / mass.fresh.
* LMA: Leaf mass per area (g m-2). Calculated as mass.dry / leaf.area for simple leaves and as mass.dry / leaflet.area for compound leaves.
* lamina.thickness: Thickness of the lamina of the leaf or leaflet (units are microns).
* lamina.density: Leaf lamina tissue density (g cm-3). Calculated as LMA / lamina.thickness and correcting units.

#### File: Pasoh\_NP.txt

**Description:** *Pasoh_NP.txt* holds mean leaf tissue nitrogen and phosphorus concentrations for 100 species and 155 species-sun exposure combinations. The 100 species were the 100 most abundant species in the 2005 census of the Pasoh 50-ha plot. We targeted the largest individuals of each species, and many tall-statured species are only represented by fully sun-exposed leaves. Short-statured species can be represented by both sun-exposed and shade leaves or by shade leaves alone. There is one row of data for each species-sun exposure combination. The only missing values are for the variable *variety*.

##### Variables

* sp: Six-letter code used to identify species in Pasoh 50-ha plot censuses.
* family: plant family
* genus: genus from Latin binomial
* species: species from Latin binomial
* variety: The category in the botanical nomenclatural hierarchy between species and form appropriate for distinctive taxa seen throughout the species range. Usually missing.
* growth.form: Tree growth forms developed for the Pasoh 50-ha plot. Values are E for species whose adults emerge above the surrounding forest canopy, C for species whose adults form part of the continuous forest canopy, U for species whose adults are typically 20 to 30 m tall, T for species whose adults are typically 10 to 20 m tall, and S for species whose adults are usually less than 10 m tall. The measured heights in the data file *Pasoh_crown_by_ind.txt* include many exceptions.
* light: Values are ‘sun’ for fully sun-exposed leaves and ‘shade’ for leaves that receive minimal direct sunlight. Methods are described above in the subsection entitled *Leaf and crown traits*.
* N: Leaf tissue nitrogen concentrations (units are percentages). The laboratory protocol can be found at the end of the Methods description.
* P: Leaf tissue phosphorus concentrations (units are percentages). The laboratory protocol can be found at the end of the Methods description.

#### File: Pasoh\_Wood\_Density.txt

**Description:** *Pasoh_Wood_Density.txt* holds wood density values for 380 individual trees of 76 species. We targeted the most abundant species in the 2005 census of the Pasoh 50-ha plot that did not already have wood density values in the literature. We collected wood samples outside the 50-ha plot. There is one row of data for each tree. The only missing values are for the variable *variety*.

##### Variables

* sp: Six-letter code used to identify species in Pasoh 50-ha plot censuses.
* family: plant family
* genus: genus from Latin binomial
* species: species from Latin binomial
* variety: The category in the botanical nomenclatural hierarchy between species and form appropriate for distinctive taxa seen throughout the species range. Usually missing.
* growth.form: Tree growth forms developed for the Pasoh 50-ha plot. Values are E for species whose adults emerge above the surrounding forest canopy, C for species whose adults form part of the continuous forest canopy, U for species whose adults are typically 20 to 30 m tall, T for species whose adults are typically 10 to 20 m tall, and S for species whose adults are usually less than 10 m tall. The measured heights reported here include many exceptions.
* numero: A unique integer assigned to each tree. This variable cannot be used to relocate trees nor to link trees with other data files.
* dbh: Diameter at breast height measured in 2008 as samples were collected (units are cm).
* exposure: A five-point scale to quantify crown exposure to sunlight. Values are 1, 2, 3, 4, or 5. Methods are described in the subsection entitled *Leaf and crown traits*.
* wood.density: wood density or wood specific gravity (units are g cm-3). Methods are described in the subsection entitled *Wood density*.

## Code/software

The data are in standard tab-delimited txt files. No special software is needed to view the data.