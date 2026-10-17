# c15_landscape_radii

Snapshot `34_spde_converged_all` compared with `35_radii_all`.

347 files compared: 323 identical, 0 numerically identical (differences below 1e-9 relative), 24 changed, 0 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| landscape.csv | processed | changed | 4206 | 4206 | 0 | 0 |  | GRASSLAND_6000, GRASSLAND_8000, ARABLE_6000, ARABLE_8000, FOREST_6000, FOREST_8000, WATER_6000, WATER_8000, URBAN_6000, URBAN_8000, NATURAL_6000, NATURAL_8000, SANGUISORBA_6000, SANGUISORBA_8000 | 0 |
| revised__data_analysis.csv | processed | changed | 6576 | 6576 | 0 | 0 |  | GRASSLAND_6000, GRASSLAND_8000, ARABLE_6000, ARABLE_8000, FOREST_6000, FOREST_8000, WATER_6000, WATER_8000, URBAN_6000, URBAN_8000, NATURAL_6000, NATURAL_8000, SANGUISORBA_6000, SANGUISORBA_8000 | 0 |
| revised__data_clean.csv | processed | changed | 6576 | 6576 | 0 | 0 |  | GRASSLAND_6000, GRASSLAND_8000, ARABLE_6000, ARABLE_8000, FOREST_6000, FOREST_8000, WATER_6000, WATER_8000, URBAN_6000, URBAN_8000, NATURAL_6000, NATURAL_8000, SANGUISORBA_6000, SANGUISORBA_8000 | 0 |
| 21_landscape_correlation_between_radii.csv | tables | changed | 42 | 56 | 0 | 14 |  | r_6000, r_8000 | 0 |
| 21_landscape_summary.csv | tables | changed | 42 | 56 | 0 | 14 |  |  | 0 |
| revised__22_nausithous_coefficients.csv | tables | changed | 288 | 291 | 12 | 15 |  |  | 375 |
| revised__22_nausithous_diagnostics.csv | tables | changed | 24 | 24 | 0 | 0 |  |  | 7 |
| revised__22_nausithous_fit_statistics.csv | tables | changed | 24 | 24 | 0 | 0 |  |  | 12 |
| revised__22_nausithous_gvif.csv | tables | changed | 195 | 198 | 12 | 15 |  |  | 60 |
| revised__22_nausithous_habitat_landscape_estimates.csv | tables | changed | 42 | 45 | 37 | 40 |  |  | 5 |
| revised__22_nausithous_landscape_best.csv | tables | changed | 7 | 7 | 5 | 5 |  |  | 0 |
| revised__22_nausithous_landscape_scales.csv | tables | changed | 42 | 56 | 0 | 14 |  |  | 0 |
| revised__22_nausithous_random_effects.csv | tables | changed | 72 | 72 | 0 | 0 |  |  | 18 |
| revised__22_nausithous_specifications.csv | tables | changed | 24 | 24 | 3 | 3 |  |  | 0 |
| revised__22_teleius_bias_reduced.csv | tables | changed | 115 | 115 | 2 | 2 |  |  | 132 |
| revised__22_teleius_coefficients.csv | tables | changed | 294 | 294 | 6 | 6 |  |  | 476 |
| revised__22_teleius_diagnostics.csv | tables | changed | 24 | 24 | 0 | 0 |  |  | 8 |
| revised__22_teleius_fit_statistics.csv | tables | changed | 24 | 24 | 0 | 0 |  |  | 9 |
| revised__22_teleius_gvif.csv | tables | changed | 201 | 201 | 6 | 6 |  |  | 84 |
| revised__22_teleius_habitat_landscape_estimates.csv | tables | changed | 48 | 48 | 42 | 42 |  |  | 6 |
| revised__22_teleius_landscape_best.csv | tables | changed | 7 | 7 | 2 | 2 |  |  | 0 |
| revised__22_teleius_landscape_scales.csv | tables | changed | 42 | 56 | 0 | 14 |  |  | 0 |
| revised__22_teleius_random_effects.csv | tables | changed | 72 | 72 | 0 | 0 |  |  | 18 |
| revised__22_teleius_specifications.csv | tables | changed | 24 | 24 | 3 | 3 |  |  | 0 |

## Changed numbers

The full list of 1210 changed cells is in `c15_landscape_radii.csv`.

### revised__22_nausithous_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| with imputed \| (Intercept) #1 | estimate | -1.83460055962425 | -3.49577384876622 |
| nau_habitat_landscape \| with imputed \| TIMING #1 | estimate | 0.7846111517752 | 0.778588793294908 |
| nau_habitat_landscape \| with imputed \| METHOD #1 | estimate | 0.741397638288844 | 0.763634050686513 |
| nau_habitat_landscape \| with imputed \| PLANTabundant #1 | estimate | 0.949382371398952 | 0.937123877786273 |
| nau_habitat_landscape \| with imputed \| PLANTdominant #1 | estimate | 0.932061349681777 | 0.922297292441121 |
| nau_habitat_landscape \| with imputed \| EVL_species #1 | estimate | 0.38078210347089 | 0.518192260739924 |
| nau_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | estimate | 0.0399986000391345 | 0.0417567409148364 |
| nau_habitat_landscape \| with imputed \| WET_10 #1 | estimate | 0.0220986170162074 | 0.0177412142004259 |
| nau_habitat_landscape \| with imputed \| X_10 #1 | estimate | -0.00112012161815638 | 0.00270987623238416 |
| nau_habitat_landscape \| with imputed \| URBAN_1000 #1 | estimate | 0.317066581336089 | 0.360396163888823 |
| nau_habitat_landscape \| with imputed \| ERA2021-2024 #1 | estimate | -0.71417380014514 | -0.674086586940407 |
| nau_habitat_landscape \| with imputed \| DOY_STD #1 | estimate | -0.00874275708242149 | -0.00533469860929205 |
| nau_habitat_landscape \| with imputed \| I(DOY_STD^2) #1 | estimate | -0.232873250701444 | -0.233963354232638 |
| nau_habitat_landscape \| with imputed \| TIMING:METHOD #1 | estimate | -0.73790224371008 | -0.735714946698332 |
| nau_habitat_landscape_pls \| with plausible imputed \| (Intercept) #1 | estimate | -1.80426946212652 | -3.36307651308281 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | estimate | 0.787577838247985 | 0.782456282891557 |
| nau_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | estimate | 0.734882255181665 | 0.755289175810893 |
| nau_habitat_landscape_pls \| with plausible imputed \| PLANTabundant #1 | estimate | 0.924270563334525 | 0.912506183211091 |
| nau_habitat_landscape_pls \| with plausible imputed \| PLANTdominant #1 | estimate | 0.895217280465565 | 0.886329667073645 |
| nau_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | estimate | 0.33883081730999 | 0.473868073914517 |
| nau_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_10 #1 | estimate | 0.0384320637449465 | 0.0400124268380687 |
| nau_habitat_landscape_pls \| with plausible imputed \| WET_10 #1 | estimate | 0.0227746333493055 | 0.0182906742659365 |
| nau_habitat_landscape_pls \| with plausible imputed \| X_10 #1 | estimate | -0.00285125122944357 | 0.000731155804032331 |
| nau_habitat_landscape_pls \| with plausible imputed \| URBAN_1000 #1 | estimate | 0.300176754985337 | 0.340673653700459 |
| nau_habitat_landscape_pls \| with plausible imputed \| ERA2021-2024 #1 | estimate | -0.666293748856413 | -0.628258881554176 |
| nau_habitat_landscape_pls \| with plausible imputed \| DOY_STD #1 | estimate | -0.00248113379342898 | 0.000266945529240463 |
| nau_habitat_landscape_pls \| with plausible imputed \| I(DOY_STD^2) #1 | estimate | -0.233606431570398 | -0.234177269192629 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING:METHOD #1 | estimate | -0.722549235599552 | -0.720742900309173 |
| nau_habitat_landscape_obs \| observed only \| (Intercept) #1 | estimate | -2.05319740278545 | -4.36801235823545 |
| nau_habitat_landscape_obs \| observed only \| TIMING #1 | estimate | 0.903109423086436 | 0.896996740997316 |
| nau_habitat_landscape_obs \| observed only \| METHOD #1 | estimate | 0.947675380481022 | 0.98475170285051 |
| nau_habitat_landscape_obs \| observed only \| PLANTabundant #1 | estimate | 1.15308893444679 | 1.14907784396153 |
| nau_habitat_landscape_obs \| observed only \| PLANTdominant #1 | estimate | 1.03667476446042 | 1.05010210602736 |
| nau_habitat_landscape_obs \| observed only \| EVL_species #1 | estimate | 0.464343220427102 | 0.664095667608644 |
| nau_habitat_landscape_obs \| observed only \| SANGUISORBA_10 #1 | estimate | 0.0434488627130522 | 0.0454987476884985 |
| nau_habitat_landscape_obs \| observed only \| WET_10 #1 | estimate | 0.0223577561612446 | 0.0200607278905799 |
| nau_habitat_landscape_obs \| observed only \| X_10 #1 | estimate | -0.00652742050061914 | -0.0017550566849277 |
| nau_habitat_landscape_obs \| observed only \| URBAN_1000 #1 | estimate | 0.319836829627336 | 0.389872029839411 |
| nau_habitat_landscape_obs \| observed only \| ERA2021-2024 #1 | estimate | -0.378635147293733 | -0.316210468789613 |
| nau_habitat_landscape_obs \| observed only \| DOY_STD #1 | estimate | -0.228281316801245 | -0.231123829059277 |
| nau_habitat_landscape_obs \| observed only \| I(DOY_STD^2) #1 | estimate | -0.184094479319736 | -0.185106018332617 |
| nau_habitat_landscape_obs \| observed only \| TIMING:METHOD #1 | estimate | -0.809780716806738 | -0.813939955495063 |
| nau_habitat_landscape \| with imputed \| (Intercept) #1 | std_error | 1.08544308387244 | 1.36744784315639 |
| nau_habitat_landscape \| with imputed \| TIMING #1 | std_error | 0.194372246742448 | 0.194580509004212 |
| nau_habitat_landscape \| with imputed \| METHOD #1 | std_error | 0.249854396514213 | 0.249631511450317 |
| nau_habitat_landscape \| with imputed \| PLANTabundant #1 | std_error | 0.142010342734134 | 0.142154573848807 |
| nau_habitat_landscape \| with imputed \| PLANTdominant #1 | std_error | 0.244860101954579 | 0.2446188457026 |
| nau_habitat_landscape \| with imputed \| EVL_species #1 | std_error | 0.293099184220048 | 0.300133660758064 |
| nau_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | std_error | 0.0171439228812318 | 0.0171540623240075 |
| nau_habitat_landscape \| with imputed \| WET_10 #1 | std_error | 0.0194045937934522 | 0.019284101288413 |
| nau_habitat_landscape \| with imputed \| X_10 #1 | std_error | 0.0228448768000614 | 0.0229715653430456 |
| nau_habitat_landscape \| with imputed \| URBAN_1000 #1 | std_error | 0.087087120723457 | 0.0835075523578181 |
| nau_habitat_landscape \| with imputed \| ERA2021-2024 #1 | std_error | 0.390010399955431 | 0.382006419657622 |
| nau_habitat_landscape \| with imputed \| DOY_STD #1 | std_error | 0.101966482741597 | 0.10193075847815 |
| nau_habitat_landscape \| with imputed \| I(DOY_STD^2) #1 | std_error | 0.0628557771188687 | 0.0628417603443605 |
| nau_habitat_landscape \| with imputed \| TIMING:METHOD #1 | std_error | 0.292992950560056 | 0.293053657560688 |
| nau_habitat_landscape_pls \| with plausible imputed \| (Intercept) #1 | std_error | 1.0723395852488 | 1.3453640685074 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | std_error | 0.192719366411972 | 0.19285513190822 |
| nau_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | std_error | 0.248141983956047 | 0.247893324438381 |
| nau_habitat_landscape_pls \| with plausible imputed \| PLANTabundant #1 | std_error | 0.141299759802686 | 0.14139352625444 |

_Showing the first 60 of 375 rows._

### revised__22_nausithous_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| with imputed \| TRUE \| FALSE \| collinear terms #1 | max_gvif_adjusted | 3.30480477849636 | 3.74924717289305 |
| nau_habitat_landscape_pls \| with plausible imputed \| TRUE \| FALSE \| collinear terms #1 | max_gvif_adjusted | 3.25517874164036 | 3.65980895197018 |
| nau_habitat_landscape_obs \| observed only \| TRUE \| FALSE \| collinear terms #1 | max_gvif_adjusted | 3.21302302921924 | 3.5785250587186 |
| nau_habitat_landscape \| with imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_uniformity_p | 0.247269271389766 | 0.335826894228151 |
| nau_habitat_landscape_pls \| with plausible imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_uniformity_p | 0.122926830793363 | 0.123590048542241 |
| nau_habitat_landscape_obs \| observed only \| TRUE \| FALSE \| collinear terms #1 | dharma_uniformity_p | 0.0751105646098552 | 0.04799781566118 |
| nau_habitat_landscape_obs \| observed only \| TRUE \| FALSE \| collinear terms #1 | dharma_dispersion_p | 0.256 | 0.368 |

### revised__22_nausithous_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | AIC | 2077.14042936784 | 2067.25496236607 |
| nau_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | AIC | 2061.25152230389 | 2051.91559325275 |
| nau_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | AIC | 1814.53090766499 | 1802.6186308789 |
| nau_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | BIC | 2193.94582362358 | 2189.62251825303 |
| nau_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | BIC | 2177.27863797253 | 2173.46780966752 |
| nau_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | BIC | 1928.99478438544 | 1922.53316839556 |
| nau_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | logLik | -1017.57021468392 | -1011.62748118303 |
| nau_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | logLik | -1009.62576115195 | -1003.95779662638 |
| nau_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | logLik | -886.265453832497 | -879.309315439451 |
| nau_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | df | 21 | 22 |
| nau_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | df | 21 | 22 |
| nau_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | df | 21 | 22 |

### revised__22_nausithous_gvif.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| with imputed \| TIMING #1 | GVIF | 1.2302903609662 | 1.23043263732699 |
| nau_habitat_landscape \| with imputed \| METHOD #1 | GVIF | 1.11717669938725 | 1.12476413506114 |
| nau_habitat_landscape \| with imputed \| PLANT #1 | GVIF | 1.20366422712782 | 1.23132783169175 |
| nau_habitat_landscape \| with imputed \| EVL_species #1 | GVIF | 1.0377335468044 | 1.06468349529506 |
| nau_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | GVIF | 1.40983361951482 | 1.42670707866363 |
| nau_habitat_landscape \| with imputed \| WET_10 #1 | GVIF | 1.35546578107409 | 1.34483302750734 |
| nau_habitat_landscape \| with imputed \| X_10 #1 | GVIF | 1.13186217494284 | 1.14679008466566 |
| nau_habitat_landscape \| with imputed \| URBAN_1000 #1 | GVIF | 1.82100427083182 | 1.63348572618404 |
| nau_habitat_landscape \| with imputed \| ERA #1 | GVIF | 1.06256396962113 | 1.0808143853115 |
| nau_habitat_landscape \| with imputed \| DOY_STD #1 | GVIF | 1.03780833856819 | 1.03924532216792 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | GVIF | 1.23526222365713 | 1.23413576247886 |
| nau_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | GVIF | 1.11614580606297 | 1.12338286929737 |
| nau_habitat_landscape_pls \| with plausible imputed \| PLANT #1 | GVIF | 1.20467574472813 | 1.2276089705463 |
| nau_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | GVIF | 1.03912600377546 | 1.06886918875636 |
| nau_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_10 #1 | GVIF | 1.40617370625499 | 1.42263521965145 |
| nau_habitat_landscape_pls \| with plausible imputed \| WET_10 #1 | GVIF | 1.34862323968278 | 1.337901573807 |
| nau_habitat_landscape_pls \| with plausible imputed \| X_10 #1 | GVIF | 1.13561582268284 | 1.15186403447824 |
| nau_habitat_landscape_pls \| with plausible imputed \| URBAN_1000 #1 | GVIF | 1.83996241094926 | 1.64997004268134 |
| nau_habitat_landscape_pls \| with plausible imputed \| ERA #1 | GVIF | 1.05250456579934 | 1.06733313563163 |
| nau_habitat_landscape_pls \| with plausible imputed \| DOY_STD #1 | GVIF | 1.03525720001771 | 1.03691944233666 |
| nau_habitat_landscape_obs \| observed only \| TIMING #1 | GVIF | 1.24457753233904 | 1.24803273670924 |
| nau_habitat_landscape_obs \| observed only \| METHOD #1 | GVIF | 1.12585304576591 | 1.13403745218127 |
| nau_habitat_landscape_obs \| observed only \| PLANT #1 | GVIF | 1.20875590861307 | 1.2351709514608 |
| nau_habitat_landscape_obs \| observed only \| EVL_species #1 | GVIF | 1.0574968831098 | 1.08275450540784 |
| nau_habitat_landscape_obs \| observed only \| SANGUISORBA_10 #1 | GVIF | 1.39511653268163 | 1.41203485371113 |
| nau_habitat_landscape_obs \| observed only \| WET_10 #1 | GVIF | 1.34926335576344 | 1.33541865932468 |
| nau_habitat_landscape_obs \| observed only \| X_10 #1 | GVIF | 1.13153236924797 | 1.14455802208241 |
| nau_habitat_landscape_obs \| observed only \| URBAN_1000 #1 | GVIF | 1.85792985161929 | 1.67399785051744 |
| nau_habitat_landscape_obs \| observed only \| ERA #1 | GVIF | 1.05873381327145 | 1.06960080773935 |
| nau_habitat_landscape_obs \| observed only \| DOY_STD #1 | GVIF | 1.02896434767595 | 1.02852218687466 |
| nau_habitat_landscape \| with imputed \| TIMING #1 | GVIF_adjusted | 1.10918454774947 | 1.10924868146281 |
| nau_habitat_landscape \| with imputed \| METHOD #1 | GVIF_adjusted | 1.05696579858917 | 1.06054897815289 |
| nau_habitat_landscape \| with imputed \| PLANT #1 | GVIF_adjusted | 1.04743320714307 | 1.05340026671944 |
| nau_habitat_landscape \| with imputed \| EVL_species #1 | GVIF_adjusted | 1.01869207653952 | 1.03183501360201 |
| nau_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | GVIF_adjusted | 1.18736414781432 | 1.19444844119101 |
| nau_habitat_landscape \| with imputed \| WET_10 #1 | GVIF_adjusted | 1.16424472559428 | 1.1596693612868 |
| nau_habitat_landscape \| with imputed \| X_10 #1 | GVIF_adjusted | 1.06389011412967 | 1.0708828529142 |
| nau_habitat_landscape \| with imputed \| URBAN_1000 #1 | GVIF_adjusted | 1.34944591252551 | 1.27807892017044 |
| nau_habitat_landscape \| with imputed \| ERA #1 | GVIF_adjusted | 1.03080743576147 | 1.0396222320206 |
| nau_habitat_landscape \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.01872878557945 | 1.01943382432011 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | GVIF_adjusted | 1.11142351228374 | 1.11091663165103 |
| nau_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | GVIF_adjusted | 1.05647801967811 | 1.05989757490871 |
| nau_habitat_landscape_pls \| with plausible imputed \| PLANT #1 | GVIF_adjusted | 1.04765319444874 | 1.05260399340671 |
| nau_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | GVIF_adjusted | 1.01937530074819 | 1.03386130054101 |
| nau_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_10 #1 | GVIF_adjusted | 1.18582195385943 | 1.19274272986736 |
| nau_habitat_landscape_pls \| with plausible imputed \| WET_10 #1 | GVIF_adjusted | 1.16130238942438 | 1.15667695308889 |
| nau_habitat_landscape_pls \| with plausible imputed \| X_10 #1 | GVIF_adjusted | 1.06565276834569 | 1.07324928813312 |
| nau_habitat_landscape_pls \| with plausible imputed \| URBAN_1000 #1 | GVIF_adjusted | 1.35645214104636 | 1.28451159694311 |
| nau_habitat_landscape_pls \| with plausible imputed \| ERA #1 | GVIF_adjusted | 1.02591645166619 | 1.03311816150508 |
| nau_habitat_landscape_pls \| with plausible imputed \| DOY_STD #1 | GVIF_adjusted | 1.01747589652911 | 1.01829241494605 |
| nau_habitat_landscape_obs \| observed only \| TIMING #1 | GVIF_adjusted | 1.11560635187285 | 1.11715385543319 |
| nau_habitat_landscape_obs \| observed only \| METHOD #1 | GVIF_adjusted | 1.06106222520921 | 1.06491194574071 |
| nau_habitat_landscape_obs \| observed only \| PLANT #1 | GVIF_adjusted | 1.04853915448259 | 1.05422125314453 |
| nau_habitat_landscape_obs \| observed only \| EVL_species #1 | GVIF_adjusted | 1.02834667457516 | 1.04055490263986 |
| nau_habitat_landscape_obs \| observed only \| SANGUISORBA_10 #1 | GVIF_adjusted | 1.18115051228945 | 1.18829072777293 |
| nau_habitat_landscape_obs \| observed only \| WET_10 #1 | GVIF_adjusted | 1.16157795939981 | 1.15560315823585 |
| nau_habitat_landscape_obs \| observed only \| X_10 #1 | GVIF_adjusted | 1.06373510294996 | 1.06984018529985 |
| nau_habitat_landscape_obs \| observed only \| URBAN_1000 #1 | GVIF_adjusted | 1.36305900518624 | 1.29383068850504 |
| nau_habitat_landscape_obs \| observed only \| ERA #1 | GVIF_adjusted | 1.02894791572336 | 1.03421506841631 |
| nau_habitat_landscape_obs \| observed only \| DOY_STD #1 | GVIF_adjusted | 1.01437879890894 | 1.01416082889977 |

### revised__22_nausithous_habitat_landscape_estimates.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| with imputed \| SANGUISORBA_10 \| 1.04 (1.01-1.08) #1 | p_value | 0.0196425498854567 | 0.0149239709927419 |
| nau_habitat_landscape \| with imputed \| WET_10 \| 1.02 (0.98-1.06) #1 | p_value | 0.254772272055966 | 0.357577067517427 |
| nau_habitat_landscape \| with imputed \| TIMING:METHOD \| 0.48 (0.27-0.85) #1 | p_value | 0.0117856457243978 | 0.0120555965084895 |
| nau_habitat_landscape_pls \| with plausible imputed \| TIMING:METHOD \| 0.49 (0.27-0.86) #1 | p_value | 0.0131289889128086 | 0.0133517539185195 |
| nau_habitat_landscape_obs \| observed only \| TIMING:METHOD \| 0.44 (0.24-0.83) #1 | p_value | 0.0111149951648175 | 0.0107864456046309 |

### revised__22_nausithous_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| nau_habitat_landscape \| with imputed \| SITMAP #1 | variance | 0.602558227954588 | 0.572580095733399 |
| nau_habitat_landscape \| with imputed \| YEAR #1 | variance | 0.161990148581983 | 0.15400379188918 |
| nau_habitat_landscape \| with imputed \| OBSERVER #1 | variance | 0.87164432982236 | 0.806757449822367 |
| nau_habitat_landscape_pls \| with plausible imputed \| SITMAP #1 | variance | 0.541754528671399 | 0.511075699875285 |
| nau_habitat_landscape_pls \| with plausible imputed \| YEAR #1 | variance | 0.165034699848794 | 0.157940150976605 |
| nau_habitat_landscape_pls \| with plausible imputed \| OBSERVER #1 | variance | 0.799035107701442 | 0.738933041254474 |
| nau_habitat_landscape_obs \| observed only \| SITMAP #1 | variance | 0.900271514452487 | 0.869759055310016 |
| nau_habitat_landscape_obs \| observed only \| YEAR #1 | variance | 0.289094522912653 | 0.282289127558934 |
| nau_habitat_landscape_obs \| observed only \| OBSERVER #1 | variance | 1.10176261268143 | 1.03520275363403 |
| nau_habitat_landscape \| with imputed \| SITMAP #1 | sd | 0.776246241829607 | 0.756690224420402 |
| nau_habitat_landscape \| with imputed \| YEAR #1 | sd | 0.402479997741482 | 0.39243316869141 |
| nau_habitat_landscape \| with imputed \| OBSERVER #1 | sd | 0.93361894251475 | 0.898196776782442 |
| nau_habitat_landscape_pls \| with plausible imputed \| SITMAP #1 | sd | 0.736039760251713 | 0.714895586694508 |
| nau_habitat_landscape_pls \| with plausible imputed \| YEAR #1 | sd | 0.406244630547646 | 0.397416847877144 |
| nau_habitat_landscape_pls \| with plausible imputed \| OBSERVER #1 | sd | 0.893887637067122 | 0.859612145827683 |
| nau_habitat_landscape_obs \| observed only \| SITMAP #1 | sd | 0.948826387940643 | 0.932608736453833 |
| nau_habitat_landscape_obs \| observed only \| YEAR #1 | sd | 0.537675109069272 | 0.531308881498262 |
| nau_habitat_landscape_obs \| observed only \| OBSERVER #1 | sd | 1.0496488044491 | 1.01744914056381 |

### revised__22_teleius_bias_reduced.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape_obs_brglm \| observed only \| (Intercept) #1 | estimate | 5.43343172613145 | 4.0206975268971 |
| tel_habitat_landscape_obs_brglm \| observed only \| TIMING #1 | estimate | 0.410017596513075 | 0.512660719147062 |
| tel_habitat_landscape_obs_brglm \| observed only \| METHOD #1 | estimate | 0.41101279037441 | 0.445703793375118 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTabundant #1 | estimate | 0.875810668570278 | 0.723993546965192 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTdominant #1 | estimate | 1.45614017692792 | 1.46765460914235 |
| tel_habitat_landscape_obs_brglm \| observed only \| EVL_species #1 | estimate | 0.937592593747161 | 0.794483792033909 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_10 #1 | estimate | -0.0486976436401297 | -0.0718774488745874 |
| tel_habitat_landscape_obs_brglm \| observed only \| WET_10 #1 | estimate | 0.00783968374696017 | 0.00698506057681001 |
| tel_habitat_landscape_obs_brglm \| observed only \| X_10 #1 | estimate | -0.0621707991027011 | -0.0802879842900655 |
| tel_habitat_landscape_obs_brglm \| observed only \| GRASSLAND_1000 #1 | estimate | -0.07377927945803 | 0.00198745177037343 |
| tel_habitat_landscape_obs_brglm \| observed only \| NATURAL_4000 #1 | estimate | 0.0648271840899318 | 0.186369207806528 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_3000 #1 | estimate | 1.86179368518841 | 1.71624057717004 |
| tel_habitat_landscape_obs_brglm \| observed only \| URBAN_4000 #1 | estimate | -0.521710596945992 | -0.250739956718961 |
| tel_habitat_landscape_obs_brglm \| observed only \| WATER_4000 #1 | estimate | 2.2013952685893 | 2.83507442317757 |
| tel_habitat_landscape_obs_brglm \| observed only \| ERA2021-2024 #1 | estimate | -5.80068879217181 | -5.75523639931106 |
| tel_habitat_landscape_obs_brglm \| observed only \| DOY_STD #1 | estimate | -1.42293385190023 | -1.46917905032322 |
| tel_habitat_landscape_obs_brglm \| observed only \| I(DOY_STD^2) #1 | estimate | 0.365325303775521 | 0.358212464285855 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2020 #1 | estimate | -6.51624753393427 | -6.62588475369951 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2021 #1 | estimate | 2.04057329846277 | 2.13750434241223 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2022 #1 | estimate | 1.71487587566862 | 1.74322916228151 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2023 #1 | estimate | 0.217769131644903 | 0.138536857126131 |
| tel_habitat_landscape_obs_brglm \| observed only \| TIMING:METHOD #1 | estimate | 0.7612694294335 | 0.881720965246292 |
| tel_habitat_landscape_obs_brglm \| observed only \| (Intercept) #1 | std_error | 2.93956216984992 | 2.95385690056745 |
| tel_habitat_landscape_obs_brglm \| observed only \| TIMING #1 | std_error | 0.608669436782409 | 0.620495132645785 |
| tel_habitat_landscape_obs_brglm \| observed only \| METHOD #1 | std_error | 0.829542898423279 | 0.858432094338526 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTabundant #1 | std_error | 0.415039508807047 | 0.417904432397012 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTdominant #1 | std_error | 0.595133044298348 | 0.599891509571118 |
| tel_habitat_landscape_obs_brglm \| observed only \| EVL_species #1 | std_error | 0.616064229985801 | 0.627927585204212 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_10 #1 | std_error | 0.050068259082968 | 0.0510350352470559 |
| tel_habitat_landscape_obs_brglm \| observed only \| WET_10 #1 | std_error | 0.0521930984966215 | 0.0524822060593647 |
| tel_habitat_landscape_obs_brglm \| observed only \| X_10 #1 | std_error | 0.0662916087627318 | 0.0673473510922961 |
| tel_habitat_landscape_obs_brglm \| observed only \| GRASSLAND_1000 #1 | std_error | 0.18526688668593 | 0.157435041949835 |
| tel_habitat_landscape_obs_brglm \| observed only \| NATURAL_4000 #1 | std_error | 0.162571262679472 | 0.14524088484809 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_3000 #1 | std_error | 0.572507115986413 | 0.584775252463336 |
| tel_habitat_landscape_obs_brglm \| observed only \| URBAN_4000 #1 | std_error | 0.462822049102777 | 0.432629008717544 |
| tel_habitat_landscape_obs_brglm \| observed only \| WATER_4000 #1 | std_error | 1.00527537982784 | 1.04125971808934 |
| tel_habitat_landscape_obs_brglm \| observed only \| ERA2021-2024 #1 | std_error | 1.96150096620222 | 1.99653282537997 |
| tel_habitat_landscape_obs_brglm \| observed only \| DOY_STD #1 | std_error | 0.424947629785743 | 0.443088962465994 |
| tel_habitat_landscape_obs_brglm \| observed only \| I(DOY_STD^2) #1 | std_error | 0.20991670945397 | 0.216523709404066 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2020 #1 | std_error | 1.88329803074231 | 1.92276821246617 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2021 #1 | std_error | 0.883343579536146 | 0.928786323655403 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2022 #1 | std_error | 0.768961310758853 | 0.784914562519923 |
| tel_habitat_landscape_obs_brglm \| observed only \| factor(YEAR)2023 #1 | std_error | 0.78713942267657 | 0.801114672932337 |
| tel_habitat_landscape_obs_brglm \| observed only \| TIMING:METHOD #1 | std_error | 0.94571383381892 | 0.969481499091529 |
| tel_habitat_landscape_obs_brglm \| observed only \| (Intercept) #1 | p_value | 0.0645472029108516 | 0.173460386366365 |
| tel_habitat_landscape_obs_brglm \| observed only \| TIMING #1 | p_value | 0.500546989261561 | 0.408683664347316 |
| tel_habitat_landscape_obs_brglm \| observed only \| METHOD #1 | p_value | 0.620269099152227 | 0.603616527308666 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTabundant #1 | p_value | 0.0348423156094174 | 0.0831955952061492 |
| tel_habitat_landscape_obs_brglm \| observed only \| PLANTdominant #1 | p_value | 0.0144151862283 | 0.0144237436157555 |
| tel_habitat_landscape_obs_brglm \| observed only \| EVL_species #1 | p_value | 0.128032332217733 | 0.205782628607128 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_10 #1 | p_value | 0.330739681577001 | 0.159014368547778 |
| tel_habitat_landscape_obs_brglm \| observed only \| WET_10 #1 | p_value | 0.880602588169732 | 0.894119133009731 |
| tel_habitat_landscape_obs_brglm \| observed only \| X_10 #1 | p_value | 0.348327597042011 | 0.233203367853612 |
| tel_habitat_landscape_obs_brglm \| observed only \| GRASSLAND_1000 #1 | p_value | 0.690458874984788 | 0.989927814382951 |
| tel_habitat_landscape_obs_brglm \| observed only \| NATURAL_4000 #1 | p_value | 0.690068841030828 | 0.199431410658105 |
| tel_habitat_landscape_obs_brglm \| observed only \| SANGUISORBA_3000 #1 | p_value | 0.00114595638746564 | 0.00333685191546234 |
| tel_habitat_landscape_obs_brglm \| observed only \| URBAN_4000 #1 | p_value | 0.259641871752637 | 0.562202816773711 |
| tel_habitat_landscape_obs_brglm \| observed only \| WATER_4000 #1 | p_value | 0.0285356239232256 | 0.00647439410880174 |
| tel_habitat_landscape_obs_brglm \| observed only \| ERA2021-2024 #1 | p_value | 0.00310375780373293 | 0.00394388563782112 |
| tel_habitat_landscape_obs_brglm \| observed only \| DOY_STD #1 | p_value | 0.000812525484959451 | 0.000913925154110064 |

_Showing the first 60 of 132 rows._

### revised__22_teleius_coefficients.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| with imputed \| (Intercept) #1 | estimate | -3.44460783699477 | -3.4554844002767 |
| tel_habitat_landscape \| with imputed \| TIMING #1 | estimate | 0.106913732707576 | 0.112041968740925 |
| tel_habitat_landscape \| with imputed \| METHOD #1 | estimate | 0.627240287485216 | 0.643996727357121 |
| tel_habitat_landscape \| with imputed \| PLANTabundant #1 | estimate | 0.591629897219723 | 0.555669303611457 |
| tel_habitat_landscape \| with imputed \| PLANTdominant #1 | estimate | 0.922657265460941 | 0.895362930145441 |
| tel_habitat_landscape \| with imputed \| EVL_species #1 | estimate | 1.45988524502733 | 1.39911408431541 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | estimate | -0.00660436072555254 | -0.00854563507470964 |
| tel_habitat_landscape \| with imputed \| WET_10 #1 | estimate | -0.0140849710464804 | -0.0140302355751644 |
| tel_habitat_landscape \| with imputed \| X_10 #1 | estimate | -0.0179558030500544 | -0.0191337583826112 |
| tel_habitat_landscape \| with imputed \| GRASSLAND_1000 #1 | estimate | 0.198332768487906 | 0.197193244068285 |
| tel_habitat_landscape \| with imputed \| NATURAL_4000 #1 | estimate | 0.0493771517108402 | 0.0652890427636786 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_3000 #1 | estimate | 0.369319693065589 | 0.32612137258335 |
| tel_habitat_landscape \| with imputed \| URBAN_4000 #1 | estimate | -0.667371844310983 | -0.601075431337075 |
| tel_habitat_landscape \| with imputed \| WATER_4000 #1 | estimate | 1.78944710818978 | 1.93839012451978 |
| tel_habitat_landscape \| with imputed \| ERA2021-2024 #1 | estimate | 0.169005795067824 | 0.207926685420489 |
| tel_habitat_landscape \| with imputed \| DOY_STD #1 | estimate | -1.23620467850679 | -1.23885049679293 |
| tel_habitat_landscape \| with imputed \| I(DOY_STD^2) #1 | estimate | 0.223808071279696 | 0.226735530232865 |
| tel_habitat_landscape \| with imputed \| TIMING:METHOD #1 | estimate | 0.161629343384758 | 0.170885966672514 |
| tel_habitat_landscape_pls \| with plausible imputed \| (Intercept) #1 | estimate | -2.79622511806898 | -2.792026128477 |
| tel_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | estimate | 0.152650556702232 | 0.165238591713487 |
| tel_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | estimate | 0.716157697033706 | 0.733265833881928 |
| tel_habitat_landscape_pls \| with plausible imputed \| PLANTabundant #1 | estimate | 0.56905908741756 | 0.542404406613158 |
| tel_habitat_landscape_pls \| with plausible imputed \| PLANTdominant #1 | estimate | 1.12089646333096 | 1.09697284100378 |
| tel_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | estimate | 1.08572829135352 | 1.04929447594748 |
| tel_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_10 #1 | estimate | -0.0196952282375033 | -0.0219556560359177 |
| tel_habitat_landscape_pls \| with plausible imputed \| WET_10 #1 | estimate | -0.00611783450779217 | -0.00593528391482211 |
| tel_habitat_landscape_pls \| with plausible imputed \| X_10 #1 | estimate | -0.0329541625374988 | -0.0342533027443626 |
| tel_habitat_landscape_pls \| with plausible imputed \| GRASSLAND_1000 #1 | estimate | 0.118780646671472 | 0.119753555225059 |
| tel_habitat_landscape_pls \| with plausible imputed \| NATURAL_4000 #1 | estimate | 0.056929814612141 | 0.074804863317581 |
| tel_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_3000 #1 | estimate | 0.389511954319039 | 0.344075564984849 |
| tel_habitat_landscape_pls \| with plausible imputed \| URBAN_4000 #1 | estimate | -0.812841780413731 | -0.759165323659666 |
| tel_habitat_landscape_pls \| with plausible imputed \| WATER_4000 #1 | estimate | 1.7912640720086 | 1.90332348298748 |
| tel_habitat_landscape_pls \| with plausible imputed \| ERA2021-2024 #1 | estimate | 0.242139961857295 | 0.265516901589453 |
| tel_habitat_landscape_pls \| with plausible imputed \| DOY_STD #1 | estimate | -1.02429896765376 | -1.03238581329704 |
| tel_habitat_landscape_pls \| with plausible imputed \| I(DOY_STD^2) #1 | estimate | 0.188069918681894 | 0.193132639251703 |
| tel_habitat_landscape_pls \| with plausible imputed \| TIMING:METHOD #1 | estimate | -0.00276145007242981 | -0.00702182804761833 |
| tel_habitat_landscape_obs \| observed only \| (Intercept) #1 | estimate | 5.88945259315514 | 29.3743882920703 |
| tel_habitat_landscape_obs \| observed only \| TIMING #1 | estimate | 41.1734139916185 | 28.5415808133863 |
| tel_habitat_landscape_obs \| observed only \| METHOD #1 | estimate | -8.40343184091532 | -20.8193975233129 |
| tel_habitat_landscape_obs \| observed only \| PLANTabundant #1 | estimate | 8.36158612314983 | 13.5527064438232 |
| tel_habitat_landscape_obs \| observed only \| PLANTdominant #1 | estimate | 61.5343586443721 | 35.4254220843586 |
| tel_habitat_landscape_obs \| observed only \| EVL_species #1 | estimate | 102.261812127502 | 63.1050424184686 |
| tel_habitat_landscape_obs \| observed only \| SANGUISORBA_10 #1 | estimate | -3.10995994105768 | -2.34557012142931 |
| tel_habitat_landscape_obs \| observed only \| WET_10 #1 | estimate | 2.45586855541633 | 0.754817843604516 |
| tel_habitat_landscape_obs \| observed only \| X_10 #1 | estimate | -2.83337401911985 | -2.40229058150404 |
| tel_habitat_landscape_obs \| observed only \| GRASSLAND_1000 #1 | estimate | -6.18347813437318 | -6.30408919184274 |
| tel_habitat_landscape_obs \| observed only \| NATURAL_4000 #1 | estimate | 13.0912366914524 | 7.10547303714984 |
| tel_habitat_landscape_obs \| observed only \| SANGUISORBA_3000 #1 | estimate | 59.2084596777559 | 49.5338251213197 |
| tel_habitat_landscape_obs \| observed only \| URBAN_4000 #1 | estimate | -79.2927232046961 | -58.8070563556048 |
| tel_habitat_landscape_obs \| observed only \| WATER_4000 #1 | estimate | 112.660462433808 | 99.950840156094 |
| tel_habitat_landscape_obs \| observed only \| ERA2021-2024 #1 | estimate | -2.45925239681251 | 8.84271738923971 |
| tel_habitat_landscape_obs \| observed only \| DOY_STD #1 | estimate | -41.9314034424586 | -37.1791076400056 |
| tel_habitat_landscape_obs \| observed only \| I(DOY_STD^2) #1 | estimate | 4.39776031192017 | 3.87320743505534 |
| tel_habitat_landscape_obs \| observed only \| TIMING:METHOD #1 | estimate | -7.49844879144581 | 12.7076527509489 |
| tel_habitat_landscape \| with imputed \| (Intercept) #1 | std_error | 1.67218411783583 | 1.73193221968742 |
| tel_habitat_landscape \| with imputed \| TIMING #1 | std_error | 0.431450708623442 | 0.433371835465793 |
| tel_habitat_landscape \| with imputed \| METHOD #1 | std_error | 0.626125801600345 | 0.627981214069128 |
| tel_habitat_landscape \| with imputed \| PLANTabundant #1 | std_error | 0.308934128347728 | 0.310373430200947 |
| tel_habitat_landscape \| with imputed \| PLANTdominant #1 | std_error | 0.477778329525162 | 0.479088862922494 |
| tel_habitat_landscape \| with imputed \| EVL_species #1 | std_error | 0.515982073757793 | 0.519603839426887 |

_Showing the first 60 of 476 rows._

### revised__22_teleius_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| with imputed \| TRUE \| FALSE \| collinear terms #1 | max_gvif_adjusted | 2.7149699183165 | 2.70183818036118 |
| tel_habitat_landscape_pls \| with plausible imputed \| TRUE \| FALSE \| collinear terms #1 | max_gvif_adjusted | 2.51367210612462 | 2.65964222561465 |
| tel_habitat_landscape_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated; collinear terms #1 | max_gvif_adjusted | 2.77254793917995 | 2.80798030474577 |
| tel_habitat_landscape \| with imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_uniformity_p | 0.412084922242051 | 0.420200564736822 |
| tel_habitat_landscape_pls \| with plausible imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_uniformity_p | 0.961418590215227 | 0.961492648675825 |
| tel_habitat_landscape_obs \| observed only \| TRUE \| FALSE \| possible separation; random-effect variance inflated; collinear terms #1 | dharma_uniformity_p | 0.400377331398752 | 0.719454758819523 |
| tel_habitat_landscape \| with imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_dispersion_p | 0.344 | 0.32 |
| tel_habitat_landscape_pls \| with plausible imputed \| TRUE \| FALSE \| collinear terms #1 | dharma_dispersion_p | 0.68 | 0.656 |

### revised__22_teleius_fit_statistics.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | AIC | 591.749318954447 | 589.043325095403 |
| tel_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | AIC | 532.69072156158 | 531.685477609149 |
| tel_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | AIC | 173.568101670291 | 175.461561808223 |
| tel_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | BIC | 711.341920905344 | 708.6359270463 |
| tel_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | BIC | 638.79855858431 | 637.793314631879 |
| tel_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | BIC | 265.256497791833 | 267.149957929765 |
| tel_habitat_landscape \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with imputed \| ok #1 | logLik | -272.874659477223 | -271.521662547702 |
| tel_habitat_landscape_pls \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| with plausible imputed \| ok #1 | logLik | -243.34536078079 | -242.842738804574 |
| tel_habitat_landscape_obs \| Management + local habitat + landscape at the chosen radii \| Habitat and landscape \| observed only \| ok #1 | logLik | -63.7840508351456 | -64.7307809041116 |

### revised__22_teleius_gvif.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| with imputed \| TIMING #1 | GVIF | 1.2536306142349 | 1.25696480944172 |
| tel_habitat_landscape \| with imputed \| METHOD #1 | GVIF | 1.14661257794938 | 1.14060120903966 |
| tel_habitat_landscape \| with imputed \| PLANT #1 | GVIF | 1.25583744813243 | 1.26354996856549 |
| tel_habitat_landscape \| with imputed \| EVL_species #1 | GVIF | 1.08329014423736 | 1.09914704910506 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | GVIF | 1.4597034166081 | 1.4447864366661 |
| tel_habitat_landscape \| with imputed \| WET_10 #1 | GVIF | 1.38725889623114 | 1.38577163853547 |
| tel_habitat_landscape \| with imputed \| X_10 #1 | GVIF | 1.1558609871687 | 1.15529556959714 |
| tel_habitat_landscape \| with imputed \| GRASSLAND_1000 #1 | GVIF | 2.66617224243713 | 2.13942391364003 |
| tel_habitat_landscape \| with imputed \| NATURAL_4000 #1 | GVIF | 2.2178669055541 | 1.93088270853731 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_3000 #1 | GVIF | 2.21865581141937 | 2.06367254133974 |
| tel_habitat_landscape \| with imputed \| URBAN_4000 #1 | GVIF | 1.67398244781613 | 1.3637480395119 |
| tel_habitat_landscape \| with imputed \| WATER_4000 #1 | GVIF | 1.15641688020853 | 1.14571392470796 |
| tel_habitat_landscape \| with imputed \| ERA #1 | GVIF | 1.11276618272008 | 1.12208256936657 |
| tel_habitat_landscape \| with imputed \| DOY_STD #1 | GVIF | 1.0817798898911 | 1.08567032900882 |
| tel_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | GVIF | 1.2453758893322 | 1.25269526927345 |
| tel_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | GVIF | 1.12387346670979 | 1.12245813850421 |
| tel_habitat_landscape_pls \| with plausible imputed \| PLANT #1 | GVIF | 1.27710180654734 | 1.2894706273846 |
| tel_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | GVIF | 1.10533322500903 | 1.1293157677238 |
| tel_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_10 #1 | GVIF | 1.5311437197152 | 1.51129223938521 |
| tel_habitat_landscape_pls \| with plausible imputed \| WET_10 #1 | GVIF | 1.49632501580601 | 1.50160958475323 |
| tel_habitat_landscape_pls \| with plausible imputed \| X_10 #1 | GVIF | 1.18830093147602 | 1.18628277096103 |
| tel_habitat_landscape_pls \| with plausible imputed \| GRASSLAND_1000 #1 | GVIF | 2.61582559146224 | 2.0306503958291 |
| tel_habitat_landscape_pls \| with plausible imputed \| NATURAL_4000 #1 | GVIF | 2.01030350737708 | 1.72157919130758 |
| tel_habitat_landscape_pls \| with plausible imputed \| SANGUISORBA_3000 #1 | GVIF | 1.9370303344358 | 1.94820232436072 |
| tel_habitat_landscape_pls \| with plausible imputed \| URBAN_4000 #1 | GVIF | 1.66944210992755 | 1.40563502075803 |
| tel_habitat_landscape_pls \| with plausible imputed \| WATER_4000 #1 | GVIF | 1.14411914548003 | 1.12760516052396 |
| tel_habitat_landscape_pls \| with plausible imputed \| ERA #1 | GVIF | 1.17669639257028 | 1.19317419397585 |
| tel_habitat_landscape_pls \| with plausible imputed \| DOY_STD #1 | GVIF | 1.1450377821639 | 1.17667834424286 |
| tel_habitat_landscape_obs \| observed only \| TIMING #1 | GVIF | 1.2934668964428 | 1.30475897738312 |
| tel_habitat_landscape_obs \| observed only \| METHOD #1 | GVIF | 1.1667361820941 | 1.1639652994924 |
| tel_habitat_landscape_obs \| observed only \| PLANT #1 | GVIF | 1.30918735712628 | 1.32744672727764 |
| tel_habitat_landscape_obs \| observed only \| EVL_species #1 | GVIF | 1.13023564855111 | 1.12570503538402 |
| tel_habitat_landscape_obs \| observed only \| SANGUISORBA_10 #1 | GVIF | 1.59683435772746 | 1.6174002426528 |
| tel_habitat_landscape_obs \| observed only \| WET_10 #1 | GVIF | 1.58424488570111 | 1.59603171498108 |
| tel_habitat_landscape_obs \| observed only \| X_10 #1 | GVIF | 1.18872086099989 | 1.20483603287614 |
| tel_habitat_landscape_obs \| observed only \| GRASSLAND_1000 #1 | GVIF | 3.16201478397899 | 2.41460946119604 |
| tel_habitat_landscape_obs \| observed only \| NATURAL_4000 #1 | GVIF | 2.01822208822315 | 1.69136362778657 |
| tel_habitat_landscape_obs \| observed only \| SANGUISORBA_3000 #1 | GVIF | 1.74140469150736 | 1.74540374973995 |
| tel_habitat_landscape_obs \| observed only \| URBAN_4000 #1 | GVIF | 1.69084771645659 | 1.57901205566877 |
| tel_habitat_landscape_obs \| observed only \| WATER_4000 #1 | GVIF | 1.23778313631133 | 1.17981776713392 |
| tel_habitat_landscape_obs \| observed only \| ERA #1 | GVIF | 1.26802050806699 | 1.29056666985848 |
| tel_habitat_landscape_obs \| observed only \| DOY_STD #1 | GVIF | 1.10819234513531 | 1.11367947566053 |
| tel_habitat_landscape \| with imputed \| TIMING #1 | GVIF_adjusted | 1.11965647152816 | 1.12114441952931 |
| tel_habitat_landscape \| with imputed \| METHOD #1 | GVIF_adjusted | 1.07079997102604 | 1.06798933002145 |
| tel_habitat_landscape \| with imputed \| PLANT #1 | GVIF_adjusted | 1.05860357744596 | 1.0602251579207 |
| tel_habitat_landscape \| with imputed \| EVL_species #1 | GVIF_adjusted | 1.04081225215567 | 1.04840214092926 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_10 #1 | GVIF_adjusted | 1.20818186404535 | 1.20199269409847 |
| tel_habitat_landscape \| with imputed \| WET_10 #1 | GVIF_adjusted | 1.17781955164242 | 1.17718802174311 |
| tel_habitat_landscape \| with imputed \| X_10 #1 | GVIF_adjusted | 1.07510975587086 | 1.07484676563552 |
| tel_habitat_landscape \| with imputed \| GRASSLAND_1000 #1 | GVIF_adjusted | 1.63284176895287 | 1.4626769683153 |
| tel_habitat_landscape \| with imputed \| NATURAL_4000 #1 | GVIF_adjusted | 1.4892504509162 | 1.38956205638227 |
| tel_habitat_landscape \| with imputed \| SANGUISORBA_3000 #1 | GVIF_adjusted | 1.48951529412066 | 1.43654883012717 |
| tel_habitat_landscape \| with imputed \| URBAN_4000 #1 | GVIF_adjusted | 1.29382473612778 | 1.16779623201648 |
| tel_habitat_landscape \| with imputed \| WATER_4000 #1 | GVIF_adjusted | 1.07536825330141 | 1.07038027107564 |
| tel_habitat_landscape \| with imputed \| ERA #1 | GVIF_adjusted | 1.05487733065039 | 1.059283989007 |
| tel_habitat_landscape \| with imputed \| DOY_STD #1 | GVIF_adjusted | 1.04008648192884 | 1.04195505133802 |
| tel_habitat_landscape_pls \| with plausible imputed \| TIMING #1 | GVIF_adjusted | 1.11596410754656 | 1.11923870075755 |
| tel_habitat_landscape_pls \| with plausible imputed \| METHOD #1 | GVIF_adjusted | 1.06012898588322 | 1.05946124917536 |
| tel_habitat_landscape_pls \| with plausible imputed \| PLANT #1 | GVIF_adjusted | 1.06305657958765 | 1.06562122817977 |
| tel_habitat_landscape_pls \| with plausible imputed \| EVL_species #1 | GVIF_adjusted | 1.05134828910739 | 1.06269269674907 |

_Showing the first 60 of 84 rows._

### revised__22_teleius_habitat_landscape_estimates.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| with imputed \| SANGUISORBA_10 \| 0.99 (0.92-1.07) #1 | p_value | 0.85942457038698 | 0.819789942463961 |
| tel_habitat_landscape \| with imputed \| WET_10 \| 0.99 (0.91-1.07) #1 | p_value | 0.731427902708878 | 0.733706869254651 |
| tel_habitat_landscape \| with imputed \| X_10 \| 0.98 (0.89-1.08) #1 | p_value | 0.710843351315869 | 0.694313268781435 |
| tel_habitat_landscape_pls \| with plausible imputed \| WET_10 \| 0.99 (0.92-1.08) #1 | p_value | 0.881484833033884 | 0.885920183646956 |
| tel_habitat_landscape_pls \| with plausible imputed \| X_10 \| 0.97 (0.88-1.06) #1 | p_value | 0.490256283112542 | 0.474233099372171 |
| tel_habitat_landscape_obs \| observed only \| URBAN_4000 \| 0.00 (0.00-0.00) #1 | p_value | 5.92769319035701e-06 | 0.00152039161194381 |

### revised__22_teleius_random_effects.csv

| row | column | before | after |
|---|---|---|---|
| tel_habitat_landscape \| with imputed \| SITMAP #1 | variance | 0.873246356505431 | 0.919915536923676 |
| tel_habitat_landscape \| with imputed \| YEAR #1 | variance | 0.0683991109052001 | 0.074426307907688 |
| tel_habitat_landscape \| with imputed \| OBSERVER #1 | variance | 0.712924445782291 | 0.694994587057944 |
| tel_habitat_landscape_pls \| with plausible imputed \| SITMAP #1 | variance | 0.186010094498548 | 0.207094648791567 |
| tel_habitat_landscape_pls \| with plausible imputed \| YEAR #1 | variance | 0.114741650030881 | 0.128624938930349 |
| tel_habitat_landscape_pls \| with plausible imputed \| OBSERVER #1 | variance | 0.704055134913852 | 0.705378699056855 |
| tel_habitat_landscape_obs \| observed only \| SITMAP #1 | variance | 61638.4112745513 | 28645.5533618812 |
| tel_habitat_landscape_obs \| observed only \| YEAR #1 | variance | 63982.5429902388 | 28826.3015564453 |
| tel_habitat_landscape_obs \| observed only \| OBSERVER #1 | variance | 16509.2699433857 | 5982.8988585099 |
| tel_habitat_landscape \| with imputed \| SITMAP #1 | sd | 0.934476514689069 | 0.959122274229765 |
| tel_habitat_landscape \| with imputed \| YEAR #1 | sd | 0.26153223683745 | 0.272811854411952 |
| tel_habitat_landscape \| with imputed \| OBSERVER #1 | sd | 0.844348533357103 | 0.833663353553426 |
| tel_habitat_landscape_pls \| with plausible imputed \| SITMAP #1 | sd | 0.4312888759272 | 0.455076530697384 |
| tel_habitat_landscape_pls \| with plausible imputed \| YEAR #1 | sd | 0.338735368733294 | 0.358643191668751 |
| tel_habitat_landscape_pls \| with plausible imputed \| OBSERVER #1 | sd | 0.839079933566435 | 0.839868262917974 |
| tel_habitat_landscape_obs \| observed only \| SITMAP #1 | sd | 248.270842578325 | 169.249973004078 |
| tel_habitat_landscape_obs \| observed only \| YEAR #1 | sd | 252.94770801539 | 169.783101504376 |
| tel_habitat_landscape_obs \| observed only \| OBSERVER #1 | sd | 128.488403925746 | 77.3492007619336 |

