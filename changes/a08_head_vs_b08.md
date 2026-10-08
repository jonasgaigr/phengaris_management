# a08_head_vs_b08

Snapshot `15_diag` compared with `16_head`.

86 files compared: 79 identical, 0 numerically identical (differences below 1e-9 relative), 4 changed, 3 added, 0 removed.

## Files that differ

| file | kind | status | rows_before | rows_after | rows_removed | rows_added | cols_removed | cols_added | cells_changed |
|---|---|---|---|---|---|---|---|---|---|
| 04_cleaning_summary.csv | tables | changed | 3 | 3 | 1 | 1 |  |  | 0 |
| 09_nausithous_diagnostics.csv | tables | changed | 35 | 35 | 1 | 1 |  |  | 45 |
| 10_teleius_diagnostics.csv | tables | changed | 38 | 38 | 3 | 3 |  |  | 51 |
| 11_both_species_diagnostics.csv | tables | changed | 5 | 5 | 0 | 0 |  |  | 2 |
| 02_imputation_rules_compared.csv | tables | added |  |  |  |  |  |  |  |
| 02_outside_range.csv | tables | added |  |  |  |  |  |  |  |
| 04_host_plant_filter.csv | tables | added |  |  |  |  |  |  |  |

## Changed numbers

The full list of 98 changed cells is in `a08_head_vs_b08.csv`.

### 09_nausithous_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| nau_null \| FALSE \| <NA> #1 | max_abs_estimate | 0.275562394124457 |  |
| nau_spatial \| <NA> \| <NA> #1 | max_abs_estimate | 2.56352644980756 | 3.65255336729126e-12 |
| nau_spatiotemporal \| <NA> \| <NA> #1 | max_abs_estimate | 3.36371162212884 | 1.66750967743873 |
| nau_area_null \| FALSE \| <NA> #1 | max_abs_estimate | 0.174698746176986 |  |
| nau_area \| FALSE \| <NA> #1 | max_abs_estimate | 0.0630535126494014 | 0.0463172706604357 |
| nau_area_null_glm \| <NA> \| <NA> #1 | max_abs_estimate | 0.162104764945165 |  |
| nau_plant \| FALSE \| <NA> #1 | max_abs_estimate | 0.756737657305242 | 0.635314859359412 |
| nau_resource_density \| FALSE \| <NA> #1 | max_abs_estimate | 1.14008135106023 | 0.859243863048338 |
| nau_mow_null \| FALSE \| <NA> #1 | max_abs_estimate | 0.224711708782824 |  |
| nau_method \| FALSE \| <NA> #1 | max_abs_estimate | 0.374748063540757 | 0.314238949202192 |
| nau_mow_null_glm \| <NA> \| <NA> #1 | max_abs_estimate | 0.139324879387728 |  |
| nau_graze_null \| FALSE \| <NA> #1 | max_abs_estimate | 0.193684616901953 |  |
| nau_graze \| FALSE \| <NA> #1 | max_abs_estimate | 0.39989469246643 | 0.108859764129992 |
| nau_graze_method \| FALSE \| <NA> #1 | max_abs_estimate | 0.454773235110146 | 0.305282548770004 |
| nau_management_het \| FALSE \| fitting warnings #1 | max_abs_estimate | 0.853738200751648 | 0.577697717271372 |
| nau_protect \| FALSE \| <NA> #1 | max_abs_estimate | 0.343786156029965 | 0.343660630960797 |
| nau_evl_combined \| FALSE \| <NA> #1 | max_abs_estimate | 0.336318963551965 | 0.300577447940387 |
| nau_evl_year \| FALSE \| possible separation; fitting warnings #1 | max_abs_estimate | 337.800576056687 | 110.772072575251 |
| nau_mzchu \| FALSE \| <NA> #1 | max_abs_estimate | 0.278394605352106 | 0.0437607407819298 |
| nau_ttp \| FALSE \| <NA> #1 | max_abs_estimate | 0.332113633681507 | 0.0831047512561974 |
| nau_het_inner \| FALSE \| <NA> #1 | max_abs_estimate | 0.148038060778954 | 0.100667795367081 |
| nau_null \| FALSE \| <NA> #1 | max_std_error | 0.213257335197062 |  |
| nau_spatial \| <NA> \| <NA> #1 | max_std_error | 0.226736557900849 | 3.00588743238621e-13 |
| nau_spatiotemporal \| <NA> \| <NA> #1 | max_std_error | 0.255679218275265 | 0.152988048484291 |
| nau_area_null \| FALSE \| <NA> #1 | max_std_error | 0.755012546606809 |  |
| nau_area \| FALSE \| <NA> #1 | max_std_error | 0.765158452281262 | 0.0442888730079379 |
| nau_area_null_glm \| <NA> \| <NA> #1 | max_std_error | 0.0357348552525538 |  |
| nau_plant \| FALSE \| <NA> #1 | max_std_error | 0.742682781270476 | 0.0910842503087665 |
| nau_resource_density \| FALSE \| <NA> #1 | max_std_error | 0.800001546114581 | 0.209738125705278 |
| nau_mow_null \| FALSE \| <NA> #1 | max_std_error | 0.209635560761508 |  |
| nau_timing \| FALSE \| <NA> #1 | max_std_error | 0.226915986442887 | 0.120892062119444 |
| nau_method \| FALSE \| <NA> #1 | max_std_error | 0.217425794811755 | 0.104487761158501 |
| nau_mow_null_glm \| <NA> \| <NA> #1 | max_std_error | 0.0484682556842184 |  |
| nau_graze_null \| FALSE \| <NA> #1 | max_std_error | 0.269499647901525 |  |
| nau_graze \| FALSE \| <NA> #1 | max_std_error | 0.297834705380278 | 0.250847910112008 |
| nau_protect \| FALSE \| <NA> #1 | max_std_error | 0.215426187911506 | 0.0945106086498115 |
| nau_evl \| FALSE \| <NA> #1 | max_std_error | 0.214922712765929 | 0.0981685477499208 |
| nau_evl_target \| FALSE \| <NA> #1 | max_std_error | 0.214441274069856 | 0.141194664846934 |
| nau_evl_combined \| FALSE \| <NA> #1 | max_std_error | 0.215047946008476 | 0.179410014450086 |
| nau_mzchu \| FALSE \| <NA> #1 | max_std_error | 0.213522519622691 | 0.158240613750038 |
| nau_ttp \| FALSE \| <NA> #1 | max_std_error | 0.219352114255268 | 0.0783184174188149 |
| nau_management_ttp \| FALSE \| possible separation #1 | max_std_error | 19.8999119232654 | 19.8997583312024 |
| nau_het_inner \| FALSE \| <NA> #1 | max_std_error | 0.228621815766442 | 0.0656365360048597 |
| nau_het_outer \| FALSE \| <NA> #1 | max_std_error | 0.235443507786529 | 0.0914674907491225 |
| nau_threats \| FALSE \| <NA> #1 | max_std_error | 0.542626818464522 | 0.0362345941360072 |

### 10_teleius_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| tel_null \| FALSE \| <NA> #1 | max_abs_estimate | 1.80623110449908 |  |
| tel_null_glm \| <NA> \| <NA> #1 | max_abs_estimate | 1.71379791317856 |  |
| tel_year_factor \| <NA> \| <NA> #1 | max_abs_estimate | 1.45278410238872 | 1.03212254739854 |
| tel_spatial \| <NA> \| <NA> #1 | max_abs_estimate | 2.29932189771463 | 7.73940428888627e-13 |
| tel_random_only \| FALSE \| <NA> #1 | max_abs_estimate | 1.80623110449908 |  |
| tel_area_null \| FALSE \| <NA> #1 | max_abs_estimate | 1.90717726205009 |  |
| tel_area \| FALSE \| <NA> #1 | max_abs_estimate | 1.81114019646598 | 0.0397719343420831 |
| tel_area_null_glm \| <NA> \| <NA> #1 | max_abs_estimate | 1.79009141212727 |  |
| tel_plant_area_subset \| FALSE \| <NA> #1 | max_abs_estimate | 2.83008403853857 | 0.565665116902206 |
| tel_plant_poly \| TRUE \| singular fit; possible separation; random-effect variance inflated #1 | max_abs_estimate | 11.8135308142686 | 11.5895443004865 |
| tel_mow_null \| FALSE \| <NA> #1 | max_abs_estimate | 1.74577446328919 |  |
| tel_timing_mow_subset \| FALSE \| <NA> #1 | max_abs_estimate | 2.41864336727332 | 0.866985411678649 |
| tel_method_mow_subset \| FALSE \| fitting warnings #1 | max_abs_estimate | 1.95613060811961 | 0.415904062645367 |
| tel_method_timing_mow_subset \| FALSE \| <NA> #1 | max_abs_estimate | 2.45034597708771 | 0.763835952542069 |
| tel_mow_null_glm \| <NA> \| <NA> #1 | max_abs_estimate | 1.63934339521241 |  |
| tel_graze_null \| TRUE \| singular fit #1 | max_abs_estimate | 0.117783038318028 |  |
| tel_graze \| FALSE \| <NA> #1 | max_abs_estimate | 1.9713744717328 | 1.73367878150945 |
| tel_method \| FALSE \| <NA> #1 | max_abs_estimate | 1.97162744369761 | 0.452886039646552 |
| tel_timing \| FALSE \| <NA> #1 | max_abs_estimate | 2.35525294706557 | 0.930801665667432 |
| tel_method_timing \| FALSE \| <NA> #1 | max_abs_estimate | 2.45034597708771 | 0.763835952542069 |
| tel_protect \| FALSE \| <NA> #1 | max_abs_estimate | 2.2853331990241 | 1.55874490785542 |
| tel_evl \| TRUE \| singular fit #1 | max_abs_estimate | 2.21916136917272 | 1.48474387399748 |
| tel_evl_target \| FALSE \| <NA> #1 | max_abs_estimate | 2.01559284492992 | 1.44105668740673 |
| tel_evl_combined \| TRUE \| singular fit #1 | max_abs_estimate | 2.22879108136959 | 1.34741140396007 |
| tel_protection_combined \| TRUE \| singular fit #1 | max_abs_estimate | 2.23999874429619 | 1.27644119417947 |
| tel_mzchu \| FALSE \| <NA> #1 | max_abs_estimate | 1.9173602217795 | 1.1192260862098 |
| tel_fsb_subset \| FALSE \| <NA> #1 | max_abs_estimate | 1.68573410194752 | 0.362963525197079 |
| tel_het_inner \| FALSE \| <NA> #1 | max_abs_estimate | 1.48425386747793 | 0.27168039524411 |
| tel_het_inner_dup_year \| FALSE \| fitting warnings #1 | max_abs_estimate | 1.48425575994805 | 0.271675805142942 |
| tel_het_outer \| FALSE \| <NA> #1 | max_abs_estimate | 1.76497880443817 | 0.0385933529781083 |
| tel_null \| FALSE \| <NA> #1 | max_std_error | 0.191733817119068 |  |
| tel_null_glm \| <NA> \| <NA> #1 | max_std_error | 0.0768118553086004 |  |
| tel_spatial \| <NA> \| <NA> #1 | max_std_error | 0.472280885760483 | 6.12500460498892e-13 |
| tel_random_only \| FALSE \| <NA> #1 | max_std_error | 0.191733817119068 |  |
| tel_area_null \| FALSE \| <NA> #1 | max_std_error | 0.283291830490611 |  |
| tel_area \| FALSE \| <NA> #1 | max_std_error | 0.354251196158654 | 0.0905178815160925 |
| tel_area_null_glm \| <NA> \| <NA> #1 | max_std_error | 0.10802521013641 |  |
| tel_plant_area_subset \| FALSE \| <NA> #1 | max_std_error | 0.483754439178506 | 0.188423420098065 |
| tel_mow_null \| FALSE \| <NA> #1 | max_std_error | 0.263933369516207 |  |
| tel_timing_mow_subset \| FALSE \| <NA> #1 | max_std_error | 0.374659131499684 | 0.291561609180355 |
| tel_method_mow_subset \| FALSE \| fitting warnings #1 | max_std_error | 0.00131136346680775 | 0.00131099526422224 |
| tel_mow_null_glm \| <NA> \| <NA> #1 | max_std_error | 0.103255335895111 |  |
| tel_graze_null \| TRUE \| singular fit #1 | max_std_error | 0.485912658163913 |  |
| tel_method \| FALSE \| <NA> #1 | max_std_error | 0.295540026989501 | 0.220462808262622 |
| tel_timing \| FALSE \| <NA> #1 | max_std_error | 0.340736216168913 | 0.270599838108934 |
| tel_protect \| FALSE \| <NA> #1 | max_std_error | 0.203597690667463 | 0.167982334850313 |
| tel_evl \| TRUE \| singular fit #1 | max_std_error | 0.193659255080364 | 0.168975129987498 |
| tel_fsb_subset \| FALSE \| <NA> #1 | max_std_error | 0.372367450704636 | 0.341525162141515 |
| tel_het_inner \| FALSE \| <NA> #1 | max_std_error | 0.257895583222455 | 0.147625023620455 |
| tel_het_inner_dup_year \| FALSE \| fitting warnings #1 | max_std_error | 0.2579120158508 | 0.14763092776439 |
| tel_het_outer \| FALSE \| <NA> #1 | max_std_error | 0.34393412992445 | 0.268091261511387 |

### 11_both_species_diagnostics.csv

| row | column | before | after |
|---|---|---|---|
| both_species \| FALSE \| <NA> #1 | max_std_error | 0.208326192885167 | 0.0993309222105141 |
| both_recorded_habitat \| FALSE \| <NA> #1 | max_std_error | 0.232389636740796 | 0.170383549070967 |

