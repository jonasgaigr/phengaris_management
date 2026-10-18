# figures_style

Snapshot `38_figures_all` compared with `39_figures_style_all`.

357 files compared: 357 identical, 0 numerically identical (differences below 1e-9 relative), 0 changed, 0 added, 0 removed.


## What changed

Presentation only: colours, themes, type sizes, labels, legends and export
sizes. No model, number, file stem or data passed to a plot changed (the
comparison above). Both specifications share the new style.

- `R/functions/plot_style.R` (new, sourced by `R/00_setup.R`): the palette
  `PHENGARIS_COLS` and its tints, `theme_phengaris()` and
  `theme_phengaris_map()`, the species, occupancy and ramp scales, and the
  print sizes. The occupancy scales moved here from `R/00_setup.R`, with
  the same names and arguments.
- `write_figure_png()` and `report_figure()` take a column preset
  (`size = "double"`, 17.5 cm, or `"single"`, 8.5 cm) and a height in cm
  instead of a width and height in inches. The type is 9 pt for titles and
  8 pt for ticks, legends and annotations at that size. `write_figure_pdf()`
  writes the PDF twins of step 13 at the size of their PNG, through the
  same helper.
- `report_base_figure()` is gone: its one caller, the screeplot of step 13,
  is now a ggplot.

## Palette

The colours were sampled from the photographs of the brief
(lepidoptera.cz, *Maculinea teleius*): the per-channel median of a box on
the wing, with every box checked on an enlarged crop so that it holds wing,
not vegetation or background.

The brief labels 1616, 1133 and 77 as the male upperside; they show the
underside of live butterflies. The male upperside is on the specimen plate
820 (top half) and the live male 76; 1132 is a live female upperside and
821 the female plate.

| name | hex | source | L\* | C\* | h | on white |
|---|---|---|---|---|---|---|
| teleius_blue | `#8387B5` | hue and lightness sampled, chroma provisional (see below) | 57.5 | 26.2 | 290 | 3.44:1 |
| deep_blue | `#4F5789` | hue and lightness sampled, chroma provisional (see below) | 38.3 | 30.7 | 290 | 6.87:1 |
| border_brown | `#644938` | sampled: forewing and hindwing margins of 820, 76 and 1132 (5 boxes) | 33.5 | 17.2 | 58 | 8.23:1 |
| underside_buff | `#B89B7F` | sampled: hindwing underside of 1616 (2 boxes), 1133 and 77 | 65.9 | 19.8 | 71 | 2.61:1 |
| spot_black | `#0A0301` | sampled: post-discal spots of 1616 (4) and 1133 (1) | 1.2 | 2.1 | 42 | 20.46:1 |
| fringe_white | `#F3F1EA` | sampled hue from the fringe of 1133 (`#D7D5CF`), lightness raised | 95.1 | 3.7 | 99 | 1.13:1 |
| nausithous_brown | `#8A5A3B` | provisional: *P. nausithous* is not in the photographs | 42.8 | 30.6 | 58 | 5.82:1 |

Tints, the same hue and chroma at another L\*:

| name | hex | L\* | use | on white |
|---|---|---|---|---|
| teleius_pale | `#BDBEE0` | 78 | box fills (step 21) | 1.81:1 |
| nausithous_light | `#B5876B` | 60 | *P. nausithous*, negative (step 13) | 3.16:1 |
| buff_dark | `#A2876B` | 58 | negative records as points | 3.38:1 |
| buff_light | `#DCC9B7` | 82 | plausible imputed absences | 1.61:1 |

**The blues.** The male upperside photographs as a near-grey silver: the
samples have a consistent hue (290-307 degrees, a lilac blue) and
lightness (upperside L\* 55-70, pooled 57.6; wing base L\* 33-38, pooled
38.4), but a chroma of only 3-5. A literal sample would be a grey, useless
next to the neutral greys of the figures. The blues keep the sampled hue
and lightness and take the chroma of the provisional `#8FA6D4` and
`#4A6496`.

**Adjustments.**

- teleius_blue: the provisional `#8FA6D4` (L\* 68) fails 3:1 on white
  (2.45:1). At the sampled L\* 57.6, hue kept, it reaches 3.44:1.
- fringe_white: the fringe is 1-3 px wide and shaded, so the cleanest
  sample (`#D7D5CF`, L\* 85) was raised to L\* 95, hue kept, for the light
  end of the ramp.
- underside_buff passes as a fill (bars next to deep blue) but not as a
  line or point (2.61:1). Points and lines take buff_dark (L\* 58, 3.38:1).
- The specimen undersides of 820 and 821 were left out: the scan is
  yellowed against the live photographs.

## Where each colour goes

| figures | encoding |
|---|---|
| species | *P. nausithous* nausithous_brown, *P. teleius* teleius_blue |
| site occupancy (`scale_fill_occupancy()`, `scale_colour_occupancy()`) | negative underside_buff (points: buff_dark), positive deep_blue; points also open vs filled |
| the three samples (steps 08, 17, 19, 23-25) | spot_black, deep_blue, teleius_blue, with circle, triangle and square, and solid, long-dash and dotted lines |
| step 13, the four groups | *P. teleius* deep_blue (positive) and teleius_blue (negative), circles; *P. nausithous* nausithous_brown and nausithous_light, triangles; positive filled, negative open |
| record outcomes (step 08) | deep_blue, underside_buff, buff_light, fringe_white, with a buff outline |
| single series: bars, histograms, lines | deep_blue |
| ordered fills (host plant, step 12) | the ramp fringe_white - teleius_blue - deep_blue |
| ordered lines (elevations, step 16; area exponent b, step 17) | the ramp spot_black - deep_blue - teleius_blue |
| distribution map (step 14) | surveyed only grey, *P. nausithous* only nausithous_brown, *P. teleius* only teleius_blue, both spot_black |
| reference lines, gridlines, hillshade | neutral greys (grey50, grey91; the hillshade grey62 to white) |
| text, arrows (step 13) | spot_black; axis lines and ticks border_brown |

## Colour-vision check

CIELAB delta E (CIE76) of each pair of colours drawn together, under
normal vision, simulated deuteranopia, protanopia and tritanopia
(`colorspace::deutan()`, `protan()`, `tritan()`, severity 1) and in
greyscale (`colorspace::desaturate()`):

| pair | normal | deutan | protan | tritan | grey |
|---|---|---|---|---|---|
| species: nausithous vs teleius | 53.1 | 53.8 | 51.8 | 43.1 | 14.7 |
| occupancy fill: negative vs positive | 55.1 | 57.8 | 52.4 | 41.6 | 27.6 |
| occupancy points: negative vs positive | 51.9 | 53.8 | 49.5 | 36.4 | 20.0 |
| samples: with imputed vs plausible | 48.7 | 48.2 | 48.7 | 41.1 | 37.1 |
| samples: plausible vs observed | 19.7 | 20.2 | 19.1 | 19.5 | 19.2 |
| samples: with imputed vs observed | 62.5 | 61.9 | 63.0 | 58.0 | 56.4 |
| ordination: teleius + vs - | 19.7 | 20.2 | 19.1 | 19.5 | 19.2 |
| ordination: nausithous + vs - | 17.9 | 17.6 | 18.0 | 17.6 | 17.4 |
| ordination: positives | 55.2 | 57.6 | 52.3 | 44.8 | 4.5 |
| ordination: negatives | 46.8 | 48.9 | 44.5 | 36.7 | 2.7 |
| ordination: teleius + vs nausithous - | 55.3 | 58.7 | 51.5 | 46.0 | 21.9 |
| outcomes: observed vs plausible imputed | 18.0 | 17.6 | 18.2 | 17.6 | 16.2 |
| outcomes: plausible vs not plausible | 15.8 | 15.2 | 15.6 | 16.3 | 13.1 |
| map: surveyed vs nausithous | 53.5 | 50.4 | 52.1 | 54.0 | 43.9 |
| map: surveyed vs teleius | 39.2 | 38.6 | 37.7 | 30.4 | 29.2 |
| map: both vs nausithous | 50.5 | 50.1 | 45.2 | 50.4 | 41.7 |
| map: both vs teleius | 62.5 | 61.9 | 63.0 | 58.0 | 56.4 |
| host plant: single vs abundant | 32.6 | 32.8 | 31.6 | 27.3 | 26.3 |
| host plant: abundant vs dominant | 15.7 | 15.8 | 15.1 | 15.6 | 15.3 |

Every pair stays at least 15 apart under each simulated deficiency; blue
against brown holds because the deficiencies leave the blue-yellow axis
intact. The pairs of one hue (the samples, positive and negative of one
species, the imputed outcomes) rely on lightness alone (15-20) and so
always carry a second cue: a shape, a line type, an open mark or a stacked
position. Deep blue and the nausithous brown have the same lightness and
merge in greyscale (4.5); step 13 therefore draws the species as circles
and triangles as well.

Text and lines on white: spot_black 20.5:1, deep_blue 6.9:1,
nausithous_brown 5.8:1, buff_dark 3.4:1, teleius_blue 3.4:1,
nausithous_light 3.2:1. The lighter colours (underside_buff, buff_light,
teleius_pale, fringe_white) are fills only.

## Other presentation fixes

- Step 13: the four-group colours were never applied. The names of
  `tap_cols` (`"nausithousTRUE"`) did not match the levels of
  `interaction(DRUH, POSITIVE)` (`"nausithous.1"`), so every point was
  drawn in the default grey. The groups now take their colours and shapes.
  Threat labels are short readable names instead of column names; the
  arrows are spot_black; labels are repelled from each other with a fixed
  seed. The screeplot is a ggplot of the same eigenvalues and broken-stick
  values that `vegan::screeplot()` drew.
- Step 14: the distribution map overlaid two half-transparent layers
  (blue, red), so fields with both species came out magenta, which the
  caption did not name. Each surveyed field now has one class: surveyed
  only, one species only, or both; the caption names all four. The site
  map colours the records by occupancy (it used the default ggplot hues on
  `NEGATIV`, with no legend) and draws the positive records on top.
- Step 14, both maps: one frame, so that they stand side by side. The
  relief is drawn as soft shadows and highlights over a near-white base
  instead of a flat grey; the site map adds a pale elevation tint
  (`ELEVATION_TINTS`, from the reference layer `elevation.tif`, aggregated
  to the 300 m of the hillshade) with its own legend. Each class of records
  has a white halo, so that the positive records stand apart from the
  negative ones under them. The legends sit in the empty corner north of
  Moravia and give the number of records, or of mapping fields, in each
  class (as in the table `14_grid_coverage`). A 100 km scale bar and a
  north arrow sit in the corner south-west of Bohemia; the arrow points to
  true north, about 7 degrees off the S-JTSK grid, computed by
  `true_north_angle()`. The site map's caption adds "tinted by elevation".
- Steps 06, 08 and 12: titles that repeated the caption are gone; ranked
  bars are horizontal and ordered by value; species names are in italics
  in axis titles, tick labels and strips. The grouped bars of ordinal or
  fixed classes (host plant quantity, management, protection) keep their
  own order.
- Steps 16-25: the forest plots already had a reference line at an odds
  ratio of 1 and a log axis; narrow panels (step 25) now show fewer log
  ticks, so the labels no longer collide.
- Axis and legend titles are in sentence case, with units where there are
  any.

## Not done

- `ggspatial` is not in `renv.lock`; the scale bar and north arrow of the
  maps are drawn with plain ggplot2 layers (`map_scale_bar()`,
  `map_north_arrow()` in `R/functions/plot_style.R`) instead.
- Mixed italic and roman text in legends is drawn with plotmath
  (`italic()`); `ggtext` is not in `renv.lock` and was not needed.

## Figures touched

**common** (2 files):

- `16_flight_curve.png`
- `21_landscape_by_radius.png`

**legacy** (37 files):

- `06_mapped_habitat_area_nausithous.png`
- `06_mapped_habitat_area_teleius.png`
- `06_mapped_habitats_nausithous.png`
- `06_mapped_habitats_teleius.png`
- `08_host_plant_nausithous.png`
- `08_host_plant_teleius.png`
- `08_management_appropriateness_nausithous.png`
- `08_management_appropriateness_teleius.png`
- `08_management_types_nausithous.png`
- `08_management_types_teleius.png`
- `08_observer_distribution.png`
- `08_protection_evl_combined_nausithous.png`
- `08_protection_evl_combined_teleius.png`
- `08_protection_evl_nausithous.png`
- `08_protection_evl_target_nausithous.png`
- `08_protection_evl_target_teleius.png`
- `08_protection_evl_teleius.png`
- `08_protection_mzchu_nausithous.png`
- `08_protection_mzchu_teleius.png`
- `08_recorded_habitats_nausithous.png`
- `08_recorded_habitats_teleius.png`
- `08_records_by_year.png`
- `08_site_occupancy_by_species.png`
- `08_species_occurrence_context.png`
- `12_area_by_plant_nausithous.png`
- `12_area_by_plant_teleius.png`
- `12_specimen_count_distribution.png`
- `12_specimen_counts_by_species.png`
- `13_pca_screeplot.png`
- `13_pca_tap.pdf`
- `13_pca_tap.png`
- `13_pca_threats_biplot.png`
- `13_rda_tap.pdf`
- `13_rda_tap.png`
- `13_rda_threats_triplot.png`
- `14_monitored_sites.png`
- `14_species_distribution.png`

**revised** (25 files):

- `08_observer_distribution.png`
- `08_records_by_outcome.png`
- `08_share_positive.png`
- `08_visit_timing.png`
- `14_monitored_sites.png`
- `14_species_distribution.png`
- `17_nausithous_connectivity_profile.png`
- `17_nausithous_predictions.png`
- `17_nausithous_season.png`
- `17_teleius_connectivity_profile.png`
- `17_teleius_predictions.png`
- `17_teleius_season.png`
- `19_threat_estimates.png`
- `22_nausithous_landscape_scales.png`
- `22_teleius_landscape_scales.png`
- `23_nausithous_detection_probability.png`
- `23_nausithous_known_detection.png`
- `23_teleius_detection_probability.png`
- `23_teleius_known_detection.png`
- `24_nausithous_designation.png`
- `24_nausithous_persistence.png`
- `24_teleius_designation.png`
- `24_teleius_persistence.png`
- `25_nausithous_robustness.png`
- `25_teleius_robustness.png`
