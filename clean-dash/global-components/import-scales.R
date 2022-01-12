# This code is used to create the colour and fill scales
# specific to the application

#### IN UPDATED GGPLOT2, THERE IS A KNOWN ISSUE WITH UNUSED FACTOR LEVELS
#### APPEARING IN THE LEGEND. WORKING ON A FIX...

# Species ----
# This uses a standardized set of species and colours/fills

species_hex <- read.csv("data/species_hex.csv") %>%
  select(species, hex) %>%
  unique()

fillScaleSpecies <- scale_fill_manual(name = "",
                                      values = setNames(species_hex %>% pull(hex),
                                                        species_hex %>% pull(species)),
                                      limits = force)

colScaleSpecies <- scale_color_manual(name = "",
                                      values = setNames(species_hex %>% pull(hex),
                                                        species_hex %>% pull(species)),
                                      limits = force)

# EM Subject ----
myFillsSubject <- setNames(
  c(
    "cadetblue1",
    "cadetblue2",
    "cadetblue3",
    "cadetblue4",
    "darkslategrey",
    "indianred1",
    "indianred2",
    "indianred3",
    "indianred4",
    "olivedrab2",
    "olivedrab3",
    "olivedrab4",
    "darkorange2",
    "goldenrod2",
    "khaki2",
    "plum3",
    "grey80"
  ),
  c(
    "Camera Blocking",
    "Camera Dirty",
    "Discarding Out Of Camera View",
    "Improper Fish Handling",
    "Lump Discarding",
    "Other System Issues",
    "Camera Failure",
    "Measuring Surface Visibility",
    "Video Gaps",
    "Other Gear Issues",
    "Gear Conflict",
    "Mechanical Failure",
    "Unusable",
    "Low",
    "Medium",
    "Slipped Or Tripped Bag",
    "Other"
  )
)

fillScaleSubject <- scale_fill_manual(name = "",
                                      values =
                                        setNames(
                                          c(
                                            "cadetblue1",
                                            "cadetblue2",
                                            "cadetblue3",
                                            "cadetblue4",
                                            "darkslategrey",
                                            "indianred1",
                                            "indianred2",
                                            "indianred3",
                                            "indianred4",
                                            "olivedrab2",
                                            "olivedrab3",
                                            "olivedrab4",
                                            "darkorange2",
                                            "goldenrod2",
                                            "khaki2",
                                            "plum3",
                                            "grey80"
                                          ),
                                          c(
                                            "Camera Blocking",
                                            "Camera Dirty",
                                            "Discarding Out Of Camera View",
                                            "Improper Fish Handling",
                                            "Lump Discarding",
                                            "Other System Issues",
                                            "Camera Failure",
                                            "Measuring Surface Visibility",
                                            "Video Gaps",
                                            "Other Gear Issues",
                                            "Gear Conflict",
                                            "Mechanical Failure",
                                            "Unusable",
                                            "Low",
                                            "Medium",
                                            "Slipped Or Tripped Bag",
                                            "Other"
                                          )
                                        ),
                                      limits = force)

# EM Category ----
fillScaleCategory <- scale_fill_manual(name = "",
                                       values = setNames(
                                         c(
                                           "cadetblue3",
                                           "indianred3",
                                           "olivedrab3",
                                           "goldenrod2",
                                           "plum3"
                                         ),
                                         c(
                                           "Crew Event",
                                           "Electronic Monitoring Event",
                                           "Fishing Operations Event",
                                           "Image Quality Event",
                                           "Vessel Event"
                                         )
                                       ),
                                       limits = force)


# Catch Type ----
fillScaleCatchType <- scale_fill_manual(name = "",
                                        values = setNames(c("cadetblue3", "goldenrod2"),
                                                          c("Kept", "Discarded")),
                                        limits = force)

# Gear Type ----
fillScaleGearType <-
  scale_fill_manual(name = "", values = setNames(
    c(
      "indianred1",
      "darkorchid2",
      "olivedrab3",
      "plum3",
      "cyan3",
      "grey50"
    ),
    c("Gillnet", "Jig", "Lobster Pot", "Longline", "Trawl", "Other")
  ),
  limits = force)

# Port ----
fillScalePort <- scale_fill_manual(name = "",
                                   values = setNames(
                                     c(
                                       "royalblue4",
                                       "yellow",
                                       "seagreen4",
                                       "goldenrod2",
                                       "plum3",
                                       "darkred",
                                       "orange2",
                                       "lightgreen",
                                       "steelblue1"
                                     ),
                                     c(
                                       "Chatham",
                                       "Portland",
                                       "Gloucester",
                                       "Plymouth",
                                       "Harwich Port",
                                       "Boston",
                                       "Newburyport",
                                       "Point Judith",
                                       "Provincetown"
                                     )
                                   ),
                                   limits = force)

# Sales Metric ----
fillScaleMetric <- scale_fill_manual(name = "",
                                     values = setNames(
                                       c(
                                         "royalblue4",
                                         "seagreen4",
                                         "indianred2"
                                         ),
                                       c(
                                         "Weight Sold (Landed)",
                                         "Weight Sold (Live)",
                                         "Weight Reported (eVTR)"
                                       )
                                     ),
                                     limits = force)

# Station ----
colScaleStation <- scale_color_manual(name = "",
                                      values = setNames(
                                        c(
                                          "dodgerblue",
                                          "maroon",
                                          "chocolate2",
                                          "olivedrab4"
                                          ),
                                        c(
                                          "A01",
                                          "B01",
                                          "E01",
                                          "F01"
                                          )
                                      ),
                                      limits = force)
