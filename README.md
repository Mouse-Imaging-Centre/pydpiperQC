# pydpiperQC
A shiny App for quality checking Pydpiper registrations.

## Installation
```r
devtools::install_github("Mouse-Imaging-Centre/pydpiperQC", upgrade_dependencies = FALSE)
```

## Quickstart Guide
This downloads 450MB of example data and uses it to showcase the application.
```r
pydpiperQC::launch_example()
```

## Basic Workflow
To launch the application,
```r
pydpiperQC::launch(annotation = "path/to/annotation.csv", consensus = "path/to/consensus.mnc")
```
or
```r
pydpiperQC::launch()
```
and then use the upload widgets at the top left to upload your annotation and consensus files.  

1. On the left, adjust the intensity range of the consensus according to the intensity histogram.
2. On the left, adjust the slice range of the consensus according to the slice indicator.
3. On the right, adjust the intensity range of the comparate according to the intensity histogram.
4. On the left, slide the contour value around and visually verify them,
or tick the alpha radio button and slide the alpha value around to verify a smooth transition between the consensus and comparate files.
5. Give the comparate a rating by hitting **1, 2, 3, 4, 5**.
6. Hit **s** to move on to the next comparate and continue.
7. At any time, scroll down to see what work you have already done.
8. On the top left, download your annotations as a csv file.

## Hotkeys
**w/s** to toggle previous/next image  
**1, 2, 3, 4, 5**  to enter a rating
