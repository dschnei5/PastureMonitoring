# PastureMonitoring
Project to utilise Sentinel-2 imagery and custom calibrations to predict paddock level pasture availability

Thanks for choosing to use the pasture monitoring processing repository written in R.  Please note there are a few pre-requisites required for this to work:  

1. R Programming Language installed
2. A username and password for the Copernicus Data Space Ecosystem: https://dataspace.copernicus.eu/
3. An account with ShinyApps.io: https://www.shinyapps.io/
4. Point or polygon shapefiles with your locations of interest in projected coordinate system WGS84 UTM Zone XXY, where XXY matches the UTM zone of the image tile that intersects that shapefile e.g. 56S.

Take your time completing the optional parameters in the settings/settings.R script with a text editor initially.  This is where you choose your parent directories and location to store your large satellite image files. You will also need to define your usernames etc. Note that when the processing script identifies an unknown username it will interactively ask for passwords and store them in your machines keychain for subsequent runs. If you change passwords you will need to manually update your keychain using the functions available in the "keyring" R library. 

## Key Features

**Cloud Masking:** The processor automatically applies cloud masking using the Scene Classification Layer (SCL) from Sentinel-2 L2A products. This masks out clouds, cloud shadows, snow, and other problematic pixels to ensure only valid pasture pixels are analyzed. Cloud masking can be enabled/disabled and customized in the settings file.

**L2A Products:** The system downloads Sentinel-2 L2A products (atmospherically corrected) directly, eliminating the need for Sen2Cor preprocessing. This provides faster processing and ensures consistent, high-quality data.

**GeoJSON-Only Deployment:** The Shiny app uses only GeoJSON files for both mapping and plotting functionality. CSV files are automatically cleaned from the www/DataOut folders before deployment to reduce upload size and improve deployment speed to ShinyApps.io.

**PDF Map Generation:** The processor automatically generates professional PDF maps for each image date and shapefile. Maps include color-coded GDM estimates, paddock labels, north arrow, scale bar, and formatted legend. Maps are saved in the DataOut/{property}/Maps directory and can be used for reporting and analysis.

**Partial Coverage Tile Detection:** The download function automatically identifies and skips partial coverage tiles based on file size. Since partial coverage tiles from Copernicus are typically much smaller than full tiles, this prevents wasted downloads and processing time. The minimum file size threshold is configurable.

## Setup Instructions

Options are available to allow you to connect to a cloud drive, assuming that you are able to mount the location to your local machine or server running the processor, be wary of large bandwidth requirements of moving and processing large image files from a remote source however. Using a web based cloud server will significantly slow down the processing and use a lot of bandwidth.  This functionality is really only designed for storage locations that are connected to a cloud based VM running the processor.

Options are available to allow you to connect to a cloud drive, assuming that you are able to mount the location to your local machine or server running the processor, be wary of large bandwidth requirements of moving and processing large image files from a remote source however. Using a web based cloud server will significantly slow down the processing and use a lot of bandwidth.  This functionality is really only designed for storage locations that are connected to a cloud based VM running the processor.

You will need to manually ascertain the Sentinel image tile which your farm/paddock shapefile intersects.  This can be done interactively in the Copernicus Data Space Ecosystem (link above).  Add the tile name e.g. "56JLM" to the list in the settings and the processor will know that you want to download new images for those tiles.

If you have lots of cores and memory available then feel free to parallel compute the preprocessing and/or post processing by setting them to TRUE in the settings.  If you are just doing one farm on one tile, this won't speed anything up.

## Cloud Masking Configuration

The processor includes automatic cloud masking that uses the Scene Classification Layer (SCL) from Sentinel-2 L2A products. To configure:

1. Set `apply.cloud.mask <- TRUE` in settings/settings.R to enable cloud masking
2. Customize `scl.mask.values` to specify which pixel types to mask out:
   - 0 = NO_DATA
   - 1 = SATURATED_OR_DEFECTIVE  
   - 3 = CLOUD_SHADOWS
   - 8 = CLOUD_MEDIUM_PROBABILITY
   - 9 = CLOUD_HIGH_PROBABILITY
   - 10 = THIN_CIRRUS
   - 11 = SNOW

The default configuration masks out clouds, cloud shadows, snow, and defective pixels while preserving vegetation, water, and other valid land cover types.

## PDF Map Configuration

Maps are automatically generated for each image date and shapefile property. To configure:

1. Set `create.maps <- TRUE` in settings/settings.R to enable map generation
2. Set `check.for.missing.maps <- TRUE` to automatically detect and create missing maps during processing

Maps are saved in DataOut/{property_name}/Maps/ with filenames like:
- `PropertyName_GDM_2024-01-15.pdf`

Each map includes:
- Color-coded GDM estimates (0-4000 kg/ha gradient)
- Paddock name labels and GDM values
- Scale bar and north arrow
- Professional legend and formatting
- Generation timestamp

## Partial Coverage Tile Filtering

The download function automatically filters out partial coverage tiles based on file size. To configure:

1. Set `skip.partial.coverage <- TRUE` in settings/settings.R to enable filtering (default: TRUE)
2. Set `min.file.size.mb <- 800` to specify the minimum file size threshold in MB (default: 800)

Partial coverage tiles from Copernicus Data Space are typically less than 500-600 MB, while full tiles are usually 900+ MB. When a tile is identified as partial coverage, it will be logged as skipped and the download will be prevented, saving bandwidth and processing time.

Messages like the following will appear:
```
Skipping partial coverage tile for 56JLL on 2024-01-15 - File size: 450 MB (threshold: 800 MB)
```

## Running the Processor

Once initial setup is complete, the processing can be initiated interactively by opening the processpasturedata.R with Rscript.  It can also be "sourced" from within the R-console or R-Studio.  After the 1st successful interactive complete run of the processpasturedata.R script the processpasturedata.R script can be automatically sourced to initiate the processing on a schedule.  This can be done using Task Scheduler on Windows or as a Cron job on Linux ensuring you select Rscript as the software to open it with.  If you really want to be tricky, why not install node-red and control the processing from your own dashboard! I will include an example flow file to get you started soon. 

It is strongly advised that after your first run when all directories are created you manually go and download as many historic/archived tiles as possible for your farms from the Copernicus Data Space Ecosystem.  You can do up to 4 concurrent downloads simultaneously.  You can also extend how many days the script looks back in time for the initial few runs to get all the available imagery for the preceding 6-12months.  Leaving this set >10 days is not recommended because it will slow down the data collection by causing the script to query the cloud% of many images from the API.  If you add new farm or paddock shapefiles to your shapefiles folder you will need to rerun the processing script interactively to set them up. The script scans the shapefiles folder every run but only processes those which it has established metadata for.     

Generating your own pasture calibration model is advised, otherwise you can simply use the default calibration supplied.  A calibration maker function is included to generate calibrations from your custom calibration data.  You specify the calibration to use for each paddock/site by updating the "PAS_TYP" column in the attributes table of your shapefiles. This column and a "PADD_NAME" column are the only two required columns in the shapefiles.  

You will also need to create a treemask for each Sentinel image tile.  This can be done using the included treemask creation function for any downloaded image. If a treemask doesn't exist for a downloaded tile, it will need to be generated by interactively running the processor and choosing to create treemasks where missing. These mask can be edited manually at any time using QGIS and the Surval add-on: https://github.com/lutraconsulting/serval/blob/master/Serval/docs/user_manual.md to ensure accuracy for each of your farms/paddocks. The best image to use to create an auto mask is an image collected when your farm was drought affected, largest difference between pasture and tree leaf greeness. The repository includes a demo shapefile and a demo calibration data set.  These can be copied and edited to include your data, or used as templates for your farms.   

Please note: I am building this repository still.  Pre-processor is working, post-processor not yet available, please check again soon...
