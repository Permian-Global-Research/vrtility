# Set NoData Value for VRT

Set the NoData value for a VRT_x object. This information is preserved
in derived VRT and GeoTIFF files.

## Usage

``` r
vrt_set_nodata(x, nodatavalue, nodata, band_idx)

# S3 method for class 'vrt_block'
vrt_set_nodata(x, nodatavalue, nodata = nodatavalue, band_idx = NULL)

# S3 method for class 'vrt_collection'
vrt_set_nodata(x, nodatavalue, nodata = nodatavalue, band_idx = NULL)
```

## Arguments

- x:

  A VRT_x object

- nodatavalue:

  A numeric value to set the NoDataValue XML tag for the VRT_x object.

- nodata:

  A numeric value to set the NODATA XML tag for the VRT_x object.
  defaults to the same as `nodatavalue`.

- band_idx:

  numeric; the target band position(s) to set the NoData value for. If
  NULL, The nodatavalue will be set for all bands.

## Value

A modified object of the same class as `x` with the NoData value set.

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
ex_collect <- vrt_collect(s2files)
ex_nodata <- vrt_set_nodata(
  ex_collect,
  nodatavalue = 0,
  nodata = 0
)
print(ex_nodata[[1]][[1]], xml = TRUE)
#> → <VRT Block>
#> VRT XML: 
#> 
#> <?xml version="1.0" encoding="UTF-8"?>
#> <VRTDataset rasterXSize="361" rasterYSize="342">
#>   <SRS dataAxisToSRSAxisMapping="1,2">PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]</SRS>
#>   <GeoTransform>  2.8981358026064973e+05,  1.9992966901116390e+01,  0.0000000000000000e+00,  9.5714024721781345e+04,  0.0000000000000000e+00, -1.9992966901116390e+01</GeoTransform>
#>   <VRTRasterBand dataType="UInt16" band="1">
#>     <ColorInterp>Gray</ColorInterp>
#>     <ComplexSource name="B02">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>1</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B02</Description>
#>     <NoDataValue>0</NoDataValue>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="2">
#>     <ComplexSource name="B03">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>2</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B03</Description>
#>     <NoDataValue>0</NoDataValue>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="3">
#>     <ComplexSource name="B04">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>3</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B04</Description>
#>     <NoDataValue>0</NoDataValue>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="4">
#>     <ComplexSource name="B08">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>4</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B08</Description>
#>     <NoDataValue>0</NoDataValue>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="5">
#>     <ComplexSource name="SCL">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>5</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>SCL</Description>
#>     <NoDataValue>0</NoDataValue>
#>   </VRTRasterBand>
#>   <Metadata>
#>     <MDI key="datetime"/>
#>   </Metadata>
#> </VRTDataset>
#> 
#> 
#> 
#>  VRT SRS: 
#> PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
#> 
#> 
#> Bounding Box: 289813.58 88876.43 297031.04 95714.02
#> Pixel res: 19.9929669011164, 19.9929669011164
#> Assets: B02, B03, B04, B08, SCL
#> No Data Value(s): 0, 0, 0, 0, 0
#> Date Time: NA
```
