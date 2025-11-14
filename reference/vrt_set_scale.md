# set the scale values for a VRT_x object

Set the Scale XML tag for a VRT_x object. This information is preserved
in derived VRT and GeoTIFF files.

## Usage

``` r
vrt_set_scale(x, scale_value, offset_value, band_idx)
```

## Arguments

- x:

  A VRT_x object

- scale_value:

  a numeric vector of length 1 or equal to the number of bands in the
  VRT_x object.

- offset_value:

  a numeric value to set the Offset XML tag for the VRT_x object.
  Default is 0.

- band_idx:

  numeric; the target band position(s) to set the scale value for. If
  NULL, The scale_value will be set for all bands.

## Details

This sets the Scale and Offset XML tags for the VRT_x object. This is
useful for scaling the pixel values of the VRT_x object. This is
however, a little confusing - the actual values are not altered but the
scaling value is applied when read by most software.

To unset a scale or offset simply set scale= 1 and offset = 0.

## Examples

``` r
s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
ex_collect <- vrt_collect(s2files)
ex_sc1 <- vrt_set_scale(
  ex_collect,
  scale_value = 1e-4
)
print(ex_sc1[[1]][[1]], xml = TRUE)
#> → <VRT Block>
#> VRT XML: 
#> 
#> <?xml version="1.0" encoding="UTF-8"?>
#> <VRTDataset rasterXSize="361" rasterYSize="342">
#>   <SRS dataAxisToSRSAxisMapping="1,2">PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]</SRS>
#>   <GeoTransform>  2.8981358026064973e+05,  1.9992966901116390e+01,  0.0000000000000000e+00,  9.5714024721781345e+04,  0.0000000000000000e+00, -1.9992966901116390e+01</GeoTransform>
#>   <VRTRasterBand dataType="UInt16" band="1">
#>     <NoDataValue>0</NoDataValue>
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
#>     <Scale>0.0001</Scale>
#>     <Offset>0</Offset>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="2">
#>     <NoDataValue>0</NoDataValue>
#>     <ComplexSource name="B03">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>2</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B03</Description>
#>     <Scale>0.0001</Scale>
#>     <Offset>0</Offset>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="3">
#>     <NoDataValue>0</NoDataValue>
#>     <ComplexSource name="B04">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>3</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B04</Description>
#>     <Scale>0.0001</Scale>
#>     <Offset>0</Offset>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="4">
#>     <NoDataValue>0</NoDataValue>
#>     <ComplexSource name="B08">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>4</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>B08</Description>
#>     <Scale>0.0001</Scale>
#>     <Offset>0</Offset>
#>   </VRTRasterBand>
#>   <VRTRasterBand dataType="UInt16" band="5">
#>     <NoDataValue>0</NoDataValue>
#>     <ComplexSource name="SCL">
#>       <SourceFilename relativeToVRT="0">/home/runner/work/_temp/Library/vrtility/s2-data/exe_2024-06-02_12-21-19_EPSG27700.tif</SourceFilename>
#>       <SourceBand>5</SourceBand>
#>       <SourceProperties RasterXSize="361" RasterYSize="342" DataType="UInt16" BlockXSize="361" BlockYSize="2"/>
#>       <SrcRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <DstRect xOff="0" yOff="0" xSize="361" ySize="342"/>
#>       <NODATA>0</NODATA>
#>     </ComplexSource>
#>     <Description>SCL</Description>
#>     <Scale>0.0001</Scale>
#>     <Offset>0</Offset>
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
