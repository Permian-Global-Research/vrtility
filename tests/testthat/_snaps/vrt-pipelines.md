# full vrt pipeline works async

    Code
      print(t_block)
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-01-01 UTC

---

    Code
      print(ex_collect)
    Message
      > <VRT Collection>
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
       PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
       PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: NA
      Pixel res: 19.9923198138287, 19.9923198138287
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect, blocks = TRUE)
    Output
      [[1]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-01-01 UTC
    Output
      
      [[2]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 460436.9 5615438.2 467554.2 5622175.6
      Pixel res: 19.9923198138287, 19.9923198138287
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-03-01 UTC
    Output
      
      [[3]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-05-01 UTC
    Output
      
      [[4]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-07-01 UTC
    Output
      
      [[5]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-09-01 UTC
    Output
      

---

    Code
      print(ex_collect_mask_warp)
    Message
      > <VRT Collection>
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp, xml = TRUE, maskfun = TRUE)
    Message
      > <VRT Collection>
      Mask Function:
      import numpy as np
      def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                        raster_ysize, buf_radius, gt, **kwargs):
          mask_vals =  [int(x) for x in kwargs['mask_values'].decode().split(',')]
          mask = np.isin(in_ar[0], mask_vals)
          out_ar[:] = np.where(mask, 0, 1)
    Output
      
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack_med)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Pixel Function: [hidden]
        run print(x, pixfun = TRUE) to view
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack_med, pixfun = TRUE)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Pixel Function:
      import numpy as np
      def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
          no_data_val = int(kwargs['no_data_value'])
          # Stack the input arrays first
          stacked = np.stack(in_ar)
          
          # Create a mask of no_data values
          mask = (stacked == no_data_val)
          
          # Create a masked array - more efficient than np.where for this operation
          masked_data = np.ma.array(stacked, mask = mask, shrink = False)
          
          # Calculate median on masked array directly
          out_ar[:] = np.ma.median(masked_data, axis=0).filled(fill_value=no_data_val)
    Output
      
    Message
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

# full vrt pipeline works synchronously

    Code
      print(t_block)
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-01-01 UTC

---

    Code
      print(ex_collect)
    Message
      > <VRT Collection>
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
       PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
       PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: NA
      Pixel res: 19.9923198138287, 19.9923198138287
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect, blocks = TRUE)
    Output
      [[1]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-01-01 UTC
    Output
      
      [[2]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 460436.9 5615438.2 467554.2 5622175.6
      Pixel res: 19.9923198138287, 19.9923198138287
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-03-01 UTC
    Output
      
      [[3]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-05-01 UTC
    Output
      
      [[4]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-07-01 UTC
    Output
      
      [[5]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["unknown",GEOGCS["unknown",DATUM["Unknown based on WGS 84 ellipsoid",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["latitude_of_center",50.72],PARAMETER["longitude_of_center",-3.51],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
      
    Message
      Bounding Box: -3540 -3360 3540 3340
      Pixel res: 20, 20
      Assets: B02, B03, B04, B08, SCL
      No Data Value(s): 0, 0, 0, 0, 0
      Date Time: 2024-09-01 UTC
    Output
      

---

    Code
      print(ex_collect_mask_warp)
    Message
      > <VRT Collection>
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp, xml = TRUE, maskfun = TRUE)
    Message
      > <VRT Collection>
      Mask Function:
      import numpy as np
      def build_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                        raster_ysize, buf_radius, gt, **kwargs):
          mask_vals =  [int(x) for x in kwargs['mask_values'].decode().split(',')]
          mask = np.isin(in_ar[0], mask_vals)
          out_ar[:] = np.where(mask, 0, 1)
    Output
      
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Pixel res: 19.9929669011164, 19.9929669011164
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack_med)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Pixel Function: [hidden]
        run print(x, pixfun = TRUE) to view
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

---

    Code
      print(ex_collect_mask_warp_stack_med, pixfun = TRUE)
    Message
      > VRT STACK
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
      Pixel Function:
      import numpy as np
      def pixfun(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize, raster_ysize, buf_radius, gt, **kwargs):
          no_data_val = int(kwargs['no_data_value'])
          # Stack the input arrays first
          stacked = np.stack(in_ar)
          
          # Create a mask of no_data values
          mask = (stacked == no_data_val)
          
          # Create a masked array - more efficient than np.where for this operation
          masked_data = np.ma.array(stacked, mask = mask, shrink = False)
          
          # Calculate median on masked array directly
          out_ar[:] = np.ma.median(masked_data, axis=0).filled(fill_value=no_data_val)
    Output
      
    Message
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["OSGB36 / British National Grid",GEOGCS["OSGB36",DATUM["Ordnance_Survey_of_Great_Britain_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],AUTHORITY["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.9996012717],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","27700"]]
      
    Message
      Bounding Box: 289813.58 88876.43 297031.04 95714.02
      Start Date: 2024-01-01 UTC
      End Date: 2024-09-01 UTC
      Number of Items: 5
      Assets: B02, B03, B04, B08, SCL

# vrt_collect works with rstac doc_items

    Code
      print(ex_collect_mask)
    Message
      > <VRT Collection>
      Mask Function: [hidden]
        run print(x, maskfun = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Start Date: 2024-06-02 11:21:19 UTC
      End Date: 2024-08-26 11:21:11 UTC
      Number of Items: 5
      Assets: B02, B03, B04

---

    Code
      print(ex_collect_mask, blocks = TRUE, maskfun = TRUE)
    Output
      [[1]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Assets: B02, B03, B04
      No Data Value(s): 0, 0, 0
      Date Time: 2024-06-02 11:21:19 UTC
    Output
      
      [[2]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Assets: B02, B03, B04
      No Data Value(s): 0, 0, 0
      Date Time: 2024-06-17 11:21:21 UTC
    Output
      
      [[3]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Assets: B02, B03, B04
      No Data Value(s): 0, 0, 0
      Date Time: 2024-08-01 11:21:19 UTC
    Output
      
      [[4]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Assets: B02, B03, B04
      No Data Value(s): 0, 0, 0
      Date Time: 2024-08-16 11:21:11 UTC
    Output
      
      [[5]]
    Message
      > <VRT Block>
      VRT XML: [hidden]
        run print(x, xml = TRUE) to view
    Output
      
       VRT SRS: 
      PROJCS["WGS 84 / UTM zone 30N",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",0],PARAMETER["central_meridian",-3],PARAMETER["scale_factor",0.9996],PARAMETER["false_easting",500000],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","32630"]]
      
    Message
      Bounding Box: 399960 5590200 509760 5700000
      Pixel res: 12.5, 12.5
      Assets: B02, B03, B04
      No Data Value(s): 0, 0, 0
      Date Time: 2024-08-26 11:21:11 UTC
    Output
      

