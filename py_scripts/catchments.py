# Script to create watersheds from dam points

# This script contains three functions:
#     the first function creates watersheds based on the GRanD dam points. Since these points are aligned to the river
#     network, they can be used directly to create watersheds

    # The second function creates watersheds for each FHreD point. These points are created using the associated
    # HydroRIVER (based on the GOID). This function creates a watershed for the central points of each river reach.
    # This is to avoid using the most downstream point, which will usually include the catchments of other rivers
    # which meet at that point.

    # The third function creates watersheds for each FHreD point which did not produce a correctly-sized watershed in
    # the previous function. These points are created using the associated
    # HydroRIVER (based on the GOID). This function creates a watershed for the most upstream point of each river reach.
    # This is to avoid using the downstream point, which will usually include the catchments of other rivers
    # which meet at that point.

import arcpy, os
from arcpy.sa import *
from tqdm import tqdm
arcpy.CheckOutExtension("Spatial")

# I set this up to run in multiple batches simultaneously, each had a different suffix on the geodatabases to not overlap -- can leave blank if only one run
batch = ""
print("Running batch 0{}".format(batch))

# flow directions at 15s resolution. this can be done using global data or by continent.
directions = r"C:\data\hydro15s.gdb\global_dir_15"
acc = r"C:\data\hydro15s.gdb\global_acc_15"
# HydroRIVERS database
hyrivs = r"C:\data\HydroRIVERS_v10\HydroRIVERS_v10{}.gdb\HydroRIVERS_v10".format(batch)
# pixel areas
area = r"C:/data/area.gdb/pxarea_15s"

# location of workspace
location = r"C:\Mira\OneDrive\WRF\hydro_catchments\hydro_catchments"

# input dam points
in_data = os.path.join(location, 'in_data{}.gdb'.format(batch))
fhred = os.path.join(in_data, 'fhred')
grand = os.path.join(in_data, 'grand_selected')

# output geodatabases -- will be created if they do not exist
fhred_shed_gdb = os.path.join(location, 'fhred_catch{}.gdb'.format(batch))
rivs_gdb = os.path.join(location, 'fhred_rivs{}.gdb'.format(batch))
grand_shed_gdb = os.path.join(location, 'grand_catch{}.gdb'.format(batch))

# set cell size and snap raster to any HydroSHEDS 15s grid to ensure alignment
arcpy.env.overwriteOutput = True
arcpy.env.CellSize = directions
arcpy.env.SnapRaster = directions
arcpy.env.ParallelProcessingFactor = "50%"


# function to create grand watersheds
def grand_catchments(dams):
    # create output geodatabase if it does not exist
    if not arcpy.Exists(grand_shed_gdb):
        arcpy.CreateFileGDB_management(location, 'grand_catch{}.gdb'.format(batch))
    arcpy.env.workspace = grand_shed_gdb

    # loop through grand points
    # create a layer so that a single point can be selected for the watershed tool
    arcpy.MakeFeatureLayer_management(dams, "GRanD_v1_3_selection")
    with arcpy.da.SearchCursor("GRanD_v1_3_selection", ["SHAPE@XY", "GRAND_ID", "CATCH_SKM"]) as cursor:
        for row in tqdm(cursor):
            # get dam ID, select that row in layer
            ID_now = str(row[1])
            query = "GRAND_ID = {}".format(ID_now)
            arcpy.SelectLayerByAttribute_management('GRanD_v1_3_selection', 'NEW_SELECTION', query)

            # respecify environments to force these settings
            with arcpy.EnvManager(snapRaster=directions,
                                  cellSize=directions, parallelProcessingFactor='80%'):
                # create watershed for selected dam point
                try:
                    catch = arcpy.sa.Watershed('global_dir_15', 'GRanD_v1_3_selection')
                    catch.save("shed_{}".format(ID_now))
                except:
                    print("Could not create catchment for {}".format(ID_now))



# function to find catchments for fhred points based on the HydroRIVER GOID
# this is slightly more complicated because the dam points are not exactly aligned to the river network, so I used
# the river reach of each dam to create the pour point
def fhred_catchments(dams):
    # create geodatabases to save outputs
    if not arcpy.Exists(rivs_gdb):
        arcpy.CreateFileGDB_management(location, 'fhred_rivs{}.gdb'.format(batch))
    if not arcpy.Exists(fhred_shed_gdb):
        arcpy.CreateFileGDB_management(location, 'fhred_catch{}.gdb'.format(batch))
    arcpy.env.workspace = fhred_shed_gdb

    # loop through fhred file
    arcpy.MakeFeatureLayer_management(hyrivs, 'HydroRIVERS_v10')
    with arcpy.da.SearchCursor(dams, ["OBJECTID", "DAM_ID", "UPLAND_SKM", "GOID"]) as cursor:
        for row in tqdm(cursor):

            # get the dam ID and the HydroRIVERS GOID
            ID_now = str(row[1])[:-2]
            riv_id = str(row[3])

            # names of output files with dam ID
            rivname = os.path.join(rivs_gdb, "rivs_{}".format(ID_now))
            out_shed = os.path.join(fhred_shed_gdb, "shed_{}".format(ID_now))

            # skip existing
            if arcpy.Exists(out_shed):
                continue

            # select corresponding HydroRIVER for dam point
            query = "OBJECTID = {}".format(riv_id)
            arcpy.SelectLayerByAttribute_management('HydroRIVERS_v10', 'NEW_SELECTION', query)

            # specifying the environment settings again, sometimes this part didn't respect the global environment settings
            with arcpy.EnvManager(snapRaster=directions,
                                  cellSize=directions, parallelProcessingFactor='80%'):

                # convert HydroRIVER reach to a raster, then find central pixels to use as watershed pour point
                try:
                    riv_pts = arcpy.PolylineToRaster_conversion('HydroRIVERS_v10', "num_1", 'riv_pts', "MAXIMUM_LENGTH", cellsize=directions)
                    foc_riv = arcpy.sa.FocalStatistics(riv_pts, "Rectangle 3 3 CELL", "SUM", "DATA")
                    riv_centre = Con(foc_riv > 2, riv_pts)
                    riv_centre.save(rivname)
                except:
                    print("Could not create river for {}".format(ID_now))

                # create watershed based on river pour point
                try:
                    catch = arcpy.sa.Watershed(directions, riv_centre)
                    catch.save(out_shed)
                except:
                    print("Could not create catchment for {}".format(ID_now))


# this function was used in cases where the fhred_catchments approach did not produce the right catchment area.
# this approach uses the most upstream point in each river reach as the pour point.
# I applied this function to only the selection of dams where the first approach did not work
# the list of these dams is the second input, written eg. ["100", "207", "3045"]
def fhred_catchments_part2(dams, list):
    # create output geodatabases
    if not arcpy.Exists(rivs_gdb):
        arcpy.CreateFileGDB_management(location, 'fhred_rivs{}.gdb'.format(batch))
    if not arcpy.Exists(fhred_shed_gdb):
        arcpy.CreateFileGDB_management(location, 'fhred_catch{}.gdb'.format(batch))

    arcpy.env.workspace = fhred_shed_gdb

    # loop through list of dam IDs which need recalculation, and select associated HydroRIVER
    arcpy.MakeFeatureLayer_management(hyrivs, 'HydroRIVERS_v10')
    for i in tqdm(list):
        with arcpy.da.SearchCursor(dams, ["OBJECTID", "DAM_ID", "UPLAND_SKM", "GOID"], where_clause ="DAM_ID = {}".format(i)) as cursor:
            for row in cursor:
                ID_now = i
                riv_id = str(row[3])
                rivname = os.path.join(rivs_gdb, "rivs_{}".format(ID_now))
                out_shed = os.path.join(fhred_shed_gdb, "shed_{}".format(ID_now))
                query = "OBJECTID = {}".format(riv_id)
                arcpy.SelectLayerByAttribute_management('HydroRIVERS_v10', 'NEW_SELECTION', query)

                # set environments to force resolution & alignment
                with arcpy.EnvManager(snapRaster=directions,
                                      cellSize=directions, parallelProcessingFactor='80%'):

                    # create raster from HydroRIVER reach and select most upstream point to use as pour point
                    try:
                        riv_pts = arcpy.PolylineToRaster_conversion('HydroRIVERS_v10', "num_1", 'riv_pts', "MAXIMUM_LENGTH", cellsize=directions)
                        zon_riv = arcpy.sa.ZonalStatistics(riv_pts, "Value", acc_grid, "MINIMUM")
                        riv_centre = Con(zon_riv == acc_grid, riv_pts)
                        riv_centre.save(rivname)
                    except:
                        print("Could not create river for {}".format(ID_now))

                    # create catchment for dam from river pour point
                    try:
                        catch = arcpy.sa.Watershed(directions, riv_centre)
                        catch.save(out_shed)
                    except:
                        print("Could not create catchment for {}".format(ID_now))

# # uncomment to call functions -- the list in fhred_catchments_part2 is an example of a dam ID list
# grand_catchments(grand)
# fhred_catchments(fhred)
# fhred_catchments_part2(fhred, ["1", "7", "1004", "3524"])