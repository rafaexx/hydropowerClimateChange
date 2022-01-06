# run zonal statistics on WRF data for each dam catchment

# this script has three functions:

# The first creates raster layers for each variable, only needs to be run the first time

# The second calculates all statistics for all watersheds in a geodatabase and writes the results to a pickle

# The third reads the pickle and writes out results to the point file

import arcpy, os
from arcpy.sa import *
from tqdm import tqdm
import pickle
import pandas as pd
arcpy.CheckOutExtension("Spatial")

# I set this up to run in multiple batches simultaneously, each had a different suffix on the geodatabases to not overlap -- can leave empty if only one run
batch = ""

# input data
folder = r'C:\Mira\OneDrive\WRF\hydro_catchments\hydro_catchments'
area_grid = r"C:/data/area.gdb/pxarea_15s"
grand_gdb = os.path.join(folder, 'grand_catch{}.gdb'.format(batch))
fhred_shed_gdb = os.path.join(folder, 'fhred_catch{}.gdb'.format(batch))

# existing point dam files to update with results
grand_pt_results = r"C:\Mira\OneDrive\WRF\hydro_catchments\hydro_catchments\dam_results.gdb\FHReD2015_withGOID",
fhred_pt_results = r"C:\Mira\OneDrive\WRF\hydro_catchments\hydro_catchments\dam_results.gdb\GRanD_v1_3_selection_stats"

# geodatabase containing rasters for each WRF variable
# if these do not already exist, use function create_vars in this script
gdb_vars = os.path.join(folder, 'WRF_vars{}.gdb'.format(batch))

# set alignment and resolution to match any HydroSHEDS grid
arcpy.env.overwriteOutput = True
arcpy.env.CellSize = "global_dir_15"
arcpy.env.SnapRaster = "global_dir_15"
arcpy.env.ParallelProcessingFactor = '80%'
print("running stats batch {}".format(batch))

# set scratch workspace, and create if not already existing
scratch = os.path.join(folder, 'scratch{}.gdb'.format(batch))
if not arcpy.Exists(scratch):
    arcpy.CreateFileGDB_management(folder, 'scratch{}.gdb'.format(batch))
arcpy.env.workspace = scratch

# list of variables to use
vars_hydro = [ "RC1", "RC2", "RC10", "RC1_O30", "RC1_O50", "RC1_C30", "RC1_C50", "RC1_P30", "RC1_P50", "RC2_O30", "RC2_O50", "RC2_C30", "RC2_C50", "RC2_P30", "RC2_P50"]
vars_chg = ["RC1_O3rc", "RC1_O5rc", "RC1_C3rc", "RC1_C5rc", "RC1_P3rc", "RC1_P5rc", "RC2_O3rc", "RC2_O5rc", "RC2_C3rc", "RC2_C5rc", "RC2_P3rc", "RC2_P5rc"]

# create one grid per variable at the 15s HydroSHEDS resolution and alignment
def create_vars():
    arcpy.CreateFileGDB_management(folder, 'WRF_vars{}.gdb'.format(batch))
    with arcpy.EnvManager(snapRaster=area_grid, cellSize=area_grid, parallelProcessingFactor = '80%', workspace=gdb_vars):
        for var in tqdm(vars_hydro):
            arcpy.PolygonToRaster_conversion('WRF_Scenarios_iwHydropower', var, var,'MAXIMUM_COMBINED_AREA')
        for var in tqdm(vars_chg):
            arcpy.PolygonToRaster_conversion('WRF_Scenarios_riskChanges', var, var,'MAXIMUM_COMBINED_AREA')

# # uncomment to run create_vars function only if the rasters of WRF variables do not already exist
# create_vars()

# list all rasters of WRF data
with arcpy.EnvManager(workspace = gdb_vars):
    all_vars = arcpy.ListRasters()

# function to calculate zonal statistics for a single variable for a single watershed
# this function gets called in the next step ("get_all")
# returns a single value of the statistic calculated
def get_stats(shed, ID, var, type):
    ZonalStatisticsAsTable(shed.format(ID), "Value", var, 'temp_stats', statistics_type=type)
    with arcpy.da.SearchCursor("temp_stats", [type]) as cursor1:
        for row1 in cursor1:
            val = row1[0]
    return val

# function to calculate all statistics for a single watershed
# this function gets called in the next step ("make_pickle")
# returns a list of calculated zonal statistic results
def get_all(shed, ID_now):

    # this step creates a temporary polygon file which is only used to define the extent of the calculations so that this function runs faster
    arcpy.conversion.RasterToPolygon(shed, "poly", "SIMPLIFY", "Value", "SINGLE_OUTER_PART")

    # use temporary polygon to set extent
    with arcpy.EnvManager(snapRaster=shed, extent="poly"):

        # call get_stats function to calculate area -- this is used to compare with the given area to check if the watersheds are correct
        try:
            area = get_stats(shed, ID_now, area_grid, "SUM")

        except:
            area = -9999
            print("No area for {}!!".format(ID_now))

        # create empty list to store results
        results_now = []

        # loop through variables, calculating the mean value for each variable within the watershed
        for x in all_vars:
            var_now = os.path.join(gdb_vars, x)
            try:
                var_val = get_stats(shed, ID_now, var_now, "MEAN")
            except:
                var_val = -9999
                print("No result for {} for variable {}!!".format(ID_now, x))
            results_now.append(var_val)
        results_now.append(area)
    return results_now

# this function creates a dictionary with dam IDs as keys, and lists of statistic calculations as values
# the dictionary is saved as a pickle
def make_pickle(sheds_gdb, to_name):

    # create dictionary to store values
    vals_dict = {}

    # get all watersheds
    with arcpy.EnvManager(workspace=sheds_gdb):
        sheds_now = arcpy.ListRasters("shed_*")

    # loop through each watershed
    for j in tqdm(sheds_now):
        name = os.path.basename(j)
        ID_now = str(name.split("_")[1])
        in_shed = os.path.join(sheds_gdb, j)
        # call function get_all to calculate all zonal statistics
        results_now = get_all(in_shed, ID_now)
        # add dam ID and statistics to dictionary
        vals_dict[ID_now] = results_now

    # save file as pickle
    picklefile = open(to_name, 'ab')
    # source, destination
    pickle.dump(vals_dict, picklefile)
    picklefile.close()

# function to update existing dam point files with calculated statistics
def update_points(point_file, dict_results):
    # create attribute columns
    var_fields = vars_hydro + vars_chg + "area_calc"
    for var in var_fields:
            arcpy.AddField_management(point_file, var, "FLOAT")
    if point_file == fhred_shed_gdb:
        vals_now = ['DAM_ID'] + var_fields
    else:
        vals_now = ['GRAND_ID'] + var_fields

    # cursor to update dam databases
    with arcpy.da.UpdateCursor(point_file, vals_now) as cursor:
        for row in cursor:
            # for each row in the file, get the dam ID
            if vals_now[0] == 'DAM_ID':
                ID = str(row[0])[:-2]
            else:
                ID = str(row[0])

            # get calculated statistics from dictionary, and update attribute columns
            vals_in = dict_results.get(ID)
            for k in range(len(all_vars)):
                print(vals_in[k])
                row[k + 1] = vals_in[k]
            cursor.updateRow(row)


# # uncomment to run
# make_pickle(grand_gdb, "grand_stats{}".format(batch))
# make_pickle(fhred_catch_gdb, "fhred_stats{}".format(batch))

# uncomment to run
# update_points(grand_pt_results, "grand_stats{}".format(batch))
# update_points(fhred_pt_results, "fhred_stats{}".format(batch))