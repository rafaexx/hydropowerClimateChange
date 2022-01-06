The catchments.py file contains scripts to create watersheds for dam points.
This script contains three functions:
     The first function creates watersheds based on the GRanD dam points. Since these points are aligned to the river network, they can be used directly to create watersheds (grand_catchments)

     The second function creates watersheds for each FHreD point. These points are created using the associated HydroRIVER (based on the GOID). This function creates a watershed for the central points of each river reach. This is to avoid using the most downstream point, which will usually include the catchments of other rivers which meet at that point. (fhred_catchments)

     The third function creates watersheds for each FHreD point which did not produce a correctly-sized watershed in the previous function. These points are created using the associated HydroRIVER (based on the GOID). This function creates a watershed for the most upstream point of each river reach. This is to avoid using the downstream point, which will usually include the catchments of other rivers which meet at that point. (fhred_catchments_part2)


The zonal_stats.py file contains scripts to calculate watershed statistics.
This script has three functions:
	The first creates raster layers for each variable, only needs to be run the first time. (create_vars)
	The second calculates all statistics for all watersheds in a geodatabase, stores them in a dictionary, and writes the results to a pickle. (make_pickle)
	The third reads the pickle and writes out results to the point file. (update_points)

The Jupyter notebook was used to check the calculated statistics and find errors. This script has no direct outputs, but identified catchments with areas larger or smaller than the given areas, or with invalid calculated statistics.

Workflow:
Run create_vars once before all zonal statistic calculations.

GRanD: run grand_catchments -> run make_pickle to calculate results -> check results in Jupyter -> correct the few watersheds which produced errors -> run grand_catchments on corrected watersheds -> run make_pickle to calculate new statistics for corrected watersheds -> update grand results dictionary with corrected watersheds -> run update points to add attributes to point file

FHReD:run fhred_catchments -> run make_pickle to calculate results -> open results in Jupyter -> list dam IDs with errors -> run fhred_catchments_part2 on points with errors -> run make_pickle to calculate new statistics for corrected watersheds -> update fhred results dictionary with corrected watersheds -> check results and correct remaining incorrect watersheds -> run make_pickle on corrected watersheds -> update fhred results dictionary with corrected watersheds -> run update points to add attributes to point file