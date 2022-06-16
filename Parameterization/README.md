These are all the scripts I used to parameterize the model. It relies on a lot of data that is too big to host easily (e.g. the entire MTBS history for the Sierra Nevada), so please get in touch with Sam Flake if you want to run any of these scripts and the data aren't available on GitHub (in the calibration data folder). 

These will be updated with some vignettes with reproducible examples in the future, hopefully!

In general, the workflow using these scripts is as follows:

For SCRPPLE fire spread:
--pre-process some input layers
	--combine_mtbs_mosaics.R
	--create_fine_and_ladder_fuels_from_landfire_whole_sierra.R
	--estimate_fine_fuels_from_landfire_tcsi_area.R
	--get_ladder_fuels_from_IC.R
	--get_slope_from_dem.R
	--process_geomac_files.R
	--
--download data
	--download_and_process_fire_spread_data.R
--fit models
	--fit_models_fire_spread.R
--calibrate
	--process_scrpple_output_parameterize_fire_spread.R
