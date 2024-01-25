import os
import time
import pandas as pd
import geopandas as gpd
import configparser
pd.options.mode.chained_assignment = None

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))

BASE_PATH = CONFIG['file_locations']['base_path']
DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH,  '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')


def generate_unconnected_csv(intersect_folder, iso3):
    """
    This function generate a single 
    csv file of unconnected population 
    for an individual country  
    by cellphone technology.
    
    Parameters
    ----------
    intersect_folder : string
        Path of the folder containing 
        intersected shapefiles
    iso3 : string
        Country ISO3 code
    """
    
    print('processing unconnected cellphone {} csv'.format(iso3))
    merged_shapefile = gpd.GeoDataFrame()

    for file_name in os.listdir(intersect_folder):

        if file_name.endswith('.shp'):

            file_path = os.path.join(intersect_folder, file_name)
            shapefile = gpd.read_file(file_path)

            shapefile[['iso3', 'technology']] = ''
            technologies = ['GSM', '3G', '4G']

            for i in range(len(shapefile)):
                
                for technology in technologies:
                    
                    if technology in file_name:

                        shapefile['technology'].loc[i] = technology

                shapefile['iso3'].loc[i] = iso3 

            shapefile = shapefile.to_crs(crs = 3857) 
            shapefile['area'] = shapefile.geometry.area      
            shapefile = shapefile[['iso3', 'GID_1_1', 'value', 'technology']]  
            merged_shapefile = pd.concat([merged_shapefile, shapefile], ignore_index = True)       
    
    fileout = '{}_unconnected_results.csv'.format(iso3, merged_shapefile).replace('shp', '_')
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')

    renamed_columns = {'GID_1_1': 'GID_1'}
    
    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)

    merged_shapefile.rename(columns = renamed_columns, inplace = True)
    merged_shapefile.to_csv(path_out, index = False)
    

    return None


def generate_poverty_csv(iso3):
    """
    This function generate a single 
    csv file of the people living 
    below the poverty level 
    for an individual country  
    by cellphone technology.
    
    Parameters
    ----------
    intersect_folder : string
        Path of the folder containing 
        intersected shapefiles
    iso3 : string
        Country ISO3 code
    """
    
    print('Generating poverty in-line population {} csv'.format(iso3))
    merged_shapefile = gpd.GeoDataFrame()
    intersect_folder = os.path.join(DATA_PROCESSED, iso3, 'poverty', 'national')

    for file_name in os.listdir(intersect_folder):

        if file_name.endswith('.shp'):
            
            file_path = os.path.join(intersect_folder, file_name)
            shapefile = gpd.read_file(file_path)

            shapefile[['iso3', 'region', 'income',]] = ''

            for i in range(len(shapefile)):

                shapefile['iso3'].loc[i] = iso3  

            merged_shapefile = pd.concat([merged_shapefile, shapefile], ignore_index = True)  
            merged_shapefile = merged_shapefile[['iso3', 'GID_1', 'GSAP2_poor', 'GSAP2_po_1', 
                                                 'GSAP2_po_2', 'income', 'region']] 
            merged_shapefile = pd.melt(merged_shapefile, id_vars = ['iso3', 'GID_1'], 
                               value_vars = ['GSAP2_poor', 'GSAP2_po_1', 
                               'GSAP2_po_2'], var_name = 'poverty_range', 
                               value_name = 'poverty_rate')
             
    fileout = '{}_poverty_results.csv'.format(iso3, merged_shapefile).replace('shp', '_')
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)
    merged_shapefile.to_csv(path_out, index = False)
    

    return None