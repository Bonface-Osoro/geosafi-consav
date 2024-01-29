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

southern = ['AGO', 'ZMB', 'ZWE', 'NAM', 'BWA', 'ZAF', 'LSO', 
            'SWZ', 'MOZ', 'MWI']

central = ['CMR', 'CAF', 'TCD', 'COD', 'GNQ', 'GAB', 'STP']

eastern = ['BDI', 'COM', 'DJI', 'ERI', 'ETH', 'SWZ', 'MDG', 
           'KEN', 'MUS', 'SDN', 'SYC', 'SOM', 'SSD', 'UGA', 
           'TZA', 'RWA']

west = ['BEN', 'BFA', 'CPV', 'CIV', 'GMB', 'GHA', 'GIN', 
        'GNB', 'LBR', 'MLI', 'MRT', 'NER', 'NGA', 'SEN', 
        'SLE', 'TGO']

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

            shapefile[['iso3', 'technology', 'region']] = ''
            technologies = ['GSM', '3G', '4G']

            for i in range(len(shapefile)):

                if iso3 in southern:

                    shapefile['region'].loc[i] = 'Southern'

                elif iso3 in central:

                    shapefile['region'].loc[i] = 'Central'

                elif iso3 in eastern:

                    shapefile['region'].loc[i] = 'Eastern'

                else: 

                    shapefile['region'].loc[i] = 'West'

            for i in range(len(shapefile)):
                
                for technology in technologies:
                    
                    if technology in file_name:

                        shapefile['technology'].loc[i] = technology

                shapefile['iso3'].loc[i] = iso3 

            shapefile = shapefile.to_crs(crs = 3857) 
            shapefile['area'] = shapefile.geometry.area      
            shapefile = shapefile[['iso3', 'GID_1_1', 'value', 'technology', 'region']]  
            merged_shapefile = pd.concat([merged_shapefile, shapefile], ignore_index = True)       
    
    fileout = '{}_unconnected_results.csv'.format(iso3, merged_shapefile).replace('shp', '_')
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')

    renamed_columns = {'GID_1_1': 'GID_1'}
    
    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)

    merged_shapefile.rename(columns = renamed_columns, inplace = True)
    unconnected = merged_shapefile.groupby(['iso3', 'GID_1', 'technology', 'region'])['value'].sum()
    unconnected.to_csv(path_out)
    

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

            shapefile[['iso3', 'region', 'poor_population']] = ''
            for i in range(len(shapefile)):

                if iso3 in southern:

                    shapefile['region'].loc[i] = 'Southern'

                elif iso3 in central:

                    shapefile['region'].loc[i] = 'Central'

                elif iso3 in eastern:

                    shapefile['region'].loc[i] = 'Eastern'

                else: 

                    shapefile['region'].loc[i] = 'West'

            for i in range(len(shapefile)):

                shapefile['iso3'].loc[i] = iso3  

            merged_shapefile = pd.concat([merged_shapefile, shapefile], ignore_index = True)  
            merged_shapefile = merged_shapefile[['iso3', 'GID_2', 'GSAP2_poor', 'GSAP2_po_1', 
                                                 'GSAP2_po_2', 'poor_population', 'region']] 
            merged_shapefile = pd.melt(merged_shapefile, id_vars = ['iso3', 'GID_2', 'poor_population', 'region'], 
                               value_vars = ['GSAP2_poor', 'GSAP2_po_1', 
                               'GSAP2_po_2'], var_name = 'poverty_range', 
                               value_name = 'poverty_rate')
            
    renamed_columns = {'GID_2': 'GID_1'}   
    merged_shapefile.rename(columns = renamed_columns, inplace = True) 

    population_data = os.path.join(DATA_RESULTS, iso3, 'population', '{}_population_results.csv'.format(iso3))
    population = pd.read_csv(population_data)
    df2 = population.merge(merged_shapefile, on = 'GID_1', how = 'outer').reset_index(drop = True)
    df2['poverty_rate'] = pd.to_numeric(df2['poverty_rate'], errors = 'coerce')

    for i in range(len(df2)):

        df2['poor_population'].loc[i] = (df2['population'].loc[i] * (df2['poverty_rate'].loc[i] / 100))

    df2 = df2.drop(columns = ['iso3_y', 'region_x', 'latitude', 'longitude', 'geometry', 'area', ])
    renamed_columns = {'iso3_x': 'iso3', 'region_y': 'region'}   
    df2.rename(columns = renamed_columns, inplace = True) 

    fileout = '{}_poverty_results.csv'.format(iso3, merged_shapefile).replace('shp', '_')
    poverty_data = df2.groupby(['iso3', 'GID_1', 'poverty_range', 
                    'poverty_rate', 'region'])['poor_population'].sum()
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')
    
    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)
    poverty_data.to_csv(path_out)
    

    return None


def coverage_poverty_csv(iso3):

    """
    This function merges the poverty 
    data with coverage and hazard layers 
    to establish the percentage of the 
    population that is poor, unconnected 
    and vulnerable to climate change 
    driven natural hazards.

    Parameters
    ----------
    iso3 : string
        Country ISO3 code

    """

    poverty_data = os.path.join(DATA_RESULTS, iso3, 'csv_files', 
        '{}_poverty_results.csv'.format(iso3))

    df = pd.read_csv(poverty_data)
    print('Processing {} csv'.format(iso3))
    vulnerability_results = os.path.join(DATA_RESULTS, iso3, 'csv_files', 
                            '{}_unconnected_results.csv'.format(iso3))
    df1 = pd.read_csv(vulnerability_results)

    df2 = df1.merge(df, on = 'GID_1', how = 'outer').reset_index(drop = True)
    df2['poor_unconnected'] = df2['value']*((df2['poverty_rate'])/100)
    df2 = df2.drop(columns = ['iso3_y', 'value', 'poverty_rate'])
    df2.rename(columns = {'iso3_x': 'iso3', 'region_x': 'region'}, inplace = True)
    pov_cov = df2.groupby(['iso3', 'GID_1', 'technology', 'poverty_range', 
                           'region'])['poor_unconnected'].sum()

    fileout = '{}_poor_unconnected.csv'.format(iso3)
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)
    pov_cov.to_csv(path_out)


    return None


def csv_merger(csv_name, iso3):
    """
    This funcion read and merge 
    multiple CSV files located 
    in different folders.

    Parameters
    ----------
    csv_name : string
        Name of the file to process. it can be
        'poverty_results.csv' or 
        'unconnected_results.csv'
    iso3 : string
        Country iso3 to be processed. 
    """

    print('Merging csv files for {}'.format(iso3))
    isos = os.listdir(DATA_RESULTS)

    merged_data = pd.DataFrame()
    for iso3 in isos:

        base_directory = os.path.join(DATA_RESULTS, iso3, 'csv_files') 

        for root, _, files in os.walk(base_directory):

            for file in files:
                
                if file.endswith('{}_{}'.format(iso3, csv_name)):
                    
                    file_path = os.path.join(base_directory, '{}_{}'.format(iso3, csv_name))
                    df = pd.read_csv(file_path)

                    merged_data = pd.concat([merged_data, df], ignore_index = True)

                    fileout = 'SSA_{}'.format(csv_name)
                    folder_out = os.path.join(DATA_RESULTS, '..', 'SSA')

                    if not os.path.exists(folder_out):

                        os.makedirs(folder_out)

                    path_out = os.path.join(folder_out, fileout)
                    merged_data.to_csv(path_out, index = False)


    return None