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

southern = ['AGO', 'ZMB', 'ZWE', 'NAM', 'BWA', 'ZAF', 
            'SWZ', 'MOZ', 'MWI', 'LSO']

central = ['CMR', 'CAF', 'TCD', 'COD', 'GNQ', 'GAB', 'STP', 'COG']

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

            first_underscore_index = file_name.find('_')
            extracted_string = file_name[first_underscore_index + 1:]
            extracted_string = os.path.splitext(extracted_string)[0]

            file_path = os.path.join(intersect_folder, file_name)
            shapefile = gpd.read_file(file_path)

            shapefile[['iso3', 'GID_1', 'technology', 'region']] = ''
            technologies = ['GSM', '3G', '4G']

            for i in range(len(shapefile)):

                shapefile['GID_1'].loc[i] = extracted_string

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
            shapefile = shapefile[['iso3', 'GID_1', 'value', 'technology', 'region']]
            renamed_columns = {'value': 'pop_unconnected'}  
            shapefile.rename(columns = renamed_columns, inplace = True)
            merged_shapefile = pd.concat([merged_shapefile, shapefile], ignore_index = True)  
    

    fileout = '{}_unconnected_mapping_results.csv'.format(iso3)
    fileout_1 = '{}_unconnected_tech_reg.csv'.format(iso3)
    fileout_2 = '{}_unconnected_sub_region_results.csv'.format(iso3)
    fileout_3 = '{}_unconnected_geo_reg.csv'.format(iso3)
    fileout_4 = '{}_unconnected_tech_geo.csv'.format(iso3)
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')
    renamed_columns = {'value': 'pop_unconnected'}
    merged_shapefile.rename(columns = renamed_columns, inplace = True)
    
    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)
    path_out_1 = os.path.join(folder_out, fileout_1)
    path_out_2 = os.path.join(folder_out, fileout_2)
    path_out_3 = os.path.join(folder_out, fileout_3)
    path_out_4 = os.path.join(folder_out, fileout_4)

    map_unconnected = merged_shapefile.groupby(['iso3', 'GID_1', 'technology', 'region'])['pop_unconnected'].sum()

    population_data = os.path.join(DATA_RESULTS, iso3, 'population', '{}_population_results.csv'.format(iso3))
    population = pd.read_csv(population_data)
    merged_shapefile = merged_shapefile.groupby(['iso3', 'GID_1', 'technology', 'region'])['pop_unconnected'].sum()
    aggregated_df = merged_shapefile.reset_index()
    df = population.merge(aggregated_df, on = 'GID_1', how = 'outer').reset_index(drop = True)

    df[['pop_density_sqkm', 'geotype']] = ''
    for i in range(len(df)):

        df['pop_density_sqkm'].loc[i] = df['population'].loc[i] / df['area'].loc[i]

        if df['pop_density_sqkm'].loc[i] >= 1000:

            df['geotype'].loc[i] = 'urban'

        elif df['pop_density_sqkm'].loc[i] >= 500 and df['pop_density_sqkm'].loc[i] <= 1000:
            
            df['geotype'].loc[i] = 'suburban'

        elif df['pop_density_sqkm'].loc[i] >= 50 and df['pop_density_sqkm'].loc[i] <= 500:

            df['geotype'].loc[i] = 'rural'

        else:

            df['geotype'].loc[i] = 'remote'

    df = df.drop(columns = ['iso3_y', 'region_x', 'latitude', 'longitude', 'geometry', 'area', 'pop_density_sqkm'])
    renamed_columns = {'iso3_x': 'iso3', 'region_y': 'region'}   
    df.rename(columns = renamed_columns, inplace = True)

    sum_technology_region = df.groupby(['iso3', 'technology', 'region'])['pop_unconnected'].sum()
    sum_geotype_region = df.groupby(['iso3', 'geotype', 'region'])['pop_unconnected'].sum()
    sum_technology_geotype = df.groupby(['iso3', 'geotype', 'technology'])['pop_unconnected'].sum()

    map_unconnected.to_csv(path_out)
    sum_technology_region.to_csv(path_out_1)
    sum_geotype_region.to_csv(path_out_3)
    sum_technology_geotype.to_csv(path_out_4)
    df.to_csv(path_out_2)


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

            gid2_less = ['LSO', 'SSD', 'BEN', 'MRT', 'SWZ']

            if iso3 in gid2_less:

                merged_shapefile = merged_shapefile[['iso3', 'GID_1', 'GSAP2_poor', 'GSAP2_po_1', 
                                                    'GSAP2_po_2', 'poor_population', 'region']] 
                merged_shapefile = pd.melt(merged_shapefile, id_vars = ['iso3', 'GID_1', 'poor_population', 'region'], 
                                value_vars = ['GSAP2_poor', 'GSAP2_po_1', 
                                'GSAP2_po_2'], var_name = 'poverty_range', 
                                value_name = 'poverty_rate')
                
            else:

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
    poverty_data = df2.groupby(['iso3', 'GID_1', 'poverty_range', 'population',
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
                            '{}_unconnected_mapping_results.csv'.format(iso3))
    df1 = pd.read_csv(vulnerability_results)
    df2 = df1.merge(df, on = 'GID_1', how = 'outer').reset_index(drop = True)

    population_data = os.path.join(DATA_RESULTS, iso3, 'population', '{}_population_results.csv'.format(iso3))
    population = pd.read_csv(population_data)

    df2 = df2.merge(population, on = 'GID_1', how = 'outer').reset_index(drop = True)
    df2 = df2.drop(columns = ['iso3_y', 'iso3_x', 'region_y', 'latitude', 'longitude', 
                              'geometry', 'area', 'population_y', 'population_x', 'region'])
    df2.rename(columns = {'iso3_x': 'iso3', 'region_x': 'region'}, inplace = True)

    df2['poor_unconnected'] = df2['pop_unconnected']*((df2['poverty_rate'])/100)
    df2 = df2.drop(columns = ['pop_unconnected', 'poverty_rate', 'poor_population'])

    fileout = '{}_poor_unconnected.csv'.format(iso3)
    folder_out = os.path.join(DATA_RESULTS, iso3, 'csv_files')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    path_out = os.path.join(folder_out, fileout)
    df2.to_csv(path_out)


    return None


def csv_merger(csv_name):
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
    isos = os.listdir(DATA_RESULTS)

    merged_data = pd.DataFrame()
    for iso3 in isos:

        print('Merging csv files for {}'.format(iso3))
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
