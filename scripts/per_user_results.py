import configparser
import os
import math
import time
import warnings
import pandas as pd
import geopandas as gpd
from shapely.wkt import loads
from shapely.ops import unary_union
from shapely.geometry import MultiPolygon, Polygon
import geosafi_consav.mobile as mb
from geosafi_consav.preprocessing import (convert_multipolygon_to_polygon, 
                                          population_decile)
from mobile_inputs import lut
from tqdm import tqdm
warnings.filterwarnings('ignore')
pd.options.mode.chained_assignment = None 

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
CELL_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'cellular')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')
DATA_SSA = os.path.join(BASE_PATH, '..', 'results', 'SSA')


def process_africa_results():

    """
    This function process Africa data by generating population deciles,
    and calaculting variables needed for capacity, cost and emission models.
    """
    print('Processing geospatial data')

    pop_data = os.path.join(DATA_SSA, 'SSA_area_population.csv')
    connect_data = os.path.join(DATA_SSA, 'SSA_poor_unconnected.csv')

    df = pd.read_csv(pop_data)
    df[['pop_density_sqkm', 'max_distance_km', 'decile_value', 'decile']] = ''
    df['pop_density_sqkm'] = (df['population'] / df['area'])

    df = df.sort_values(by = 'pop_density_sqkm', ascending = True)                   
    df['decile_value'] = pd.qcut(df['pop_density_sqkm'], 10, 
                                    labels = False) + 1

    df = df.sort_values(by = 'pop_density_sqkm', ascending = True)
    df['decile_value'] = pd.qcut(df['pop_density_sqkm'], 10, 
                                    labels = False) + 1
    
    # Ensure that the geometries are stored properly in the geodataframe
    df['geometry'] = df['geometry'].apply(loads)
    df = gpd.GeoDataFrame(df, geometry = 'geometry')

    # Merge any multipolygons into polygon
    df['geometry'] = df['geometry'].apply(convert_multipolygon_to_polygon)

    # Calculate the furthest point from the centroid and convert the resultant 
    # distance in degrees into kilometers using 1 degree = 111 kilometers

    df['max_distance_km'] = ((df['geometry'].apply(mb.calc_maximum_distance)) 
                             * 111)
    
    for i in range(len(df)):

        df['decile'].loc[i] = population_decile(df['decile_value'].loc[i])
    
    df = df.drop(['region', 'decile_value', 'latitude', 'longitude', 'iso3', 
                  'geometry'], axis = 1)

    df1 = pd.read_csv(connect_data)
    df1 = df1[df1['technology'] == '3G']
    df1 = df1[df1['poverty_range'] == 'GSAP2_poor']
    df1 = df1.drop(['region', 'technology', 'poverty_range', 'iso3'], axis = 1)
    df1 = df1.groupby(['GID_1'])['poor_unconnected'].sum().reset_index()

    df2 = pd.merge(df1, df, on = 'GID_1', how = 'inner')
    df2['poor_unconnected'] = df2['poor_unconnected'].round(0) 

    #df2 = df2.groupby(['GID_1'])['population'].mean().reset_index()
    filename = 'SSA_to_be_served_population.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df2.to_csv(path_out, index = False)

process_africa_results()