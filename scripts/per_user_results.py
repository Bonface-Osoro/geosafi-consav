import configparser
import os
import math
import time
import warnings
import pandas as pd
import geopandas as gpd
from shapely.wkt import loads
import geosafi_consav.mobile as mb
from geosafi_consav.preprocessing import (convert_multipolygon_to_polygon, 
                                          population_decile)
from inputs import maritime
from tqdm import tqdm
warnings.filterwarnings('ignore')
pd.options.mode.chained_assignment = None 

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
CELL_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'cellular')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')
DATA_SSA = os.path.join(BASE_PATH, '..', 'results', 'SSA')
VALID = os.path.join(BASE_PATH, '..', 'validation')

def process_africa_results():

    """
    This function process Africa data by generating population deciles,
    and calaculting variables needed for capacity, cost and emission models.
    """
    print('Processing geospatial data')

    pop_data = os.path.join(DATA_SSA, 'SSA_area_population.csv')
    connect_data = os.path.join(DATA_SSA, 'SSA_poor_unconnected.csv') 
    gni_data = os.path.join(VALID, 'monthly_broadband_costs.csv') 

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

    gni = pd.read_csv(gni_data)
    gni = gni[['code', 'cost_per_1GB', 'monthly_GNI', 'cost_per_month_usd', 
               'adoption_rate', 'arpu_usd']]
    df = pd.merge(df, gni, left_on = 'iso3', right_on = 'code')

    df = df.drop(['region', 'decile_value', 'latitude', 'longitude', 'iso3', 
                  'code', 'geometry'], axis = 1)
    
    df1 = pd.read_csv(connect_data)
    df1['maritime_km'] = ''
    for i in range(len(df1)):

        df1['maritime_km'].loc[i] = mb.maritime_distance(df1['iso3'].loc[i], maritime)

    df1 = df1[df1['technology'] == '3G']
    df1 = df1[df1['poverty_range'] == 'GSAP2_poor']
    df1 = df1.drop(['region', 'technology', 'poverty_range', 'iso3'], axis = 1)
    #df1 = df1.groupby(['GID_1'])['poor_unconnected'].sum().reset_index()

    df1 = df1.groupby(['GID_1']).agg(poor_unconnected = ('poor_unconnected', 'sum'),
        maritime_km = ('maritime_km', 'mean')).reset_index()

    df2 = pd.merge(df1, df, on = 'GID_1', how = 'inner')
    df2['poor_unconnected'] = df2['poor_unconnected'].round(0) 

    #df2 = df2.groupby(['GID_1'])['population'].mean().reset_index()
    filename = 'SSA_to_be_served_population.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df2.to_csv(path_out, index = False)


    return None


def model_data():
    """
    This function calculates the summary statistics of the African poverty and 
    connectivity results needed in the capacity, cost and emission model.
    """
    print('Generating decile summary statistics')

    pop_data = os.path.join(DATA_SSA, 'SSA_to_be_served_population.csv')
    df = pd.read_csv(pop_data)

    df = df.groupby(['decile']).agg(total_population = ('population', 'sum'),
        total_poor_unconnected = ('poor_unconnected', 'sum'), total_area_sqkm = 
        ('area', 'sum'), total_max_distance_km = ('max_distance_km', 'sum'), 
        mean_poor_connected = ('poor_unconnected', 'mean'), mean_area_sqkm = 
        ('area', 'mean'), mean_distance_km = ('max_distance_km', 'mean'),
        maritime_km = ('maritime_km', 'mean'), 
        cost_per_1GB_usd = ('cost_per_1GB', 'mean'),
        monthly_income_usd = ('monthly_GNI', 'mean'), 
        cost_per_month_usd = ('cost_per_month_usd', 'mean'), 
        adoption_rate_perc = ('adoption_rate', 'mean'), 
        arpu_usd = ('arpu_usd', 'mean')).reset_index()
    coverage_area_4g_base_station = math.pi * 3 ** 2
    coverage_area_5g_base_station = math.pi * 1.6 ** 2

    df['no_of_4g_base_stations'] = round(df['mean_area_sqkm'] / 
                                         coverage_area_4g_base_station)
    df.loc[df['no_of_4g_base_stations'] == 0, 'no_of_4g_base_stations'] = 1
    df['no_of_5g_base_stations'] = round(df['mean_area_sqkm'] / 
                                         coverage_area_5g_base_station)

    filename = 'SSA_decile_summary_stats.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)


    return None


def decile_emissions_per_user():
    """
    This function calculates the per user metrics for each decile.
    """
    print('Generating per user metrics')

    pop_data = os.path.join(CELL_RESULTS, 'mobile_emission_results.csv')
    df = pd.read_csv(pop_data)
    
    ################### Per user emissions #####################
    df['per_user_mfg_ghg_kg'] = (df['total_mfg_ghg'] / 
                                 df['total_poor_unconnected'])
    
    df['per_user_trans_ghg_kg'] = (df['total_trans_ghg_kg'] / 
                                 df['total_poor_unconnected'])
    
    df['per_user_construct_ghg_kg'] = (df['total_construction_ghg_kg'] / 
                                 df['total_poor_unconnected'])
    
    df['per_user_ops_ghg_kg'] = (df['total_operations_ghg_kg'] / 
                                 df['total_poor_unconnected'])

    df['per_user_eolt_ghg_kg'] = (df['total_eolt_ghg_kg'] / 
                                 df['total_poor_unconnected'])
    
    df['per_user_ghg_kg'] = (df['total_emissions_ghg_kg'] / 
                                 df['total_poor_unconnected'])
    
    df['scc_cost_usd'] = (df['total_emissions_ghg_kg'] / 1e3 
                          * df['social_carbon_cost_usd'])
    
    df['per_user_scc_cost_usd'] = (df['scc_cost_usd'] 
                                   / df['total_poor_unconnected'])
    
    df['annualized_per_user_scc_cost_usd'] = (df['per_user_scc_cost_usd'] 
                                              / df['assessment_period'])
    
    df = pd.melt(df, id_vars = ['cell_generation', 'decile', 
         'per_user_scc_cost_usd', 'annualized_per_user_scc_cost_usd',
         'assessment_period', 'per_user_ghg_kg', 'total_emissions_ghg_kg'], 
         value_vars = ['per_user_mfg_ghg_kg', 'per_user_trans_ghg_kg', 
          'per_user_construct_ghg_kg', 'per_user_ops_ghg_kg', 
          'per_user_eolt_ghg_kg'], var_name = 'lca_phase', value_name = 
          'phase_per_user_kg')
    
    df['annualized_per_user_ghg'] = (df['per_user_ghg_kg'] / 
                                     df['assessment_period'])
    
    df['annualized_phase_per_user_kg'] = (df['phase_per_user_kg'] 
                                          / df['assessment_period'])

    df = df[['cell_generation', 'decile', 'assessment_period', 'lca_phase', 
             'phase_per_user_kg', 'annualized_phase_per_user_kg', 
             'per_user_ghg_kg', 'annualized_per_user_ghg', 
             'total_emissions_ghg_kg', 'per_user_scc_cost_usd',
             'annualized_per_user_scc_cost_usd']]

    df['technology'] = 'cellular'
    filename = 'SSA_decile_emissions.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)

    return None


def decile_cost_per_user():
    """
    This function calculates the per user metrics for each decile.
    """
    print('Generating per user metrics')

    pop_data = os.path.join(CELL_RESULTS, 'mobile_cost_results.csv')
    df = pd.read_csv(pop_data)
    
    ################### Per user costs #####################
    
    df['per_user_tco_usd'] = ((df['total_base_station_tco_usd'] / 
                                 df['total_poor_unconnected']) 
                                 * df['adoption_rate_perc'])
    
    df['annualized_per_user_cost_usd'] = (df['per_user_tco_usd'] 
                                          / df['assessment_years'])
    
    df['monthly_per_user_cost_usd'] = (df['annualized_per_user_cost_usd'] / 12)

    df['monthly_price'] = (df['monthly_per_user_cost_usd']) 
    
    df['percent_gni'] = df['monthly_price'] / df['monthly_income_usd'] * 100
    
    df = df[['cell_generation', 'decile', 'per_user_tco_usd',
             'total_base_station_tco_usd', 'annualized_per_user_cost_usd', 
             'monthly_per_user_cost_usd', 'adoption_rate_perc', 'arpu_usd', 
             'monthly_income_usd', 'percent_gni', 'monthly_price']]

    df['technology'] = 'cellular'
    filename = 'SSA_decile_costs.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)


    return None


def decile_capacity_per_user():
    """
    This function calculates the per user metrics for each decile.
    """
    print('Generating per user metrics')

    pop_data = os.path.join(CELL_RESULTS, 'mobile_capacity_results.csv')
    df = pd.read_csv(pop_data)
    
    ################### Per user costs #####################
    
    df['per_user_capacity_mbps'] = (df['base_station_capacity_mbps'] / 
                                 (df['total_poor_unconnected'] * 
                                  df['network_load_perc']))
    
    df['per_area_capacity_mbps'] = (df['base_station_capacity_mbps'] / 
                                 df['mean_area_sqkm'])
    
    df = df[['cell_generation', 'decile', 'network_load_perc', 
             'per_user_capacity_mbps', 'per_area_capacity_mbps']]

    df['technology'] = 'cellular'
    filename = 'SSA_decile_capacity.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)


    return None


if __name__ == '__main__':

    #process_africa_results()

    #model_data()
    
    #decile_capacity_per_user()

    decile_cost_per_user()

    #decile_emissions_per_user()