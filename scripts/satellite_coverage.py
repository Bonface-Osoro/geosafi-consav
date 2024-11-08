import os
import time
import pandas as pd
import geopandas as gpd
import configparser
import warnings
from inputs import decile_satellites, satellites
pd.options.mode.chained_assignment = None
warnings.filterwarnings('ignore')

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))

BASE_PATH = CONFIG['file_locations']['base_path']
SAT_CAPACITY = os.path.join(BASE_PATH, '..', '..', 'saleos', 'data', 'processed')
DATA_PROCESSED = os.path.join(BASE_PATH,  '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')
DATA_SSA = os.path.join(BASE_PATH, '..', 'results', 'SSA')


def decile_satellite(decile):

    """
    This function assigns the number of satellites based on the decile

    Parameters
    ----------
    decile : string
        Population decile category

    Returns
    -------
    number_of_satellites : int
        Number of satellites over a country.
    """
    for key, sat_numbers in decile_satellites.items():

        if key == decile:
            
            number_of_satellites = sat_numbers

    return number_of_satellites


def satellite_coverage():
    """
    This function calculate the capacity provided by the satellites for users in
    different areas across Sub-Saharan Africa.
    """

    satellite_capacity = os.path.join(SAT_CAPACITY, 'interim_results_capacity.csv')

    sat = pd.read_csv(satellite_capacity)

    starlink_cap = sat[sat['constellation'] == 'Starlink']
    starlink_cap = starlink_cap['capacity_per_single_satellite_mbps'].mean()

    oneweb_cap = sat[sat['constellation'] == 'OneWeb']
    oneweb_cap = (oneweb_cap['capacity_per_single_satellite_mbps'].mean() * 
                (648 / 4425))

    kuiper_cap = sat[sat['constellation'] == 'Kuiper']
    kuiper_cap = (kuiper_cap['capacity_per_single_satellite_mbps'].mean() *  
                (3236 / 4425) * 0.8)

    geo_cap = sat[sat['constellation'] == 'GEO']
    geo_cap = (geo_cap['capacity_per_single_satellite_mbps'].mean()) * 0.08

    uncov_population = os.path.join(DATA_SSA, 'SSA_poor_unconnected.csv')
    ssa = os.path.join(DATA_SSA, 'SSA_subregional_population_deciles.csv')
    df1 = pd.read_csv(ssa)
    df1 = df1[['GID_2', 'decile']]
    df1 = df1.rename(columns = {'GID_2': 'GID_1'})

    df = pd.read_csv(uncov_population)
    df = df[df['technology'] == 'GSM']
    df = df[df['poverty_range'] == 'GSAP2_poor']
    df = df[['iso3', 'GID_1', 'poor_unconnected']]
    df = df.groupby(['iso3', 'GID_1']).agg({'poor_unconnected': 'mean'}).reset_index()
    df = pd.merge(df, df1, on = 'GID_1', how = 'inner')

    constellations = ['Starlink', 'OneWeb', 'Kuiper', 'GEO']

    dfs = []
    for constellation in constellations:

        df_copy = df.copy()
        
        df_copy['constellation'] = constellation
        dfs.append(df_copy)

    df = pd.concat(dfs, ignore_index = True)
    df[['number_of_sats', 'capacity_mbps', 'user_capacity_mbs_per_user']] = ''

    for i in range(len(df)):

        df['number_of_sats'].loc[i] = decile_satellite(df['decile'].loc[i])

        if df['constellation'].loc[i] == 'Starlink':

            sat_cap = starlink_cap

        elif df['constellation'].loc[i] == 'OneWeb':

            sat_cap =  oneweb_cap

        elif df['constellation'].loc[i] == 'Kuiper':

            sat_cap = kuiper_cap

        else:

            sat_cap = (geo_cap / df['number_of_sats'].loc[i])

        df['capacity_mbps'].loc[i] = df['number_of_sats'].loc[i] * sat_cap
        df['user_capacity_mbs_per_user'].loc[i] = (df['capacity_mbps'].loc[i] / 
                                                df['poor_unconnected'].loc[i])

    fileout = 'satellite_coverage.csv'
    path_out = os.path.join(DATA_SSA, fileout)
    df.to_csv(path_out)

    return None

if __name__ == '__main__':

    print('Calculating satellite capacity per area')
    satellite_coverage()