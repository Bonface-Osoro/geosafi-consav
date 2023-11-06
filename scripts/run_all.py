import configparser
import os
import warnings
import pandas as pd
from geosafi_consav.preprocessing import ProcessCountry, ProcessRegions, ProcessPopulation
from geosafi_consav.preprocessing import PovertyProcess
from geosafi_consav.preprocessing import CoverageProcess
from geosafi_consav.generator import PointsGenerator, EdgeGenerator
from geosafi_consav.intersections import IntersectLayers
pd.options.mode.chained_assignment = None
warnings.filterwarnings('ignore')

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']

DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH, 'processed')


path = os.path.join(DATA_RAW, 'countries.csv')
pop_tif_loc = os.path.join(DATA_RAW, 'WorldPop', 'ppp_2020_1km_Aggregated.tif')
poverty_shp = os.path.join(DATA_RAW, 'poverty_data', 'GSAP2.shp')

countries = pd.read_csv(path, encoding = 'latin-1')

if __name__ == '__main__':

    for idx, country in countries.iterrows():
            
        if not country['region'] == 'Sub-Saharan Africa' or country['Exclude'] == 1:
            
        #if not country['iso3'] == 'BFA':
            
            continue 

        country = ProcessCountry(path, countries['iso3'].loc[idx])
        #country.process_country_shapes()

        regions = ProcessRegions(countries['iso3'].loc[idx], countries['lowest'].loc[idx])
        #regions.process_regions()
        #regions.process_sub_region_boundaries()

        populations = ProcessPopulation(path, countries['iso3'].loc[idx], countries['lowest'].loc[idx], pop_tif_loc)
        #populations.process_national_population()
        #populations.process_population_tif()
        #populations.process_sub_regional_pop_tiff()
        #populations.pop_process_shapefiles()

        poverty = PovertyProcess(path, countries['iso3'].loc[idx], countries['lowest'].loc[idx], poverty_shp)
        #poverty.country_poverty()

        coverage = CoverageProcess(path, countries['iso3'].loc[idx])
        #coverage.process_national_coverage()
        #coverage.process_regional_coverage()
        techs = ['GSM', '3G', '4G']
        for tech in techs:

            intersection = IntersectLayers(countries['iso3'].loc[idx], tech)
            intersection.pop_coverage()

        points_generator = PointsGenerator(countries['iso3'].loc[idx])
        #points_generator.generate_gid_points()

        edges_generator = EdgeGenerator(countries['iso3'].loc[idx])
        #edges_generator.fit_regional_node_edges()
        #edges_generator.fit_country_node_edges()