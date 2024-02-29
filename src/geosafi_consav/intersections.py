import os
import warnings
import configparser
import geopandas as gpd
import pandas as pd
pd.options.mode.chained_assignment = None
warnings.filterwarnings('ignore')

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']

DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH, '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')


class IntersectLayers:

    """
    This class intersect all the processed layers.
    """

    def __init__(self, country_iso3, cell_gen):
        """
        A class constructor

        Arguments
        ---------
        country_iso3 : string
            Country iso3 to be processed..
        cell_gen : string
            Cellphone technology. It can only be 
            'GSM', '3G' or '4G'.
        flood_file : string
            Name of the flood layer containing 
            the scenario and period information
        """
        self.country_iso3 = country_iso3
        self.cell_gen = cell_gen


    def pop_coverage(self):

        population_folder = os.path.join(DATA_PROCESSED, self.country_iso3, 'population', 'shapefiles')
        coverage_folder = os.path.join(DATA_PROCESSED, self.country_iso3, 'uncovered', self.cell_gen)
        folder_out_1 = os.path.join(DATA_RESULTS, self.country_iso3, 'pop_connected')
        if not os.path.exists(folder_out_1):

            os.makedirs(folder_out_1)

        IntersectLayers.intersect_coverage(self, population_folder, coverage_folder, folder_out_1)

        return None

    
    
    def intersect_coverage(self, folder_1, folder_2, folder_out):

        print('Processing {} coverage results {}'.format(
            self.cell_gen, self.country_iso3))
        
        for firstfile in os.listdir(folder_1):
            
            try:

                if firstfile.endswith('.shp'):

                    first_shapefile = os.path.join(folder_1, firstfile)
                    first_gdf = gpd.read_file(first_shapefile)

                    for secondfile in os.listdir(folder_2):

                        if secondfile.endswith('.shp'):

                            second_shapefile = os.path.join(folder_2, secondfile)
                            second_gdf = gpd.read_file(second_shapefile)

                            if firstfile in secondfile:

                                intersection = gpd.overlay(first_gdf, second_gdf, 
                                                           how = 'intersection')
                                
                                region_part = str(firstfile)
                                cell_type = str(self.cell_gen)
                        
                                filename = cell_type + '_{}'.format(region_part)
                                
                                if not os.path.exists(folder_out):

                                    os.makedirs(folder_out)

                                path_out = os.path.join(folder_out, filename)

                                intersection.to_file(path_out, driver = 'ESRI Shapefile')
                                
            except:

                pass


        return None