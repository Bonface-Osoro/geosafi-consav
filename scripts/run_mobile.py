"""
Simulation run script for mobile broadband.

Written by Bonface Osoro & Ed Oughton.

September 2024

"""
import configparser
import os
import math
import time
import pandas as pd
import geosafi_consav.mobile as mb
from mobile_inputs import lut
from tqdm import tqdm
pd.options.mode.chained_assignment = None 

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
RESULTS = os.path.join(BASE_PATH, '..', 'results', 'cellular')


def run_uq_processing_capacity():
    """
    Run the UQ inputs through the mobile broadband model. 
    
    """
    path = os.path.join(RESULTS, 'uq_parameters_capacity.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_capacity.csv')

    df = pd.read_csv(path)
    df = df.to_dict('records')

    results = []

    for item in tqdm(df, desc = "Processing uncertainty mobile results"):

        random_variation = mb.generate_log_normal_dist_value(
            item['frequency_mhz'], item['mu'], item['sigma'], 
            item['seed_value'], item['draws'])
        
        hk_rural_correction_db = mb.hk_rural_correction_model(
            item['frequency_mhz'])
        
        hk_city_correction_db = mb.hk_city_correction_model(
            item['frequency_mhz'], item['user_antenna_height_m'])
        
        path_loss_db = mb.hk_path_loss_model(item['frequency_mhz'], 
                item['transmitter_height_m'], item['user_antenna_height_m'], 
                item['trans_user_dist_km'], item['iteration'], random_variation)

        received_power_db = mb.calc_power_received(item['transmitter_power_dbm'], 
                        item['trans_antenna_gain_dbi'], path_loss_db, 
                        item['shadow_fading_db'], 
                        item['building_penetration_loss_db'])
        
        noise_db = mb.calc_noise(item['channel_bandwidth_mhz'])
        
        cnr_db = mb.calc_cnr(received_power_db, noise_db, 
                             item['user_antenna_gain_dbi'], 
                             item['user_antenna_loss_db'], 
                             item['interference_db'])
        
        spectral_efficiency_bpshz = mb.get_spectral_efficiency(lut, 
                                    item['cell_generation'], cnr_db)
        
        channel_capacity_mbps = (mb.calc_channel_capacity(
            spectral_efficiency_bpshz, item['channel_bandwidth_mhz']))
        
        results.append({
            'cell_generation' : item['cell_generation'],
            'frequency_mhz' : item['frequency_mhz'],
            'trans_user_dist_km' : item['trans_user_dist_km'],
            'hk_rural_correction_db' : hk_rural_correction_db,
            'hk_city_correction_db' : hk_city_correction_db,
            'path_loss_db' : path_loss_db,
            'received_power_db' : received_power_db,
            'noise_db' : noise_db,
            'network_load_perc' : item['network_load'],
            'cnr_db' : cnr_db,
            'spectral_efficiency_bpshz' : spectral_efficiency_bpshz,
            'channel_capacity_mbps' : channel_capacity_mbps
        })

        df = pd.DataFrame.from_dict(results)

        filename = 'mobile_capacity_results.csv'
        
        if not os.path.exists(RESULTS):

            os.makedirs(RESULTS)

        path_out = os.path.join(RESULTS, filename)
        df.to_csv(path_out, index = False)


    return


def run_uq_processing_cost():
    """
    Run the UQ inputs through the mobile broadband model. 
    
    """
    path = os.path.join(RESULTS, 'uq_parameters_cost.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_cost.csv')

    df = pd.read_csv(path)
    df = df.to_dict('records')

    results = []

    for item in tqdm(df, desc = "Processing uncertainty mobile cost results"):
        
        equipment_cost_usd = mb.equipment_cost(item['sector_antenna_usd'],
                item['remote_radio_unit_usd'], item['io_fronthaul_usd'],
                item['control_unit_usd'], item['cooling_fans_usd'], 
                item['battery_power_usd'], item['bbu_cabinet_usd'], 
                item['tower_usd'], item['civil_materials_usd'], 
                item['router_usd'])
        
        spectrum_cost_usd = mb.spectrum_cost(item['frequency_mhz'], 10000, 0.03)

        capex_cost_usd = mb.capex_cost(equipment_cost_usd, spectrum_cost_usd,
                item['installation_usd'], item['transportation_usd'])
        
        opex_cost_usd = mb.opex_cost(item['site_rental_usd'], 
                item['base_station_energy_usd'], item['staff_costs_usd'], 
                item['sector_antenna_usd'], item['remote_radio_unit_usd'], 
                item['bbu_cabinet_usd'], item['router_usd'], 
                item['fiber_link_usd'])
        
        total_cost_ownership = mb.total_cost_ownership(capex_cost_usd, 
                opex_cost_usd, item['discount_rate'], item['assessment_years'])
        
        results.append({
            'cell_generation' : item['cell_generation'],
            'frequency_mhz' : item['frequency_mhz'],
            'equipment_cost_usd' : equipment_cost_usd,
            'spectrum_cost_usd' : spectrum_cost_usd,
            'capex_cost_usd' : capex_cost_usd,
            'opex_cost_usd' : opex_cost_usd,
            'total_cost_ownership' : total_cost_ownership
        })

        df = pd.DataFrame.from_dict(results)

        filename = 'mobile_cost_results.csv'
        
        if not os.path.exists(RESULTS):

            os.makedirs(RESULTS)

        path_out = os.path.join(RESULTS, filename)
        df.to_csv(path_out, index = False)


    return

run_uq_processing_capacity()