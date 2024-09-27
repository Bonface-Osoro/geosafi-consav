"""
Preprocess all Uncertainty Quantification (UQ) inputs. 

Written by Bonface Osoro & Ed Oughton.

September 2024

"""
import configparser
import os
import random
import numpy as np
import pandas as pd
from mobile_inputs import parameters
from geosafi_consav.mobile import generate_log_normal_dist_value
pd.options.mode.chained_assignment = None 

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results')
DATA_SSA = os.path.join(BASE_PATH, '..', 'results', 'SSA')

deciles = ['Decile 1', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5',
           'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 'Decile 10']

def multigeneration_cell_capacity(i, mobile_params):
    """
    This function generates random values within the given parameter ranges. 

    Parameters
    ----------
    i : int.
        number of iterations
    mobile_params : dict
        Dictionary containing mobile engineering details

    Return
    ------
        output : list
            List containing capacity outputs

    """
    output = []

    for decile in deciles:

        for frequency in mobile_params['frequencies_mhz']:

            for network in mobile_params['network_load']:

                trans_user_dist_km = random.randint(
                    mobile_params['trans_user_dist_low_km'], 
                    mobile_params['trans_user_dist_high_km'])
                
                transmitter_height_m = random.randint(
                    mobile_params['transmitter_height_low_m'], 
                    mobile_params['transmitter_height_high_m'])
                
                user_antenna_height_m = random.randint(
                    mobile_params['user_antenna_height_low_m'], 
                    mobile_params['user_antenna_height_high_m'])
                
                transmitter_power_dbm = random.randint(
                    mobile_params['transmitter_power_low_dbm'], 
                    mobile_params['transmitter_power_high_dbm'])
                
                trans_antenna_gain_dbi = random.randint(
                    mobile_params['trans_antenna_gain_low_dbi'], 
                    mobile_params['trans_antenna_gain_high_dbi'])
                
                user_antenna_gain_dbi = random.randint(
                    mobile_params['user_antenna_gain_low_dbi'], 
                    mobile_params['user_antenna_gain_high_dbi'])
                
                user_antenna_loss_db = random.randint(
                    mobile_params['user_antenna_loss_low_db'], 
                    mobile_params['user_antenna_loss_high_db'])
                
                interference_db = random.randint(
                    mobile_params['interference_low_db'], 
                    mobile_params['interference_high_db'])

                
                output.append({
                    'iteration' : i,
                    'mu' : mobile_params['mu'],
                    'sigma' : mobile_params['sigma'],
                    'seed_value' : mobile_params['seed_value'],
                    'draws' : mobile_params['draws'],
                    'cell_generation' : mobile_params['cell_generation'],
                    'frequency_mhz' : frequency,
                    'channel_bandwidth_mhz' : mobile_params['channel_bandwidth_mhz'],
                    'transmitter_height_m' : transmitter_height_m,
                    'trans_user_dist_km' : trans_user_dist_km,
                    'user_antenna_height_m' : user_antenna_height_m,
                    'transmitter_power_dbm' : transmitter_power_dbm,
                    'trans_antenna_gain_dbi' : trans_antenna_gain_dbi,
                    'user_antenna_gain_dbi' : user_antenna_gain_dbi,
                    'user_antenna_loss_db' : user_antenna_loss_db,
                    'interference_db' : interference_db,
                    'shadow_fading_db' : mobile_params['shadow_fading_db'],
                    'building_penetration_loss_db' : (
                        mobile_params['building_penetration_loss_db']),
                    'antenna_sectors' : mobile_params['antenna_sectors'],
                    'system_temperature_k' : mobile_params['system_temperature_k'],
                    'network_load' : network,
                    'decile' : decile
                })


    return output


def uq_inputs_capacity(parameters):
    """
    Generate all UQ capacity inputs in preparation for running through the 
    mobile broadband model. 

    Parameters
    ----------
    parameters : dict
        dictionary of dictionary containing mobile engineering values.

    """
    iterations = []

    for key, mobile_params in parameters.items():

        for i in range(0, mobile_params['iterations']):

            if key in ['4G', '5G']:
                
                data = multigeneration_cell_capacity(i, mobile_params)

            iterations = iterations + data

    df = pd.DataFrame.from_dict(iterations)

    # Import user data
    pop_path = os.path.join(DATA_SSA, 'SSA_decile_summary_stats.csv') 
    df1 = pd.read_csv(pop_path)

    filename = 'uq_parameters_capacity.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    merged_df = pd.merge(df, df1, on = 'decile')
    path_out = os.path.join(folder_out, filename)
    merged_df.to_csv(path_out, index = False)

    return


def multigeneration_cell_costs(i, mobile_params):
    """
    This function generates random values within the given parameter ranges. 

    Parameters
    ----------
    i : int.
        number of iterations
    mobile_params : dict
        Dictionary containing mobile engineering details

    Return
    ------
        output : list
            List containing cost outputs

    """
    output = []

    for decile in deciles:

        for usd_mhz_pop in mobile_params['usd_per_mhz_pop']:

            for frequency in mobile_params['frequencies_mhz']:

                sector_antenna = random.randint(mobile_params['sector_antenna_low'], 
                    mobile_params['sector_antenna_high'])
                
                remote_radio_unit = random.randint(mobile_params['remote_radio_unit_low'], 
                    mobile_params['remote_radio_unit_high'])
                
                io_fronthaul = random.randint(mobile_params['io_fronthaul_low'], 
                    mobile_params['io_fronthaul_high'])
                
                control_unit = random.randint(mobile_params['control_unit_low'], 
                    mobile_params['control_unit_high'])
                
                cooling_fans = random.randint(mobile_params['cooling_fans_low'], 
                    mobile_params['cooling_fans_high'])
                
                power_supply = random.randint(mobile_params['power_supply_low'], 
                    mobile_params['power_supply_high'])
                
                battery_power = random.randint(mobile_params['battery_power_low'], 
                    mobile_params['battery_power_high'])
                
                bbu_cabinet = random.randint(mobile_params['bbu_cabinet_low'], 
                    mobile_params['bbu_cabinet_high'])
                
                tower = random.randint(mobile_params['tower_low'], 
                    mobile_params['tower_high'])
                
                civil_materials = random.randint(mobile_params['civil_materials_low'], 
                    mobile_params['civil_materials_high'])
                
                transportation = random.randint(mobile_params['transportation_low'], 
                    mobile_params['transportation_high'])
                
                installation = random.randint(mobile_params['installation_low'], 
                    mobile_params['installation_high'])
                
                site_rental = random.randint(mobile_params['site_rental_low'], 
                    mobile_params['site_rental_high'])
                
                router = random.randint(mobile_params['router_low'], 
                    mobile_params['router_high'])
                
                fiber_link = random.randint(mobile_params['fiber_link_low'], 
                    mobile_params['fiber_link_high'])
                
                base_station_energy = random.randint(mobile_params['power_supply_low'], 
                    mobile_params['power_supply_high'])
                
                staff_costs = random.randint(mobile_params['staff_costs_low'], 
                    mobile_params['staff_costs_high'])
            
                output.append({
                    'iteration' : i,
                    'mu' : mobile_params['mu'],
                    'sigma' : mobile_params['sigma'],
                    'seed_value' : mobile_params['seed_value'],
                    'draws' : mobile_params['draws'],
                    'cell_generation' : mobile_params['cell_generation'],
                    'frequency_mhz' : frequency,
                    'channel_bandwidth_mhz' : mobile_params['channel_bandwidth_mhz'],
                    'sector_antenna_usd' : sector_antenna,
                    'remote_radio_unit_usd' : remote_radio_unit,
                    'io_fronthaul_usd' : io_fronthaul,
                    'control_unit_usd' : control_unit,
                    'cooling_fans_usd' : cooling_fans,
                    'power_supply_usd' : power_supply,
                    'battery_power_usd' : battery_power,
                    'bbu_cabinet_usd' : bbu_cabinet,
                    'tower_usd' : tower,
                    'civil_materials_usd' : civil_materials,
                    'transportation_usd' : transportation,
                    'installation_usd' : installation,
                    'site_rental_usd' : site_rental,
                    'base_station_energy_usd' : base_station_energy,
                    'router_usd' : router,
                    'fiber_link_usd' : fiber_link,
                    'staff_costs_usd' : staff_costs,
                    'mhz_per_pop_usd' : usd_mhz_pop,
                    'assessment_years' : mobile_params['assessment_period'],
                    'discount_rate' : mobile_params['discount_rate'],
                    'decile' : decile
                })


    return output


def uq_inputs_costs(parameters):
    """
    Generate all UQ cost inputs in preparation for running through the 
    mobile broadband model. 

    Parameters
    ----------
    parameters : dict
        dictionary of dictionary containing mobile cost values.

    """
    iterations = []

    for key, mobile_params in parameters.items():

        for i in range(0, mobile_params['iterations']):

            if key in ['4G', '5G']:
                
                data = multigeneration_cell_costs(i, mobile_params)

            iterations = iterations + data

    df = pd.DataFrame.from_dict(iterations)

    # Import user data
    pop_path = os.path.join(DATA_SSA, 'SSA_decile_summary_stats.csv') 
    df1 = pd.read_csv(pop_path)

    filename = 'uq_parameters_cost.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    merged_df = pd.merge(df, df1, on = 'decile')
    path_out = os.path.join(folder_out, filename)
    merged_df.to_csv(path_out, index = False)


    return


def multigeneration_cell_emissions(i, mobile_params):
    """
    This function generates random values within the given emission ranges. 

    Parameters
    ----------
    i : int.
        number of iterations
    mobile_params : dict
        Dictionary containing mobile engineering details

    Return
    ------
        output : list
            List containing capacity outputs

    """
    output = []

    for decile in deciles:

        bbu_rru_pcb_kg = random.randint(mobile_params['bbu_rru_pcb_low_kg'], 
            mobile_params['bbu_rru_pcb_high_kg'])
        
        bbu_rru_aluminium_kg = random.randint(
            mobile_params['bbu_rru_aluminium_low_kg'], 
            mobile_params['bbu_rru_aluminium_high_kg'])
        
        copper_antenna_kg = random.randint(
            mobile_params['copper_antenna_low_kg'], 
            mobile_params['copper_antenna_high_kg'])
        
        aluminium_antenna_kg = random.randint(
            mobile_params['aluminium_antenna_low_kg'], 
            mobile_params['aluminium_antenna_high_kg'])
        
        pvc_antenna_kg = random.randint(mobile_params['pvc_antenna_low_kg'], 
                                        mobile_params['pvc_antenna_high_kg'])
        
        iron_antenna_kg = random.randint(mobile_params['iron_antenna_low_kg'], 
                                            mobile_params['iron_antenna_high_kg'])
        
        steel_antenna_kg = random.randint(mobile_params['steel_antenna_low_kg'], 
                                            mobile_params['steel_antenna_high_kg'])
        
        steel_tower_kg = random.randint(mobile_params['steel_tower_low_kg'], 
                                        mobile_params['steel_tower_high_kg'])
        
        aluminium_frame_kg = random.randint(
            mobile_params['aluminium_frame_low_kg'], 
            mobile_params['aluminium_frame_high_kg'])
        
        steel_pole_kg = random.randint(mobile_params['steel_pole_low_kg'], 
                                        mobile_params['steel_pole_high_kg'])
        
        machine_concrete_kg = random.randint(
            mobile_params['machine_concrete_low_kg'], 
            mobile_params['machine_concrete_high_kg'])
        
        machine_steel_kg = random.randint(mobile_params['machine_steel_low_kg'], 
                                            mobile_params['machine_steel_high_kg'])
        
        basic_aluminium_device_kg = random.randint(
            mobile_params['basic_aluminium_device_low_kg'], 
            mobile_params['basic_aluminium_device_high_kg'])
        
        consumption_lt_per_km = random.uniform(
            mobile_params['consumption_low_lt_per_km'], 
            mobile_params['consumption_high_lt_per_km'])
        
        machine_fuel_eff_lt_per_hr = random.randint(
            mobile_params['machine_fuel_eff_low_lt_per_hr'], 
            mobile_params['machine_fuel_eff_high_lt_per_hr'])
        
        machine_operation_hrs = random.randint(
            mobile_params['machine_operation_low_hrs'], 
            mobile_params['machine_operation_high_hrs'])
        
        cpe_kwh = random.uniform(mobile_params['cpe_low_kwh'], 
            mobile_params['cpe_high_kwh'])
        
        base_station_power_kwh = random.uniform(
            mobile_params['base_station_power_low_kwh'], 
            mobile_params['base_station_power_high_kwh'])
        
        output.append({
            'iteration' : i,
            'cell_generation' : mobile_params['cell_generation'],
            'bbu_rru_pcb_kg' : bbu_rru_pcb_kg,
            'bbu_rru_aluminium_kg' : bbu_rru_aluminium_kg,
            'copper_antenna_kg' : copper_antenna_kg,
            'aluminium_antenna_kg' : aluminium_antenna_kg,
            'pvc_antenna_kg' : pvc_antenna_kg,
            'iron_antenna_kg' : iron_antenna_kg,
            'steel_antenna_kg' : steel_antenna_kg,
            'steel_tower_kg' : steel_tower_kg,
            'aluminium_frame_kg' : aluminium_frame_kg,
            'steel_pole_kg' : steel_pole_kg,
            'machine_concrete_kg' : machine_concrete_kg,
            'machine_steel_kg' : machine_steel_kg,
            'basic_aluminium_device_kg' : basic_aluminium_device_kg,
            'pcb_kg_co2e' : mobile_params['pcb_kg_co2e'],
            'aluminium_kg_co2e' : mobile_params['aluminium_kg_co2e'],
            'copper_kg_co2e' : mobile_params['copper_kg_co2e'],
            'pvc_kg_co2e' : mobile_params['pvc_kg_co2e'],
            'iron_kg_co2e' : mobile_params['iron_kg_co2e'],
            'steel_kg_co2e' : mobile_params['steel_kg_co2e'],
            'concrete_kg_co2e' : mobile_params['concrete_kg_co2e'],
            'olnu_kg_co2e' : mobile_params['olnu_kg_co2e'],
            'electricity_kg_co2e' : mobile_params['electricity_kg_co2e'],
            'plastics_factor_kgco2e' : mobile_params['plastics_factor_kgco2'],
            'metals_factor_kgco2e' : mobile_params['metals_factor_kgco2'],
            'diesel_factor_kgco2e' : mobile_params['diesel_factor_kgco2e'],
            'consumption_lt_per_km' : consumption_lt_per_km,
            'machine_fuel_eff_lt_per_hr' : machine_fuel_eff_lt_per_hr,
            'machine_operation_hrs' : machine_operation_hrs,
            'cpe_kwh' : cpe_kwh,
            'base_station_power_kwh' : base_station_power_kwh,
            'assessment_period' : mobile_params['assessment_period'],
            'decile' : decile,
        })


    return output


def uq_inputs_emissions(parameters):
    """
    Generate all UQ emission inputs in preparation for running through the 
    mobile broadband model. 

    Parameters
    ----------
    parameters : dict
        dictionary of dictionary containing mobile cost values.

    """
    iterations = []

    for key, mobile_params in parameters.items():

        for i in range(0, mobile_params['iterations']):

            if key in ['4G', '5G']:
                
                data = multigeneration_cell_emissions(i, mobile_params)

            iterations = iterations + data

    df = pd.DataFrame.from_dict(iterations)

    # Import user data
    pop_path = os.path.join(DATA_SSA, 'SSA_decile_summary_stats.csv') 
    df1 = pd.read_csv(pop_path)

    filename = 'uq_parameters_emission.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    merged_df = pd.merge(df, df1, on = 'decile')
    path_out = os.path.join(folder_out, filename)
    merged_df.to_csv(path_out, index = False)


    return


if __name__ == '__main__':

    print('Setting seed for consistent results')
    random.seed(10)

    print('Running uq_capacity_inputs_generator()')
    uq_inputs_capacity(parameters)

    print('Running uq_cost_inputs_generator()')
    uq_inputs_costs(parameters)

    print('Running uq_inputs_emissions_generator()')
    uq_inputs_emissions(parameters)