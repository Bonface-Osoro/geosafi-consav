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
from concurrent.futures import ThreadPoolExecutor, as_completed
import concurrent.futures
pd.options.mode.chained_assignment = None 

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
RESULTS = os.path.join(BASE_PATH, '..', 'results', 'cellular')
SSA_DATA = os.path.join(BASE_PATH, '..', 'results', 'SSA')
RAW_DATA = os.path.join(BASE_PATH, '..', 'data', 'raw', 'tower')


def process_capacity_item(item):
    """
    Helper function to process each item in parallel.
    """
    #user_demand = mb.user_demand(item['mean_monthly_demand_GB'], 
                #item['traffic_busy_hour'], item['smartphone_penetration'], 1, 1) 
                #item['mean_poor_connected'], item['mean_area_sqkm'])
    
    random_variation = mb.generate_log_normal_dist_value(
        item['frequency_MHz'], item['mu'], item['sigma'], 
        item['seed_value'], item['draws'])
    
    hk_rural_correction_db = mb.hk_rural_correction_model(
        item['frequency_MHz'])
    
    hk_city_correction_db = mb.hk_city_correction_model(
        item['frequency_MHz'], item['user_antenna_height_m'])
    
    intersite_distance_km = mb.calc_signal_path(item['transmitter_x'], 
        item['transmitter_y'], item['receiver_x'], item['receiver_y'])
    
    interference_signal_path_km = mb.calc_signal_path(item['interference_x'], 
        item['interference_y'], item['receiver_x'], item['receiver_y'])
    
    path_loss_db = mb.hk_path_loss_model(item['frequency_MHz'], 
            item['transmitter_height_m'], item['user_antenna_height_m'], 
            intersite_distance_km, item['iteration'], random_variation)
    
    int_path_loss_db = mb.hk_path_loss_model(item['frequency_MHz'], 
            item['transmitter_height_m'], item['user_antenna_height_m'], 
            interference_signal_path_km, item['iteration'], random_variation)

    received_power_db = mb.calc_power_received(item['transmitter_power_dbm'], 
                    item['trans_antenna_gain_dbi'], path_loss_db, 
                    item['shadow_fading_db'], 
                    item['building_penetration_loss_db'])

    int_received_power_db = mb.calc_power_received(item['transmitter_power_dbm'], 
                    item['trans_antenna_gain_dbi'], int_path_loss_db, 
                    item['shadow_fading_db'], 
                    item['building_penetration_loss_db'])

    cell_generation = mb.system_type(item['frequency_MHz'])

    channel_bandwidth_mhz = mb.bandwidth(cell_generation)

    noise_db = mb.calc_noise(item['frequency_MHz'], channel_bandwidth_mhz)

    interference_db = mb.calc_interference(int_received_power_db, noise_db,
                item['user_antenna_gain_dbi'], item['user_antenna_loss_db'])
    
    sinr_db = mb.calc_sinr(received_power_db, noise_db, 
                         item['user_antenna_gain_dbi'], 
                         item['user_antenna_loss_db'], 
                         interference_db)
    
    spectral_efficiency_bpshz = mb.get_spectral_efficiency(lut, 
                                        cell_generation, sinr_db)
    
    capacity_mbps = mb.calc_capacity(spectral_efficiency_bpshz, 
                channel_bandwidth_mhz, item['antenna_sectors']) * 3
    
    site_area_sqkm = mb.calc_site_area(intersite_distance_km)

    capacity_mbps_km2 = mb.calc_area_capacity(capacity_mbps, site_area_sqkm)
    
    return {
        'transmitter_x' : item['transmitter_x'],
        'transmitter_y' : item['transmitter_y'],
        'receiver_x' : item['receiver_x'],
        'receiver_y' : item['receiver_y'],
        'cell_generation' : cell_generation,
        'frequency_mhz' : item['frequency_MHz'],
        'intersite_distance_km' : intersite_distance_km,
        'interference_signal_path_km' : interference_signal_path_km,
        'hk_rural_correction_db' : hk_rural_correction_db,
        'hk_city_correction_db' : hk_city_correction_db,
        'path_loss_db' : path_loss_db,
        'int_path_loss_db' : int_path_loss_db,
        'received_power_db' : received_power_db,
        'noise_db' : noise_db,
        'interference_db' : interference_db,
        'sinr_db' : sinr_db,
        'spectral_efficiency_bpshz' : spectral_efficiency_bpshz,
        'capacity_mbps' : capacity_mbps,
        'site_area_sqkm' : site_area_sqkm,
        'capacity_mbps_km2' : capacity_mbps_km2,
        'mean_monthly_demand_GB' : item['mean_monthly_demand_GB'],
        'traffic_busy_hour' : item['traffic_busy_hour'],
        'channel_bandwidth_mhz' : channel_bandwidth_mhz,
        'smartphone_penetration' : item['smartphone_penetration'],
        'decile' : item['decile']
    }


def run_uq_processing_capacity():
    """
    Run the UQ inputs through the mobile broadband model in parallel.
    """
    path = os.path.join(RESULTS, 'uq_parameters_capacity.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_capacity.csv')
        return

    df = pd.read_csv(path)
    df = df.to_dict('records')  

    results = []
    

    with ThreadPoolExecutor() as executor:
    
        futures = {executor.submit(process_capacity_item, item): item for item in df}
        
        for future in tqdm(as_completed(futures), total=len(futures), 
                           desc="Processing uncertainty mobile results"):
            
            result = future.result()  
            results.append(result)

    df_results = pd.DataFrame(results)

    filename = 'mobile_capacity_results.csv'
    if not os.path.exists(RESULTS):
        os.makedirs(RESULTS)

    path_out = os.path.join(RESULTS, filename)
    df_results.to_csv(path_out, index=False)


    return None


def process_cost_item(item):
    """
    Process a single item to compute various costs.
    This is the function that will be run in parallel for each item.
    """
    equipment_cost_usd = mb.equipment_cost(item['sector_antenna_usd'],
            item['remote_radio_unit_usd'], item['io_fronthaul_usd'],
            item['control_unit_usd'], item['cooling_fans_usd'], 
            item['battery_power_usd'], item['bbu_cabinet_usd'], 
            item['tower_usd'], item['civil_materials_usd'], 
            item['router_usd'])

    spectrum_cost_usd = mb.spectrum_cost(item['channel_bandwidth_mhz'], 
            item['mean_poor_connected'], item['mhz_per_pop_usd'])

    capex_cost_usd = mb.capex_cost(equipment_cost_usd, spectrum_cost_usd,
            item['installation_usd'], item['transportation_usd'])

    opex_cost_usd = mb.opex_cost(item['site_rental_usd'], 
            item['base_station_energy_usd'], item['staff_costs_usd'], 
            item['sector_antenna_usd'], item['remote_radio_unit_usd'], 
            item['bbu_cabinet_usd'], item['router_usd'], 
            item['fiber_link_usd'])

    total_cost_ownership = mb.total_cost_ownership(capex_cost_usd, 
            opex_cost_usd, item['discount_rate'], item['assessment_years'])
    
    total_decile_tco_usd = total_cost_ownership * item['no_of_required_sites']

    result = {
        'cell_generation': item['cell_generation'],
        'frequency_mhz': item['channel_bandwidth_mhz'],
        'equipment_cost_usd': equipment_cost_usd,
        'spectrum_cost_usd': spectrum_cost_usd,
        'capex_cost_usd': capex_cost_usd,
        'opex_cost_usd': opex_cost_usd,
        'total_base_station_tco_usd': total_cost_ownership,
        'total_decile_tco_usd' : total_decile_tco_usd,
        'total_poor_unconnected' : item['total_poor_unconnected'],
        'mean_area_sqkm': item['mean_area_sqkm'],
        'mean_poor_connected': item['mean_poor_connected'],
        'total_area_sqkm' : item['total_area_sqkm'],
        'cost_per_1GB_usd': item['cost_per_1GB_usd'],
        'monthly_income_usd': item['monthly_income_usd'],
        'cost_per_month_usd': item['cost_per_month_usd'],
        'arpu_usd': item['arpu_usd'],
        'assessment_years': item['assessment_years'],
        'adoption_rate' : item['adoption_rate'],
        'existing_tower_no' : item['existing_tower_no'],
        'number_of_sites' : item['no_of_required_sites'],
        'decile': item['decile']}

    return result


def run_uq_processing_cost():
    """
    Run the UQ inputs through the mobile broadband model. 
    Parallelized for better performance.
    """
    path = os.path.join(RESULTS, 'uq_parameters_cost.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_cost.csv')

    df = pd.read_csv(path)
    df = df.to_dict('records')

    results = []

    with concurrent.futures.ThreadPoolExecutor() as executor:
        
        results = list(tqdm(executor.map(process_cost_item, df), 
            desc="Processing uncertainty mobile cost results", total=len(df)))

    df_results = pd.DataFrame(results)

    filename = 'mobile_cost_results.csv'

    if not os.path.exists(RESULTS):
        os.makedirs(RESULTS)

    path_out = os.path.join(RESULTS, filename)
    df_results.to_csv(path_out, index=False)


    return None


def process_emission_item(item):
    """
    Process a single item (row) for the emissions calculations.
    This function will be executed in parallel for each row in the dataframe.
    """
    lca_mfg = mb.lca_manufacturing(item['bbu_rru_pcb_kg'], 
                    item['bbu_rru_aluminium_kg'], item['copper_antenna_kg'], 
                    item['aluminium_antenna_kg'], item['pvc_antenna_kg'],
                    item['iron_antenna_kg'], item['steel_antenna_kg'], 
                    item['steel_tower_kg'], item['aluminium_frame_kg'], 
                    item['steel_pole_kg'], item['machine_concrete_kg'], 
                    item['machine_steel_kg'], item['basic_aluminium_device_kg'], 
                    item['pcb_kg_co2e'], item['aluminium_kg_co2e'], 
                    item['copper_kg_co2e'], item['pvc_kg_co2e'], 
                    item['iron_kg_co2e'], item['steel_kg_co2e'], 
                    item['concrete_kg_co2e'], item['smartphone_kg'],
                    item['ict_equipment_kg'], item['power_supply_kg'],
                    item['lithium_battery_kg'], item['mean_poor_connected'])

    aluminium_mfg_ghg = lca_mfg['aluminium_ghg']
    steel_iron_mfg_ghg = lca_mfg['steel_iron_ghg']
    concrete_mfg_ghg = lca_mfg['concrete_ghg']
    plastics_mfg_ghg = lca_mfg['plastics_ghg']
    other_metals_mfg_ghg = lca_mfg['other_metals_ghg']

    total_mfg_ghg = (aluminium_mfg_ghg + steel_iron_mfg_ghg + concrete_mfg_ghg 
                     + plastics_mfg_ghg + other_metals_mfg_ghg)    
    total_mfg_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                    total_mfg_ghg, item['no_of_required_sites'])

    lca_trans = mb.lca_transportation(item['mean_distance_km'], 
                                      item['consumption_lt_per_km'], 
                                      item['diesel_factor_kgco2e'],
                                      item['maritime_km'],
                                      item['container_ship_kgco2e'])
    total_trans_ghg_kg = lca_trans['trans_ghg_kg']
    total_trans_ghg_kg = mb.phase_emission_ghg(item['cell_generation'], 
                        total_trans_ghg_kg, item['no_of_required_sites'])

    lca_constr = mb.lca_construction(item['machine_fuel_eff_lt_per_hr'], 
                                     item['machine_operation_hrs'], 
                                     item['diesel_factor_kgco2e'])
    total_construction_ghg = lca_constr['construction_ghg']
    total_construction_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_construction_ghg, item['no_of_required_sites'])

    lca_ops = mb.lca_operations(item['smartphone_kwh'], item['ict_kwh'],
                                item['base_band_unit_kwh'], 
                                item['mean_poor_connected'], 
                                item['radio_frequency_kwh'],
                                item['epc_center_kwh'], 
                                item['number_epc_centers'],
                                item['electricity_kg_co2e'],
                                item['no_of_required_sites'])
    total_operations_ghg = lca_ops['operations_ghg']
    total_operations_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                        total_operations_ghg, item['no_of_required_sites'])

    lca_eolts = mb.lca_eolt(item['bbu_rru_pcb_kg'], item['bbu_rru_aluminium_kg'], 
                            item['copper_antenna_kg'], 
                            item['aluminium_antenna_kg'], item['pvc_antenna_kg'],
                            item['iron_antenna_kg'], item['steel_antenna_kg'], 
                            item['steel_tower_kg'], item['aluminium_frame_kg'], 
                            item['steel_pole_kg'], item['machine_steel_kg'], 
                            item['basic_aluminium_device_kg'], 
                            item['metals_factor_kgco2e'], 
                            item['plastics_factor_kgco2e'])
    
    aluminium_eolt_ghg = lca_eolts['aluminium_ghg']
    steel_iron_eolt_ghg = lca_eolts['steel_iron_ghg']
    plastics_eolt_ghg = lca_eolts['plastics_ghg']
    other_metals_eolt_ghg = lca_eolts['other_metals_ghg']

    total_eolt_ghg = (aluminium_eolt_ghg + steel_iron_eolt_ghg 
                      + plastics_eolt_ghg + other_metals_eolt_ghg)
    
    total_eolt_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                    total_eolt_ghg, item['no_of_required_sites'])

    total_emissions_ghg_kg = (total_mfg_ghg + total_trans_ghg_kg 
                        + total_construction_ghg + total_operations_ghg 
                        + total_eolt_ghg) 
    
    result = {
        'cell_generation': item['cell_generation'],
        'aluminium_mfg_ghg_kg': aluminium_mfg_ghg,
        'steel_iron_mfg_ghg_kg': steel_iron_mfg_ghg,
        'concrete_mfg_ghg_kg': concrete_mfg_ghg,
        'plastics_mfg_ghg_kg': plastics_mfg_ghg,
        'other_metals_mfg_ghg_kg': other_metals_mfg_ghg,
        'aluminium_eolt_ghg': aluminium_eolt_ghg,
        'steel_iron_eolt_ghg': steel_iron_eolt_ghg,
        'plastics_eolt_ghg': plastics_eolt_ghg,
        'other_metals_eolt_ghg': other_metals_eolt_ghg,
        'total_mfg_ghg': total_mfg_ghg,
        'total_trans_ghg_kg': total_trans_ghg_kg,
        'total_construction_ghg_kg': total_construction_ghg,
        'total_operations_ghg_kg': total_operations_ghg,
        'total_eolt_ghg_kg': total_eolt_ghg,
        'total_emissions_ghg_kg': total_emissions_ghg_kg,
        'total_population' : item['total_population'],
        'total_poor_unconnected': item['total_poor_unconnected'],
        'mean_poor_connected': item['mean_poor_connected'],
        'social_carbon_cost_usd' : item['social_carbon_cost_usd'],
        'no_of_required_sites': item['no_of_required_sites'],
        'assessment_period': item['assessment_period'],
        'decile': item['decile']
    }

    return result


def run_uq_processing_emission():
    """
    Run the UQ inputs through the mobile broadband model.
    Parallelized for better performance.
    """
    path = os.path.join(RESULTS, 'uq_parameters_emission.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_emission.csv')

    df = pd.read_csv(path)
    df['number_epc_centers'] = (df['mean_poor_connected'] * 1 / 150280)
    df = df.to_dict('records')

    with concurrent.futures.ThreadPoolExecutor() as executor:

        results = list(tqdm(executor.map(process_emission_item, df), 
                desc ="Processing uncertainty mobile results", total = len(df)))

    df_results = pd.DataFrame(results)

    filename = 'mobile_emission_results.csv'

    if not os.path.exists(RESULTS):

        os.makedirs(RESULTS)

    path_out = os.path.join(RESULTS, filename)
    df_results.to_csv(path_out, index = False)


    return None


if __name__ == '__main__':

    #print('Running mobile broadband capacity model')
    run_uq_processing_capacity()

    #print('Running mobile broadband cost model')
    #run_uq_processing_cost()

    #print('Running mobile broadband emissions model')
    #run_uq_processing_emission()