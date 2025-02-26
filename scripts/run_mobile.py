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

        base_station_number = [item['no_of_4g_base_stations'], 
                               item['no_of_5g_base_stations']]
        
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
        
        base_station_capacity_mbps = ((mb.base_station(
            item['cell_generation'], channel_capacity_mbps, base_station_number)
            ) * item['antenna_sectors']) * item['subcarriers']
        
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
            'channel_capacity_mbps' : channel_capacity_mbps,
            'base_station_capacity_mbps' : base_station_capacity_mbps,
            'total_poor_unconnected' : item['total_poor_unconnected'],
            'mean_poor_connected' : item['mean_poor_connected'],
            'mean_area_sqkm' : item['mean_area_sqkm'],
            'no_of_4g_base_stations' : item['no_of_4g_base_stations'],
            'no_of_5g_base_stations' : item['no_of_5g_base_stations'],
            'decile' : item['decile']
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

        base_station_number = [item['no_of_4g_base_stations'], 
                               item['no_of_5g_base_stations']]
        
        equipment_cost_usd = mb.equipment_cost(item['sector_antenna_usd'],
                item['remote_radio_unit_usd'], item['io_fronthaul_usd'],
                item['control_unit_usd'], item['cooling_fans_usd'], 
                item['battery_power_usd'], item['bbu_cabinet_usd'], 
                item['tower_usd'], item['civil_materials_usd'], 
                item['router_usd'])
        
        spectrum_cost_usd = mb.spectrum_cost(item['frequency_mhz'], 
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
        
        total_base_station_tco = mb.base_station(item['cell_generation'], 
                            total_cost_ownership, base_station_number)
        
        results.append({
            'cell_generation' : item['cell_generation'],
            'frequency_mhz' : item['frequency_mhz'],
            'equipment_cost_usd' : equipment_cost_usd,
            'spectrum_cost_usd' : spectrum_cost_usd,
            'capex_cost_usd' : capex_cost_usd,
            'opex_cost_usd' : opex_cost_usd,
            'total_cost_ownership' : total_cost_ownership,
            'total_base_station_tco_usd' : total_base_station_tco,
            'total_poor_unconnected' : item['total_poor_unconnected'],
            'mean_poor_connected' : item['mean_poor_connected'],
            'cost_per_1GB_usd' : item['cost_per_1GB_usd'],
            'monthly_income_usd' : item['monthly_income_usd'],
            'cost_per_month_usd' : item['cost_per_month_usd'],
            'adoption_rate_perc' : item['adoption_rate_perc'],
            'arpu_usd' : item['arpu_usd'],
            'no_of_4g_base_stations' : item['no_of_4g_base_stations'],
            'no_of_5g_base_stations' : item['no_of_5g_base_stations'],
            'assessment_years' : item['assessment_years'],
            'decile' : item['decile']
        })

        df = pd.DataFrame.from_dict(results)

        filename = 'mobile_cost_results.csv'
        
        if not os.path.exists(RESULTS):

            os.makedirs(RESULTS)

        path_out = os.path.join(RESULTS, filename)
        df.to_csv(path_out, index = False)


    return


def run_uq_processing_emission():
    """
    Run the UQ inputs through the mobile broadband model.

    Parameters
    ----------
    users : int.
        Number of users served by the base station.
    
    """
    path = os.path.join(RESULTS, 'uq_parameters_emission.csv') 

    if not os.path.exists(path):
        print('Cannot locate uq_parameters_emission.csv')

    df = pd.read_csv(path)
    df['number_epc_centers'] = ''
    df['number_epc_centers'] = (df['mean_poor_connected'] * 1 / 150280)
    df = df.to_dict('records')

    results = []

    for item in tqdm(df, desc = "Processing uncertainty mobile results"):

        base_station_number = [item['no_of_4g_base_stations'], 
                               item['no_of_5g_base_stations']]

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
        
        aluminium_mfg_ghg = (lca_mfg['aluminium_ghg'])
        steel_iron_mfg_ghg = (lca_mfg['steel_iron_ghg'])
        concrete_mfg_ghg = (lca_mfg['concrete_ghg'])
        plastics_mfg_ghg = (lca_mfg['plastics_ghg'])
        other_metals_mfg_ghg = (lca_mfg['other_metals_ghg'])

        total_mfg_ghg = (aluminium_mfg_ghg + steel_iron_mfg_ghg 
                        + concrete_mfg_ghg + plastics_mfg_ghg 
                        + other_metals_mfg_ghg)    
        total_mfg_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_mfg_ghg, base_station_number)
        
        lca_trans = mb.lca_transportation(item['mean_distance_km'], 
                                          item['consumption_lt_per_km'], 
                                          item['diesel_factor_kgco2e'],
                                          item['maritime_km'],
                                          item['container_ship_kgco2e'])
        
        total_trans_ghg_kg = (lca_trans['trans_ghg_kg'])
        total_trans_ghg_kg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_trans_ghg_kg, base_station_number)

        lca_constr = mb.lca_construction(item['machine_fuel_eff_lt_per_hr'], 
                                         item['machine_operation_hrs'], 
                                         item['diesel_factor_kgco2e'])
        
        total_construction_ghg = (lca_constr['construction_ghg'])
        total_construction_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_construction_ghg, base_station_number)
        
        lca_ops = mb.lca_operations(item['smartphone_kwh'], item['ict_kwh'],
                                    item['base_band_unit_kwh'], 
                                    item['mean_poor_connected'], 
                                    item['radio_frequency_kwh'],
                                    item['epc_center_kwh'], 
                                    item['cell_generation'],
                                    item['number_epc_centers'],
                                    item['electricity_kg_co2e'],
                                    base_station_number)
        
        total_operations_ghg = (lca_ops['operations_ghg'])
        total_operations_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_operations_ghg, base_station_number)

        lca_eolts = mb.lca_eolt(item['bbu_rru_pcb_kg'], 
                    item['bbu_rru_aluminium_kg'], item['copper_antenna_kg'], 
                    item['aluminium_antenna_kg'], item['pvc_antenna_kg'],
                    item['iron_antenna_kg'], item['steel_antenna_kg'], 
                    item['steel_tower_kg'], item['aluminium_frame_kg'], 
                    item['steel_pole_kg'], item['machine_steel_kg'], 
                    item['basic_aluminium_device_kg'], 
                    item['metals_factor_kgco2e'], 
                    item['plastics_factor_kgco2e'])

        aluminium_eolt_ghg = (lca_eolts['aluminium_ghg'])
        steel_iron_eolt_ghg = (lca_eolts['steel_iron_ghg'])
        plastics_eolt_ghg = (lca_eolts['plastics_ghg'])
        other_metals_eolt_ghg = (lca_eolts['other_metals_ghg'])

        total_eolt_ghg = (aluminium_eolt_ghg + steel_iron_eolt_ghg 
                         + plastics_eolt_ghg + other_metals_eolt_ghg)
        total_eolt_ghg = mb.phase_emission_ghg(item['cell_generation'], 
                            total_eolt_ghg, base_station_number)
        
        total_emissions_ghg_kg = (total_mfg_ghg + total_trans_ghg_kg 
                        + total_construction_ghg + total_operations_ghg 
                        + total_eolt_ghg) 
        
        results.append({
            'cell_generation' : item['cell_generation'],
            'aluminium_mfg_ghg_kg' : aluminium_mfg_ghg,
            'steel_iron_mfg_ghg_kg' : steel_iron_mfg_ghg,
            'concrete_mfg_ghg_kg' : concrete_mfg_ghg,
            'plastics_mfg_ghg_kg' : plastics_mfg_ghg,
            'other_metals_mfg_ghg_kg' : other_metals_mfg_ghg,
            'aluminium_eolt_ghg' : aluminium_eolt_ghg,
            'steel_iron_eolt_ghg' : steel_iron_eolt_ghg,
            'plastics_eolt_ghg' : plastics_eolt_ghg,
            'other_metals_eolt_ghg' : other_metals_eolt_ghg,
            'total_mfg_ghg' : total_mfg_ghg,
            'total_trans_ghg_kg' : total_trans_ghg_kg,
            'total_construction_ghg_kg' : total_construction_ghg,
            'total_operations_ghg_kg' : total_operations_ghg,
            'total_eolt_ghg_kg' : total_eolt_ghg,
            'total_emissions_ghg_kg' : total_emissions_ghg_kg,
            'total_poor_unconnected' : item['total_poor_unconnected'],
            'mean_poor_connected' : item['mean_poor_connected'],
            'no_of_4g_base_stations' : item['no_of_4g_base_stations'],
            'no_of_5g_base_stations' : item['no_of_5g_base_stations'],
            'social_carbon_cost_usd' : item['social_carbon_cost_usd'],
            'assessment_period' : item['assessment_period'],
            'decile' : item['decile']
        })

        df = pd.DataFrame.from_dict(results)

        filename = 'mobile_emission_results.csv'
        
        if not os.path.exists(RESULTS):

            os.makedirs(RESULTS)

        path_out = os.path.join(RESULTS, filename)
        df.to_csv(path_out, index = False)


    return


if __name__ == '__main__':

    print('Running mobile broadband capacity model')
    #run_uq_processing_capacity()

    print('Running mobile broadband cost model')
    #run_uq_processing_cost()

    print('Running mobile broadband emissions model')
    run_uq_processing_emission()