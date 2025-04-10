"""
Preprocess all Uncertainty Quantification (UQ) inputs. 

Written by Bonface Osoro & Ed Oughton.

September 2024

"""
import ast
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
DATA_raw = os.path.join(BASE_PATH, '..', 'data', 'raw')

deciles = ['Decile 1', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5',
           'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', 'Decile 10']


def process_mobile_data():
    """
    Prepare mobile data including spectrum, Internet traffic, smartphone 
    penetration for calculating capacity.
    """    
    ssa_mob_data = os.path.join(DATA_raw, 'mobi_data.csv')
    df = pd.read_csv(ssa_mob_data)
    df = df.loc[df['region'] == 'Sub-Saharan Africa']
    df = df[['iso3', 'adoption_low', 'traffic_hour', 'spectrum_MHz']]
    ssa_regions = os.path.join(DATA_SSA, 'SSA_subregional_population_deciles.csv')
    df1 = pd.read_csv(ssa_regions)

    df2 = pd.merge(df, df1, on = 'iso3', how = 'inner')
    df2['frequency_MHz'] = df2.apply(select_frequency, axis = 1)

    df2 = df2.groupby(['decile', 'frequency_MHz']).agg(smartphone_penetration = 
        ('adoption_low', 'mean'), traffic_busy_hour=('traffic_hour', 'mean')
        ).reset_index() 

    filename = 'SSA_mobile_data.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

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
        arpu_usd = ('arpu_usd', 'mean'),
        adoption_rate = ('adoption_rate', 'mean')).reset_index()

    filename = 'SSA_decile_summary_stats.csv'
    folder_out = os.path.join(DATA_SSA)

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)


    return None


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


        for demand in mobile_params['mean_monthly_demand_GB']:


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
            
            for _ in range(50):
                
                transmitter_x = random.uniform(0, mobile_params['grid_length'])
                transmitter_y = random.uniform(0, mobile_params['grid_length'])
                receiver_x = random.uniform(0, (mobile_params['grid_length'] + 5))
                receiver_y = random.uniform(0, (mobile_params['grid_length'] + 5))
                interceptor_x = random.uniform(0, mobile_params['grid_length'])
                interceptor_y = random.uniform(0, mobile_params['grid_length'])
       
            output.append({
                'transmitter_x': transmitter_x, 
                'transmitter_y': transmitter_y,
                'receiver_x': receiver_x,
                'receiver_y': receiver_y,
                'interference_x': interceptor_x,
                'interference_y': interceptor_y,
                'no_transmitters': mobile_params['transmitters'],
                'iteration' : i,
                'mu' : mobile_params['mu'],
                'sigma' : mobile_params['sigma'],
                'seed_value' : mobile_params['seed_value'],
                'draws' : mobile_params['draws'],
                'transmitter_height_m' : transmitter_height_m,
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
                'subcarriers' : mobile_params['subcarriers'],
                'system_temperature_k' : mobile_params['system_temperature_k'],
                'mean_monthly_demand_GB' : demand,
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
    mob_path = os.path.join(DATA_RESULTS, 'cellular', 'SSA_mobile_data.csv') 
    df1 = pd.read_csv(mob_path)

    filename = 'uq_parameters_capacity.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    merged_df = pd.merge(df, df1, on = 'decile')
    path_out = os.path.join(folder_out, filename)
    merged_df.to_csv(path_out, index = False)

    return None


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
    site_path = os.path.join(DATA_SSA, 'SSA_number_of_sites.csv')
    tower_path = os.path.join(DATA_raw, 'tower', 'GID_2_tower_locations.csv')
    region_data = os.path.join(DATA_SSA, 'SSA_subregional_population_deciles.csv')

    df1 = pd.read_csv(pop_path)
    df2 = pd.read_csv(site_path)
    df3 = pd.read_csv(tower_path)
    df4 = pd.read_csv(region_data)

    df2 = df2[['cell_generation', 'channel_bandwidth_mhz', 
               'no_of_required_sites', 'decile']]
    
    df1 = pd.merge(df1, df2, on = 'decile')

    df3 = pd.merge(df3, df4, on = 'GID_2', how = 'inner')
    df3 = df3.groupby(['decile']).agg(existing_tower_no = 
                    ('existing_tower_no', 'mean')).reset_index()
    
    df3['existing_tower_no'] = df3['existing_tower_no'].round().astype(int)

    filename = 'uq_parameters_cost.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)
    
    df = pd.merge(df, df1, on = 'decile')
    df = pd.merge(df, df3, on = 'decile', how = 'inner')
    path_out = os.path.join(folder_out, filename)
    df.to_csv(path_out, index = False)


    return None


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
        
        smartphone_kg = random.randint(
            mobile_params['smartphone_low_kg'], 
            mobile_params['smartphone_high_kg'])

        ict_equipment_kg = random.uniform(
            mobile_params['ict_equipment_low_kg'], 
            mobile_params['ict_equipment_high_kg'])
        
        power_supply_kg = random.uniform(
            mobile_params['power_supply_low_kg'], 
            mobile_params['power_supply_high_kg'])
        
        lithium_battery_kg = random.uniform(
            mobile_params['lithium_battery_low_kg'], 
            mobile_params['lithium_battery_high_kg'])

        consumption_lt_per_km = random.uniform(
            mobile_params['consumption_low_lt_per_km'], 
            mobile_params['consumption_high_lt_per_km'])
        
        machine_fuel_eff_lt_per_hr = random.randint(
            mobile_params['machine_fuel_eff_low_lt_per_hr'], 
            mobile_params['machine_fuel_eff_high_lt_per_hr'])
        
        machine_operation_hrs = random.randint(
            mobile_params['machine_operation_low_hrs'], 
            mobile_params['machine_operation_high_hrs'])

        smartphone_kwh = random.uniform(mobile_params['smartphone_low_kwh'], 
            mobile_params['smartphone_high_kwh'])
        
        ict_kwh = random.uniform(mobile_params['ict_low_kwh'], 
            mobile_params['ict_high_kwh'])
        
        base_band_unit_kwh = random.uniform(mobile_params['base_band_unit_low_kwh'], 
            mobile_params['base_band_unit_high_kwh'])
        
        radio_frequency_kwh = random.uniform(mobile_params['radio_frequency_low_kwh'], 
            mobile_params['radio_frequency_high_kwh'])
        
        epc_center_kwh = random.uniform(mobile_params['epc_center_low_kwh'], 
            mobile_params['epc_center_high_kwh'])
        
        cpe_kwh = random.uniform(mobile_params['cpe_low_kwh'], 
            mobile_params['cpe_high_kwh'])
        
        base_station_power_kwh = random.uniform(
            mobile_params['base_station_power_low_kwh'], 
            mobile_params['base_station_power_high_kwh'])
        
        output.append({
            'iteration' : i,
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
            'smartphone_kg' : smartphone_kg,
            'ict_equipment_kg' : ict_equipment_kg,
            'power_supply_kg' : power_supply_kg,
            'lithium_battery_kg' : lithium_battery_kg,
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
            'container_ship_kgco2e' : mobile_params['container_ship_kgco2e'],
            'consumption_lt_per_km' : consumption_lt_per_km,
            'machine_fuel_eff_lt_per_hr' : machine_fuel_eff_lt_per_hr,
            'machine_operation_hrs' : machine_operation_hrs,
            'cpe_kwh' : cpe_kwh,
            'smartphone_kwh' : smartphone_kwh,
            'ict_kwh' : ict_kwh,
            'base_band_unit_kwh' : base_band_unit_kwh,
            'base_station_power_kwh' : base_station_power_kwh,
            'radio_frequency_kwh' : radio_frequency_kwh,
            'epc_center_kwh' : epc_center_kwh,
            'assessment_period' : mobile_params['assessment_period'],
            'social_carbon_cost_usd' : mobile_params['social_carbon_cost_usd'],
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
    site_path = os.path.join(DATA_SSA, 'SSA_number_of_sites.csv')


    df1 = pd.read_csv(pop_path)
    df2 = pd.read_csv(site_path)


    df2 = df2[['cell_generation', 'no_of_required_sites', 'decile']]
    df1 = pd.merge(df1, df2, on = 'decile')

    filename = 'uq_parameters_emission.csv'
    folder_out = os.path.join(DATA_RESULTS, 'cellular')

    if not os.path.exists(folder_out):

        os.makedirs(folder_out)

    merged_df = pd.merge(df, df1, on = 'decile')
    path_out = os.path.join(folder_out, filename)
    merged_df.to_csv(path_out, index = False)


    return


def select_frequency(row):
    """
    This is a helper function to select the frequency value from a list of 
    frequency spectrum in a country based on the deciles
    """
    spectrum_list = (ast.literal_eval(row['spectrum_MHz']) if 
                     isinstance(row['spectrum_MHz'], str) 
                     else row['spectrum_MHz'])
    
    if isinstance(spectrum_list, list):
          
        if row['decile'] in ['Decile 10', 'Decile 9', 'Decile 8']:
            
            return min(spectrum_list)  
        
        elif row['decile'] in ['Decile 1', 'Decile 2', 'Decile 3']:
            
            return max(spectrum_list)  
        
        else:

            return spectrum_list[0] 
        
    return None


if __name__ == '__main__':

    print('Setting seed for consistent results')
    random.seed(10)

    print('Preparing mobile data')
    #process_mobile_data()
    #model_data()

    print('Running uq_capacity_inputs_generator()')
    #uq_inputs_capacity(parameters)

    print('Running uq_cost_inputs_generator()')
    uq_inputs_costs(parameters)

    print('Running uq_inputs_emissions_generator()')
    uq_inputs_emissions(parameters)