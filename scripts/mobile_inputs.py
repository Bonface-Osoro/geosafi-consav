"""
Inputs for mobile broadband for rural areas simulation.

Written by Bonface Osoro & Ed Oughton.

September 2024

"""
parameters = {                                            
    '4G': {
        'cell_generation' : '4G',
        'grid_length' : 100,
        'transmitters' : 1,
        'mean_monthly_demand_GB' : [10, 20, 30],
        'frequencies_mhz': [700, 1800],
        'transmitter_height_low_m' : 30,
        'transmitter_height_high_m' : 50,
        'trans_user_dist_low_km' : 1,
        'trans_user_dist_high_km' : 50,
        'user_antenna_height_low_m' : 1,
        'user_antenna_height_high_m' : 10,
        'transmitter_power_low_dbm' : 43,
        'transmitter_power_high_dbm' : 46,
        'trans_antenna_gain_low_dbi' : 15,
        'trans_antenna_gain_high_dbi' : 18,
        'user_antenna_gain_low_dbi' : -5,
        'user_antenna_gain_high_dbi' : 10,
        'user_antenna_loss_low_db' : 0,
        'user_antenna_loss_high_db' : 4,
        'shadow_fading_db' : 0,
        'building_penetration_loss_db' : 12,
        'antenna_sectors' : 3,
        'subcarriers' : 1200,
        'system_temperature_k' : 294,
        'interference_low_db' : 1,
        'interference_high_db' : 3,
        'sector_antenna_low' : 1400,
        'sector_antenna_high' : 1600,
        'remote_radio_unit_low' : 3400,
        'remote_radio_unit_high' : 3600,
        'io_fronthaul_low' : 1400,
        'io_fronthaul_high' : 1600,
        'control_unit_low' : 1900,
        'control_unit_high' : 2100,
        'cooling_fans_low' : 240,
        'cooling_fans_high' : 260,
        'power_supply_low' : 240,
        'power_supply_high' : 260,
        'battery_power_low' : 9000,
        'battery_power_high' : 11000,
        'bbu_cabinet_low' : 190,
        'bbu_cabinet_high' : 210,
        'tower_low' : 4500,
        'tower_high' : 5500,
        'civil_materials_low' : 4500,
        'civil_materials_high' : 5500,
        'transportation_low' : 1200,
        'transportation_high' : 1500,
        'installation_low' : 4500,
        'installation_high' : 5500,
        'site_rental_low' : 900,
        'site_rental_high' : 1200,
        'router_low' : 1500,
        'router_high' : 2200,
        'fiber_link_low' : 8000,
        'fiber_link_high' : 15000,
        'staff_costs_low' : 20000,
        'staff_costs_high' : 60000,
        'usd_per_mhz_pop' : [0.1, 0.2, 0.3],
        'network_load' : [1],
        'discount_rate': 7,
        'assessment_period': 10,
        'bbu_rru_pcb_low_kg' : 28,
        'bbu_rru_pcb_high_kg' : 35,
        'bbu_rru_aluminium_low_kg' : 110,
        'bbu_rru_aluminium_high_kg' : 115,
        'copper_antenna_low_kg' : 45,
        'copper_antenna_high_kg' : 50,
        'aluminium_antenna_low_kg' : 17,
        'aluminium_antenna_high_kg' : 22,
        'pvc_antenna_low_kg' : 5,
        'pvc_antenna_high_kg' : 8,
        'iron_antenna_low_kg' : 6,
        'iron_antenna_high_kg' : 9,
        'steel_antenna_low_kg' : 85,
        'steel_antenna_high_kg' : 85,
        'steel_tower_low_kg' : 895,
        'steel_tower_high_kg' : 1500,
        'aluminium_frame_low_kg' : 65,
        'aluminium_frame_high_kg' : 70,
        'steel_pole_low_kg' : 25,
        'steel_pole_high_kg' : 40,
        'machine_concrete_low_kg' : 98000,
        'machine_concrete_high_kg' : 105000,
        'machine_steel_low_kg' : 900,
        'machine_steel_high_kg' : 1100,
        'basic_aluminium_device_low_kg' : 65,
        'basic_aluminium_device_high_kg' : 75,
        'smartphone_low_kg' : 50,
        'smartphone_high_kg' : 60,
        'ict_equipment_low_kg' : 34.29,
        'ict_equipment_high_kg' : 40.29,
        'power_supply_low_kg' : 28.3,
        'power_supply_high_kg' : 33.3,
        'lithium_battery_low_kg' : 8.98,
        'lithium_battery_high_kg' : 13.02,
        'consumption_low_lt_per_km' : 2.4,
        'consumption_high_lt_per_km' : 7.7,
        'machine_fuel_eff_low_lt_per_hr' : 24,
        'machine_fuel_eff_high_lt_per_hr' : 40,
        'machine_operation_low_hrs' : 100,
        'machine_operation_high_hrs' : 300,
        'cpe_low_kwh' : 0.0132,
        'cpe_high_kwh' : 0.05,
        'base_station_power_low_kwh' : 1.1,
        'base_station_power_high_kwh' : 4,
        'pcb_kg_co2e' : 29.76,
        'aluminium_kg_co2e' : 19.4,
        'copper_kg_co2e' : 4.91,
        'pvc_kg_co2e' : 3.413,
        'iron_kg_co2e' : 2.14,
        'steel_kg_co2e' : 2.56,
        'concrete_kg_co2e' : 120,
        'olnu_kg_co2e' : 0.3234,
        'smartphone_low_kwh' : 24,
        'smartphone_high_kwh' : 32,
        'ict_low_kwh' : 120,
        'ict_high_kwh' : 138,
        'base_band_unit_low_kwh' : 261,
        'base_band_unit_high_kwh' : 311,
        'radio_frequency_low_kwh' : 601,
        'radio_frequency_high_kwh' : 701,
        'epc_center_low_kwh' : 20024,
        'epc_center_high_kwh' : 21024,
        'electricity_kg_co2e' : 0.19338,
        'plastics_factor_kgco2' : 21.28,
        'metals_factor_kgco2' : 0.9847,
        'diesel_factor_kgco2e' : 2.68,
        'container_ship_kgco2e' : 0.0122,
        'social_carbon_cost_usd' : 75,
        'iterations' : 50,
        'seed_value' : 42,
        'mu' : 2, 
        'sigma' : 10,
        'draws' : 100 
    },
    '5G': {
        'cell_generation' : '5G',
        'grid_length' : 100,
        'transmitters' : 1,
        'mean_monthly_demand_GB' : [10, 20, 30],
        'frequencies_mhz': [700, 850],
        'transmitter_height_low_m' : 30,
        'transmitter_height_high_m' : 50,
        'trans_user_dist_low_km' : 1,
        'trans_user_dist_high_km' : 50,
        'user_antenna_height_low_m' : 1,
        'user_antenna_height_high_m' : 10,
        'transmitter_power_low_dbm' : 40,
        'transmitter_power_high_dbm' : 45,
        'trans_antenna_gain_low_dbi' : 16,
        'trans_antenna_gain_high_dbi' : 18,
        'user_antenna_gain_low_dbi' : -5,
        'user_antenna_gain_high_dbi' : 10,
        'user_antenna_loss_low_db' : 0,
        'user_antenna_loss_high_db' : 4,
        'shadow_fading_db' : 0,
        'building_penetration_loss_db' : 12,
        'antenna_sectors' : 3,
        'subcarriers' : 600,
        'system_temperature_k' : 294,
        'interference_low_db' : 1,
        'interference_high_db' : 3,
        'sector_antenna_low' : 1400,
        'sector_antenna_high' : 1600,
        'remote_radio_unit_low' : 3400,
        'remote_radio_unit_high' : 3600,
        'io_fronthaul_low' : 1400,
        'io_fronthaul_high' : 1600,
        'control_unit_low' : 1900,
        'control_unit_high' : 2100,
        'cooling_fans_low' : 240,
        'cooling_fans_high' : 260,
        'power_supply_low' : 240,
        'power_supply_high' : 260,
        'battery_power_low' : 9000,
        'battery_power_high' : 11000,
        'bbu_cabinet_low' : 190,
        'bbu_cabinet_high' : 210,
        'tower_low' : 4500,
        'tower_high' : 5500,
        'civil_materials_low' : 4500,
        'civil_materials_high' : 5500,
        'transportation_low' : 1200,
        'transportation_high' : 1500,
        'installation_low' : 4500,
        'installation_high' : 5500,
        'site_rental_low' : 900,
        'site_rental_high' : 1200,
        'router_low' : 1500,
        'router_high' : 2200,
        'fiber_link_low' : 8000,
        'fiber_link_high' : 15000,
        'staff_costs_low' : 20000,
        'staff_costs_high' : 60000,
        'usd_per_mhz_pop' : [0.1, 0.2, 0.3],
        'network_load' : [1],
        'discount_rate': 7,
        'assessment_period': 10,
        'bbu_rru_pcb_low_kg' : 20,
        'bbu_rru_pcb_high_kg' : 40,
        'bbu_rru_aluminium_low_kg' : 108,
        'bbu_rru_aluminium_high_kg' : 120,
        'copper_antenna_low_kg' : 45,
        'copper_antenna_high_kg' : 55,
        'aluminium_antenna_low_kg' : 17,
        'aluminium_antenna_high_kg' : 27,
        'pvc_antenna_low_kg' : 5,
        'pvc_antenna_high_kg' : 10,
        'iron_antenna_low_kg' : 6,
        'iron_antenna_high_kg' : 12,
        'steel_antenna_low_kg' : 85,
        'steel_antenna_high_kg' : 100,
        'steel_tower_low_kg' : 10000,
        'steel_tower_high_kg' : 15000,
        'aluminium_frame_low_kg' : 65,
        'aluminium_frame_high_kg' : 80,
        'steel_pole_low_kg' : 25,
        'steel_pole_high_kg' : 40,
        'machine_concrete_low_kg' : 98000,
        'machine_concrete_high_kg' : 105000,
        'machine_steel_low_kg' : 900,
        'machine_steel_high_kg' : 1100,
        'basic_aluminium_device_low_kg' : 65,
        'basic_aluminium_device_high_kg' : 75,
        'smartphone_low_kg' : 50,
        'smartphone_high_kg' : 60,
        'ict_equipment_low_kg' : 34.29,
        'ict_equipment_high_kg' : 40.29,
        'power_supply_low_kg' : 28.3,
        'power_supply_high_kg' : 33.3,
        'lithium_battery_low_kg' : 8.98,
        'lithium_battery_high_kg' : 13.02,
        'consumption_low_lt_per_km' : 2.4,
        'consumption_high_lt_per_km' : 7.7,
        'machine_fuel_eff_low_lt_per_hr' : 24,
        'machine_fuel_eff_high_lt_per_hr' : 40,
        'machine_operation_low_hrs' : 100,
        'machine_operation_high_hrs' : 300,
        'cpe_low_kwh' : 0.0132,
        'cpe_high_kwh' : 0.05,
        'base_station_power_low_kwh' : 4.3,
        'base_station_power_high_kwh' : 8,
        'pcb_kg_co2e' : 29.76,
        'aluminium_kg_co2e' : 19.4,
        'copper_kg_co2e' : 4.91,
        'pvc_kg_co2e' : 3.413,
        'iron_kg_co2e' : 2.14,
        'steel_kg_co2e' : 2.56,
        'concrete_kg_co2e' : 120,
        'olnu_kg_co2e' : 0.3234,
        'smartphone_low_kwh' : 24,
        'smartphone_high_kwh' : 32,
        'ict_low_kwh' : 120,
        'ict_high_kwh' : 138,
        'base_band_unit_low_kwh' : 261,
        'base_band_unit_high_kwh' : 311,
        'radio_frequency_low_kwh' : 601,
        'radio_frequency_high_kwh' : 701,
        'epc_center_low_kwh' : 20024,
        'epc_center_high_kwh' : 21024,
        'electricity_kg_co2e' : 0.19338,
        'plastics_factor_kgco2' : 21.28,
        'metals_factor_kgco2' : 0.9847,
        'diesel_factor_kgco2e' : 2.68,
        'container_ship_kgco2e' : 0.0122,
        'social_carbon_cost_usd' : 75,
        'iterations' : 50,
        'seed_value' : 42,
        'mu' : 2, 
        'sigma' : 10,
        'draws' : 100 
    },
}

lut = [
        # ETSI. 2018. ‘5G; NR; Physical Layer Procedures for Data
        # (3GPP TS 38.214 Version 15.3.0 Release 15)’. Valbonne, France: ETSI.
        # Generation MIMO CQI Index	Modulation	Coding rate
        # Spectral efficiency (bps/Hz) SINR estimate (dB)
        ('4G', '2x2', 1, 'QPSK', 78, 0.3, -6.7),
        ('4G', '2x2', 2, 'QPSK', 120, 0.46, -4.7),
        ('4G', '2x2', 3, 'QPSK', 193, 0.74, -2.3),
        ('4G', '2x2', 4, 'QPSK', 308, 1.2, 0.2),
        ('4G', '2x2', 5, 'QPSK', 449, 1.6, 2.4),
        ('4G', '2x2', 6, 'QPSK', 602, 2.2, 4.3),
        ('4G', '2x2', 7, '16QAM', 378, 2.8, 5.9),
        ('4G', '2x2', 8, '16QAM', 490, 3.8, 8.1),
        ('4G', '2x2', 9, '16QAM', 616, 4.8, 10.3),
        ('4G', '2x2', 10, '64QAM', 466, 5.4, 11.7),
        ('4G', '2x2', 11, '64QAM', 567, 6.6, 14.1),
        ('4G', '2x2', 12, '64QAM', 666, 7.8, 16.3),
        ('4G', '2x2', 13, '64QAM', 772, 9, 18.7),
        ('4G', '2x2', 14, '64QAM', 973, 10.2, 21),
        ('4G', '2x2', 15, '64QAM', 948, 11.4, 22.7),
        ('5G', '8x8', 1, 'QPSK', 78, 0.30, -6.7),
        ('5G', '8x8', 2, 'QPSK', 193, 2.05, -4.7),
        ('5G', '8x8', 3, 'QPSK', 449, 4.42, -2.3),
        ('5G', '8x8', 4, '16QAM', 378, 6.40, 0.2),
        ('5G', '8x8', 5, '16QAM', 490, 8.00, 2.4),
        ('5G', '8x8', 6, '16QAM', 616, 10.82, 4.3),
        ('5G', '8x8', 7, '64QAM', 466, 12.40, 5.9),
        ('5G', '8x8', 8, '64QAM', 567, 16.00, 8.1),
        ('5G', '8x8', 9, '64QAM', 666, 19.00, 10.3),
        ('5G', '8x8', 10, '64QAM', 772, 22.00, 11.7),
        ('5G', '8x8', 11, '64QAM', 873, 28.00, 14.1),
        ('5G', '8x8', 12, '256QAM', 711, 32.00, 16.3),
        ('5G', '8x8', 13, '256QAM', 797, 38.00, 18.7),
        ('5G', '8x8', 14, '256QAM', 885, 44.00, 21),
        ('5G', '8x8', 15, '256QAM', 948, 50.00, 22.7),
    ]

carbon_factors = {
    'mfg_emissions' : {
        'pcb_kg_co2e' : 29.760,
        'aluminium_kg_co2e' : 19.400,
        'copper_kg_co2e' : 4.910,
        'pvc_kg_co2e' : 3.413,
        'iron_kg_co2e' : 2.140,
        'steel_kg_co2e' : 2.560,
        'concrete_kg_co2e' : 120,
        'olnu_kg_co2e' : 0.3234,
        'electricity_kg_co2e' : 0.19338
    },
    'eolt_emissions' : {
        'glass_kg_co2e' : 21.2801938,
        'pcb_kg_co2e' : 21.2801938,
        'metals_kg_co2e' : 0.98470835,
        'concrete_kg_co2e' : 0.98470835,
    },
    'trans_emissions' : {
        'olnu_router_kg_co2e' : 0.3234,
    },
    'ops_emissions' : {
        'electricity_kg_co2e' : 0.19338
    }
}

operations = {
   'power_consumption' : {
       'cpe_power_kwh' : 0.0132,
       'fiber_point_pwr_kwh' : 5,
       'terminal_unit_pwr_kwh' : 0.5
   }
}