"""
Inputs for mobile broadband for rural areas simulation.

Written by Bonface Osoro & Ed Oughton.

September 2024

"""
parameters = {                                            
    '4G': {
        'cell_generation' : '4G',
        'frequencies_mhz': [700, 1800],
        'channel_bandwidth_mhz': 20,
        'transmitter_height_low_m' : 30,
        'transmitter_height_high_m' : 50,
        'trans_user_dist_low_km' : 1,
        'trans_user_dist_high_km' : 20,
        'user_antenna_height_low_m' : 1,
        'user_antenna_height_high_m' : 10,
        'transmitter_power_low_dbm' : 40,
        'transmitter_power_high_dbm' : 45,
        'trans_antenna_gain_low_dbi' : 16,
        'trans_antenna_gain_high_dbi' : 23,
        'shadow_fading_db' : 0,
        'building_penetration_loss_db' : 12,
        'antenna_sectors' : 3,
        'system_temperature_k' : 294,
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
        'network_load' : 0.5,
        'discount_rate': 7,
        'assessment_period': 10,
        'iterations' : 50,
        'seed_value' : 42,
        'mu' : 2, 
        'sigma' : 10,
        'draws' : 100 
    },
    '5G': {
        'cell_generation' : '5G',
        'frequencies_mhz': [600, 700],
        'channel_bandwidth_mhz': 20,
        'transmitter_height_low_m' : 30,
        'transmitter_height_high_m' : 50,
        'trans_user_dist_low_km' : 1,
        'trans_user_dist_high_km' : 20,
        'user_antenna_height_low_m' : 1,
        'user_antenna_height_high_m' : 10,
        'transmitter_power_low_dbm' : 40,
        'transmitter_power_high_dbm' : 45,
        'trans_antenna_gain_low_dbi' : 16,
        'trans_antenna_gain_high_dbi' : 23,
        'shadow_fading_db' : 0,
        'building_penetration_loss_db' : 12,
        'antenna_sectors' : 3,
        'system_temperature_k' : 294,
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
        'network_load' : 0.5,
        'discount_rate': 7,
        'assessment_period': 10,
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
        ('4G', '1x1', 1, 'QPSK', 78, 0.1523, -6.7),
        ('4G', '1x1', 2, 'QPSK', 120, 0.2344, -4.7),
        ('4G', '1x1', 3, 'QPSK', 193, 0.377, -2.3),
        ('4G', '1x1', 4, 'QPSK', 308, 0.6016, 0.2),
        ('4G', '1x1', 5, 'QPSK', 449, 0.877, 2.4),
        ('4G', '1x1', 6, 'QPSK', 602, 1.1758, 4.3),
        ('4G', '1x1', 7, '16QAM', 378, 1.4766, 5.9),
        ('4G', '1x1', 8, '16QAM', 490, 1.9141, 8.1),
        ('4G', '1x1', 9, '16QAM', 616, 2.4063, 10.3),
        ('4G', '1x1', 10, '64QAM', 466, 2.7305, 11.7),
        ('4G', '1x1', 11, '64QAM', 567, 3.3223, 14.1),
        ('4G', '1x1', 12, '64QAM', 666, 3.9023, 16.3),
        ('4G', '1x1', 13, '64QAM', 772, 4.5234, 18.7),
        ('4G', '1x1', 14, '64QAM', 973, 5.1152, 21),
        ('4G', '1x1', 15, '64QAM', 948, 5.5547, 22.7),
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