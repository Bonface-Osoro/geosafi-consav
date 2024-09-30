"""
Mobile broadband simulation model.

Developed by Bonface Osoro and Ed Oughton.

September 2024

"""
import math
import itertools
import numpy as np
import geopandas as gpd
from shapely.geometry import Point, Polygon
from collections import OrderedDict

################################
######## CAPACITY MODEL ########
################################

def generate_log_normal_dist_value(frequency, mu, sigma, seed_value, draws):
    """
    Generates random values using a lognormal distribution, given a specific 
    mean (mu) and standard deviation (sigma).

    Original function is found here:

    https://github.com/edwardoughton/globalsat/blob/fcda0bdfc004870a946712f68fc
    88c1fac0cf97e/src/globalsat/sim.py#L175C1-L215C28

    The parameters mu and sigma in np.random.lognormal are not the mean and STD 
    of the lognormal distribution. They are the mean and STD of the underlying 
    normal distribution.

    Parameters
    ----------
    frequency : float
        Carrier frequency value in megahertz.
    mu : int
        Mean of the desired distribution.
    sigma : int
        Standard deviation of the desired distribution.
    seed_value : int
        Starting point for pseudo-random number generator.
    draws : int
        Number of required values.

    Returns
    -------
    random_variation : float
        Mean of the random variation over the specified itations.

    """
    frequency_mhz = frequency * 1e6
    if seed_value == None:

        pass

    else:

        frequency_seed_value = seed_value * frequency_mhz * 100
        np.random.seed(int(str(frequency_seed_value)[:2]))

    normal_std = np.sqrt(np.log10(1 + (sigma/mu) ** 2))
    normal_mean = np.log10(mu) - normal_std ** 2 / 2

    random_variation  = np.random.lognormal(normal_mean, normal_std, draws)

    return random_variation


def hk_rural_correction_model(frequency_mhz):
    """
    This is a function for calculating the rural correction factor for Hata 
    Okamura path loss model.

    Parameters
    ----------
    frequency_mhz : float
        Transmission frequency in megahertz.

    Returns
    -------
    correction_loss_db : float
        Correction path loss value in dB.

    """
    first_term = 4.78 * (math.log10(frequency_mhz)) ** 2
    second_term = 18.33 * math.log10(frequency_mhz)
    correction_loss_db = (first_term + second_term) - 40.94


    return correction_loss_db


def hk_city_correction_model(frequency_mhz, user_antenna_m):
    """
    This is a function for calculating the urban correction factor for Hata 
    Okamura path loss model.

    Parameters
    ----------
    frequency_mhz : float
        Transmission frequency in megahertz.
    user_antenna_m : float
        User antenna height in meters

    Returns
    -------
    correction_loss_db : float
        Correction path loss value in dB.

    """
    first_term = ((1.1 * math.log10(frequency_mhz)) - 0.7) * user_antenna_m
    second_term = (1.56 * math.log10(frequency_mhz)) - 0.8
    correction_loss_db = first_term - second_term


    return correction_loss_db


def hk_path_loss_model(frequency_mhz, transmitter_height, user_antenna_m, 
                       trans_user_dist_km, i, random_variations):
    """
    This is a function for calculating the rural correction factor for Hata 
    Okamura path loss model.

    Parameters
    ----------
    frequency_mhz : float
        Transmission frequency in megahertz.
    transmitter_height : float
        Transmitter height in meters
    user_antenna_m : float
        User antenna height in meters
    trans_user_dist_km : float
        Distance between the transmitter and the receiver in meters
    random_variations : list
        List of random variation components.
    i : integer
        Iteration number.

    Returns
    -------
    path_loss_db : float
        Path loss value in dB.

    """
    first_term = 69.55 + (26.16 * math.log10(frequency_mhz))
    second_term = 13.82 * math.log10(transmitter_height)
    third_term = hk_city_correction_model(frequency_mhz, user_antenna_m)
    fourth_term = math.log10(trans_user_dist_km)
    fifth_term = 44.9 - (6.55 * math.log10(transmitter_height))

    interim_ans = fourth_term * fifth_term
    random_variation = random_variations[i]

    path_loss_db = (first_term - second_term - third_term + interim_ans 
                    + random_variation)


    return path_loss_db


def calc_power_received(transmitter_power_db, transmitter_antenna_gain_db, 
                        path_loss_db, shadow_fading_db, penetration_loss_db):
    """
    Estimates the power received at the user end.

    Parameters
    ----------
    transmitter_power_db : float
        Transmitter power in dB.
    transmitter_antenna_gain_db : float
        Transmitter antenna gain in dBi.
    path_loss_db : float
        Path loss in dB calculated using Hata Okamura model
    shadow_fading_db : float
        Shadow fading value in dB.
    penetration_loss_db : float
        Building penetration loss in dB

    Returns
    -------
    power_received_db : float
        Estimated power received by user in dBm.

    """

    power_received_dbm = ((transmitter_power_db + transmitter_antenna_gain_db) - 
                        (path_loss_db + shadow_fading_db + penetration_loss_db))
    

    return power_received_dbm


def calc_noise(chn_bandwidth_mhz):
    """
    Estimates the thermal noise.

    Parameters
    ----------
    chn_bandwidth_mhz : float.
        channel bandwidth in MHz

    Returns
    -------
    noise_db : float
        Received noise in dB.
    """
    k = 1.38e-23 #Boltzmann's constant k = 1.38×10−23 joules per kelvin
    t = 294 #Temperature of the receiver system T0 in kelvins

    noise_db = (10 * (math.log10((k * t * 1000)))) + \
                (10 * (math.log10(chn_bandwidth_mhz * 10 ** 6))) + 1.5
    

    return noise_db
  

def calc_cnr(received_power, noise, ue_gain, ue_losses, interference_db):
    """
    Calculate the Carrier-to-Noise Ratio (CNR).

    Returns
    -------
    received_power : float
        The received signal power at the receiver in dB.
    noise : float
        Received noise power spectral density in dBm.
    ue_gain : float
        User equipment antenna gain in dBi
    ue_losses : float
        User equipment antenna losses in dBi
    interference_db : float
        Interference noise power in db

    Returns
    -------
    cnr : float
        Carrier-to-Noise Ratio (CNR) in dB.

    """
    cnr = ((received_power + ue_gain) - (noise + ue_losses + interference_db))

    return cnr


def get_spectral_efficiency(lut, network_type, cnr_value):
    """
    Given a carrier-to-noise ratio, the function calculates the spectral 
    efficiency based on [2].

    Parameters
    ----------
    lut : list of tuples
        Lookup table for CNR to spectral efficiency.
    network_type : string
        Cellular generation.
    cnr_value : float
        Carrier-to-Noise Ratio (CNR) in dB.

    Returns
    -------
    spectral_efficiency : float
        The number of bits per Hertz able to be transmitted.
    """

    filtered_lut = [entry for entry in lut if entry[0] == network_type]

    if cnr_value < filtered_lut[0][6]:

        return filtered_lut[0][5]

    if cnr_value > filtered_lut[-1][6]:

        return filtered_lut[-1][5]

    for (lower_entry, upper_entry) in itertools.pairwise(filtered_lut):

        lower_sinr = lower_entry[6]
        upper_sinr = upper_entry[6]


        if lower_sinr <= cnr_value < upper_sinr:

            return lower_entry[5] 

    return None


def calc_maximum_distance(geometry):

    """
    This is a helper function for calculate the maximum distance from a centroid 
    of a polygon.

    Parameters
    ----------
    geometry : polygon
        Geographic polygon where the mobile station serves.

    Returns
    -------
    max_distance_km : float
        Maximum distance from the centroid.
    """

    centroid = geometry.centroid 
    max_distance_km = max(centroid.distance(Point(point)) for point in geometry.exterior.coords)
    

    return max_distance_km


def calc_channel_capacity(spectral_efficiency, chn_bandwidth_mhz):
    """
    Calculate the channel capacity in Mbps.

    Channel Capacity (Mbps) = Spectral efficiency x Channel bandwidth (MHz)

    Parameters
    ----------
    spectral_efficiency : float
        The number of bits per Hertz that can be transmitted.
    chn_bandwidth_mhz: float
        The channel bandwidth in Megahertz.

    Returns
    -------
    channel_capacity_mbps : float
        The channel capacity in Mbps.

    """
    channel_capacity_mbps = ((spectral_efficiency * chn_bandwidth_mhz * 10 ** 6) 
                             / 1e6)


    return channel_capacity_mbps


def base_station_capacity(cell_generation, capacity_value, base_station_list):
    """
    This function calculates the capacity based on the cellular generation 
    technology

    Parameters
    ----------
    cell_generation : string.
        Cellphone generation technology.
    capacity_value : float.
        Capacity value.
    base_station_list : list.
        List containing the number of base station.

    Returns
    -------
    base_station_capacity_mbps : float
        Base station capacity
    """

    for base_station in base_station_list:

        if cell_generation == '4G':

            base_station_capacity_mbps = ((capacity_value * 
                                           base_station_list[0]))
            
        else:

            base_station_capacity_mbps = ((capacity_value * 
                                           base_station_list[1]))
            

    return base_station_capacity_mbps


############################
######## COST MODEL ########
############################

def equipment_cost(sector_antenna, remote_radio_unit, fronthaul_interface,
                   control_unit, cooling_fans, battery_system, 
                   bbu_unit, tower, civil_materials, router):
    """
    This function calculates operating expenditures

    Parameters
    ----------
    sector_antenna : int.
        Sector radio antenna cost.
    remote_radio_unit : int.
        Remote radio system cost.
    fronthaul_interface : int.
        Input-output fronthaul interface cost.
    control_unit : int.
        System control unit cost.
    cooling_fans : int.
        Power cooling fans cost.
    battery_system : int.
        cost of battery and power system costs.
    bbu_unit : int.
        Baseband unit cost.
    tower : int.
        Antenna tower cost.
    civil_materials : int.
        Civil materials like cement, concrete  etc cost.
    router : int.
        Network router cost.

    Returns
    -------
    equipement_cost : int
            Total equipment cost.

    """
    equipment_cost = (sector_antenna + remote_radio_unit + fronthaul_interface 
                      + control_unit + cooling_fans + battery_system + bbu_unit 
                      + tower + civil_materials + router)


    return equipment_cost


def spectrum_cost(frequency_band, population, cost_per_mhz):
    """
    This function calculates the total spectrum costs

    Parameters
    ----------
    frequency_band : int.
        Operating frequency in MHz.
    population : int.
        Total population of the country.
    cost_per_mhz : int.
        Cost of spectrum per Hz.

    Returns
    -------
    spectrum_cost : float
            The total spectrum costs.

    """
    frequency_hz = frequency_band
    spectrum_cost = frequency_hz * population * cost_per_mhz


    return spectrum_cost


def capex_cost(equipment_cost, spectrum_cost, installation, transportation):
    """
    This function calculates capital expenditures

    Parameters
    ----------
    equipment_cost : int.
        Total equipment cost.
    spectrum_cost : int.
        Total spectrum cost.
    installation : int.
        Total cost of installing the macro base station.
    transportation : int.
        Cost of transporting equipment and staff during construction.

    Returns
    -------
    capex_costs : float
            The capital expenditure costs.

    """
    capex_costs = (equipment_cost + spectrum_cost + installation 
                  + transportation) 


    return capex_costs


def opex_cost(site_rental, base_station_energy, staff_costs, antenna_upgrade,
              remote_radio_upgrade, bbu_unit_upgrade, router_upgrade, 
              fiber_link_upgrade):
    """
    This function calculates operating expenditures

    Parameters
    ----------
    site_rental : int.
        Monthly land rental cost.
    base_station_energy : int.
        ground station cost.
    staff_costs : int.
        staff costs.
    antenna_upgrade : int.
        Cost of upgrading radio antenna unit.
    remote_radio_upgrade : int.
        Cost of upgrading remote radio unit.
    bbu_unit_upgrade : int.
        Cost of upgrading baseband radio unit.
    router_upgrade : float.
        Cost of upgrading radio router unit.
    fiber_link_upgrade : int.
        Cost of upgrading fiber link unit.
    Returns
    -------
    annual_opex : float
            The operating expenditure costs annually.

    """
    antenna_upgrade = antenna_upgrade * 0.1
    remote_radio_upgrade = remote_radio_upgrade * 0.1
    bbu_unit_upgrade = bbu_unit_upgrade * 0.1
    router_upgrade = router_upgrade * 0.1
    fiber_link_upgrade = fiber_link_upgrade * 0.1
    opex_costs = (site_rental + base_station_energy + staff_costs 
                  + antenna_upgrade + remote_radio_upgrade + bbu_unit_upgrade 
                  + router_upgrade + fiber_link_upgrade) 
   
    annual_opex = opex_costs


    return annual_opex


def total_cost_ownership(total_capex, total_opex, discount_rate, 
                         assessment_period):
    """
    Calculate the total cost of ownership(TCO) in US$:

    Parameters
    ----------
    total_capex : int.
        Total initial capital expenditures.
    total_opex : int.
        Total annual operating expenditures.
    discount_rate : float.
        discount rate.
    assessment_period : int.
        assessment period of the infrastructure.

    Returns
    -------
    total_cost_ownership : float
            The total cost of ownership.

    """

    year_costs = []

    for time in np.arange(1, assessment_period):  

        yearly_opex = total_opex / (((discount_rate / 100) + 1) ** time)
        year_costs.append(yearly_opex)

    total_cost_ownership = total_capex + sum(year_costs) + total_opex


    return total_cost_ownership


def base_station_tco(cell_generation, tco_value, base_station_list):
    """
    This function calculates the TCO based on the cellular generation technology

    Parameters
    ----------
    cell_generation : string.
        Cellphone generation technology.
    tco_value : float.
        TCO value.
    base_station_list : list.
        List containing the number of base station.

    Returns
    -------
    base_station_tco_usd : float
        Base station TCO
    """

    for base_station in base_station_list:

        if cell_generation == '4G':

            base_station_tco_usd = tco_value * base_station_list[0]
            
        else:

            base_station_tco_usd = tco_value * base_station_list[1]
            

    return base_station_tco_usd


#################################
######## EMISSIONS MODEL ########
#################################

def lca_manufacturing(pcb_emissions_kg, alu_bbu_rru_kg, cu_antenna_kg, 
                      alu_antenna_kg, pvc_antenna_kg, iron_antenna_kg, 
                      steel_antenna_kg, tower_kg, alu_frame_kg, steel_pole_kg, 
                      room_concrete_kg, room_steel_kg, basic_alu_kg,
                      pcb_carbon_factor, alu_carbon_factor, cu_carbon_factor, 
                      pvc_carbon_factor, fe_carbon_factor, steel_carbon_factor, 
                      concrete_carbon_factor):
    """
    This function calculates the total GHG emissions in the manufacturing 
    phase LCA of mobile broadband using carbon emission factors.

    Parameters
    ----------
    pcb_emissions_kg : float.
        Mass of printed circuit board.
    alu_bbu_rru_kg : float.
        Mass of aluminium used in baseband and remote radio unit.
    cu_antenna_kg : float.
        Mass of copper metal used in building antenna.
    alu_antenna_kg : float.
        Mass of alumonium metal used in building antenna.
    pvc_antenna_kg : float.
        Mass of PVC material used in building antenna.
    iron_antenna_kg : float.
        Mass of iron metal used in building antenna.
    steel_antenna_kg : float.
        Mass of steel metal used in building antenna.
    tower_kg : float.
        Mass of steel tower.
    alu_frame_kg : float.
        Mass of aluminium frame.
    steel_pole_kg : float.
        Mass of steel poles used in the base station construction.
    room_concrete_kg : float.
        Mass of concrete used in building machine room.
    room_steel_kg : float.
        Mass of steel ribar used in building machine room.
    basic_alu_kg : float.
        Mass of aluminium used in all basic materials in the base station.

    pcb_carbon_factor, alu_carbon_factor, cu_carbon_factor, pvc_carbon_factor, 
    fe_carbon_factor, steel_carbon_factor, concrete_carbon_factor : float.
        Carbon emission factors sof PCB, alumnium, copper, PVC, iron, steel and
        concrete respectively. 

    Returns
    -------
    mfg_emission_dict : dict
        Dictionary containing GHG emissions by type.

    """
    mfg_emission_dict = {}

    pcb_ghg = (pcb_emissions_kg * pcb_carbon_factor)
    
    alu_bbu__ghg = (alu_bbu_rru_kg * alu_carbon_factor)
    
    cu_antenna_ghg = (cu_antenna_kg * cu_carbon_factor)
    
    alu_antenna_ghg = (alu_antenna_kg * alu_carbon_factor)
    
    pvc_ghg = (pvc_antenna_kg * pvc_carbon_factor)
    
    iron_ghg = (iron_antenna_kg * fe_carbon_factor)
    
    steel_ghg = (steel_antenna_kg * steel_carbon_factor)

    tower_ghg = (tower_kg * steel_carbon_factor)
    
    alu_frame_ghg = (alu_frame_kg * alu_carbon_factor)

    steel_pole_ghg = (steel_pole_kg * steel_carbon_factor)
    
    room_concrete_ghg = (room_concrete_kg * concrete_carbon_factor)

    room_steel_ghg = (room_steel_kg * steel_carbon_factor)
    
    basic_alu_device_ghg = (basic_alu_kg * alu_carbon_factor)
    
    mfg_emission_dict['aluminium_ghg'] = (alu_bbu__ghg + alu_antenna_ghg 
                                        + alu_frame_ghg + basic_alu_device_ghg)

    mfg_emission_dict['steel_iron_ghg'] = (iron_ghg + steel_ghg + tower_ghg 
                                           + steel_pole_ghg + room_steel_ghg)

    mfg_emission_dict['concrete_ghg'] = room_concrete_ghg

    mfg_emission_dict['plastics_ghg'] = (pcb_ghg + pvc_ghg)

    mfg_emission_dict['other_metals_ghg'] = cu_antenna_ghg


    return mfg_emission_dict


def lca_transportation(distance_km, consumption_lt_per_km, diesel_factor_kgco2e):
    """
    This function calculates the total GHG emissions in the transportation 
    LCA phase of mobile broadband deployment.

    Parameters
    ----------
    distance_km : float.
        Distance travelled by the vehicle.
    consumption_lt_per_km : float.
        Fuel consumption of the vehicle per distance.
    diesel_factor_kgco2e : float.
        Carbon emission factor of diesel fuel.

    Returns
    -------
    trans_emission_dict : dict
        Dictionary containing GHG emissions by type.
    """
    trans_emission_dict = {}

    trans_ghg = (distance_km * consumption_lt_per_km * diesel_factor_kgco2e)

    trans_emission_dict['trans_ghg_kg'] = trans_ghg


    return trans_emission_dict


def lca_construction(fuel_efficiency, machine_operating_hours, 
                     diesel_factor_kgco2e):
    """
    This function calculates the total GHG emissions in the construction 
    LCA phase of mobile broadband deployment.

    Parameters
    ----------
    fuel_efficiency : float.
        Fuel efficiency of the machine.
    machine_operating_hours : float.
        Number of hours the machine operated.
    diesel_factor_kgco2e : float.
        Carbon emission factor of diesel fuel.

    Returns
    -------
    trans_emission_dict : dict
        Dictionary containing GHG emissions by type.
    """
    construction_emission_dict = {}

    construction_ghg_kg = (fuel_efficiency * machine_operating_hours 
                     * diesel_factor_kgco2e)

    construction_emission_dict['construction_ghg'] = construction_ghg_kg


    return construction_emission_dict


def lca_operations(cpe_power_kwh, base_station_power_kwh, number_of_users, 
                   electricity_kg_co2e):
    """
    This function calculates the total GHG emissions due to operation of the 
    fiber broadband

    Parameters
    ----------
    cpe_power_kwh : float.
        Mobile phone/user equipment power consumption.
    base_station_power_kwh : float.
        Total power consumption of base station.
    number_of_users : int.
        Number of users accessing the base station.
    electricity_kg_co2e : float.
        Carbon emission factor of electricity.

    Returns
    -------
    operations_emission_dict : dict
        Dictionary containing GHG emissions by type.
    """

    operations_emission_dict = {}

    per_user_power = cpe_power_kwh + (base_station_power_kwh / number_of_users)
    operations_ghg_kg = (per_user_power * electricity_kg_co2e)

    operations_emission_dict['operations_ghg'] = operations_ghg_kg


    return operations_emission_dict


def lca_eolt(pcb_emissions_kg, alu_bbu_rru_kg, cu_antenna_kg, alu_antenna_kg, 
             pvc_antenna_kg, iron_antenna_kg, steel_antenna_kg, tower_kg, 
             alu_frame_kg, steel_pole_kg, room_steel_kg, basic_alu_kg, 
             metals_factor_kgco2, plastics_factor_kgco2):
    """
    This function calculates the total GHG emissions in the end-of-life treatment 
    phase LCA of mobile broadband using carbon emission factors.

    Parameters
    ----------
    pcb_emissions_kg : float.
        Mass of printed circuit board.
    alu_bbu_rru_kg : float.
        Mass of aluminium used in baseband and remote radio unit.
    cu_antenna_kg : float.
        Mass of copper metal used in building antenna.
    alu_antenna_kg : float.
        Mass of alumonium metal used in building antenna.
    pvc_antenna_kg : float.
        Mass of PVC material used in building antenna.
    iron_antenna_kg : float.
        Mass of iron metal used in building antenna.
    steel_antenna_kg : float.
        Mass of steel metal used in building antenna.
    tower_kg : float.
        Mass of steel tower.
    alu_frame_kg : float.
        Mass of aluminium frame.
    steel_pole_kg : float.
        Mass of steel poles used in the base station construction.
    room_concrete_kg : float.
        Mass of concrete used in building machine room.
    room_steel_kg : float.
        Mass of steel ribar used in building machine room.
    basic_alu_kg : float.
        Mass of aluminium used in all basic materials in the base station.
    metals_factor_kgco2 : float.
        Carbon emissions factor of metals.
    plastics_factor_kgco2 : float.
        Carbon emissions factor of plastics.

    Returns
    -------
    mfg_emission_dict : dict
        Dictionary containing GHG emissions by type.

    """
    eolt_emission_dict = {}

    pcb_ghg = (pcb_emissions_kg * plastics_factor_kgco2)
    
    alu_bbu__ghg = (alu_bbu_rru_kg * metals_factor_kgco2)
    
    cu_antenna_ghg = (cu_antenna_kg * metals_factor_kgco2)
    
    alu_antenna_ghg = (alu_antenna_kg * metals_factor_kgco2)
    
    pvc_ghg = (pvc_antenna_kg * plastics_factor_kgco2)
    
    iron_ghg = (iron_antenna_kg * metals_factor_kgco2)
    
    steel_ghg = (steel_antenna_kg * metals_factor_kgco2)

    tower_ghg = (tower_kg * metals_factor_kgco2)
    
    alu_frame_ghg = (alu_frame_kg * metals_factor_kgco2)

    steel_pole_ghg = (steel_pole_kg * metals_factor_kgco2)

    room_steel_ghg = (room_steel_kg * metals_factor_kgco2)
    
    basic_alu_device_ghg = (basic_alu_kg * metals_factor_kgco2)
    
    eolt_emission_dict['aluminium_ghg'] = (alu_bbu__ghg + alu_antenna_ghg 
                                        + alu_frame_ghg + basic_alu_device_ghg)

    eolt_emission_dict['steel_iron_ghg'] = (iron_ghg + steel_ghg + tower_ghg 
                                           + steel_pole_ghg + room_steel_ghg)

    eolt_emission_dict['plastics_ghg'] = (pcb_ghg + pvc_ghg)

    eolt_emission_dict['other_metals_ghg'] = cu_antenna_ghg


    return eolt_emission_dict


def phase_emission_ghg(cell_generation, emission_type_value, base_station_list):
    """
    This function calculates the total phase GHG emissions based on the cellular 
    generation technology

    Parameters
    ----------
    cell_generation : string.
        Cellphone generation technology.
    emission_type_value : float.
        Total LCA phase emissions value.
    base_station_list : list.
        List containing the number of base station emissions.

    Returns
    -------
    phase_emission_kg : float
        Phase emission value based on the cellular generation
    """

    for base_station in base_station_list:

        if cell_generation == '4G':

            phase_emission_kg = emission_type_value * base_station_list[0]
            
        else:

            phase_emission_kg = emission_type_value * base_station_list[1]
            

    return phase_emission_kg