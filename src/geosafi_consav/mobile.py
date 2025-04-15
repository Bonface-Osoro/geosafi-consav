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
def user_demand(mean_monthly_demand_GB, traffic_busy_hour, smartphone_penetration, 
                mean_poor_connected, mean_area_sqkm):
    """
    This is a function for calculating the user demand based on future monthly 
    traffic.

    Parameters
    ----------
    mean_monthly_demand_GB : float
        Future monthly traffic in GB.
    traffic_busy_hour : float
        Quantity of daily traffic taking place within the busiest hour of the day.
    smartphone_penetration : float
        Future smartphone penetration rate in percentage.
    mean_poor_connected : float
        Average number of unconnected population in a given statistical area.
    mean_area_sqkm : float
        Average area where the unconnected population lives in.
        
    Returns
    -------
    demand_density_mbps_sqkm : float
        Traffic demand density per user at a given time in Mbps/km^2.

    """
    user_demand = (mean_monthly_demand_GB * 1000 * 8 * (1 / 30) 
                   * (traffic_busy_hour / 100) * (1 / 3600))
    
    demand_density_mbps_sqkm = ((mean_poor_connected * (smartphone_penetration 
                                / 100) * user_demand) / (mean_area_sqkm))


    return demand_density_mbps_sqkm


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


def calc_signal_path(transmitter_x, transmitter_y, receiver_x, receiver_y):
    """
    Calculate the Euclidean distance between the transmitter
    and receiver.

    Parameters
    ----------
    transmitter_x (float) : 
        x coordinates of transmitter (km).
    transmitter_y (float) : 
        y coordinates of transmitter (km).
    receiver_x (float) : 
        x coordinates of receiver (km).
    receiver_y (float) : 
        y coordinates of receiver (km).

    Returns
    -------
    distance : float
        Distance in km.
    """
    x_term = (receiver_x - transmitter_x) ** 2
    y_term = (receiver_y - transmitter_y) ** 2
    distance = math.sqrt(x_term + y_term)

    return distance


def calc_interference_path(interference_x, interference_y, receiver_x, 
                           receiver_y):
    """
    Calculates the Euclidean distance between the interfering site
    and the user.

    Parameters
    ----------
    interference_x (float) : 
        x coordinates of interfering site (km).
    interference_y (float) : 
        y coordinates of interfering site (km).
    receiver_x (float) : 
        x coordinates of receiver (km).
    receiver_y (float) : 
        y coordinates of receiver (km).

    Returns
    -------
    interference_user_distance : float
        Interfering site to the user's distance in km.
    """  
    x_i_term = (interference_x - receiver_x) ** 2
    y_i_term = (interference_y - receiver_y) ** 2 
    interference_user_distance = math.sqrt(x_i_term + y_i_term)


    return interference_user_distance


def calc_intersite_distance(interference_x, interference_y, transmitter_x, 
                            transmitter_y):
    """
    Calculates the Euclidean distance between the interfering site
    and the transmitter.

    Parameters
    ----------
    interference_x (float) : 
        x coordinates of interfering site (km).
    interference_y (float) : 
        y coordinates of interfering site (km).
    transmitter_x (float) : 
        x coordinates of the transmitter (km).
    transmitter_y (float) : 
        y coordinates of transmitter (km).

    Returns
    -------
    intersite_distance : float
        Interfering site to the transmitter's distance in km.
    """  
    x_i_term = (interference_x - transmitter_x) ** 2
    y_i_term = (interference_y - transmitter_y) ** 2 
    intersite_distance = math.sqrt(x_i_term + y_i_term)


    return intersite_distance


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
    #fourth_term = math.log10(trans_user_dist_km)
    if abs(trans_user_dist_km) > 0:
        
        fourth_term = math.log10(abs(trans_user_dist_km))

    else:
        fourth_term = 1

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


def system_type(frequency_mhz):
    """
    This is a helper function for determining whether the technology is 4G or 5G

    Parameters
    ----------
    frequency_mhz : int.
        transmitting frequency in MHz

    Returns
    -------
    cell_generation : string
        Cellular technology/generation.
    """
    five_G_frequencies = [700, 3500, 5800]
    frequencies = [frequency_mhz]

    if any(num in five_G_frequencies for num in frequencies):

        cell_generation = '5G'

    else:

        cell_generation = '4G'


    return cell_generation


def bandwidth(cell_generation):
    """
    This is a helper fucntion to calculate the channel bandwidth

    Parameters
    ----------
    cell_generation : float.
        Cellular technology/generation

    Returns
    -------
    chn_bandwidth_mhz : float
        Channel bandwidth.
    """

    if cell_generation == '4G':

        chn_bandwidth_mhz = 10

    else:

        chn_bandwidth_mhz = 40


    return chn_bandwidth_mhz


def calc_noise(frequency_mhz, chn_bandwidth_mhz):
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

    if frequency_mhz == 5800 or frequency_mhz == 3500:

        chn_bandwidth_mhz = 40

    else:

        chn_bandwidth_mhz = 10

    noise_db = (10 * (math.log10((k * t * 1000)))) + \
                (10 * (math.log10(chn_bandwidth_mhz * 10 ** 6))) + 1.5
    

    return noise_db
  

def calc_sinr(received_power, noise, ue_gain, ue_losses, interference_db):
    """
    Calculate the Signal-to-Interference-plu-Noise ratio (SINR).

    Parameters
    ----------
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
    sinr : float
        Signal-to-Interference-plu-Noise ratio (SINR) in dB.

    """
    sinr = ((received_power + ue_gain) - (noise + ue_losses + interference_db))

    return sinr


def calc_interference(received_power, noise, ue_gain, ue_losses):
    """
    Calculate the noise from interfering site.

    Parameters
    ----------
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
    inteference : float
        Interefernce dB.

    """
    inteference = ((received_power + ue_gain) - (noise + ue_losses))


    return inteference


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
    max_distance_km = max(centroid.distance(Point(point)) for point in 
                          geometry.exterior.coords)
    

    return max_distance_km


def calc_capacity(spectral_efficiency, chn_bandwidth_mhz, antenna_sectors):
    """
    Calculate the channel capacity in Mbps.

    Channel Capacity (Mbps) = Spectral efficiency x Channel bandwidth (MHz)

    Parameters
    ----------
    spectral_efficiency : float
        The number of bits per Hertz that can be transmitted.
    chn_bandwidth_mhz: float
        The channel bandwidth in Megahertz.
    antenna_sectors : int
        Number if antenna sectors, usually 3

    Returns
    -------
    channel_capacity_mbps : float
        The channel capacity in Mbps.

    """
    channel_capacity_mbps = ((spectral_efficiency * chn_bandwidth_mhz * 10 ** 6) 
                             / 1e6) * antenna_sectors


    return channel_capacity_mbps


def calc_site_area(intersite_distance_km):
    """
    Calculate the site area served by a cellular tower.

    Parameters
    ----------
    intersite_distance_km : float
        Spatial distance between the transmitter and the receiver.

    Returns
    -------
    site_area_sqkm : float
        Site area.
    """
    inter_site_distance = intersite_distance_km * 2
    site_area_km2 = (math.sqrt(3) / 2 * inter_site_distance ** 2)
    site_area_sqkm = 1 / site_area_km2


    return site_area_sqkm


def calc_area_capacity(capacity_mbps, site_area_sqkm):
    """
    Calculate the area capacity.

    Parameters
    ----------
    capacity_mbps : float
        Channel capacity in Mbps.
    site_area_sqkm : float
        Site are in km^2.

    Returns
    -------
    capacity_mbps_km2 : float
        Capacity per km2.
    """
    capacity_mbps_km2 = capacity_mbps / site_area_sqkm


    return capacity_mbps_km2


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


#################################
######## EMISSIONS MODEL ########
#################################

def lca_manufacturing(pcb_emissions_kg, alu_bbu_rru_kg, cu_antenna_kg, 
                      alu_antenna_kg, pvc_antenna_kg, iron_antenna_kg, 
                      steel_antenna_kg, tower_kg, alu_frame_kg, steel_pole_kg, 
                      room_concrete_kg, room_steel_kg, basic_alu_kg,
                      pcb_carbon_factor, alu_carbon_factor, cu_carbon_factor, 
                      pvc_carbon_factor, fe_carbon_factor, steel_carbon_factor, 
                      concrete_carbon_factor, smartphone_ghg, ict_ghg,
                      power_supply_ghg, battery_ghg, users):
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
    smartphones_ghg : float.
        Unit carbon emission factor of smartphone
    ict_ghg : float.
        Unit carbon emission factor of ICT equipment
    power_supply_ghg : float.
        Unit carbon emission factor of power supply
    battery_ghg : float.
        Unit carbon emission factor of battery
    users : int
        The number of users. 
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

    mfg_emission_dict['smartphones_ghg'] = smartphone_ghg * users * 4

    mfg_emission_dict['ict_equipment_ghg'] = ict_ghg * users * 2 * 0.3

    mfg_emission_dict['power_ghg'] = power_supply_ghg * users * 3

    mfg_emission_dict['battery_ghg'] = battery_ghg * users


    return mfg_emission_dict


def lca_transportation(distance_km, consumption_lt_per_km, diesel_factor_kgco2e,
                       maritime_km, container_ship_kgco2e):
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
    maritime_km : float
        Distance between the port of China and SSA country.
    container_ship_kgco2e : float
        Carbon emission factor of a container ship.

    Returns
    -------
    trans_emission_dict : dict
        Dictionary containing GHG emissions by type.
    """
    trans_emission_dict = {}

    maritime_ghg = maritime_km * container_ship_kgco2e

    road_ghg = (distance_km * consumption_lt_per_km * diesel_factor_kgco2e)

    trans_emission_dict['trans_ghg_kg'] = maritime_ghg + road_ghg


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


def lca_operations(smartphone_kg, ict_kg, base_band_unit_kwh, 
                   number_of_users, radio_frequency_unit_kwh, epc_center_kwh, 
                   number_epc_centers, electricity_kg_co2e, number_of_sites):
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

    smartphone_kwh = smartphone_kg * number_of_users

    ict_kwh = ict_kg * number_of_users * 0.3

    end_user_device_kwh = smartphone_kwh + ict_kwh
    
    base_station_kwh = ((base_band_unit_kwh + radio_frequency_unit_kwh) 
                        * number_of_sites)

    epc_kwh = epc_center_kwh * number_epc_centers

    total_power_kwh = (end_user_device_kwh + base_station_kwh + epc_kwh)

    operations_emission_dict['operations_ghg'] = (total_power_kwh 
                                                * electricity_kg_co2e)


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


def phase_emission_ghg(cell_generation, emission_type_value, number_of_sites):
    """
    This function calculates the total phase GHG emissions based on the cellular 
    generation technology

    Parameters
    ----------
    cell_generation : string.
        Cellphone generation technology.
    emission_type_value : float.
        Total LCA phase emissions value.
    number_of_sites : int.
        Number of base station emissions.

    Returns
    -------
    phase_emission_kg : float
        Phase emission value based on the cellular generation
    """

    phase_emission_kg = emission_type_value * number_of_sites
            

    return phase_emission_kg


def maritime_distance(iso3, maritime_dict):
    """
    This function calculates the distance between the origin Port of China and 
    any SSA country

    Parameters
    ----------
    iso3 : string.
        Country ISO3.
    maritime_dict : dict
        Dictionary containing ISO3 codes as keys and distances as values.

    Returns
    -------
    distance_km : float
        Distance between the two ports
    """

    return maritime_dict.get(iso3, None)