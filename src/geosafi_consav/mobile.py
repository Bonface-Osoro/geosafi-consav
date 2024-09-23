"""
Mobile broadband simulation model.

Developed by Bonface Osoro and Ed Oughton.

September 2024

"""
import math
import numpy as np
import itertools
from collections import OrderedDict


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
  

def calc_cnr(received_power, noise):
    """
    Calculate the Carrier-to-Noise Ratio (CNR).

    Returns
    -------
    received_power : float
        The received signal power at the receiver in dB.
    noise : float
        Received noise power spectral density in dBm/Hz.

    Returns
    -------
    cnr : float
        Carrier-to-Noise Ratio (CNR) in dB.

    """
    cnr = received_power - noise

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