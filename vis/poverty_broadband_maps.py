import os
import configparser
import warnings
import contextily as ctx
import pandas as pd
import matplotlib.pyplot as plt 
import geopandas as gpd
import seaborn as sns
from shapely import wkt
from mpl_toolkits.axes_grid1 import make_axes_locatable
pd.options.mode.chained_assignment = None
warnings.filterwarnings('ignore')

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']
DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESS = os.path.join(BASE_PATH, '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results')
DATA_AFRICA = os.path.join(BASE_PATH, '..', 'results', 'SSA')
DATA_VIS = os.path.join(BASE_PATH, '..', 'vis', 'figures')
path = os.path.join(DATA_RAW, 'countries.csv')

def get_regional_shapes():
    """
    Load regional shapes.

    """
    output = []

    for item in os.listdir(DATA_PROCESS):

        filename_gid2 = 'regions_2_{}.shp'.format(item)
        path_gid2 = os.path.join(DATA_PROCESS, item, 'regions', filename_gid2)

        filename_gid1 = 'regions_1_{}.shp'.format(item)
        path_gid1 = os.path.join(DATA_PROCESS, item, 'regions', filename_gid1)

        if os.path.exists(path_gid2):
            data = gpd.read_file(path_gid2)
            data['GID_id'] = data['GID_2']
            data = data.to_dict('records')

        elif os.path.exists(path_gid1):
            data = gpd.read_file(path_gid1)
            data['GID_id'] = data['GID_1']
            data = data.to_dict('records')

        else:

            print('No shapefiles for {}'.format(item))
            continue

        for datum in data:
            output.append({
                'geometry': datum['geometry'],
                'properties': {
                    'GID_1': datum['GID_id'],
                },
            })

    output = gpd.GeoDataFrame.from_features(output, crs = 'epsg:4326')

    return output

def plot_regions_by_poverty():
    """
    Plot poverty rates by regions.
    """
    print('Plotting poverty rates by regions')
    
    regions = get_regional_shapes()
    
    DATA_AFRICA = os.path.join(BASE_PATH, '..', 'results', 'SSA', 'SSA_poverty_results.csv')
    data = pd.read_csv(DATA_AFRICA)
    n = int(len(data) / 4)
    data['poor_population'] = round(data['poor_population'])
    data = data[['GID_1', 'poor_population', 'poverty_range']]
    
    value_mapping = {'GSAP2_poor': 'Below $US 1.9', 'GSAP2_po_1': 'Below $US 3.2', 'GSAP2_po_2': 'Below $US 5.5'}
    data['poverty_range'] = data['poverty_range'].replace(value_mapping)
    
    regions = regions[['GID_1', 'geometry']]
    regions = regions.merge(data, left_on='GID_1', right_on='GID_1')
    regions.reset_index(drop=True, inplace=True)

    metric = 'poor_population'
    bins = [-1, 1000, 5000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000, 3000000]
    labels = [
        '<1k$',
        '5 - 10k',
        '10 - 25k',
        '25 - 50k',
        '50 - 100k',
        '100 - 250k',
        '250 - 500k',
        '500k - 1 mn',
        '1 - 3 mn',
        '>3 mn']

    regions['bin'] = pd.cut(
        regions[metric],
        bins=bins,
        labels=labels)
    
    sns.set(font_scale=0.9)
    
    # Create subplots for each category in the "poverty_range" column
    fig, axes = plt.subplots(3, 1, figsize = (10, 12), sharex = True, sharey = True)

    for i, poverty_category in enumerate(data['poverty_range'].unique()):
        subset_regions = regions[regions['poverty_range'] == poverty_category]
        ax = axes[i]

        base = subset_regions.plot(column = 'bin', ax = ax,
                                   cmap = 'YlGnBu', linewidth = 0.2,
                                   legend = True, edgecolor = 'grey')

        handles, labels = ax.get_legend_handles_labels()

        ctx.add_basemap(ax, crs = regions.crs, source = ctx.providers.CartoDB.Voyager)

        name = f'Poverty-line Population - {poverty_category} (n={n})'
        ax.set_title(name, fontsize = 14)

    fig.subplots_adjust(wspace = 0)
    fig.tight_layout(rect = [0, 0, 1, 1])

    path = os.path.join(DATA_VIS, 'poverty_line_population.png')
    fig.savefig(path)
    plt.close(fig)

plot_regions_by_poverty()