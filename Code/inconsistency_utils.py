# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.14.4
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

import pandas as pd
import numpy as np
import pyarrow as pa
import pyarrow.parquet as pq
import geopandas as gpd
from scipy import stats
from scipy.stats import skew, kurtosis
import pygris
from shapely.geometry import Polygon
import shapely


def process_geographic_data(df, drop_duplicate_set, drop_na_set):
    # Dropping duplicates based on specified columns
    df_geographic_unique = df[drop_duplicate_set].drop_duplicates()
    
    # Dropping rows with missing values in specified columns
    df_geographic_unique = df_geographic_unique.dropna(subset=drop_na_set)
    
    return df_geographic_unique


def format_geographic_units(df, columns_to_format):
    for column, length in columns_to_format.items():
        if column in df:
            # Convert to int then to string, handling NaN values
            df[column] = df[column].dropna().apply(lambda x: str(int(float(x))))

            # Zero-fill to the specified length
            df[column] = df[column].apply(lambda x: x.zfill(length))
    
    return df


def process_year_of_loss(df, bins_1980_2021, labels_1980_2021, custom_bins, custom_labels):

    df['yearOfLoss_1980_2021'] = pd.cut(df['yearOfLoss'], bins=bins_1980_2021, labels=labels_1980_2021, right=False).astype(int)

    # Assert to ensure correct number of labels and bins for custom binning
    assert len(custom_bins) == len(custom_labels) + 1, "Number of bin labels must be one fewer than the number of bin edges."

    # Apply custom binning to create 'zip_year_bin'
    df['zip_year_bin'] = pd.cut(
        df['yearOfLoss'],
        bins=custom_bins,
        labels=custom_labels,
        right=False,
        include_lowest=True
    ).astype(int)

    # Remove duplicates
    df = df.drop_duplicates()

    return df


def load_and_rename_geographic_data_whole(units, base_path):
    dfs = {}

    for unit in units:
        if unit == 'state':
            states = pygris.states()
            state_df = states[['STUSPS', 'NAME', 'geometry']]
            state_df.rename(columns={'geometry': 'geometry_state'}, inplace=True)
            dfs[unit] = state_df

        elif unit == 'lat_long':
            df_read = pd.read_parquet(f"{base_path}lat_long_geometry.parquet.gzip")
            lat_long_df = gpd.GeoDataFrame(df_read, geometry=df_read['geometry'].apply(shapely.wkt.loads))
            lat_long_df.rename(columns={'geometry': 'geometry_lat_long'}, inplace=True)
            dfs[unit] = lat_long_df

        elif unit == 'zipcode':

            chunk_size = 50000
            chunks = [x for x in range(0, 300000, chunk_size)]
            gdf_list = []

            for start in chunks:
                end = start + chunk_size
                temp_df = pd.read_parquet(f"{base_path}zipcode_geometry_{start}_{end}.parquet.gzip")
                gdf_read = gpd.GeoDataFrame(temp_df, geometry=temp_df['geometry'].apply(lambda x: shapely.wkt.loads(x)))
                gdf_list.append(gdf_read)

            gdf1 = pd.concat(gdf_list, ignore_index=True)

            df2 = pd.read_parquet(f"{base_path}zipcode_geometry_2011_union.parquet.gzip")
            df2 = df2.drop(columns='flag')

            gdf2 = gpd.GeoDataFrame(df2, geometry=df2['geometry'].apply(shapely.wkt.loads))
            
            gdf = pd.concat([gdf1, gdf2], ignore_index=True)
            gdf.rename(columns={'geometry': 'geometry_zipcode'}, inplace=True)
            dfs[unit] = gdf

        elif unit == 'County':
            df = pd.read_parquet(f"{base_path}County_geometry.parquet.gzip")
            gdf = gpd.GeoDataFrame(df, geometry=df['geometry'].apply(shapely.wkt.loads))
            gdf.rename(columns={'geometry': 'geometry_county'}, inplace=True)
            dfs[unit] = gdf

        elif unit == 'BG':

            chunk_size = 40000 
            chunks = [x for x in range(0, 120000, chunk_size)]

            gdf_list = []

            for start in chunks:
                end = start + chunk_size
                temp_df = pd.read_parquet(f"{base_path}BG_geometry_{start}_{end}.parquet.gzip")
                gdf_read = gpd.GeoDataFrame(temp_df, geometry=temp_df['geometry'].apply(lambda x: shapely.wkt.loads(x)))
                gdf_list.append(gdf_read)
            
            gdf = pd.concat(gdf_list, ignore_index=True)
            gdf.rename(columns={'geometry': 'geometry_BG'}, inplace=True)
            dfs[unit] = gdf

        elif unit == 'Tract':
            chunk_size = 30000 
            chunks = [x for x in range(0, 60000, chunk_size)]

            gdf_list = []

            for start in chunks:
                end = start + chunk_size
                temp_df = pd.read_parquet(f"{base_path}Tract_geometry_{start}_{end}.parquet.gzip")
                gdf_read = gpd.GeoDataFrame(temp_df, geometry=temp_df['geometry'].apply(lambda x: shapely.wkt.loads(x)))
                gdf_list.append(gdf_read)

            gdf = pd.concat(gdf_list, ignore_index=True)
            gdf.rename(columns={'geometry': 'geometry_tract'}, inplace=True)
            dfs[unit] = gdf

    return dfs
