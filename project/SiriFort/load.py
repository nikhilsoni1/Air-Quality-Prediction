import os
from pyhdf.SD import SD, SDC
import pandas as pd
import numpy as np


def getfname():
    dir_path = os.path.dirname(os.path.realpath(__file__))
    files_raw = os.listdir(dir_path)
    files = []
    for ctr in files_raw:
        if '.hdf' in ctr:
            files.append(ctr)
    return files


def key_at_depth(dct, dpt):
    if dpt > 0:
        return [key for subdct in dct.itervalues() for key in key_at_depth(subdct, dpt - 1)]
    else:
        return dct.keys()


def extract(flist):
    keys = ['Scan_Start_Time', 'Aerosol_Type_Land']
    hdf = SD(flist[0], SDC.READ)
    a1 = hdf.select(keys[0]).get().flatten()
    a2 = hdf.select(keys[1]).get().flatten()
    status = 0
    for counter in flist[1:]:
        hdf = SD(counter, SDC.READ)
        status = status + 1
        print(status)
        a1 = np.concatenate([a1, hdf.select(keys[0]).get().flatten()])
        a2 = np.concatenate([a2, hdf.select(keys[1]).get().flatten()])
    df = pd.DataFrame({'Scan_Start_Time': a1,
                       'Aerosol_Type_Land': a2})
    df = df[df.Aerosol_Type_Land != -9999]
    df = df.reset_index(drop=True)
    df['Epoch'] = pd.Timestamp('19930101')
    df['Date'] = pd.to_datetime(df['Epoch']) + pd.to_timedelta(df['Scan_Start_Time'], unit='s')
    df['Date'] = df['Date'].dt.date
    df = df.drop(['Epoch', 'Scan_Start_Time'], axis=1)
    df = df[['Date', 'Aerosol_Type_Land']]
    df = df.groupby(['Date'])['Aerosol_Type_Land'].mean().to_frame()
    df['Date'] = df.index
    df = df[['Date', 'Aerosol_Type_Land']]
    df = df.reset_index(drop=True)
    return df


extract(getfname()).to_csv('Data.csv', index=False)



