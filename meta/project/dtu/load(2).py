import os
from pyhdf.SD import SD, SDC
import pyhdf
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


# def key_at_depth(dct, dpt):
#     if dpt > 0:
#         return [key for subdct in dct.itervalues() for key in key_at_depth(subdct, dpt - 1)]
#     else:
#         return dct.keys()


def cleaner(inp_df):
    inp_df = inp_df[inp_df.Aerosol_Type_Land != -9999]
    inp_df = inp_df.reset_index(drop=True)
    inp_df['Epoch'] = pd.Timestamp('19930101')
    inp_df['Date'] = pd.to_datetime(inp_df['Epoch']) + pd.to_timedelta(inp_df['Scan_Start_Time'], unit='s')
    inp_df['Date'] = inp_df['Date'].dt.date
    inp_df = inp_df.drop(['Epoch', 'Scan_Start_Time'], axis=1)
    inp_df = inp_df[['Date', 'Aerosol_Type_Land']]
    inp_df = inp_df.groupby(['Date'])['Aerosol_Type_Land'].mean().to_frame()
    inp_df['Date'] = inp_df.index
    inp_df = inp_df[['Date', 'Aerosol_Type_Land']]
    inp_df = inp_df.reset_index(drop=True)
    return inp_df


def extract(flist):
    keys = ['Scan_Start_Time', 'Aerosol_Type_Land']
    container = pd.DataFrame(columns=['Date', 'Aerosol_Type_Land'])
    i = 0
    status = 0
    print(len(flist))
    while i <= len(flist):
        try:
            hdf = SD(flist[i], SDC.READ)
            # print(flist[i])
            status = status + 1
            print(status)
            a1 = hdf.select(keys[0]).get().flatten()
            a2 = hdf.select(keys[1]).get().flatten()
        except pyhdf.error.HDF4Error:
            print(flist[i])
            print('Handled Bitch')
            a1 = np.array([0])
            a2 = np.array([0])
        for counter in flist[i+1:i+6]:
            # print(counter)
            status = status + 1
            print(status)
            try:
                hdf = SD(counter, SDC.READ)
                a1 = np.concatenate([a1, hdf.select(keys[0]).get().flatten()])
                a2 = np.concatenate([a2, hdf.select(keys[1]).get().flatten()])
            except pyhdf.error.HDF4Error:
                print(counter)
                print('Fuck Yeah!')
        df = pd.DataFrame({'Scan_Start_Time': a1,
                           'Aerosol_Type_Land': a2})
        container = pd.concat([container, cleaner(df)])
        a1 = None
        a2 = None
        df = None
        i = i + 6
        # print('---')
        # print(i)
        # print('---')
    print(container.head(5))
    print(container.shape)
    return container


df_csv = extract(getfname())
df_csv.to_csv('Data.csv', index=False)




