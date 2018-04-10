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
    err = []
    while i <= len(flist):
        a1 = np.array([0])
        a2 = np.array([0])
        for counter in flist[i:i+10]:
            # print(counter)
            try:
                hdf = SD(counter, SDC.READ)
                status = status + 1
                print(str(status) + '/'+ str(len(flist)))
                a1 = np.concatenate([a1, hdf.select(keys[0]).get().flatten()])
                a2 = np.concatenate([a2, hdf.select(keys[1]).get().flatten()])
            except pyhdf.error.HDF4Error:
                err.append(counter)
                status = status + 1
                print(status, ' >>> ', counter, ' <<< ', status)
                # print('Fuck Yeah!')
        df = pd.DataFrame({'Scan_Start_Time': a1,
                           'Aerosol_Type_Land': a2})
        container = pd.concat([container, cleaner(df)])
        i = i + 10
    print(container.head(5))
    print(container.shape)
    if err:
        err_df = pd.DataFrame({'Filename': err})
        err_df.to_csv('Errors.csv', index=False)
    return container


df_csv = extract(getfname())
df_csv.to_csv('Data.csv', index=False)




