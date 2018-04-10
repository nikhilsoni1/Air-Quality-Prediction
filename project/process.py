import os
import pandas as pd


code = {'AnandVihar': 'AV',
        'AshokVihar': 'REM',
        'ayanagar': 'AYA',
        'burari': 'BC',
        'crri': 'CRRI',
        'dtu': 'DTU',
        'dwarka': 'NSIT',
        'ean': 'EAN',
        'igi': 'IGI',
        'ihbas': 'IHBAS',
        'ITO': 'ITO',
        'karni': 'REM',
        'LodhiRoad': 'LODHI',
        'northcampus': 'NC',
        'punjabibagh': 'PB',
        'pusa': 'PUSA',
        'rkpuram': 'RKP',
        'Rohini': 'REM',
        'Shadipur': 'SHAD',
        'SiriFort': 'SIRI',
        'SoniaVihar': 'REM',
        'VivekVihar': 'REM',
        'MandirMarg': 'MM'}

def getfname():
    dir_path = os.path.dirname(os.path.realpath(__file__))
    files_raw = os.listdir(dir_path)
    files = []
    for ctr in files_raw:
        if '.csv' in ctr:
            files.append(ctr)
    return files

store = []
for ctr in getfname():
    temp = pd.read_csv(ctr)
    try:
        temp.insert(0, 'Station', code[ctr.split('.')[0]])
        store.append(temp)
    except KeyError:
        pass
    temp = None
df = pd.concat(store)
df = df[df.Station != 'REM']
df = df.reset_index(drop=True)
print(df.shape)
print(df.head(3))
print(df.tail(3))
df.to_csv('../AOT.csv', index=False)
