from os.path import join, basename, sep, dirname, abspath
from os import walk, rename
import shutil

j = 0

for root, dirs, files in walk("."):
    path = root.split(sep)
    for file in files:
        if 'csv' in file:
            j = j + 1
            if join('./' + path[1], file) != join('./' + path[1], basename(root) + '.csv'):
                print(join('./' + path[1], file))
                print(join('./' + path[1], basename(root) + '.csv'))
                rename(join('./' + path[1], file), join('./' + path[1], basename(root) + '.csv'))

print(dirname(abspath(__file__)))

src_dir = dirname(abspath(__file__))
dst_dir = dirname(abspath(__file__))
for root, dirs, files in walk(src_dir):
    for f in files:
        if f.endswith('.csv'):
            shutil.copy(join(root, f), dst_dir)
