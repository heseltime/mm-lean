import os
import re

def count_files(path, regex):
    regObj = re.compile(regex)
    ct = 0
    for root, dirs, fnames in os.walk(path):
        for fname in fnames:
            file_path = os.path.join(root, fname)
            with open(file_path, 'r', errors='ignore') as f:
                content = f.read()
                if regObj.search(content):
                    ct += 1
    return ct


def count_occurrences(path, regex):
    regObj = re.compile(regex)
    count = 0
    for root, dirs, fnames in os.walk(path):
        for fname in fnames:
            file_path = os.path.join(root, fname)
            with open(file_path, 'r', errors='ignore') as f:
                content = f.read()
                matches = regObj.findall(content)
                count += len(matches)
    return count

print(count_occurrences('C:\\Public\\WSS2023\\mathlib\\src', '(theorem|lemma)'))

# make callable with dir argument?