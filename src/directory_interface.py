import os
import re
import argparse

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

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('directory', help='Directory to search in')
    args = parser.parse_args()

    regex = '(theorem|lemma)'
    #return count_occurrences(args.directory, regex)
    print(f'Count of occurrences of "{regex}" in "{args.directory}":', count_occurrences(args.directory, regex))

if __name__ == '__main__':
    main()
    