#!/usr/bin/env python

import os
from os import path
import subprocess
import sys
import time

FILEDIR = path.dirname(path.abspath(__file__))

def abspath(*args):
    return path.join(FILEDIR, *args)

CORPUS_DIR = abspath("calgary")
CORPUS_LIST = abspath("calgary", "corpus.txt")

def load_corpus(listfile=CORPUS_LIST):
    with open(listfile, 'r') as infile:
        return [abspath("calgary", line.strip()) for line in infile]

def bwt(filename, blocksize=900000):
    out = filename + ".bwt"
    cmd = "%s -b %d -c < %s > %s" % (abspath("bwt"), blocksize, filename, out)
    os.system(cmd)
    return out

def bzip2(filename, blocksize=9):
    out = filename + ".bz2"
    bzip2path = path.join("/","opt", "local", "bin", "bzip2")
    subprocess.check_call((bzip2path, "--keep", "--force", "--best", filename))
    return out

def gzip(filename):
    out = filename + ".gz"
    gzippath = path.join("/", "opt", "local", "bin", "gzip")
    os.system(" ".join([gzippath, "--force", "--best", "-c", filename, ">",
                       out]))
    return out

BENCHMARKS = [gzip, bzip2, bwt]

def filesize(filename):
    return float(os.stat(filename).st_size)

def run_benchmark(filename):
    fsize = filesize(filename)
    results = []
    for bmfn in BENCHMARKS:
        t = time.time()
        outname = bmfn(filename)
        timediff = time.time() - t
        sizeratio = filesize(outname)/fsize
        compressionSpeed = fsize/timediff
        results.append((timediff, sizeratio, compressionSpeed))
    return results

def run_all(filenames):
    skip = set(["geo", "pic", "obj1", "obj2"])
    for filename in filenames:
        base = path.basename(filename)
        if base in skip:
            continue
        print base
        for fn,result in zip(BENCHMARKS, run_benchmark(filename)):
            stringify = lambda s: "%.03f" % s
            resultString = "\t".join(map(stringify,result))
            print "\t", "%s:\t" % fn.func_name, resultString

def main(argv):
    corpus = load_corpus()
    run_all(corpus)
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
