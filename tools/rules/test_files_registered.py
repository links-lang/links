#!/usr/bin/env python3

import glob, os, re, sys

TEST_DIR="tests"
BLACKLIST_STRING="#IGNORED_BY_TEST_REGISTRATION_RULE_CHECKING_SCRIPT"


links_files = set()

#Collect .links files that should appear in a ,tests file
for links_file in glob.glob(os.path.join(TEST_DIR, "**", "*.links"), recursive=True):
    with open(links_file) as f:
        first_line = f.readline().strip()
        if(first_line != BLACKLIST_STRING):
            normed = os.path.normpath(links_file)
            links_files.add(normed)
            #print("normed : %s" % normed)


#Iterate all .tests files, looking for the .links files accumulated above
for test_file in glob.glob(os.path.join(TEST_DIR, "**", "*.tests"), recursive=True):
    with open(test_file, "r") as f:

        #At which line of a test case defintion are we?
        test_offset = 0

        for line in f:
            line = line.strip()

            if line == "":
                test_offset = 0
            else:
                test_offset += 1

            #If this is the second line in a test case def, it may be the path of a  .links file
            if(test_offset == 2 and re.match(".*\.links", line)):
                normed = os.path.normpath(line)
                #print("normed: %s" % normed)
                if normed in links_files:
                    #print("found %s" %line)
                    links_files.remove(normed)
            prev_line = line


#Those .links files remaining in test_files are the ones we haven't found a .tests file for
if links_files:
    print("Error: The following .links files in the %s subfolder are neither mentioned in a .tests files nor do they start with \'%s\'" % (TEST_DIR, BLACKLIST_STRING))
    for f in links_files:
        print(f)
    sys.exit(1)
