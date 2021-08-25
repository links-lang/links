#!/usr/bin/env python3

import glob, json, os, re, sys

TEST_DIR="tests"

# The following files are excluded from being tested,
# because they are not entry points to some test
BLACKLIST = {
  "tests/empty_prelude.links",
  "tests/freezeml_prelude.links",
  "tests/presence_type_arg_typename.links",
  "tests/modules/alien_blocks_dependency.links",
  "tests/modules/cyclicA.links",
  "tests/modules/cyclicB.links",
  "tests/modules/cyclicC.links",
  "tests/modules/import0.links",
  "tests/modules/import_via_open1.links",
  "tests/modules/moduleB.links",
  "tests/modules/moduleC.links",
  "tests/modules/open_is_not_include0.links",
  "tests/modules/open_is_not_include1.links",
  "tests/modules/overflow-test/a0.links",
  "tests/modules/overflow-test/a1.links",
  "tests/modules/overflow-test/a10.links",
  "tests/modules/overflow-test/a11.links",
  "tests/modules/overflow-test/a12.links",
  "tests/modules/overflow-test/a13.links",
  "tests/modules/overflow-test/a14.links",
  "tests/modules/overflow-test/a2.links",
  "tests/modules/overflow-test/a3.links",
  "tests/modules/overflow-test/a4.links",
  "tests/modules/overflow-test/a5.links",
  "tests/modules/overflow-test/a6.links",
  "tests/modules/overflow-test/a7.links",
  "tests/modules/overflow-test/a8.links",
  "tests/modules/overflow-test/a9.links",
  "tests/modules/varRefB.links",
  "tests/session-exceptions/cancel5-cp.links",
  "tests/session-exceptions/clientCancel1.links",
  "tests/session-exceptions/clientCancel10.links",
  "tests/session-exceptions/clientCancel11.links",
  "tests/session-exceptions/clientCancel2.links",
  "tests/session-exceptions/clientCancel3.links",
  "tests/session-exceptions/clientCancel4.links",
  "tests/session-exceptions/clientCancel5.links",
  "tests/session-exceptions/clientCancel6.links",
  "tests/session-exceptions/clientCancel7.links",
  "tests/session-exceptions/clientCancel8.links",
  "tests/session-exceptions/clientCancel9.links",
  "tests/session-exceptions/clientClosure.links",
  "tests/session-exceptions/clientClosure2.links",
 }



links_files = set()

#Collect .links files that should appear in a ,tests file
for links_file in glob.glob(os.path.join(TEST_DIR, "**", "*.links"), recursive=True):
    with open(links_file) as f:
        normed = os.path.normpath(links_file)
        first_line = f.readline().strip()
        if(normed not in BLACKLIST):
            links_files.add(normed)


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
                if normed in links_files:
                    links_files.remove(normed)
            prev_line = line


#Iterate testsuite.config files, containing paths (relative from their location) to database tests
for testsuite_file in glob.glob(os.path.join(TEST_DIR, "**", "testsuite.json"), recursive=True):
    path_dir = os.path.dirname(testsuite_file)

    with open(testsuite_file, "r") as f:
        config = json.load(f)
        for line in config['tests']:
            path_file = line.strip() + ".links"

            combined_path = os.path.join(path_dir,path_file)
            normed = normed = os.path.normpath(combined_path)
            if normed in links_files:
                    links_files.remove(normed)



#Those .links files remaining in test_files are the ones we haven't found a .tests file for
if links_files:
    print("Error: The following .links files in the %s subfolder are neither mentioned in a .tests files nor are they part of the blacklist in this script" % TEST_DIR)
    for f in sorted(links_files):
        print(f)
    sys.exit(1)
