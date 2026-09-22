import subprocess
import sys


version=subprocess.check_output(["bin/helper", "-v"]).decode("UTF-8").strip()
date=subprocess.check_output(["date", "+%Y%m%d"]).decode("UTF-8").strip()
tags=subprocess.check_output(["git", "tag"]).decode("UTF-8").strip().split("\n")
tag=version + "-" + date

fail=False

if len(subprocess.check_output(["git", "status", "--porcelain"]).decode("UTF-8").strip()) > 0:
    print("Folder not clean!")
    print(subprocess.check_output(["git", "status", "--porcelain"]).decode("UTF-8").strip())
    print("\n")
    fail=True

for i in tags:    
    if version in i:
        print (f"Tag {version} already exists.")
        print("\n")
        fail=True
        
with open("README.md") as inf:
    readme=inf.read()
    if tag not in readme:
        print (f"Tag  {tag} not in 'README.md'.")
        print("\n")
        fail=True
    
if not fail:
    subprocess.call(["git","tag",tag])
    subprocess.call(["git","push"])
    subprocess.call(["git","push","--tag"])
    sys.exit(0)
else:    
    sys.exit(1)
            

#print(version)
#print(date)
#print(tags)
