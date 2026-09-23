import subprocess
import sys


version = subprocess.check_output(["bin/helper", "-v"]).decode("UTF-8").strip()
date = subprocess.check_output(["date", "+%Y%m%d"]).decode("UTF-8").strip()
tags = subprocess.check_output(["git", "tag"]).decode("UTF-8").strip().split("\n")
tag = version + "-" + date

fail = False


def check_output(cmd):
    return subprocess.check_output(cmd).decode("UTF-8").strip()

    
if len(check_output(["git", "status", "--porcelain"])) > 0:
    print("Folder not clean!")
    print(check_output(["git", "status", "--porcelain"]))
    print("\n")
    fail = True

for i in tags:    
    if version in i:
        print(f"Tag {version} already exists.")
        print("\n")
        fail = True
        
with open("README.md") as inf:
    readme = inf.read()
    if tag not in readme:
        print(f"Tag  {tag} not in 'README.md'.")
        print("\n")
        fail = True
    
if not fail:
    subprocess.call(["git", "tag", tag])
    subprocess.call(["git", "push"])
    subprocess.call(["git", "push", "--tag"])
    sys.exit(0)
else:    
    sys.exit(1)
