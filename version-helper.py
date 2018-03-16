#!/usr/bin/env python

import re
matcher=re.compile(""".*Version := "([0-9.]+)";""")
with file("spawn_manager.gpr") as l:
  for i in l:
    matches=matcher.match(i)
    if matches:
       print matches.group(1)
