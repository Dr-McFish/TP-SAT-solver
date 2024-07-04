# This file uses the real sat solver `minisat instead of my homespun version`

import subprocess
import numpy
import matplotlib.pyplot as plt
from timeit import default_timer

v = 5
def test_random_instance(n,p):
	random_instance = subprocess.run(["./_build/install/default/bin/tp", f"{v}", f"{n}", f"{p}"], 
                                  stdout=subprocess.PIPE)
	satifiability = subprocess.run(["minisat"], input=random_instance.stdout, stdout=subprocess.DEVNULL)
	# print(random_instance.stdout)
	return(10 == satifiability.returncode)
	
reasonable_number = 50
def proportion_repeated_test(n,p):
	num_good =0
	for i in range(0, reasonable_number) :
		if test_random_instance(n,p) :
			num_good += 1
	return(num_good/reasonable_number)

start_date = default_timer()
table = [[proportion_repeated_test(n,p) for n in range(1, 20)] for p in range(1, 20)]
print(f"took {default_timer() - start_date}s", )

plt.imshow(table, cmap='hot', interpolation='nearest')
plt.xlabel ('n')
plt.gca().invert_yaxis()
plt.ylabel ('p')
plt.show()