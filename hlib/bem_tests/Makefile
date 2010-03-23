bar: bar_example.c
	gcc -o bar_example.out -I/home/$(USER)/HLib-1.3p16/Library/  -L/home/$(USER)/HLib-1.3p16/Library/.libs  -lhmatrix-1.3 -lm -llapack bar_example.c


cube: cube_example.c
	gcc -o cube_example.out -I/home/$(USER)/HLib-1.3/Library  -L/home/$(USER)/HLib-1.3/Library/.libs  -lhmatrix-1.3 -lm -llapack cube_example.c
#the variable LD_LIBRARY_PATH has to include path /home/$(USER)/HLib-1.3/Library/.libs

sphere: sphere_example.c
	gcc -o sphere_example.out -Wall -I/home/$(USER)/HLib-1.3/Library  -L/home/$(USER)/HLib-1.3/Library/.libs  -lhmatrix-1.3 -lm -llapack sphere_example.c

sphere_parallel: sphere_parallel.c
	gcc -o sphere_parallel.out -Wall -I/home/$(USER)/HLib-1.3/Library  -L/home/$(USER)/HLib-1.3/Library/.libs  -lhmatrix-1.3 -lm -llapack sphere_parallel.c
