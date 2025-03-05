log: main.cpp ../include/elog.hpp ../elog.cpp
	c++ -Os -std=c++20 -I../include main.cpp -o log
	c++ -O0 -std=c++20 -g -I../include main.cpp -o logd

analyzer: analyzer.cpp ../include/elog.hpp ../elog.cpp
	c++ -Os -std=c++20 -I../include analyzer.cpp -o analyzer


clean:
	-rm log
