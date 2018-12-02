//	25 ms

#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>

#include <chrono>

using namespace std;

int diff(const string &a, const string &b)
{
	//	assume a and b have the same length
	int ret = 0;
	for (int i = 0; i < a.size(); i++)
	{
		if (a[i] != b[i])
			ret++;
	}
	
	return ret;
}


int main()
{
	ifstream file("/Users/Technist/Desktop/aoc/input/input4.txt");
	if (!file.is_open())
	{
		cout << "File couldn't open.\n";
		return 1;
	}
	auto start = chrono::steady_clock::now();
	
	vector<string> hold;
	int found = 0;
	
	string buffer;
	while (getline(file, buffer) && found == 0)
	{
		
		for (int i = 0; i < hold.size(); i++)
			if (diff(hold[i], buffer) == 1)
			{
				found = i;
				break;
			}
		
		hold.push_back(buffer);
	}
	
	auto end = chrono::steady_clock::now();
	
	cout << "Found Original: " << hold[found] << endl;
	cout << "And New: " << hold.back() << endl << endl;
	cout << "Time Taken: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms\n"; 
	
	return 0;
}
