//	10 ms

#include <iostream>
#include <map>
#include <string>
#include <fstream>

#include <chrono>

using namespace std;

int main()
{
	ifstream file("input.txt");
	if (!file.is_open())
	{
		cout << "File couldn't open.\n";
		return 1;
	}
	auto start = chrono::steady_clock::now();
	
	int64_t numTwos = 0, numThrees = 0;
	
	string buffer;
	while (getline(file, buffer))
	{
		map<char, int> counter;
		int isTwo = 0, isThree = 0;
		for (const char &c : buffer)
		{
			counter[c] += 1;
			if (counter[c] == 2)
				isTwo++;
			if (counter[c] == 3)
			{
				isTwo--;
				isThree++;
			}
			if (counter[c] == 4)
				isThree--;
		}
		
		if (isTwo > 0)
			numTwos++;
		if(isThree > 0)
			numThrees++;
	}
	
	auto end = chrono::steady_clock::now();
	
	cout << "Number of Twos: " << numTwos << endl;
	cout << "Number of Threes: " << numThrees << endl;
	
	cout << "Checksum: " << numTwos * numThrees << endl << endl;
	cout << "Time Taken: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms\n";
	
	return 0;
}
