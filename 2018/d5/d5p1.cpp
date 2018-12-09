#include <iostream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <string>
#include <fstream>

using namespace std;

int main()
{
	
	ifstream in("input.txt");
	
	if (!in.is_open())
	{
		cout << "File couldn't open!\n";
		return 1;
	}
	string buff;
	getline(in, buff);
	
	while (1)
	{
		cout << "Loop: " << buff << endl;
		
		int changes = 0;
		for (int i = 1; i < buff.size(); i++)
		{
			if (isupper(buff[i - 1]) == islower(buff[i]) && tolower(buff[i - 1]) == tolower(buff[i]))
			{
				changes++;
				buff.erase(buff.begin() + i - 1, buff.begin() + i + 1);
				i -= 2;
				
			}
		}
		if (changes == 0)
			break;
	}
	
	cout << buff << endl << endl;
	cout << "Length: " << buff.size() << endl;
	
	
	return 0;
}




