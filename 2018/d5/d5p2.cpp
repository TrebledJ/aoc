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
	
	vector<int> counter(26, 0);
	
	string cpyBuff = buff;
	
	for (int i = 0; i < 26; i++)
	{
		buff = cpyBuff;
		
		buff.erase(std::remove_if(buff.begin(), buff.end(), [i](char c){return tolower(c) == 97 + i;}), buff.end());

		while (1)
		{
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
		
		cout << "Size of " << i << " :: " << buff.size() << endl;
		counter[i] = buff.size();
	}
	
	cout << "Min Length: " << *min_element(counter.begin(), counter.end()) << endl;
	
	
	return 0;
}




