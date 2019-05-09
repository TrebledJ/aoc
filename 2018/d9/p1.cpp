#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <queue>
#include <list>
#include <map>
#include <chrono>
#include <unordered_map>

//	the node to be removed is always to the left of the current node minus 3 * 2
//		i = 23
//		three * 2 to the left = 22 - 3 = 19
//		left of 19 is 9

#define INT u_int64_t

int main() {
	//	ifstream file("/Users/JLAW/Desktop/aoc/2018/d9/input.txt");
	//	if (!file.is_open())
	//	{
	//		cout << "File couldn't open.\n";
	//		return 1;
	//	}
	
	INT players = 438;
	INT marbles = 71626 * 100;		//	7,162,600
									//	int players = 7;
									//	int marbles = 2000;
	std::list<INT> hold;
	std::map<INT, INT> scores;
	
	hold.push_back(0);
	hold.push_back(1);
	
	//	std::ofstream oFile("/Users/JLAW/Desktop/aocd9.md");
	//	if (!oFile.is_open())
	//	{
	//		std::cout << "oops\n";
	//		return 1;
	//	}
	
	for (INT i = 2,
		 marblePos = 1,
		 currentPlayer = 1;		//	0 to 437
		 i <= marbles;
		 i++, currentPlayer++)
	{
		//		std::cout << "Current Marble Pos: " << marblePos << " has value (" << *std::next(hold.begin(), marblePos) << ")\n"; 
		
		if (i % 23 == 0)
		{
			marblePos = (unsigned(marblePos - 7 + hold.size()) % hold.size());
			
			auto it = std::next(hold.begin(), marblePos);
			
			//			oFile << (i/23) << " :: Marble " << i << " : + : " << *it << '\n';
			
			//			std::cout << "Removing marble " << *it << " at position " << marblePos << "\n"; 
			scores[currentPlayer] += i + *it;
			hold.erase(it);
		}
		else
		{
			marblePos = (marblePos + 2) % (hold.size());
			if (marblePos == 0)
			{
				marblePos = INT(hold.size());
				hold.insert(hold.end(), i);
			}
			else
				hold.insert(std::next(hold.begin(), marblePos), i);
		}
		
		if (currentPlayer >= players)
			currentPlayer %= players;
		
		if (i % 10000 == 0)
		{
			std::cout << "Checkpoint: " << i << '\n';
		}
		//		std::cout << "\n\nContents:\n";
		
		//		int counter = 0;
		//		for (auto it = hold.begin(); it != hold.end(); ++it, ++counter)
		//		{
		//			oFile << " ";
		//			if (counter == marblePos) oFile << "(";
		//			oFile << *it;
		//			if (counter == marblePos) oFile << ")";
		//		}
		//		oFile << "\n";
		
	}
	
	std::cout << "Done" << std::endl;
	
	auto it = std::max_element(scores.begin(), scores.end(),
							   [](const std::pair<INT, INT>& p1, const std::pair<INT, INT>& p2) {
								   return p1.second < p2.second; });
	std::cout << "Highest Score: " << it->second << std::endl;
	
}

