//	25 ms

#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <queue>
#include <map>
#include <chrono>

using namespace std;

using Node = string;
using Edge = pair<Node, Node>;

bool priority(const Node &a, const Node &b)
{
	return a > b;
}

template<typename T>
bool find(const vector<T> &vec, const T &element)
{
	return find(vec.begin(), vec.end(), element) != vec.end();
}

int main()
{
	ifstream file("/Users/JLAW/Desktop/aoc/input/input13.txt");
	//	ifstream file("/Users/JLAW/Desktop/aoc/input/input13 copy.txt");
	if (!file.is_open())
	{
		cout << "File couldn't open.\n";
		return 1;
	}
	auto start = chrono::steady_clock::now();
	
	vector<Edge> edges;
	map<Node, int> inDegrees;
	map<Node, int> outDegrees;
	string buff;
	regex reg("Step (\\w+) must be finished before step (\\w+) can begin.");
	match_results<Node::const_iterator> match;
	while (getline(file, buff))
	{
		if (regex_match(buff, match, reg))
		{
			edges.push_back(make_pair(match[1], match[2]));
			outDegrees[match[1]]++;
			if (inDegrees[match[1]]);	//	activate zero
			inDegrees[match[2]]++;
		}
	}
	
	//	Node home = edges.front().first;
	
	//	vector<Node> assembled;
	
	using pQueue = priority_queue<Node, vector<Node>, function<bool(Node, Node)>>;
	pQueue readyNodes(priority);
	
	for (auto it = inDegrees.begin(); it != inDegrees.end(); it++)
	{
		//	degree
		if (it->second == 0)
			readyNodes.push(it->first);
	}
	
	string res;
	
	while (!readyNodes.empty())
	{
		Node curr = readyNodes.top();
		readyNodes.pop();
		
		//		if (find(assembled, curr))
		//			continue;
		
		//		bool found = false;
		//		for (int i = 0; i < edges.size(); i++)
		//			if (edges[i].second == curr)		//	check if there is another requirement that needs to be fulfilled first
		//			{
		//				found = true;
		//				break;
		//			}
		//		if (found) continue;
		//		
		//		assembled.push_back(curr);
		
		cout << "\nCurr: " << curr << endl;
		
		res += curr;
		
		for (int i = 0; i < edges.size(); i++)
		{
			if (edges[i].first == curr)
			{
				Node next = edges[i].second;
				//				readyNodes.push(next);
				
				inDegrees[next]--;
				
				if (inDegrees[next] == 0)
					readyNodes.push(next);
				
				//				edges.erase(edges.begin() + i);
				//				i--;
				
				cout << "Push " << edges[i].second << endl;
			}
		}
	}
	
	
	auto end = chrono::steady_clock::now();
	
	cout << "Result: " << res << endl;
	
	//	cout << "Found Original: " << hold[found] << endl;
	//	cout << "And New: " << hold.back() << endl << endl;
	cout << "Time Taken: " << chrono::duration_cast<chrono::milliseconds>(end - start).count() << "ms\n"; 
	
	return 0;
}
