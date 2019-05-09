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
#include <thread>
#define INT u_int64_t

class Point
{
public:
	int x, y;
	Point() : x(0), y(0)
	{}
	Point(int _x, int _y) : x(_x), y(_y)
	{}
	
	Point operator+ (const Point &other) const
	{
		return {x+other.x, y+other.y};
	}
	
	Point& operator+= (const Point &other)
	{
		x += other.x;
		y += other.y;
		return *this;
	}
};

class Star
{
public:
	Point position;
	Point velocity;
	
	Star() =default;
	Star(int pos_x, int pos_y, int vel_x, int vel_y) : position(Point(pos_x, pos_y)), velocity(Point(vel_x, vel_y))
	{}
	
	void move()
	{
		position += velocity;
	}
};

class Map
{
public:
	std::vector<std::vector<char>> map;
	std::vector<Star> stars;
	
	int left = 0;
	int right = 0;
	int top = 0;
	int bottom = 0;
	int width = 0;
	int height = 0;
	
	void setTile(const Point &point, char c)
	{
		//		map[point.y + (height/2)][point.x + (width/2)] = c;
		map[point.y - top][point.x - left] = c;
	}
	
	void allocate(int width, int height)
	{
		map.assign(height, std::vector<char>(width, '.'));
	}
	
	void setStars(const std::vector<Star> &_stars)
	{
		stars = _stars;
	}
	
	void hide()
	{
		for (int i = 0; i < stars.size(); i++)
			setTile(stars[i].position, '.');
	}
	
	void show()
	{
		for (int i = 0; i < stars.size(); i++)
			setTile(stars[i].position, '#');
	}
	
	void update()
	{
		if (!map.empty())
			hide();
		
		left = right = top = bottom = 0;
		for (int i = 0; i < stars.size(); i++)
		{
			stars[i].move();
			
			if (stars[i].position.x < left)
				left = stars[i].position.x;
			if (right < stars[i].position.x)
				right = stars[i].position.x;
			if (stars[i].position.y < top)
				top = stars[i].position.y;
			if (bottom < stars[i].position.y)
				bottom = stars[i].position.y;
		}
		width = right - left + 1;
		height = bottom - top + 1;
		
		std::cout << "[UPDATE] ::: " << width << " x " << height << " (" << left << ", " << right << ", " << top << ", " << bottom << ")" << std::endl;
		
		if (width < 2000 && height < 2000)
		{
			map.clear();
			allocate(width, height);
			show();
		}
	}
	
	void print() const
	{
		std::cout << std::endl;
		for (int i = 0; i < map.size(); i++)
		{
			for (int j = 0; j < map[i].size(); j++)
				std::cout << " " << map[i][j];
			std::cout << std::endl;
		}
		
		std::cout << std::endl;
	}
};


int main() {
	std::ifstream file("/Users/JLAW/Documents/Github/aoc/2018/d10/input.txt");
	if (!file.is_open())
	{
		std::cout << "File couldn't open.\n";
		return 1;
	}
	
	std::string buff;
	
	std::regex regex("position=<[ ]*(.*),[ ]*(.*)> velocity=<[ ]*(.*),[ ]*(.*)>");
	std::match_results<std::string::const_iterator> match;
	
	int left, right, top, bottom;
	left = right = top = bottom = 0;
	
	std::vector<Star> stars;
	
	while (getline(file, buff))
	{
		//		std::cout << "[READ] ::: " << buff << std::endl;
		if (std::regex_match(buff, match, regex))
		{
			//			std::cout << "[MATCH] ::: " << match[1] << " " << match[2] << std::endl;
			Star star(std::stoi(match[1]),
					  std::stoi(match[2]),
					  std::stoi(match[3]),
					  std::stoi(match[4]));
			
			stars.push_back(star);
			
			//			if (star.position.x < left)
			//				left = star.position.x;
			//			if (right < star.position.x)
			//				right = star.position.x;
			//			if (star.position.y < top)
			//				top = star.position.y;
			//			if (bottom < star.position.y)
			//				bottom = star.position.y;
		}
	}
	
	//	int width = right - left + 1;
	//	int height = bottom - top + 1;
	
	//	std::cout << "[DONE] width X height ::: " << width << " x " << height << std::endl;
	
	Map map;
	map.setStars(stars);
	//	map.allocate(width, height);
	//	map.show();
	map.update();
	map.print();
	
	INT iter = 0;
	while (1)
	{
		iter++;
		map.update();
		
		if (iter % 100 == 0)
			map.print();
		
		std::this_thread::sleep_for(std::chrono::milliseconds(10));
	}
	
	
	
}

