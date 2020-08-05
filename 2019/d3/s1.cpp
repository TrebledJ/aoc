#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cassert>


enum Direction { LEFT, UP, RIGHT, DOWN };

Direction parse_dir(char d)
{
    switch (d)
    {
        default:
        case 'L': return LEFT;
        case 'U': return UP;
        case 'R': return RIGHT;
        case 'D': return DOWN;
    }
}

static inline std::pair<Direction, int> parse(std::string const& inst)
{
    return std::make_pair(parse_dir(inst[0]), std::stoul(inst.substr(1)));
}

class Wire
{
public:
    Wire(std::string const& data = "") 
        : data(data), ss(data)
    {
    }

    void reset()
    {
        ss.seekg(ss.beg);
    }
    
    std::pair<Direction, int> next()
    {
        std::string inst;
        std::getline(ss, inst, ',');
        return parse(inst);
    }

    operator bool() { return ss.rdbuf()->in_avail(); }

    friend std::istream& operator>> (std::istream& is, Wire& wire)
    {
        std::string str; std::getline(is, str);
        wire.ss = std::stringstream(str);
        return is;
    }

private:
    std::string data;
    std::stringstream ss;
};

struct Point { int x = 0, y = 0; };
using Line = std::pair<Point, Point>;

/// @brief  Moves the given point in the given direction by an amount
static inline Point move(Point const& point, Direction dir, int len)
{
    static const int dx[] = {-1, 0, +1, 0};
    static const int dy[] = {0, -1, 0, +1};
    return Point{point.x + dx[dir] * len, point.y + dy[dir] * len};
}

/// @brief  Checks if two directions are perpendicular to each other
static inline bool is_perpendicular(Direction a, Direction b)
{
    return (a - b + 4) % 2;
}

#define BETWEEN(a, b, d) ((a) < (d) && (d) < (b))

/**
 * @brief   Checks if a horizontal and vertical line intersect
 * @pre     `horizontal` is a horizontal line, `vertical` is a vertical line, and their ends are ordered from smaller to higher (according to their direction)
 */
static inline bool intersects(Line const& horizontal, Line const& vertical)
{
    assert(horizontal.first.y == horizontal.second.y && vertical.first.x == vertical.second.x
            && horizontal.first.x < horizontal.second.x && vertical.first.y < vertical.second.y);
    return BETWEEN(vertical.first.y, vertical.second.y, horizontal.first.y) && BETWEEN(horizontal.first.x, horizontal.second.x, vertical.first.x);
}

/// @brief  Manhanttan distance of a point to the origin
static inline unsigned distance(Point const& point)
{
    return abs(point.x) + abs(point.y);
}


int main()
{
    std::fstream ifs{"input.txt"};
    Wire w1, w2;
    ifs >> w1 >> w2;
    
    Point curr1 = {0, 0};
    Point closest_point = {100000, 100000};

    while (w1)
    {
        auto [dir1, len1] = w1.next();

        Point temp1 = move(curr1, dir1, len1);

        Point curr2 = {0, 0};
        w2.reset();
        while (w2)
        {
            auto [dir2, len2] = w2.next();
            
            Point temp2 = move(curr2, dir2, len2);
            if (is_perpendicular(dir1, dir2))
            {
                Line horizontal = (dir1 == LEFT ? Line{temp1, curr1} 
                    : dir1 == RIGHT ? Line{curr1, temp1}
                    : dir2 == LEFT ? Line{temp2, curr2}
                    : /*dir2 == RIGHT*/ Line{curr2, temp2});
                Line vertical = (dir1 == UP ? Line{temp1, curr1}
                    : dir1 == DOWN ? Line{curr1, temp1}
                    : dir2 == UP ? Line{temp2, curr2}
                    : /*dir2 == DOWN*/ Line{curr2, temp2});
                if (intersects(horizontal, vertical))
                {
                    Point intersection = {vertical.first.x, horizontal.first.y};
                    if (distance(intersection) < distance(closest_point))
                        closest_point = intersection;
                }
            }
            curr2 = temp2;
        }

        curr1 = temp1;
    }

    std::cout << "closest point: " << closest_point.x << " " << closest_point.y << std::endl;
    std::cout << "manhattan: " << distance(closest_point) << std::endl;
}