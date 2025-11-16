#include <cassert>
#include <iostream>
#include <optional>
#include <variant>
#include <vector>

#include "grid.hpp"
#include "intcode.hpp"

namespace {

using namespace grid::planar;

using scaffold_view = std::vector<std::vector<char>>;

bool check_bounds(const scaffold_view& view, position p)
{
    return !view.empty() && p[1] >= 0 && static_cast<size_t>(p[1]) < view.size() && p[0] >= 0 && static_cast<size_t>(p[0]) < view[0].size();
}

char at(const scaffold_view& view, position p)
{
    if (!check_bounds(view, p)) {
        return ' ';
    }
    return view[p[1]][p[0]];
}

struct ascii_prog {
    intcode::memory img;
    intcode::address ip = 0;
    intcode::value rel_base = 0;
};

scaffold_view render(ascii_prog& ascii)
{
    scaffold_view view;
    view.push_back({});
    for (position p = { 0, 0 };;) {
        const auto [op, addr] = intcode::run_intrpt(ascii.img, ascii.ip, ascii.rel_base);
        switch (op) {
        case intcode::opcode::output:
            if (ascii.img[addr] == '\n') {
                view.push_back({});
                p[0] = 0;
                p[1]++;
            } else {
                view.back().push_back(ascii.img[addr]);
                p[0]++;
            }
            break;
        case intcode::opcode::halt:
            return view;
        default:
            throw std::invalid_argument(__func__);
        }
    }
}

std::tuple<position, direction> vacuum_robot(const scaffold_view& view)
{
    for (position p = { 0, 0 }; static_cast<size_t>(p[1]) < view.size(); ++p[1]) {
        for (p[0] = 0; static_cast<size_t>(p[0]) < view[p[1]].size(); ++p[0]) {
            switch (view[p[1]][p[0]]) {
            case '^':
                return { p, direction::up };
            case '<':
                return { p, direction::left };
            case '>':
                return { p, direction::right };
            case 'v':
                return { p, direction::down };
            }
        }
    }
    throw std::invalid_argument(__func__);
}

size_t calibrate_cameras(const scaffold_view& view)
{
    size_t sum = 0;
    for (size_t i = 0; i < view.size(); ++i) {
        if (i == 0 || i == (view.size() - 1)) {
            continue;
        }
        for (size_t j = 0; j < view[i].size(); ++j) {
            if (j == 0 || j == (view[i].size() - 1)) {
                continue;
            }
            if (view[i][j] != '.' && (view[i - 1][j] != '.' && view[i][j - 1] != '.' && view[i][j + 1] != '.' && view[i + 1][j] != '.')) {
                sum += i * j;
            }
        }
    }
    return sum;
}

using command = std::variant<rotation, size_t>;

std::optional<command> next_command(const scaffold_view& view, vacuum_robot& robot)
{
    {
        const auto u = to_offset(robot.dir);
        auto q = robot.p + u;
        if (at(view, q) == '#') {
            size_t units = 0;
            do {
                ++units;
                robot.p = q;
                q += u;
            } while (at(view, q) == '#');
            return { { units } };
        }
    }

    {
        const auto dir = rotate(rotation::left, robot.dir);
        const auto u = to_offset(dir);
        if (at(view, robot.p + u) == '#') {
            robot.dir = dir;
            return { { rotation::left } };
        }
    }

    {
        const auto dir = rotate(rotation::right, robot.dir);
        const auto u = to_offset(dir);
        if (at(view, robot.p + u) == '#') {
            robot.dir = dir;
            return { { rotation::right } };
        }
    }

    return {};
}

std::vector<command> commands(const scaffold_view& view, vacuum_robot& robot)
{
    std::optional<command> move;
    std::vector<command> moves;
    while ((move = next_command(view, robot))) {
        moves.push_back(*move);
    }
    return moves;
}

} // namespace

int main(int argc, char* argv[])
{
#if PART == 1
    ascii_prog ascii = { intcode::load(argc, argv) };

    const auto view = render(ascii);
    for (const auto& row : view) {
        for (const auto c : row) {
            std::cerr << c;
        }
        std::cerr << '\n';
    }

    auto [robot_pos, robot_dir] = vacuum_robot(view);
    std::cerr << "robot_pos " << robot_pos << " robot_dir " << direction_symbol(robot_dir) << '\n';
    const auto cmds = commands(view, robot);
    for (size_t i = 0; i < cmds.size(); ++i) {
        if (i != 0) {
            std::cerr << ',';
        }
        if (std::holds_alternative<rotation>(*act)) {
            std::cerr << rotation_symbol(std::get<rotation>(*act), false);
        } else {
            std::cerr << std::get<size_t>(*act);
        }
    }
    std::cerr << '\n';

    std::cout << calibrate_cameras(view) << '\n';
#elif PART == 2
    assert(ascii.img[0] == 1);
    ascii.img[0] = 2;

    // TODO
#endif // PART

    return 0;
}

int test()
{
#if PART == 1
    assert(
        calibrate_cameras({
            { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.' },
            { '.', '.', '#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.' },
            { '#', '#', '#', '#', '#', '#', '#', '.', '.', '.', '#', '#', '#' },
            { '#', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '#' },
            { '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#' },
            { '.', '.', '#', '.', '.', '.', '#', '.', '.', '.', '#', '.', '.' },
            { '.', '.', '#', '#', '#', '#', '#', '.', '.', '.', '^', '.', '.' },
        })
        == 76);
#endif // PART
    return 0;
}

/*
..................>>>>>>>>>>>>v........
..................^...........v........
..................^...........v........
..................^...........v........
..................^...........v........
..................^...........v........
..................^...........v........
..................^...........v........
..............>>>>^...........v........
..............^...............v........
..............^...........v<<<<........
..............^...........v............
......>>>>>>>>^>v.........v............
......^.......^.v.........v............
......^.......^.v.........v............
......^.......^.v.........v............
......^.....v<^<<.........v............
......^.....v.^...........v............
......^.....v.^<<<<<<<<...>>>>>>>>v....
......^.....v.........^...........#....
......^.....v.........^...........#....
......^.....v.........^...........#....
......^.....v.........^...........#....
......^.....v.........^...........#....
......^<<<<<v<<.....##^########...#....
............v.^.....#.^.......#...#....
............>>>>>>>>>>^.....###########
..............^.....#.......#.#...#...#
..............^.....#.......#.#####...#
..............^.....#.......#.........#
>>>>v.........^.....#.......#.........#
^...v.........^.....#.......#..........
^...v.......v<^<<<<<<<<.....#..........
^...v.......v.^.....#.^.....#..........
^...>>>>>>>>>>^.....##^######..........
^...........v.........^................
^...........v.........^................
^...........v.........^................
^...........v.........^................
^...........v.........^................
^<<<<<<<<<<<<.........^................
......................^................
..................>>>>^................

R,4,L,10,L,10,L,8,R,12,R,10,R,4,R,4,L,10,L,10,L,8,R,12,R,10,R,4,R,4,L,10,L,10,L,8,L,8,R,10,R,4,L,8,R,12,R,10,R,4,L,8,L,8,R,
*/
