#include <algorithm>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>

#include <sysexits.h>

namespace {

// 9 ORE => 2 A
// 8 ORE => 3 B
// 7 ORE => 5 C
// 3 A 4 B => 1 AB
// 5 B 7 C => 1 BC
// 4 C 1 A => 1 CA
// 2 AB 3 BC 4 CA => 1 FUEL
//
// -1 FUEL : +(1 /^ 1 = 1) FUEL | +1 FUEL -(2 AB 3 BC 4 CA)
// -2 AB -3 BC -4 CA : +(2 /^ 1 = 2) AB | +2 AB -2(3 A 4 B)
// -3 BC -4 CA -6 A -8 B : +(3 /^ 1 = 3) BC | +3 BC -3(5 B 7 C)
// -4 CA -6 A -21 C -23 B : +(4 /^ 1 = 4) CA | +4 CA -4(4 C 1 A)
// -10 A -23 B -37 C : +(10 /^ 2 = 5) A | +10 A -5(9 ORE)
// -23 B -37 C -45 ORE : +(23 /^ 3 = 8) B | +24 B -8(8 ORE)
// 1 B -37 C -109 ORE : +(37 /^ 5 = 8) C | +40 C -8(7 ORE)
// 3 C 1 B -165 ORE

int64_t div_ceil(int64_t a, int64_t b)
{
    return a / b + (a % b ? 1 : 0);
}

// Chemical symbols along with the corresponding quantity.
using chemicals = std::map<std::string, int64_t>;

// Maps chemical symbol S to a reaction. In a reaction +N units of S
// are produced, and some other chemicals are consumed (i.e., they
// appear with negative quantities in the equation).
using reactions = std::map<std::string, chemicals>;

chemicals mul(const chemicals& a, int64_t m)
{
    chemicals c = a;
    for (auto& [_, n] : c) {
        n *= m;
    }
    return c;
}

chemicals add(const chemicals& a, const chemicals& b)
{
    chemicals c;

    auto ai = a.begin();
    auto bi = b.begin();

    while (ai != a.end() && bi != b.end()) {
        if (ai->first < bi->first) {
            if (ai->second != 0) {
                c.insert(*ai);
            }
            ++ai;
        } else if (bi->first < ai->first) {
            if (bi->second != 0) {
                c.insert(*bi);
            }
            ++bi;
        } else {
            if (const auto n = ai->second + bi->second; n != 0) {
                c.insert({ ai->first, n });
            }
            ++ai;
            ++bi;
        }
    }

    for (; ai != a.end(); ++ai) {
        if (ai->second != 0) {
            c.insert(*ai);
        }
    }

    for (; bi != b.end(); ++bi) {
        if (bi->second != 0) {
            c.insert(*bi);
        }
    }

    return c;
}

// Like add() but raises std::underflow_error if there's a negative
// result.
chemicals add_underflow(const chemicals& a, const chemicals& b)
{
    chemicals c;

    auto ai = a.begin();
    auto bi = b.begin();

    while (ai != a.end() && bi != b.end()) {
        if (ai->first < bi->first) {
            if (ai->second > 0) {
                c.insert(*ai);
            } else if (ai->second < 0) {
                throw std::underflow_error(__func__);
            }
            ++ai;
        } else if (bi->first < ai->first) {
            if (bi->second > 0) {
                c.insert(*bi);
            } else if (bi->second < 0) {
                throw std::underflow_error(__func__);
            }
            ++bi;
        } else {
            if (const auto n = ai->second + bi->second; n > 0) {
                c.insert({ ai->first, n });
            } else if (n < 0) {
                throw std::underflow_error(__func__);
            }
            ++ai;
            ++bi;
        }
    }

    for (; ai != a.end(); ++ai) {
        if (ai->second > 0) {
            c.insert(*ai);
        } else if (ai->second < 0) {
            throw std::underflow_error(__func__);
        }
    }

    for (; bi != b.end(); ++bi) {
        if (bi->second > 0) {
            c.insert(*bi);
        } else if (bi->second < 0) {
            throw std::underflow_error(__func__);
        }
    }

    return c;
}

std::istream& operator>>(std::istream& in, reactions& reacts)
{
    std::string line;

    reacts.clear();
    while (std::getline(in, line)) {
        const auto i = line.find("=>");

        if (i == line.npos) {
            in.setstate(std::ios::failbit);
            return in;
        }

        std::replace_if(
            line.begin(), line.end(), [](char c) {
                return c == ',';
            },
            ' ');

        std::istringstream to_in(line.substr(i + 2));
        std::istringstream from_in(line.substr(0, i));

        std::string chem;
        int64_t n;

        if (!(to_in >> n >> chem)) {
            in.setstate(std::ios::failbit);
            return in;
        }

        auto& chems = reacts[chem];

        chems[chem] = n;

        while (from_in >> n >> chem) {
            chems[chem] -= n;
        }

        if (!from_in.eof()) {
            in.setstate(std::ios::failbit);
            return in;
        }
    }

    return in;
}

int64_t max_fuel(const chemicals& cargo)
{
    return 0;
}

} // namespace

int main()
{
    reactions reacts;

    if (!(std::cin >> reacts).eof()) {
        return EX_DATAERR;
    }

#if PART==1
    chemicals cargo = { { "FUEL", -1 } };

    while (true) {
        std::optional<std::tuple<std::string, int64_t>> consume;

        for (const auto& [chem, n] : cargo) {
            if (n > 0 || chem == "ORE") {
                continue;
            }
            if (!consume || n > std::get<1>(*consume)) {
                consume = { chem, n };
            }
        };

        if (!consume) {
            break;
        }

        const auto& [chem, n] = *consume;
        const auto& products = reacts.at(chem);
        const auto m = div_ceil(-n, products.at(chem));
        cargo = add(cargo, mul(products, m));
    }

    std::cout << -cargo.at("ORE") << '\n';
#elif PART==2
    std::cout << max_fuel({ { "ORE", 1000000000000 } }) << '\n';
#endif // PART

    return 0;
}
