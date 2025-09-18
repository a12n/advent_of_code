#include "advent.hpp"

#if PART == 2
namespace {

optional<pair<intcode::value, intcode::value>> gravity_assist(const intcode::memory& p, intcode::value k)
{
    for (intcode::value noun = 0; noun < 100; ++noun) {
        for (intcode::value verb = 0; verb < 100; ++verb) {
            intcode::memory t = p;
            t[1] = noun;
            t[2] = verb;
            intcode::run(t, 0);
            if (t[0] == k) {
                return { { noun, verb } };
            }
        }
    }
    return nullopt;
}

} // namespace
#endif // PART

int main()
{
    intcode::memory p;
    intcode::read(cin, p);

#if PART == 1
    p[1] = 12;
    p[2] = 2;
    intcode::run(p, 0);
    cout << p[0] << endl;
#elif PART == 2
    const auto params = gravity_assist(p, 19690720);
    if (!params) {
        return 1;
    }
    cerr << params->first << ' ' << params->second << '\n';
    cout << 100 * params->first + params->second << endl;
#endif // PART

    return 0;
}
