#include <iostream>
#include <vector>
#include <deque>
#include <fstream>
#include <unordered_map>

using namespace std;

enum class StateType {
    START,
    ACCEPT,
    REJECT,
    BLANK,
    UNKNOWN
};

ostream& operator<<(ostream& out, StateType const& t) {
        string str;
        switch (t) {
        case StateType::START:
            str = "START";
            break;
        case StateType::ACCEPT:
            str = "ACCEPT";
            break;
        case StateType::REJECT:
            str = "REJECT";
            break;
        case StateType::BLANK:
            str = "BLANK";
            break;
        case StateType::UNKNOWN:
            str = "UNKNOWN";
            break;
        }
        out << str;
        return out;
    }

enum Shift {
    LEFT,
    RIGHT,
    NONE,
    UNKNOWN
};

struct Transition {
    string from;
    string to;
    char character;
    char replace_by;
    Shift shift;
};

struct TuringMachine {
    unordered_map<StateType, string> controls;
    unordered_map<string, unordered_map<char, Transition>> transitions;
};

StateType getStateType(string word) {
    if (word == "start:") {
        return StateType::START;
    } else if (word == "accept:") {
        return StateType::ACCEPT;
    } else if (word == "reject:") {
        return StateType::REJECT;
    } else if (word == "blank:") {
        return StateType::BLANK;
    }
    return StateType::UNKNOWN;
}

Shift getShift(string word) {
    if (word == ">") {
        return Shift::RIGHT;
    } else if (word == "<") {
        return Shift::LEFT;
    } else if (word == "^") {
        return Shift::NONE;
    }
    return Shift::UNKNOWN;
}

void simulate(TuringMachine const& machine, string const& input, bool debug);

int main(int argc, char** argv) {
    if (argc < 2 || argc > 3 || (argc == 3 && string(argv[2]) != "-debug")) {
        cout << "Usage: turing machine.out [-debug]\n";
        return 1;
    }

    ifstream file(string(argv[1]), ifstream::in);
    bool debug = (argc == 3);

    if (!file.is_open()) {
        cout << "file couldn't be opened\n";
        return 1;
    }

    string word;
    TuringMachine machine{};

    while (file >> word) {
        StateType type = getStateType(word);
        if (type != StateType::UNKNOWN) {
            if (machine.controls.count(type)) {
                cout << "Control state \"" << type << "\" already set\n";
                return 1;
            }

            string state;
            if (!(file >> state)) {
                cout << "State expected after \"" << word << "\"\n";
                return 1;
            }

            if (type == StateType::BLANK && state.size() != 1) {
                cout << "Blank option expects a character\n";
                return 1;
            }

            machine.controls[type] = state;

        } else {
            Transition t{};
            t.from = word;

            if (!(file >> t.character)) {
                cout << "Character to find expected\n";
                return 1;
            }

            if (!(file >> word) || word != "->") {
                cout << "Arrow sequence expected\n";
                return 1;
            }

            if (!(file >> t.to)) {
                cout << "Destination state expected\n";
                return 1;
            }

            if (!(file >> t.replace_by)) {
                cout << "Character to replace by expected\n";
                return 1;
            }

            if (!(file >> word)) {
                cout << "Shift sequence expected\n";
                return 1;
            }

            t.shift = getShift(word);

            if (t.shift == Shift::UNKNOWN) {
                cout << "Shift sequence expected, got \"" + word + "\"\n";
                return 1;
            }

            if (machine.transitions[t.from].count(t.character)) {
                cout << "Transition from \"" + t.from + "\" by \"" + t.character + "\" is already set\n";
                return 1;
            }

            // transition is valid here

            machine.transitions[t.from][t.character] = t;
        }
    }

    cout << "Turing machine loaded\n";
    cout << "Type input: ";

    string input;

    if (!getline(cin, input)) {
        cout << "failed\n";
        return 1;
    }

    simulate(machine, input, debug);
    return 1;
}

void dumpTape(deque<char> tape, int pos, char blank) {
    // truncate blanks

    while (pos != 0 && !tape.empty() && tape.front() == blank) {
        tape.pop_front();
        --pos;
    }

    while (tape.size() != pos + 1 && !tape.empty() && tape.back() == blank) {
        tape.pop_back();
    }

    cout << "Tape:\n";
    for (char c: tape) {
        cout << c;
    }
    cout << endl;
    cout << string(pos, ' ') << "^" << endl;
}

void simulate(TuringMachine const& machine, string const& input, bool debug) {
    deque<char> tape(input.size());

    for (int i = 0; i < input.size(); ++i) {
        tape[i] = input[i];
    }

    if (tape.empty()) {
        tape.push_back(machine.controls.at(StateType::BLANK)[0]);
    }

    // tape is ready

    string state = machine.controls.at(StateType::START);
    bool accepted = true;
    int iterations = 0;
    int pos = 0;

    while (state != machine.controls.at(StateType::REJECT) && state != machine.controls.at(StateType::ACCEPT)) {

        if (debug) {
            cout << "\n--- Shot " << iterations << " ---\n";
            cout << "State: \"" + state + "\"\n\n";
            dumpTape(tape, pos, machine.controls.at(StateType::BLANK)[0]);
        }

        if (!machine.transitions.count(state)) {
            accepted = false;
            break;
        }

        if (!machine.transitions.at(state).count(tape[pos])) {
            accepted = false;
            break;
        }

        Transition const& t = machine.transitions.at(state).at(tape[pos]);

        state = t.to;
        tape[pos] = t.replace_by;
        pos += (t.shift == Shift::LEFT ? -1 : (t.shift == Shift::RIGHT ? 1 : 0));

        if (pos < 0) {
            tape.push_front(machine.controls.at(StateType::BLANK)[0]);
            ++pos;
        }

        if (pos >= tape.size()) {
            tape.push_back(machine.controls.at(StateType::BLANK)[0]);
        }

        iterations++;
    }

    if (state == machine.controls.at(StateType::REJECT)) {
        accepted = false;
    }

    cout << "\n--- Result ---\n";
    cout << "Verdict: " <<(accepted ? "Accepted" : "Rejected") << "\n";
    cout << "State: \"" + state + "\"\n\n";

    dumpTape(tape, pos, machine.controls.at(StateType::BLANK)[0]);
}


