#include <string>
#include <iostream>
#include <vector>
#include <memory>
#include <exception>
#include <unordered_map>
#include <bits/unique_ptr.h>

using std::string;
using std::vector;
using std::unique_ptr;
using std::make_pair;
using std::pair;
using std::unordered_map;
using std::make_unique;
using std::runtime_error;
using std::cin;
using std::cout;
using std::endl;

typedef struct {
    const char * name;
    void       * pointer;
} symbol_t;

void jit_compile_expression_to_arm(
        const char * expression, const symbol_t * externs, void * out_buffer);

// some

bool is_digit(char c) {
    return '0' <= c and c <= '9';
}
bool is_letter(char c) {
    return ('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z');
}

// tokens

enum TokenType {TOKEN_NUMBER, TOKEN_PUNCT, TOKEN_OPER, TOKEN_NAMED};
enum OperType {PLUS, MINUS, MUL, UNAR_MINUS};
enum PunctType {OPEN, CLOSE, COMMA};

struct Token {
    virtual ~Token() = default;
    virtual TokenType type() = 0;
};

struct TokenNumber : public Token {
    TokenType type() override { return TOKEN_NUMBER; }
    int c;
    explicit TokenNumber(int c): c(c) {}
};

struct TokenPunct : public Token {
    TokenType type() override { return TOKEN_PUNCT; }
    PunctType pt;
    explicit TokenPunct(PunctType bt): pt(bt) {}
};

struct TokenOper : public Token {
    TokenType type() override { return TOKEN_OPER; }
    OperType opt;
    explicit TokenOper(OperType opt): opt(opt) {}
};

struct TokenNamed : public Token {
    TokenType type() override { return TOKEN_NAMED; }
    string name;
    explicit TokenNamed(const string& name): name(name) {}
};

typedef vector<unique_ptr<Token> > ArrayOfTokens;
OperType get_oper_type(Token&);
PunctType get_punct_type(Token&);
string get_name(Token&);

ArrayOfTokens tokenate(const string& expr);

// tree

enum NodeType {NODE_CONST, NODE_VAR, NODE_FUNC, NODE_OPER};

struct Node {
    Node(): code(nullptr) {}
    virtual ~Node() = default;

    void* code;
    virtual NodeType type() = 0;
};

struct NodeConst : public Node {
    NodeType type() override { return NODE_CONST; }
    int c;
    explicit NodeConst(int c):
            c(c) {}
};

struct NodeVar : public Node {
    NodeType type() override { return NODE_VAR; }
    string name;
    explicit NodeVar(const string& name):
            name(name) {}
};

struct NodeFunc : public Node {
    NodeType type() override { return NODE_FUNC; }
    string name;
    vector <Node*> childs;
    NodeFunc(const string& name, const vector<Node*>& childs):
            name(name), childs(childs) {}
};

struct NodeOper : public Node {
    NodeType type() override { return NODE_OPER; }
    OperType oper_type;
    vector <Node*> childs;
    NodeOper(OperType opt, const vector<Node*>& childs):
            oper_type(opt), childs(childs) {}
};

void clear(Node* node);

pair<Node*, int> scan_expr_plus(const ArrayOfTokens& tokens, int i);
pair<Node*, int> scan_expr_mult(const ArrayOfTokens& tokens, int i);
pair<Node*, int> scan_expr_sub(const ArrayOfTokens& tokens, int i);
Node* build_tree(const ArrayOfTokens& tokens);

class TreePrinter {
public:
    void print(Node*);
    void print(Node* node, int parent);
private:
    int n;
};

// IMPLEMENTATION

void clear(Node* node) {
    if (node->type() == NODE_OPER) {
        auto casted = static_cast<NodeOper*>(node);
        for (Node* u : casted->childs)
            clear(u);
    }
    else if (node->type() == NODE_FUNC) {
        auto casted = static_cast<NodeFunc*>(node);
        for (Node* u : casted->childs)
            clear(u);
    }

    delete node;
}

ArrayOfTokens tokenate(const string& expr) {
    enum Condition {basic, start_number, start_word};
    Condition q = basic;
    ArrayOfTokens result;
    int cur_num = 0;
    string cur_word;

    for (char c: expr) {
        if (q == start_number) { // !!!!!!
            if (is_digit(c)) {
                cur_num *= 10;
                cur_num += c - '0';
                continue;
            }
            else {
                result.emplace_back(new TokenNumber(cur_num));
                q = basic;
            }
        }

        if (q == start_word) { // !!!!!!
            if (is_letter(c)) {
                cur_word.push_back(c);
                continue;
            }
            else {
                result.emplace_back(new TokenNamed(cur_word));
                q = basic;
            }
        }

        if (c == '+') {
            result.emplace_back(new TokenOper(PLUS));
        }
        else if (c == '-') {
            result.emplace_back(new TokenOper(MINUS));
        }
        else if (c == '*') {
            result.emplace_back(new TokenOper(MUL));
        }
        else if (c == '(') {
            result.emplace_back(new TokenPunct(OPEN));
        }
        else if (c == ')') {
            result.emplace_back(new TokenPunct(CLOSE));
        }
        else if (c == ',') {
            result.emplace_back(new TokenPunct(COMMA));
        }
        else if (is_letter(c)) {
            cur_word.clear();
            cur_word.push_back(c);
            q = start_word;
        }
        else if (is_digit(c)) {
            cur_num = c - '0';
            q = start_number;
        }
        else if (c != ' ') {
            throw runtime_error("tokenate: uncorrect symbol in expression");
        }
    }

    if (q == start_number)
        result.emplace_back(new TokenNumber(cur_num));

    if (q == start_word) {
        result.emplace_back(new TokenNamed(cur_word));
    }

    return result;
}

OperType get_oper_type(Token &token) {
    return dynamic_cast<TokenOper&>(token).opt;
}

PunctType get_punct_type(Token &token) {
    return dynamic_cast<TokenPunct&>(token).pt;
}

string get_name(Token& token) {
    return dynamic_cast<TokenNamed&>(token).name;
}

pair<Node*, int> scan_expr_plus(const ArrayOfTokens& tokens, int i) {
    if (i == tokens.size())
        throw runtime_error("scan_expr_plus: called with empty suffix");

    bool minus = false;
    if (tokens[i]->type() == TOKEN_OPER and get_oper_type(*tokens[i]) == MINUS) {
        i++;
        minus = true;
    }

    auto p = scan_expr_mult(tokens, i);
    Node* node = p.first;
    i = p.second;
    if (minus) {
        node = new NodeOper(UNAR_MINUS, {node});
    }

    while (i != tokens.size()) {
        if (tokens[i]->type() == TOKEN_OPER and
                (get_oper_type(*tokens[i]) == PLUS or
                 get_oper_type(*tokens[i]) == MINUS)) {
            OperType opt = get_oper_type(*tokens[i]);
            i++;
            p = scan_expr_mult(tokens, i);

            node = new NodeOper(opt, {node, p.first});
            i = p.second;
        }
        else {
            break;
        }
    }

    return make_pair(node, i);
};


pair<Node*, int> scan_expr_mult(const ArrayOfTokens& tokens, int i) {
    if (i == tokens.size())
        throw runtime_error("scan_expr_mult: called with empty suffix");

    auto p = scan_expr_sub(tokens, i);
    Node* node = p.first;
    i = p.second;

    while (i != tokens.size()) {
        if (tokens[i]->type() == TOKEN_OPER and get_oper_type(*tokens[i]) == MUL) {
            i++;
            p = scan_expr_sub(tokens, i);

            node = new NodeOper(MUL, {node, p.first});
            i = p.second;
        }
        else {
            break;
        }
    }

    return make_pair(node, i);
};

pair<Node*, int> scan_expr_sub(const ArrayOfTokens& tokens, int i) {
    if (i == tokens.size())
        throw runtime_error("scan_expr_sub: called with empty suffix");

    if (tokens[i]->type() == TOKEN_NUMBER) {
        auto token = dynamic_cast<TokenNumber*>(tokens[i].get());
        Node* node = new NodeConst(token->c);
        return make_pair(node, i+1);
    }
    else if (tokens[i]->type() == TOKEN_NAMED) {
        if (i+1 != tokens.size() and tokens[i+1]->type() == TOKEN_PUNCT and
                get_punct_type(*tokens[i + 1]) == OPEN) {
            string name = get_name(*tokens[i]);
            i += 2;
            vector<Node*> childs;
            while (true) {
                auto p = scan_expr_plus(tokens, i);
                childs.push_back(p.first);
                i = p.second;

                if (tokens[i]->type() == TOKEN_PUNCT) {
                    if (get_punct_type(*tokens[i]) == CLOSE) {
                        i++;
                        break;
                    }
                    else if (get_punct_type(*tokens[i]) == COMMA) {
                        i++;
                        continue;
                    }
                }
                throw runtime_error("scan_expr_sub: expeted , or )");
            }
            Node* node = new NodeFunc(name, childs);
            return make_pair(node, i);
        }
        else {
            Node* node = new NodeVar(get_name(*tokens[i]));
            return make_pair(node, i+1);
        }
    }
    else if (tokens[i]->type() == TOKEN_PUNCT and get_punct_type(*tokens[i]) == OPEN) {
        ++i;
        auto p = scan_expr_plus(tokens, i);
        i = p.second;
        if (i >= tokens.size() or
                tokens[i]->type() != TOKEN_PUNCT or
                get_punct_type(*tokens[i]) != CLOSE) {
            throw runtime_error("scan_expr_sub: expected )");
        }
        p.second++;
        return p;
    }
};

Node* build_tree(const ArrayOfTokens& tokens) {
    auto p = scan_expr_plus(tokens, 0);
    if (p.second != tokens.size())
        throw runtime_error("build_tree: invalid expression");

    return p.first;
}

void TreePrinter::print(Node* node) {
    n = 0;
    print(node, 0);
}

void TreePrinter::print(Node* node, int parent) {
    int v = n;
    n++;
    cout << parent << " ";

    if (node->type() == NODE_CONST) {
        auto node2 = dynamic_cast<NodeConst*>(node);
        cout << "num " << node2->c << endl;
    }
    else if (node->type() == NODE_OPER) {
        auto node2 = dynamic_cast<NodeOper*>(node);
        char c;
        if (node2->oper_type == PLUS)
            c = '+';
        if (node2->oper_type == MINUS)
            c = '-';
        if (node2->oper_type == MUL)
            c = '*';
        if (node2->oper_type == UNAR_MINUS)
            c = 'm';
        cout << "oper " << c << endl;
        for (Node* child : node2->childs)
            print(child, v);
    }
    else if (node->type() == NODE_VAR) {
        auto node2 = dynamic_cast<NodeVar*>(node);
        cout << "var " << node2->name << endl;
    }
    else if (node->type() == NODE_FUNC) {
        auto node2 = dynamic_cast<NodeFunc*>(node);
        cout << "func " << node2->name << endl;
        for (Node* child : node2->childs)
            print(child, v);
    }
}

int main() {
    string s;
    std::getline(cin, s);
    auto tokens = tokenate(s);
    TreePrinter printer;
    printer.print(build_tree(tokens));
};
