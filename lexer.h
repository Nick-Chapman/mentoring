#include <string>

enum Kind
  {
   NoMoreTokens,
   UNKNOWN_CHAR,
   Identifier,
   Number,
   Dash,
   Plus,
   Star,
   Equals,
   DoubleEquals,
   LP,
   RP,
   Langle,
   Rangle,
   Colon,
   If,
   Then,
   Else,
   DefKeyword,
  };

std::string showKind(Kind);

class Token {
 private:
  std::string _input;
  Kind _kind;
  unsigned _pos;
  unsigned _length;
 public:
 Token(std::string s, Kind k, unsigned p, unsigned l)
   : _input(s), _kind(k), _pos(p), _length(l)
    {}
  Kind kind() { return _kind; }
  unsigned startPos() { return _pos; }
  unsigned endPos() { return _pos + _length - 1; }
  std::string text();
};

class LexState {
 private:
  std::string _input;
  unsigned _pos;
 public:
  LexState(std::string);
  Token get_token();
};
