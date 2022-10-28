#ifndef LEXER_H
#define LEXER_H

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
   Semi,
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
  unsigned _row;
  unsigned _col;
 public:
  Token(std::string s, Kind k, unsigned p, unsigned l, unsigned r, unsigned c)
    : _input(s), _kind(k), _pos(p), _length(l), _row(r), _col(c)
    {}
  Kind kind() { return _kind; }
  unsigned startPos() { return _pos; }
  unsigned endPos() { return _pos + _length - 1; }
  std::string text();
  std::string startRC();
};

class LexState {
 private:
  std::string _input;
  unsigned _pos;
  unsigned _row;
  unsigned _col;
 public:
  LexState(std::string);
  Token get_token();
};

#endif
