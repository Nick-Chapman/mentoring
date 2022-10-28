#include "lexer.h"

std::string showKind(Kind k) {
#define kind(E) case E: return #E
  switch (k) {
    kind(NoMoreTokens);
    kind(UNKNOWN_CHAR);
    kind(Identifier);
    kind(Number);
    kind(LP);
    kind(RP);
    kind(Langle);
    kind(Rangle);
    kind(Dash);
    kind(Plus);
    kind(Star);
    kind(Equals);
    kind(DoubleEquals);
    kind(Colon);
    kind(Semi);
    kind(If);
    kind(Then);
    kind(Else);
    kind(DefKeyword);
  }
  return "<showKind: unknown kind>";
}

std::string Token::text() {
  return _input.substr(_pos,_length);
}

std::string Token::startRC() {
  return std::to_string(_row) + "." + std::to_string(_col);
}

LexState::LexState(std::string s)
  : _input(s)
  , _pos(0)
  , _row(1)
  , _col(0)
{}

static bool isWhite(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

static bool isAlpha(char c) {
  return (c >= 'a' &&  c <= 'z') || (c >= 'A' &&  c <= 'Z');
}

static bool isNumber(char c) {
  return (c >= '0' &&  c <= '9');
}

static bool isUnderscore(char c) {
  return c == '_';
}

static bool isIdentStart(char c) {
  return isAlpha(c) || isUnderscore(c);
}

static bool isIdentContinue(char c) {
  return isIdentStart(c) || isNumber(c);
}


Token LexState::get_token() {

  // TODO: comments (end-of-line / begin-end comment markers)
  {
    char c;
    while (isWhite(c = _input[_pos])) {
      _pos++;
      if (c == '\n') {
        _row++;
        _col = 0;
      } else {
        _col++;
      }
    }
  }

  if (_pos >= _input.size()) {
    return Token(_input,NoMoreTokens,_pos,0,_row,_col);
  }

  Kind kind = UNKNOWN_CHAR;

  unsigned start = _pos;
  unsigned startRow = _row;
  unsigned startCol = _col;
  char c = _input[_pos++]; _col++;

  // TODO: literal strings; floats...
  switch (c) {
  case '-': kind = Dash; break;
  case '+': kind = Plus; break;
  case '*': kind = Star; break;
  case '=':
    kind = Equals;
    if (_input[_pos] == '=') {
      kind = DoubleEquals;
      _pos++; _col++;
    }
    break;
  case '(': kind = LP; break;
  case ')': kind = RP; break;
  case '<': kind = Langle; break;
  case '>': kind = Rangle; break;
  case ':': kind = Colon; break;
  case ';': kind = Semi; break;

  case 'i': {
    kind = Identifier;
    while (isIdentContinue(_input[_pos])) { _pos++; _col++; }
    std::string s = _input.substr(start, _pos - start);
    if (s == "if") kind = If;
    break;
  }

  default: {

    if (isNumber(c)) {
      while (isNumber(_input[_pos])) { _pos++; _col++; }
      kind = Number;
    }

    if (isIdentStart(c)) {
      while (isIdentContinue(_input[_pos])) { _pos++; _col++; }
      kind = Identifier;
      std::string s = _input.substr(start, _pos - start);
      if (s == "then") kind = Then;
      if (s == "else") kind = Else;
      if (s == "def") kind = DefKeyword;
    }
  }

  }

  unsigned len = _pos - start;
  return Token(_input,kind,start,len,startRow,startCol);
}
