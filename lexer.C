
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
    kind(DoubleStar);
    kind(Colon);
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

LexState::LexState(std::string s)
  : _input(s)
  , _pos(0)
{}


/*v1...
Token LexState::get_token() {
  if (_pos >= _input.size()) {
    return Token(_input,NoMoreTokens,_pos,0);
  }
  unsigned start = _pos++;
  Kind kind = UnknownChar;
  unsigned len = _pos - start;
  return Token(_input,kind,start,len);
}
*/

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
  while (isWhite(_input[_pos])) _pos++;

  if (_pos >= _input.size()) {
    return Token(_input,NoMoreTokens,_pos,0);
  }

  Kind kind = UNKNOWN_CHAR;

  unsigned start = _pos;
  char c = _input[_pos++];

  // TODO: literal strings; floats...
  switch (c) {
  case '-': kind = Dash; break;
  case '+': kind = Plus; break;
  case '*':
    kind = Star;
    if (_input[_pos] == '*') {
      kind = DoubleStar;
      _pos++;
    }
    break;
  case '(': kind = LP; break;
  case ')': kind = RP; break;
  case '<': kind = Langle; break;
  case '>': kind = Rangle; break;
  case ':': kind = Colon; break;

  case 'i': {
    kind = Identifier;
    while (isIdentContinue(_input[_pos])) _pos++;
    std::string s = _input.substr(start, _pos - start);
    if (s == "if") kind = If;
    break;
  }

  default: {

    if (isNumber(c)) {
      while (isNumber(_input[_pos])) _pos++;
      kind = Number;
    }

    if (isIdentStart(c)) {
      while (isIdentContinue(_input[_pos])) _pos++;
      kind = Identifier;
      std::string s = _input.substr(start, _pos - start);
      if (s == "then") kind = Then;
      if (s == "else") kind = Else;
      if (s == "def") kind = DefKeyword;
    }
  }

  }

  unsigned len = _pos - start;
  return Token(_input,kind,start,len);
}
