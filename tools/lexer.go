//   Copyright (C) 2022 Ashwin Godbole
//
//   This file is part of coloff.
//
//   coloff is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   coloff is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with coloff. If not, see <https://www.gnu.org/licenses/>.

package tools

import (
	"fmt"
	"os"
)

// Lexer : Current state of the lexer
type Lexer struct {
	Source  string
	CurrPos int
	NextPos int
	Ch      byte
	line    int
}

// CreateLexerState : to create a new lexer state and initialize it
func CreateLexerState(source string) *Lexer {
	l := Lexer{
		Source:  source,
		CurrPos: 0,
		NextPos: 1,
		Ch:      source[0],
		line:    0,
	}
	return &l
}

// ReadChar : to read the next character
func (l *Lexer) ReadChar() {
	if l.NextPos >= len(l.Source) {
		l.Ch = 0
	} else {
		l.Ch = l.Source[l.NextPos]
	}
	l.CurrPos = l.NextPos
	l.NextPos++
}

// PeekChar : to peek at the next character
func (l *Lexer) PeekChar() byte {
	if l.NextPos >= len(l.Source) {
		return 0
	}
	return l.Source[l.NextPos]
}

// NextToken : to get the next token from the source
func (l *Lexer) NextToken() Token {
	var token Token
	l.consumeWhiteSpace()
	switch l.Ch {
	case '\n':
		token = NewToken(EOL, "<nl>", l.line)
		l.line++
	case ',':
		token = NewToken(COM, ",", l.line)
	case '+':
		token = NewToken(PLS, "+", l.line)
	case '-':
		token = NewToken(MIN, "-", l.line)
	case '*':
		token = NewToken(PRD, "*", l.line)
	case '/':
		token = NewToken(DIV, "/", l.line)
	case '%':
		token = NewToken(REM, "%", l.line)
	case '^':
		token = NewToken(POW, "^", l.line)
	case '(':
		token = NewToken(LPR, "(", l.line)
	case ')':
		token = NewToken(RPR, ")", l.line)
	case '[':
		token = NewToken(LSB, "[", l.line)
	case ']':
		token = NewToken(RSB, "]", l.line)
	case '{':
		token = NewToken(LCB, "{", l.line)
	case '}':
		token = NewToken(RCB, "}", l.line)
	case '=':
		if l.PeekChar() == '=' {
			token = NewToken(EQL, "==", l.line)
			l.ReadChar()
		} else {
			token = NewToken(ASN, "=", l.line)
		}
	case '!':
		if l.PeekChar() == '=' {
			token = NewToken(NEQ, "!=", l.line)
			l.ReadChar()
		} else {
			token = NewToken(LNT, "!", l.line)
		}
	case '>':
		if l.PeekChar() == '=' {
			token = NewToken(GRE, ">=", l.line)
			l.ReadChar()
		} else {
			token = NewToken(GRT, ">", l.line)
		}
	case '<':
		if l.PeekChar() == '=' {
			token = NewToken(LSE, "<=", l.line)
			l.ReadChar()
		} else {
			token = NewToken(LST, "<", l.line)
		}
	case '&':
		token = NewToken(LND, "&", l.line)
	case '|':
		token = NewToken(LOR, "|", l.line)
	case 0:
		token = NewToken(EOF, "", l.line)
    case '\'':
        l.ReadChar()
        token = NewToken(CHR, string(l.Ch), l.line)
        l.ReadChar()
	default:
		if IsDigit(l.Ch) {
			token = l.readNumber()
		} else if IsLetter(l.Ch) {
			token = l.readWord()
		} else if l.Ch == '"' {
			token = l.readString()
		} else if l.Ch == '#' {
			token = l.readComment()
		} else {
			token = NewToken(ILG, string(l.Ch), l.line)
		}
	}

    l.ReadChar()
	if token.TokType == ILG {
		fmt.Println("error (line :", l.line, ") found illegal token `", token.Literal, "`.")
		// os.Exit(65)
	}
	return token
}

func (l *Lexer) updateChar() {
	if !(l.NextPos >= len(l.Source)) {
		l.Ch = l.Source[l.CurrPos]
	}
}

func (l *Lexer) readNumber() Token {
	start := l.CurrPos
	end := l.CurrPos
	floating := false

	for IsDigit(l.PeekChar()) || l.PeekChar() == '.' {
		if l.Ch == '.' {
			if !IsDigit(l.PeekChar()) {
				return NewToken(ILG, "", l.line)
			}
			floating = true
		}
		end++
		l.ReadChar()
	}

	numberType := INT
	if floating == true {
		numberType = FLT
	}
	l.CurrPos = end
	l.updateChar()
	return NewToken(numberType, l.Source[start:end+1], l.line)
}

func (l *Lexer) readWord() Token {
	start := l.CurrPos
	end := l.CurrPos

	for IsLetter(l.PeekChar()) {
		end++
		l.ReadChar()
	}
    // println("LOG:", l.Source[start : end+1])
	word := l.Source[start : end+1]
    // println("LOG:", word, Keywords[word])
	l.CurrPos = end
	l.updateChar()

	if IsKeyword(word) {
		return NewToken(Keywords[word], word, l.line)
	} else if word == "true" || word == "false" {
		return NewToken(BOL, word, l.line)
	}

	return NewToken(IDN, word, l.line)
}

// FIXME: strings currently treat escape sequences as 2 individial characters
func (l *Lexer) readString() Token {
    l.ReadChar()
	start := l.CurrPos
	end := l.CurrPos

	for l.Ch != '"' {
		if l.Ch == '\\' && l.PeekChar() == '"' {
			end += 2
			l.ReadChar()
		} else {
			end++
		}
		l.ReadChar()

		if l.Ch == 0 {
			fmt.Printf("error (line : %d) String literal may not be closed.\n", l.line)
			os.Exit(1)
		}
	}
	l.CurrPos = end
	l.updateChar()
	return NewToken(STR, l.Source[start:end], l.line)
}

func (l *Lexer) readComment() Token {
	l.ReadChar()
	for l.Ch != '\n' {
		l.ReadChar()
	}
	return NewToken(EOL, "", l.line)
}

func (l *Lexer) consumeWhiteSpace() {
	for l.Ch == ' ' || l.Ch == '\t' || l.Ch == '\r' {
		l.ReadChar()
	}
}

// Lex : returns a list of all tokens
func (l *Lexer) Lex() []Token {
	var tokens []Token
	for l.Ch != 0 {
		tokens = append(tokens, l.NextToken())
	}
	// pad tokens with an EOF at the end in case the input does not end in EOF
	if tokens[len(tokens)-1].TokType != EOF {
		tokens = append(tokens, NewToken(EOF, "", tokens[len(tokens)-1].Line+1))
	}
	return tokens
}
