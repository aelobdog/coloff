package main

import (
	"coloff/tools"
	"fmt"
)

func main() {
    text := `
def hello = 123.44
def bye = "hello"
`
    fmt.Println("Source code :", text)
    
    l := tools.CreateLexerState(text)
    p := tools.CreateParserState(l)

    // t := tools.CreateLexerState(text).Lex()
    // for i:=0; i<len(t); i++ {
    //     fmt.Println(t[i].Line, t[i].Literal, t[i].TokType.String())
    // }
    
    tree := p.Parse()
    fmt.Println("tree : ", tree)
}
