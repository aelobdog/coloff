package main

import (
	"coloff/tools"
	"fmt"
)

func main() {
    text := `
def hello = 123.44 + 23.1 * 10
def bye = "hello"
`
    fmt.Println("Source code :", text)
    
    l := tools.CreateLexerState(text)
    p := tools.CreateParserState(l)

    tree := p.Parse()
    for _, i := range(tree.Statements) {
        fmt.Println(i.String())
    }
}
