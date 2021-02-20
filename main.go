package main

import (
	"fmt"
	"os"
	"os/user"

	"gomonkey/repl"
)

func main() {

	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! This is GoMonkey Programming Language Yo!\n",
		user.Name)
	fmt.Printf("Feel free to monkey around\n")
	repl.Start(os.Stdin, os.Stdout)
}
