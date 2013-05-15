package main

import (
	"fmt"
	"math/rand"
	"sort"
	"strings"
	"time"
)

const ngram_n = 3

type text struct {
	text []string
	words []int
}

func (t text) Word(i int) string {
	return t.text[t.words[i]]
}

func (t text) Len() int {
	return len(t.words)
}

func (t text) Less(i, j int) bool {
	return t.Word(i) < t.Word(j)
}

func (t text) Swap(i, j int) {
	t.words[i], t.words[j] = t.words[j], t.words[i]
}

func main() {
	rand.Seed(time.Now().UnixNano())

	// Read text and split it into space delimited words.
	t := text{}
	for i, done := 0, false; !done; i++ {
		var s string
		n, err := fmt.Scan(&s)
		if n == 0 || err != nil {
			done = true
		}
		t.text = append(t.text, s)
		t.words = append(t.words, i)
	}

	// Pre-process words.
	sort.Sort(t)

	// Print priming words.
	/* Find a fullstop. */
	var aux int
	for done := false; !done; {
		aux = rand.Int() % t.Len()
		for ; aux < t.Len() && !strings.HasSuffix(t.Word(aux), "."); aux++ {
		}
		done = aux < t.Len() && strings.HasSuffix(t.Word(aux), ".")
	}
    // words[aux] is now something ending in a fullstop, so don't
    // print the first word.
	for i := 1; i < ngram_n && t.words[aux] + i < t.Len(); i++ {
		fmt.Printf("%s ", t.text[t.words[aux] + i])
	}
	fmt.Println()

	for i := 0; i < 3; i++ {
		// Binary search for the selected phrase.
		start := sort.Search(t.Len(), func (i int) bool {
			return t.Less(t.words[i], t.words[aux])
		})
		end := sort.Search(t.Len(), func (i int) bool {
			return t.Less(t.words[aux], t.words[i])
		})
		fmt.Println(start)
		fmt.Println(end)
	}
}
