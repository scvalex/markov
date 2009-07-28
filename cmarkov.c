#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXINPUT 10000000
#define MAXWORDS (MAXINPUT / 4)

char text[MAXINPUT];
char *words[MAXWORDS]; /* avg len of eng words = 5.10 */
int N; /* num of words */

/* read the file into text */
void readtext(char fn[]) {
	FILE *fi = fopen(fn, "r");
	fread(text, sizeof(char), MAXINPUT, fi);
	fclose(fi);
}

/* rm lf's that don't mark new para's */
void preprocess() {
	char *c;
	for (c = text+1; *c; ++c)
		if ((*c != '\n') && (*(c-1) == '\n'))
			*(c-1) = ' ';
}

/* fill in words with pointers to the start of words in text */
void getwords() {
	char *c;
	int newword = 1;
	for (c = text, N=0; *c && (N < MAXWORDS); ++c) {
		if (isalnum(*c)) {
			if (newword)
				words[N++] = c;
			newword = 0;
		} else {
			newword = 1;
		}
	}
}

/* print the i-th word */
void printword(int i) {
	char *c;
	for (c = words[i]; isalnum(*c); ++c)
		putchar(*c);
	printf("\n");
}

/* print all words between l and u */
void printwords(int l, int u) {
	for (; l <= u; ++l)
		printword(l);
}

int pstrcmp(const void *a, const void *b) {
	return strcmp(*(char**)a, *(char**)b);
}

/* sort words alphabetically */
void sortwords() {
	qsort(words, N, sizeof(words[0]), pstrcmp);
}

int main(int argc, char *argv[]) {
	readtext(argv[1]);
	preprocess();
	getwords();
	sortwords();

	printwords(100, 160);

	return 0;
}
