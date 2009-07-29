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

/* print num words starting with the i-th one */
void printword(int i, int num) {
	char *c;
	int newword = 1;
	for (c = words[i]; *c && num; ++c) {
		putchar(*c);
		if (isalnum(*c)) {
			newword = 0;
		} else {
			if (!newword)
				--num;
			newword = 1;
		}
	}
	printf("\n");
}

/* print all words between l and u */
void printwords(int l, int u, int num) {
	for (; l <= u; ++l)
		printword(l, num);
}

/* compare words */
int wordscmp(char *c, char *d, int num) {
	for (; *c && *d; ++c, ++d) {
		if (!isalnum(*c)) {
			--num;
			if (!num)
				break;
		}
		while (!isalnum(*c) && !isalnum(*d))
			++c, ++d;
		if (*c != *d)
			return (*c-*d);
	}
	return 0;
}

int pstrcmp(const void *a, const void *b) {
	return wordscmp(*(char**)a, *(char**)b, 1);
}

/* sort words alphabetically */
void sortwords() {
	qsort(words, N, sizeof(words[0]), pstrcmp);
}

int bs(char *w, int num) {
	int i, l = 0;
	for (i = 1; i < N; i <<= 1)
		;
	for (; i > 0; i >>= 1)
		if ((l+i < N) && (wordscmp(w, words[l+i], num) > 0 ))
			l += i;
	if (wordscmp(w, words[l], num) == 0)
		return l;
	return -1;
}

int main(int argc, char *argv[]) {
	readtext(argv[1]);
	preprocess();
	getwords();
	sortwords();

	printwords(100, 110, 2);

	printf("%d\n", bs("A man", 1));

	return 0;
}
