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

/* print num words */
void printword(char *c, int num) {
	for (; *c && num; --num) {
		for (; *c && isalnum(*c); ++c)
			putchar(*c);
		for (; *c && !isalnum(*c); ++c)
			putchar(*c);
	}
}

/* print all words between l and u */
void printwords(int l, int u, int num) {
	for (; l <= u; ++l)
		printword(words[l], num);
	printf("\n");
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

int ORDER;
int pstrcmp(const void *a, const void *b) {
	return wordscmp(*(char**)a, *(char**)b, ORDER);
}

/* sort words alphabetically */
void sortwords(int num) {
	ORDER = num;
	qsort(words, N, sizeof(words[0]), pstrcmp);
}

int bsl(char *w, int num) {
	int i, l = 0;
	for (i = 1; i < N; i <<= 1)
		;
	for (; i > 0; i >>= 1) {
		if ((l+i < N) && (wordscmp(w, words[l+i], num) > 0))
			l += i;
	}
	++l;
	if (wordscmp(w, words[l], num) == 0)
		return l;
	return -1;
}

int bsu(char *w, int num) {
	int i, l = 0;
	for (i = 1; i < N; i <<= 1)
		;
	for (; i > 0; i >>= 1) {
		if ((l+i < N) && (wordscmp(w, words[l+i], num) >= 0))
			l += i;
	}
	if (wordscmp(w, words[l], num) == 0)
		return l;
	return -1;
}

/* generate n words starting with c */
void generatetext(char *c, int num, int n) {
	printword(c, num);
//	printf("\n");
	int l, u, wsf;
	char *s;
	srand(23);
	while (n--) {
//		printf("Current: ");
//		printword(c, num);
		l = bsl(c, num);
		u = bsu(c, num);
//		printf("\n(l, u) = (%d, %d)\n", l, u);
		s = words[l + (rand() % (u-l+1))];
//		printf("Selected: ");
//		printword(s, num+1);
//		printf("\n");

		/* skip over the first word, set the second as c and
		 * print the third (num-th actually) */
		for (wsf = 0; *s && (wsf <= num); ++s) {
			while (!isalnum(*s))
				++s;
			if (wsf == 1)
				c = s;
			if (wsf == num)
				printword(s, 1);
			while (isalnum(*s))
				++s;
			++wsf;
		}
//		printf("\n");
	}
	printf("\n");
}

int main(int argc, char *argv[]) {
	readtext(argv[1]);
	preprocess();
	getwords();
	sortwords(2);

	printwords(100, 120, 2);
	generatetext("A maxim", 2, 40);

	return 0;
}
