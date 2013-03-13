#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int k = 2;
char inputchars[20000000];
char *word[4000000];
int nword;

int wordncmp(char *p, char *q) {
        int n = k;
        for (; *p == *q; ++p, ++q)
                if ((*p == 0) && (--n == 0))
                        return 0;
        return (*p-*q);
}

int sortcmp(const void *a, const void *b) {
        return wordncmp(*(char**)a, *(char**)b);
}

char *skip(char *c, int n) {
        for (; n > 0; ++c)
                if (*c == 0)
                        --n;
        return c;
}

int main(void) {
        srand(time(0));

        word[0] = inputchars;
        while (scanf("%s", word[nword]) != EOF) {
                word[nword+1] = word[nword] + strlen(word[nword]) + 1;
                ++nword;
        }

        int i;
        for (i = 0; i < k; ++i)
                word[nword][i] = 0;
        int aux = rand() % nword;
        for (i = 0; i < k; ++i)
                printf("%s ", word[aux+i]);
        qsort(word, nword, sizeof(word[0]), sortcmp);

        char *p, *phrase = word[aux];
        int wordsleft, l, u, m;
        for (wordsleft = 300; wordsleft > 0; --wordsleft) {
                l = -1;
                u = nword;
                while (l+1 != u) {
                        m = (l+u)/2;
                        if (wordncmp(word[m], phrase) < 0)
                                l = m;
                        else
                                u = m;
                }
                for (i = 0; wordncmp(phrase, word[u+i]) == 0; ++i)
                        if (rand() % (i+1) == 0)
                                p = word[u+i];
                phrase = skip(p, 1);
                if (strlen(skip(phrase, k-1)) == 0)
                        break;
                printf("%s ", skip(phrase, k-1));
        }

        return 0;
}
