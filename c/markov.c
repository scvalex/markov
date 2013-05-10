#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

const int ngram_n = 3;

/* Compare the `ngram_n` words. */
int wordsncmp(char *p, char *q) {
    int n = ngram_n;
    for (; *p == *q; ++p, ++q) {
        if (*p == 0) {
            n--;
            if (n == 0) {
                return 0;
            }
        }
    }
    return (*p-*q);
}

int sortcmp(const void *a, const void *b) {
    return wordsncmp(*(char**)a, *(char**)b);
}

/* Move `text` by skipping `n` words. */
char* skip_words(char *text, int n) {
    for (; n > 0; ++text)
        if (*text == 0)
            --n;
    return text;
}

int main(int argc, char *argv[]) {
    srand(time(0));

    /* Reading more than this number of chars will cause a panic. */
    int text_size = 4 * 1024 * 1024;
    char *text = (char*)calloc(text_size, sizeof(char));
    assert(text);

    /* Reading more than this number of words will cause a panic. */
    int words_count = 1024 * 1024;
    char **words = (char**)calloc(words_count, sizeof(char*));
    assert(words);

    /* Read text and split it into space delimited words. */
    int nword = 0;
    words[0] = text;
    while (scanf("%s", words[nword]) != EOF) {
        assert(nword < words_count - ngram_n);
        assert(words[nword] < text + text_size - 20);

        /* Note that we're leaving a 0 after each word. */
        words[nword+1] = words[nword] + strlen(words[nword]) + 1;
        ++nword;
    }

    /* Pre-process words. */
    qsort(words, nword, sizeof(words[0]), sortcmp);

    /* Print priming words. */
    /* Find a fullstop. */
    int aux = -1;
    while (aux == -1) {
        aux = rand() % nword;
        while (words[aux] && words[aux][strlen(words[aux]) - 1] != '.') {
            ++aux;
        }
    }
    /* words[aux] is now something ending in a fullstop, so don't
     * print the first word. */
    for (int i = 1; i < ngram_n && skip_words(words[aux], i) != 0; ++i)
        printf("%s ", skip_words(words[aux], i));

    char *phrase = words[aux];
    for (int wordsleft = 300; wordsleft > 0; --wordsleft) {
        /* Binary search for last printed word. */
        int l, u, m;
        l = -1;
        u = nword;
        while (l+1 != u) {
            m = (l+u)/2;
            if (wordsncmp(words[m], phrase) < 0)
                l = m;
            else
                u = m;
        }

        /* Find all word sequences that start with `phrase`. */
        int start = u;
        int end = u + 1;
        for (; wordsncmp(phrase, words[end]) == 0; ++end)
            ;
        --end;

        /* Select one of the sequences at random. */
        char *next_phrase = words[start + rand() % (end - start + 1)];

        phrase = skip_words(next_phrase, 1);
        if (strlen(skip_words(phrase, ngram_n - 1)) == 0)
            continue;
        printf("%s ", skip_words(phrase, ngram_n - 1));
    }
    printf("\n");

    free(words);
    free(text);

    return 0;
}
