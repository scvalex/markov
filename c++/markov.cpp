#include <assert.h>
#include <memory>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

using namespace std;

const int ngram_n = 3;

/* Compare the `ngram_n` words. */
int wordsncmp(const char *p, const char *q) {
    int n = ngram_n;
    for (; *p == *q; ++p, ++q) {
        if (*p == 0) {
            n--;
            if (n == 0) {
                return 0;
            }
        }
    }
    return (*p - *q);
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
    unique_ptr<char[]> text_up(new char[text_size]());
    char *text = (char*)text_up.get();

    /* Reading more than this number of words will cause a panic. */
    int words_count = 1024 * 1024;
    unique_ptr<char*[]> words_up(new char*[words_count]());
    char **words = (char**)words_up.get();

    /* Read text and split it into space delimited words. */
    int nwords = 0;
    words[0] = text;
    while (scanf("%s", words[nwords]) != EOF) {
        assert(nwords < words_count - ngram_n);
        assert(words[nwords] < text + text_size - 20);

        /* Note that we're leaving a 0 after each word. */
        words[nwords + 1] = words[nwords] + strlen(words[nwords]) + 1;
        ++nwords;
    }

    /* Pre-process words. */
    sort(words, words + nwords, [](const char *a, const char *b) {
            return (wordsncmp(a, b) < 0);
        });

    /* Print priming words. */
    /* Find a fullstop. */
    int aux = -1;
    while (aux == -1) {
        aux = rand() % nwords;
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
        /* Binary search for the last selected phrase. */
        int i = 1;
        for (; (i<<1) < nwords && wordsncmp(words[i<<1], phrase) < 0; i = i<<1)
            ;
        int lower = 0;
        for (; i > 0; i = i>>1) {
            if (lower + i < nwords && wordsncmp(words[lower + i], phrase) < 0)
                lower += i;
        }
        lower++;

        /* Find all word sequences that start with `phrase`. */
        int start = lower;
        int end = lower + 1;
        for (; wordsncmp(phrase, words[end]) == 0; ++end)
            ;
        --end;

        /* Select one of the sequences at random. */
        char *next_phrase = words[start + rand() % (end - start + 1)];

        /* Advance "phrase" past its first word and don't print the
         * first `ngram_n` words since they've already been
         * printed.' */
        phrase = skip_words(next_phrase, 1);
        if (strlen(skip_words(phrase, ngram_n - 1)) == 0)
            continue;
        printf("%s ", skip_words(phrase, ngram_n - 1));
    }
    printf("\n");

    return 0;
}
