#ifndef STRINGENCODINGTYPE_H
#define STRINGENCODINGTYPE_H

typedef enum {
    CE_NATIVE = 0,
    CE_UTF8   = 1,
    CE_LATIN1 = 2,
    CE_BYTES = 3,
    CE_SYMBOL = 5,
    CE_ANY    =99
} cetype_t;

#endif
