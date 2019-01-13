#ifndef STRING_H
#define STRING_H

int kstrlen(char* str);
char kstrcmp(char* str1, char* str2);
char kstrncmp(char* str1, char* str2, int);
void kstrcpy(char* dest, char* src);
void kstrrev(char* str);
void kitoa(long i, int base, char* str);

#endif
