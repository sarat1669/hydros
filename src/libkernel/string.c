#include <string.h>
#include <console.h>
#include <debug.h>
#include <memory.h>

// Count the number of characters in a string.
int kstrlen(char* str) {
	int i = 0;
	
	while(*(str++) != 0)
		i++;
	
	return i;
}

// Reverse a given (non-constant) string
void kstrrev(char* str) {
	kmemrev(str, kstrlen(str));
}

// Copy a string
void kstrcpy(char* dest, char* src) {
	kmemcpy(dest, src, kstrlen(src) + 1);
}

// Compares 2 strings
char kstrcmp(char* str1, char* str2) {
	while((*str1 != 0) && (*str2  != 0))
		if(*(str1++) != *(str2++))
			return 0;

	return (*str1 == 0) && (*str2 == 0);
}

char kstrncmp(char* str1, char* str2, int n) {
	while(n-- > 0)
		if(*(str1++) != *(str2++))
			return 0;

	return 1;
}

// Generate a strng in the space provided that represents
// the given number in the given base.
void kitoa(long i, int base, char* str) {
	// TODO: Support for negative numbers
	char* str_start = str;
	if(i == 0)
		*(str++) = '0';
	
	// Generate the string in reverse
	while(i != 0) {
		*(str++) = "0123456789abcdef"[i % base];
		i /= base;
	}

	// Terminate the string
	*str = 0;

	// Reverse the string so that the numbers are in the correct order.
	kstrrev(str_start);
}
