#pragma once

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** @brief 8-bit boolean type */
typedef _Bool boolean;

#define false 0
#define true 1

#ifdef DEBUG
#define debug_printf(fmt, ...) printf(fmt);
#else
#define debug_printf(fmt, ...) /* Do nothing */
#endif