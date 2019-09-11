/*
 * utilities.h
 *
 * Copyright 2017 Tom Fraser <thomas.abercrombie.fraser@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 * Program:			utilities.h
 * Author:			Tom Fraser
 * Creation Date:	22-Nov-2018
 * Last update:		11-Sep-2019
 * Purpose:         utility routines library for i/o , etc.
*/

#ifndef UTILITIES_H
#define UTILITIES_H
#define _XOPEN_SOURCE 700
#define get_a_line_BUFFER 5

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>

// this is the definite truth:
enum boolean
{
    False=0,
    True=1
};

/* Interface: */
extern char * strchr(const char *str, int c);
extern char * getpass(const char * prompt);
void nl(unsigned int n);
enum boolean is_a_file(const char * filename);
void echo_on();
void echo_off();
char * get_a_line(const char * prompt);
char * get_a_password(const char * prompt);
enum boolean is_a_file(const char * filename);
char * get_n_chars(const char * prompt, int i);
char * get_limited_password(const char * prompt, int i);

#endif
