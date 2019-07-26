/*
 * utilities.c
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
 * Program:			utilities.c
 * Author:			Tom Fraser
 * Creation Date:	22-Nov-2017
 * Last update:		25-Dec-2017
 * Purpose:         utility routines library for i/o , etc.
 *
 * gcc -c -std=c11 utilities.c
 *
*/

#include "utilities.h"

struct termios current;
short can_restore=0;

enum boolean is_a_file(const char * filename)
{
    enum boolean ret=False;

	if (filename==NULL) return ret;
	if (access(filename, F_OK)!=-1) ret = True;
    return ret;
}

void nl(unsigned int n)
{
	if (n<1) return;
	for (int i=0;i<n;i++) printf("\n");
}

char * get_a_line(const char * prompt)
{
    char * input=NULL;
    char * ret=NULL;
    char * c=NULL;
    size_t buf_size=get_a_line_BUFFER; //or whatever
    int i;

    printf("%s", prompt);
    input=(char *) calloc(buf_size, sizeof(char));
    i=getline(&input,&buf_size,stdin);
    c=strchr(input,(unsigned char)0x0A);
    if(c!=NULL) *c='\0';
    i=strlen(input);
    if(i==0) return ret;
    ret=malloc((size_t) i+1);
    (char *) memccpy(ret,input,0X00,(size_t) i);
    memset(input,0X00,buf_size);
    free(input);
    return ret;
}

char * get_a_password(const char * prompt)
{
    char * ret;

    echo_off();
    ret=get_a_line(prompt);
    echo_on();
    nl(1);
    return(ret);
}

void echo_on()
{
    if(can_restore==1) tcsetattr(STDIN_FILENO, TCSANOW, &current);
}

void echo_off()
{
    struct termios terminal;
    char * name;

    if (!isatty(STDIN_FILENO))
    {
        // if we get here, the whole thing's bolloxed anyway
        fprintf(stderr, "\nNot a terminal.\n");
        exit (EXIT_FAILURE);
    }
    tcgetattr(STDIN_FILENO,&current);
    can_restore=1;
    tcgetattr(STDIN_FILENO, &terminal);
    terminal.c_lflag &= ~(ECHO);
    terminal.c_cc[VMIN] = 1;
    terminal.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal);
}

char * get_n_chars(const char * prompt, int i)
{
    char input[i+1];
    char * ret=NULL;
    char * c=NULL;
    int j=0;

    printf("%s", prompt);
    fgets(input,i,stdin);
    c=strchr(input,'\n');
    if(c!=NULL) *c=0;
    j=strlen(input);
    if(j==0) return ret;
    ret=(char*) malloc(j+1);
    memccpy(ret, input, 0, j+1);
    return ret;
}

char * get_limited_password(const char * prompt, int i)
{
    char * ret;

    echo_off();
    ret=get_n_chars(prompt, i);
    echo_on();
    nl(1);
    return(ret);
}
