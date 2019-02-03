/*
 * db_FORTRAN.c
 *
 * Copyright 2019 Tom Fraser <thomas.abercrombie.fraser@gmail.com>
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
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Project: db_FORTRAN - MySQL Fortran Interface
 * File: db_FORTRANLib.c
 * Version: 6.00
 * Author: Tom Fraser
 * Description: SQLite interface for FORTRAN - c wrapper
 * Date created: 22-NOV-2018
 * Date last modified: 02-FEB-2019
 * GNU General Public License <http://www.gnu.org/licenses/>.
 * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 */

#include <my_global.h>
#include <mysql.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "utilities.h"

#define LIFE_THE_UNIVERSE_EVERYTHING 0x2A
#define OBJECT_TABLE _object_table
#define CONNECTION (MYSQL *) OBJECT_TABLE[1]
#define MAX_OBJECTS 0x3E8
#define SAFETY_MARGIN 0x00A
#define SYS_LOW_LIMIT 0x007
#define MySQL_SUCCESS 0x00
#define MySQL_GENERAL_FAIL -1

struct _signaller
{
	int signal;
	int object;
	int flag;
	int data_external;
	int int_val;
	int16_t int_val_16;
	int32_t int_val_32;
	int64_t int_val_64;
	__int128_t int_val_128;
	float float_val;
	double double_val;
	long double long_double_val;
	char short_buffer[65];
	char message[1000];
	char aux_message[1000];
	char aux_message_1[1000];
	char aux_message_2[1000];
};

enum _signals
{
	sig_login=0x00,
	sig_connect,
	sig_disconnect,
	sig_execute_command,
	sig_select_database, // obsolete
	sig_retrieve_data,
	sig_next_record,
	sig_previous_record,
	sig_print_row,
	sig_go_first,
	sig_go_last,
	sig_goto_n,
	sig_goto_start,
	sig_goto_end,
	sig_item_return,
	sig_switch_database,
	sig_stream,
	sig_list_databases,
	sig_next_source //17
};

enum _object_types
{
	SESSION = 0x00,
	NOTHING,
	DATABASE,
	STEPTHROUGH,
	RECORDSET,
	DATAHEADER,
	STREAM
};

enum _object_status
{
	BLANK = 10,
	SQL_SET,
	IN_USE,
	DONE,
	LOST_CONTEXT
};

struct _GENERIC // don't instantiate!
{
	enum _object_types o_type;
	int ref;
	char * text;
};

struct _session
{
	enum _object_types o_type;
	int ref;
	char * text;
	long my_size;
	int obj_count;
	int next_slot;
	int high_water_level;
	unsigned short reporting_on;
	unsigned short db_attached;
}_the_session;

struct _long_text
{
    int ref;
    int ordinal;
    char * text;
    int len;
}_the_only_one;

struct _database
{
	enum _object_types o_type;
	int ref;
	char * text;
};
typedef struct _database _db;

struct _recordset
{
	enum _object_types o_type;
	int ref;
	char * text;
	void * result;
	MYSQL_ROW * row;
	MYSQL_FIELD * metadata;
	enum _object_status o_status;
	long record_count;
	long field_count;
	long traversal;
};
typedef struct _recordset _rs;

struct _setting
{
	int mutiple_sources;
	int show_nulls;
}_DB_setting;

enum _fatal_error
{
	NO_SESSION=0x00,
	NO_CONNECTION,
	MEMORY_TABLE_CORRUPTED,
	CANT_ALLOCATE_MEMORY
};

enum _return_type
{
	IS_NUMERIC=0,
	IS_TEXT,
	IS_INT,
	IS_FLOAT
};

enum _stream_to
{
	screen = 0,
	file = 1,
	CSV = 2,
	TSV = 3
};

extern struct _signaller _send_signal;
extern struct _signaller _receive_signal;
typedef size_t * _address;
_address _object_table;
short session_in_progress = 0;
int _session_in_progress();
struct _GENERIC * _object_ptr(int ref, enum _object_types obj_type);
extern char * get_a_line(const char * prompt);
extern char * get_a_password(const char * prompt);
extern char * get_n_chars(const char * prompt, unsigned short i);
extern char * get_limited_password(const char * prompt,
		unsigned short i);
extern unsigned short max_login_;
extern void call_exit_();
void _get_more_object_space();
int _free_all();
void _free_db(int ref);
void _free_RS(int ref);
int _c_signal(char * what);
int _decode_signal(char * what);
int _login();
int _connect();
int _select_db();
int _execute_command(char * what);
int _retrieve_data(char * what);
int _new_RS(char * SQL);
int _check_in(void * o);
int _check_out(int kill_this);
int _update_slots();
int _next_record();
int _previous_record();
int _print_row();
int _go_first();
int _go_last();
int _goto_n();
int _goto_start();
int _goto_end();
int _item_value();
int _get_item_ordinal(const _rs * RS);
int _switch_database();
void _message(const char * info);
void _blank_slate();
void _list_databases();
int _stream();
int _text_copy(char * s);
static const char * DB_ALREADY_CONNECTED =
	"db_FORTRAN warning: Already connected to server!";
static const char * NO_DB_CONNECTED =
	"ERROR 1046 (3D000): No database is currently loaded!";
static const char * NOT_A_DB =
	"db_FORTRAN warning: Specified database is NOT on this server!";
static const char * NOT_A_RECORDSET =
	"db_FORTRAN warning: Object is NOT a recordset!";
static const char * NO_DaTA =
	"db_FORTRAN Info: Retrieval resulted in NO ROWS.";
static const char * RECORDSET_RANGE =
	"db_FORTRAN warning: Requested row is OUTSIDE the recordset range!";
static const char * LOCO_INCOGNITO =
	"db_FORTRAN warning: Unimplemented parameter transmission location: ";
static const char * AINT_GOT_THAT =
	"db_FORTRAN warning: Requested field %s is not in this recordset!";
static const char * HOST_MSG = "Host [Max %d characters]: ";
static const char * UID_MSG = "User ID [Max %d characters]: ";
static const char * PWD_MSG = "Password [Max %d characters]: ";
static const char * DB_MSG = "Database [Max %d characters]: ";
static const char * RS_EXTRACTED = "OK, %d Records extracted.\n";
static const char * RS_END = "db_FORTRAN Info: End of Recordset!";
static const char * RS_START = "Start of Recordset!";
static const char * RS_ROW =
		"No row available, either recordset BOF or EOF!";
static const char * SEPERATOR =
		"-----------------------------------------\n";
static const char * AINT_NUMERIC = "of this collection is not a numeric.";
static const char * DB_SWITCHED = "OK, switched to new database: %s";
static const char * CANT_OPEN_FILE = "Failed to open file: ";
static const char * db_FORTRAN_ERROR = "db_FORTRAN Error";
static const char * NOT_INT =
	"db_FORTRAN Warning: requested field does NOT contain an integer!";
static const char * NOT_REAL =
	"db_FORTRAN Warning: requested field does NOT contain a real value!";
static const char * MIXED_CONTENT =
	"db_FORTRAN Warning: requested field has NON-NUMERICAL data:";
static const char * NO_MORE_RS =
	"db_FORTRAN Info: no more recordsets available.";
static const char * NULL_S='\0';
static const char * NULLS="(null)";
static const char * UNKNOWN_SIGNAL=
	"db_FORTRAN Warning: Unrecognised signal received!";
	
int _c_signal(char * what)
{
	int ret_val = MySQL_GENERAL_FAIL;
	if ((session_in_progress == 0)
			&& (_send_signal.signal != sig_connect)/*must be connect */
			&& (_send_signal.signal != sig_login)) /* or login */
	{
		_receive_signal.int_val = NO_SESSION;
		call_exit_(); // request is bollox, bail out!
	}
	return _decode_signal(what);
}

int _decode_signal(char * what)
{

	int ret_val = MySQL_GENERAL_FAIL;
	switch(_send_signal.signal)
	{
		case sig_login:
			if(session_in_progress!=0)
			{
				_message(DB_ALREADY_CONNECTED);
				return 0;
			}
			ret_val=_login();
			break;
		case sig_connect:
			if(session_in_progress!=0)
			{
				_message(DB_ALREADY_CONNECTED);
				return 0;
			}
			ret_val = _connect();
			break;
		case sig_execute_command:
			ret_val = _execute_command(what);
			break;
		case sig_disconnect:
			ret_val = _free_all();
			break;
		case sig_retrieve_data:
			ret_val = _retrieve_data(what);
			break;
		case sig_next_record:
			ret_val = _next_record();
			break;
		case sig_previous_record:
			ret_val = _previous_record();
			break;
		case sig_print_row:
			ret_val = _print_row();
			break;
		case sig_go_first:
			ret_val = _go_first();
			break;
		case sig_go_last:
			ret_val = _go_last();
			break;
		case sig_goto_n:
			ret_val = _goto_n();
			break;
		case sig_goto_start:
			ret_val = _goto_start();
			break;
		case sig_goto_end:
			ret_val = _goto_end();
			break;
		case sig_item_return:
			ret_val = _item_value();
			break;
		case sig_switch_database:
			ret_val = _switch_database();
			break;
		case sig_list_databases:
			_list_databases();
			ret_val = 0;
			break;
		case sig_stream:
			ret_val = _stream();
			break;
		case sig_next_source:
			ret_val = _next_source();
			break;
		default:
			_message(UNKNOWN_SIGNAL);
	}
	return ret_val;
}

int _login()
{
	MYSQL * conn = malloc(sizeof(MYSQL));
	MYSQL_RES * result;
	MYSQL_ROW row;
	int num_fields;
	char prompt[32];
	int multi = 0;
	sprintf(prompt, HOST_MSG, max_login_);
	char *host = get_n_chars(prompt, max_login_);
	sprintf(prompt, UID_MSG, max_login_);
	char *uid = get_n_chars(prompt, max_login_);
	sprintf(prompt, PWD_MSG, max_login_);
	char *password = get_limited_password(prompt, max_login_);
	sprintf(prompt, DB_MSG, max_login_);
	char *db = get_n_chars(prompt, max_login_);
	printf("%s", SEPERATOR);
	nl(1);
	mysql_library_init(0, NULL, NULL);
	conn = mysql_init(NULL);
	if(_DB_setting.mutiple_sources==0) multi = CLIENT_MULTI_STATEMENTS;
	conn = mysql_real_connect(conn,host,uid,password,db,0,NULL, multi);
	if(conn==NULL)
	{
		free(host);
		free(uid);
		free(password);
		free(db);
		free(conn);
		_receive_signal.int_val = NO_CONNECTION;
		call_exit_();
	}
	else
	{
		_the_session.o_type = SESSION;
		session_in_progress = 1;
		_the_session.ref = 0;
		_the_session.text = host;
		_the_session.reporting_on = 1;
		_the_session.my_size = MAX_OBJECTS;
		_object_table = malloc(sizeof(size_t) * MAX_OBJECTS);
		_blank_slate();
		OBJECT_TABLE[0] = &_the_session;
		OBJECT_TABLE[1] = conn;
		OBJECT_TABLE[2] = host;
		OBJECT_TABLE[3] = uid;
		OBJECT_TABLE[4] = password;
		if (db==NULL)
		{
			OBJECT_TABLE[5] = NULL;
			OBJECT_TABLE[6] = NULL;
			_the_session.next_slot = 7;
			_the_session.high_water_level = 8;
			_the_session.obj_count = SYS_LOW_LIMIT;
			_the_session.db_attached=0;
		}
		else
		{
			_db * d = malloc(sizeof(_db));
			d->o_type = DATABASE;
			d->ref = 5;
			d->text = db;
			OBJECT_TABLE[5] = d;
			OBJECT_TABLE[6] = d;
			_the_session.next_slot = SYS_LOW_LIMIT;
			_the_session.obj_count = SYS_LOW_LIMIT;
			_the_session.high_water_level = SYS_LOW_LIMIT +1;
			_the_session.db_attached=1;
		}
	}
	return 0;
}

int _connect()
{
	MYSQL *conn = malloc(sizeof(MYSQL));
	MYSQL_RES *result;
	MYSQL_ROW row;
	int num_fields;
	int i;
    char *pw;
    char prompt[32];
    int multi = 0;
    if(_send_signal.aux_message_1[0] == '\0')
    {
        sprintf(prompt, PWD_MSG, max_login_);
	    pw = get_limited_password(prompt, max_login_);
	    _message(SEPERATOR);
	    nl(1);
	}
	else
	{
		int len=strlen(_send_signal.aux_message_1);
		pw=malloc(len+1);
	    strcpy(pw,_send_signal.aux_message_1);
	}
	mysql_library_init(0, NULL, NULL);
	conn = mysql_init(NULL);
	if(_DB_setting.mutiple_sources==0) multi = CLIENT_MULTI_STATEMENTS;
	conn = mysql_real_connect(conn,_send_signal.message,
			_send_signal.aux_message, pw,
			_send_signal.aux_message_2,0,NULL,multi);
	if(conn==NULL)
	{
		free(conn);
		free(pw);
		_receive_signal.int_val = NO_CONNECTION;
		call_exit_();
	}
	else
	{
		_the_session.o_type = SESSION;
		session_in_progress = 1;
		_the_session.ref = 0;
		_the_session.reporting_on = 1;
		_the_session.my_size = MAX_OBJECTS;
		_object_table = malloc(sizeof(size_t) * MAX_OBJECTS);
		_blank_slate();
		OBJECT_TABLE[0] = &_the_session;
		OBJECT_TABLE[1] = conn;
		char * p = malloc(strlen(_send_signal.message)+1);
		strcpy(p,_send_signal.message);
		OBJECT_TABLE[2] = p;
		_the_session.text = p;
		p = malloc(strlen(_send_signal.aux_message)+1);
		strcpy(p,_send_signal.aux_message);
		OBJECT_TABLE[3] = p;
		p = malloc(strlen(_send_signal.aux_message_1)+1);
		strcpy(p,pw);
		OBJECT_TABLE[4] = p;
		if (strlen(_send_signal.aux_message_2)==0)
		{
			OBJECT_TABLE[5] = NULL;
			OBJECT_TABLE[6] = NULL;
			_the_session.next_slot = 7;
			_the_session.obj_count = SYS_LOW_LIMIT;
			_the_session.db_attached=0;
			_the_session.high_water_level = SYS_LOW_LIMIT + 1;
		}
		else
		{
			_db * d = malloc(sizeof(_db));
			d->o_type = DATABASE;
			d->ref = 5;
			char * p = malloc(strlen(_send_signal.aux_message_2)+1);
			strcpy(p,_send_signal.aux_message_2);
			d->text = p;
			OBJECT_TABLE[5] = d;
			OBJECT_TABLE[6] = d;
			_the_session.next_slot = SYS_LOW_LIMIT;
			_the_session.obj_count = SYS_LOW_LIMIT;
			_the_session.high_water_level = SYS_LOW_LIMIT + 1;
			_the_session.db_attached=1;
		}
	}
	return 0;
}

int _switch_database()
{
	int ret_val = MySQL_GENERAL_FAIL;

	if (mysql_select_db(CONNECTION, _send_signal.short_buffer)!=0)
	{
		char * msg = strcat(_send_signal.short_buffer, " : ");
		strcat(msg, NOT_A_DB);
		_message(msg);
		return ret_val;
	}
	else
	{
		_db * d = malloc(sizeof(_db *));
		d->o_type = DATABASE;
		if (OBJECT_TABLE[5] == NULL)
		{
			d->ref = 5;
			char * dtext = malloc(strlen(_send_signal.short_buffer)+1);
			strcpy(dtext,_send_signal.short_buffer);
			d->text = dtext;
			OBJECT_TABLE[5] = d;
			OBJECT_TABLE[6] = d;
		}
		else
		{
			int ref = _check_in(d);
			d->ref = ref;
			char * dtext = malloc(strlen(_send_signal.short_buffer)+1);
			strcpy(dtext,_send_signal.short_buffer);
			d->text = dtext;
			d->o_type = DATABASE;
			OBJECT_TABLE[ref] = d;
		}
		_the_session.db_attached=1;
		ret_val = MySQL_SUCCESS;
		char msg[100];
		sprintf(msg, DB_SWITCHED, _send_signal.short_buffer);
		_message(msg);
	}
	return ret_val;
}

struct _GENERIC * _object_ptr(int ref, enum _object_types obj_type)
{
	if (ref < 5) return NULL;
	if (OBJECT_TABLE[ref] == NULL) return NULL;
	struct _GENERIC * ret_val = OBJECT_TABLE[ref];
	if (ret_val->o_type != obj_type) return NULL;
	return ret_val;
}

void _message(const char * info)
{
	if(_the_session.reporting_on) puts(info);
}

int _execute_command(char * what)
{
	int ret_val = MySQL_GENERAL_FAIL;

	if(_send_signal.data_external) ret_val=
		mysql_query(CONNECTION, what);
	else ret_val = mysql_query(CONNECTION, _send_signal.message);
	if(mysql_error(CONNECTION)[0])
	{
		printf("\nSQL Error: %s\n", mysql_error(CONNECTION));
	}
	else
	{
		printf("\nOK, %lld Row(s) affected.\n",
				mysql_affected_rows(CONNECTION));
	}
	return ret_val;
}

int _retrieve_data(char * what)
{
	int ret_val = MySQL_GENERAL_FAIL;
	struct _recordset * this;
	MYSQL_RES * result = malloc(sizeof(MYSQL_RES *));
	int num_fields;
	int ref;
	int i;

	if (_send_signal.data_external)	mysql_query(CONNECTION, what);
	else mysql_query(CONNECTION, _send_signal.message);
	result = mysql_store_result(CONNECTION);
	if (result == NULL)
	{
		if (mysql_errno(CONNECTION)!=0) printf("\n%s\n",
					(mysql_error(CONNECTION)));
		_message(NO_DaTA);
		return ret_val;
	}
	ref = _send_signal.object;
	if (ref == 0)
	{
		this = (_rs *) malloc(sizeof(struct _recordset));
		ref = _check_in(this);
	}
	else
	{
		this = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
		free(this->text);
		mysql_free_result(this->result);
		free(this->row);
	}
	this->o_type = RECORDSET;
	this->ref = ref;
	ret_val = ref;
	if (_send_signal.data_external)
	{
		this->text = malloc(strlen(what)+1);
		strcpy(this->text, what);
	}
	else
	{
		this->text = malloc(strlen(_send_signal.message)+1);
		strcpy(this->text, _send_signal.message);
	}
	this->result = result;
	this->row=NULL;
	this->field_count = mysql_num_fields(result);
	this->o_status = IN_USE;
	this->record_count =  mysql_num_rows(result);
	char c[35];
	sprintf(c, RS_EXTRACTED, this->record_count);
	_message(c);
	this->traversal=-1;
	MYSQL_FIELD * fields = malloc(sizeof(MYSQL_FIELD));
	fields = mysql_fetch_fields(result);
	this->metadata = fields;
	return ret_val;
}

int _check_in(void * o)
{
	if (_the_session.my_size - _the_session.obj_count <= SAFETY_MARGIN)
	{
		_get_more_object_space();
	}
	int ret_val = _the_session.next_slot;
	if (OBJECT_TABLE[ret_val] != NULL)
	{
		ret_val = _update_slots();
		if (ret_val == -1)
		{
			_receive_signal.int_val = MEMORY_TABLE_CORRUPTED;
			_free_all();
			call_exit_();
		}
	}
	OBJECT_TABLE[ret_val] = o;
	++_the_session.obj_count;
	struct _GENERIC * oo = (struct _GENERIC *) o;
	oo->ref = ret_val;
	if (ret_val >= _the_session.high_water_level)
		++_the_session.high_water_level;
	_update_slots();
	return ret_val;
}

int _check_out(int kill_this)
{
	int ret_val = -1;
	OBJECT_TABLE[kill_this] = NULL;
	_the_session.next_slot = _update_slots();
	--_the_session.obj_count;
	return _the_session.next_slot;
}

int _update_slots()
{
	int ret_val = -1;
	int i;
	for (i=SYS_LOW_LIMIT;i<=_the_session.high_water_level;i++)
	{
		if (OBJECT_TABLE[i] == NULL)
		{
			_the_session.next_slot = i;
			ret_val = i;
			break;
		}
	}
	return ret_val;
}

void _blank_slate()
{
	memset(_object_table, 0X00, sizeof(size_t) * _the_session.my_size);
}

void _get_more_object_space()
{
	int current = _the_session.my_size;
	_object_table = (_address) realloc(_object_table,
					sizeof(size_t) * (current + MAX_OBJECTS));
	if (_object_table == NULL)
	{
		_receive_signal.int_val = CANT_ALLOCATE_MEMORY;
		_free_all();
		call_exit_();
	}
	for (int i=current;i<current + MAX_OBJECTS;i++)
	{
		OBJECT_TABLE[i] = (size_t) 0;
	}
	_the_session.my_size = current + MAX_OBJECTS;
}

int _free_all()
{
	int ret_val = MySQL_GENERAL_FAIL;
	if (OBJECT_TABLE[4]!=NULL)
	{
		int l = strlen(OBJECT_TABLE[4]);
		memset(OBJECT_TABLE[4], 0X00, l);
		free(OBJECT_TABLE[4]);
		OBJECT_TABLE[4] = NULL;
	}
	if (OBJECT_TABLE[2]!=NULL)
	{
		int l = strlen(OBJECT_TABLE[2]);
		memset(OBJECT_TABLE[2], 0X00, l);
		free(OBJECT_TABLE[2]);
		OBJECT_TABLE[2] = NULL;
	}
	if (OBJECT_TABLE[3]!=NULL)
	{
		int l = strlen(OBJECT_TABLE[3]);
		memset(OBJECT_TABLE[3], 0X00, l);
		free(OBJECT_TABLE[3]);
		OBJECT_TABLE[3] = NULL;
	}
	_free_db(5);
	OBJECT_TABLE[6] = NULL;
	for (int i=SYS_LOW_LIMIT;i<_the_session.high_water_level;i++)
	{
		if (OBJECT_TABLE[i] != NULL)
		{
			void * whatever = _object_ptr(i, RECORDSET);
			if (whatever != NULL)
			{
				_free_RS(i);
			}
			else
			{
				_free_db(i);
			}
		}
	}
	mysql_close(CONNECTION);
	free(_object_table);
	mysql_library_end();
	ret_val = MySQL_SUCCESS;
	session_in_progress = 0;
	return (ret_val);
}

void _free_db(int ref)
{

	if (OBJECT_TABLE[ref]!=NULL)
	{
		_db *db = (_db*) _object_ptr(ref, DATABASE);
		free(db->text);
		free(db);
		OBJECT_TABLE[ref] = NULL;
	}
}

void _free_RS(int ref)
{
	if (OBJECT_TABLE[ref]!=NULL)
	{
		_rs *rs = (_rs*) _object_ptr(ref, RECORDSET);
		if (rs!=NULL)
		{
			if(rs->text!=NULL) free(rs->text);
			if(rs->row!=NULL) rs->row=NULL;
			if(rs->result!=NULL) mysql_free_result(rs->result);
			free(rs);
			OBJECT_TABLE[ref] = NULL;
		}
	}
}

int _next_record()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if (RS->traversal==RS->record_count)
	{
		RS->row=NULL;
		_message(RS_END);
		return ret_val;
	}
	++RS->traversal;
	if (RS->traversal==RS->record_count)
	{
		RS->row=NULL;
		return ret_val;
	}
	mysql_data_seek(RS->result, RS->traversal);
	RS->row = mysql_fetch_row(RS->result);
	if (RS->row)
	{
		ret_val = MySQL_SUCCESS;
	}
	return ret_val;
}

int _previous_record()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if (RS->traversal==-1)
	{
		RS->row=NULL;
		_message(RS_START);
		return ret_val;
	}
	--RS->traversal;
	if (RS->traversal==-1)
	{
		RS->row=NULL;
		return ret_val;
	}
	mysql_data_seek(RS->result, RS->traversal);
	RS->row = mysql_fetch_row(RS->result);
	if (RS->row)
	{
		ret_val = MySQL_SUCCESS;
	}
	return ret_val;
}

int _print_row()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	char * null_placeholder="";
	if(_DB_setting.show_nulls==1) null_placeholder=NULL_S;
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if (RS->row)
	{
		for(int i=0;i<RS->field_count;i++)
		{
			printf("%s\t", RS->row[i] ? RS->row[i] : null_placeholder);
		}
		printf("\n");
		ret_val = MySQL_SUCCESS;
	}
	else _message(RS_ROW);
	return ret_val;
}

int _go_first()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	RS->traversal = 0;
	mysql_data_seek(RS->result, RS->traversal);
	RS->row = mysql_fetch_row(RS->result);
	if (RS->row) ret_val = MySQL_SUCCESS;
	return ret_val;
}

_go_last()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	RS->traversal = RS->record_count-1;
	mysql_data_seek(RS->result, RS->traversal);
	RS->row = mysql_fetch_row(RS->result);
	if (RS->row)ret_val = MySQL_SUCCESS;
	return ret_val;
}

int _goto_n()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if ((_send_signal.int_val<1)||
			(_send_signal.int_val>RS->record_count))
	{
		_message(RECORDSET_RANGE);
		RS->traversal = RS->record_count;
		RS->row=NULL;
		return ret_val;
	}
	RS->traversal = _send_signal.int_val-1;
	mysql_data_seek(RS->result, RS->traversal);
	RS->row = mysql_fetch_row(RS->result);
	if (RS->row) ret_val = MySQL_SUCCESS;
	return ret_val;
}

int _goto_start()
{
	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	RS->traversal = -1;
	return MySQL_SUCCESS;
}

int _goto_end()
{
	int ret_val = MySQL_GENERAL_FAIL;
	MYSQL_ROW row;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	RS->traversal = RS->record_count;
	return MySQL_SUCCESS;
}

int _item_value()
{
	int ret_val = MySQL_GENERAL_FAIL;
	int len;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	int requested;
	char buffer [150];
	int n;
	switch(_send_signal.int_val_16)
	{
		case 1:
			requested = _send_signal.int_val;
			break;
		case 2:
			requested = _get_item_ordinal(RS);
			if (requested==-1)
			{
				sprintf(buffer,AINT_GOT_THAT,_send_signal.short_buffer);
				_message(buffer);
				return ret_val;
			}
			break;
		default:
			n = sprintf (buffer, "%s %s", LOCO_INCOGNITO,
			_send_signal.int_val);
			_message(buffer);
			return ret_val;
	}
	if (RS->row)
	{
		if ((_send_signal.int_val<=RS->field_count) &&
			(_send_signal.int_val>0))
		{
			if (RS->row[_send_signal.int_val-1])
			{
				len = strlen(RS->row[_send_signal.int_val-1]);
				if ((len<64) || (_send_signal.int_val_32>0))
				{
					switch(_receive_signal.flag)
					{
						case IS_INT:
						{
							char *p;
							char *s;
							long ret;

							s=RS->row[_send_signal.int_val-1];
							ret = strtol(s, &p, 10);
							if(p==s)
							{
								_message(NOT_INT);
							   ret_val=MySQL_GENERAL_FAIL;
							}
							else if(p-s<len)
							{
								_message(MIXED_CONTENT);
								_message(p);
								_receive_signal.int_val_128=ret;
								ret_val = MySQL_SUCCESS;
							}
							else
							{
								_receive_signal.int_val_128=ret;
								ret_val = MySQL_SUCCESS;
							}
							break;
						}
						case IS_FLOAT:
						{
							char *p;
							char *s;
							double ret;

							s=RS->row[_send_signal.int_val-1];
							ret = strtod(s, &p);
							if(p==s)
							{
								_message(NOT_REAL);
							   ret_val=MySQL_GENERAL_FAIL;
							}
							else if(p-s<len)
							{
								_message(MIXED_CONTENT);
								_message(p);
								_receive_signal.long_double_val=ret;
								ret_val = MySQL_SUCCESS;
							}
							else
							{
								_receive_signal.long_double_val=ret;
								ret_val = MySQL_SUCCESS;
							}
							break;
						}
						case IS_TEXT:
						{
						    if(_send_signal.int_val_32>0)
						    {
						        if(_send_signal.int_val_32>len)
						        {
						            memcpy(_receive_signal.short_buffer,
										RS->row[_send_signal.int_val-1]
										, len);
						        }
						        else
						        {
						            memcpy(_receive_signal.short_buffer,
									    RS->row[_send_signal.int_val-1]
									    ,_send_signal.int_val_32 );
						        }
						        ret_val = MySQL_SUCCESS;
						    }
						    else
						    {
							    _receive_signal.int_val = len;
							    memcpy(_receive_signal.short_buffer,
									RS->row[_send_signal.int_val-1]
									, len);
							    ret_val = MySQL_SUCCESS;
						    }
						}
					}
				}
				else
				{
					if (_receive_signal.flag>IS_TEXT)
					{
						n = sprintf (buffer, "Field %d %s", requested,
									   AINT_NUMERIC);
						_message(buffer);
						return(ret_val);
					}
					_the_only_one.len=len;
					_receive_signal.int_val = len;
					_the_only_one.ref=_send_signal.object;
                    _the_only_one.ordinal=_send_signal.int_val-1;
					ret_val = MySQL_SUCCESS;
				}
			}
		}
	}
	return ret_val;
}

int _text_copy(char * s)
{
    int ret_val = MySQL_GENERAL_FAIL;
    _rs * RS = (_rs *) _object_ptr(_the_only_one.ref, RECORDSET);
    if (RS->row[_the_only_one.ordinal])
    {
        memcpy((void *) s,(const void *) RS->row[_the_only_one.ordinal]
                ,_the_only_one.len);
        ret_val = MySQL_SUCCESS;
    }
    return ret_val;
}

int _get_item_ordinal(const _rs * RS)
{
	int ret_val = -1;
	for(int i=0;i<RS->field_count;i++)
	{
		//(MYSQL_FIELD *) ((_rs*) OBJECT_TABLE[_send_signal.object])->metadata[i].name
		if (strcmp(_send_signal.short_buffer, RS->metadata[i].name)==0)
		{
			ret_val=i+1;
			_send_signal.int_val=i+1;
			return ret_val;
		}
	}
	return ret_val;
}

void _list_databases()
{
	MYSQL_RES *result;
	MYSQL_ROW row;
	int num_fields;
	int i;
	result = mysql_list_dbs(CONNECTION, (char *)NULL);
	printf("Databases available in this session:\n");
	while(row = mysql_fetch_row(result))
	{
		printf("\t%s \n", row[0] ? row[0] : "NULL");
	}
	printf("OK, %lld Databases found.\n\n", mysql_num_rows(result));
	mysql_free_result(result);
}

int _stream()
{
	MYSQL_RES *result;
	MYSQL_ROW row;
	int num_fields;
	int i;

	int ret_val = MySQL_GENERAL_FAIL;
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	char * null_placeholder="";
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if(_send_signal.flag==0)
	{
		for(int i=0;i<RS->field_count;i++)
		{
			//(MYSQL_FIELD *) ((_rs*) OBJECT_TABLE[_send_signal.object])->metadata[i].name
			printf("%s\t|",RS->metadata[i].name);
		}
		nl(1);
		printf("----------------------------------------------\n");
		mysql_data_seek(RS->result, 0);
		if(_DB_setting.show_nulls==1) null_placeholder=NULL_S;
		while ((row = mysql_fetch_row(RS->result)))
		{
			for(int i=0;i<RS->field_count;i++)
			{
				printf("%s\t", row[i] ? row[i] : null_placeholder);
			}
			nl(1);
		}
		ret_val=MySQL_SUCCESS;
	}
	else
	{
		FILE *fp;
		char buffer[100];
		fp = fopen(_send_signal.message, "w+");
		char c;
		if(fp==NULL)
		{
		    snprintf (buffer, 100, " %s %s \n", CANT_OPEN_FILE,
			                        _send_signal.message);
			nl(1);
		    _message(buffer);
			return ret_val;
		}
		switch (_send_signal.flag)
		{
			case 1:	c=' ';
			break;
			case 2: c=',';
			break;
			case 3: c='\t';
			break;
			default: c=' ';
		}
		for(int i=0;i<RS->field_count;i++)
		{
			fprintf(fp,"%s",RS->metadata[i].name);
			fprintf(fp,"%c",c);
		}
		fprintf(fp,"\n");
		fprintf(fp,"----------------------------------------------\n");
		mysql_data_seek(RS->result, 0);
		if(_DB_setting.show_nulls==1) null_placeholder=NULLS;
		while ((row = mysql_fetch_row(RS->result)))
		{
			for(int i=0;i<RS->field_count;i++)
			{
				fprintf(fp,"%s", row[i] ? row[i] : null_placeholder);
				fprintf(fp,"%c",c);
			}
			fprintf(fp,"\n");
		}
		printf("\nOK, %lld records streamed to file: %s\n",
				RS->record_count, _send_signal.message);
		fclose(fp);
		ret_val=MySQL_SUCCESS;
	}
	return ret_val;
}

int _next_source()
{
	int ret_val=MySQL_GENERAL_FAIL;
	int ref = _send_signal.object;
	if(mysql_more_results(CONNECTION)==0)
	{
		_message(NO_MORE_RS);
		return ret_val;
	}
	_rs * RS = (_rs *) _object_ptr(_send_signal.object, RECORDSET);
	if (RS==NULL)
	{
		_message(NOT_A_RECORDSET);
		return ret_val;
	}
	if (RS->result!=NULL)
	{
		mysql_free_result(RS->result);
		free(RS->row);
	}
	if (mysql_next_result(CONNECTION)!=0)
	{
		_message(NO_MORE_RS);
		return ret_val;
	}
	RS->result = mysql_store_result(CONNECTION);
	RS->row=NULL;
	RS->field_count = mysql_num_fields(RS->result);
	RS->o_status = IN_USE;
	RS->record_count =  mysql_num_rows(RS->result);
	_message("\nNew data source");
	char ch[35];
	sprintf(ch, RS_EXTRACTED, RS->record_count);
	_message(ch);
	RS->traversal=-1;
	MYSQL_FIELD * fields = malloc(sizeof(MYSQL_FIELD));
	fields = mysql_fetch_fields(RS->result);
	RS->metadata = fields;
	return MySQL_SUCCESS;
}

int _session_in_progress()
{
    if(session_in_progress == 1) return 1;
    else return 0;
}
