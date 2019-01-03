! db_FORTRAN.f95

! Copyright 2019 Tom Fraser <thomas.abercrombie.fraser@gmail.com>

! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program; if not, write to:
! Free Software Foundation, Inc.,
! 51 Franklin Street,
! Fifth Floor,
! Boston,
! MA 02110-1301,
! USA.

! * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! * Project: db_FORTRAN - MySQL Fortran Interface
! * File: db_FORTRAN.f95
! * Version: 1.00
! * Author: Tom Fraser
! * Description: MySQL interface for FORTRAN - wrapper
! * Date created: 30-NOV-2018
! * Date last modified: 01-JAN-2019
! * GNU General Public License <http://www.gnu.org/licenses/>.
! * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! gfortran -c db_FORTRAN.f95
! gcc -c db_FORTRANLib.c utilities.o `mysql_config --cflags --libs`

MODULE MySQL_data
USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, C_INT64_T, &
		C_INT32_T, C_FLOAT, C_DOUBLE, C_LONG_DOUBLE

	TYPE, BIND(C) :: signaller
		INTEGER(C_INT) :: signal
		INTEGER(C_INT) :: object
		INTEGER(C_INT) :: flag
		LOGICAL :: data_external
		INTEGER(C_INT) :: int_val
		INTEGER(C_INT16_T) :: int_val_16
		INTEGER(C_INT32_T) :: int_val_32
		INTEGER(C_INT64_T) :: int_val_64
		INTEGER(C_INT128_T) :: int_val_128
		REAL(C_FLOAT) :: float_val
		REAL(C_DOUBLE) :: double_val
		REAL(C_LONG_DOUBLE) :: long_double_val
		CHARACTER(LEN=65) :: short_buffer
		CHARACTER(LEN=1000) :: message
		CHARACTER(LEN=1000) :: aux_message
		CHARACTER(LEN=1000) :: aux_message_1
		CHARACTER(LEN=1000) :: aux_message_2
	END TYPE
	TYPE(signaller), BIND(C, NAME="_send_signal") :: send_signal
	TYPE(signaller), BIND(C, NAME="_receive_signal") :: receive_signal

!	utility memory
	CHARACTER (LEN=:), TARGET, ALLOCATABLE :: receive_buffer_1
	CHARACTER (LEN=:), TARGET, ALLOCATABLE :: receive_buffer_2
	CHARACTER (LEN=:), TARGET, ALLOCATABLE :: receive_buffer_3
	CHARACTER (LEN=:), POINTER :: receive_buffer_p

!	Interop signals
	INTEGER(C_INT), PARAMETER :: sig_login = z'0'
	INTEGER(C_INT), PARAMETER :: sig_connect = z'1'
	INTEGER(C_INT), PARAMETER :: sig_disconnect = z'2'
	INTEGER(C_INT), PARAMETER :: sig_execute_command = z'3'
	INTEGER(C_INT), PARAMETER :: sig_select_database = z'4' ! obsolete
	INTEGER(C_INT), PARAMETER :: sig_retrieve_data = z'5'
	INTEGER(C_INT), PARAMETER :: sig_next_record = z'6'
	INTEGER(C_INT), PARAMETER :: sig_previous_record = z'7'
	INTEGER(C_INT), PARAMETER :: sig_print_row = z'8'
	INTEGER(C_INT), PARAMETER :: sig_go_first = z'9'
	INTEGER(C_INT), PARAMETER :: sig_go_last = z'A'
	INTEGER(C_INT), PARAMETER :: sig_goto_n = z'B'
	INTEGER(C_INT), PARAMETER :: sig_goto_start = z'C'
	INTEGER(C_INT), PARAMETER :: sig_goto_end = z'D'
	INTEGER(C_INT), PARAMETER :: sig_item_return = z'E'
	INTEGER(C_INT), PARAMETER :: sig_switch_database = z'F'
	INTEGER(C_INT), PARAMETER :: sig_stream = z'10'
	INTEGER(C_INT), PARAMETER :: sig_list_databases = z'11' 
	INTEGER(C_INT), PARAMETER :: sig_next_source = z'12' !18

!	Formatting
	CHARACTER, PARAMETER :: nl = CHAR(10)

!	Fatal errors - immediate exit
	CHARACTER (LEN=*), PARAMETER :: no_session =  &
			"- db_FORTRAN Fatal Error!"//nl &
			//"Session not instantiated! You must login!"//nl
	CHARACTER (LEN=*), PARAMETER :: no_connection  =  &
			"- db_FORTRAN Fatal Error!"//nl &
			//"Cannot establish database connection!";
	CHARACTER (LEN=*), PARAMETER :: memory_table_corrupted = &
			"- db_FORTRAN Fatal Error!"//nl &
			//"Fatal memory corruption!";
	CHARACTER (LEN=*), PARAMETER :: cant_allocate_memory = &
			"- db_FORTRAN Fatal Error!"//nl &
			//"Can't allocate any more memory!";
	CHARACTER (LEN=*), PARAMETER :: improper_disconnection = &
			"- db_FORTRAN Fatal Error!"//nl &
			//"Failed to correctly disconnect!";

	! Error messages
	CHARACTER (LEN=*), PARAMETER :: fishy1 = &
			"Hmm, this paramenter list looks very suspicious!"
	CHARACTER (LEN=*), PARAMETER :: fishy2 = &
			"Hmm, this paramenter looks very suspicious!"
	CHARACTER (LEN=*), PARAMETER :: no_rs = &
			"db_FORTRAN Error. Recordset has no data!"//char(10) &
			//"You must retrieve data into the recordset before" &
			//" pulling down rows."
	CHARACTER (LEN=*), PARAMETER :: too_small = &
			"db_FORTRAN Warning: Value does not fit in" &
			//" the supplied variable:"
	CHARACTER (LEN=*), PARAMETER :: empty_filename = &
			"File name is BLANK! This is silly."
	! Login restrictions
	INTEGER(C_INT), BIND(C, NAME = "max_login_") :: max_login = z'40'

	! data send location indicators
	INTEGER(C_INT), PARAMETER :: ordinal = 1
	INTEGER(C_INT), PARAMETER :: short_buffer = 2

	! request type
	INTEGER(C_INT), PARAMETER :: is_numeric = 0
	INTEGER(C_INT), PARAMETER :: is_text = 1
	INTEGER(C_INT), PARAMETER :: is_int = 2
	INTEGER(C_INT), PARAMETER :: is_float = 3

	! steam options
	INTEGER(C_INT), PARAMETER :: to_screen = 0
	INTEGER(C_INT), PARAMETER :: to_file = 1
	INTEGER(C_INT), PARAMETER :: to_CSV = 2
	INTEGER(C_INT), PARAMETER :: to_TSV = 3

END MODULE MySQL_data
!***********************************************************************

MODULE MySQL_types
USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, C_FLOAT &
		, C_DOUBLE, C_LONG_DOUBLE
IMPLICIT NONE

	TYPE object
	PRIVATE
		INTEGER(C_INT) :: o_id
		INTEGER(C_INT) :: o_type
	END TYPE object

	TYPE db_session
		TYPE(object) :: obj
		INTEGER(C_INT) :: permissions
	END TYPE db_session

	TYPE db_database
		TYPE(object) :: obj
		INTEGER(C_INT) :: permissions
	END TYPE db_database

	TYPE db_stepthrough
		TYPE(object) :: obj
		LOGICAL :: in_memory = .FALSE.
		INTEGER :: parent
	END TYPE db_stepthrough

	TYPE db_recordset
		TYPE(object) :: obj
		LOGICAL :: in_memory = .FALSE.
		INTEGER :: parent
	END TYPE db_recordset

	PUBLIC :: disclose

CONTAINS

	SUBROUTINE set_o(o, o_id, o_type)
	USE iso_c_binding,	ONLY : C_INT
	IMPLICIT NONE
	TYPE(object) :: o
	INTEGER(C_INT) :: o_id, o_type
		o = object(o_id, o_type)
	END SUBROUTINE set_o

	SUBROUTINE set_step(o, o_id, o_type)
	USE iso_c_binding,	ONLY : C_INT
	IMPLICIT NONE
	TYPE(db_stepthrough) :: o
	INTEGER(C_INT) :: o_id, o_type
		CALL set_o(o%obj, o_id, o_type)
		o%in_memory = .TRUE.
	END SUBROUTINE set_step

	SUBROUTINE set_RS(o, o_id, o_type)
	USE iso_c_binding,	ONLY : C_INT
	IMPLICIT NONE
	TYPE(db_recordset) :: o
	INTEGER(C_INT) :: o_id, o_type
		CALL set_o(o%obj, o_id, o_type)
		o%in_memory = .TRUE.
	END SUBROUTINE set_RS

	SUBROUTINE o_print(o)
	USE iso_c_binding,	ONLY : C_INT
	IMPLICIT NONE
	TYPE(object) :: o
		PRINT *, o%o_id, o%o_type
	END SUBROUTINE o_print

	INTEGER FUNCTION disclose(o)
		USE iso_c_binding,	ONLY : C_INT
		TYPE(object) :: o
			disclose = o%o_id
	END FUNCTION disclose

END MODULE MySQL_types
!***********************************************************************

MODULE MySQL_interfaces
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
IMPLICIT NONE

	INTERFACE signal
		INTEGER(C_INT) FUNCTION c_signal(what) BIND &
			(C, NAME = "_c_signal")
		USE iso_c_binding,	ONLY : C_CHAR, C_INT
		CHARACTER :: what(*)
		END FUNCTION c_signal
	END INTERFACE signal

	INTERFACE db_login
		LOGICAL FUNCTION do_login()
		USE iso_c_binding, ONLY : C_CHAR, C_INT, C_NULL_CHAR
		USE MySQL_data, ONLY : sig_login, send_signal
		IMPLICIT NONE
		END FUNCTION do_login
	END INTERFACE db_login

	INTERFACE clear_send_signal
		SUBROUTINE clear_send()
		END SUBROUTINE clear_send

		SUBROUTINE quick_clear_send(i)
		INTEGER :: i
		END SUBROUTINE quick_clear_send
	END INTERFACE clear_send_signal

	INTERFACE clear_receive
		SUBROUTINE clear_receive()
		USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, &
				C_FLOAT,C_DOUBLE, C_LONG_DOUBLE, C_NULL_CHAR
		USE MySQL_data, ONLY : send_signal
		END SUBROUTINE clear_receive
	END INTERFACE

	INTERFACE db_connect
		LOGICAL FUNCTION db_connect(host, user, password, db_name)
		CHARACTER*(*), INTENT(IN) :: host
		CHARACTER*(*), INTENT(IN) :: user
		CHARACTER*(*), INTENT(IN), OPTIONAL :: password
		CHARACTER*(*), INTENT(IN), OPTIONAL :: db_name
		END FUNCTION db_connect
	END INTERFACE db_connect

	INTERFACE db_disconnect
		SUBROUTINE db_disconnect()
		END SUBROUTINE db_disconnect
	END INTERFACE db_disconnect

	INTERFACE db_switch_database
		LOGICAL FUNCTION do_switch(db_name)
		CHARACTER*(*), INTENT(IN) :: db_name
		END FUNCTION do_switch
	END INTERFACE db_switch_database

	INTERFACE db_execute
		LOGICAL FUNCTION db_execute(str_SQL)
		CHARACTER*(*), INTENT(IN) :: str_SQL
		CHARACTER(LEN=:), ALLOCATABLE ::what
		END FUNCTION db_execute
	END INTERFACE db_execute

	INTERFACE db_retrieve_data
		LOGICAL FUNCTION retrieve_RS(RS, str_SQL)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset) :: RS
		CHARACTER*(*), INTENT(IN) :: str_SQL
		END FUNCTION retrieve_RS
	END INTERFACE db_retrieve_data

	INTERFACE db_next_row
		LOGICAL FUNCTION db_next_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_next_RS
	END INTERFACE db_next_row

	INTERFACE db_previous_row
		LOGICAL FUNCTION db_previous_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_previous_RS
	END INTERFACE db_previous_row

	INTERFACE db_first_row
		LOGICAL FUNCTION db_first_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_first_RS
	END INTERFACE db_first_row

	INTERFACE db_last_row
		LOGICAL FUNCTION db_last_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_last_RS
	END INTERFACE db_last_row

	INTERFACE db_goto_row
		LOGICAL FUNCTION db_goto_RS(RS,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER :: i
		END FUNCTION db_goto_RS
	END INTERFACE db_goto_row

	INTERFACE db_goto_start
		LOGICAL FUNCTION db_goto_start_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_goto_start_RS
	END INTERFACE db_goto_start

	INTERFACE db_goto_end
		LOGICAL FUNCTION db_goto_end_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_goto_end_RS
	END INTERFACE db_goto_end

	INTERFACE db_print_row
		SUBROUTINE db_print_row(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END SUBROUTINE db_print_row
	END INTERFACE db_print_row

	INTERFACE db_item_value

		LOGICAL FUNCTION db_item_int(RS,j,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		INTEGER, INTENT(OUT) :: i
		END FUNCTION db_item_int

		LOGICAL FUNCTION db_item_int_named(RS,field_name,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		INTEGER, INTENT(OUT) :: i
		END FUNCTION db_item_int_named

		LOGICAL FUNCTION db_item_int_1(RS,j,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		INTEGER(KIND=1), INTENT(OUT) :: i
		END FUNCTION db_item_int_1

		LOGICAL FUNCTION db_item_int_1_named(RS,field_name,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		INTEGER(KIND=1), INTENT(OUT) :: i
		END FUNCTION db_item_int_1_named

		LOGICAL FUNCTION db_item_int_2(RS,j,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		INTEGER(KIND=2), INTENT(OUT) :: i
		END FUNCTION db_item_int_2

		LOGICAL FUNCTION db_item_int_2_named(RS,field_name,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		INTEGER(KIND=2), INTENT(OUT) :: i
		END FUNCTION db_item_int_2_named

		LOGICAL FUNCTION db_item_int_8(RS,j,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		INTEGER(KIND=8), INTENT(OUT) :: i
		END FUNCTION db_item_int_8

		LOGICAL FUNCTION db_item_int_8_named(RS,field_name,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		INTEGER(KIND=8), INTENT(OUT) :: i
		END FUNCTION db_item_int_8_named

		LOGICAL FUNCTION db_item_int_16(RS,j,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		INTEGER(KIND=16), INTENT(OUT) :: i
		END FUNCTION db_item_int_16

		LOGICAL FUNCTION db_item_int_16_named(RS,field_name,i)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		INTEGER(KIND=16), INTENT(OUT) :: i
		END FUNCTION db_item_int_16_named

		LOGICAL FUNCTION db_item_real(RS,j,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		REAL, INTENT(OUT) :: r
		END FUNCTION db_item_real
		
		LOGICAL FUNCTION db_item_real_8(RS,j,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		REAL(KIND=8), INTENT(OUT) :: r
		END FUNCTION db_item_real_8
		
		LOGICAL FUNCTION db_item_real_16(RS,j,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		INTEGER, INTENT(IN) :: j
		REAL(KIND=16), INTENT(OUT) :: r
		END FUNCTION db_item_real_16
		
		LOGICAL FUNCTION db_item_real_named(RS,field_name,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		REAL, INTENT(OUT) :: r
		END FUNCTION db_item_real_named
		
		LOGICAL FUNCTION db_item_real_8_named(RS,field_name,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		REAL(KIND=8), INTENT(OUT) :: r
		END FUNCTION db_item_real_8_named
		
		LOGICAL FUNCTION db_item_real_16_named(RS,field_name,r)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(IN) :: field_name
		REAL(KIND=16), INTENT(OUT) :: r
		END FUNCTION db_item_real_16_named
			
		INTEGER FUNCTION db_item_alloc_s(RS,j,string)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
		END FUNCTION db_item_alloc_s
		
		INTEGER FUNCTION db_item_alloc_s_named(RS,field,string)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
		CHARACTER*(*), INTENT(IN) :: field
		END FUNCTION db_item_alloc_s_named
		
		INTEGER FUNCTION db_item_fixed_s(RS,j,string,k)
		USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(OUT) :: string
		INTEGER :: j,k
		END FUNCTION db_item_fixed_s
		
		INTEGER FUNCTION db_item_fixed_named_s(RS,field,string,k)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		CHARACTER*(*), INTENT(OUT) :: string
		CHARACTER*(*), INTENT(IN) :: field
		INTEGER :: j,k
		END FUNCTION db_item_fixed_named_s
		
	END INTERFACE db_item_value

	INTERFACE db_list_databases
		SUBROUTINE db_list_dbs()
		USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
		USE MySQL_data, ONLY : send_signal, sig_list_databases
		USE MySQL_types, ONLY : db_recordset, disclose
		END SUBROUTINE db_list_dbs
	END INTERFACE db_list_databases

	INTERFACE db_stream

		LOGICAL FUNCTION db_stream_screen(RS)
		USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
		USE MySQL_data, ONLY : send_signal, sig_stream
		USE MySQL_types, ONLY : db_recordset, disclose
		IMPLICIT NONE
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_stream_screen

		LOGICAL FUNCTION db_stream_file(RS, file_name)
		USE MySQL_types, ONLY : db_recordset, disclose
		CHARACTER*(*), INTENT(IN) :: file_name
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_stream_file

	END INTERFACE db_stream
	
	INTERFACE db_next_source
		LOGICAL FUNCTION db_next_source_RS(RS)
		USE MySQL_types, ONLY : db_recordset, disclose
		TYPE(db_recordset), INTENT(IN) :: RS
		END FUNCTION db_next_source_RS
	END INTERFACE db_next_source
	END MODULE MySQL_interfaces
	
!***********************************************************************

LOGICAL FUNCTION do_login()
USE iso_c_binding, ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
USE MySQL_data, ONLY : sig_login, send_signal, max_login
IMPLICIT NONE

	do_login = .FALSE.
	CALL clear_send_signal(1)
	send_signal%signal = sig_login
	send_signal%data_external = .FALSE.
	send_signal%int_val = max_login
	IF (c_signal(C_CHAR_""//C_NULL_CHAR).EQ.0) THEN
		do_login = .TRUE.
	ENDIF

END FUNCTION do_login

SUBROUTINE clear_send()
USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, C_FLOAT &
	,C_DOUBLE, C_LONG_DOUBLE, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal

	send_signal%signal = 0
	send_signal%object = 0
	send_signal%flag = 0
	send_signal%data_external = .FALSE.
	send_signal%int_val = 0
	send_signal%int_val_128 = 0
	send_signal%int_val_32 = 0
	send_signal%int_val_64 = 0
	send_signal%float_val = 0
	send_signal%double_val = 0
	send_signal%long_double_val = 0
	send_signal%short_buffer =  ""//C_NULL_CHAR
	send_signal%message = ""//C_NULL_CHAR
	send_signal%aux_message_1 = ""//C_NULL_CHAR
	send_signal%aux_message_1 = ""//C_NULL_CHAR
	send_signal%aux_message_2 = ""//C_NULL_CHAR

END SUBROUTINE clear_send

SUBROUTINE clear_receive()
USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, C_FLOAT &
	,C_DOUBLE, C_LONG_DOUBLE, C_NULL_CHAR
USE MySQL_data, ONLY : receive_signal

	receive_signal%signal = 0
	receive_signal%object = 0
	receive_signal%flag = 0
	receive_signal%data_external = .FALSE.
	receive_signal%int_val = 0
	receive_signal%int_val_128 = 0
	receive_signal%int_val_32 = 0
	receive_signal%int_val_64 = 0
	receive_signal%int_val_16 = 0
	receive_signal%float_val = 0
	receive_signal%double_val = 0
	receive_signal%long_double_val = 0
	receive_signal%short_buffer =  ""//C_NULL_CHAR
	receive_signal%message = ""//C_NULL_CHAR
	receive_signal%aux_message_1 = ""//C_NULL_CHAR
	receive_signal%aux_message_1 = ""//C_NULL_CHAR
	receive_signal%aux_message_2 = ""//C_NULL_CHAR

END SUBROUTINE clear_receive

SUBROUTINE quick_clear_send(i)
USE iso_c_binding,	ONLY : C_INT, C_INT128_T, C_INT16_T, C_FLOAT &
							,C_DOUBLE, C_LONG_DOUBLE, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal

	send_signal%signal = 0
	IF (i > 2) THEN
		send_signal%object = 0
		send_signal%flag = 0
		send_signal%int_val = 0
	ELSE IF (i == 2) THEN
		send_signal%object = 0
	ENDIF

END SUBROUTINE quick_clear_send

LOGICAL FUNCTION db_connect(host, user, password, db_name)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_interfaces, ONLY : c_signal
USE MySQL_data, ONLY : sig_connect, send_signal, fishy1
IMPLICIT NONE
CHARACTER*(*), INTENT(IN) :: host
CHARACTER*(*), INTENT(IN) :: user
CHARACTER*(*), INTENT(IN), OPTIONAL :: password
CHARACTER*(*), INTENT(IN), OPTIONAL :: db_name

	db_connect = .FALSE.
	CALL clear_send()
	send_signal%signal = sig_connect
	IF(PRESENT(password)) THEN
		send_signal%aux_message_1 = C_CHAR_""//password//C_NULL_CHAR
	ELSE
		send_signal%aux_message_1 = C_NULL_CHAR
	ENDIF
	IF(PRESENT(db_name)) THEN
		send_signal%aux_message_2 = C_CHAR_""//db_name//C_NULL_CHAR
	ELSE
		send_signal%aux_message_2 = C_NULL_CHAR
	ENDIF
	IF ((LEN(host) < 65) .AND. (LEN(user) < 65) &
			.AND. (LEN(password) < 65) .AND. (LEN(db_name) < 65)) &
			THEN
		send_signal%data_external = .FALSE.
		send_signal%message = C_CHAR_""//host//C_NULL_CHAR
		send_signal%aux_message = C_CHAR_""//user//C_NULL_CHAR
		IF (c_signal(C_CHAR_""//C_NULL_CHAR).EQ.0) THEN
			db_connect = .TRUE.
		ENDIF
	ELSE
		print *, fishy1
	END IF
	CALL clear_send()
	RETURN

END FUNCTION db_connect

SUBROUTINE db_disconnect()
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_interfaces, ONLY : c_signal
USE MySQL_data, ONLY : sig_connect, send_signal, sig_disconnect, &
						receive_signal
IMPLICIT NONE

	CALL clear_send()
	send_signal%signal = sig_disconnect
	IF (c_signal(C_CHAR_""//C_NULL_CHAR).NE.0) THEN
		receive_signal%int_vaL = 5
		CALL call_exit()
	END IF
	RETURN
	
END SUBROUTINE db_disconnect

SUBROUTINE call_exit()
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : receive_signal, no_session, &
		no_connection, memory_table_corrupted, cant_allocate_memory, &
		improper_disconnection
IMPLICIT NONE

	CALL clear_send
	SELECT CASE (receive_signal%int_vaL)
	CASE (0)
		STOP no_session
	CASE (1)
		STOP no_connection
	CASE (2)
		STOP memory_table_corrupted
	CASE (4)
		STOP cant_allocate_memory
	CASE (5)
		STOP improper_disconnection
	ENDSELECT
	RETURN
	
END SUBROUTINE call_exit

LOGICAL FUNCTION do_switch(db_name)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_interfaces, ONLY : c_signal
USE MySQL_data, ONLY : send_signal, fishy2, sig_switch_database
IMPLICIT NONE
CHARACTER*(*), INTENT(IN) :: db_name

	do_switch = .FALSE.
	CALL clear_send()
	send_signal%signal = sig_switch_database
	IF ((LEN(db_name) < 65)) THEN
		send_signal%data_external = .FALSE.
		send_signal%short_buffer = C_CHAR_""//db_name//C_NULL_CHAR
		IF (c_signal(C_CHAR_""//C_NULL_CHAR).EQ.0) THEN
			do_switch = .TRUE.
		ENDIF
	ELSE
		print *, fishy2
	END IF
	RETURN
	
END FUNCTION do_switch

LOGICAL FUNCTION db_execute(str_SQL)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : sig_execute_command, send_signal
USE MySQL_interfaces, ONLY : c_signal
IMPLICIT NONE
CHARACTER*(*), INTENT(IN) :: str_SQL
CHARACTER(LEN=:), ALLOCATABLE ::what

	db_execute=.FALSE.
	CALL clear_send()
	send_signal%signal = sig_execute_command
	IF (LEN(str_SQL) < 1000) THEN
		send_signal%data_external = .FALSE.
		send_signal%message = C_CHAR_""//str_SQL//C_NULL_CHAR
		IF (c_signal(C_CHAR_""//C_NULL_CHAR).EQ.0) THEN
			db_execute=.TRUE.
		END IF
	ELSE
		send_signal%data_external = .TRUE.
		ALLOCATE(what, SOURCE=C_CHAR_""//str_SQL//C_NULL_CHAR)
		IF(c_signal(C_CHAR_""//str_SQL//C_NULL_CHAR).EQ.0) THEN
			db_execute = .TRUE.
		END IF
	END IF
	IF (ALLOCATED(what)) DEALLOCATE(what)
	RETURN

END FUNCTION db_execute

LOGICAL FUNCTION retrieve_RS(RS, str_SQL)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
USE MySQL_types, ONLY : db_recordset, disclose, set_RS
USE MySQL_interfaces, ONLY : c_signal
IMPLICIT NONE
TYPE(db_recordset) :: RS
CHARACTER*(*), INTENT(IN) :: str_SQL
CHARACTER(LEN=:), ALLOCATABLE :: what
INTEGER :: i

	retrieve_RS = .FALSE.
	CALL clear_send()
	send_signal%signal = sig_retrieve_data
	send_signal%flag = 4 !enum in C
	IF (RS%in_memory.EQV..TRUE.) THEN
		send_signal%object=disclose(RS%obj)
	END IF
	IF (LEN(str_SQL) < 1000) THEN
		send_signal%data_external = .FALSE.
		send_signal%message = C_CHAR_""//str_SQL//C_NULL_CHAR
		i = c_signal(C_CHAR_""//C_NULL_CHAR)
	ELSE
		send_signal%data_external = .TRUE.
		ALLOCATE(what, SOURCE=str_SQL//C_NULL_CHAR)
		i = c_signal(C_CHAR_""//str_SQL//C_NULL_CHAR)
	END IF
	IF (i.EQ.-1) THEN
		RETURN
	END IF
	retrieve_RS = .TRUE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		CALL set_RS(RS, i, 4) !4 is enum in c
	ENDIF
	IF (ALLOCATED(what)) DEALLOCATE(what)
	RETURN

END FUNCTION retrieve_RS

LOGICAL FUNCTION db_next_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_next_record, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS

	db_next_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_next_record
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_next_RS = .TRUE.
	ENDIF
	RETURN

END FUNCTION db_next_RS

LOGICAL FUNCTION db_previous_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_previous_record, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS

	db_previous_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_previous_record
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_previous_RS = .TRUE.
	ENDIF
	RETURN

END FUNCTION db_previous_RS

SUBROUTINE db_print_row(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_print_row, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i

	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_print_row
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	i = c_signal(""//C_NULL_CHAR)
	RETURN

END SUBROUTINE db_print_row

LOGICAL FUNCTION db_first_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_go_first, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i

	db_first_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_go_first
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_first_RS  = .TRUE.
	ENDIF
	RETURN

END FUNCTION db_first_RS

LOGICAL FUNCTION db_last_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_go_last, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i

	db_last_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_go_last
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_last_RS  = .TRUE.
	ENDIF
	RETURN
	
END FUNCTION db_last_RS

LOGICAL FUNCTION db_goto_RS(RS,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_goto_n, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i

	db_goto_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_goto_n
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	send_signal%int_val = i
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_goto_RS  = .TRUE.
	ENDIF
	RETURN

END FUNCTION db_goto_RS

LOGICAL FUNCTION db_goto_start_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_goto_start, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS

	db_goto_start_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_goto_start
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_goto_start_RS  = .TRUE.
	ENDIF
	RETURN

END FUNCTION db_goto_start_RS

LOGICAL FUNCTION db_goto_end_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_goto_end, no_rs
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
USE MySQL_types, ONLY : db_recordset, disclose
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS

	db_goto_end_RS = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_goto_end
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_goto_end_RS  = .TRUE.
	ENDIF
	RETURN
	
END FUNCTION db_goto_end_RS

LOGICAL FUNCTION db_item_int(RS,j,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
						sig_item_return, ordinal, short_buffer, & 
						too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
INTEGER, INTENT(OUT) :: i

	db_item_int = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	receive_signal%short_buffer=""
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int=.TRUE.
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_int

LOGICAL FUNCTION db_item_int_named(RS,field_name,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
INTEGER, INTENT(OUT) :: i
INTEGER :: length

	db_item_int_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_named

LOGICAL FUNCTION db_item_int_1(RS,j,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
INTEGER(KIND=1), INTENT(OUT) :: i

	db_item_int_1 =.FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_1=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_1

LOGICAL FUNCTION db_item_int_1_named(RS,field_name,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
INTEGER(KIND=1), INTENT(OUT) :: i
INTEGER :: length

	db_item_int_1_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_1_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_1_named

LOGICAL FUNCTION db_item_int_2(RS,j,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
INTEGER(KIND=2), INTENT(OUT) :: i

	db_item_int_2 = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_2=.TRUE.
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_int_2

LOGICAL FUNCTION db_item_int_2_named(RS,field_name,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
INTEGER(KIND=2), INTENT(OUT) :: i
INTEGER :: length

	db_item_int_2_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_2_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_2_named

LOGICAL FUNCTION db_item_int_8(RS,j,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
INTEGER(KIND=8), INTENT(OUT) :: i

	db_item_int_8 = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_8=.TRUE.
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_int_8

LOGICAL FUNCTION db_item_int_8_named(RS,field_name,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
INTEGER(KIND=8), INTENT(OUT) :: i
INTEGER :: length

	db_item_int_8_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_8_named=.TRUE.
		END IF
	END IF
	RETURN
		
END FUNCTION db_item_int_8_named

LOGICAL FUNCTION db_item_int_16(RS,j,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
INTEGER(KIND=16), INTENT(OUT) :: i

	db_item_int_16 = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF(c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_16=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_16

LOGICAL FUNCTION db_item_int_16_named(RS,field_name,i)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_int
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
INTEGER(KIND=16), INTENT(OUT) :: i
INTEGER :: length

	db_item_int_16_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	CALL clear_send_signal(3)
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_int
	IF(c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF((receive_signal%int_val_128.GT.HUGE(i)).OR. &
				(receive_signal%int_val_128.LT.(HUGE(i)*(-1)-1))) THEN
			PRINT*, too_small, receive_signal%int_val_128
		ELSE
			i=receive_signal%int_val_128
			db_item_int_16_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_int_16_named

LOGICAL FUNCTION db_item_real(RS,j,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
REAL, INTENT(OUT) :: r

	db_item_real = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_real

LOGICAL FUNCTION db_item_real_8(RS,j,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
REAL(KIND=8), INTENT(OUT) :: r

	db_item_real_8=.FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real_8=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_real_8

LOGICAL FUNCTION db_item_real_16(RS,j,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER, INTENT(IN) :: j
REAL(KIND=16), INTENT(OUT) :: r

	db_item_real_16 = .FALSE.
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%int_val = j
	send_signal%flag = 4
	send_signal%int_val_16 = ordinal
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real_16=.TRUE.
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_real_16

LOGICAL FUNCTION db_item_real_named(RS,field_name,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
REAL, INTENT(OUT) :: r
INTEGER :: length

	db_item_real_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_real_named

LOGICAL FUNCTION db_item_real_8_named(RS,field_name,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
REAL(KIND=8), INTENT(OUT) :: r
INTEGER :: length

	db_item_real_8_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real_8_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_real_8_named

LOGICAL FUNCTION db_item_real_16_named(RS,field_name,r)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_numeric, is_float
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(IN) :: field_name
REAL(KIND=16), INTENT(OUT) :: r
INTEGER :: length

	db_item_real_16_named = .FALSE.
	length = LEN(field_name)
	IF (length >64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory .NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	CALL clear_receive()
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field_name//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	CALL clear_receive
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_float
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF (receive_signal%int_val.LT.65) THEN
			r=receive_signal%long_double_val
			db_item_real_16_named=.TRUE.
		END IF
	END IF
	RETURN

END FUNCTION db_item_real_16_named

INTEGER FUNCTION db_item_alloc_s(RS,j,string)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: string
INTEGER :: j

	db_item_alloc_s = 0
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	IF(ALLOCATED(string)) DEALLOCATE(string)
	CALL clear_send_signal
	CALL clear_receive
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//C_NULL_CHAR
	send_signal%flag = 4
	send_signal%int_val = j
	send_signal%int_val_16 = ordinal
	receive_signal%object = send_signal%object
	receive_signal%flag = is_text
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF(receive_signal%int_val.GE.64)THEN
			string = TRIM(receive_signal%message)
			db_item_alloc_s = receive_signal%int_val
		ELSE
			string = TRIM(receive_signal%short_buffer)
			db_item_alloc_s = receive_signal%int_val
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_alloc_s

INTEGER FUNCTION db_item_fixed_s(RS,j,string,k)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(OUT) :: string
INTEGER :: j,k

	db_item_fixed_s = 0
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal
	CALL clear_receive
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//C_NULL_CHAR
	send_signal%flag = 4
	send_signal%int_val = j
	send_signal%int_val_16 = ordinal
	receive_signal%object = send_signal%object
	receive_signal%flag = is_text
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF(receive_signal%int_val.GE.64)THEN
			string = TRIM(receive_signal%message)
			db_item_fixed_s = receive_signal%int_val
		ELSE
			string = TRIM(receive_signal%short_buffer)
			db_item_fixed_s = receive_signal%int_val
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_fixed_s

INTEGER FUNCTION db_item_fixed_named_s(RS,field,string,k)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*), INTENT(OUT) :: string
CHARACTER*(*), INTENT(IN) :: field
INTEGER :: j,k

	db_item_fixed_named_s = 0
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal
	CALL clear_receive
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	receive_signal%object = send_signal%object
	receive_signal%flag = is_text
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF(receive_signal%int_val.GE.64)THEN
			string = TRIM(receive_signal%message)
			db_item_fixed_named_s = receive_signal%int_val
		ELSE
			string = TRIM(receive_signal%short_buffer)
			db_item_fixed_named_s = receive_signal%int_val
		END IF
	END IF
	RETURN
	
END FUNCTION db_item_fixed_named_s

INTEGER FUNCTION db_item_alloc_s_named(RS,field,string)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
CHARACTER*(*), INTENT(IN) :: field
INTEGER :: length

	db_item_alloc_s_named = 0
	length = LEN(field)
	IF (length>64) THEN
		PRINT*, fishy2
		RETURN
	END IF
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	END IF
	IF(ALLOCATED(string)) DEALLOCATE(string)
	CALL clear_send_signal
	CALL clear_receive
	send_signal%signal = sig_item_return
	send_signal%object = disclose(RS%obj)
	send_signal%short_buffer = C_CHAR_""//field//C_NULL_CHAR
	send_signal%int_val_16 = short_buffer
	send_signal%flag = 4
	receive_signal%object = send_signal%object
	receive_signal%int_val = 0
	receive_signal%flag = is_text
	IF (c_signal(""//C_NULL_CHAR).EQ.0)THEN
		IF(receive_signal%int_val.GE.64)THEN
			string = TRIM(receive_signal%message)
			db_item_alloc_s_named = receive_signal%int_val
		ELSE
			string = TRIM(receive_signal%short_buffer)
			db_item_alloc_s_named = receive_signal%int_val
		END IF
	END IF
	
END FUNCTION db_item_alloc_s_named

SUBROUTINE db_list_dbs()
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_list_databases
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
INTEGER :: i

	CALL clear_send_signal(3)
	send_signal%signal = sig_list_databases
	i = c_signal(""//C_NULL_CHAR)
	RETURN

END SUBROUTINE db_list_dbs

LOGICAL FUNCTION db_stream_screen(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_print_row, no_rs, &
						sig_stream, to_screen, to_file
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i

	db_stream_screen=.FALSE.
	IF (RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_stream
	send_signal%object = disclose(RS%obj)
	send_signal%flag = to_screen
	i = c_signal(""//C_NULL_CHAR)
	IF(i.EQ.0) THEN
		db_stream_screen=.TRUE.
	ENDIF
	RETURN

END FUNCTION db_stream_screen

LOGICAL FUNCTION db_stream_file(RS, file_name)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_print_row, no_rs, &
						sig_stream, to_screen, to_file, to_CSV, to_TSV &
						, empty_filename
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
CHARACTER*(*), INTENT(IN) :: file_name
TYPE(db_recordset), INTENT(IN) :: RS
INTEGER :: i
CHARACTER(LEN=4) :: x

		db_stream_file=.FALSE.
		IF (RS%in_memory.NEQV..TRUE.) THEN
			PRINT*, no_rs
			RETURN
		ENDIF
		CALL clear_send_signal()
		send_signal%signal = sig_stream
		send_signal%object = disclose(RS%obj)
		IF(file_name=="") THEN
			PRINT*,empty_filename
			RETURN
		END IF
		i=LEN_TRIM(file_name)
		send_signal%flag = to_file
		IF(i.GT.4) THEN
			x=(file_name(i-3:i))
			IF((x==".csv").OR.(x==".CSV")) THEN
				send_signal%flag = to_CSV
			ELSE IF((x==".tsv").OR.(x==".TSV")) THEN
				send_signal%flag = to_TSV
			END IF
		END IF
		send_signal%message = C_CHAR_""//file_name//C_NULL_CHAR
		i = c_signal(""//C_NULL_CHAR)
		IF(i.EQ.0) THEN
			db_stream_file=.TRUE.
		ENDIF
		RETURN

END FUNCTION db_stream_file

LOGICAL FUNCTION db_next_source_RS(RS)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, sig_next_source, no_rs
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS

	db_next_source_RS=.FALSE.
	IF(RS%in_memory.NEQV..TRUE.) THEN
		PRINT*, no_rs
		RETURN
	ENDIF
	CALL clear_send_signal(3)
	send_signal%signal = sig_next_source
	send_signal%object = disclose(RS%obj)
	send_signal%flag = 4
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		db_next_source_RS=.TRUE.
	ENDIF
	RETURN
	
END FUNCTION db_next_source_RS
