! test.f95

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
! * File: test.f95
! * Version: 1.00
! * Author: Tom Fraser
! * Description: MySQL interface for FORTRAN - FORTRAN wrapper
! * Date created: 22-NOV-2018
! * Date last modified: 01-JAN-2019
! * GNU General Public License <http://www.gnu.org/licenses/>.
! * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! gcc -c -std=c11 utilities.c
! gfortran -c mysql1.f95
! gcc -std=c11 test12.c utilities.o -c `mysql_config --cflags --libs`
! gfortran test.f95 db_FORTRAN.o db_FORTRANLib.o utilities.o -o tom `mysql_config --cflags --libs`

PROGRAM sql
!USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
!USE mysql_interfaces, ONLY : db_login, db_connect,  &
!		db_execute
!USE MySQL_data, ONLY : send_signal, sig_retrieve_data
!USE MySQL_interfaces, ONLY
!IMPLICIT NONE
!INTEGER i
!LOGICAL tf

	!CALL test1
	!CALL test2
	!CALL test4
	!CALL test5
	!CALL test6
	!CALL test7
	!CALL test8
	!CALL test9
	!CALL test10
	!CALL test11
	!CALL test12
	!CALL test13
	!CALL test14
	!CALL test15
	!CALL test16
	!CALL test17
	!CALL test18
	!CALL test19
	!CALL test20
	!CALL test21
	!CALL test22
	!CALL test23
	!CALL test24
	!CALL test25
	!CALL test26
	!CALL test27
	!CALL test28
	!CALL test29
	!CALL test30
	CALL test31
	CALL EXIT

END PROGRAM sql

SUBROUTINE test1()
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect, &
		db_execute
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
USE MySQL_interfaces, ONLY : db_connect, db_execute
IMPLICIT NONE
INTEGER i
LOGICAL tf


	tf = db_connect("localhost", "stream", "297Kaprekar703", &
					db_name="testdb")
	tf = db_execute("insert into Cars values(10,'Reliant Robin',69)")
	!i = db_execute("delete from Cars where price=69")
	tf = db_execute("plop pooh plop;")
	print *, tf

END SUBROUTINE test1

SUBROUTINE test2
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
		db_execute, db_retrieve_data, db_login
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER :: i
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
TYPE(db_recordset) :: your_RS

	!tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
	!		"TamFraser",  "297Kaprekar703","TamFraser$Kaprekar")
	tf = db_connect("localhost","f", "plop","test")
	tf = db_retrieve_data(my_RS, "SELECT * FROM Cars;")
	tf = db_retrieve_data(my_RS, "SELECT Id FROM Cars;")
	tf = db_retrieve_data(your_RS, "select Cars.Id, name, model from Cars, model where Cars.Id=model.Id order by model;")
	tf = db_retrieve_data(my_RS, "SELECT * FROM Cars;")
	tf = db_retrieve_data(your_RS, "SELECT * FROM Cars;")
	CALL test3(your_RS)

END SUBROUTINE test2

SUBROUTINE test3(RS)
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect, &
		db_execute, db_retrieve_data, db_login
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER :: i
LOGICAL :: tf
TYPE(db_recordset), INTENT(IN) :: RS

		tf = db_retrieve_data(RS, "SELECT Id, name FROM Cars;")

END SUBROUTINE

SUBROUTINE test4
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
		db_execute, db_retrieve_data
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS
TYPE(db_recordset) :: your_RS

	tf = db_login()
	i = db_execute("insert into Cars values(10,'Reliant Robin',69)")
	tf = db_retrieve_data(your_RS, "select Cars.Id, name, model from" &
			//" Cars, model where Cars.Id=model.Id order by model;")
	print *, tf

END SUBROUTINE test4

SUBROUTINE test5
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
		db_execute, db_retrieve_data, db_next_row, db_list_databases &
		,db_print_row
USE MySQL_data, ONLY : send_signal, sig_retrieve_data, sig_next_record
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 5 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	PRINT*,
	CALL db_list_databases
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Writers;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
PRINT*, tf

END SUBROUTINE test5

SUBROUTINE test6
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row
USE MySQL_data, ONLY : send_signal, sig_retrieve_data, sig_next_record,&
						sig_previous_record
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS

	tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703","TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_previous_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)

PRINT*, tf
	CALL db_print_row(my_RS)

END SUBROUTINE test6

SUBROUTINE test7
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, db_first_row, &
							db_last_row, db_goto_row
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS

print*, "TEST 7"
!   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!			"TamFraser",  "297Kaprekar703","TamFraser$test")
	tf = db_connect("localhost","f", "plop","test")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	DO WHILE (db_previous_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

	tf=db_first_row(my_RS)
	CALL db_print_row(my_RS)
	tf=db_previous_row(my_RS)
print*,tf
	CALL db_print_row(my_RS)
	tf=db_previous_row(my_RS)
print*,tf
	CALL db_print_row(my_RS)
print*,"here!"
	tf=db_last_row(my_RS)
	CALL db_print_row(my_RS)
print*,"last row",tf
	tf=db_next_row(my_RS)
	CALL db_print_row(my_RS)
print*,tf
	tf=db_next_row(my_RS)
	CALL db_print_row(my_RS)
print*,tf
print*,"here!"
	tf=db_goto_row(my_RS,5)
	CALL db_print_row(my_RS)
print*,"row 5"
	tf=db_goto_row(my_RS,50)
	CALL db_print_row(my_RS)
	tf=db_goto_row(my_RS,0)
	CALL db_print_row(my_RS)
	print*,"here! row 0"
	tf=db_next_row(my_RS)
	CALL db_print_row(my_RS)
	tf = db_previous_row(my_RS)
	CALL db_print_row(my_RS)

END SUBROUTINE test7

SUBROUTINE test8
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS

print*, "TEST 8"
!   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!			"TamFraser",  "297Kaprekar703","TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	PRINT*,"Back to start!"
	CALL db_print_row(my_RS)
	tf = db_goto_end(my_RS)
	PRINT*,"At the end!"
	DO WHILE (db_previous_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

END SUBROUTINE test8

SUBROUTINE test9
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh

print*, "TEST 9"
!   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!			"TamFraser",  "297Kaprekar703","TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")

	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	j = 1
	tf = db_item_value(my_RS,j,i)
	PRINT*,"this is an integer: ", i, i+68
	call db_print_row(my_RS)
	print*
	tf = db_last_row(my_RS)
	CALL db_print_row(my_RS)
	tf = db_item_value(my_RS,"Id",i)
	PRINT*,"this is an integer: ", i
	call db_print_row(my_RS)
!	tf = db_item_value(my_RS,"plopppy plooopy poopyplop poooooooh poopyplop poooooooh plop pooooooooooop",i)

END SUBROUTINE test9

SUBROUTINE test10()
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
USE MySQL_data, ONLY : send_signal, sig_retrieve_data
IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

	print*, "TEST 10"
	tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703", "TamFraser$test")
!	i = db_execute("insert into Cars values(10,'Reliant Robin',69)")
!	i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	CALL db_print_row(my_RS)
	tf = db_goto_start(my_RS)
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_first_row(my_RS)
	print*, "Another Row here:"
	CALL db_print_row(my_RS)
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

END SUBROUTINE test10

SUBROUTINE test11
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh

print*, "TEST 11"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703")
!	tf = db_connect("localhost","f", "plop","test")
   i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by Id;")
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	j = 1
	tf=db_last_row(my_RS)
	tf = db_item_value(my_RS,2,i)
print*,"tf",tf,"i", i
	tf = db_item_value(my_RS,1,i)
print*, tf, "integer i", i
	tf = db_item_value(my_RS,"Id",i)
print*, tf, "Id = ", i
	i = 69
	tf = db_item_value(my_RS,"Name",i)
print*, tf, "name = ", i

END SUBROUTINE test11

SUBROUTINE test12
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database
USE MySQL_types, ONLY : db_recordset

IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh

print*, "TEST 12"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703", "TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")
!   i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	 DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_switch_database("Kaprekar")


END SUBROUTINE test12

SUBROUTINE test13
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database
USE MySQL_types, ONLY : db_recordset

IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh
CHARACTER(LEN=*), PARAMETER :: SQL ="select * from Cars order by name;"

print*, "TEST 13"
	tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703", "TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")
	i = db_execute("use test;")
	i = db_execute("insert into Cars values(10,'Reliant Robin',69)")
!	tf = db_retrieve_data(my_RS, "select * from " &
!			//"Cars order by name;")
	IF (db_retrieve_data(my_RS,SQL).EQV..FALSE.) THEN
		PRINT*, "Bolloxed!"
	END IF
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_switch_database("TamFraser$Kaprekar")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Decimal_Kaprekars order by Kaprekar;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

END SUBROUTINE test13

SUBROUTINE test14
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database, db_disconnect
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
TYPE(db_recordset) :: your_RS
TYPE(db_recordset) :: big_RS
TYPE(db_recordset) :: another_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh


print*, "TEST 14"
	tf=.false.
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
!	i = db_execute("use test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
print*,"after retrieve", tf
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_switch_database("Kaprekar")
print*, "TEST 14 to here"
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Decimal_Kaprekars order by Kaprekar;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

	tf = db_switch_database("test")
	tf = db_retrieve_data(your_RS, "select * from " &
			//"Cars order by name;")
print*,"after retrieve", tf
	DO WHILE (db_next_row(your_RS).EQV..TRUE.)
		CALL db_print_row(your_RS)
	END DO
	tf = db_retrieve_data(big_RS, "select * from " &
			//"Cars order by name;")
	tf = db_switch_database("information_schema")
	tf = db_switch_database("test")
	tf = db_retrieve_data(another_RS, "select * from " &
			//"Cars order by name;")
	!CALL db_disconnect()

END SUBROUTINE test14


SUBROUTINE test15
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database
USE MySQL_types, ONLY : db_recordset

IMPLICIT NONE
INTEGER(KIND=4) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh


print*, "TEST 15"
	tf = db_login()
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by name;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_switch_database("Kaprekar")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Decimal_Kaprekars order by Kaprekar;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO

END SUBROUTINE test15

SUBROUTINE test16
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=1) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS
REAL(KIND=4) :: plop
REAL(KIND=8) :: ploppy
REAL(KIND=16) :: poooh

print*, "TEST 16"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703")
!	tf = db_connect("localhost","f", "plop","test")
   i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by Id;")
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	j = 1
	tf=db_last_row(my_RS)
	tf = db_item_value(my_RS,2,i)
print*,"tf",tf,"i", i
	tf = db_item_value(my_RS,1,i)
print*, tf, "field 1 integer = ", i
	tf = db_item_value(my_RS,"Id",i)
print*, tf, "Named Id = ", i
	i = 69
	tf = db_item_value(my_RS,"Name",i)
print*, tf, "no int name = ", i

END SUBROUTINE test16

SUBROUTINE test17
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=2) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "TEST 17 (KIND=2)"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703")
!	tf = db_connect("localhost","f", "plop","test")
   i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"Cars order by Id;")
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	j = 1
	tf=db_goto_row(my_RS,4)
	tf = db_item_value(my_RS,2,i)
print*,"tf",tf,"i", i
	tf = db_item_value(my_RS,1,i)
print*, tf, "field 1 integer = ", i
	tf = db_item_value(my_RS,"Id",i)
print*, tf, "Named Id = ", i
	i = 69
	tf = db_item_value(my_RS,"Name",i)
print*, tf, "no int name = ", i

END SUBROUTINE test17

SUBROUTINE test18
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "TEST 18 (KIND=8)"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703")
!	tf = db_connect("localhost","f", "plop","test")
   i = db_execute("use TamFraser$test;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"intz order by Id;")
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	tf=db_next_row(my_RS)
	j = 1
	tf=db_goto_row(my_RS,4)
	tf = db_item_value(my_RS,2,i)
print*,"tf",tf,"i", i
	tf = db_item_value(my_RS,1,i)
print*, tf, "field 1 integer = ", i
	tf = db_item_value(my_RS,"Id",i)
print*, tf, "Named Id = ", i
	i = 69
	tf = db_item_value(my_RS,"Name",i)
print*, tf, "no int name = ", i

END SUBROUTINE test18

SUBROUTINE test19
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 19 ++++++++++++++++++++++++"
!   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!			"TamFraser",  "297Kaprekar703")
!	tf = db_connect("localhost","f", "plop","test")
!   i = db_execute("use TamFraser$Kaprekar;")
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"intz order by Id;")
	PRINT*,"huge ", huge(i)
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		PRINT*,"******* NEW ROW *******"
		CALL db_print_row(my_RS)
		tf = db_item_value(my_RS,2,i)
		PRINT*,"int val= ", i
		PRINT*,
	END DO

END SUBROUTINE test19

!SUBROUTINE test20
!USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
!USE mysql_interfaces, ONLY : db_login, db_connect,  &
!							db_execute, db_retrieve_data, db_next_row, &
!							db_previous_row, db_print_row, &
!							db_first_row, db_last_row, db_goto_row, &
!							db_goto_start, db_goto_end, db_item_value, &
!							db_switch_database, db_disconnect
!USE MySQL_types, ONLY : db_recordset
!IMPLICIT NONE
!INTEGER :: j
!REAL(KIND=16) :: i
!LOGICAL :: tf
!TYPE(db_recordset) :: my_RS

!print*,
!print*, "++++++++++++++++++++++++ TEST 20 ++++++++++++++++++++++++"
!!  tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!!			"TamFraser",  "297Kaprekar703")
!!	  tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
!!			"TamFraser",  "297Kaprekar703","TamFraser$test")
!	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
!	tf = db_retrieve_data(my_RS, "select * from " &
!			//"reals;")
!	tf = db_first_row(my_RS)

!print*, "first row"
!    CALL db_print_row(my_RS)
!print*,
!	tf = db_item_value(my_RS,2,i)
!	print*,"i= ", i
!print*,
!print*,
!	print*, "last row"
!	tf=db_last_row(my_RS)
!	CALL db_print_row(my_RS)
!	tf = db_item_value(my_RS,2,i)
!print*,"last",i
!	tf = db_first_row(my_RS)
!	tf = db_item_value(my_RS,2,i)
!print*,"ROW 3++++++++++++++++++++++++++++++++++++++++++++++++++++ "
!	TF = db_goto_row(my_RS,3)
!	tf = db_item_value(my_RS,2,i)
!print*,"3rd",i
!    CALL db_disconnect()

!END SUBROUTINE test20

SUBROUTINE test21
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_connect,  &
							db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database, db_disconnect, &
							db_list_databases
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER :: j
REAL(KIND=16) :: i
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "++++++++++++++++++++++++ TEST 21 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	PRINT*,
	CALL db_list_databases
	CALL db_disconnect

END SUBROUTINE test21

SUBROUTINE test22
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_connect,  db_login, &
							db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value, &
							db_switch_database, db_disconnect, &
							db_list_databases, db_stream
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER :: j
REAL(KIND=16) :: i
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "++++++++++++++++++++++++ TEST 22 ++++++++++++++++++++++++"
  tf = db_connect("localhost", &
			"stream")!, db_name="TamFraser$test")
	!tf = db_login()
	tf = db_switch_database("testdb")
	PRINT*,
	CALL db_list_databases
	tf = db_retrieve_data(my_RS, "select * from Writers;")
	tf= db_stream(my_RS)
	tf=db_stream(my_RS,"./plop/writers.tsv")
	CALL db_disconnect

END SUBROUTINE test22

SUBROUTINE test23
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 23 ++++++++++++++++++++++++"
   tf = db_connect("TamFraser.mysql.pythonanywhere-services.com", &
			"TamFraser",  "297Kaprekar703"); !, "TamFraser$test")
!	tf = db_connect("localhost","f", "plop","test")

!	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
!	i = db_execute("START TRANSACTION;")
	i = db_execute("SET autocommit=0;")
	i = db_execute("insert into intz values" &
					//"(20,20)," &
					//"(21,21);")
	i = db_execute("COMMIT;")

	i = db_execute("insert into intz values" &
				//"(22,22);")
	i = db_execute("ROLLBACK;")
	tf = db_retrieve_data(my_RS, "select * from " &
			//"intz order by Id; select * from reals;")
	PRINT*,"huge ", huge(i)
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		PRINT*,"******* NEW ROW *******"
		CALL db_print_row(my_RS)
		tf = db_item_value(my_RS,2,i)
		PRINT*,"int val= ", i
		PRINT*,
	END DO
	CALL db_disconnect

END SUBROUTINE test23

SUBROUTINE test24
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
LOGICAL :: tf
CHARACTER(LEN=:), ALLOCATABLE :: s
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 24 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	tf = db_retrieve_data(my_RS, "select * from book;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		i=db_item_value(my_RS,"title",s)
		PRINT*,s
	END DO
	tf = db_retrieve_data(my_RS, "select * from darkside;")
	tf = db_first_row(my_RS)
	i=db_item_value(my_RS,"ds",s)
	PRINT*,"s=",s
	IF(ALLOCATED(s)) DEALLOCATE(s)
	CALL db_disconnect

END SUBROUTINE test24

SUBROUTINE test25
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, db_stream, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
LOGICAL :: tf
CHARACTER(LEN=:), ALLOCATABLE :: s
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 24 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	tf = db_retrieve_data(my_RS, "select * from book;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		i=db_item_value(my_RS,"title",s)
		PRINT*,s
	END DO
	tf = db_retrieve_data(my_RS, "show create table Writers;")
	tf = db_first_row(my_RS)
	i=db_item_value(my_RS,1,s)
	PRINT*,"s=",s
	tf= db_stream(my_RS)
	IF(ALLOCATED(s)) DEALLOCATE(s)
	CALL db_disconnect

END SUBROUTINE test25


SUBROUTINE test26
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER :: j
INTEGER(KIND=4) :: i
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

	PRINT*, "TEST 26 - Integers "
	tf = db_connect("localhost", &
			"stream",  "297Kaprekar703", "testdb")
	tf = db_retrieve_data(my_RS, "select * from intz;")
	 DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
		tf = db_item_value(my_RS,"i",i)
		print*,"returned",i
	END DO

END SUBROUTINE test26

SUBROUTINE test27
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
REAL:: r
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "TEST 26 - floats "
   tf = db_connect("localhost", &
			"stream", "297Kaprekar703", db_name="testdb")
	tf = db_retrieve_data(my_RS, "select * from reals;")
	 DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf = db_goto_start(my_RS)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_next_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	tf=db_goto_end(my_RS)
	tf = db_last_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	print*,"r=",r
	tf = db_previous_row(my_RS)
	tf = db_item_value(my_RS,2,r)
	print*,"r=",r

END SUBROUTINE test27

SUBROUTINE test28
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
REAL(KIND=8):: r
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "TEST 28 - 8 bytes floats "
   tf = db_connect("localhost", "stream", "297Kaprekar703", &
					db_name="testdb")
	tf = db_retrieve_data(my_RS, "select * from reals;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
		tf = db_item_value(my_RS,2,r)
		print*,"r=",r
	END DO
	
END SUBROUTINE test28

SUBROUTINE test29
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
REAL(KIND=16):: r
LOGICAL :: tf
TYPE(db_recordset) :: my_RS

print*, "TEST 29 - 16 bytes floats "
   tf = db_connect("localhost", "stream", "297Kaprekar703", &
					db_name="testdb")
	tf = db_retrieve_data(my_RS, "select id,val1 as plop from reals;" & 
									//"select * from book;")
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf=db_next_source(my_RS)
	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		CALL db_print_row(my_RS)
	END DO
	tf=db_next_source(my_RS)
	CALL db_disconnect
	
END SUBROUTINE test29

SUBROUTINE test30
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, db_stream, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
INTEGER :: tf
CHARACTER(LEN=:), ALLOCATABLE :: s
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 30 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	tf = db_retrieve_data(my_RS, "select * from book;")

	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		i=db_item_value(my_RS,"title",s)
		PRINT*,s
	END DO
	CALL db_disconnect

END SUBROUTINE test30

SUBROUTINE test31
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces, ONLY : db_login, db_connect,  &
							db_execute, db_retrieve_data, db_next_row, &
							db_previous_row, db_print_row, db_stream, &
							db_first_row, db_last_row, db_goto_row, &
							db_goto_start, db_goto_end, db_item_value
USE MySQL_types, ONLY : db_recordset
IMPLICIT NONE
INTEGER(KIND=8) :: i, j
INTEGER :: tf
CHARACTER(140) :: s
TYPE(db_recordset) :: my_RS

PRINT*, "++++++++++++++++++++++++ TEST 31 ++++++++++++++++++++++++"
	tf = db_connect("localhost","stream", "297Kaprekar703","testdb")
	tf = db_retrieve_data(my_RS, "select * from darkside;")

	DO WHILE (db_next_row(my_RS).EQV..TRUE.)
		i=db_item_value(my_RS,2,s,76)
		PRINT*,"fort string: ",s
	END DO
	CALL db_disconnect

END SUBROUTINE test31
