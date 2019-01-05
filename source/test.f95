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
	CALL test4
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
	!CALL test31
	CALL EXIT

END PROGRAM sql


SUBROUTINE test4
USE iso_c_binding, ONLY : C_CHAR, C_NULL_CHAR, C_INT
USE mysql_interfaces
!USE MySQL_data
USE MySQL_types
IMPLICIT NONE
INTEGER i
LOGICAL tf
TYPE(db_recordset) :: my_RS

	tf = db_connect("localhost","stream",db_name="testdb")
	tf = db_retrieve_data(my_RS, "select * from Writers;")
	tf = db_stream(my_RS)
	print *, tf
	CALL db_disconnect

END SUBROUTINE test4

