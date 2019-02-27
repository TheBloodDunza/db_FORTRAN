

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
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal, copy_text
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: string
INTEGER :: j,k

	db_item_alloc_s = 0
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
		IF(receive_signal%int_val.LT.64)THEN
			IF(ALLOCATED(string)) DEALLOCATE(string)
			string = TRIM(receive_signal%short_buffer)
			db_item_alloc_s = receive_signal%int_val
		ELSE
			IF(ALLOCATED(string)) DEALLOCATE(string)
			ALLOCATE(CHARACTER(len=receive_signal%int_val) :: string)
            k=copy_text(string)
            db_item_alloc_s = receive_signal%int_val
		END IF
	END IF
	RETURN

END FUNCTION db_item_alloc_s

INTEGER FUNCTION db_item_alloc_s_named(RS,field,string)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal, copy_text
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
CHARACTER*(*), INTENT(IN) :: field
INTEGER :: j,k, length

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
		IF(receive_signal%int_val.LT.64)THEN
			IF(ALLOCATED(string)) DEALLOCATE(string)
			string = TRIM(receive_signal%short_buffer)
			db_item_alloc_s_named = receive_signal%int_val
		ELSE
			IF(ALLOCATED(string)) DEALLOCATE(string)
			ALLOCATE(CHARACTER(len=receive_signal%int_val) :: string)
            k=copy_text(string)
            db_item_alloc_s_named = receive_signal%int_val
		END IF
	END IF

END FUNCTION db_item_alloc_s_named

INTEGER FUNCTION db_item_fixed_s(RS,j,string,k)
USE iso_c_binding,	ONLY : C_CHAR, C_INT, C_NULL_CHAR
USE MySQL_data, ONLY : send_signal, receive_signal, no_rs, &
					sig_item_return, fishy2, ordinal, short_buffer, &
					too_small, is_text
USE MySQL_types, ONLY : db_recordset, disclose
USE MySQL_interfaces, ONLY : c_signal, clear_send_signal, copy_text
IMPLICIT NONE
TYPE(db_recordset), INTENT(IN) :: RS
CHARACTER*(*) :: string
INTEGER :: j,k, length

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
	length = LEN(string)
	IF(length<k) THEN
	    send_signal%int_val_32 = length
	ELSE
	    send_signal%int_val_32 = k
	ENDIF
	receive_signal%object = send_signal%object
	receive_signal%flag = is_text
	IF (c_signal(""//C_NULL_CHAR).EQ.0) THEN
		IF(receive_signal%int_val.GE.64)THEN
			db_item_fixed_s = receive_signal%int_val
			k=copy_text(string)
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
INTEGER :: j,k, length

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
	length = LEN(string)
	IF(length<k) THEN
	    send_signal%int_val_32 = length
	ELSE
	    send_signal%int_val_32 = k
	ENDIF
