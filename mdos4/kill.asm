; June 2, 2023
; Requires Microsoft MASM 4.0

.model tiny
.data
    msg01 db "KILL 1.0 ©2023 starfrost - KILL [pid]", 0Dh, 0Ah
    msg02 db "KILL",
.code
main proc 
    mov bx, @msg01
    jmp print_string

    ; figure out how sig8 works
    ; and then do it
    MOVE_PID_HERE_FROM_CMDLIN
    mov ah, KILL_FUNCTION_HERE
    int 21h
main endp 
end main

print_string:  
    ; BX -> Pointer to string
    ; String must end with 0x0Ah, or it reads out random data 

    mov dx, [bx]
    mov ah, 9 
    int 21h
    ret
