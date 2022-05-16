# MIPSFormat

A basic, opinionated formatter for MIPS assembly.

The program takes in the MIPS code through STDIN and outputs the formatted code through STDOUT.

## Sample Usage

This is the unformatted code:
```asm
.data
number: .word 0
result: .word 0
.text
main:
# first, initialize the stack pointer to the highest word in memory
li $sp 0x7ffffffc

# read an integer from input
li $v0 5 # set syscall number to read_int
syscall
move $t0 $v0 # move the read integer into $t0

# store integer into variable number
sw $t0 number

# catalan's stack frame will contain 2 words: the RETURN ADDRESS and the frame pointer
# catalan takes in an integer parameter, which is another word
sub $sp $sp 4 # subtract 3 words; 1 for argument, one for $ra and one for $fp

sw $t0 12($sp) # put the input in args

jal catalan

add $sp $sp 4 # pop argument off the stack

# print the number
li $v0 1 # print_int number
lw $a0 result # load the number from result
syscall

# print a newline
li $v0 11 # print_char
li $a0 10 # newline character
syscall

# exit program
li $v0 10 # exit
syscall
catalan:
lw $t0 4($fp) # get the number from args
li $t1 1 # set $t1 to 0; counter for n
li $t2 1 # catalan(n) value

beqz $t0 catalan_end # if n is 0, then don't need to do anything
loop:
# $t3 = 2n - 1
mul $t3 $t1 2
sub $t3 $t3 1

mul $t2 $t2 2 # multiply by 2

mul $t2 $t2 $t3 # multiply by 2n - 1

add $t3 $t1 1 # n + 1

div $t2 $t2 $t3 # numerator / denominator

add $t1 $t1 1 # increment counter
ble $t1 $t0 loop # loop back if counter is less than number
catalan_end:
# store return value
sw $t2 result

# epilogue
lw $fp 8($sp) # reset $fp
add $sp $sp 8 # reset $sp
jr $ra # jump back
error:
# print -1
li $v0 1 # set syscall number to print_int
li $a0 -1 # tell it to print -1
syscall
```

This is the formatted code:
```asm
.data
    number: .word   0
    result: .word   0
.text
main:
    # first, initialize the stack pointer to the highest word in memory
    li      $sp     0x7ffffffc

    # read an integer from input
    li      $v0     5               # set syscall number to read_int
    syscall
    move    $t0     $v0             # move the read integer into $t0

    # store integer into variable number
    sw      $t0     number

    # catalan's stack frame will contain 2 words: the RETURN ADDRESS and the frame pointer
    # catalan takes in an integer parameter, which is another word
    sub     $sp     $sp     4       # subtract 3 words; 1 for argument, one fo
                                    # r $ra and one for $fp

    sw      $t0     12($sp)         # put the input in args

    jal     catalan

    add     $sp     $sp     4       # pop argument off the stack

    # print the number
    li      $v0     1               # print_int number
    lw      $a0     result          # load the number from result
    syscall

    # print a newline
    li      $v0     11              # print_char
    li      $a0     10              # newline character
    syscall

    # exit program
    li      $v0     10              # exit
    syscall
catalan:
    lw      $t0     4($fp)          # get the number from args
    li      $t1     1               # set $t1 to 0; counter for n
    li      $t2     1               # catalan(n) value

    beqz    $t0     catalan_end     # if n is 0, then don't need to do anythin
                                    # g
loop:
    # $t3 = 2n - 1
    mul     $t3     $t1     2
    sub     $t3     $t3     1

    mul     $t2     $t2     2       # multiply by 2

    mul     $t2     $t2     $t3     # multiply by 2n - 1

    add     $t3     $t1     1       # n + 1

    div     $t2     $t2     $t3     # numerator / denominator

    add     $t1     $t1     1       # increment counter
    ble     $t1     $t0     loop    # loop back if counter is less than number
catalan_end:
    # store return value
    sw      $t2     result

    # epilogue
    lw      $fp     8($sp)          # reset $fp
    add     $sp     $sp     8       # reset $sp
    jr      $ra                     # jump back
error:
    # print -1
    li      $v0     1               # set syscall number to print_int
    li      $a0     -1              # tell it to print -1
    syscall
```

Notice that directives and labels are flush to the left, instructions and comments are intended by 4 characters, instruction arguments are aligned, and comments are both aligned and wrapped to 80 characters.
