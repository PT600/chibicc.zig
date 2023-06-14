  .data
  .globl .L..5
.L..5:
  .byte 79
  .byte 75
  .byte 10
  .byte 0
  .data
  .globl .L..4
.L..4:
  .byte 115
  .byte 105
  .byte 122
  .byte 101
  .byte 111
  .byte 102
  .byte 40
  .byte 34
  .byte 34
  .byte 41
  .byte 0
  .data
  .globl .L..3
.L..3:
  .byte 0
  .data
  .globl .L..2
.L..2:
  .byte 34
  .byte 34
  .byte 91
  .byte 48
  .byte 93
  .byte 0
  .data
  .globl .L..1
.L..1:
  .byte 0
  .globl main
  .text
main:
  push %rbp
  mov %rsp, %rbp
  sub $0, %rsp
  mov $0, %rax
  push %rax
  mov $0, %rax
  push %rax
  mov $1, %rax
  pop %rdi
  imul %rdi, %rax
  push %rax
  lea .L..1(%rip), %rax
  pop %rdi
  add %rdi, %rax
  movsbq (%rax), %rax
  push %rax
  lea .L..2(%rip), %rax
  push %rax
  pop %rdx
  pop %rsi
  pop %rdi
  mov $0, %rax
  call assert
  mov $1, %rax
  push %rax
  mov $1, %rax
  push %rax
  lea .L..4(%rip), %rax
  push %rax
  pop %rdx
  pop %rsi
  pop %rdi
  mov $0, %rax
  call assert
  lea .L..5(%rip), %rax
  push %rax
  pop %rdi
  mov $0, %rax
  call printf
  mov $0, %rax
  jmp .L.return.main
.L.return.main:
  mov %rbp, %rsp
  pop %rbp
  ret
