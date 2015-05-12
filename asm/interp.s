.data
_banner:
  .asciz "Calculator (by Jay McCarthy)\n"
_prompt:
  .asciz "$ "

.text
.globl _main
_main:
  subq $8, %rsp

  movq $1, %rdi
  leaq _banner(%rip), %rsi
  call _fputs

_read:
  movq $1, %rdi
  leaq _prompt(%rip), %rsi
  call _fputs

  movq $0, %rdi
  call _fgetc
    
  movq %rax, %rdi
  movq $1, %rsi  
  call _fputc  

  movq $0, %rdi
  call _exit
