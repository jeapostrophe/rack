;; -*- prover-cmd: ". ~/.profile; racket mctop.rkt"; eval: (prover-proof-mode 1) -*-

(compile
 ("@.str = private unnamed_addr constant [13 x i8] c\"hello world\\0A\\00\""

  "declare i32 @puts(i8* nocapture) nounwind"

  "define i32 @main() {
    %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0
    call i32 @puts(i8* %cast210)
    ret i32 0
   }"

  "define i32 @square_unsigned(i32 %a) {
    %1 = mul i32 %a, %a
    ret i32 %1
   }"

  "define <4 x i32> @multiply_four(<4 x i32> %a, <4 x i32> %b) {
    %1 = mul <4 x i32> %a,  %b
    ret <4 x i32> %1
   }"))
