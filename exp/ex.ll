; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind

define fastcc i32 @f( i32 %x ) {
 %r = add i32 %x, %x
 ret i32 %r
}

; Definition of main function
define i32 @main( i32 %argc, i8** %argv ) { 
 ; Convert [13 x i8]* to i8  *...
 %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

 ; Call puts function to write out the string to stdout.
 %e = call i32 @puts(i8* %cast210)
 ; Can't musttail because args of f and main don't match
 %r = tail call fastcc i32 (i32)* @f ( i32 0 )
 ret i32 %r
}
