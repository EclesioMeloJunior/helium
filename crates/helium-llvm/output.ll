; ModuleID = 'main_helium'
source_filename = "main_helium"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 2, ptr %a, align 4
  %b = alloca i32, align 4
  store i32 2, ptr %b, align 4
  %a1 = load i32, ptr %a, align 4
  %b2 = load i32, ptr %b, align 4
  %sum = add i32 %a1, %b2
  %c = alloca i32, align 4
  store i32 %sum, ptr %c, align 4
  %c3 = load i32, ptr %c, align 4
  ret i32 %c3
}
