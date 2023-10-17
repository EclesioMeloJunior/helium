; ModuleID = 'main_helium'
source_filename = "main_helium"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"

define i32 @main() {
entry:
  %c = alloca float, align 4
  store float 0x40019999A0000000, ptr %c, align 4
  %c1 = load float, ptr %c, align 4
  %sum = fadd float %c1, 0x3FF6666660000000
  %b = alloca float, align 4
  store float %sum, ptr %b, align 4
  ret i32 0
}
