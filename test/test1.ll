; ModuleID = 'a.bc'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
target triple = "i386-pc-linux-gnu"

define i32 @foo(i32 %y) nounwind {
entry:
  %"y_addr" = alloca i32, align 4
  %"retval" = alloca i32
  %0 = alloca i32
  %x = alloca i32
  %z = alloca i32
  %"alloca point" = bitcast i32 0 to i32
  call void @llvm.dbg.declare(metadata !{i32* %y_addr}, metadata !6), !dbg !7
  store i32 %y, i32* %y_addr
  call void @llvm.dbg.declare(metadata !{i32* %x}, metadata !8), !dbg !10
  call void @llvm.dbg.declare(metadata !{i32* %z}, metadata !11), !dbg !10
  %1 = load i32* %y_addr, align 4, !dbg !12
  store i32 %1, i32* %x, align 4, !dbg !12
  %2 = load i32* %x, align 4, !dbg !13
  store i32 %2, i32* %z, align 4, !dbg !13
  %3 = load i32* %z, align 4, !dbg !14
  store i32 %3, i32* %0, align 4, !dbg !14
  %4 = load i32* %0, align 4, !dbg !14
  store i32 %4, i32* %retval, align 4, !dbg !14
  br label %return, !dbg !14

return:                                           ; preds = %entry
  %retval1 = load i32* %retval, !dbg !14
  ret i32 %retval1, !dbg !14
}

declare void @llvm.dbg.declare(metadata, metadata) nounwind readnone

!llvm.dbg.sp = !{!0}

!0 = metadata !{i32 524334, i32 0, metadata !1, metadata !"foo", metadata !"foo", metadata !"foo", metadata !1, i32 2, metadata !3, i1 false, i1 true, i32 0, i32 0, null, i1 false, i1 false, i32 (i32)* @foo} ; [ DW_TAG_subprogram ]
!1 = metadata !{i32 524329, metadata !"a.c", metadata !"/home/wangn/idynsafe.org/trunk/compiler/rt-meta/src/main/Llvm/test/", metadata !2} ; [ DW_TAG_file_type ]
!2 = metadata !{i32 524305, i32 0, i32 1, metadata !"a.c", metadata !"/home/wangn/idynsafe.org/trunk/compiler/rt-meta/src/main/Llvm/test/", metadata !"4.2.1 (Based on Apple Inc. build 5658) (LLVM build)", i1 true, i1 false, metadata !"", i32 0} ; [ DW_TAG_compile_unit ]
!3 = metadata !{i32 524309, metadata !1, metadata !"", metadata !1, i32 0, i64 0, i64 0, i64 0, i32 0, null, metadata !4, i32 0, null} ; [ DW_TAG_subroutine_type ]
!4 = metadata !{metadata !5, metadata !5}
!5 = metadata !{i32 524324, metadata !1, metadata !"int", metadata !1, i32 0, i64 32, i64 32, i64 0, i32 0, i32 5} ; [ DW_TAG_base_type ]
!6 = metadata !{i32 524545, metadata !0, metadata !"y", metadata !1, i32 1, metadata !5} ; [ DW_TAG_arg_variable ]
!7 = metadata !{i32 1, i32 0, metadata !0, null}
!8 = metadata !{i32 524544, metadata !9, metadata !"x", metadata !1, i32 3, metadata !5} ; [ DW_TAG_auto_variable ]
!9 = metadata !{i32 524299, metadata !0, i32 2, i32 0, metadata !1, i32 0} ; [ DW_TAG_lexical_block ]
!10 = metadata !{i32 3, i32 0, metadata !9, null}
!11 = metadata !{i32 524544, metadata !9, metadata !"z", metadata !1, i32 3, metadata !5} ; [ DW_TAG_auto_variable ]
!12 = metadata !{i32 4, i32 0, metadata !9, null}
!13 = metadata !{i32 5, i32 0, metadata !9, null}
!14 = metadata !{i32 6, i32 0, metadata !9, null}
