open Assert
open Rux86
open Rux86interpreter

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let st_test (s:string) (code:insn_block list) (f:x86_state -> bool) () =
       let st = mk_init_state () in
          let _ = interpret code st (mk_lbl_named "main") in
               if (f st) then () else failwith ("expected " ^ s)

let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part II", [
  ]);
 
  Test ("Studet-Provided test for Add instr", [
    ("add", st_test "eax=ebx=*1023=1"
            [(mk_block "main" [
                Add (Imm 1l, eax);
	            Add (eax, ebx);
                Add (Imm 2147483647l, ecx);
                Add (ecx, edx);
                Add (Imm 1l, edx);
                Add (ecx, esi);
                Add (Imm 2147483646l, esi);
                Add (esi, edi);
                Add (Imm 2147483646l, edi);
            ])]
            (fun state ->
                state.s_regs.(0) = 1l &&
                state.s_regs.(1) = 1l &&
                state.s_regs.(2) = 2147483647l &&
                state.s_regs.(3) = -2147483648l &&
                state.s_regs.(4) = -3l) 
    );
  ]);
] 
