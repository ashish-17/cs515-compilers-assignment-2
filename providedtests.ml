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
 
  Test ("Student-Provided test for Add instr", [
    ("add", st_test "..."
            [(mk_block "main" [
                Add (Imm 1l, eax);
	            Add (eax, ebx);
                Add (Imm 2147483647l, ecx);
                Add (ecx, edx);
                Add (Imm 1l, edx);
                Add (ecx, esi);
                Add (Imm 2147483646l, esi);
            ])]
            (fun state ->
                state.s_regs.(0) = 1l &&
                state.s_regs.(1) = 1l &&
                state.s_regs.(2) = 2147483647l &&
                state.s_regs.(3) = -2147483648l &&
                state.s_regs.(4) = -3l) 
    );
  ]);
  Test ("Student-Provided test for Sub instr", [
    ("sub", st_test "..."
            [(mk_block "main" [
                Sub (Imm 1l, eax);
	            Sub (eax, ebx);
                Sub (Imm 2147483647l, ecx);
                Sub (ecx, edx);
                Sub (Imm 1l, edx);
                Sub (ecx, esi);
                Sub (Imm 2147483646l, esi);
            ])]
            (fun state ->
                state.s_regs.(0) = -1l &&
                state.s_regs.(1) = 1l &&
                state.s_regs.(2) = -2147483647l &&
                state.s_regs.(3) = 2147483646l &&
                state.s_regs.(4) = 1l) 
    );
  ]);
  Test ("Student-Provided test for Imul instr", [
    ("Imul", st_test "..."
            [(mk_block "main" [
                Add (Imm 1l, eax);
                Add (eax, ebx);
                Add (eax, ecx);
                Add (eax, edx);
	            Imul (Imm 20l, Eax);
                Imul (Imm 2147483647l, Ebx);
                Imul (Imm 2147483647l, Ecx);
                Imul (ecx, Ecx);
                Imul (Imm 2147483647l, Edx);
                Imul (Imm 2l, Edx);
            ])]
            (fun state ->
                state.s_regs.(0) = 20l &&
                state.s_regs.(1) = 2147483647l &&
                state.s_regs.(2) = 1l &&
                state.s_regs.(3) = -2l) 
    );
  ]);
] 
