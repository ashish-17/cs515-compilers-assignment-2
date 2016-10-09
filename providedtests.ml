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
  Test ("Student-Provided test for Not | And | Or | Xor instr", [
    ("Logical", st_test "..."
            [(mk_block "main" [
                Not (eax);
                Not (ebx);
                Not (ecx);
                Not (edx);
                And (Imm 0x1al, ebx);
                Or (Imm 0x00l, ecx);
	            Xor (Imm 0xfffffffel, edx);
            ])]
            (fun state ->
                state.s_regs.(0) = 0xffffffffl &&
                state.s_regs.(1) = 0x1al &&
                state.s_regs.(2) = 0xffffffffl &&
                state.s_regs.(3) = 1l) 
    );
  ]);
  Test ("Student-Provided test for pop instr", [
    ("pop", st_test "..."
            [(mk_block "main" [
                Add (Imm (-8l), esp);
                Mov (Imm (42l),  stack_offset 0l);
                Pop (eax);
                Ret;
            ])]
            (fun state -> state.s_regs.(0) = 0x2al &&
        state.s_regs.(7) = 0xFFFFFFFCl)
    );
  ]);
  
  Test ("Student-Provided test for And instr", [
  ("and", st_test "eax=2 ebx=2 ecx=1 *1023=1" [(mk_block "main"  [
      Mov (Imm 2l, eax);
      Mov (Imm 3l, ebx);
      Mov (Imm 255l, ecx);
      Mov (Imm 1l , stack_offset 0l);
      And (eax, eax);
      And (Imm 2l, eax);
      And (eax, ebx);
      And (stack_offset 0l, ecx);
      Ret;
    ])] 
  (fun state ->(*print_state state;*)
        state.s_regs.(0) = 2l &&
        state.s_regs.(1) = 2l &&
        state.s_regs.(2) = 1l &&
        state.s_memory.(mem_size-1) = 1l));
  ]);
] 
