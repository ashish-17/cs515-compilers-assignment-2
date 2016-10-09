(* CS515: Assignment 2 RUX86 Programming and RUX86 Interpreter *)

open Rux86

exception X86_segmentation_fault of string

(* Registers are interpreted as indices into register array *)
let ieax = 0
let iebx = 1
let iecx = 2
let iedx = 3
let iesi = 4
let iedi = 5
let iebp = 6
let iesp = 7

let get_register_id = function
 | Eax -> ieax
 | Ebx -> iebx
 | Ecx -> iecx
 | Edx -> iedx
 | Esi -> iesi
 | Edi -> iedi
 | Ebp -> iebp
 | Esp -> iesp


let mem_size = 1024                (* Size of memory in words *)
let mem_top : int32 = 0xfffffffcl  (* Last addressable memory location *)
let mem_bot: int32 = (Int32.mul (Int32.of_int (mem_size * 4)) (-1l))

(* Maps virtual addresses (int32 addresses) to physical addresses (int
 indices). Raises an X86_segmentation_fault exception if the provided
 virtual address does not map or if the address is unaligned.
*)

let map_addr (addr: int32) : int = 
    if Int32.rem addr 4l <> 0l || addr < mem_bot || addr > mem_top then
        raise (X86_segmentation_fault "Invalid Address")
    else
        Int32.to_int (Int32.div (Int32.sub addr mem_bot) 4l)
        

type x86_state = {
   s_memory : int32 array;  (* 1024 32-bit words -- the heap *)
   s_regs : int32 array;    (* 8 32-bit words -- the register file *)
   mutable s_of : bool;     (* overflow flag *)
   mutable s_sf : bool;     (* sign bit flag *)
   mutable s_zf : bool;     (* zero flag *)
}

let mk_init_state (): x86_state = 
  let xs = {
    s_memory = Array.make mem_size 0l;
    s_regs   = Array.make 8 0l;
    s_of     = false;
    s_sf     = false;
    s_zf     = false;
  } in 
  xs.s_regs.(iesp) <- mem_top; xs

let print_state (xs:x86_state): unit = 
 (Array.iter (fun e -> Printf.printf "%lx" e) xs.s_memory);
 (Printf.printf "\neax: %lx ebx: %lx ecx: %lx edx: %lx" xs.s_regs.(ieax) 
     xs.s_regs.(iebx) xs.s_regs.(iecx) xs.s_regs.(iedx));
 (Printf.printf "\nesi: %lx edi: %lx ebp: %lx esp: %lx" xs.s_regs.(iesi) 
     xs.s_regs.(iedi) xs.s_regs.(iebp) xs.s_regs.(iesp));
 (Printf.printf "\n OF: %b SF: %b ZF: %b" xs.s_of xs.s_sf xs.s_zf)



(* Helper function that determines whether a given condition code
   applies in the x86 state xs. *)  
let condition_matches (xs:x86_state) (c:Rux86.ccode) : bool =
    begin match c with
        | Sgt       -> xs.s_of = xs.s_sf && xs.s_zf == false
        | Sge       -> xs.s_of = xs.s_sf
        | Slt       -> xs.s_of <> xs.s_sf
        | Sle       -> xs.s_of <> xs.s_sf || xs.s_zf
        | Eq        -> xs.s_zf
        | NotEq     -> xs.s_zf = false
        | Zero      -> xs.s_zf
        | NotZero   -> xs.s_zf = false
    end

(* Returns the bit at a given index in a 32-bit word as a boolean *)
let get_bit bitidx n =
  let shb = Int32.shift_left 1l bitidx in
  Int32.logand shb n = shb  

let has_same_sign (s:int64) (d:int64) : bool = 
    let isSPos = Int64.compare s (Int64.of_int32 0l) >= 0 in
    let isDPos = Int64.compare d (Int64.of_int32 0l) >= 0 in
    if (isSPos && isDPos) || (not isSPos && not isDPos) then true else false

let eval_reg (r:reg) (xs:x86_state) :int32 = Array.get xs.s_regs (get_register_id r) 

let eval_disp (d:disp) : int32 = 
    match d with
    | DImm x -> x
    | _ ->  raise (X86_segmentation_fault "Can't evaluate a label")

let eval_ind (i:ind) (xs:x86_state): int32 = 
    begin
        match i with
        | {i_base=Some b; i_iscl=Some s; i_disp=Some d} -> Int32.add (Int32.add (eval_reg b xs) (Int32.mul (eval_reg (fst s) xs) (snd s))) (eval_disp d)
        | {i_base=Some b; i_iscl=Some s;}               -> Int32.add (eval_reg b xs) (Int32.mul (eval_reg (fst s) xs) (snd s))
        | {i_base=Some b; i_disp=Some d}                -> Int32.add (eval_reg b xs) (eval_disp d)
        | {i_iscl=Some s; i_disp=Some d}                -> Int32.add (Int32.mul (eval_reg (fst s) xs) (snd s)) (eval_disp d)
        | {i_base=Some b;}                              -> eval_reg b xs
        | {i_iscl=Some s;}                              -> Int32.mul (eval_reg (fst s) xs) (snd s)
        | {i_disp=Some d}                               -> eval_disp d
        | _                                             -> 0l
    end

let rec eval_operand (o:operand) (xs:x86_state) : int32 = 
    begin
        match o with
        | Imm x -> x
        | Lbl l -> raise (X86_segmentation_fault "Can't evaluate a label")
        | Reg r -> eval_reg r xs
        | Ind i -> eval_ind i xs
    end


let read_mem (addr:int32) (xs:x86_state) :int32 = 
    begin
        let idx = map_addr(addr) in
        Array.get xs.s_memory idx
    end

let rec eval_operand_val (o:operand) (xs:x86_state) : int32 = 
    begin
        match o with
        | Imm x -> x
        | Lbl l -> raise (X86_segmentation_fault "Can't evaluate a label")
        | Reg r -> eval_reg r xs
        | Ind i -> read_mem (eval_ind i xs) xs
    end

let read_reg (r:reg) (xs:x86_state) :int32 = Array.get xs.s_regs (get_register_id r)
let write_reg (r:reg) (data:int32) (xs:x86_state) :unit = Array.set xs.s_regs (get_register_id r) data

let write_mem (addr:int32) (data:int32) (xs:x86_state) :unit = 
    begin
        let idx = map_addr(addr) in
        Array.set xs.s_memory idx data
    end

let rec read_operand (o:operand) (xs:x86_state) : int32 = 
    begin
        match o with
        | Imm x -> read_mem x xs
        | Lbl l -> raise (X86_segmentation_fault "Can't evaluate a label")
        | Reg r -> read_reg r xs
        | Ind i -> read_mem (eval_ind i xs) xs
    end

let rec write_operand (o:operand) (data:int32) (xs:x86_state) : unit = 
    begin
        match o with
        | Imm x -> write_mem x data xs
        | Lbl l -> raise (X86_segmentation_fault "Can't evaluate a label")
        | Reg r -> write_reg r data xs
        | Ind i -> write_mem (eval_ind i xs) data xs
    end

let set_sf (xs:x86_state) (result:int32) : unit = 
    begin
        let _ = (xs.s_sf <- get_bit 31 result) in ()
    end

let set_zf (xs:x86_state) (result:int32) : unit = 
    begin
        let _ = (if result = 0l then xs.s_zf <- true else xs.s_zf <- false) in ()
    end

let rec find_block (code: insn_block list) (l:lbl) : insn_block = 
    begin
        match code with
        | [] -> raise (X86_segmentation_fault "Invalid Label")
        | h::t -> if h.label = l then h else find_block t l
    end

let interpret_neg (d:operand) (xs:x86_state) : unit = 
    begin
        let d_val = read_operand d xs in
        let result = Int32.neg d_val in
        let _ = (if Int32.min_int = result then xs.s_of <- true else xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_add (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        let d_val = read_operand d xs in
        let result64 = Int64.add (Int64.of_int32 s_val) (Int64.of_int32 d_val) in
        let result = Int64.to_int32 result64 in
        let _ = (if (has_same_sign (Int64.of_int32 s_val) (Int64.of_int32 d_val) && not (has_same_sign (Int64.of_int32 result) (Int64.of_int32 s_val))) then 
            xs.s_of <- true 
        else xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_sub (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = Int32.mul (eval_operand_val s xs) (-1l) in
        let d_val = read_operand d xs in
        let result64 = Int64.add (Int64.of_int32 s_val) (Int64.of_int32 d_val) in
        let result = Int64.to_int32 result64 in
        let _ = (if (has_same_sign (Int64.of_int32 s_val) (Int64.of_int32 d_val) && not (has_same_sign (Int64.of_int32 result) (Int64.of_int32 s_val)) || s_val = Int32.min_int) then 
            xs.s_of <- true 
        else xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_mul (s:operand) (d:reg) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        let d_val = read_reg d xs in
        let result64 = Int64.mul (Int64.of_int32 s_val) (Int64.of_int32 d_val) in
        let result = Int64.to_int32 result64 in
        let _ = (if result64 <> Int64.of_int32 result then 
            xs.s_of <- true 
        else xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        Printf.printf("%lx x %lx = %lx") s_val d_val result;
        write_reg d result xs
    end

let interpret_not (d:operand) (xs:x86_state) : unit = 
    begin
        let d_val = read_operand d xs in
        let result = Int32.lognot d_val in
        let _ = (xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_and (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        let d_val = read_operand d xs in
        let result = Int32.logand s_val d_val in
        let _ = (xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_or (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        let d_val = read_operand d xs in
        let result = Int32.logor s_val d_val in
        let _ = (xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_xor (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        let d_val = read_operand d xs in
        let result = Int32.logxor s_val d_val in
        let _ = (xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
        write_operand d result xs
    end

let interpret_sar (amnt:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let amnt_val = Int32.to_int(eval_operand_val amnt xs) in
        let d_val = read_operand d xs in
        let result = Int32.shift_right d_val amnt_val in
        write_operand d result xs;
        if amnt_val <> 0 then
            set_sf xs result;
            set_zf xs result;
        if amnt_val = 1 then
            let _ = (xs.s_of <- false) in ();
    end

let interpret_shl (amnt:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let amnt_val = Int32.to_int(eval_operand_val amnt xs) in
        let d_val = read_operand d xs in
        let result = Int32.shift_left d_val amnt_val in
        write_operand d result xs;
        if amnt_val <> 0 then
            set_sf xs result;
            set_zf xs result;
        if amnt_val = 1 && ((get_bit 31 result) <> (get_bit 30 result)) then
            let _ = (xs.s_of <- true) in ();
    end

let interpret_shr (amnt:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let amnt_val = Int32.to_int(eval_operand_val amnt xs) in
        let d_val = read_operand d xs in
        let result = Int32.shift_right_logical d_val amnt_val in
        write_operand d result xs;
        if amnt_val <> 0 then
            set_sf xs result;
            set_zf xs result;
        if amnt_val = 1 then
            let _ = (xs.s_of <- get_bit 31 d_val) in ();
    end

let interpret_setb (cc:Rux86.ccode) (d:operand) (xs:x86_state) : unit = 
    begin
        let d_val = read_operand d xs in
        let isCCMatch = condition_matches xs cc in
        let result = if isCCMatch then Int32.logand d_val 0xfffffff1l else Int32.logand d_val 0xfffffff0l in
        write_operand d result xs
    end

let interpret_lea (s:ind) (d:reg) (xs:x86_state) : unit = 
    begin
        let s_val = eval_ind s xs in
        write_reg d s_val xs
    end

let interpret_mov (s:operand) (d:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand_val s xs in
        write_operand d s_val xs
    end

let interpret_push (s:operand) (xs:x86_state) : unit = 
    begin
        let s_val = eval_operand s xs in
        let curr_esp = read_reg Esp xs in
        let new_esp = Int32.sub curr_esp 4l in
        write_reg Esp new_esp xs;
        write_mem new_esp s_val xs
    end

let interpret_pop (d:operand) (xs:x86_state) : unit = 
    begin
        let curr_esp = read_reg Esp xs in
        let data = read_mem curr_esp xs in
        let new_esp = Int32.add curr_esp 4l in
        write_reg Esp new_esp xs;
        write_operand d data xs
    end

let interpret_cmp (s1:operand) (s2:operand) (xs:x86_state) : unit = 
    begin
        let s1_val = Int32.mul (-1l) (eval_operand_val s1 xs) in 
        let s2_val = eval_operand_val s2 xs in  
        let result64 = Int64.add (Int64.of_int32 s1_val) (Int64.of_int32 s2_val) in
        let result = Int64.to_int32 result64 in
        let _ = (if (has_same_sign (Int64.of_int32 s1_val) (Int64.of_int32 s2_val) && not (has_same_sign (Int64.of_int32 result) (Int64.of_int32 s1_val)) || s1_val = Int32.min_int) then 
            xs.s_of <- true 
        else xs.s_of <- false) in ();
        set_sf xs result;
        set_zf xs result;
    end

let rec interpret (code:insn_block list) (xs:x86_state) (l:lbl) : unit = 
    Printf.printf("\nLabel\n");
    begin
        match code with
        | [] -> ()
        | _  -> interpret_insns (find_block code l).insns xs code
    end
    and interpret_insns (code_insns:insn list) (xs:x86_state) (code:insn_block list): unit = 
    begin
        match code_insns with
        | [] -> ()
        | i::rest ->
                begin
                    match i with
                    | Neg (d) -> 
                            interpret_neg d xs;
                            interpret_insns rest xs code
                    | Add (s, d) -> 
                            Printf.printf("\nadd\n");
                            interpret_add s d xs;
                            interpret_insns rest xs code
                    | Sub (s, d) -> 
                            interpret_sub s d xs;
                            interpret_insns rest xs code
                    | Imul (s, d) -> 
                            interpret_mul s d xs;
                            interpret_insns rest xs code
                    | Not (d) -> 
                            interpret_not d xs;
                            interpret_insns rest xs code
                    | And (s, d) -> 
                            interpret_and s d xs;
                            interpret_insns rest xs code
                    | Or (s, d) -> 
                            interpret_or s d xs;
                            interpret_insns rest xs code
                    | Xor (s, d) -> 
                            interpret_xor s d xs;
                            interpret_insns rest xs code
                    | Sar (amnt, d) -> 
                            interpret_sar amnt d xs;
                            interpret_insns rest xs code
                    | Shl (amnt, d) -> 
                            interpret_shl amnt d xs;
                            interpret_insns rest xs code
                    | Shr (amnt, d) -> 
                            interpret_shr amnt d xs;
                            interpret_insns rest xs code
                    | Setb (cc, d) -> 
                            interpret_setb cc d xs;
                            interpret_insns rest xs code
                    | Lea (s, d) -> 
                            interpret_lea s d xs;
                            interpret_insns rest xs code
                    | Mov (s, d) -> 
                            Printf.printf("\nMov\n");
                            interpret_mov s d xs;
                            interpret_insns rest xs code
                    | Push (s) -> 
                            Printf.printf("\npush\n");
                            interpret_push s xs;
                            interpret_insns rest xs code
                    | Pop (d) -> 
                            Printf.printf("\npop\n");
                            interpret_pop d xs;
                            interpret_insns rest xs code
                    | Cmp (s1, s2) -> 
                            interpret_cmp s1 s2 xs;
                            interpret_insns rest xs code
                    | Jmp (s) -> 
                            interpret_jmp s xs code;
                    | Call (s) -> 
                            interpret_call s xs code;
                            interpret_insns rest xs code
                    | Ret -> 
                            interpret_ret xs
                    | J (cc, l) ->
                            Printf.printf("\nJJ\n");
                            print_state xs;
                            begin
                                if condition_matches xs cc then
                                    interpret code xs l
                                else
                                    interpret_insns rest xs code
                            end
                end
    end
    and interpret_jmp (d:operand) (xs:x86_state) (code:insn_block list) : unit = 
        Printf.printf("\njmp\n");
        begin
            match d with
            | Lbl l -> interpret code xs l     
            | _ -> raise (X86_segmentation_fault "Invalid jump")
        end
    and interpret_call (d:operand) (xs:x86_state) (code:insn_block list) : unit = 
        Printf.printf("\nCall\n");
        begin
            match d with
            | Lbl l -> 
                    begin
                        interpret_push (Imm 0l) xs;
                        interpret code xs l;
                    end
            | _ -> raise (X86_segmentation_fault "Invalid jump");
        end
    and interpret_ret (xs:x86_state) : unit = 
        Printf.printf("\nRet\n");
        begin
            let curr_esp = read_reg Esp xs in
            let new_esp = Int32.add curr_esp 4l in
            write_reg Esp new_esp xs;
        end

let run (code:insn_block list): int32 = 
    Printf.printf("\nRun code");
  let main = mk_lbl_named "main" in 
  let xs = mk_init_state () in 
  let _ = interpret code xs main in 
    xs.s_regs.(ieax)
