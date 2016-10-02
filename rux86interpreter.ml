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


let interpret (code:insn_block list) (xs:x86_state) (l:lbl) : unit = 
failwith "unimplemented"

let run (code:insn_block list): int32 = 
  let main = mk_lbl_named "main" in 
  let xs = mk_init_state () in 
  let _ = interpret code xs main in 
    xs.s_regs.(ieax)
