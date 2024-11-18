use std::{env, fs};

const LDCNST: i32 = 1;
const STLOC: i32 = 2;
const LDLOC: i32 = 3;
const ADD: i32 = 4;
const SUB: i32 = 5;
const MUL: i32 = 6;
const DIV: i32 = 7;
const CMP: i32 = 8;
const NOT: i32 = 9;
const AND: i32 = 10;
const OR: i32 = 11;
const JMP: i32 = 12;
const JMPNIF: i32 = 13;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Invalid Number of Arguments given");
    }

    let bin_path = args[1].clone();
    let content_u8: Vec<u8> = match fs::read(bin_path) {
        Ok(contents) => contents,
        Err(err) => panic!("Error reading file: {0}", err)
    };

    let mut code: Vec<i32> = Vec::with_capacity(content_u8.len() / 4);
    for i in (0..content_u8.len()).step_by(4) {
        let slice: &[u8] = &content_u8[i..i+4];
        code.push(i32::from_le_bytes(slice.try_into().unwrap()));
    }

    let locals_size: i32 = code[0];
    let stack_size: i32 = code[1];
    let stack_frame_size: i32 = 1 + locals_size + stack_size;

    alloca::with_alloca_zeroed(stack_frame_size as usize, |stack_frame| {
        exec(&code, locals_size, stack_frame);
    });
}

fn exec(code: &Vec<i32>, locals_size: i32, stack_frame: &mut [u8]) {
    let (locals, stack) = stack_frame.split_at_mut(locals_size as usize);

    let mut curr_stack_size = 0;
    let mut curr = 2;
    while curr < code.len() {
        let op_code = code[curr];
        let mut jmp = 0;

        if op_code == LDCNST {
            let param = code[curr+1] as u8;
            stack[curr_stack_size] = param;
            dbg!("LDCNST :: Loaded {0} to .s{1}", param, curr_stack_size);

            curr_stack_size += 1;
            jmp = 2;
        }
        else if op_code == LDLOC {
            let param = code[curr+1] as usize;
            stack[curr_stack_size] = locals[param];
            dbg!("LDLOC :: Loaded {0}/.l{1} to .s{2}", locals[param], param, curr_stack_size);

            curr_stack_size += 1;
            jmp = 2;
        }
        else if op_code == STLOC {
            let param = code[curr+1] as usize;
            locals[param] = stack[curr_stack_size - 1];


            dbg!("STLOC :: Stored {0}/.s{1} to .l{2}", stack[curr_stack_size - 1], curr_stack_size - 1, param);
            curr_stack_size -= 1;
            jmp = 2;
        }

        curr += jmp;
    }

    dbg!("Execution finished, locals state: ", locals);

}
