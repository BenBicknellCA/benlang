use anyhow::{Error, Result};
use parser::value::Literal;
use parser::value::Value;
use slotmap::{SecondaryMap, SlotMap};

use codegen::OpCode;
use codegen::{FuncProto, RegOrConst};
use parser::object::FuncId;
use parser::scanner::{Symbol, SymbolTable};

pub struct Upvalue {
    value: Value,
    closed: bool,
    // frame offset?
    location: usize,
}

pub struct VM {
    func_protos: SecondaryMap<FuncId, FuncProto>,
    string_interner: SymbolTable,
    main: FuncId,
    call_stack: Vec<CallFrame>,
    frame_pointer: usize,
}

struct RegisterStack([Value; 255]);
impl Default for RegisterStack {
    fn default() -> Self {
        Self([const { Value::Literal(Literal::Nil) }; 255])
    }
}

impl RegisterStack {
    pub fn get(&self) -> &[Value; 255] {
        &self.0
    }

    pub fn get_mut(&mut self) -> &mut [Value; 255] {
        &mut self.0
    }
}

pub struct CallFrame {
    registers: RegisterStack,
    pub func: FuncId,
    pub return_address: usize,
    top: usize,
    base: usize,
    ip: usize,
}

impl CallFrame {
    pub fn new(func: FuncId, return_address: usize, base: usize) -> Self {
        Self {
            func,
            return_address,
            registers: RegisterStack::default(),
            top: 1,
            ip: 0,
            base,
        }
    }
}

impl VM {
    pub fn new(
        func_protos: SecondaryMap<FuncId, FuncProto>,
        string_interner: SymbolTable,
        main: FuncId,
    ) -> Self {
        Self {
            func_protos,
            string_interner,
            main,
            call_stack: Vec::new(),
            frame_pointer: 0,
        }
    }

    pub fn run_program(&mut self) {
        self.call_stack.push(CallFrame::new(self.main, 0, 0));
        for func in self.func_protos.values() {
            println!();
            println!();
            println!();
            for op in &func.bytecode {
                println!("{op:?}");
            }
            println!();
            println!();
        }
        self.eval();
    }

    //    fn call(&mut self, func: FuncId, return_address: usize) {
    //        self.call_stack.push(CallFrame::new(func, return_address));
    //        self.frame_pointer += 1;
    //    }

    pub fn opnd(&self, reg_or_const: RegOrConst) -> Value {
        let frame = &self.call_stack[self.frame_pointer];
        let func_id = frame.func;
        let const_pool = &self.func_protos[func_id].const_pool;
        match reg_or_const {
            RegOrConst::Reg(reg) => frame.registers.get()[reg as usize],
            RegOrConst::Const(const_) => Value::Literal(const_pool[const_]),
        }
    }

    pub fn get_frame_pointer(&self) -> usize {
        self.frame_pointer
    }

    pub fn return_from_frame(&mut self) {
        self.frame_pointer -= 1;
        self.call_stack.pop();
    }

    pub fn map_args_to_func(&mut self, func_reg: u8, arg_count: usize) {
        let fp = self.frame_pointer;
        let val = self.call_stack[self.frame_pointer].registers.get_mut()[func_reg as usize];
        let func_id = val.get_literal().unwrap().get_func().unwrap();

        //        let to_call = &self.func_protos[func_id];
        let frame = &self.call_stack[self.frame_pointer];
        let dst_base = self.func_protos[func_id].arity;
        let ip = frame.ip;

        self.call_stack
            .push(CallFrame::new(func_id, 0, dst_base as usize));
        println!("{func_reg}");
        for idx in 1..=arg_count {
            let dst_idx = idx + func_reg as usize;
            let src_idx = func_reg as usize + idx;

            println!("idx: {idx} dst: {dst_base}, src: {src_idx} ip: {ip}");
            println!(
                "VAL_1: {:?}",
                self.call_stack[self.frame_pointer].registers.get_mut()[src_idx]
            );
            println!(
                "VAL_2: {:?}",
                self.call_stack[self.frame_pointer + 1].registers.get_mut()[dst_idx]
            );
            self.call_stack[self.frame_pointer + 1].registers.get_mut()[dst_idx] =
                self.call_stack[self.frame_pointer].registers.get_mut()[src_idx];
            //            println!("VAL_2: {idx:?} {:?}", self.call_stack[self.frame_pointer + 1].registers.get_mut()[idx]);
        }
        self.frame_pointer += 1;
    }

    pub fn get_next_op(&self) {}

    fn eval(&mut self) -> Result<()> {
        loop {
            let instructions = &self.func_protos[self.call_stack[self.frame_pointer].func];
            let op = &instructions.bytecode[self.call_stack[self.frame_pointer].ip];
            let ip = self.call_stack[self.frame_pointer].ip;
            let fp = self.frame_pointer;

            match op {
                OpCode::Add(dst, lhs, rhs) => {
                    print!("{fp}: {ip}: Add: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? + rhs.get_literal()?);

                    println!("call_stack[{fp}][{dst:?}] =  {lhs:?} + {rhs:?}",);

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Sub(dst, lhs, rhs) => {
                    print!("{fp}: {ip}: Sub: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? - rhs.get_literal()?);

                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} - {rhs:?}",);

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = res;
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Mul(dst, lhs, rhs) => {
                    print!("{fp}: {ip}: Mul: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? * rhs.get_literal()?);

                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} * {rhs:?}",);

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Div(dst, lhs, rhs) => {
                    print!("{fp}: {ip}: Div: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? / rhs.get_literal()?);

                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} / {rhs:?}",);

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }

                OpCode::LoadConst(dst, const_id) => {
                    print!("{fp}: {ip}: LoadConst: ");
                    let func = &self.call_stack[self.frame_pointer];
                    let func_id = func.func;
                    let const_ = self.func_protos[func_id].const_pool[*const_id];

                    println!(
                        "call_stack[frame_pointer][{dst:?}] =  {:?}",
                        Value::Literal(const_)
                    );

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] =
                        Value::Literal(const_);

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Move(dst, src) => {
                    print!("{fp}: {ip}: Move: ");
                    let src = self.opnd(*src);
                    println!("call_stack[frame_pointer][{dst:?}] =  {src:?}");

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = src;
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Gt(lhs, rhs) => {
                    print!("{fp}: {ip}: Gt: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    println!("{lhs:?} > {rhs:?}",);

                    if lhs > rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Lt(lhs, rhs) => {
                    print!("{fp}: {ip}: Lt: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    println!("{lhs:?} < {rhs:?}",);

                    if lhs < rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Le(lhs, rhs) => {
                    print!("{fp}: {ip}: Le: ");
                    let val_lhs = self.opnd(*lhs);
                    let val_rhs = self.opnd(*rhs);
                    println!("{lhs:?}: {val_lhs:?} <= {rhs:?}: {val_rhs:?}",);
                    if val_lhs <= val_rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Eq(lhs, rhs) => {
                    print!("{fp}: {ip}: Eq: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);

                    println!("{lhs:?} == {rhs:?}",);
                    if lhs == rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Ne(lhs, rhs) => {
                    print!("{fp}: {ip}: Ne: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);

                    println!("{lhs:?} != {rhs:?}",);
                    if lhs != rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Call(func_reg, args, arg_count) => {
                    print!("{fp}: {ip}: Call: ");
                    //                    println!("{func:?}");

                    self.map_args_to_func(*func_reg, *arg_count as usize);
                    self.call_stack[self.frame_pointer].ip += 1;
                    //                    println!("{func_reg:?}");
                }
                OpCode::Jmp(jmp) => {
                    println!("{fp}: {ip} Jmp: {ip} + {jmp:?}");
                    self.call_stack[self.frame_pointer].ip = (ip as i32 + *jmp) as usize;
                }
                OpCode::Return0 => break,
                OpCode::Return1(ret_val) => {
                    print!("{fp}: {ip}: Return1: ");

                    let ret_val = self.opnd(*ret_val);
                    let return_to = self.call_stack[self.frame_pointer - 1].top;

                    self.call_stack[self.frame_pointer - 1].registers.get_mut()[return_to] =
                        ret_val;

                    println!("{ret_val:?} to {return_to:?}");

                    //                    self.call_stack[self.frame_pointer].registers.get_mut()[return_address] =
                    //                        ret_val;
                    self.return_from_frame()
                    //                    self.call_stack[self.frame_pointer].registers
                }
                _ => todo!("{op:?}"),
            }
        }
        Ok(())
    }
}
