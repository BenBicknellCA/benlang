use anyhow::Result;
use parser::value::Literal;
use parser::value::Value;
use slotmap::SecondaryMap;

use codegen::OpCode;
use codegen::{FuncProto, RegOrConst};
use parser::object::FuncId;
use parser::scanner::{Symbol, SymbolTable};
use std::collections::HashMap;

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
    globals: HashMap<Symbol, Value>,
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
    pub return_frame: usize,
    ip: usize,
}

impl CallFrame {
    pub fn new(func: FuncId, return_address: usize, return_frame: usize) -> Self {
        Self {
            func,
            return_address,
            return_frame,
            registers: RegisterStack::default(),
            ip: 0,
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
            globals: HashMap::new(),
        }
    }

    pub fn run_program(&mut self) -> Result<()> {
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
        self.eval()?;
        Ok(())
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

                    //                    println!("{:?}", self.call_stack[self.frame_pointer].registers.get());

                    let val_lhs = self.opnd(*lhs);
                    let val_rhs = self.opnd(*rhs);
                    let res = Value::Literal(val_lhs.get_literal()? - val_rhs.get_literal()?);

                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = res;
                    println!(
                        "call_stack[{fp}][{dst}] = {lhs:?}: {val_lhs:?} - {rhs:?}: {val_rhs:?}",
                    );
                    //                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} - {rhs:?} : {:?}",
                    //                             self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize]
                    //                   );
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

                OpCode::Copy(dst, src) => {
                    let src = self.opnd(*src);

                    println!("DST: {dst:?} SRC: {src:?}");

                    print!("{fp}: {ip}: Copy: ");
                    self.call_stack[self.frame_pointer].registers.get_mut()[*dst as usize] = src;
                    println!("[{dst}] = {src:?}");
                    self.call_stack[self.frame_pointer].ip += 1;
                }

                // 1 is func_reg, 2 is func_id, 3 is arg_count
                OpCode::Call(func_id, ret_reg, arg_count) => {
                    print!("{fp}: {ip}: Call: ");
                    let func: FuncId = self.call_stack[self.frame_pointer].registers.get()
                        [*func_id as usize]
                        .get_literal()?
                        .get_func()?;
                    self.call_stack
                        .push(CallFrame::new(func, *ret_reg as usize, fp));

                    println!(
                        "func in Reg[{func_id:?}], {arg_count} args, return to Reg[{ret_reg:?}]"
                    );
                    let func_reg = *func_id as usize;
                    for arg in 1..=(*arg_count as usize) {
                        let arg_val =
                            self.call_stack[self.frame_pointer].registers.get()[arg + func_reg];
                        //                        println!("arg: {arg}");
                        //                        println!("{:?}", self.call_stack[self.frame_pointer].registers.get_mut()[arg + ip - 1]);
                        self.call_stack[self.frame_pointer + 1].registers.get_mut()[arg] =
                            self.call_stack[self.frame_pointer].registers.get()[arg + func_reg];
                        //                        println!("{:?}", self.call_stack[self.frame_pointer].registers.get_mut()[arg + func_reg]);
                        //                        println!("{:?}", self.call_stack[self.frame_pointer + 1].registers.get_mut()[arg]);
                        //                        println!("res: {:?}", self.call_stack[self.frame_pointer + 1].registers.get_mut()[arg]);
                    }

                    self.call_stack[self.frame_pointer].ip += 1;
                    self.frame_pointer += 1;
                    println!();
                }
                OpCode::Jmp(jmp) => {
                    println!("{fp}: {ip} Jmp: {ip} + {jmp:?}");
                    self.call_stack[self.frame_pointer].ip = (ip as i32 + *jmp) as usize;
                }
                OpCode::Return0 => break,
                OpCode::Return1(ret_val) => {
                    print!("{fp}: {ip}: Return1: ");

                    let ret_fp = self.call_stack[self.frame_pointer].return_frame;
                    let ret_reg = self.call_stack[self.frame_pointer].return_address;

                    let ret_val =
                        self.call_stack[self.frame_pointer].registers.get()[ret_val.get_reg()];
                    println!("{ret_val:?}");

                    //                    println!("{ret_val:?} to {return_to:?}");

                    self.call_stack[ret_fp].registers.get_mut()[ret_reg] = ret_val;

                    self.frame_pointer = ret_fp;
                    self.call_stack.pop();

                    //                    self.call_stack[self.frame_pointer].registers.get_mut()[return_address] =
                    //                        ret_val;
                    //                    self.return_from_frame()
                    //                    self.call_stack[self.frame_pointer].registers
                }
                OpCode::GetGlobal(dst, src) => {
                    print!("{fp}: {ip}: GetGlobal: ");

                    let dst = dst.get_reg();
                    let name = self.opnd(*src).get_literal()?.get_symbol()?;
                    let val = self.globals[&name];

                    self.call_stack[self.frame_pointer].registers.get_mut()[dst] = val;

                    println!("dst: {dst:?} src: {src:?}");

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::SetGlobal(dst, src) => {
                    print!("{fp}: {ip}: SetGlobal: ");

                    let name = self.opnd(*dst).get_literal()?.get_symbol()?;
                    let val = self.opnd(*src);

                    println!("{name:?} : {val:?} ");

                    self.globals.insert(name, val);

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                _ => todo!("{op:?}"),
            }
        }
        Ok(())
    }
}
