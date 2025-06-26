use anyhow::Result;
use parser::value::Literal;
use parser::value::Value;
use slotmap::SecondaryMap;

use compiler::{FuncProto, OpCode, RegOrConst};
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
    globals: HashMap<Symbol, Value>,
    frame_pointer: usize,
}

struct RegisterStack([Value; 255]);
impl Default for RegisterStack {
    fn default() -> Self {
        const LIT_NIL: Literal = Literal::Nil;
        const VAL_NIL: Value = Value::Literal(LIT_NIL);
        Self([const { VAL_NIL }; 255])
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
        let call_stack = vec![CallFrame::new(main, 0, 0)];
        Self {
            func_protos,
            string_interner,
            main,
            call_stack,
            globals: HashMap::new(),
            frame_pointer: 0,
        }
    }

    pub fn run_program(&mut self) -> Result<()> {
        // for func in self.func_protos.values() {
        //     println!();
        //     println!();
        //     println!();
        //     for op in &func.bytecode {
        //         println!("{op:?}");
        //     }
        //     println!();
        //     println!();
        // }
        self.eval()?;
        Ok(())
    }

    //    fn call(&mut self, func: FuncId, return_address: usize) {
    //        self.call_stack.push(CallFrame::new(func, return_address));
    //        fp += 1;
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

    pub fn return_from_frame(&mut self, fp: &mut usize) {
        *fp += 1;
        self.call_stack.pop();
    }

    pub fn get_next_op(&self) {}

    pub fn print_val(&self, val: RegOrConst) {
        let val = self.opnd(val);
        match val {
            Value::Literal(lit) => match lit {
                Literal::String(sym) => {
                    println!("{}", self.string_interner.resolve(sym).unwrap());
                }
                Literal::Number(num) => {
                    println!("{num}");
                }
                _ => todo!("{val:?}"),
            },
            Value::Object(_) => todo!(),
        }
    }

    fn eval(&mut self) -> Result<()> {
        loop {
            let instructions = &self.func_protos[self.call_stack[self.frame_pointer].func];
            let op = &instructions.bytecode[self.call_stack[self.frame_pointer].ip];
            let ip = self.call_stack[self.frame_pointer].ip;
            let fp = self.frame_pointer;

            let op = &instructions.bytecode[ip];
            match op {
                OpCode::Add(dst, lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Add: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? + rhs.get_literal()?);

                    #[cfg(feature = "vm_debug")]
                    println!("call_stack[{fp}][{dst:?}] =  {lhs:?} + {rhs:?}");

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Sub(dst, lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Sub: ");

                    //                    println!("{:?}", self.call_stack[fp].registers.get());

                    let val_lhs = self.opnd(*lhs);
                    let val_rhs = self.opnd(*rhs);
                    let res = Value::Literal(val_lhs.get_literal()? - val_rhs.get_literal()?);

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = res;
                    #[cfg(feature = "vm_debug")]
                    println!(
                        "call_stack[{fp}][{dst}] = {lhs:?}: {val_lhs:?} - {rhs:?}: {val_rhs:?}"
                    );
                    //                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} - {rhs:?} : {:?}",
                    //                             self.call_stack[fp].registers.get_mut()[*dst as usize]
                    //                   );
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Mul(dst, lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Mul: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? * rhs.get_literal()?);

                    #[cfg(feature = "vm_debug")]
                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} * {rhs:?}");

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }

                OpCode::Mod(dst, lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Mod: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(Literal::Number(
                        lhs.get_literal()?.get_number()? % rhs.get_literal()?.get_number()?,
                    ));

                    #[cfg(feature = "vm_debug")]
                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} % {rhs:?}");

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }

                OpCode::Div(dst, lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Div: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    let res = Value::Literal(lhs.get_literal()? / rhs.get_literal()?);

                    #[cfg(feature = "vm_debug")]
                    println!("call_stack[{fp}][{dst:?}] = {lhs:?} / {rhs:?}");

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = res;

                    self.call_stack[self.frame_pointer].ip += 1;
                }

                OpCode::LoadConst(dst, const_id) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: LoadConst: ");
                    let func = &self.call_stack[fp];
                    let func_id = func.func;
                    let const_ = self.func_protos[func_id].const_pool[*const_id];

                    #[cfg(feature = "vm_debug")]
                    println!(
                        "call_stack[frame_pointer][{dst:?}] =  {:?}",
                        Value::Literal(const_)
                    );

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = Value::Literal(const_);

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Move(dst, src) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Move: ");
                    let src = self.opnd(*src);
                    #[cfg(feature = "vm_debug")]
                    println!("call_stack[frame_pointer][{dst:?}] =  {src:?}");

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = src;
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Gt(lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Gt: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    #[cfg(feature = "vm_debug")]
                    println!("{lhs:?} > {rhs:?}",);

                    if lhs > rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Lt(lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Lt: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);
                    #[cfg(feature = "vm_debug")]
                    println!("{lhs:?} < {rhs:?}",);

                    if lhs < rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Le(lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Le: ");
                    let val_lhs = self.opnd(*lhs);
                    let val_rhs = self.opnd(*rhs);
                    #[cfg(feature = "vm_debug")]
                    println!("{lhs:?}: {val_lhs:?} <= {rhs:?}: {val_rhs:?}",);
                    if val_lhs <= val_rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Eq(lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Eq: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);

                    #[cfg(feature = "vm_debug")]
                    println!("{lhs:?} == {rhs:?}");
                    if lhs == rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Ne(lhs, rhs) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Ne: ");
                    let lhs = self.opnd(*lhs);
                    let rhs = self.opnd(*rhs);

                    #[cfg(feature = "vm_debug")]
                    println!("{lhs:?} != {rhs:?}",);
                    if lhs != rhs {
                        self.call_stack[self.frame_pointer].ip += 1;
                    }
                    self.call_stack[self.frame_pointer].ip += 1;
                }

                OpCode::Copy(dst, src) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Copy: ");
                    let src = self.opnd(*src);

                    self.call_stack[fp].registers.get_mut()[*dst as usize] = src;
                    #[cfg(feature = "vm_debug")]
                    println!("[{dst}] = {src:?}");
                    self.call_stack[self.frame_pointer].ip += 1;
                }

                // 1 is func_reg, 2 is func_id, 3 is arg_count
                OpCode::Call(func_id, ret_reg, arg_count) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Call: ");
                    let func: FuncId = self.call_stack[self.frame_pointer].registers.get()
                        [*func_id as usize]
                        .get_literal()?
                        .get_func()?;
                    self.call_stack
                        .push(CallFrame::new(func, *ret_reg as usize, fp));

                    #[cfg(feature = "vm_debug")]
                    println!(
                        "func in Reg[{func_id:?}], {arg_count} args, return to Reg[{ret_reg:?}]"
                    );

                    let func_reg = *func_id as usize;
                    for arg in 1..=(*arg_count as usize) {
                        self.call_stack[self.frame_pointer + 1].registers.get_mut()[arg] =
                            self.call_stack[self.frame_pointer].registers.get()[arg + func_reg];
                    }

                    self.call_stack[self.frame_pointer].ip += 1;
                    self.frame_pointer += 1;

                    #[cfg(feature = "vm_debug")]
                    println!();
                }
                OpCode::Jmp(jmp) => {
                    #[cfg(feature = "vm_debug")]
                    println!("{fp}: {ip} Jmp: {ip} + {jmp:?}");
                    self.call_stack[fp].ip = (ip as i32 + *jmp) as usize;
                }
                OpCode::Return0 => break,
                OpCode::Return1(ret_val) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Return1: ");

                    let ret_fp = self.call_stack[fp].return_frame;
                    let ret_reg = self.call_stack[fp].return_address;

                    let ret_val = self.call_stack[fp].registers.get()[ret_val.get_reg()];
                    #[cfg(feature = "vm_debug")]
                    println!("{ret_val:?}");

                    self.frame_pointer -= 1;

                    //                    println!("{ret_val:?} to {return_to:?}");

                    self.call_stack[ret_fp].registers.get_mut()[ret_reg] = ret_val;

                    self.call_stack.pop();

                    #[cfg(feature = "vm_debug")]
                    println!();

                    //                    self.call_stack[fp].registers.get_mut()[return_address] =
                    //                        ret_val;
                    //                    self.return_from_frame()
                    //                    self.call_stack[fp].registers
                }
                OpCode::GetGlobal(dst, src) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: GetGlobal: ");

                    let dst = dst.get_reg();
                    let name = self.opnd(*src).get_literal()?.get_symbol()?;
                    let val = self.globals[&name];

                    self.call_stack[fp].registers.get_mut()[dst] = val;

                    #[cfg(feature = "vm_debug")]
                    println!("dst: {dst:?} src: {src:?}");

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::SetGlobal(dst, src) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: SetGlobal: ");

                    let name = self.opnd(*dst).get_literal()?.get_symbol()?;
                    let val = self.opnd(*src);

                    #[cfg(feature = "vm_debug")]
                    println!("{name:?} : {val:?} ");

                    self.globals.insert(name, val);

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                OpCode::Print(src) => {
                    #[cfg(feature = "vm_debug")]
                    print!("{fp}: {ip}: Print: ");

                    self.print_val(*src);

                    self.call_stack[self.frame_pointer].ip += 1;
                }
                _ => todo!("{op:?}"),
            }
        }
        Ok(())
    }
}
