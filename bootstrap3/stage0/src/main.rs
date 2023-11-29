use std::{env, fs, path::Path, process::Command};

use stage0::{ast, codegen, lexer, transform_ast};

struct MultiContainer<T, U> {
    first: T,
    second: U,
}

trait ComplexTrait<T, U> {
    fn get_first(&self) -> &T;
    fn get_second(&self) -> &U;
}

impl<T, U> ComplexTrait<T, U> for MultiContainer<T, U> {
    fn get_first(&self) -> &T {
        &self.first
    }
    fn get_second(&self) -> &U {
        &self.second
    }
}

trait Doubler<T> {
    fn double(&self) -> Self;
}

struct Value {
    value: i32,
}

impl Doubler<Value> for Value {
    fn double(&self) -> Self {
        Value {
            value: self.value * 2,
        }
    }
}

fn main() -> Result<(), String> {
    unsafe {
        backtrace_on_stack_overflow::enable();
    }
    let args: Vec<String> = env::args().collect();
    let file_name = Box::leak(args[1].clone().into_boxed_str());
    // We just leak the string, because it makes our life easier.
    let src = Box::leak(fs::read_to_string(&file_name).unwrap().into_boxed_str());
    let tokens = lexer::lexer(file_name, src).unwrap();
    let mut module = ast::parse(tokens)?;
    println!("{:#?}", &module);
    transform_ast::transform(&mut module);
    println!("{:#?}", &module);
    let code = codegen::generate_code(module)?;
    println!("{}", code);
    compile(file_name, code)?;
    Ok(())
}

fn compile(file_name: &str, code: String) -> Result<(), String> {
    let base_name = Path::new(&file_name).file_stem().unwrap().to_str().unwrap();
    let ll_file = format!("build/{}.ll", base_name);
    let obj_file = format!("build/{}.o", base_name);
    let bin_file = format!("build/{}.bin", base_name);
    fs::write(&ll_file, code).unwrap();
    let output = Command::new("llc")
        .arg("-O=0")
        .arg("-opaque-pointers")
        .arg("-filetype=obj")
        .arg(&ll_file)
        .arg("-o")
        .arg(&obj_file)
        .output()
        .unwrap();
    println!(
        "{}: {} {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    if !output.status.success() {
        return Err("llc failed".to_string());
    }
    let output = Command::new("gcc")
        .arg(&obj_file)
        .arg("-o")
        .arg(&bin_file)
        .output()
        .unwrap();
    println!(
        "{}: {} {}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    if !output.status.success() {
        return Err("gcc failed".to_string());
    }
    Ok(())
}
