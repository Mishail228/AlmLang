use alm_box::ast::Value;
use alm_box::{
    lexer::lex,
    parser::Parser,
    vm::VM,
    compiler::Compiler
};
use std::fs::File;
use std::env;
use std::io::Read;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let file_path = match args.get(1) {
        Some(path) => path.clone(),
        None => {
            eprintln!("You need to choose which file you want to compile");
            return;
        }
    };
    let mut file = File::open(file_path).unwrap();
    let mut code = String::new();
    file.read_to_string(&mut code).unwrap();
    let res = compile(code);
    println!("Result: {:?}", res);
}

fn compile(code: String) -> Value {
    let lexed = lex(&code).unwrap();
    let mut parser = Parser::new(lexed);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let functions =  compiler.compile_ast(ast).unwrap();
    let mut vm = VM::new();
    for func in functions {
        vm.add_function(func);
    }

    let res = vm.run("main".to_string()).unwrap();
    return res;
}