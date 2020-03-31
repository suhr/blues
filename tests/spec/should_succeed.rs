// use wrapterm::Pass;

const SPEC_DIR: &'static str =
    concat!(env!("CARGO_MANIFEST_DIR"), "/tests/spec");

#[test] fn parse_stlc() {
    let path = format!("{}/syntax/stlc.blues", SPEC_DIR);
    let path = std::path::PathBuf::from(path);
    
    let mut errata = wrapterm::Errata::new();
    let mut file_table = wrapterm::FileTable::new();
    let mut loader = blues::parser::ModuleLoader::new(&mut file_table, &mut errata);
    
    let res = loader.load_file(path);

    if res.is_err() {
        let errors = errata.format_all(&file_table);
        panic!("{}", errors);
    }
}

#[test] fn parse_lists() {
    let path = format!("{}/basic/lists.blues", SPEC_DIR);
    let path = std::path::PathBuf::from(path);
    
    let mut errata = wrapterm::Errata::new();
    let mut file_table = wrapterm::FileTable::new();
    let mut loader = blues::parser::ModuleLoader::new(&mut file_table, &mut errata);
    
    let res = loader.load_file(path);

    if res.is_err() {
        let errors = errata.format_all(&file_table);
        panic!("{}", errors);
    }
}

#[test] fn parse_colormap() {
    let path = format!("{}/basic/colormap.blues", SPEC_DIR);
    let path = std::path::PathBuf::from(path);
    
    let mut errata = wrapterm::Errata::new();
    let mut file_table = wrapterm::FileTable::new();
    let mut loader = blues::parser::ModuleLoader::new(&mut file_table, &mut errata);
    
    let res = loader.load_file(path);

    if res.is_err() {
        let errors = errata.format_all(&file_table);
        panic!("{}", errors);
    }
}
