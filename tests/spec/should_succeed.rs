use wrapterm::Pass;

const SPEC_DIR: &'static str =
    concat!(env!("CARGO_MANIFEST_DIR"), "/tests/spec");

#[test] fn parse_stlc() {
    let path = format!("{}/syntax/stlc.blues", SPEC_DIR);
    let path = std::path::PathBuf::from(path);
    
    let mut errata = wrapterm::Errata::new();
    let mut loader = blues::ast::FileLoader::new();
    
    let res = loader.run(&mut errata, path);

    if res.is_err() {
        let file_table = loader.into_file_table();
        let errors = errata.format_all(&file_table);
        panic!("{}", errors);
    }
}

#[test] fn parse_colormap() {
    let path = format!("{}/basic/colormap.blues", SPEC_DIR);
    let path = std::path::PathBuf::from(path);
    
    let mut errata = wrapterm::Errata::new();
    let mut loader = blues::ast::FileLoader::new();
    
    let res = loader.run(&mut errata, path);

    if res.is_err() {
        let file_table = loader.into_file_table();
        let errors = errata.format_all(&file_table);
        panic!("{}", errors);
    }
}
