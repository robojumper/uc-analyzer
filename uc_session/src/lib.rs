use std::path::{Path, PathBuf};

use uc_files::{Files, X2FileDomain, X2FileId, XCom2SetupFiles};

pub fn example_session() {
    let src_orig = PathBuf::from(
        r"d:\Steam\SteamApps\common\XCOM 2 War of the Chosen SDK\Development\SrcOrig\",
    );
    let curr_mod = PathBuf::from(concat!(
        env!("USERPROFILE"),
        r"\Documents\FiraxisModBuddy\XCOM\CovertInfiltration\"
    ));
    let mut session = XCom2SetupFiles::new(&src_orig, &curr_mod);

    let globals = PathBuf::from(r"Core\Globals.uci");
    let globals = session
        .load_file(&globals, Some(X2FileDomain::SOURCE_SRCORIG))
        .unwrap();
}
