use std::{
    fmt, io,
    num::NonZeroU32,
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct FileNum(NonZeroU32);

pub trait FileId: Copy + fmt::Debug + 'static {
    type Domain: Copy + fmt::Debug + 'static;

    fn get_domain(&self) -> Self::Domain;
}

impl FileId for FileNum {
    type Domain = ();

    fn get_domain(&self) -> Self::Domain {}
}

pub trait Files {
    type FileId: FileId;

    fn load_file(
        &mut self,
        path: &Path,
        domain: Option<<Self::FileId as FileId>::Domain>,
    ) -> io::Result<Self::FileId>;
    fn get_file(&self, id: Self::FileId) -> Arc<str>;
}

/// A directory containing a bunch of UnrealScript package sources.
/// This could be the SrcOrig/Src folders of an SDK, the Src folder
/// of the current mod, or the Src folders of dependency mods.
#[derive(Debug)]
struct WorkspaceFiles {
    path: PathBuf,
}

impl WorkspaceFiles {
    fn new(path: &Path) -> Self {
        todo!()
    }
}

impl Files for WorkspaceFiles {
    type FileId = FileNum;

    fn load_file(&mut self, path: &Path, domain: Option<()>) -> io::Result<Self::FileId> {
        todo!()
    }

    fn get_file(&self, id: Self::FileId) -> Arc<str> {
        todo!()
    }
}
