use std::{
    fmt, io,
    num::NonZeroU32,
    ops::ControlFlow,
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

// The following representation keeps our `X2FileId`s small, which is
// important because we will store a file ID in every span. Currently
// this ensures that an X2FileId occupies 8 bytes.

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct X2FileDomain(u32);

impl X2FileDomain {
    pub const SOURCE_SRCORIG: X2FileDomain = X2FileDomain(0);
    pub const SOURCE_MODSRC: X2FileDomain = X2FileDomain(1);
}

const DEP_OFFSET: u32 = 2;

impl FileId for X2FileId {
    type Domain = X2FileDomain;

    fn get_domain(&self) -> Self::Domain {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct X2FileId(X2FileDomain, FileNum);

/// A directory containing a bunch of UnrealScript package sources.
/// This could be the SrcOrig/Src folders of an SDK, the Src folder
/// of the current mod, or the Src folders of dependency mods.
#[derive(Debug)]
pub struct XCom2SetupFiles {
    src_orig: WorkspaceFiles,
    mod_dependencies: Vec<WorkspaceFiles>,
    curr_mod: WorkspaceFiles,
}

fn resolve_with<F: FnOnce(FileNum) -> X2FileId>(
    w: &mut WorkspaceFiles,
    path: &Path,
    f: F,
) -> ControlFlow<io::Result<X2FileId>, ()> {
    match w.load_file(path, None) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => ControlFlow::Continue(()),
        res => ControlFlow::Break(res.map(|num| f(num))),
    }
}

impl XCom2SetupFiles {
    pub fn new(src_orig: &Path, curr_mod: &Path) -> Self {
        Self {
            src_orig: WorkspaceFiles::new(src_orig),
            mod_dependencies: vec![],
            curr_mod: WorkspaceFiles::new(curr_mod),
        }
    }

    pub fn add_dep(&mut self, dep: &Path) -> X2FileDomain {
        let idx = self.mod_dependencies.len();
        if idx as u32 == u32::MAX - DEP_OFFSET {
            panic!("too many files");
        }
        self.mod_dependencies.push(WorkspaceFiles::new(dep));
        X2FileDomain(idx as u32 + DEP_OFFSET)
    }

    fn get_workspace(&self, id: X2FileDomain) -> &WorkspaceFiles {
        match id {
            X2FileDomain::SOURCE_SRCORIG => &self.src_orig,
            X2FileDomain::SOURCE_MODSRC => &self.curr_mod,
            idx => &self.mod_dependencies[(idx.0 - DEP_OFFSET) as usize],
        }
    }

    /*fn get_workspace_mut(&mut self, id: u32) -> &mut WorkspaceFiles {
        match id {
            SOURCE_SRCORIG => &mut self.src_orig,
            SOURCE_MODSRC => &mut self.curr_mod,
            idx => &mut self.mod_dependencies[(idx - DEP_OFFSET) as usize],
        }
    }*/

    fn load_file_internal(
        &mut self,
        path: &Path,
        domain: Option<X2FileDomain>,
    ) -> ControlFlow<io::Result<X2FileId>, ()> {
        if let Some(id) = domain {
            match id {
                X2FileDomain::SOURCE_SRCORIG => resolve_with(&mut self.src_orig, path, |num| {
                    X2FileId(X2FileDomain::SOURCE_SRCORIG, num)
                })?,
                X2FileDomain::SOURCE_MODSRC => resolve_with(&mut self.curr_mod, path, |num| {
                    X2FileId(X2FileDomain::SOURCE_MODSRC, num)
                })?,
                idx => resolve_with(
                    &mut self.mod_dependencies[(idx.0 - DEP_OFFSET) as usize],
                    path,
                    |num| X2FileId(idx, num),
                )?,
            }
        }

        resolve_with(&mut self.curr_mod, path, |num| {
            X2FileId(X2FileDomain::SOURCE_MODSRC, num)
        })?;
        for (idx, dir) in self.mod_dependencies.iter_mut().enumerate().rev() {
            resolve_with(dir, path, |num| {
                X2FileId(X2FileDomain(idx as u32 + DEP_OFFSET), num)
            })?;
        }
        resolve_with(&mut self.src_orig, path, |num| {
            X2FileId(X2FileDomain::SOURCE_SRCORIG, num)
        })?;
        ControlFlow::Break(Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("couldn't resolve path {}", path.display()),
        )))
    }
}

impl Files for XCom2SetupFiles {
    type FileId = X2FileId;

    fn load_file(&mut self, path: &Path, domain: Option<X2FileDomain>) -> io::Result<Self::FileId> {
        match self.load_file_internal(path, domain) {
            ControlFlow::Break(val) => val,
            ControlFlow::Continue(_) => unreachable!(),
        }
    }

    fn get_file(&self, id: Self::FileId) -> Arc<str> {
        self.get_workspace(id.0).get_file(id.1)
    }
}
