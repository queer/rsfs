//! A zero cost wrapper around [`std::fs`].
//!
//! The [`FS`] struct is an empty struct. All methods on it use `std::fs` functions. The intent of
//! this module is to set the filesystem you use to `rsfs::disk::FS` in `main.rs` and to set the
//! filesystem to `rsfs::mem::test::FS` (once it exists) in your tests.
//!
//! [`std::fs`]: https://doc.rust-lang.org/std/fs/
//! [`FS`]: struct.FS.html
//!
//! # Examples
//!
//! ```
//! # async fn foo() -> std::io::Result<()> {
//! use rsfs::*;
//! use rsfs::unix_ext::*;
//!
//! let fs = rsfs::disk::FS;
//!
//! let meta = fs.metadata("/").await?;
//! assert!(meta.is_dir());
//! assert_eq!(meta.permissions().mode(), 0o755);
//! # Ok(())
//! # }
//! ```

use std::ffi::OsString;
use std::io::{Result, SeekFrom};
use std::os::unix::fs::FileExt;
use std::os::unix::prelude::{PermissionsExt, MetadataExt};
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::time::SystemTime;

use pin_utils::unsafe_pinned;
use tokio::fs as rs_fs;
use tokio::io::{AsyncRead, AsyncSeek, AsyncWrite};
use tokio_stream::Stream;

use crate::fs;

#[cfg(unix)]
use crate::unix_ext;

/// A builder used to create directories in various manners.
///
/// This builder is a single element tuple containing a [`std::fs::DirBuilder`] that implements [`rsfs::DirBuilder`] and supports [unix extensions].
///
/// [`std::fs::DirBuilder`]: https://doc.rust-lang.org/std/fs/struct.DirBuilder.html
/// [`rsfs::DirBuilder`]: ../trait.DirBuilder.html
/// [unix extensions]: ../unix_ext/trait.DirBuilderExt.html
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// let fs = rsfs::disk::FS;
/// let db = fs.new_dirbuilder();
/// db.create("dir").await?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct DirBuilder(rs_fs::DirBuilder);

#[async_trait::async_trait]
impl fs::DirBuilder for DirBuilder {
    fn recursive(&mut self, recursive: bool) -> &mut Self {
        self.0.recursive(recursive);
        self
    }
    async fn create<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        self.0.create(path).await
    }
}

#[cfg(unix)]
impl unix_ext::DirBuilderExt for DirBuilder {
    fn mode(&mut self, mode: u32) -> &mut Self {
        self.0.mode(mode);
        self
    }
}

/// Entries returned by the [`ReadDir`] iterator.
///
/// An instance of `DirEntry` implements [`rsfs::DirEntry`] and represents an entry inside a
/// directory on the in-memory filesystem. This struct is a single element tuple containing a
/// [`std::fs::DirEntry`].
///
/// [`ReadDir`]: struct.ReadDir.html
/// [`rsfs::DirEntry`]: ../trait.DirEntry.html
/// [`std::fs::DirEntry`]: https://doc.rust-lang.org/std/fs/struct.DirEntry.html
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// use tokio_stream::StreamExt;
///
/// let fs = rsfs::disk::FS;
/// let mut read_dir = fs.read_dir(".").await?;
///
/// while let Some(entry) = read_dir.next().await {
///     let entry = entry?;
///     if let Some(entry) = entry {
///         println!("{:?}: {:?}", entry.path(), entry.metadata().await?.permissions());
///     }
/// }
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct DirEntry(rs_fs::DirEntry);

#[async_trait::async_trait]
impl fs::DirEntry for DirEntry {
    type Metadata = Metadata;
    type FileType = FileType;

    fn path(&self) -> PathBuf {
        self.0.path()
    }
    async fn metadata(&self) -> Result<Self::Metadata> {
        self.0.metadata().await.map(Metadata)
    }
    async fn file_type(&self) -> Result<Self::FileType> {
        self.0.file_type().await.map(FileType)
    }
    fn file_name(&self) -> OsString {
        self.0.file_name()
    }
}

/// Returned from [`Metadata::file_type`], this structure represents the type of a file.
///
/// This structure is a single element tuple containing a [`std::fs::FileType`] that implements [`rsfs::FileType`].
///
/// [`Metadata::file_type`]: ../trait.Metadata.html#tymethod.file_type
/// [`std::fs::FileType`]: https://doc.rust-lang.org/std/fs/struct.FileType.html
/// [`rsfs::FileType`]: ../trait.FileType.html
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// let fs = rsfs::disk::FS;
/// let f = fs.create_file("f").await?;
/// assert!(fs.metadata("f").await?.file_type().is_file());
/// # Ok(())
/// # }
/// ```
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FileType(std::fs::FileType);

impl fs::FileType for FileType {
    fn is_dir(&self) -> bool {
        self.0.is_dir()
    }
    fn is_file(&self) -> bool {
        self.0.is_file()
    }
    fn is_symlink(&self) -> bool {
        self.0.is_symlink()
    }
}

/// A view into a file on the filesystem.
///
/// An instance of `File` can be read or written to depending on the options it was opened with.
/// Files also implement `Seek` to alter the logical cursor position of the internal file.
///
/// This struct is a single element tuple containing a [`std::fs::File`] that implements
/// [`rsfs::File`] and has [unix extensions].
///
/// [`std::fs::File`]: https://doc.rust-lang.org/std/fs/struct.File.html
/// [`rsfs::File`]: ../trait.File.html
/// [unix extensions]: ../unix_ext/trait.FileExt.html
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # use tokio::io::AsyncWriteExt;
/// # async fn foo() -> std::io::Result<()> {
/// let fs = rsfs::disk::FS;
/// let mut f = fs.create_file("f").await?;
/// assert_eq!(f.write(&[1, 2, 3]).await?, 3);
/// # Ok(())
/// # }
/// ```
#[repr(transparent)]
#[derive(Debug)]
pub struct File {
    file: rs_fs::File,
}

impl File {
    unsafe_pinned!(file: rs_fs::File);
}

#[async_trait::async_trait]
impl fs::File for File {
    type Metadata = Metadata;
    type Permissions = Permissions;

    async fn sync_all(&self) -> Result<()> {
        self.file.sync_all().await
    }
    async fn sync_data(&self) -> Result<()> {
        self.file.sync_data().await
    }
    async fn set_len(&self, size: u64) -> Result<()> {
        self.file.set_len(size).await
    }
    async fn metadata(&self) -> Result<Self::Metadata> {
        self.file.metadata().await.map(Metadata)
    }
    async fn try_clone(&self) -> Result<Self> {
        self.file.try_clone().await.map(|file| File { file })
    }
    async fn set_permissions(&self, perm: Self::Permissions) -> Result<()> {
        self.file.set_permissions(perm.0).await
    }
}

impl AsyncRead for File {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.poll_read(cx, buf)
    }
}
impl AsyncWrite for File {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::result::Result<usize, std::io::Error>> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.poll_write(cx, buf)
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::result::Result<(), std::io::Error>> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.poll_flush(cx)
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::result::Result<(), std::io::Error>> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.poll_shutdown(cx)
    }
}
impl AsyncSeek for File {
    fn start_seek(self: std::pin::Pin<&mut Self>, position: SeekFrom) -> std::io::Result<()> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.start_seek(position)
    }

    fn poll_complete(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<u64>> {
        let file: Pin<&mut rs_fs::File> = self.file();
        file.poll_complete(cx)
    }
}

// TODO: Figure out how to implement this right
// impl<'a> AsyncRead for &'a File {
//     fn poll_read(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//         buf: &mut tokio::io::ReadBuf<'_>,
//     ) -> std::task::Poll<std::io::Result<()>> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.poll_read(cx, buf)
//     }
// }
// impl<'a> AsyncWrite for &'a File {
//     fn poll_write(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//         buf: &[u8],
//     ) -> std::task::Poll<std::result::Result<usize, std::io::Error>> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.poll_write(cx, buf)
//     }

//     fn poll_flush(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<std::result::Result<(), std::io::Error>> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.poll_flush(cx)
//     }

//     fn poll_shutdown(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<std::result::Result<(), std::io::Error>> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.poll_shutdown(cx)
//     }
// }
// impl<'a> AsyncSeek for &'a File {
//     fn start_seek(self: std::pin::Pin<&mut Self>, position: SeekFrom) -> std::io::Result<()> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.start_seek(position)
//     }

//     fn poll_complete(
//         self: std::pin::Pin<&mut Self>,
//         cx: &mut std::task::Context<'_>,
//     ) -> std::task::Poll<std::io::Result<u64>> {
//         let file: Pin<&mut rs_fs::File> = self.file();
//         file.poll_complete(cx)
//     }
// }

#[async_trait::async_trait]
impl unix_ext::FileExt for File {
    async fn read_at(&self, buf: &mut [u8], offset: u64) -> Result<usize> {
        let file = self.file.try_clone().await?.into_std().await;
        let join_handle = tokio::task::spawn_blocking(move || {
            let mut buf = vec![];
            let res = file.read_at(&mut buf, offset);
            (res, buf)
        });
        let (res, out_buf) = join_handle.await?;
        buf.copy_from_slice(&out_buf);
        res
    }
    async fn write_at(&self, buf: &[u8], offset: u64) -> Result<usize> {
        let file = self.file.try_clone().await?.into_std().await;
        let in_buf = buf.to_vec();
        let join_handle = tokio::task::spawn_blocking(move || file.write_at(&in_buf, offset));
        join_handle.await?
    }
}

/// Metadata information about a file.
///
/// This structure, a single element tuple containing a [`std::fs::Metadata`] that implements
/// [`rsfs::Metadata`], is returned from the [`metadata`] or [`symlink_metadata`] methods and
/// represents known metadata information about a file at the instant in time this structure is
/// instantiated.
///
/// [`std::fs::Metadata`]: https://doc.rust-lang.org/std/fs/struct.Metadata.html
/// [`rsfs::Metadata`]: ../trait.Metadata.html
/// [`metadata`]: ../trait.GenFS.html#tymethod.metadata
/// [`symlink_metadata`]: ../trait.GenFS.html#tymethod.symlink_metadata
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// let fs = rsfs::disk::FS;
/// fs.create_file("f").await?;
/// println!("{:?}", fs.metadata("f").await?);
/// # Ok(())
/// # }
#[derive(Clone, Debug)]
pub struct Metadata(std::fs::Metadata);

impl fs::Metadata for Metadata {
    type Permissions = Permissions;
    type FileType = FileType;

    fn file_type(&self) -> Self::FileType {
        FileType(self.0.file_type())
    }
    fn is_dir(&self) -> bool {
        self.0.is_dir()
    }
    fn is_file(&self) -> bool {
        self.0.is_file()
    }
    fn len(&self) -> u64 {
        self.0.len()
    }
    fn permissions(&self) -> Self::Permissions {
        Permissions(self.0.permissions())
    }
    fn modified(&self) -> Result<SystemTime> {
        self.0.modified()
    }
    fn accessed(&self) -> Result<SystemTime> {
        self.0.accessed()
    }
    fn created(&self) -> Result<SystemTime> {
        self.0.created()
    }
    fn uid(&self) -> Result<u32> {
        Ok(self.0.uid())
    }
    fn gid(&self) -> Result<u32> {
        Ok(self.0.gid())
    }
}

/// Options and flags which can be used to configure how a file is opened.
///
/// This builder, created from `GenFS`s [`new_openopts`], exposes the ability to configure how a
/// [`File`] is opened and what operations are permitted on the open file. `GenFS`s [`open_file`]
/// and [`create_file`] methods are aliases for commonly used options with this builder.
///
/// This builder is a single element tuple containing a [`std::fs::OpenOptions`] that implements
/// [`rsfs::OpenOptions`] and supports [unix extensions].
///
/// [`new_openopts`]: ../trait.GenFS.html#tymethod.new_openopts
/// [`open_file`]: ../trait.GenFS.html#tymethod.open_file
/// [`create_file`]: ../trait.GenFS.html#tymethod.create_file
/// [`std::fs::OpenOptions`]: https://doc.rust-lang.org/std/fs/struct.OpenOptions.html
/// [`rsfs::OpenOptions`]: ../trait.OpenOptions.html
/// [unix extensions]: ../unix_ext/trait.OpenOptionsExt.html
///
/// # Examples
///
/// Opening a file to read:
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// # let fs = rsfs::disk::FS;
/// let f = fs.new_openopts()
///           .read(true)
///           .open("f")
///           .await?;
/// # Ok(())
/// # }
/// ```
///
/// Opening a file for both reading and writing, as well as creating it if it doesn't exist:
///
/// ```
/// # use rsfs::*;
/// # async fn foo() -> std::io::Result<()> {
/// # let fs = rsfs::disk::FS;
/// let mut f = fs.new_openopts()
///               .read(true)
///               .write(true)
///               .create(true)
///               .open("f")
///               .await?;
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug)]
pub struct OpenOptions(rs_fs::OpenOptions);

#[async_trait::async_trait]
impl fs::OpenOptions for OpenOptions {
    type File = File;

    fn read(&mut self, read: bool) -> &mut Self {
        self.0.read(read);
        self
    }
    fn write(&mut self, write: bool) -> &mut Self {
        self.0.write(write);
        self
    }
    fn append(&mut self, append: bool) -> &mut Self {
        self.0.append(append);
        self
    }
    fn truncate(&mut self, truncate: bool) -> &mut Self {
        self.0.truncate(truncate);
        self
    }
    fn create(&mut self, create: bool) -> &mut Self {
        self.0.create(create);
        self
    }
    fn create_new(&mut self, create_new: bool) -> &mut Self {
        self.0.create_new(create_new);
        self
    }
    async fn open<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::File> {
        self.0.open(path).await.map(|file| File { file })
    }
}

#[cfg(unix)]
impl unix_ext::OpenOptionsExt for OpenOptions {
    fn mode(&mut self, mode: u32) -> &mut Self {
        self.0.mode(mode);
        self
    }
    fn custom_flags(&mut self, flags: i32) -> &mut Self {
        self.0.custom_flags(flags);
        self
    }
}

/// Representation of the various permissions on a file.
///
/// This struct is a single element tuple containing a [`std::fs::Permissions`] that implements
/// [`rsfs::Permissions`] and has [unix extensions].
///
/// [`std::fs::Permissions`]: https://doc.rust-lang.org/std/fs/struct.Permissions.html
/// [`rsfs::Permissions`]: ../trait.Permissions.html
/// [unix extensions]: ../unix_ext/trait.PermissionsExt.html
///
/// # Examples
///
/// ```
/// # use rsfs::*;
/// # use rsfs::mem::FS;
/// use rsfs::unix_ext::*;
/// use rsfs::mem::Permissions;
/// # async fn foo() -> std::io::Result<()> {
/// # let fs = FS::new();
/// # fs.create_file("foo.txt").await?;
///
/// fs.set_permissions("foo.txt", Permissions::from_mode(0o400)).await?;
/// # Ok(())
/// # }
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Permissions(std::fs::Permissions);

impl fs::Permissions for Permissions {
    fn readonly(&self) -> bool {
        self.0.readonly()
    }
    fn set_readonly(&mut self, readonly: bool) {
        self.0.set_readonly(readonly)
    }
}

#[cfg(unix)]
impl unix_ext::PermissionsExt for Permissions {
    fn mode(&self) -> u32 {
        self.0.mode()
    }
    fn set_mode(&mut self, mode: u32) {
        self.0.set_mode(mode)
    }
    fn from_mode(mode: u32) -> Self {
        Permissions(std::fs::Permissions::from_mode(mode))
    }
}

/// Iterator over entries in a directory.
///
/// This is returned from the [`read_dir`] method of `GenFS` and yields instances of
/// `io::Result<DirEntry>`. Through a [`DirEntry`], information about contents of a directory can
/// be learned.
///
/// This struct is as ingle element tuple containing a [`std::fs::ReadDir`].
///
/// [`read_dir`]: struct.FS.html#method.read_dir
/// [`DirEntry`]: struct.DirEntry.html
/// [`std::fs::ReadDir`]: https://doc.rust-lang.org/std/fs/struct.ReadDir.html
#[derive(Debug)]
#[repr(transparent)]
pub struct ReadDir {
    read_dir: rs_fs::ReadDir,
}

impl ReadDir {
    unsafe_pinned!(read_dir: rs_fs::ReadDir);
}

impl Stream for ReadDir {
    type Item = std::result::Result<Option<DirEntry>, std::io::Error>;

    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        let mut read_dir = self.read_dir();
        read_dir
            .poll_next_entry(cx)
            .map(|r| Some(r.map(|o| o.map(DirEntry))))
    }
}

/// An empty struct that satisfies [`rsfs::FS`] by calling [`std::fs`] functions.
///
/// Because this is an empty struct, it is inherently thread safe and copyable. The power of using
/// `rsfs` comes from the ability to choose what filesystem you want to use where: your main can
/// use a disk backed filesystem, but your tests can use a test filesystem with injected errors.
///
/// Alternatively, the in-memory filesystem could suit your needs without forcing you to use disk.
///
/// [`rsfs::FS`]: ../trait.FS.html
/// [`std::fs`]: https://doc.rust-lang.org/std/fs/
///
/// # Examples
///
/// ```
/// use rsfs::*;
///
/// let fs = rsfs::disk::FS;
/// ```
#[derive(Copy, Clone, Debug)]
pub struct FS;

#[async_trait::async_trait]
impl fs::GenFS for FS {
    type DirBuilder = DirBuilder;
    type DirEntry = DirEntry;
    type File = File;
    type Metadata = Metadata;
    type OpenOptions = OpenOptions;
    type Permissions = Permissions;
    type ReadDir = ReadDir;

    async fn canonicalize<P: AsRef<Path> + Send>(&self, path: P) -> Result<PathBuf> {
        rs_fs::canonicalize(path).await
    }
    async fn copy<P: AsRef<Path> + Send, Q: AsRef<Path> + Send>(
        &self,
        from: P,
        to: Q,
    ) -> Result<u64> {
        rs_fs::copy(from, to).await
    }
    async fn create_dir<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        rs_fs::create_dir(path).await
    }
    async fn create_dir_all<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        rs_fs::create_dir_all(path).await
    }
    async fn hard_link<P: AsRef<Path> + Send, Q: AsRef<Path> + Send>(
        &self,
        src: P,
        dst: Q,
    ) -> Result<()> {
        rs_fs::hard_link(src, dst).await
    }
    async fn metadata<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::Metadata> {
        rs_fs::metadata(path).await.map(Metadata)
    }
    async fn read_dir<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::ReadDir> {
        rs_fs::read_dir(path)
            .await
            .map(|read_dir| ReadDir { read_dir })
    }
    async fn read_link<P: AsRef<Path> + Send>(&self, path: P) -> Result<PathBuf> {
        rs_fs::read_link(path).await
    }
    async fn remove_dir<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        rs_fs::remove_dir(path).await
    }
    async fn remove_dir_all<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        rs_fs::remove_dir_all(path).await
    }
    async fn remove_file<P: AsRef<Path> + Send>(&self, path: P) -> Result<()> {
        rs_fs::remove_file(path).await
    }
    async fn rename<P: AsRef<Path> + Send, Q: AsRef<Path> + Send>(
        &self,
        from: P,
        to: Q,
    ) -> Result<()> {
        rs_fs::rename(from, to).await
    }
    async fn set_permissions<P: AsRef<Path> + Send>(
        &self,
        path: P,
        perm: Self::Permissions,
    ) -> Result<()> {
        rs_fs::set_permissions(path, perm.0).await
    }
    async fn symlink_metadata<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::Metadata> {
        rs_fs::symlink_metadata(path).await.map(Metadata)
    }
    fn new_openopts(&self) -> Self::OpenOptions {
        OpenOptions(rs_fs::OpenOptions::new())
    }
    fn new_dirbuilder(&self) -> Self::DirBuilder {
        DirBuilder(rs_fs::DirBuilder::new())
    }
    async fn open_file<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::File> {
        rs_fs::File::open(path).await.map(|file| File { file })
    }
    async fn create_file<P: AsRef<Path> + Send>(&self, path: P) -> Result<Self::File> {
        rs_fs::File::create(path).await.map(|file| File { file })
    }
}

#[cfg(unix)]
#[async_trait::async_trait]
impl unix_ext::GenFSExt for FS {
    async fn symlink<P: AsRef<Path> + Send, Q: AsRef<Path> + Send>(
        &self,
        src: P,
        dst: Q,
    ) -> Result<()> {
        tokio::fs::symlink(src, dst).await
    }
}
