//! Unix specific traits that extend the traits in [`rsfs`].
//!
//! These traits are separate from `rsfs` traits to ensure users of these traits opt-in to Unix
//! specific functionality.
//!
//! # Examples
//!
//! This module allows checking and using filesystem modes:
//!
//! ```
//! use rsfs::*;
//! use rsfs::unix_ext::*;
//! # async fn foo() -> std::io::Result<()> {
//! let fs = rsfs::disk::FS;
//!
//! assert_eq!(fs.metadata("/").await?.permissions().mode(), 0o755);
//! # Ok(())
//! # }
//! ```
//!
//! We can also symlink files:
//!
//! ```
//! use rsfs::*;
//! use rsfs::unix_ext::*;
//! use rsfs::mem::FS;
//! # async fn foo() -> std::io::Result<()> {
//! let fs = FS::new();
//!
//! fs.symlink("a.txt", "b.txt").await?;
//! # Ok(())
//! # }
//! ```
//!
//! There are even more useful Unix extensions!
//!
//! [`rsfs`]: ../index.html

use std::io::Result;
use std::path::Path;

/// Unix specific [`rsfs::DirBuilder`] extensions.
///
/// [`rsfs::DirBuilder`]: ../trait.DirBuilder.html
pub trait DirBuilderExt {
    /// Sets the mode bits to create new directories with. This option defaults to 0o777.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// let fs = FS::new();
    ///
    /// let mut builder = fs.new_dirbuilder();
    /// builder.mode(0o755);
    /// ```
    fn mode(&mut self, mode: u32) -> &mut Self;
}

/// Unix specific [`rsfs::File`] extensions.
///
/// [`rsfs::File`]: ../trait.File.html
#[async_trait::async_trait]
pub trait FileExt {
    /// Reads a number of bytes starting from the given offset, returning the number of bytes read.
    ///
    /// The offset is relative to the start of the file and this read does not affect the file's
    /// current cursor position.
    ///
    /// Note that, similar to `File::read`, it is not an error to return with a short read.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// let mut file = fs.open_file("foo.txt").await?;
    /// let mut buffer = [0; 10];
    ///
    /// // read up to 10 bytes starting from offset 10 in the file
    /// file.read_at(&mut buffer[..], 10).await?;
    /// # Ok(())
    /// # }
    /// ```
    async fn read_at(&self, buf: &mut [u8], offset: u64) -> Result<usize>;
    /// Writes a number of bytes starting from the given offset, returning the number of bytes
    /// written.
    ///
    /// The offset is relative to the start of the file and this read does not affect the file's
    /// current cursor position.
    ///
    /// When writing beyond the end of a file, the file is zero extended to `offset`.
    ///
    /// Note that, similar to `File::write`, it is not an error to return with a short write.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// let mut file = fs.create_file("foo.txt").await?;
    ///
    /// // write starting from offset 10 in the file, potentially zero extending the start
    /// file.write_at(b"some bytes", 10).await?;
    /// # Ok(())
    /// # }
    /// ```
    async fn write_at(&self, buf: &[u8], offset: u64) -> Result<usize>;
}

/// Unix specific [`rsfs::OpenOptions`] extensions.
///
/// [`rsfs::OpenOptions`]: ../trait.OpenOptions.html
pub trait OpenOptionsExt {
    /// Sets the mode bits that a new file will be opened with.
    ///
    /// The default mode for new files is 0o666.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    ///
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// let mut options = fs.new_openopts();
    /// options.mode(0o600); // only owner can read/write
    /// let file = options.open("foo.txt").await?;
    /// # Ok(())
    /// # }
    /// ```
    fn mode(&mut self, mode: u32) -> &mut Self;
    /// Pass custom flags to the `flags` argument of `open`.
    ///
    /// The bits that define the access mode are masked out with `O_ACCMODE` to ensure they do not
    /// interfere with the access mode set by Rust options.
    ///
    /// `custom_flags` can only set flags, not remove flags set by Rust options. This option
    /// overwrites any previously set custom flags.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    ///
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    /// 
    /// let mut options = fs.new_openopts();
    /// options.write(true);
    /// if cfg!(unix) {
    ///     options.custom_flags(0x8000); // O_NOFOLLOW (use libc in real code)
    /// }
    /// let file = options.open("foo.txt").await?;
    /// # Ok(())
    /// # }
    /// ```
    fn custom_flags(&mut self, flags: i32) -> &mut Self;
}

/// Unix specific [`rsfs::Permissions`] extensions.
///
/// [`rsfs::Permissions`]: ../trait.Permissions.html
pub trait PermissionsExt {
    /// Returns the underlying Unix mode of these permissions.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// let file = fs.create_file("foo.txt").await?;
    /// let metadata = file.metadata().await?;
    /// let permissions = metadata.permissions();
    ///
    /// println!("permission: {:o}", permissions.mode());
    /// # Ok(())
    /// # }
    /// ```
    fn mode(&self) -> u32;
    /// Sets the underlying Unix mode for these permissions.
    ///
    /// This does not modify the filesystem. To modify the filesystem, use the filesystem's
    /// [`set_permissions`] function.
    ///
    /// [`set_permissions`]: ../trait.FS.html#tymethod.set_permissions
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// let file = fs.create_file("foo.txt").await?;
    /// let metadata = file.metadata().await?;
    /// let mut permissions = metadata.permissions();
    ///
    /// permissions.set_mode(0o644);
    /// assert_eq!(permissions.mode(), 0o644);
    /// # Ok(())
    /// # }
    /// ```
    fn set_mode(&mut self, mode: u32);
    /// Creates a new Permissions from the given Unix mode.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::Permissions;
    ///
    /// let permissions = Permissions::from_mode(0o644);
    /// assert_eq!(permissions.mode(), 0o644);
    /// ```
    fn from_mode(mode: u32) -> Self;
}

/// Unix specific [`rsfs::GenFS`] extensions.
///
/// [`rsfs::GenFS`]: ../trait.GenFS.html
#[async_trait::async_trait]
pub trait GenFSExt {
    /// Creates a new symbolic link on the filesystem.
    ///
    /// The `dst` path will be a symbolic link pointing to the `src` path.
    ///
    /// # Examples
    ///
    /// ```
    /// use rsfs::*;
    /// use rsfs::unix_ext::*;
    /// use rsfs::mem::FS;
    /// # async fn foo() -> std::io::Result<()> {
    /// let fs = FS::new();
    ///
    /// fs.symlink("a.txt", "b.txt").await?;
    /// # Ok(())
    /// # }
    /// ```
    async fn symlink<P: AsRef<Path> + Send, Q: AsRef<Path> + Send>(&self, src: P, dst: Q) -> Result<()>;
}
