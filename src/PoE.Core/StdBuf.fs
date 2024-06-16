(*

  Copyright (c) SoftSec Lab. @ KAIST, since 2016

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

 *)

namespace PoE

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices
open B2R2

module StdBuf =
  let private getOS () =
    if RuntimeInformation.IsOSPlatform OSPlatform.Windows then OS.Windows
    elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then OS.Linux
    elif RuntimeInformation.IsOSPlatform OSPlatform.OSX then OS.MacOSX
    else raise UnknownOSException

  let private getArch () =
    match RuntimeInformation.OSArchitecture with
    | Architecture.X86 -> Arch.IntelX86
    | Architecture.X64 -> Arch.IntelX64
    | Architecture.Arm64 -> Arch.AARCH64
    | _ -> Arch.UnknownISA

  let private getPreloadKey = function
    | OS.Linux -> "LD_PRELOAD"
    | OS.MacOSX -> "DYLD_INSERT_LIBRARIES"
    | _ -> raise UnknownOSException

  let private getLibraryPathKey = function
    | OS.Linux -> "LD_LIBRARY_PATH"
    | OS.MacOSX -> "DYLD_LIBRARY_PATH"
    | _ -> raise UnknownOSException

  let private getNameSeparator = function
    | OS.Linux -> " "
    | OS.MacOSX -> ":"
    | _ -> raise UnknownOSException
    
  let private getDirSeparator = function
    | OS.Linux | OS.MacOSX -> ":"
    | _ -> raise UnknownOSException

  let private getArchSuffix os arch =
    match arch with
    | _ when os = OS.MacOSX -> "macos-universal"
    | Arch.IntelX86 -> "linux-x86"
    | Arch.IntelX64 -> "linux-x64"
    | Arch.AARCH64 -> "linux-aarch64"
    | _ -> raise UnknownOSException
    
  let private setEnvironmentVariable key value sep (pInfo: ProcessStartInfo) =
    let currVar = Environment.GetEnvironmentVariable key
    let newVar = $"{currVar}{sep}{value}"
    pInfo.EnvironmentVariables[key] <- newVar
    pInfo

  let setPreloadPath (pInfo: ProcessStartInfo) =
    let os = getOS ()
    let arch = getArch ()
    match os, arch with
    | OS.Linux, _
    | OS.MacOSX, Arch.IntelX64
    | OS.MacOSX, Arch.AARCH64 ->
      let suffix = getArchSuffix os arch
      let libDir = Path.GetFullPath AppDomain.CurrentDomain.BaseDirectory
      let libName = $"stdbuf-{suffix}"
      let libPath = $"{libDir}/{libName}"
      if not <| File.Exists libPath then pInfo
      else 
        let nameKey = getPreloadKey os
        let nameSep = getNameSeparator os
        let dirKey = getLibraryPathKey os
        let dirSep = getDirSeparator os
        pInfo
        |> setEnvironmentVariable nameKey libName nameSep
        |> setEnvironmentVariable dirKey libDir dirSep
    | _ -> pInfo
