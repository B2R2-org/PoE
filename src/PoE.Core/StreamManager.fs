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

/// Interface for managing streams. Note that we name it as "manager" but it
/// behaves like a stream itself, i.e., it is also IStreamOperatable and can be
/// operatable as well.
type IStreamManager =
  inherit IStreamOperatable

  abstract Pid: int option
  abstract OnCreate: unit -> unit
  abstract OnClose: unit -> unit

and IStreamOperatable =
  abstract Read: int -> int option -> byte [] option
  abstract ReadErr: int -> int option -> byte [] option
  abstract ReadUntil: byte [] -> int option -> byte [] option
  abstract ReadAll: int option -> byte [] option
  abstract Write: int -> byte [] -> bool
