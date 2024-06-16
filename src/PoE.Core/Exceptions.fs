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

/// Line number annotation.
type Annotation = int option

/// Exceptions which can happen while parsing a PoE.
exception ParsingException of msg: string * Annotation

/// A type error.
exception DecodingException of info: string
  with override this.Message = this.info

/// An encoding error.
exception EncodingException of string

/// Exceptions which can occur while evaluating a PoE.
exception RuntimeException of info: string
  with override this.Message = this.info

module Exception =
  let err exn annot msg =
    match annot with
    | None -> raise (exn msg)
    | Some (line) ->
      let line = line.ToString ()
      let pos = " @ (Line " + line + ")"
      raise (exn (msg + pos))
