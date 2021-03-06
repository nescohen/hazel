# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Genrule for the Happy parser generator.

Example:
  load("//third_party/haskell/happy:build_defs.bzl", "genhappy")

  genhappy(
      src = "MyParser.y",
      out = "MyParser.y.hs"
  )

  haskell_binary(
      name = "MyParser",
      srcs = [ "MyParser.y.hs" ]
      ...
  )
"""

def genhappy(src, out):
  native.genrule(
      name = src + ".hs_happy",
      srcs = [src],
      outs = [out],
      tools = ["@haskell_happy//:happy_bin"],
      cmd = "$(location @haskell_happy//:happy_bin) -g -a -c -o $(OUTS) $(SRCS)",
  )
