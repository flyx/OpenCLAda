--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with CL.Contexts;

package CL.Samplers is
   type Sampler is new Runtime_Object with null record;

   type Addressing_Mode is (None, Clamp_To_Edge, Clamp, Repeat,
                            Mirrored_Repeat);


   type Filter_Mode is (Nearest, Linear);

   package Constructors is

      function Create (Context           : Contexts.Context'Class;
                       Normalized_Coords : Boolean;
                       Addressing        : Addressing_Mode;
                       Filter            : Filter_Mode) return Sampler;
   end Constructors;

   overriding procedure Adjust (Object : in out Sampler);

   overriding procedure Finalize (Object : in out Sampler);

   function Reference_Count (Source : Sampler) return UInt;

   function Context (Source : Sampler) return Contexts.Context;

   function Addressing (Source : Sampler) return Addressing_Mode;

   function Filter (Source : Sampler) return Filter_Mode;

   function Are_Coords_Normalized (Source : Sampler) return Boolean;
private
   for Addressing_Mode use (None            => 16#1130#,
                            Clamp_To_Edge   => 16#1131#,
                            Clamp           => 16#1132#,
                            Repeat          => 16#1133#,
                            Mirrored_Repeat => 16#1134#);
   for Addressing_Mode'Size use UInt'Size;

   for Filter_Mode use (Nearest => 16#1140#,
                        Linear  => 16#1141#);
   for Filter_Mode'Size use UInt'Size;
end CL.Samplers;
