--------------------------------------------------------------------------------
--  Copyright (c) 2011, Felix Krause
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED.
--  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
--  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
