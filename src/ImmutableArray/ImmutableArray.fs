(*
Copyright (C) 2023  Florian Brinkmeyer

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

namespace System

type ImmutableArray<'Index, 'T when 'Index :> IComparable and 'Index : comparison> =
    abstract Item : 'Index -> Option<'T>
    abstract GetNext : 'Index -> Option<'T> -> ImmutableArray<'Index, 'T>
    abstract Keys : seq<'Index>
    abstract Values : seq<'T>
    abstract KeyValuePairs : seq<'Index * 'T>
    abstract Previous : Option<ImmutableArray<'Index, 'T>>