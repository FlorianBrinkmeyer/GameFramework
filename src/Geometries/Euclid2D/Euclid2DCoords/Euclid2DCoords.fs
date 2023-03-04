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

namespace Euclid2D

open System

type Euclid2DCoords (tuple : int * int) =
    new (x, y) = Euclid2DCoords (x, y)
    member this.X = 
        match tuple with
        | x,y -> x
    member this.Y = 
        match tuple with
        | x,y -> y
    member this.AsTuple = tuple
    override this.Equals other =
        if other = null then
            false
        else    
            let otherTuple =
                match other with
                | :? Euclid2DCoords as coords ->
                    coords.AsTuple
                | :? (int * int) as tup -> 
                    tup
                | _ -> raise (Exception "Not a comparable type.")
            tuple.Equals otherTuple
    override this.GetHashCode () = tuple.GetHashCode ()
    override this.ToString () = sprintf " X : %A, Y: %A" this.X this.Y
    static member (+) (coords1 : Euclid2DCoords, coords2 : Euclid2DCoords) = Euclid2DCoords (coords1.X+coords2.X, coords1.Y+coords2.Y)
    static member (-) (coords1 : Euclid2DCoords, coords2 : Euclid2DCoords) = Euclid2DCoords (coords1.X-coords2.X, coords1.Y-coords2.Y)
    static member (*) (scalar : int, coords : Euclid2DCoords) = Euclid2DCoords (scalar * coords.X, scalar * coords.Y)
    static member (*) (coords : Euclid2DCoords, scalar : int) = Euclid2DCoords (scalar * coords.X, scalar * coords.Y)
    static member (~-) (coords : Euclid2DCoords) = (-1) * coords
    interface IComparable with
        member x.CompareTo other =
            if other = null then
                1
            else    
                let otherTuple =
                    match other with
                    | :? Euclid2DCoords as coords ->
                        coords.AsTuple
                    | :? (int * int) as tup -> tup
                    | _ -> raise (Exception "Not a comparable type.")
                (tuple :> IComparable).CompareTo otherTuple
    member this.NormalizeCompWise =
        let newX =
            if this.X > 0 then
                1
            elif this.X < 0 then
                -1
            else
                0                                       
        let newY =
            if this.Y > 0 then
                1
            elif this.Y < 0 then
                -1
            else
                0
        Euclid2DCoords (newX, newY)                                                             