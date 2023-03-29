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

module ImmutableEnumerable2DArray
open System
open System.Collections
open Euclid2D
open Euclid2D.Enumerable2DArray

let twoDArrayToMap (array : 't [,]) =
    let dict = Generic.Dictionary<int*int, 't> ()
    array |> Array2D.iteri (fun x y value -> 
        if value <> null then
            dict[(x,y)] <- value
    )
    Seq.zip dict.Keys dict.Values |> Map.ofSeq

let seqOfSeqToMap (seqSeq : seq<seq<'t>>) =
    let dict = Generic.Dictionary<int*int, 't> ()
    seqSeq |> Seq.iteri (fun y line ->
        line |> Seq.iteri (fun x value ->
            if value <> null then
                dict[(x,y)] <- value
        )
    )
    Seq.zip dict.Keys dict.Values |> Map.ofSeq

let twoDArrayOptToMap (array : Option<'t> [,]) =
    let dict = Generic.Dictionary<int*int, 't> ()
    array |> Array2D.iteri (fun x y value -> value |> Option.iter (fun v -> dict[(x,y)] <-v))
    Seq.zip dict.Keys dict.Values |> Map.ofSeq

let seqOfSeqOptToMap (seqSeq : seq<seq<Option<'t>>>) =
    let dict = Generic.Dictionary<int*int, 't> ()
    seqSeq |> Seq.iteri (fun y line ->
        line |> Seq.iteri (fun x value ->
            value |> Option.iter (fun v -> dict[(x,y)] <-v)
        )
    )
    Seq.zip dict.Keys dict.Values |> Map.ofSeq

type ImmutableEnumerable2DArray<'t when 't : equality> (xdim, ydim, map : Map<int*int, 't>, ?emptyCoords : Set<int*int>, ?previous : ImmutableEnumerable2DArray<'t>) =
    member x.InternalMap = map
    override this.Equals other =
        let castedOther = other :?> ImmutableEnumerable2DArray<'t>
        map = castedOther.InternalMap
    override this.GetHashCode () = (map :> Object).GetHashCode ()    
    override this.ToString () =
        let longestElemReprLen = (map.Values |> Seq.map (fun value -> (value.ToString ()).Length) |> Seq.max) + 1
        let empty = String.replicate longestElemReprLen " "
        let getRow y =
            let row = (this :> IEnumerable2DArray<'t>).ToSeqInDir (0,y) (1,0) |> Seq.map (fun maybeValue ->
                match maybeValue with
                | Some value ->
                    let str = value.ToString ()
                    let rest = String.replicate (longestElemReprLen - str.Length) " "
                    str + rest
                | None ->
                    empty    
            )
            let linkedRow = row |> Seq.reduce (+)
            linkedRow + "\n"
        [0..ydim-1] |> List.map getRow |> List.reduce (+)          
    interface ImmutableArray<int*int, 't> with
        member x.Item coords = map.TryFind coords
        member this.GetNext index maybeValue =
            let nextMap =
                match maybeValue with
                | Some value ->
                    map |> Map.add index value
                | None ->
                    map |> Map.remove index
            let nextEmptyCoords =
                match emptyCoords with
                | Some coords ->
                    match maybeValue with
                    | Some _ ->
                        coords |> Set.remove index
                    | None ->
                        coords |> Set.add index 
                | None ->
                    let allCoords = (this :> IEnumerable2DArray<'t>).AllCoords |> Set.ofSeq
                    let usedCoords = nextMap.Keys |> Set.ofSeq
                    allCoords - usedCoords
            ImmutableEnumerable2DArray<'t> (xdim, ydim, nextMap, nextEmptyCoords, this) 
        member x.Keys = map.Keys
        member x.Values = map.Values
        member x.KeyValuePairs = Seq.zip map.Keys map.Values
        member x.Previous = previous |> Option.map (fun value -> value :> ImmutableArray<int*int,'t>)
    interface ArrayType<Euclid2DCoords,'t> with
        member this.get_Item coords =
            match map.TryFind coords.AsTuple with
            | Some value ->
                value
            | None ->
                Unchecked.defaultof<'t>    
    interface ArrayType<int*int, 't> with
        member this.get_Item coords =
            match map.TryFind coords with
            | Some value ->
                value
            | None ->
                Unchecked.defaultof<'t>    
    interface TwoDArrayType<'t> with
        member this.get_Item (x,y) =
            match map.TryFind (x,y) with
            | Some value ->
                value
            | None ->
                Unchecked.defaultof<'t>    
    interface IEnumerable2DArray<'t> with
        member x.xDim = xdim   
        member x.yDim = ydim
        member this.Item (x,y) = map.TryFind (x,y)
        member this.Item coords = map.TryFind coords.AsTuple
        member this.ToSeqInDir (x,y) (xDir,yDir) = ArrayToSeq<'t> (this, (x,y), (xDir,yDir))    
        member this.ToSeqInDirWithCoords (x,y) (xDir,yDir) = ArrayToSeqTuple<'t> (this, (x,y), (xDir,yDir))            
        member this.ToSeqWhole = WholeArrayToSeq<'t> this
        member x.AllCoords = 
            let stack = Generic.Stack<int * int> ()
            for X in 0..(xdim-1) do
                for Y in 0..(ydim-1) do
                    stack.Push (X,Y)
            stack
        member this.FilterCoordsByBoundaries coordsSeq = coordsSeq |> Seq.filter (fun (x,y) -> x >= 0 && x < xdim && y>=0 && y < ydim) 
        member this.CheckCoordsByBoundaries (x,y) =
            if x >= 0 && x < xdim && y>=0 && y < ydim then
                Some (x,y)
            else
                None        
        member this.To2DArray =
            Array2D.init xdim ydim (fun x y -> 
                match map.TryFind (x,y) with
                | Some value ->
                    value
                | None ->
                    Unchecked.defaultof<'t>    
            )
        member this.AllEmptyCoords = 
            match emptyCoords with
            | Some coords ->
                coords 
            | None ->
                let allCoords = (this :> IEnumerable2DArray<'t>).AllCoords |> Set.ofSeq
                let usedCoords = map.Keys |> Set.ofSeq
                allCoords - usedCoords :> seq<int*int>
        member x.AllUsedCoords = map.Keys
        member x.AllEntries = map.Values
        member x.AllEntriesWithCoords = Seq.zip map.Values map.Keys