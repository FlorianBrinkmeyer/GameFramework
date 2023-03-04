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

module Euclid2D.Enumerable2DArray

open System.Collections.Generic
open System.Collections
open System
open Euclid2D

let allDirs = [(1,0);(0,1);(-1,0);(0,-1);(1,1);(1,-1);(-1,1);(-1,-1)]
let orthDirs = [(1,0);(0,1);(-1,0);(0,-1)]
let diaDirs = [(1,1);(1,-1);(-1,1);(-1,-1)]

let nthColumn n = ((n,0),(0,1)) 
let nthRow n = ((0,n),(1,0))
let mainDia = ((0,0),(1,1))

type IEnumerable2DArray<'t> =
    inherit ArrayType<Euclid2DCoords,'t>
    inherit TwoDArrayType<'t>
    abstract Item: int*int -> Option<'t>
    abstract Item: Euclid2DCoords -> Option<'t>
    abstract xDim: int
    abstract yDim: int
    abstract ToSeqInDir: int*int -> int*int -> seq<Option<'t>>
    abstract ToSeqInDirWithCoords: int*int -> int*int -> seq<Option<'t>*(int*int)>
    abstract ToSeqWhole: seq<Option<'t>>
    abstract AllCoords: seq<int*int>
    abstract FilterCoordsByBoundaries : seq<int*int> -> seq<int*int>
    abstract CheckCoordsByBoundaries : int*int -> Option<int*int>
    abstract AllUsedCoords : seq<int*int>
    abstract AllEmptyCoords : seq<int*int>
    abstract AllEntriesWithCoords : seq<'t * (int*int)>
    abstract AllEntries : seq<'t>
    abstract To2DArray: Option<'t> [,]

type ToSeqEnumerator<'t> (array : IEnumerable2DArray<'t>, Start: int * int, Direction: int * int) =
    let mutable x = (Start |> fst) - (Direction |> fst) 
    let mutable y = (Start |> snd) - (Direction |> snd)
    interface IEnumerator<Option<'t>> with
        member this.Reset () =
            x <- (Start |> fst) - (Direction |> fst)    
            y <- (Start |> snd) - (Direction |> snd)
        member this.Current
            with get () = array.[x,y]
        member this.Current 
            with get () = array.[x,y] :> Object    
        member this.MoveNext () =
            match Direction with
            | (dx, dy) ->    
                x <- x + dx
                y <- y + dy
            (x>=0) && (x < array.xDim) && (y>=0) && (y< array.yDim) 
    interface IDisposable with
        member this.Dispose () = ()          

type ArrayToSeq<'t> (array : IEnumerable2DArray<'t>, start: int * int, direction: int * int) = 
    interface IEnumerable<Option<'t>> with
        member this.GetEnumerator () = new ToSeqEnumerator<'t> (array, start, direction) :> IEnumerator<Option<'t>>
    interface IEnumerable with
        member this.GetEnumerator () = new ToSeqEnumerator<'t> (array, start, direction) :> IEnumerator

type ToSeqEnumeratorTuple<'t> (array : IEnumerable2DArray<'t>, start: int * int, direction: int * int) =
    let mutable x = (start |> fst) - (direction |> fst) 
    let mutable y = (start |> snd) - (direction |> snd)
    interface IEnumerator<Option<'t> * (int * int)> with
        member this.Reset () =
            x <- (start |> fst) - (direction |> fst)    
            y <- (start |> snd) - (direction |> snd)
        member this.Current
            with get () = (array.[x,y], (x,y))
        member this.Current 
            with get () = (array.[x,y], (x,y)) :> Object    
        member this.MoveNext () =
            match direction with
            | (dx, dy) ->    
                x <- x + dx
                y <- y + dy
            (x>=0) && (x< array.xDim) && (y>=0) && (y<array.yDim) 
    interface IDisposable with
        member this.Dispose () = ()          

type ArrayToSeqTuple<'t> (array : IEnumerable2DArray<'t>, start: int * int, direction: int * int) = 
    interface IEnumerable<Option<'t> * (int * int)> with
        member this.GetEnumerator () = new ToSeqEnumeratorTuple<'t> (array, start, direction) :> IEnumerator<Option<'t> * (int * int)>
    interface IEnumerable with
        member this.GetEnumerator () = new ToSeqEnumeratorTuple<'t> (array, start, direction) :> IEnumerator

type WholeToSeqEnumerator<'t> (array : IEnumerable2DArray<'t>) =
    let mutable x = -1 
    let mutable y = 0
    interface IEnumerator<Option<'t>> with
        member this.Reset () =
            x <- -1    
            y <- 0
        member this.Current
            with get () = array.[x,y]
        member this.Current 
            with get () = array.[x,y] :> Object    
        member this.MoveNext () =
            x <- x+1
            if x > array.xDim - 1 then
                x <-0
                y <-y+1
            y< array.yDim 
    interface IDisposable with
        member this.Dispose () = ()          

type WholeArrayToSeq<'t> (array : IEnumerable2DArray<'t>) = 
    interface IEnumerable<Option<'t>> with
        member this.GetEnumerator () = (new WholeToSeqEnumerator<'t> (array)) :> IEnumerator<Option<'t>>
    interface IEnumerable with
        member this.GetEnumerator () = (new WholeToSeqEnumerator<'t> (array)) :> IEnumerator