open Practices.Environments
open System
open Utils.IOUtils

let world = Gridworld (4, Static)
Console.Write (display2DArray (world.Display()))
