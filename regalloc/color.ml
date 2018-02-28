module type T = sig
module Frame : Frame.T
module Liveness : Liveness.T
module Temp : Temp.T
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
val color :  color -> allocation * Temp.temp list
end

module F (Frame: Frame.T) (Liveness: Liveness.T) =
struct
module Frame = Frame
module Liveness = Liveness
module Temp = Frame.Temp
type allocation = Frame.register Temp.Table.table
type color = {interference: Liveness.igraph; initial: allocation; spillCost: Graph.node -> int; registers: Frame.register list} 
let color color  = 

    let precolored = [] in
    let initial = [] in
    let simplifyWorklist = [] in
    let freezeWorklist = []in
    let spillWorklist = [] in
    let spilledNodes = [] in
    let coalescedNodes = [] in
    let coloredNodes = [] in
    let selectStack = [] in

    let coalescedMoves = [] in
    let constrainedMoves = [] in
    let frozenMoves = [] in
    let worklistMoves = [] in
    let activeMoves = [] in

    let adjSet = () in
    let adjList = () in
    let degree = () in
    let moveList = () in
    let alias = () in
    let color = () in
    
    (Temp.Table.empty, [])
end