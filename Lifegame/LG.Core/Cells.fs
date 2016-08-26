module Cells

///<summary>セルの状態に対する、次の生存状態。</summary>
type Cell =
    {
        live: bool
        neighbors: Cell[]
    }
    ///<summary>セルの状態に対する、次の生存状態。</summary>
    static member next cell =
        let count =
            cell.neighbors
                |> Array.toSeq
                |> Seq.filter (fun c -> c.live)
                |> Seq.length
        cell.live && count = 2 || count = 3

let create (width, height) =
    let p2i x y = x + y * width
    let i2p i = (i % width, i / width)
    let neighbors =
        let src = [| -1 .. 1 |] |> Array.toSeq
        src
            |> Seq.map (fun y -> src |> Seq.map (fun x -> (x, y)))
            |> Seq.concat
            |> Seq.toArray
    let cells =
        { live = false; neighbors = [||] }
            |> Array.create (width * height)
    cells |> Array.mapi (fun i c -> ())
    ()
