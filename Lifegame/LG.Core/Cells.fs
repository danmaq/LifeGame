module Cells

open System

///<summary>セルの状態。</summary>
type Cell =
    {
        ///<summary>生存しているかどうか。</summary>
        live: bool
        ///<summary>隣接セルへの参照。</summary>
        neighbors: Cell []
    }
    static member init = { live = false; neighbors = [||] }
    ///<summary>セルの状態に対する、次の生存状態。</summary>
    static member next cell =
        let count =
            cell.neighbors
                |> Array.toSeq
                |> Seq.filter (fun c -> c.live)
                |> Seq.length
        cell.live && count = 2 || count = 3

///<summary>隣接セルのオフセット。</summary>
let private neighborsOffset =
    let src = [| -1 .. 1 |] |> Array.toSeq
    src
        |> Seq.map (fun y -> src |> Seq.map (fun x -> (x, y)))
        |> Seq.concat
        |> Seq.filter (fun xy -> xy <> (0, 0))
        |> Seq.toArray

///<summary>ランダムで真偽を返す関数。</summary>
let private random =
    let rnd = new Random()
    fun () -> rnd.Next 10 >= 5

///<summary>セル一覧の状態。</summary>
type Cells =
    {
        ///<summary>セルの列数。</summary>
        width: int
        ///<summary>セルの行数。</summary>
        height: int
        ///<summary>セル一覧。</summary>
        cells: Cell []
    }
    ///<summary>列数・X座標・Y座標に対応する、一次元インデックス。</summary>
    static member toIndex w x y = y * w + x
    ///<summary>列数・一次元インデックスに対応する、X座標・Y座標。</summary>
    static member toPos w i = (i % w, i / w)
    ///<summary>セル情報一覧とセル情報に対応する座標。</summary>
    static member pos cells cell =
        match cells.cells |> Array.tryFindIndex (fun c -> c = cell) with
            | Some c -> c |> Cells.toPos cells.width |> Some
            | None -> None
    ///<summary>列数・行数に対応するセル情報一覧。</summary>
    static member create (width, height) =
        let cells = Cell.init |> Array.create (width * height)
        let neighborsIndex i =
            let (bx, by) = i |> Cells.toPos width
            neighborsOffset
                |> Array.toSeq
                |> Seq.map (fun (x, y) -> cells.[Cells.toIndex width x y])
                |> Seq.toArray
        {
            width = width
            height = height
            cells = cells |> Array.mapi (fun i c -> { c with neighbors = i |> neighborsIndex })
        }
