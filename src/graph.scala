package graph

import scala.io.Source

object G {
  type Vertex = Int
  type Edge = Vertex
  type Graph = Seq[Seq[Edge]]

  def load(path: String): Graph = {
    Source
      .fromFile(path) // ファイルを開き
      .getLines() // 一行ずつ読み込み
      .map { // その各行について
        case "" => Seq() // 空行なら空列に
        // さもなくばカンマで刻んだ要素を整数に変換してから正順化
        case line => line.split(",").map(_.toInt).toSeq.sorted
      }
      .toSeq
  }

  def output(g: Graph): Unit = {
    g.zipWithIndex
      .foreach { // zipWithIndex は [x0, x1, ...] な列を [(x0, 0), (x1, 1), ...]に変換する
        case (gi, i) => println(s"$i: ${gi.map(_.toString()).mkString(",")}")
      }
  }

  def output_matrix(g: G.Graph): Unit = {
    Range(0, g.length).foreach(i =>
      println(
        s"$i ${Range(0, g.length)
          .map(j => if (is_connected(g, i, j)) 'x' else ' ')
          .mkString(" ")}"
      )
    )
  }

  def is_connected(g: G.Graph, i: Int, j: Int): Boolean = {
    val neighbors_i = g(i)
    try {  // neigobors_i は整順化されているので、以下では二分法で検索している
      var k1 = 0; var k2 = neighbors_i.length - 1
      while (k1 <= k2) {
        val m = k1 + (k2 - k1) / 2
        val v = neighbors_i(m)
        if (v < j) k1 = m + 1
        else if (j < v) k2 = m - 1
        else throw new Exception()
      }
      false
    } catch {
      case e: Exception => true
    }
  }

  type Vertices = Seq[Vertex]

  def is_walk(g: Graph, s: Vertices): Boolean = {
    s.length <= 1 ||
    Range(0, s.length - 1).forall(i => is_connected(g, s(i), s(i + 1)))
  }

  def is_path(g: Graph, s: Vertices): Boolean = {
    is_walk(g, s) && Set(s).size == s.length  // 経過する頂点が重複しないことを集合に変換してから、大きさを調べることで確認している。
  }

  def is_cycle(g: Graph, s: Vertices): Boolean = {
    s.length <= 1 ||
    is_walk(g, s) && s(0) == s(s.length - 1)
  }
}

object Traversal {
  import scala.collection.mutable.ArraySeq
  import G._
  type OnVisit = Vertex => Unit
  type Traversal[T] = (Graph, Vertex, OnVisit) => T

  // グラフgの頂点sからの深さ優先探索（再帰）
  def depth_first_rec: Traversal[Unit] =
    (g, s, on_visit) => {
      val visited = ArraySeq.fill(g.length)(false) // 訪問の有無を記録 (青or赤)

      // 頂点uの訪問
      def visit(u: Vertex): Unit = {
        visited(u) = true // 訪問したことの記録：青→赤
        on_visit(u)
        g(u).foreach(v => { // 各隣接頂点について
          if (!visited(v)) visit(v) // 未訪問なら訪問
        })
      }

      visit(s) // 頂点sを訪問
    }

  // グラフgの頂点sからの深さ優先探索（Stackを使用）
  def depth_first: Traversal[Unit] =
    (g, s, on_visit) => {
      import scala.collection.mutable.Stack

      val found = ArraySeq.fill(g.length)(false) // 存在が認められた頂点群
      found(s) = true
      val to_visit = Stack(s) // 今後，訪問すべき頂点群
      while (!to_visit.isEmpty) { // 訪問先がなくなるまでループ
        val u = to_visit.pop() // スタックから次に訪問すべき頂点を取り出す
        on_visit(u)
        g(u).foreach(v => { // 各隣接頂点について
          if (!found(v)) { // 新発見のものについて
            found(v) = true // 見つけたことを記録し
            to_visit.push(v) // 訪問先に追加
          }
        })
      }
    }

  // グラフgの頂点sからの幅優先探索（Queueを使用）
  def breadth_first: Traversal[Unit] =
    (g, s, on_visit) => {
      import scala.collection.mutable.Queue

      val found = ArraySeq.fill(g.length)(false) // 存在が認められた頂点群
      found(s) = true
      val to_visit = Queue(s) // 今後，訪問すべき頂点群
      while (!to_visit.isEmpty) { // 訪問先がなくなるまでループ
        val u = to_visit.dequeue() // スタックから次に訪問すべき頂点を取り出す
        on_visit(u)
        g(u).foreach(v => { // 各隣接頂点について
          if (!found(v)) { // 新発見のものについて
            found(v) = true // 見つけたことを記録し
            to_visit.enqueue(v) // 訪問先に追加
          }
        })
      }
    }
}

object Distance {
  import scala.collection.mutable.ArraySeq
  import G._

  /**
    * 幅優先探索の変形により，与えられた頂点からの距離を全頂点を計算
    * found: ArraySeq[Boolean] の distance: ArraySeq[Int] への変形が要点
    */
  def breadth_first(g: Graph, s: Vertex): ArraySeq[Int] = {
    import scala.collection.mutable.Queue

    val distance = ArraySeq.fill(g.length)(-1) // 存在が認められた頂点のsからの距離
    distance(s) = 0
    val to_visit = Queue(s) // 今後，訪問すべき頂点群

    while (!to_visit.isEmpty) { // 訪問先がなくなるまでループ
      val u = to_visit.dequeue() // スタックから次に訪問すべき頂点を取り出す
      g(u).foreach(v => { // 各隣接頂点について
        if (distance(v) < 0) { // 新発見のものについ
          distance(v) = distance(u) + 1 // 距離を計算し
          to_visit.enqueue(v) // 訪問先に追加
        }
      })
    }
    distance
  }
}

object Search {
  import scala.collection.mutable.ArraySeq
  import G._

  def depth_first(g: Graph, s: Vertex, t: Vertex): Boolean = {
    val found = ArraySeq.fill(g.length)(false) // 存在が認められた頂点群

    def visit(u: Vertex): Unit = {
      found(u) = true
      g(u).foreach(v => {
        if (!found(v)) visit(v)
      })
    }

    visit(s)
    found(t)
  }
}

object Topological {
  import scala.collection.mutable.ArraySeq
  import scala.collection.mutable.Queue
  import G._

  def traverse(g: Graph): Unit = {
    val found = ArraySeq.fill(g.length)(false) // 存在が認められた頂点群
    val to_visit = Queue[String]()

    def visit(u: Vertex): Unit = {
      to_visit.enqueue(s"${u}in")
      found(u) = true
      g(u).foreach(v => {
        if (!found(v)) visit(v)
      })
      to_visit.enqueue(s"${u}out")
    }

    for (s <- Range(0, g.length)) {
      if (!found(s)) {
        visit(s)
        println(to_visit.mkString(" -> "))
        to_visit.clear()
      }
    }
  }

  def sort(g: Graph): Queue[Vertex] = {
    val found = ArraySeq.fill(g.length)(false)
    val visited = Queue[Vertex]()

    def visit(u: Vertex): Unit = {
      found(u) = true
      g(u).foreach(v => {
        if (!found(v)) visit(v)
      })
      visited.enqueue(u)
    }

    for (s <- Range(0, g.length)) {
      if (!found(s)) visit(s)
    }
    visited.reverse
  }
}

@main def ex1_io() = {
  val g = G.load("data/undirected.csv")
  println("隣接リスト形式")
  G.output(g)
  println()
  println("マトリックス形式")
  G.output_matrix(g)
}

@main def ex2_traverse() = {
  val g = G.load("data/undirected.csv")
  G.output(g)

  print("Visiting by DFS-rec:     "); Traversal.depth_first_rec(g, 0, print _); println()
  print("Visiting by DFS (stack): "); Traversal.depth_first(g, 0, print _); println()

  print("Visiting by BFS (queue): "); Traversal.breadth_first(g, 0, print _); println()
}

@main def ex3_distances() = {
  val g = G.load("data/directed.csv")
  G.output(g)

  println(s"Distances from vertex 0: ${Distance.breadth_first(g, 0)}")
  println(s"Distances from vertex 1: ${Distance.breadth_first(g, 1)}")
  println(s"0-7 path: ${Search.depth_first(g, 0, 7)}")
  println(s"7-0 path: ${Search.depth_first(g, 7, 0)}")
}

@main def ex4_topological_sort() = {
  val g = G.load("data/directed.csv")
  G.output(g)

  println("Depth-first search over a directed graph (data/directed.csv)")
  Topological.traverse(g)
  println(Topological.sort(g).map(_.toString).mkString(", "))
}