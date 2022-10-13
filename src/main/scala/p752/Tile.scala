package p752

import p752.Tiles.toLines
import p752.Tiles.pureSize
import p752.Tiles.fillSquare

trait Tile:
  def update(event: Event): Tile
  val render: String

  
