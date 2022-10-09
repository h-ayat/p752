package p752


trait Tile[-T]:
  def update(event: Either[Event,T]): Tile[T]
  val render: String

  
