package p752


trait Tile[-E]:
  def update(event: E): Tile[E]
  val render: String

  
