package p752

trait Tile[-E, S, +MSG]:
  def render(state: S): String
  def update(event: E, state: S): (S, MSG)
