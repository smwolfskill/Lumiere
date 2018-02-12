using UnityEngine;

/// <summary>
/// A tile object that represents walls (Things the player cannot navigate through)
/// </summary>
public class WallTile : GameTile
{
    /// <summary>
    /// Constructor for a wall tile. Walls are impassable.
    /// </summary>
    /// <param name="spritedata">The wall's sprite data.</param>
    public WallTile(Sprite spritedata) : base(spritedata)
    {
        // Nothing to see here, move along.
    }
}
