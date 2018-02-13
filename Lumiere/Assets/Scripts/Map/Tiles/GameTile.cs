using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Representation of tiles for the creation of the game and the map.
/// </summary>
public abstract class GameTile : BaseObject
{
	/// <summary>
	/// Base constructor for an ultra generic gametile.
	/// </summary>
	/// <param name="spritedata">Contains the image information in regards to this tile.</param>
	public GameTile(Sprite sprite)
	{
		this.sprite = sprite;
	}
}

