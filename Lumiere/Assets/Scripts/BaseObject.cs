using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Base object class, designed for general use for all sub-classes that represent some object in the game.
/// </summary>
public abstract class BaseObject : ScriptableObject 
{
    protected Sprite sprite;    // Sprite data.
    
    /// <summary>
    /// Getter function for sprite data.
    /// </summary>
    /// <returns>The sprite data of the object.</returns>
    public Sprite getSprite() 
    {
        return sprite;
    }

    /// <summary>
    /// Setter function for sprite data.
    /// </summary>
    /// <param name="sprite">The new sprite being set.</param>
    public void setSprite(Sprite sprite) 
    {
        this.sprite = sprite;
    }
}