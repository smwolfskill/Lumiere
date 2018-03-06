using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A class to represent decisions that an AI can make. 
/// The decision made will impact the next state the AI will transition to.
/// </summary>
public abstract class Decision : ScriptableObject 
{
    public abstract bool Decide ();
}
