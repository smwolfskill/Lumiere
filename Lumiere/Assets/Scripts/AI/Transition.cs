using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A simple class to represent a transition in the overall state machine.
/// It holds relevant data such as the decision to be made and the states that can result from this decision.
/// We represent Transition as a serializable class so that Unity can recognize it in the editor when building a state machine.
/// </summary>
[System.Serializable]
public class Transition
{
    public Decision decision;
    public State trueState;
}
