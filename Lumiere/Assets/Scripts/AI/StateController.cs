using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A state controller that controls the execution of the overall state machine and keeps track of the current state.
/// This controller is a MonoBehavior as it will be attached to the AI itself.
/// </summary>
public class StateController : MonoBehaviour 
{
    public State currentState;

	// Use this for initialization
	void Start () 
    {
		
	}
	
	// Update is called once per frame
	void Update () 
    {
        currentState.UpdateState (this); //Update the state of this controller every frame
	}

    /// <summary>
    /// Transitions to the next state.
    /// </summary>
    /// <param name="nextState">The next state to transition to.</param>
    public void TransitionToState(State nextState)
    {
        if (nextState != null) 
        {
            currentState = nextState;
        }
    }
}
