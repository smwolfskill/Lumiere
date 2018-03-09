using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A simple class to represent a State in the overall state machine.
/// A state contains several actions that should be performed when the AI is in the state.
/// Furthermore, the state also contains its possible transitions.
/// We can easily create states as ScriptableObjects and store them for re-use.
/// </summary>
[CreateAssetMenu(menuName = "Lumiere/AI/State")]
public class State : ScriptableObject
{
    public Action[] actions;
    public Transition[] transitions;

    /// <summary>
    /// Updates the state of the state controller based on the transitions given, and executes actions.
    /// </summary>
    /// <param name="stateController">State controller.</param>
    public void UpdateState(StateController stateController)
    {
        DoActions (stateController);
        CheckTransitions (stateController);
    }

    /// <summary>
    /// Executes the actions to be performed in this state in the order they were specified.
    /// </summary>
    /// <param name="stateController">The state controller of the AI.</param>
    private void DoActions (StateController stateController)
    {
        GameObject gameObject = stateController.gameObject;
        if (gameObject == null) 
        {
            throw new MissingReferenceException ();
        }

        foreach (Action action in actions) 
        {
            if (action.Validate (gameObject)) 
            {
                action.Execute (gameObject);
            }
        }
    }

    /// <summary>
    /// Check all the transitions and evaluate what the next state should be.
    /// Update the controller's state when necessary. Checks the transitions.
    /// </summary>
    /// <param name="stateController">The state controller of the AI.</param>
    private void CheckTransitions(StateController stateController)
    {
        for (int i = 0; i < transitions.Length; i++) 
        {
            if (transitions [i].decision.Decide (stateController)) 
            {
                stateController.TransitionToState (transitions [i].trueState);
            } 
            else 
            {
                stateController.TransitionToState (transitions [i].falseState);
            }
        }
    }


}
