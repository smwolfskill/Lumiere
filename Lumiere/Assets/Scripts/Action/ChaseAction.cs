using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/MonsterMoveActions/ChaseAction")]
public class ChaseAction : MonsterMoveAction 
{
    /// <summary>
    /// The distance to stop chasing the target.
    /// </summary>
    public float stoppingDistance;
    public float pathfindingThreshold = 3f;

    private Vector2 ourPosition;
    private Vector2 targetPosition;
    private Vector2 oldTargetPosition = new Vector2(int.MaxValue, int.MaxValue);
    private Pathfinding pathfinding = null;
    private List<TileObj> path = new List<TileObj> ();
    private int currentPathIndex = 1;

    /// <summary>
    /// Checks whether this action should be executed or not for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action should be executed, false otherwise.</returns>
    public override bool Validate (GameObject obj)
    {
        StateController stateController = obj.GetComponent<StateController>();
        if(stateController == null)
        {
            return false;
        }

        if(pathfinding == null)
        {
            pathfinding = new Pathfinding(stateController.map);
        }

        return true;
    }

    /// <summary>
    /// Executes this action for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute (GameObject obj)
    {
        ourPosition = obj.transform.position;
        GameObject target = GameObject.FindGameObjectWithTag("Player");
        targetPosition = target.transform.position;
        float oldTargetDistance = Vector2.Distance(targetPosition, oldTargetPosition);
        Rigidbody2D rb = obj.GetComponent<Rigidbody2D> ();
        if (rb == null) 
        {
            return false;
        }

        if (Vector2.Distance (ourPosition, targetPosition) <= stoppingDistance) 
        {
            rb.velocity = Vector2.zero;
            currentPathIndex = 1;
            return false;
        }

        if (path.Count > 1 && Vector2.Distance (ourPosition, targetPosition) < Vector2.Distance (new Vector2(path [currentPathIndex].x, path[currentPathIndex].y), targetPosition)) 
        {
            currentPathIndex++;
        }

        if(path.Count == 0 || currentPathIndex > path.Count - 1 || oldTargetDistance > pathfindingThreshold)
        {
            //Do pathfinding
            path = pathfinding.GetPath(ourPosition, targetPosition);
            currentPathIndex = 1;
        }

        Vector2 direction = new Vector2 (path [currentPathIndex].x - ourPosition.x, path [currentPathIndex].y - ourPosition.y);
        direction.Normalize ();
        rb.velocity = direction * speed;
        oldTargetPosition = targetPosition;

        return true;

    }
}
