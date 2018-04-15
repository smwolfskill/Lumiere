using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovementAnimation : MonoBehaviour 
{
    private Vector2 velocity;
    private Rigidbody2D rb;
    private Animator anim;
    private Dictionary<Direction, string> animationMapping;
    private Direction lastDirection;

    public enum Direction
    {
        NORTH,
        SOUTH,
        EAST,
        WEST
    }

	// Use this for initialization
	void Awake () 
    {
        rb = GetComponent<Rigidbody2D> ();
        anim = GetComponent<Animator> ();
        lastDirection = Direction.NORTH;
        animationMapping = new Dictionary<Direction, string> ();
        animationMapping [Direction.NORTH] = "TNorth";
        animationMapping [Direction.EAST] = "TEast";
        animationMapping [Direction.WEST] = "TWest";
        animationMapping [Direction.SOUTH] = "TSouth";
	}
	
	void FixedUpdate () 
    {
        UpdateLastDirection ();
        UpdateAnimation ();
	}

    void UpdateLastDirection()
    {
        velocity = rb.velocity;
        if (velocity.x > 0) 
        {
            lastDirection = Direction.EAST;
        } 
        else if (velocity.x < 0) 
        {
            lastDirection = Direction.WEST;
        } 
        else if (velocity.y > 0) 
        {
            lastDirection = Direction.NORTH;
        } 
        else if (velocity.y < 0) 
        {
            lastDirection = Direction.SOUTH;
        }
    }

    void UpdateAnimation()
    {
        float speed = velocity.magnitude;
        string animationModifier = "";
        if (speed < 0.1f) 
        {
            animationModifier = "Idle";
        } 
        else 
        {
            animationModifier = "Walk";    
        }

        anim.SetTrigger (animationMapping [lastDirection] + animationModifier);

    }
}
