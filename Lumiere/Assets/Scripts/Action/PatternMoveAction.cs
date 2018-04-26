using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/EntityActions/PatternMoveAction")]
public class PatternMoveAction : MonsterMoveAction
{
    public string[] inputPattern;
    private int[,] pattern;

    private int currentStep;
    private int numSteps;

    public float directionChangeTimer = 5f;
    private float timer = 0f;
    private bool initialized = false;

    public override bool Validate(GameObject obj)
    {
        if (!base.Validate(obj))
        {
            return false;
        }

        //If monster is requesting to move for the first time, then allow the move.
        if (!initialized)
        {
            initialized = true;
            //We have validated once, set the timer to what it should be after one validation
            timer = Time.deltaTime;
            return true;
        }

        //Otherwise, only change movement every directionChangeTimer seconds.
        if (timer >= directionChangeTimer)
        {
            //Reset the timer to what it would've been after one validation
            timer = Time.deltaTime;
            return true;
        }

        //Increment the timer every time Action is validated
        timer += Time.deltaTime;
        return false;
    }

    public override bool Execute(GameObject obj)
    {
        Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D>();
        if (rigidbody == null)
        {
            return false;
        }

        if (pattern == null)
        {
            InitializePattern();
        }

        //Debug.Log ("Steps: " + numSteps);
        //Debug.Log ("Current Step: " + currentStep);
        int nextStep = (currentStep + 1) % numSteps;
        Vector2 speedScale = new Vector2(speed, -speed);
        Vector2 velocity = (FindStep(nextStep) - FindStep(currentStep));
        velocity.Scale(speedScale);
        rigidbody.velocity = velocity;
        currentStep = nextStep;

        return true;
    }

    private Vector2 FindStep(int step)
    {
        for (int i = 0; i < pattern.GetLength(0); i++)
        {
            for (int j = 0; j < pattern.GetLength(1); j++)
            {
                if (pattern[i, j] == step)
                {
                    return new Vector2(j, i);
                }
            }

        }

        return new Vector2(-1, -1);
    }

    private void InitializePattern()
    {
        int columns = 0;
        int rows = inputPattern.Length;
        currentStep = 0;
        numSteps = 0;
        if (rows > 0)
        {
            columns = inputPattern[0].Split(' ').Length;
        }

        pattern = new int[rows, columns];
        for (int i = 0; i < rows; i++)
        {
            string[] row = inputPattern[i].Split(' ');
            for (int j = 0; j < columns; j++)
            {
                pattern[i, j] = int.Parse(row[j]);

                if (pattern[i, j] > numSteps)
                {
                    numSteps = pattern[i, j];
                }
            }
        }

        numSteps++;
    }
}
