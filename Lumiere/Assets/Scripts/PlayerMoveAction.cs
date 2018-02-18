using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/PlayerMoveAction")]
public class PlayerMoveAction : EntityAction
{
    public float speed = 1f;

    public override bool Validate(GameObject obj)
    {
        //TODO: Replace this in future iterations
        return true;
    }

    public override bool Execute(GameObject obj)
    {
        Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D> ();

        //Safety check for rigidbody
        if (rigidbody == null)
        {
            return false;
        }

        //Get input on horizontal and vertical axes (Keybindings in project settings)
        float h = Input.GetAxis ("Horizontal");
        float v = Input.GetAxis ("Vertical");

        //Move only along the axis of the 'stronger' input to prevent diagonal movement
        if (Mathf.Abs(h) >= Mathf.Abs(v))
        {
            rigidbody.velocity = new Vector2 (h, 0f);
        }
        else
        {
            rigidbody.velocity = new Vector2 (0f, v);
        }

        return true;
    }
}
