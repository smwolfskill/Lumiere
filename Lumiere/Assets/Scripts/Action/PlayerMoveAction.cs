using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/PlayerMoveAction")]
public class PlayerMoveAction : EntityAction
{
    public float speed = 1.0f;
    public float movement_multiplier = 1.1f;
    public float movement_delta = 0.0001f;
    protected float m_left = 0.0f;      //current horizontal movement
    protected float m_right = 0.0f;      //current vertical movement
    protected float m_up = 0.0f;
    protected float m_down = 0.0f;
    protected float m_max = 1.0f;

    public override bool Validate(GameObject obj)
    {
        //TODO: Replace this in future iterations
        return true;
    }

    public override bool Execute(GameObject obj)
    {
        //Debug.Log("exec");
        Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D> ();

        //Safety check for rigidbody
        if (rigidbody == null)
        {
            return false;
        }

        //OLD:
        /*//Get input on horizontal and vertical axes (Keybindings in project settings)
        float h = Input.GetAxis ("Horizontal");
        float v = Input.GetAxis ("Vertical");
        rigidbody.velocity = new Vector2(speed * h, speed * v);*/

        //NEW w/ SettingsManager:
        UpdateMovement();
        float movement_h = m_right - m_left;
        float movement_v = m_up - m_down;
        //Allow diagonal movement
        rigidbody.velocity = new Vector2(speed * movement_h, speed * movement_v);

        return true;
    }

    protected void UpdateMovement()
    {
        //TODO: backup prev values to have smooth transitions. *2 each time?
        bool h_left = Input.GetKey(SettingsManager.GetMoveLeft());
        bool h_right = Input.GetKey(SettingsManager.GetMoveRight());
        bool v_up = Input.GetKey(SettingsManager.GetMoveUp());
        bool v_down = Input.GetKey(SettingsManager.GetMoveDown());
        if(h_left)
        {
            m_left += movement_multiplier * m_left + movement_delta;
        }
        else
        {
            m_left -= m_left / movement_multiplier + movement_delta;
        }

        if(h_right)
        {
            m_right += movement_multiplier * m_right + movement_delta;
        }
        else
        {
            m_right -= m_right / movement_multiplier + movement_delta;
        }

        if(v_up)
        {
            m_up += movement_multiplier * m_up + movement_delta;
        }
        else
        {
            m_up -= m_up / movement_multiplier + movement_delta;
        }

        if(v_down)
        {
            m_down += movement_multiplier * m_down + movement_delta;
        }
        else
        {
            m_down -= m_down / movement_multiplier + movement_delta;
        }

        //Clamp to max/min values if needed
        if(m_left > m_max)
        {
            m_left = m_max;
        }
        else if(m_left < 0.0f)
        {
            m_left = 0.0f;
        }

        if(m_right > m_max)
        {
            m_right = m_max;
        }
        else if(m_right < 0.0f)
        {
            m_right = 0.0f;
        }

        if(m_up > m_max)
        {
            m_up = m_max;
        }
        else if(m_up < 0.0f)
        {
            m_up = 0.0f;
        }

        if(m_down > m_max)
        {
            m_down = m_max;
        }
        else if(m_down < 0.0f)
        {
            m_down = 0.0f;
        }

    }
}
