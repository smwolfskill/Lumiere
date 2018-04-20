using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/PlayerMoveAction")]
public class PlayerMoveAction : EntityAction
{
    public float speed = 1.0f;
    public float movement_multiplier = 1.5f;
    public float movement_loss_divider = 1.5f;
    public float direction_change_delta = 0.1f;
    public float movement_delta = 0.0001f;
    public float max_dist = 1.0f;
    public float max_dist_walking = 0.7f;

    protected float m_horiz = 0.0f; //movement along horizontal axis in [-1, 1]
    protected float m_vert = 0.0f; //movement along vertical axis in [-1, 1]
    protected float m_dist = 0.0f; //amount to scale (m_horiz, m_vert)

    public override bool Validate(GameObject obj)
    {
        //Needed so that velocity can be set to 0 when no movement is occurring
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
            
        UpdateMovement();
        float movement_h = m_horiz;
        float movement_v = m_vert;
        //Allow diagonal movement
        rigidbody.velocity = new Vector2(speed * movement_h * m_dist, speed * movement_v * m_dist);

        return true;
    }

    protected void UpdateMovement()
    {
        bool h_left = Input.GetKey(SettingsManager.GetMoveLeft());
        bool h_right = Input.GetKey(SettingsManager.GetMoveRight());
        bool horiz = h_left ^ h_right; //XOR
        bool v_up = Input.GetKey(SettingsManager.GetMoveUp());
        bool v_down = Input.GetKey(SettingsManager.GetMoveDown());
        bool vert = v_up ^ v_down; //XOR
        bool walking = Input.GetKey(SettingsManager.GetWalk());
        float max_d = max_dist; //max movement directly along an axis (x or y)
        if(walking)
        {
            max_d = max_dist_walking;
        }
        /*if(h_left && h_right)
        {
            horiz = false;
        }
        if(v_up && v_down)
        {
            vert = false;
        }*/
        if(horiz && vert) //move diagonally
        {
            float eq_max = Mathf.Sqrt(2) / 2;
            if(Mathf.Abs(m_horiz) < Mathf.Abs(m_vert)) //move more horizontally towards equilibrium (perfect diagonal)
            {
                if(h_left)
                {
                    m_horiz -= 2 * direction_change_delta;    
                }
                else
                {
                    m_horiz += 2 * direction_change_delta;
                }

                if(m_horiz > eq_max)
                {
                    m_horiz = eq_max;
                }
                else if(m_horiz < -eq_max)
                {
                    m_horiz = -eq_max;
                }
                m_vert = Mathf.Sign(m_vert) * Mathf.Sqrt(1 - m_horiz * m_horiz);
            }
            else if(Mathf.Abs(m_horiz) > Mathf.Abs(m_vert)) //move more vertically towards equilibrium (perfect diagonal)
            {
                if(v_down)
                {
                    m_vert -= 2 * direction_change_delta;    
                }
                else
                {
                    m_vert += 2 * direction_change_delta;
                }

                if(m_vert > eq_max)
                {
                    m_vert = eq_max;
                }
                else if(m_vert < -eq_max)
                {
                    m_vert = -eq_max;
                }
                m_horiz = Mathf.Sign(m_horiz) * Mathf.Sqrt(1 - m_vert * m_vert);
            }
            else
            {
                if(h_left)
                {
                    m_horiz = -eq_max;
                }
                else
                {
                    m_horiz = eq_max;
                }
                if(v_down)
                {
                    m_vert = -eq_max;
                }
                else
                {
                    m_vert = eq_max;
                }
            }
        }
        else if(horiz) //only moving horizontally
        {
            float old_horiz = m_horiz;
            if(h_left)
            {
                m_horiz = -max_d;
            }
            else
            {
                m_horiz = max_d;
            }
            if(m_horiz > max_d)
            {
                m_horiz = max_d;
            }
            else if(m_horiz < -max_d)
            {
                m_horiz = -max_d;
            }
            m_vert = 0.0f;
        }
        else if(vert) //only moving vertically
        {
            float old_vert = m_vert;
            if(v_down)
            {
                m_vert = -max_d;
            }
            else
            {
                m_vert = max_d;
            }
            if(m_vert > max_d)
            {
                m_vert = max_d;
            }
            else if(m_vert < -max_d)
            {
                m_vert = -max_d;
            }
            m_horiz = 0.0f;
        }
        //2. Apply distance multiplier
        if(!horiz && !vert) //if not moving, slow to a stop.
        {
            m_dist = m_dist / movement_loss_divider - movement_delta;
        }
        else //if moving, increase distance travelled until max.
        {
            m_dist = m_dist * movement_multiplier + movement_delta;
        }
        if(m_dist < 0.0f)
        {
            m_dist = 0.0f;
        }
        else if(m_dist > max_d)
        {
            m_dist = max_d;
        }
        //Debug.Log("m_dist = " + m_dist.ToString() + "; max_dist = " + max_dist.ToString() + "; m_d_w = " + max_dist_walking.ToString());
    }
}
