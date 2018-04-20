using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/PlayerMoveAction")]
public class PlayerMoveAction : EntityAction
{
    public float speed = 1.0f;
    public float movement_multiplier = 1.5f;
    public float movement_loss_divider = 2.05f;
    public float movement_delta = 0.1001f; //old: 0.0001f
    /*protected float m_left = 0.0f;      //current horizontal movement
    protected float m_right = 0.0f;      //current vertical movement
    protected float m_up = 0.0f;
    protected float m_down = 0.0f;*/
    protected float m_horiz = 0.0f;
    protected float m_vert = 0.0f;
    protected float m_dist = 0.0f;
    protected float m_max = 1.0f;
    protected bool moving_horiz = false;
    protected bool moving_vert = false;

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
        float movement_h = m_horiz;
        float movement_v = m_vert;
        //Allow diagonal movement
        rigidbody.velocity = new Vector2(speed * movement_h * m_dist, speed * movement_v * m_dist);

        return true;
    }

    protected void UpdateMovement()
    {
        //TODO: backup prev values to have smooth transitions. *2 each time?
        bool h_left = Input.GetKey(SettingsManager.GetMoveLeft());
        bool h_right = Input.GetKey(SettingsManager.GetMoveRight());
        bool horiz = h_left || h_right;
        bool v_up = Input.GetKey(SettingsManager.GetMoveUp());
        bool v_down = Input.GetKey(SettingsManager.GetMoveDown());
        bool vert = v_up || v_down;
        bool walk = Input.GetKey(KeyCode.LeftAlt);
        //float old_max = m_max;
        //if(walk)
            //m_max = 0.5f;
        float max_d = m_max; //max direction along an axis (x or y)
        if(h_left && h_right)
        {
            horiz = false;
        }
        if(v_up && v_down)
        {
            vert = false;
        }
        if(horiz && vert) //&& !moving_horiz && !moving_vert*/) //rare case when press both at same time when stopped previously
        {
            float eq_max = Mathf.Sqrt(2) / 2;
            float delta = 0.1f;
            if(Mathf.Abs(m_horiz) < Mathf.Abs(m_vert)) //move more horizontally towards equilibrium
            {
                if(h_left)
                {
                    m_horiz -= 2 * delta;    
                }
                else
                {
                    m_horiz += 2 * delta;
                }

                if(m_horiz > eq_max)
                {
                    m_horiz = eq_max;
                }
                else if(m_horiz < -eq_max)
                {
                    m_horiz = -eq_max;
                }
                m_vert = Mathf.Sign(m_vert) * Mathf.Sqrt(1 - m_horiz * m_horiz); //travel in same direction, but less
                Debug.Log("both, v stronger: m_h = " + m_horiz.ToString() + "; m_v = " + m_vert.ToString());
            }
            else if(Mathf.Abs(m_horiz) > Mathf.Abs(m_vert)) //move more vertically towards equilibrium
            {
                if(v_down)
                {
                    m_vert -= 2 * delta;    
                }
                else
                {
                    m_vert += 2 * delta;
                }

                if(m_vert > eq_max)
                {
                    m_vert = eq_max;
                }
                else if(m_vert < -eq_max)
                {
                    m_vert = -eq_max;
                }
                m_horiz = Mathf.Sign(m_horiz) * Mathf.Sqrt(1 - m_vert * m_vert); //travel in same direction, but less
                Debug.Log("both, h stronger: m_h = " + m_horiz.ToString() + "; m_v = " + m_vert.ToString());
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
                Debug.Log("both, EQ: m_h = " + m_horiz.ToString() + "; m_v = " + m_vert.ToString());
            }
        }
        else if(horiz) //&& !moving_horiz)
        {
            if(h_left)
            {
                //m_horiz -= 0.0001f;
                m_horiz = -max_d;
            }
            else
            {
                //m_horiz += 0.0001f;
                m_horiz = max_d;
            }
            if(m_horiz > max_d)
            {
                m_horiz = max_d;
                m_vert = 0.0f;
            }
            else if(m_horiz < -max_d)
            {
                m_horiz = -max_d;
                m_vert = 0.0f;
            }
            else
            {
                //m_vert = Mathf.Sign(m_vert) * Mathf.Sqrt(1 - m_horiz * m_horiz); //travel in same direction, but less
            }
            m_vert = 0.0f;
            Debug.Log("only h: m_h = " + m_horiz.ToString() + "; m_v = " + m_vert.ToString());
        }
        else if(vert)// && !moving_vert) //only moving vertically
        {
            if(v_down)
            {
                //m_vert -= 0.0001f;
                m_vert = -max_d;
            }
            else
            {
                //m_vert += 0.0001f;
                m_vert = max_d;
            }
            if(m_vert > max_d)
            {
                m_vert = max_d;
                m_horiz = 0.0f;
            }
            else if(m_vert < -max_d)
            {
                m_vert = -max_d;
                m_horiz = 0.0f;
            }
            else
            {
                //m_horiz = Mathf.Sign(m_horiz) * Mathf.Sqrt(1 - m_vert * m_vert); //travel in same direction, but less
            }
            m_horiz = 0.0f;
            Debug.Log("only v: m_h = " + m_horiz.ToString() + "; m_v = " + m_vert.ToString());
        }
        if(!horiz && !vert) //if not moving, slow to a stop.
        {
            m_dist = m_dist / 1.2f - 0.0001f;
        }
        else //if moving, increase distance travelled until max.
        {
            //TODO: fine-tune this! starts too slow, and don't want it to stay high if reverse change of direction!
            m_dist = m_dist * movement_multiplier + 0.0001f;
        }
        if(m_dist < 0.0f)
        {
            m_dist = 0.0f;
        }
        else if(m_dist > m_max)
        {
            m_dist = m_max;
        }
        moving_horiz = m_horiz != 0.0f;
        moving_vert = m_vert != 0.0f;
    }

    /*public override bool Execute(GameObject obj)
    {
        //Debug.Log("exec");
        Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D> ();

        //Safety check for rigidbody
        if (rigidbody == null)
        {
            return false;
        }

        //OLD:
        //Get input on horizontal and vertical axes (Keybindings in project settings)
        //float h = Input.GetAxis ("Horizontal");
        //float v = Input.GetAxis ("Vertical");
        //rigidbody.velocity = new Vector2(speed * h, speed * v);

        //NEW w/ SettingsManager:
        UpdateMovement();
        float movement_h = m_right - m_left;
        float movement_v = m_up - m_down;
        //Allow diagonal movement
        rigidbody.velocity = new Vector2(speed * movement_h, speed * movement_v);

        return true;
    }*/

    /*protected void UpdateMovement()
    {
        //TODO: backup prev values to have smooth transitions. *2 each time?
        bool h_left = Input.GetKey(SettingsManager.GetMoveLeft());
        bool h_right = Input.GetKey(SettingsManager.GetMoveRight());
        bool v_up = Input.GetKey(SettingsManager.GetMoveUp());
        bool v_down = Input.GetKey(SettingsManager.GetMoveDown());
        bool walk = Input.GetKey(KeyCode.LeftAlt);
        float old_max = m_max;
        if(walk)
            m_max = 0.5f;
        if(h_left)
        {
            m_left += movement_multiplier * m_left + movement_delta;
        }
        else
        {
            m_left -= m_left / movement_loss_divider;// + movement_delta;
        }

        if(h_right)
        {
            m_right += movement_multiplier * m_right + movement_delta;
        }
        else
        {
            m_right -= m_right / movement_loss_divider + movement_delta;
        }

        if(v_up)
        {
            m_up += movement_multiplier * m_up + movement_delta;
        }
        else
        {
            m_up -= m_up / movement_loss_divider + movement_delta;
        }

        if(v_down)
        {
            m_down += movement_multiplier * m_down + movement_delta;
        }
        else
        {
            m_down -= m_down / movement_loss_divider + movement_delta;
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
        m_max = old_max;
    }*/
}
