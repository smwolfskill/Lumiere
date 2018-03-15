using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.Experimental.UIElements;
using UnityEngine.XR.WSA;

[CreateAssetMenu(menuName = "Lumiere/Actions/PickupItem")]
public class PickupItem : EntityAction
{
    private float lastInput = 0.0f;
    private GameObject itemObj = null;
    private ItemManager itemManager = null;
    private GameItem toPickup = null;

    /// <summary>
    /// Checks whether the player can pick up an item from the ground.
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if no item clicked upon, or player not in range of the object.</returns>
    public override bool Validate(GameObject obj)
    {
        //TODO: prevent player picking up items while inventory is open. 
        //There is also an infinite loop bug if attempting to move while inventory is open.

        float pickupItemInput = Input.GetAxis("PickupItem");
        bool clickedOnItem = false;
        if(pickupItemInput > 0.0f && pickupItemInput != lastInput)
        {
            //Gather 3D mouse position and raycasting information
            Ray ray = Camera.main.ScreenPointToRay(Input.mousePosition);
            Vector3 origin = obj.transform.position;    //location of the object (player)
            Vector3 worldPointClicked = Camera.main.ScreenToWorldPoint(Input.mousePosition);

            //Convert to 2D and detect any raycast hits on 2D colliders
            Ray2D ray2D = new Ray2D(new Vector2(ray.origin.x, ray.origin.y), new Vector2(ray.direction.x, ray.direction.y));
            RaycastHit2D hit2D = Physics2D.Raycast(ray2D.origin, ray2D.direction);
            if(hit2D.collider != null)
            {
                itemObj = hit2D.collider.gameObject;
                itemManager = itemObj.GetComponent<ItemManager>();
                if(itemManager != null)     //hit an item on the ground
                {
                    BoxCollider2D objCollider = obj.GetComponent<BoxCollider2D>();
                    float dist = Physics2D.Distance(objCollider, hit2D.collider).distance;
                    if(dist <= 0.0f)    //within collision range (so within pickup range)
                    {
                        toPickup = itemManager.item;
                        EntityActionManager actionManager = obj.GetComponent<EntityActionManager>();
                        target = actionManager.entity; //player
                        clickedOnItem = true;
                    }
                }
            }
        }
        lastInput = pickupItemInput;
        return clickedOnItem;
    }

    /// <summary>
    /// Pick up the item.
    /// </summary>
    /// <param name="obj">The GameObject (Player) that wants to execute this action.</param>
    /// <returns>Returns true if the item was picked up successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        //Debug.Log("Picking up item '" + toPickup.Name + "'");
        GameItem itemsLeft = target.inventory.AddItem(toPickup);

        if (itemsLeft == null) //remove item from physical world if picked up entire quantity
        {
            Object.Destroy(itemObj);
        }
        else //inventory full. Update ground item quantity after adding any we could to inventory.
        {
            toPickup.Quantity = itemsLeft.Quantity;
        }
        return true;
    }
}
